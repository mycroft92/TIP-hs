module Analysis.TypeChecker where

import Control.Monad (forM)
import Data.Foldable (foldlM, foldrM)
import Data.Map as Map (Map, empty, foldrWithKey, fromList, insert, lookup, member)
import Solvers.Unification
import Solvers.UnionFindSolver (Substs)

-- MonadIO (liftIO),

import AST.AST
import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.State (
    MonadState (get, put),
    MonadTrans (lift),
    StateT (runStateT),
 )

type Env = (Map.Map String Type)

add :: String -> Type -> Env -> Env
add k v e = Map.insert k v e

look :: String -> Env -> Maybe Type
look k e = Map.lookup k e

-- This is the only analysis we do on AST, rest all its on Normalized AST
-- Analysis should be simpler since we don't have scopes (only two- local and global)
-- we have recursive functions but not mutually recursive functions
data TypeState = TS
    { localenv :: Env -- this is needed to avoid capturing variables from other functions
    , globalenv :: Env
    , freshVar :: Int
    , soln :: Substs Type
    }

type TypeCheck a = ExceptT String (StateT TypeState IO) a

putEnv :: String -> Type -> TypeCheck ()
putEnv name typ = do
    x <- lift get
    let venv = add name typ (localenv x)
    put (x{localenv = venv})

-- error checks to be performed at typeCheckFunction
putGlobalEnv :: String -> Type -> TypeCheck ()
putGlobalEnv name typ = do
    x <- lift get
    let venv = add name typ (globalenv x)
    put (x{globalenv = venv})
fresh :: TypeCheck Int
fresh = do
    st <- lift get
    let fresh' = freshVar st
    put (st{freshVar = fresh' + 1})
    return fresh'

getType :: String -> TypeCheck (Maybe Type)
getType fname = do
    x <- lift get
    return (look fname (localenv x))

getSoln :: TypeCheck Solution
getSoln = do
    st <- lift get
    return (solution . soln $ st)

applySoln :: Type -> TypeCheck Type
applySoln ty = do
    sol <- getSoln
    return (substSoln ty sol)

-- This function is the workhorse of unification, it keeps maintaining the state (of equivalent type expressions)
unifyTypes :: Type -> Type -> TypeCheck ()
unifyTypes t1 t2 = do
    st <- lift get
    let subs = soln st
    case unify' subs t1 t2 of
        Left err -> throwError err
        Right subs' -> do
            lift $ put (st{soln = subs'})

renameFType :: Type -> TypeCheck Type
renameFType (Arrow args ret) = do
    ins <- (mapM genNewType args)
    ret' <- genNewType ret
    return (Arrow ins ret')
  where
    --- generates new type if it is a var type else passes it as is. This is a helper for renaming function types
    genNewType :: Type -> TypeCheck Type
    genNewType (Var _) = do
        newid <- fresh
        return (Var newid)
    genNewType ty = return ty
renameFType ty = return ty

initState :: TypeState
initState = TS Map.empty Map.empty 0 Map.empty

runTypeChecker :: [AFuncDec] -> IO (Either String TypeState)
runTypeChecker funcs = do
    x <- runStateT (runExceptT (typeCheckProgram funcs)) initState
    case x of
        (Left err, _) -> return $ Left err
        (Right (), st) -> return $ Right st

typeCheckProgram :: [AFuncDec] -> TypeCheck ()
typeCheckProgram funcs = mapM_ typeCheckFun funcs

typeCheckFun :: AFuncDec -> TypeCheck a
typeCheckFun f = undefined

typeCheckExpr :: AExpr -> TypeCheck Type
-- we need to check if the identifier maps to a function as well! Then do fresh variable renaming for args
typeCheckExpr (Id name rng) = do
    x <- getType name
    case x of
        Just ty -> return ty -- applySoln ty
        Nothing -> throwError $ "Undeclared identifier '" ++ name ++ "' used @" ++ show rng
typeCheckExpr (Number _ _) = return INT
typeCheckExpr e@(Binop e1 AEqq e2 r) = do
    e1ty <- typeCheckExpr e1
    e2ty <- typeCheckExpr e2
    unifyTypes e1ty e2ty
    -- need to unify types e1ty and e2ty
    -- save the unification result in the map
    return INT
typeCheckExpr e@(Binop e1 _ e2 r) = do
    e1ty <- typeCheckExpr e1
    e2ty <- typeCheckExpr e2
    unifyTypes e1ty INT
    unifyTypes e2ty INT
    -- need to unify types e1ty and e2ty
    -- save the unification result in the map
    return INT
typeCheckExpr (Input _) = return INT
typeCheckExpr (Alloc e _) = do
    ty <- typeCheckExpr e
    return (Points ty)
-- we need to check if the identifier maps to a function as well!
typeCheckExpr e@(VarRef name r) = do
    ty <- getType name
    case ty of
        Just ty -> return (Points ty)
        Nothing -> throwError $ "Undeclared identifier in VarRef expr:  " ++ show e
typeCheckExpr (Unop ATimes e _) = do
    ety <- typeCheckExpr e
    newid <- fresh
    -- ety should be a pointer type
    unifyTypes (Points (Var newid)) ety
    return (Var newid)
typeCheckExpr (Unop AMinus e _) = do
    ety <- typeCheckExpr e
    unifyTypes ety INT
    return INT
typeCheckExpr e@(Unop _ _ _) = throwError $ "Invalid unop expression encountered " ++ show e
typeCheckExpr (Null _) = do
    newid <- fresh
    return (Points (Var newid))
typeCheckExpr (CallExpr e args _) = do
    ety <- typeCheckExpr e
    argtys <- (mapM typeCheckExpr args)
    newid <- fresh
    -- function type is same as expression's type (ex: function pointer deref)
    unifyTypes ety (Arrow argtys (Var newid))
    return (Var newid)
typeCheckExpr (Record _ _) = throwError "unimplemented"
typeCheckExpr (FieldAccess _ _ _) = throwError "unimplemented"

typeCheckLE :: LExp -> TypeCheck Type
typeCheckLE e@(Ident name _) = do
    ty <- getType name
    case ty of
        Just ty -> return (Points ty)
        Nothing -> throwError $ "Undeclared identifier in VarRef expr:  " ++ show e
typeCheckLE (ExprWrite exp _) = typeCheckExpr exp
typeCheckLE _ = throwError "unimplemented"

typeCheckStmt :: AStmt -> TypeCheck ()
typeCheckStmt (SimpleAssign le exp _) = do
    lety <- typeCheckLE le
    expty <- typeCheckExpr exp
    unifyTypes lety expty
typeCheckStmt s = undefined
