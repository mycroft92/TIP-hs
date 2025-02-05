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
-- Analysis should be simpler since we don't have scopes
data TypeState = TS
    { varEnv :: Env
    , funcEnv :: Env -- This has resolved global func types
    , freshVar :: Int
    , soln :: Substs Type
    }

type TypeCheck a = ExceptT String (StateT TypeState IO) a

putVarEnv :: String -> Type -> TypeCheck ()
putVarEnv name typ = do
    x <- lift get
    let venv = add name typ (varEnv x)
    put (x{varEnv = venv})

putFuncEnv :: String -> Type -> TypeCheck ()
putFuncEnv name typ = do
    x <- lift get
    let fenv = add name typ (funcEnv x)
    put (x{funcEnv = fenv})

getFuncType :: String -> TypeCheck (Maybe Type)
getFuncType fname = do
    x <- lift get
    return (look fname (funcEnv x))

getVarType :: String -> TypeCheck (Maybe Type)
getVarType fname = do
    x <- lift get
    return (look fname (varEnv x))

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
typeCheckExpr (Id name rng) = do
    x <- getVarType name
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
typeCheckExpr e@(VarRef name r) = do
    ty <- getVarType name
    case ty of
        Just ty -> return (Points ty)
        Nothing -> throwError $ "Undeclared identifier in VarRef expr:  " ++ show e
typeCheckExpr (Unop Ampersand e _) = undefined
