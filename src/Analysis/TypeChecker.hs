module Analysis.TypeChecker where

import Control.Monad (forM)
import Data.Foldable (foldlM, foldrM)
import Data.Map as Map (Map, empty, foldrWithKey, fromList, insert, lookup, member)
import Solvers.Unification

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
    , soln :: Solution
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
    return (soln st)

addSoln :: Solution -> TypeCheck ()
addSoln ss = do
    st <- lift get
    -- not checking conflicts currently, we perform substitution at every stage hence this should work
    let sol' = Map.foldrWithKey (\k v acc -> if Map.member k acc then acc else Map.insert k v acc) (soln st) ss
    lift $ put (st{soln = sol'})

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
        Just ty -> return ty
        Nothing -> throwError $ "Undeclared identifier '" ++ name ++ "' used @" ++ show rng
typeCheckExpr (Number _ _) = return INT
typeCheckExpr e@(Binop e1 AEqq e2 r) = do
    e1ty <- typeCheckExpr e1
    e2ty <- typeCheckExpr e2
    -- need to unify types e1ty and e2ty
    -- save the unification result in the map
    return INT
