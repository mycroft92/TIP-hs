module Analysis.TypeChecker where

import Control.Monad (forM)
import Data.Foldable (foldlM, foldrM)
import Data.Map as Map (Map, empty, foldrWithKey, fromList, insert, lookup, member)

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

initState :: TypeState
initState = TS Map.empty Map.empty

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
typeCheckExpr e@(Binop e1 _ e2 r) = do
    e1ty <- typeCheckExpr e1
    e2ty <- typeCheckExpr e2
    -- need to unify types e1ty and e2ty with INT
    return INT
