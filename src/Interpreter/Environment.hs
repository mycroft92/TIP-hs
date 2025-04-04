-- stack of envs

module Interpreter.Environment where

import AST.NAST
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map as M (foldrWithKey, keys)
import Data.Map.Strict as Map (Map, empty, insert, lookup, member)
import Interpreter.SemanticValues

data Env = Env
    { e_values :: IORef (Map.Map String Int) -- name to its storage location map. This is needed to support var references
    , e_refs :: IORef [Value]
    , -- we need to add to e_values, the keys from efuncs so that the environment is consistent
      e_funcs :: IORef (Map.Map Value (Env, NFunDec)) -- future proofing this if I want to do add nested functions
    , enclosing :: IORef (Maybe Env)
    }

printEnv :: Env -> IO String
printEnv e = do
    ev <- readIORef (e_values e)
    enc <- readIORef (enclosing e)
    case enc of
        Nothing -> do
            let x = foldrWithKey (\k v acc -> show k ++ ":" ++ show v ++ ", " ++ acc) "" ev
             in print x >> return x
        Just e' -> do
            p <- printEnv e'
            let x = foldrWithKey (\k v acc -> show k ++ ":" ++ show v ++ ", " ++ acc) "" ev
             in do
                    print $ "\n\t" ++ x
                    return $ p ++ "\n\t" ++ x

newEnv :: IO Env
newEnv = do
    ev <- newIORef Map.empty
    er <- newIORef []
    ef <- newIORef Map.empty
    enc <- newIORef Nothing
    return $ Env ev er ef enc

defineName :: String -> Value -> Env -> IO Env
defineName name val env = do
    -- print "Modifying environment"
    -- printEnv env
    idx <- addRef val env
    modifyIORef' (e_values env) (Map.insert name idx)
    return env

addRef :: Value -> Env -> IO Int
addRef val env = do
    -- print "Modifying environment"
    -- printEnv en
    list <- readIORef (e_refs env)
    let list' = list ++ [val]
    let index = length list' - 1
    _ <- writeIORef (e_refs env) list'
    return index

getRef :: Int -> Env -> IO (Maybe Value)
getRef ref env = do
    er <- readIORef (e_refs env)
    if length er <= ref then return Nothing else return (Just (er !! ref))

addFunction :: Value -> NFunDec -> Env -> IO ()
addFunction key fd env = do
    modifyIORef' (e_funcs env) (Map.insert key (env, fd))

createChildEnv :: Env -> IO Env
createChildEnv env = do
    -- print "Create Env called with parent:"
    -- printEnv env
    -- print "########"
    f' <- readIORef (e_funcs env)
    ev <- newIORef Map.empty
    er <- newIORef []
    ef <- newIORef f'
    enc <- newIORef $ Just env
    let env' = Env ev er ef enc
    _addFuncValues f' env'
    return env'
  where
    -- we are making the child env consistent by adding the function keys from e_funcs
    _addFuncValues :: Map.Map Value (Env, NFunDec) -> Env -> IO ()
    _addFuncValues funcs ev = do
        let keys = M.keys funcs
        mapM_ (\f@(Fn name _ _) -> defineName name f ev) keys

getVar :: String -> Env -> IO (Maybe Value)
getVar vnam env = do
    m <- readIORef (e_values env)
    enc <- readIORef (enclosing env)
    -- print "Searching current"
    -- s <- printEnv env
    -- print s
    case Map.lookup vnam m of
        Nothing -> case enc of
            Nothing -> return Nothing
            Just ev -> getVar vnam ev
        Just x -> getRef x env

getVarRef :: String -> Env -> IO (Maybe Int)
getVarRef name env = do
    m <- readIORef (e_values env)
    return $ Map.lookup name m
