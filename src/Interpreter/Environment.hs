-- stack of envs

module Interpreter.Environment where

import AST.NAST
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map as M (foldrWithKey)
import Data.Map.Strict as Map (Map, empty, insert, lookup, member)
import Interpreter.SemanticValues

data Env = Env
    { e_values :: IORef (Map.Map String Value)
    , e_refs :: IORef [Value]
    , e_funcs :: IORef (Map.Map Value (Env, NFunDec)) -- future proofing this if I want to do add nested functions
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

define :: String -> Value -> Env -> IO ()
define name val env = do
    -- print "Modifying environment"
    -- printEnv env
    modifyIORef' (e_values env) (Map.insert name val)

createChildEnv :: Env -> IO Env
createChildEnv env = do
    -- print "Create Env called with parent:"
    -- printEnv env
    -- print "########"
    ev <- newIORef Map.empty
    er <- newIORef []
    f' <- readIORef (e_funcs env)
    ef <- newIORef f'
    enc <- newIORef $ Just env
    return $ Env ev er ef enc
