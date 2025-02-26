-- stack of envs

module Interpreter.Environment where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map as M (foldrWithKey)
import Data.Map.Strict as Map (Map, empty, insert, lookup, member)
import Interpreter.SemanticValues

data Env = Env
    { e_values :: IORef (Map.Map String Value)
    , e_refs :: IORef [Value]
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
    enc <- newIORef Nothing
    return $ Env ev er enc
