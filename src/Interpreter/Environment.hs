-- stack of envs

module Interpreter.Environment where

import AST.NAST
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map as M (foldrWithKey)
import Data.Map.Strict as Map (Map, empty, insert, lookup, member)
import Data.Maybe (fromJust)
import Interpreter.SemanticValues

data Env = Env
    { e_values :: IORef (Map.Map String Int) -- name to its storage location map. This is needed to support var references
    , e_refs :: IORef [Value]
    , -- , -- we need to add to e_values, the keys from efuncs so that the environment is consistent
      -- e_funcs :: IORef (Map.Map Value (Env, NFunDec)) -- moved functions to main env instead
      enclosing :: IORef (Maybe Env)
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

declareName :: String -> Env -> IO ()
declareName name env = do
    idx <- addRef NULL env
    modifyIORef' (e_values env) (Map.insert name idx)

-- return env

defineName :: String -> Value -> Env -> IO (Maybe ())
defineName name val env = do
    -- print "Modifying environment"
    -- printEnv env
    idx <- getVarRef name env
    case idx of
        Nothing -> return Nothing
        Just idx' -> Just <$> writeRef idx' val env

declareNDefineName :: String -> Value -> Env -> IO ()
declareNDefineName name val env = do
    declareName name env
    idx <- getVarRef name env
    let idx' = fromJust idx
    writeRef idx' val env

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

writeRef :: Int -> Value -> Env -> IO ()
writeRef idx val env = do
    er <- readIORef (e_refs env)
    let ns = splitAt idx er
    let er' = fst ns ++ (val : tail (snd ns))
    writeIORef (e_refs env) er'

createChildEnv :: Env -> IO Env
createChildEnv env = do
    -- print "Create Env called with parent:"
    -- printEnv env
    -- print "########"
    ev <- newIORef Map.empty
    er <- newIORef []
    enc <- newIORef $ Just env
    let env' = Env ev er enc
    return env'

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
