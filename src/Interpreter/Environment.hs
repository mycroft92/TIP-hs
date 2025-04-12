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
    , depth :: Int
    }

printEnv :: Env -> IO String
printEnv e = do
    ev <- readIORef (e_values e)
    er <- readIORef (e_refs e)
    enc <- readIORef (enclosing e)
    case enc of
        Nothing -> do
            let x = foldrWithKey (\k v acc -> show k ++ ":" ++ show v ++ ", " ++ acc) "" ev
             in let y = foldr (\k acc -> show k ++ ", " ++ acc) "" er
                 in putStrLn x >> putStrLn y >> return x
        Just e' -> do
            p <- printEnv e'
            let x = foldrWithKey (\k v acc -> show k ++ ":" ++ show v ++ ", " ++ acc) "" ev
             in let y = foldr (\k acc -> show k ++ ", " ++ acc) "" er
                 in do
                        putStrLn $ "d:" ++ show (depth e) ++ "\n\t" ++ x ++ "\n\t" ++ y
                        return $ p ++ "\n\t" ++ x ++ "\ner: " ++ y

newEnv :: IO Env
newEnv = do
    ev <- newIORef Map.empty
    er <- newIORef []
    enc <- newIORef Nothing
    return $ Env ev er enc 0

declareName :: String -> Env -> IO ()
declareName name env = do
    (idx, _) <- addRef NULL env -- ignore the env number
    modifyIORef' (e_values env) (Map.insert name idx)

-- return env

defineName :: String -> Value -> Env -> IO (Maybe ())
defineName name val env = do
    -- print "Modifying environment"
    -- printEnv env
    idx <- getVarRef name env
    case idx of
        Nothing -> return Nothing
        Just (idx', dep) -> if dep /= depth env then return Nothing else Just <$> writeRef idx' (depth env) val env

declareNDefineName :: String -> Value -> Env -> IO ()
declareNDefineName name val env = do
    declareName name env
    idx <- getVarRef name env
    let (idx', dep) = fromJust idx
    writeRef idx' (depth env) val env

addRef :: Value -> Env -> IO (Int, Int)
addRef val env = do
    list <- readIORef (e_refs env)
    let list' = list ++ [val]
    let index = length list' - 1
    _ <- writeIORef (e_refs env) list'
    return (index, depth env)

_getRef :: Int -> Env -> IO (Maybe Value)
_getRef ref env = do
    er <- readIORef (e_refs env)
    if length er <= ref then return Nothing else return (Just (er !! ref))

writeRef :: Int -> Int -> Value -> Env -> IO ()
writeRef idx enum val env = do
    ev <- _findEnvAtNum enum env
    case ev of
        Nothing -> return () -- probably should fail
        Just ev' -> do
            er <- readIORef (e_refs ev')
            let ns = splitAt idx er
            let er' = fst ns ++ (val : tail (snd ns))
            writeIORef (e_refs ev') er'

_findEnvAtNum :: Int -> Env -> IO (Maybe Env)
_findEnvAtNum num ev
    | depth ev == num = return $ Just ev
    | otherwise = do
        enc <- readIORef (enclosing ev)
        case enc of
            Nothing -> return Nothing
            Just enc' -> _findEnvAtNum num enc'

createChildEnv :: Env -> IO Env
createChildEnv env = do
    -- print "Create Env called with parent:"
    -- printEnv env
    -- print "########"
    ev <- newIORef Map.empty
    er <- newIORef []
    enc <- newIORef $ Just env
    let dep = depth env + 1
    let env' = Env ev er enc dep
    return env'

getVar :: String -> Env -> IO (Maybe Value)
getVar vnam env = do
    m <- readIORef (e_values env)
    enc <- readIORef (enclosing env)
    -- print "Searching current"
    -- s <- printEnv env
    -- print s
    case Map.lookup vnam m of
        Nothing ->
            -- return Nothing -- getVar only searches in the current environment
            case enc of
                Nothing -> return Nothing
                Just ev -> getVar vnam ev
        Just x -> _getRef x env

getRefAtEnv :: Int -> Int -> Env -> IO (Maybe Value)
getRefAtEnv ref dep e@(Env ev er enc d)
    | d == dep = print " env:" >> printEnv e >> _getRef ref e
    | otherwise = do
        enc' <- readIORef enc
        print "env: "
        printEnv e
        case enc' of
            Just ev' -> getRefAtEnv ref dep ev'
            Nothing -> return Nothing

getVarRef :: String -> Env -> IO (Maybe (Int, Int))
getVarRef name env = do
    m <- readIORef (e_values env)
    case Map.lookup name m of
        Just x -> return $ Just (x, depth env)
        Nothing -> do
            enc <- readIORef (enclosing env)
            case enc of
                Nothing -> return Nothing
                Just env' -> getVarRef name env'
