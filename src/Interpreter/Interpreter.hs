module Interpreter.Interpreter where

import AST.AST (Operator (..))
import AST.NAST
import Interpreter.SemanticValues

import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.State (
    MonadIO (liftIO),
    MonadState (get, put),
    MonadTrans (lift),
    StateT (runStateT),
 )
import Data.Foldable (foldrM)
import Data.IORef
import Interpreter.Environment (Env (..), addFunction, addRef, defineName, getRef, getVar)

data InterpreterState = InterpreterState
    { env :: IORef Env
    , globals :: Env -- why is this problematic with IOREFs?
    }

type Interpreter a = ExceptT InterpreterException (StateT InterpreterState IO) a

_getEnv :: Interpreter Env
_getEnv = do
    st <- lift get
    liftIO $ readIORef (env st)

_putEnv :: Env -> Interpreter ()
_putEnv ev = do
    st <- lift get
    liftIO $ writeIORef (env st) ev

_define :: String -> Value -> Interpreter ()
_define name val = do
    st <- _getEnv
    ev' <- liftIO $ defineName name val st
    _putEnv ev'

_getName :: String -> Interpreter Value
_getName var = do
    ev <- _getEnv
    mval <- liftIO (getVar var ev)
    case mval of
        Nothing -> throwError $ Err ("No variable: '" ++ var ++ "' found in the environment!")
        Just v -> return v
_getRef :: Int -> Interpreter Value
_getRef ref = do
    ev <- _getEnv
    val <- liftIO $ getRef ref ev
    case val of
        Nothing -> throwError $ Err ("No reference at index: '" ++ show ref ++ "' found in the environment!")
        Just v -> return v

_addRef :: Value -> Interpreter Int
_addRef val = do
    ev <- _getEnv
    liftIO $ addRef val ev

evaluateExp :: NExpr -> Interpreter Value
evaluateExp (NId name) = _getName name
evaluateExp (NBinop e1 op e2) = do
    e1' <- evaluateExp e1
    e2' <- evaluateExp e2
    case op of
        APlus -> handle e1' e2' (+)
        AMinus -> handle e1' e2' (-)
        ATimes -> handle e1' e2' (*)
        ADivide -> divide e1' e2'
        ANEq -> notEq e1' e2'
        AEqq -> equal e1' e2'
        AGe -> handleBool e1' e2' (>=)
        AGt -> handleBool e1' e2' (>)
        ALe -> handleBool e1' e2' (<=)
        ALt -> handleBool e1' e2' (<)
        ALOr -> orMe e1' e2'
        ALAnd -> andMe e1' e2'
  where
    handle :: Value -> Value -> (Int -> Int -> Int) -> Interpreter Value
    handle (INTVAL e1) (INTVAL e2) func = return (INTVAL (func e1 e2))
    handle e1' e2' _ = throwError $ Err ("Typecheck failure: Illegal operands '" ++ show e1 ++ "', '" ++ show e2 ++ "' for arithmetic operation")

    handleBool :: Value -> Value -> (Int -> Int -> Bool) -> Interpreter Value
    handleBool (INTVAL e1) (INTVAL e2) func
        | func e1 e2 = return $ INTVAL 1
        | otherwise = return $ INTVAL 0
    handleBool e1' e2' _ = throwError $ Err ("Typecheck failure: Illegal operands '" ++ show e1 ++ "', '" ++ show e2 ++ "' for comparison operation")

    divide :: Value -> Value -> Interpreter Value
    divide (INTVAL e1) (INTVAL e2)
        | e2 /= 0 = return (INTVAL (div e1 e2))
        | otherwise = throwError $ Err ("Division by zero error!")
    divide e1' e2' = throwError $ Err ("Typecheck failure: Illegal operands '" ++ show e1 ++ "', '" ++ show e2 ++ "' for arithmetic operation")

    notEq :: Value -> Value -> Interpreter Value
    notEq e1' e2' = do
        v <- equal e1' e2'
        case v of
            INTVAL 0 -> return (INTVAL 1)
            INTVAL _ -> return (INTVAL 0)
            _ -> throwError $ Err "critical failure"

    equal :: Value -> Value -> Interpreter Value
    equal e1' e2'
        | e1' == e2' = return (INTVAL 1)
        | otherwise = return (INTVAL 0)
    orMe :: Value -> Value -> Interpreter Value
    orMe (INTVAL e1') (INTVAL e2')
        | e1' > 0 = return (INTVAL 1)
        | e2' > 0 = return (INTVAL 1)
        | otherwise = return (INTVAL 0)
    orMe e1' e2' = throwError $ Err ("Typecheck failure: Illegal operands '" ++ show e1 ++ "', '" ++ show e2 ++ "' for boolean operation")

    andMe :: Value -> Value -> Interpreter Value
    andMe (INTVAL e1') (INTVAL e2')
        | e1' > 0 && e2' > 0 = return (INTVAL 1)
        | otherwise = return (INTVAL 0)
    andMe e1' e2' = throwError $ Err ("Typecheck failure: Illegal operands '" ++ show e1 ++ "', '" ++ show e2 ++ "' for boolean operation")
evaluateExp (NUnop op exp) = do
    e' <- evaluateExp exp
    case op of
        ATimes -> case e' of
            REFVAL i -> _getRef i
            _ -> throwError $ Err ("Not a valid reference: " ++ show e' ++ " obtained from exp: " ++ show exp)
        AMinus -> case e' of
            INTVAL i -> return (INTVAL (-i))
            _ -> throwError $ Err ("Invalid expression: " ++ show exp ++ " to unary minus")
        _ -> throwError $ Err ("Critical error, unexpected operator at unop exp: " ++ show (NUnop op exp))
evaluateExp NNull = return NULL
evaluateExp (NNum i) = return (INTVAL i)
evaluateExp (NInput) = do
    v <- liftIO getLine
    let v' = read v :: Int
    return (INTVAL v')
evaluateExp (NAlloc e) = do
    e' <- evaluateExp e
    idx <- _addRef e'
    return (REFVAL idx)
evaluateExp (NRec fs) = do
    fields <- mapM evaluateField fs
    return (RECVAL fields)
evaluateExp (NFieldAccess recname recfield) = do
    val <- _getName recname
    case val of
        (RECVAL fvals) -> case findField recfield fvals of
            Nothing -> throwError $ Err ("No such field: " ++ recfield ++ " in record name: " ++ recname ++ " val: " ++ show val)
            Just v -> return v
        e' -> throwError $ Err ("critical error, not a record value:" ++ show e' ++ " of var: " ++ show recname)

evaluateField :: NRecField -> Interpreter (String, Value)
evaluateField (RF fname fexp) = do
    e' <- evaluateExp fexp
    return (fname, e')
