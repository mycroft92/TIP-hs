module Interpreter.Interpreter where

import AST.AST (Operator (..))
import AST.NAST
import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.State (
    MonadIO (liftIO),
    MonadState (get, put),
    MonadTrans (lift),
    StateT (runStateT),
 )
import Data.Foldable (foldlM, foldrM)
import Data.IORef
import Data.List (intercalate)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Interpreter.Environment (Env (..), addRef, createChildEnv, declareName, defineName, getRef, getVar, getVarRef, writeRef)
import Interpreter.SemanticValues

import Data.Map.Strict as Map (Map, empty, insert, lookup, member)

data InterpreterState = InterpreterState
    { env :: IORef Env
    , funcs :: Map.Map Value (Env, NFunDec) -- Range keeps the keeps unique since they are inherited from defintion
    , globals :: Env -- why is this problematic with IOREFs?
    }

type Interpreter a = ExceptT InterpreterException (StateT InterpreterState IO) a

_getEnv :: Interpreter Env
_getEnv = do
    st <- lift get
    liftIO $ readIORef (env st)

_getFEnv :: Interpreter (Map.Map Value (Env, NFunDec))
_getFEnv = do
    st <- lift get
    return (funcs st)

_putFEnv :: Map.Map Value (Env, NFunDec) -> Interpreter ()
_putFEnv ev = do
    st <- lift get
    lift $ put (st{funcs = ev})

_putEnv :: Env -> Interpreter ()
_putEnv ev = do
    st <- lift get
    liftIO $ writeIORef (env st) ev

_declare :: String -> Interpreter ()
_declare name = do
    st <- _getEnv
    liftIO $ declareName name st
_define :: String -> Value -> Interpreter ()
_define name val = do
    st <- _getEnv
    res <- liftIO $ defineName name val st
    case res of
        Nothing -> throwError $ Err $ "No variable '" ++ name ++ "' in the environment!"
        Just _ -> return ()

_addFunction :: Value -> (Env, NFunDec) -> Interpreter ()
_addFunction key val = do
    funcs <- _getFEnv
    let funcs' = Map.insert key val funcs
    _putFEnv funcs'

_getName :: String -> Interpreter Value
_getName var = do
    ev <- _getEnv
    mval <- liftIO (getVar var ev)
    case mval of
        Nothing -> throwError $ Err ("No variable: '" ++ var ++ "' found in the environment!")
        Just v -> return v
_getVarRef :: String -> Interpreter Int
_getVarRef name = do
    env' <- _getEnv
    idx <- liftIO $ getVarRef name env'
    case idx of
        Just idx' -> return idx'
        Nothing -> throwError $ Err ("No variable: '" ++ name ++ "' found in the environment!")

_getFunc :: Value -> Interpreter (Env, NFunDec)
_getFunc name = do
    env' <- _getFEnv
    case Map.lookup name env' of
        Just idx' -> return idx'
        Nothing -> throwError $ Err ("No function: '" ++ show name ++ "' found in the environment!")

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
        | otherwise = throwError $ Err "Division by zero error!"
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
evaluateExp NInput = do
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

assignNLExp :: NLexp -> Value -> Interpreter ()
assignNLExp (NIdent name) v = _define name v
assignNLExp (NDerefWrite name) v = do
    ref <- _getVarRef name
    ev <- _getEnv
    liftIO $ writeRef ref v ev
assignNLExp (NDirectWrite name field) v = do
    recval <- _getName name
    case recval of
        RECVAL _ -> do
            let newrec = changeField field v recval
            case newrec of
                Just newrec' -> _define name newrec'
                Nothing -> throwError $ Err $ "No such field '" ++ field ++ "' in record '" ++ name ++ "' val: " ++ show recval
        _ -> throwError $ Err $ "'" ++ name ++ "' is not a record type! value: " ++ show recval

evaluateStmt :: NStmt -> Interpreter ()
evaluateStmt (NOutput exp) = do
    e' <- evaluateExp exp
    liftIO $ print e'
evaluateStmt (NEAssign lhs exp) = do
    e' <- evaluateExp exp
    assignNLExp lhs e'
evaluateStmt e@(NWhile cond stmts) = do
    cval <- evaluateExp cond
    case cval of
        INTVAL 0 -> return ()
        INTVAL _ -> mapM_ evaluateStmt stmts >> evaluateStmt (NWhile cond stmts)
        v -> throwError $ Err ("Invalid conditional value: " ++ show v ++ " in while statement: " ++ show e)
evaluateStmt e@(NIfStmt cond stmt1 (Just stmt2)) = do
    cval <- evaluateExp cond
    case cval of
        INTVAL 0 -> mapM_ evaluateStmt stmt2
        INTVAL _ -> mapM_ evaluateStmt stmt1
        v -> throwError $ Err ("Invalid conditional value: " ++ show v ++ " in if statement: " ++ show e)
evaluateStmt e@(NIfStmt cond stmt1 Nothing) = do
    cval <- evaluateExp cond
    case cval of
        INTVAL 0 -> return ()
        INTVAL _ -> mapM_ evaluateStmt stmt1
        v -> throwError $ Err ("Invalid conditional value: " ++ show v ++ " in if statement: " ++ show e)
evaluateStmt e@(NFCAssign lhs (Func name args)) = do
    argVals <- mapM _getName args
    funval <- _getName name
    case funval of
        Fn n arity FFI _ -> do
            val <- ffiCall funval argVals
            assignNLExp lhs val
        Fn _ arity _ _ -> do
            val <- call funval argVals
            assignNLExp lhs val
        _ -> throwError $ Err ("Not a function! " ++ show (Func name args) ++ " : " ++ show funval)
evaluateStmt e@(NRefAssign lhs rec) = do
    idx <- _getVarRef rec
    assignNLExp lhs (REFVAL idx)

-- might need a different way to handle function environments
call :: Value -> [Value] -> Interpreter Value
call f@(Fn n arity _ _) args = do
    check args arity
    oldenv <- _getEnv
    (fenv, NFunDec fn argns vars body ret range) <- _getFunc f
    -- put the function environment, add args and execute fstmt
    fenv' <- liftIO $ createChildEnv fenv
    _putEnv fenv'
    -- callee's job to declare inputs and define them
    declareVars argns
    defineArgs argns args
    declareVars vars
    mapM_ evaluateStmt body
    val <- evaluateExp ret
    _putEnv oldenv
    return val
  where
    check :: [Value] -> Int -> Interpreter ()
    check args arity = if length args == arity then return () else throwError $ Err ("Wrong arity for: " ++ show f ++ ", given: " ++ intercalate "," (map show args))

    defineArgs :: [String] -> [Value] -> Interpreter ()
    defineArgs [] [] = return ()
    defineArgs (n : ns) (a : argVals) = _define n a >> defineArgs ns argVals
    defineArgs _ _ = throwError $ Err "impossible situation"

    declareVars :: [String] -> Interpreter ()
    declareVars [] = return ()
    declareVars (v : vs) = _declare v >> declareVars vs
call f _ = throwError $ Err $ show f ++ " not callable!"

ffiCall :: Value -> [Value] -> Interpreter Value
ffiCall f@(Fn n arity _ _) args = do
    check args arity
    case n of
        "clock" -> do
            mtime <- liftIO getPOSIXTime
            return $ INTVAL $ fromIntegral $ (round . (* 1000)) mtime
        _ -> throwError $ Err $ show f ++ " is not a defined ffi!"
  where
    check :: [Value] -> Int -> Interpreter ()
    check args arity = if length args == arity then return () else throwError $ Err ("Wrong arity for: " ++ show f ++ ", given: " ++ intercalate "," (map show args))
ffiCall f _ = throwError $ Err $ show f ++ " Invalid value for ffi!"
