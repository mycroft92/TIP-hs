module AST.Normalization (normalizeFunctions) where

import AST.AST
import AST.NAST
import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.State (
    -- MonadIO (liftIO),
    MonadState (get, put),
    MonadTrans (lift),
    StateT (runStateT),
    liftIO,
 )
import Data.Foldable (foldlM, foldrM)
import Parser.Lexer (AlexPosn (..), Range (..))
import Parser.TokenTypes (Token (String))

-- Fresh var list needs to be created for every function
data NormState = NormState
    { freshvar :: Int
    , id_list :: [String]
    , stmts :: [NStmt]
    }
    deriving (Eq, Show)

type Normalize a = ExceptT String (StateT NormState IO) a

initState :: NormState
initState = NormState 0 [] []

addStmt :: NStmt -> Normalize ()
addStmt x = do
    state <- lift get
    lift $ put (state{stmts = x : stmts state})

addAssignStmt :: String -> NExpr -> Normalize ()
addAssignStmt id e = addStmt (NEAssign (NIdent id) e)

getStmt :: Normalize [NStmt]
getStmt = do
    st <- lift get
    return (stmts st)
getIds :: Normalize [String]
getIds = do
    st <- lift get
    return (id_list st)

resetState :: Normalize ()
resetState = lift $ put initState

newid :: Normalize String
newid = do
    st <- lift get
    let myid = "$t" ++ show (freshvar st)
    lift $ put (st{freshvar = freshvar st + 1, id_list = myid : id_list st})
    return myid

normalizeLExp :: LExp -> Normalize NLexp
normalizeLExp (Ident x _) = return (NIdent x)
normalizeLExp e@(ExprWrite aexp _) = do
    id <- normalizeExp aexp
    case id of
        (NId x) -> return (NIdent x)
        (NUnop ATimes (NId x)) -> return (NDerefWrite x)
        _ -> throwError $ "Normalization failed for Lexp: " ++ show e
-- normalizeLExp e@(DirectWrite rn fn _) = return (NDirectWrite rn fn)
normalizeLExp e@(IndirectWrite (Id rn _) fn _) = return (NDirectWrite rn fn)
normalizeLExp e@(IndirectWrite exp fn _) = do
    id <- normalizeExp exp
    case id of
        (NId x) -> return (NDirectWrite x fn)
        _ -> throwError $ "Normalization failed for Lexp: " ++ show e

normalizeRecField :: RecField -> Normalize NRecField
normalizeRecField (RecField x e _) = do
    nexp <- normalizeExp e
    return (RF x nexp)

normalizeExp :: AExpr -> Normalize NExpr
normalizeExp (Id s _) = return (NId s)
normalizeExp (Binop e1 op e2 _) = do
    e1' <- normalizeExp e1
    e2' <- normalizeExp e2
    -- This was buggy because in loop conditions while (n-1) translates to t0 = n-1, while (t0) which is wrong semantically.
    -- new <- newid
    -- addAssignStmt new (NBinop e1' op e2')
    -- return (NId new)
    return (NBinop e1' op e2')
normalizeExp (Unop ATimes e _) = do
    case e of
        (Id e' _) -> return (NUnop ATimes (NId e'))
        _ -> do
            e' <- normalizeExp e
            -- Normalize the dereferencing so that we have lexps only in forms t, *t
            new <- newid
            addAssignStmt new e'
            return (NUnop ATimes (NId new))
normalizeExp (Unop op e _) = do
    e' <- normalizeExp e
    return (NUnop op e')
normalizeExp (Number x _) = return (NNum x)
normalizeExp (Input _) = return NInput
normalizeExp (Record xs _) = do
    es <- mapM normalizeRecField xs
    return (NRec es)
normalizeExp (Null _) = return NNull
normalizeExp (FieldAccess (Id x _) fn _) = return (NFieldAccess x fn)
normalizeExp e@(FieldAccess (Record xs _) fn _) =
    -- xs' <- mapM normalizeRecField xs
    case findRecField xs fn of
        Just e -> normalizeExp e
        Nothing -> throwError $ "No field : " ++ fn ++ " in record expr: " ++ show e
  where
    findRecField :: [RecField] -> String -> Maybe AExpr
    findRecField [] _ = Nothing
    findRecField ((RecField n' e _) : xs) n
        | n' == n = Just e
        | otherwise = findRecField xs n
normalizeExp (FieldAccess e fn _) = do
    e' <- normalizeExp e
    new <- newid
    addAssignStmt new e'
    return (NFieldAccess new fn)
-- references come only at equation level
normalizeExp (VarRef x _) = do
    new <- newid
    addStmt (NRefAssign (NIdent new) x)
    return (NId new)
normalizeExp (Alloc e _) = do
    e' <- normalizeExp e
    return (NAlloc e')
normalizeExp (CallExpr fe args _) = do
    fn' <- caseNormExp fe
    args' <- mapM caseNormExp args
    new <- newid
    addStmt (NFCAssign (NIdent new) (Func fn' args'))
    return (NId new)
  where
    -- either this needs to allow Binops or Binops need a temporary added, choosing the latter
    caseNormExp :: AExpr -> Normalize String
    caseNormExp e = do
        e' <- normalizeExp e
        case e' of
            (NId x) ->
                -- liftIO (print ("farg e: " ++ show e ++ " v: " ++ x))
                return x
            _ -> do
                new <- newid
                -- liftIO $ print ("farg: " ++ show e ++ " norm: " ++ show e' ++ " id:" ++ new)
                addStmt (NEAssign (NIdent new) e')
                return new

-- case e' of
-- (NId x) -> return x

-- _ -> throwError $ "cannot normalize expr (function call) : " ++ show e ++ " normalized to :" ++ show e'

normalizeStmt :: AStmt -> Normalize ()
normalizeStmt s@(SimpleAssign le e _) = do
    -- liftIO $ print ("Normalizing " ++ show s)
    le' <- normalizeLExp le
    e' <- normalizeExp e
    -- liftIO $ print ("lexp: " ++ show le' ++ " exp: " ++ show e')
    addStmt (NEAssign le' e')
normalizeStmt (FieldAssign le e _) = do
    le' <- normalizeLExp le
    e' <- normalizeExp e
    case le' of
        (NDirectWrite _ _) -> addStmt (NEAssign le' e')
        _ -> throwError $ "Normalization failed, expected record type for lexp: " ++ show le ++ " got: " ++ show le'
normalizeStmt (Output ae _) = do
    ae' <- normalizeExp ae
    addStmt (NOutput ae')
normalizeStmt (Seq st1 st2 _) = normalizeStmt st1 >> normalizeStmt st2
normalizeStmt (IfStmt cond tstmt (Just fstmt) _) = do
    cond' <- normalizeExp cond
    tstmt' <- handleBlock tstmt
    fstmt' <- handleBlock fstmt
    addStmt (NIfStmt cond' tstmt' (Just fstmt'))
normalizeStmt (IfStmt cond tstmt Nothing _) = do
    cond' <- normalizeExp cond
    tstmt' <- handleBlock tstmt
    addStmt (NIfStmt cond' tstmt' Nothing)
normalizeStmt (WhileStmt cond block _) = do
    cond' <- normalizeExp cond
    block' <- handleBlock block
    addStmt (NWhile cond' block')
normalizeStmt (NullStmt _) = return ()

handleBlock :: AStmt -> Normalize [NStmt]
handleBlock block = do
    st <- lift get
    -- reset state before running stmts
    lift $ put (NormState{freshvar = freshvar st, id_list = [], stmts = []})
    normalizeStmt block
    blst <- lift get
    let idlist = id_list st ++ id_list blst
    let fv = freshvar blst
    let block' = reverse (stmts blst)
    -- put state back, new statement must be handled by top scope
    lift $ put (NormState{freshvar = fv, id_list = idlist, stmts = stmts st})
    return block'

-- normalizeFunction :: AFuncDec -> IO (Either String NFunDec)
normalizeFunction :: AFuncDec -> Normalize NFunDec
normalizeFunction (Fun fn fargs fvars fbody fret (Range a1 a2)) = do
    body <- normalizeStmt fbody
    ret <- normalizeExp fret
    stmts <- getStmt
    vars <- getIds
    let rng = (MyRange start stop)
    resetState
    return (NFunDec fn fargs (fvars ++ vars) (reverse stmts) ret rng)
  where
    posc (AlexPn a l c) = MyPos l c
    start = posc a1
    stop = posc a2

normalizeFunctions :: [AFuncDec] -> IO (Either String [NFunDec])
normalizeFunctions fs = do
    ret <- runStateT (runExceptT (mapM normalizeFunction fs)) initState
    case ret of
        (Left err, _) -> return $ Left err
        (Right fs, _) -> return $ Right fs

-- do
--     ret <- runStateT (runExceptT (normalizeStmt fbody >> normalizeExp fret)) initState
--     let start = posc a1
--     let stop = posc a2
--     case ret of
--         (Left err, _) -> return $ Left err
--         (Right exp, NormState _ ids stmts) -> return $ Right (NFunDec fn fargs (fvars ++ ids) (reverse stmts) exp (MyRange start stop))
--   where
--     posc (AlexPn a l c) = MyPos l c
