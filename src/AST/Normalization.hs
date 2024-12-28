module AST.Normalization where

import AST.NAST
import AST.AST
import Data.Foldable (foldlM, foldrM)
import Control.Monad.State (StateT(runStateT),
      MonadIO(liftIO),
      MonadState(get,put),
      MonadTrans(lift))
import Control.Monad.Except (ExceptT(..), runExceptT, throwError, catchError)
import Control.Monad (forM)
import Parser.TokenTypes (Token(String))
import Control.Exception (bracket)

-- Fresh var list needs to be created for every function
data NormState = NormState {
    freshvar :: Int,
    id_list  :: [String],
    stmts    :: [NStmt]
                           } deriving (Eq, Show)

type Normalize a = ExceptT String (StateT NormState IO) a

initState :: NormState
initState = NormState 0 [] []

addStmt :: NStmt -> Normalize ()
addStmt x = do
    state <- lift get
    lift $ put (state { stmts = x:stmts state})

addAssignStmt :: String -> NExpr -> Normalize ()
addAssignStmt id e = addStmt (NEAssign (NIdent id) e) 


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
      _ -> throwError $ "Normalization failed for Lexp: " ++ show  e
normalizeLExp e@(DirectWrite rn fn _) = return (NDirectWrite rn fn)
normalizeLExp e@(IndirectWrite (Id rn _) fn _) = return (NDirectWrite rn fn)
normalizeLExp e@(IndirectWrite exp fn _) = do
    id <- normalizeExp exp
    case id of
      (NId x) -> return (NIndirectWrite x fn)
      _       -> throwError $ "Normalization failed for Lexp: " ++ show e

normalizeRecField :: RecField -> Normalize NRecField
normalizeRecField (RecField x e _ ) = undefined

normalizeExp :: AExpr -> Normalize NExpr
normalizeExp (Id s _ ) = return (NId s)
normalizeExp (Binop e1 op e2 _) = do
    e1' <- normalizeExp e1
    e2' <- normalizeExp e2
    return (NBinop e1' op e2')
normalizeExp (Unop op e _) = do
    e' <- normalizeExp e
    case op of
      -- Normalize the dereferencing so that we have lexps only in forms t, *t
      ATimes -> do
          new <- newid
          addAssignStmt new e'
          return (NUnop op (NId new))
      _ -> return (NUnop op e')
normalizeExp (Number x _) = return (NNum x)
normalizeExp (Input _)    = return (NInput)
normalizeExp (Record xs _) = do
        es <- mapM normalizeRecField xs
        return (NRec es)
normalizeExp (Null _) = return NNull
normalizeExp (FieldAccess (Id x _) fn _) = return (NFieldAccess x fn)
normalizeExp e@(FieldAccess (Record xs _) fn _) = 
    -- xs' <- mapM normalizeRecField xs
    case findRecField xs fn of
      Just e  -> normalizeExp e
      Nothing -> throwError $ "No field : "++fn ++" in record expr: " ++ show e
    where
        findRecField :: [RecField] -> String -> Maybe AExpr
        findRecField [] _ = Nothing
        findRecField ((RecField n' e _ ):xs) n 
                    | n' == n = Just e
                    | otherwise = findRecField xs n
normalizeExp (FieldAccess e fn _) = do
    e' <- normalizeExp e
    new <- newid
    addAssignStmt new e'
    return (NFieldAccess new fn)
--references come only at equation level    
normalizeExp (VarRef x _) = do
    new <- newid
    addStmt (NRefAssign (NIdent new) x)
    return (NId x)

normalizeExp (Alloc e _)  = do
    e' <- normalizeExp e
    return (NAlloc e')
normalizeExp (CallExpr fe args _) = do
    fn'   <- caseNormExp fe
    args' <- mapM caseNormExp args
    new   <- newid
    addStmt (NFCAssign (NIdent new) (Func fn' args'))
    return (NId new)
    where
        caseNormExp :: AExpr -> Normalize String
        caseNormExp e = do
            e' <- normalizeExp e
            case e' of
              (NId x) -> return x
              _       -> throwError $ "cannot normalize expr (function call) : "++show e ++ " normalized to :" ++ show e'

normalizeStmt :: AStmt -> Normalize ()
normalizeStmt (SimpleAssign le e _) = do
    le' <- normalizeLExp le
    e'  <- normalizeExp e
    addStmt (NEAssign le' e')
    
     
normalizeStmt (FieldAssign le e _) = do
   le' <- normalizeLExp le
   e'  <- normalizeExp e
   case le' of
     (NDirectWrite _ _) -> addStmt (NEAssign le' e')
     (NIndirectWrite _ _) -> addStmt (NEAssign le' e')
     _ -> throwError $ "Normalization failed, expected record type for lexp: " ++ show le ++ " got: "++ show le'

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
 
handleBlock :: AStmt -> Normalize [NStmt]
handleBlock block = do
    st <- lift get
    -- reset state before running stmts
    lift $ put (NormState {freshvar = freshvar st, id_list = [], stmts = []})
    normalizeStmt block
    blst <- lift get
    let idlist = id_list st ++ id_list blst
    let fv = freshvar blst
    let block' = reverse (stmts blst)
    -- put state back, new statement must be handled by top scope
    lift $ put (NormState {freshvar = fv, id_list = idlist, stmts = stmts st})
    return block'


normalizeFunction :: AFuncDec -> IO (Either String NFunDec)
normalizeFunction (Fun fn fargs fvars fbody fret _) = do
    ret <- runStateT (runExceptT (normalizeStmt fbody >> normalizeExp fret)) initState
    case ret of
      (Left err, _) -> return $ Left err
      (Right exp, NormState _ ids stmts) -> return $ Right (NFunDec fn fargs (fvars++ids) (reverse stmts) exp) 

    
