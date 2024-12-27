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
    let myid = "t" ++ show (freshvar st)
    lift $ put (st{freshvar = freshvar st + 1, id_list = myid : id_list st})
    return myid


normalizeLExp :: LExp -> Normalize NLexp
normalizeLExp (Ident x _) = return (NIdent x)
normalizeLExp e@(ExprWrite aexp _) = do
    id <- normalizeExp aexp
    case id of
      (NId x) -> return (NIdent x)
      (NUnop ARef (NId x)) -> return (NDerefWrite x)
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
    return (NUnop op e')
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
normalizeExp (VarRef x _) = return (NVarRef x)
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



normalizeStmt :: AStmt -> Normalize [NStmt]
normalizeStmt stm = undefined 
