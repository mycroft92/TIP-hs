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

normalizeExp :: AExpr -> Normalize NExpr
normalizeExp e = undefined

normalizeStmt :: AStmt -> Normalize [NStmt]
normalizeStmt stm = undefined 
