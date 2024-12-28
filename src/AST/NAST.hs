module AST.NAST where
--- Normalized AST description
---
import AST.AST (Operator)

data NRecField = RF !String !NExpr deriving (Eq)

instance Show NRecField where
    show (RF x e) = x ++ " : " ++ show e

data NExpr = NId !String 
    | NBinop !NExpr !Operator !NExpr
    | NUnop  !Operator !NExpr
    | NNum   !Int
    | NNull
    | NInput 
    | NAlloc !NExpr
    | NRec ![NRecField] 
    | NFieldAccess !String !String
    deriving (Show, Eq)

data NLexp = NIdent !String | NDerefWrite !String | NDirectWrite !String !String | NIndirectWrite !String !String deriving (Show, Eq)

data Func  = Func !String ![String] deriving (Eq, Show)
data NStmt = 
    NEAssign !NLexp !NExpr
   | NOutput !NExpr
 --  | NSeq !NStmt !NStmt
   | NIfStmt !NExpr ![NStmt] !(Maybe [NStmt])
   | NWhile !NExpr ![NStmt]
   | NRefAssign !NLexp !String
   | NFCAssign !NLexp !Func deriving (Show, Eq)

data NFunDec = NFunDec {
         nfname  :: String,
         nfargs  :: [String],
         nfvars  :: [String],
         nfbody  :: [NStmt],
         nfret   :: NExpr} deriving (Eq, Show)

 
