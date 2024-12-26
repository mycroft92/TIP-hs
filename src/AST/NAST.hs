module AST.NAST where
--- Normalized AST description
---
import AST.AST (Operator)

data NRecField = RF !String !NExpr deriving (Show, Eq)

data NExpr = NId !String 
    | NBinop !NExpr !Operator !NExpr
    | NUnop  !Operator !NExpr
    | NNum   !Int
    | NNull
    | NInput 
    | NRec ![NRecField] 
    | NFieldAccess !String !String
    | NVarRef !String deriving (Show, Eq)

data NLexp = NIdent !String | NDerefWrite !String | NDirectWrite !String !String | NIndirectWrite !String !String deriving (Show, Eq)

data Func  = Func !String ![String] deriving (Eq, Show)
data NStmt = 
    NEAssign !NLexp !NExpr
   | NOutput !NExpr
 --  | NSeq !NStmt !NStmt
   | NIfStmt !NExpr ![NStmt] !(Maybe [NStmt])
   | NWhile !NExpr ![NStmt] 
   | NFCAssign !NLexp !Func deriving (Show, Eq)

data NFunDec = NFunDec {
         nfname  :: String,
         nfargs  :: [String],
         nfvars  :: [String],
         nfbody  :: [NStmt],
         nfret   :: NExpr} deriving (Eq, Show)

 
