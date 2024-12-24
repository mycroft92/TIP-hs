module AST.NAST where
--- Normalized AST description
---
import AST.FreshMonad
import AST.AST (Operator)

data NRecField = RF !String !NExpr deriving (Show, Eq)

data NExpr = NId !String 
    | NBinop !NExpr !Operator !NExpr
    | NUnop  !Operator !NExpr
    | NNum   !Int
    | NNull
    | NRec ![NRecField] 
    | NFieldAccess !String !String
    | NVarRef !String deriving (Show, Eq)
