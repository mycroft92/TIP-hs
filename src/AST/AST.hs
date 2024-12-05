module AST.AST where

import AST.FreshMonad (TypeVar)
data Type = INT | Var !TypeVar | Points !Type | Arrow ![Type] !Type | Mu !TypeVar !Type deriving (Show, Eq)

data Operator = APlus | AMinus | ATimes | ADivide | AEqq | AGt | ALt | ADeref deriving (Show, Eq)

--data Expr = 

