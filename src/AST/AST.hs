module AST.AST where

import AST.FreshMonad (TypeVar)
import Parser.Lexer(Range, Range(Range))
--data Loc  = Loc {start:: (Int, Int), end:: (Int,Int)} deriving (Show, Eq)
data Type = INT | Var !TypeVar | Points !Type | Arrow ![Type] !Type | Mu !TypeVar !Type deriving (Show, Eq)

data Operator = APlus | AMinus | ATimes | ADivide | AEqq | ANEq | AGt | AGe | ALe | ALt | ALOr | ALAnd |ADeref deriving (Show, Eq)

data RecField = RecField !String !Expr !Range deriving (Show, Eq) 

data Expr = 
    Id !String !Range 
      | Binop !Expr !Operator !Expr !Range 
      | Unop !Operator !Expr !Range 
      | Number !Int !Range 
      | Input !Range 
      | Alloc !Expr !Range   
      | VarRef !String !Range
      | Null !Range
      | Record ![RecField] !Range
      | RecordField !String !Expr !Range
      | FieldAccess !Expr !String !Range
      -- | DerefExp !Expr !Range
      | CallExpr !Expr ![Expr] !Range deriving (Eq, Show)

class Ranger c where
    range :: c -> Range

instance Ranger RecField where
    range (RecField _ _ r) = r

instance Ranger Expr where
    range (Id _ r) = r
    range (Binop _ _ _ r) = r
    range (Unop _ _ r) = r
    range (Number _ r) = r
    range (Input r) = r
    range (Alloc _ r) = r
    range (VarRef _ r) = r
    range (Null r) = r
    range (Record _ r) = r
    range (RecordField _ _ r) = r
    range (FieldAccess _ _ r) = r
    range (CallExpr _ _ r)    = r
    --range (DerefExp _ r)      = r

(<->) :: (Ranger c , Ranger d) => c -> d -> Range
c <-> d = fromTo (range c) (range d)
    where
        fromTo :: Range -> Range -> Range
        fromTo (Range a1 _) (Range _ b2) = Range a1 b2
