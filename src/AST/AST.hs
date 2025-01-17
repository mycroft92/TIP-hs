module AST.AST where

import Data.List (intercalate)
import Parser.Lexer (Range (Range))

type TypeVar = Int

-- data Loc  = Loc {start:: (Int, Int), end:: (Int,Int)} deriving (Show, Eq)
data Type = INT | Var !TypeVar | Points !Type | Arrow ![Type] !Type | Mu !TypeVar !Type deriving (Eq, Ord)

instance Show Type where
    show INT = "int"
    show (Var v) = "t" ++ show v
    show (Points t) = "&" ++ show t
    show (Arrow args ret) = "(" ++ intercalate "," (map show args) ++ ")" ++ "->" ++ show ret
    show (Mu v t) = "mu " ++ show v ++ "." ++ show t

data Operator = APlus | AMinus | ATimes | ADivide | AEqq | ANEq | AGt | AGe | ALe | ALt | ALOr | ALAnd deriving (Eq)

data RecField = RecField !String !AExpr !Range deriving (Eq)

-- write a show Instance for RecField with id : expr.

instance Show RecField where
    show (RecField id expr r) = id ++ " : " ++ show expr ++ " @" ++ show r

data AExpr
    = Id !String !Range
    | Binop !AExpr !Operator !AExpr !Range
    | Unop !Operator !AExpr !Range
    | Number !Int !Range
    | Input !Range
    | Alloc !AExpr !Range
    | VarRef !String !Range
    | Null !Range
    | Record ![RecField] !Range
    | -- | RecordField !String !AExpr !Range
      FieldAccess !AExpr !String !Range
    | -- | DerefExp !AExpr !Range
      CallExpr !AExpr ![AExpr] !Range
    deriving (Eq, Show)

data LExp = Ident !String !Range | ExprWrite !AExpr !Range | DirectWrite !String !String !Range | IndirectWrite !AExpr !String !Range deriving (Show, Eq)
data AAssign = AAssign !LExp !AExpr !Range deriving (Eq, Show)

data AStmt
    = SimpleAssign !LExp !AExpr !Range
    | FieldAssign !LExp !AExpr !Range
    | Output !AExpr !Range
    | Seq !AStmt !AStmt !Range
    | IfStmt !AExpr !AStmt !(Maybe AStmt) !Range
    | WhileStmt !AExpr !AStmt !Range
    deriving (Eq, Show)

data AFuncDec = Fun
    { fname :: String
    , fargs :: [String]
    , fvars :: [String]
    , fbody :: AStmt
    , fret :: AExpr
    , frange :: Range
    }
    deriving (Eq, Show)

class Ranger c where
    range :: c -> Range

instance Ranger RecField where
    range (RecField _ _ r) = r

instance Ranger AExpr where
    range (Id _ r) = r
    range (Binop _ _ _ r) = r
    range (Unop _ _ r) = r
    range (Number _ r) = r
    range (Input r) = r
    range (Alloc _ r) = r
    range (VarRef _ r) = r
    range (Null r) = r
    range (Record _ r) = r
    -- range (RecordField _ _ r) = r
    range (FieldAccess _ _ r) = r
    range (CallExpr _ _ r) = r

-- range (DerefExp _ r)      = r
instance Ranger LExp where
    range (Ident _ r) = r
    range (DirectWrite _ _ r) = r
    range (IndirectWrite _ _ r) = r
    range (ExprWrite _ r) = r

instance Ranger AStmt where
    range (SimpleAssign _ _ r) = r
    range (FieldAssign _ _ r) = r
    range (Output _ r) = r
    range (Seq _ _ r) = r
    range (IfStmt _ _ _ r) = r
    range (WhileStmt _ _ r) = r

instance Ranger AFuncDec where
    range (Fun _ _ _ _ _ r) = r

(<->) :: (Ranger c, Ranger d) => c -> d -> Range
c <-> d = fromTo (range c) (range d)
  where
    fromTo :: Range -> Range -> Range
    fromTo (Range a1 _) (Range _ b2) = Range a1 b2

instance Show Operator where
    show APlus = "+"
    show AMinus = "-"
    show ATimes = "*"
    show ADivide = "/"
    show AEqq = "=="
    show ANEq = "!="
    show AGt = ">"
    show AGe = ">="
    show ALe = "<="
    show ALt = "<"
    show ALOr = "||"
    show ALAnd = "&&"
