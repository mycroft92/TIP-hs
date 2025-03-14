{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AST.AST where

import Data.List (intercalate)
import Parser.Lexer (Range (Range))

type TypeVar = Int

-- data Loc  = Loc {start:: (Int, Int), end:: (Int,Int)} deriving (Show, Eq)
data Type = INT | Var !TypeVar | Points !Type | Arrow ![Type] !Type | Rec ![(String, Type)] | Mu !TypeVar !Type | Abs deriving (Eq, Ord)

instance Show Type where
    show INT = "int"
    show (Var v) = "var" ++ show v
    show (Points t) = "&" ++ show t
    show (Arrow args ret) = "(" ++ intercalate "," (map show args) ++ ")" ++ "->" ++ show ret
    show (Rec id_type_list) = "{" ++ intercalate ", " (map (\(x, y) -> x ++ ":" ++ show y) id_type_list) ++ "}"
    show (Mu v t) = "mu " ++ show v ++ "." ++ show t
    show Abs = "◆"

data Operator = APlus | AMinus | ATimes | ADivide | AEqq | ANEq | AGt | AGe | ALe | ALt | ALOr | ALAnd deriving (Eq, Ord)

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
    deriving (Eq)

instance Show AExpr where
    show (Id name _) = name
    show (Binop e1 op e2 _) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"
    show (Unop op e _) = "(" ++ show op ++ show e ++ ")"
    show (Number i _) = show i
    show (Input _) = "input"
    show (Alloc exp _) = "alloc " ++ show exp
    show (VarRef name _) = "&" ++ name
    show (Null _) = "null"
    show (CallExpr nexp args _) = show nexp ++ "(" ++ intercalate "," (map show args) ++ ")"
    show (Record rs _) = show rs ++ "{" ++ intercalate "," (map show rs) ++ "}"
    show (FieldAccess exp fname _) = show exp ++ "." ++ show fname

-- before normalization everything is Indirectwrite
data LExp = Ident !String !Range | ExprWrite !AExpr !Range | IndirectWrite !AExpr !String !Range deriving (Eq)

instance Show LExp where
    show (Ident name _) = name
    show (ExprWrite exp _) = show exp
    show (IndirectWrite exp s _) = show exp ++ "." ++ show s

data AStmt
    = SimpleAssign !LExp !AExpr !Range
    | FieldAssign !LExp !AExpr !Range
    | Output !AExpr !Range
    | Seq !AStmt !AStmt !Range
    | IfStmt !AExpr !AStmt !(Maybe AStmt) !Range
    | WhileStmt !AExpr !AStmt !Range
    | NullStmt !Range
    deriving (Eq)

instance Show AStmt where
    show (SimpleAssign le e _) = show le ++ " = " ++ show e
    show (Output e _) = "output " ++ show e
    show (Seq s1 s2 _) = show s1 ++ ";\n" ++ show s2
    show (IfStmt c e1 (Just e2) _) = "if (" ++ show c ++ ") {" ++ show e1 ++ "} \n else {" ++ show e2 ++ "}"
    show (IfStmt c e1 Nothing _) = "if (" ++ show c ++ ") {" ++ show e1 ++ "}"
    show (WhileStmt c s _) = "while (" ++ show c ++ ") {" ++ show s ++ "}"
    show (NullStmt _) = ""
    show (FieldAssign lexp expr _) = show lexp ++ " = " ++ show expr

data AFuncDec = Fun
    { fname :: String
    , fargs :: [String]
    , fvars :: [String]
    , fbody :: AStmt
    , fret :: AExpr
    , frange :: Range
    }
    deriving (Eq)
instance Show AFuncDec where
    show (Fun fn fa vars body ret _) = show fn ++ "(" ++ intercalate "," (map show fa) ++ ") {" ++ intercalate "," (map show vars) ++ "\n" ++ show body ++ "\nreturn " ++ show ret ++ ";}"
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
    range (IndirectWrite _ _ r) = r
    range (ExprWrite _ r) = r

instance Ranger AStmt where
    range (SimpleAssign _ _ r) = r
    range (FieldAssign _ _ r) = r
    range (Output _ r) = r
    range (Seq _ _ r) = r
    range (IfStmt _ _ _ r) = r
    range (WhileStmt _ _ r) = r
    range (NullStmt r) = r

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
