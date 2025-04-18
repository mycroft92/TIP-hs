module AST.NAST where

--- Normalized AST description

---
import AST.AST (Operator)
import Data.List (intercalate)

data MyPos = MyPos {line :: Int, col :: Int} deriving (Show, Ord, Eq)
data MyRange = MyRange {start :: MyPos, stop :: MyPos} deriving (Show, Ord, Eq)
data NRecField = RF !String !NExpr deriving (Eq, Ord)

instance Show NRecField where
    show (RF x e) = x ++ " : " ++ show e

data NExpr
    = NId !String
    | NBinop !NExpr !Operator !NExpr
    | NUnop !Operator !NExpr
    | NNum !Int
    | NNull
    | NInput
    | NAlloc !NExpr
    | NRec ![NRecField]
    | NFieldAccess !String !String
    deriving (Eq, Ord)

instance Show NExpr where
    show (NId x) = x
    show (NBinop e1 op e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"
    show (NUnop op e) = show op ++ show e
    show (NNum n) = show n
    show NNull = "null"
    show NInput = "input"
    show (NAlloc e) = "alloc " ++ show e
    show (NRec fields) = "{" ++ unwords (map show fields) ++ "}"
    show (NFieldAccess r f) = r ++ "." ++ f

data NLexp = NIdent !String | NDerefWrite !String | NDirectWrite !String !String deriving (Eq)

instance Show NLexp where
    show (NIdent s) = s
    show (NDerefWrite s) = "*" ++ s
    show (NDirectWrite r f) = r ++ "." ++ f

-- show (NIndirectWrite r f) = r ++ "." ++ f

data Func = Func !String ![String] deriving (Eq, Show)
data NStmt
    = NEAssign !NLexp !NExpr
    | NOutput !NExpr
    | NIfStmt !NExpr ![NStmt] !(Maybe [NStmt])
    | NWhile !NExpr ![NStmt]
    | NRefAssign !NLexp !String
    | NFCAssign !NLexp !Func
    deriving (Eq)

instance Show NStmt where
    show (NEAssign lhs rhs) = show lhs ++ " := " ++ show rhs
    show (NOutput expr) = "output " ++ show expr
    show (NIfStmt cond thenStmts elseStmts) =
        " if ("
            ++ show cond
            ++ ") "
            ++ showStmts thenStmts
            ++ case elseStmts of
                Nothing -> ""
                Just stmts -> " else " ++ showStmts stmts
    show (NWhile cond stmts) =
        " while (" ++ show cond ++ ") " ++ showStmts stmts
    show (NRefAssign lhs ref) = show lhs ++ " := &" ++ ref
    show (NFCAssign lhs (Func name args)) =
        show lhs ++ " := " ++ name ++ "(" ++ unwords (map show args) ++ ")"

showStmts :: [NStmt] -> String
showStmts stmts = "{" ++ intercalate ";\n  " (map show stmts) ++ "}"

data NFunDec = NFunDec
    { nfname :: String
    , nfargs :: [String]
    , nfvars :: [String]
    , nfbody :: [NStmt]
    , nfret :: NExpr
    , frange :: MyRange -- needed to keep the list of functions unique
    }
    deriving (Eq)

instance Show NFunDec where
    show (NFunDec name args vars body ret _) =
        name
            ++ "("
            ++ intercalate ", " args
            ++ ") "
            ++ "{\n"
            ++ " vars "
            ++ intercalate ", " vars
            ++ ";\n "
            ++ intercalate "; \n" (map show body)
            ++ ";\n return "
            ++ show ret
            ++ "}"

-- write a function to display NFunDec with proper nesting in nested If blocks.

showNFunDec :: NFunDec -> String
showNFunDec (NFunDec name args vars body ret _) =
    name
        ++ "("
        ++ intercalate ", " args
        ++ ") {\n"
        ++ "  vars "
        ++ intercalate ", " vars
        ++ ";\n"
        ++ showNStmts 1 body
        ++ "  return "
        ++ show ret
        ++ "\n}"

showNStmts :: Int -> [NStmt] -> String
showNStmts indent stmts = concatMap (showNStmt indent) stmts

showNStmt :: Int -> NStmt -> String
showNStmt indent stmt = case stmt of
    NIfStmt cond thenStmts elseStmts ->
        indentation
            ++ "if ("
            ++ show cond
            ++ ") {\n"
            ++ showNStmts (indent + 1) thenStmts
            ++ indentation
            ++ "}"
            ++ maybe "" (\stmts -> " else {\n" ++ showNStmts (indent + 1) stmts ++ indentation ++ "}") elseStmts
            ++ "\n"
    NWhile cond stmts ->
        indentation
            ++ "while ("
            ++ show cond
            ++ ") {\n"
            ++ showNStmts (indent + 1) stmts
            ++ indentation
            ++ "}\n"
    _ -> indentation ++ show stmt ++ ";\n"
  where
    indentation = replicate (2 * indent) ' '
