module Interpreter.SemanticValues where

data FType = FFI | Native deriving (Eq, Ord, Show)
data Value
    = INTVAL Int
    | Fn String Int FType -- name, arity, type
    | NULL
    | REFVAL Int -- Reference to location
    | RECVAL [(String, Value)]
    deriving (Show, Eq, Ord)

findField :: String -> [(String, Value)] -> Maybe Value
findField _ [] = Nothing
findField name ((name', v) : fvals)
    | name == name' = Just v
    | otherwise = findField name fvals

data InterpreterException
    = Err String
    | ReturnException Value
    deriving (Show, Eq)
