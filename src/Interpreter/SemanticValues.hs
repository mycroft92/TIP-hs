module Interpreter.SemanticValues where

data FType = FFI | Native deriving (Eq, Ord, Show)
data Value
    = INTVAL Int
    | Fn String Int FType -- name, arity, type
    | NULL
    | REFVAL Int -- Reference to location
    | RECVAL [(String, Value)]
    -- record references will be addedd later
    deriving (Show, Eq, Ord)

data InterpreterException
    = Err String
    | ReturnException Value
    deriving (Show, Eq)
