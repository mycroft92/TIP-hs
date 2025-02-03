module Interpreter.SemanticValues where

data FType = FFI | Native deriving (Eq, Ord, Show)
data Value
    = INT_ Int
    | Fn String Int FType -- name, arity, type
    | NULL
    | Ref Int -- Reference to location
    -- record references will be addedd later
    deriving (Show, Eq, Ord)
