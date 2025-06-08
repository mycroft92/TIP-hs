module Interpreter.SemanticValues where

import AST.NAST (MyRange)

data FType = FFI | Native deriving (Eq, Ord, Show)
data Value
    = INTVAL Int
    | Fn String Int FType MyRange -- name, arity, type
    | NULL
    | -- | REFVAR String
      REFVAL Int Int -- Reference to location stack size
    | RECVAL [(String, Value)]
    deriving (Show, Eq, Ord)

findField :: String -> [(String, Value)] -> Maybe Value
findField _ [] = Nothing
findField name ((name', v) : fvals)
    | name == name' = Just v
    | otherwise = findField name fvals

changeField :: String -> Value -> Value -> Maybe Value
changeField fname val (RECVAL fields) =
    case findIdx fname fields of
        Just idx ->
            let (fs, ss) = splitAt idx fields
             in Just $ RECVAL (fs ++ (fname, val) : tail ss)
        Nothing -> Nothing
  where
    findIdx fname [] = Nothing
    findIdx fname ls@((f, _) : fs)
        | fname == f = Just (length fields - length ls)
        | otherwise = findIdx fname fs
changeField _ _ _ = Nothing
data InterpreterException
    = Err String
    deriving (Show, Eq)
