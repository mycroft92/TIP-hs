module Lib
    ( someFunc
    ) where
import AST.AST
import Parser.Parser

-- run the parser and get the output
someFunc :: IO ()
someFunc = putStrLn "someFunc"
