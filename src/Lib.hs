module Lib where

import AST.AST
import AST.NAST
import AST.Normalization (normalizeFunctions)
import Analysis.TypeChecker (TypeState (..), runTypeChecker)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Foldable (foldlM)
import Interpreter.Interpreter (runInterpreter)
import qualified Parser.Lexer as L
import Parser.Parser (parse)

-- run the parser and get the output

runFile :: String -> IO Int
runFile s = do
    contents <- readFile s
    case L.runAlex (BS.pack contents) parse of
        Left err -> putStr (show err) >> return 1
        Right exp -> do
            -- print exp
            putStrLn "Running Type checker on parse output"
            outty <- runTypeChecker exp
            case outty of
                Left err -> do
                    putStrLn err
                    return 1
                Right ts -> do
                    print "Parse finished, normalizing:"
                    funcs <- normalizeFunctions exp
                    -- print funcs
                    case funcs of
                        Left err -> print err >> return 1
                        Right funcs -> do
                            print "running the interpreter"
                            ret <- runInterpreter (reverse funcs)
                            return 0
