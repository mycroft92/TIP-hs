module Lib where
import AST.AST
import AST.NAST
import AST.Normalization (normalizeFunction)
import Parser.Parser (parse)
import qualified Parser.Lexer as L
import qualified Data.ByteString.Lazy.Char8 as BS

-- run the parser and get the output

runFile :: String -> IO Int
runFile s = do
    contents <- readFile s
    case L.runAlex (BS.pack contents) parse of
        Left err  -> putStr (show err) >> return 1
        Right exp -> do
            -- print exp
            print "Parse finished, normalizing:"
            funcs <- mapM normalizeFunction exp
            print funcs
            return 0
