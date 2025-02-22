module Main (main) where

import Lib (runFile)

import System.Environment
import System.IO
import qualified System.Exit as Exit

banner :: String
banner = "\
\################################################\n\
\             TIP Analysis framework            \n\
\              Author: Madhukar Y.R            \n\
\################################################"

main :: IO ()
main = do
        putStrLn banner
        args <- getArgs
        progName <- getProgName
        putStrLn  $ "Running: "++ progName
        if null args
            then (do
                putStrLn "Usage: tip <file>"
                -- hSetBuffering stdin NoBuffering
                hSetBuffering stdout NoBuffering
                -- runPrompt
                Exit.exitSuccess)
            else (do
                putStrLn $ "Running program "++ head args
                x <- runFile $ head args
                if x == 0 
                   then Exit.exitSuccess
                   else Exit.exitFailure)
