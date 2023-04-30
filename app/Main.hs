module Main where

import System.Environment
import System.Directory

import Shell
import ShellTypes
import ShellParserHelper

main :: IO ()
main = do
    let ini = "ShellEnv.ini"
    input <- readFile ini
    case parseEnv input of
        Left err -> print err
        Right env -> do
            exists <- doesDirectoryExist $ root env
            if exists then shell env 
            else putStrLn "Root directory in .ini file does not exist."