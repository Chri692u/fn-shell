module Main where

import Control.Conditional
import System.Environment
import System.FilePath
import System.Directory

import Shell
import ShellTypes
import ShellParserHelper
import System.Directory (doesFileExist)

isUnchanged :: ShellEnv -> FilePath -> IO Bool
isUnchanged env bpath = do
    let bEnv = "ShellEnv.bin"
    exist <- doesFileExist (bpath </> bEnv)
    if exist then do
        env' <- loadSE (bpath </> bEnv)
        return $ env == env'
    else
        return False

main :: IO ()
main = do
    let ini = "ShellEnv.ini"
    shellDir <- getCurrentDirectory
    input <- readFile ini
    case parseEnv input of
        Left err -> print err
        Right env -> do
            putStrLn "Starting (fn shell)..."
            print env
            exists <- doesDirectoryExist $ root env
            envOk <- isUnchanged env shellDir
            unless exists notFound
            unless envOk $ do
                putStrLn "New or changed .ini file"
                let binEnv = shellDir </> "ShellEnv.bin"
                    binFST = shellDir </> "tree.bin"
                existFST <- doesFileExist binFST
                when existFST $ do
                    putStrLn "Removing old FST"
                    removeFile binFST
                saveSE env $ shellDir </> binEnv
                shell env
            when (envOk && exists) $ shell env
            where notFound = putStrLn nfError
                  nfError = "Root directory in .ini file does not exist."