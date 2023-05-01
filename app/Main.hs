module Main where

import Control.Conditional
import System.Environment
import System.FilePath
import System.Directory

import Shell
import ShellTypes
import ShellParserHelper

-- Check if ShellEnv.ini was changed
isUnchanged :: ShellEnv -> FilePath -> IO Bool
isUnchanged env bpath = do
    let bEnv = "ShellEnv.bin"
    exist <- doesFileExist (bpath </> bEnv)
    if exist then do
        env' <- loadSE (bpath </> bEnv)
        return $ env == env'
    else
        return False

-- Update .ini binary and remove old FST binary
updateAndRun :: ShellEnv -> FilePath -> FilePath -> IO ()
updateAndRun env binEnv binFST = do
    putStrLn "New or changed .ini file"
    existFST <- doesFileExist binFST
    when existFST $ do
        putStrLn "Removing old FST"
        removeFile binFST
    saveSE env binEnv
    shell env

-- Top level
main :: IO ()
main = do
    shellDir <- getCurrentDirectory
    let binEnv = shellDir </> "ShellEnv.bin"
        binFST = shellDir </> "FST.bin"
        ini    = "ShellEnv.ini"

    input <- readFile ini
    case parseEnv input of
        Left err -> print err
        Right env -> do
            putStrLn "Starting (fn shell)..."
            print env
            exists <- doesDirectoryExist $ root env
            envOk <- isUnchanged env shellDir
            unless exists notFound
            unless envOk $ updateAndRun env binEnv binFST
            when (envOk && exists) $ shell env
            where notFound = putStrLn nfError
                  nfError = "Root directory in .ini file does not exist."