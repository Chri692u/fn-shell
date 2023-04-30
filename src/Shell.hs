{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Shell(shell) where

import Data.List (isPrefixOf)
import Control.Monad.Trans
import Control.Monad.State
import System.Console.Repline
import System.Directory
import System.FilePath
import FileSystem
import ShellCommands
import ShellTypes
import ShellUtility

initState :: ShellEnv -> IO IState
initState env = do
    let tree = "/tree.bin"
    exist <- doesFileExist (root env ++ tree)
    if exist then do
        fs <- loadFs (root env </> tree)
        cursor <- initCursor fs
        return $ IState fs cursor
    else do
        fs <- initFs $ root env
        cursor <- initCursor fs
        saveFs fs (root env ++ tree)
        return $ IState fs cursor

initCursor :: FileSystemTree -> IO [Directory]
initCursor (Dir d) = return [d]

-- Start the REPL
start :: Shell ()
start = do
  liftIO initMsg

-- Quit the REPL
end :: Shell ExitDecision
end = liftIO (putStrLn "Leaving (fn shell).") >> return Exit

-- Handle input --
cmd :: String -> Shell ()
cmd arg = liftIO $ print arg

-- Tab completion --
complete :: Monad m => WordCompleter m
complete n = return $ filter (isPrefixOf n) allCmds

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter complete) []

-- Top level --
shell :: ShellEnv -> IO ()
shell env = do
    st <- initState env
    flip evalStateT st $ evalRepl symbol cmd settings char paste completer start end
              where
                symbol = const . pure $ ">"
                char = Just ':'
                paste = Just "paste"