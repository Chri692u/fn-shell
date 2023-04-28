module Shell(shell) where

import Data.List (isPrefixOf)
import Control.Monad.Trans
import Control.Monad.State
import System.Console.Repline
import System.Directory
import FileSystem
import Commands
import ShellTypes
import ShellUtility

initState :: IO IState
initState = do
    root <- getCurrentDirectory
    fs <- initFs root
    cursor <- initCursor fs
    return $ IState fs cursor

initCursor :: FileSystemTree -> IO [Directory]
initCursor (Dir d) = return [d]
            
-- Start the REPL
start :: Repl ()
start = do
  liftIO initMsg

-- Quit the REPL
end :: Repl ExitDecision
end = liftIO (putStrLn "Leaving (fn shell).") >> return Exit

-- Handle input --
cmd :: String -> Repl ()
cmd arg = liftIO $ print arg

-- Tab completion --
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load" , fileCompleter)
  ]

complete :: Monad m => WordCompleter m
complete n = return $ filter (isPrefixOf n) allCmds

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter complete) defaultMatcher

-- Top level --
shell ::  IO ()
shell = do
    st <- initState
    flip evalStateT st $ evalRepl symbol cmd settings char paste completer start end
              where
                symbol = const . pure $ ">"
                char = Just ':'
                paste = Just "paste"