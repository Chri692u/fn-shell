module Commands(exec, load, settings) where

import Control.Monad.Trans
import Control.Monad.State
import Control.Conditional
import System.Process (callCommand)
import System.Exit (exitSuccess)
import System.Console.Repline
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import ShellTypes
import FileSystem
import ShellUtility (allCmds, checkFiles)
import System.Directory (getCurrentDirectory)

-- Execute Command

-- Options --
settings :: [(String, String -> Repl ())]
settings = [ 
      ("help", help) -- :help
    , ("quit", quit) -- :quit
    , ("load", load . words) -- :load
    , ("pwd", pwd) -- :pwd
    ]

-- Commands -- 
help :: String -> Repl ()
help _ = (liftIO $ showHelp allCmds) >> return ()
    where
        showHelp [] = return ()
        showHelp (x:xs) = case x of
            ":help" -> putStrLn (":help -- " ++ "List all commands") >> showHelp xs
            ":quit" -> putStrLn (":quit -- " ++ "Leaves the shell") >> showHelp xs
            ":load" -> putStrLn (":load -- " ++ "Load file(s)") >> showHelp xs

quit :: a -> Repl ()
quit _ = (liftIO $ putStrLn "Leaving (fn shell).") >> liftIO exitSuccess

load :: [String] -> Repl ()
load args = liftIO $ ifM (checkFiles args) (exist) (putStrLn "File(s) do not exist" >> return ())
    where exist = do
            contents <- liftIO $ mapM L.readFile args
            liftIO $ mapM exec (map L.unpack contents) >> return ()

pwd :: String -> Repl ()
pwd _ = do
    st <- get
    liftIO $ showFs 2 (fs st)
    return ()

-- mock execute function
exec = putStrLn