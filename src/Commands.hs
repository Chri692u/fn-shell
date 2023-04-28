module Commands(exec, load, settings) where

import Control.Monad.Trans
import Control.Monad.State hiding (when)
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
import System.FilePath

-- Options --
settings :: [(String, String -> Repl ())]
settings = [
      ("help", help) -- :help
    , ("quit", quit) -- :quit
    , ("load", load . words) -- :load
    , ("pwd", pwd) -- :pwd
    , ("ls" , ls) -- :ls
    ]

-- Commands -- 
help :: String -> Repl ()
help _ = void $ liftIO (showHelp allCmds)
    where
        showHelp [] = return ()
        showHelp (x:xs) = case x of
            ":help" -> putStrLn (":help -- " ++ "List all commands") >> showHelp xs
            ":quit" -> putStrLn (":quit -- " ++ "Leaves the shell") >> showHelp xs
            ":load" -> putStrLn (":load -- " ++ "Load file(s)") >> showHelp xs
            ":pwd" -> putStrLn (":load -- " ++ "Print working directory") >> showHelp xs
            ":ls" -> putStrLn (":load -- " ++ "List files in current directory") >> showHelp xs
            ":ls D" -> putStrLn (":load -- " ++ "List files in directory") >> showHelp xs

quit :: a -> Repl ()
quit _ = liftIO (putStrLn "Leaving (fn shell).") >> liftIO exitSuccess

load :: [String] -> Repl ()
load args = liftIO $ ifM (checkFiles args) exist (void $ putStrLn "File(s) do not exist")
    where exist = do
            contents <- liftIO $ mapM L.readFile args
            liftIO $ mapM_ (exec . L.unpack) contents

pwd :: String -> Repl ()
pwd _ = do
    st <- get
    let c = cursor st
    void $ liftIO $ putStrLn (dirName c)

lsCurrent :: IState -> Repl ()
lsCurrent st = do
    let c = cursor st
    liftIO $ putStrLn $ dirName c
    forM_ (dirTree c) $ \t ->
        do liftIO $ putStrLn $ content t

ls :: String -> Repl ()
ls path = do
    st <- get
    let c = cursor st
    exists <- liftIO $ search c path
    if exists then do
        forM_ (files c) $ \t -> do
            when (nameFs t == path) $ do
                liftIO $ mapM_ (putStrLn . content) (files $ dirId t)
    else
        if path == "" then do
            lsCurrent st
        else notFound

        where
            files (DirNode name fs) = fs
            dirId (Dir d) = d
            notFound = liftIO $ putStrLn "Directory does not exist"
-- mock execute function
exec = putStrLn