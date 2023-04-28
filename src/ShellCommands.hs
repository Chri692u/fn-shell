module ShellCommands(settings, exec) where

import Control.Monad.Trans
import Control.Monad.State hiding (when, unless)
import Control.Conditional
import System.Process (callCommand)
import System.Exit (exitSuccess)
import System.Console.Repline
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import ShellTypes
import FileSystem
import ShellUtility
import System.Directory (getCurrentDirectory)
import Data.Foldable (find)
import Data.Maybe
import System.FilePath (takeFileName)
-- Options --
settings :: [(String, String -> Repl ())]
settings = [
      ("help", help) -- :help
    , ("quit", quit) -- :quit
    , ("cat", cat . words) -- :cat
    , ("pwd", pwd) -- :pwd
    , ("ls" , ls) -- :ls
    , ("cd" , cd) -- :cd
    ]

-- Commands -- 
help :: String -> Repl ()
help _ = void $ liftIO $ mapM_ putStrLn 
            [":help -- List all commands"
            ,":quit -- Leaves the shell"
            ,":cat F(s) -- Print file(s)"
            ,":pwd -- Print working directory"
            ,":ls ?D -- List files in directory"
            ,":cd D/.. -- Change directory to D or to ../pwd\n  where ?: optional argument, F: file in directory"]



-- Quit the shell
quit :: a -> Repl ()
quit _ = liftIO (putStrLn "Leaving (fn shell).") >> abort

cat :: [String] -> Repl ()
cat args = do
    st <- get
    let c = cursor st
    bools <- liftIO $ mapM (exists c) args
    unless (and bools) notFound
    contents <- liftIO $ mapM L.readFile args
    liftIO $ mapM_ (putStrLn . L.unpack) contents
        where notFound = liftIO $ putStrLn "Error: File(s) do not exist."

-- Print current directory
pwd :: String -> Repl ()
pwd _ = do
    st <- get
    let c = cursor st
    void $ liftIO $ putStrLn $ "[" ++ dirName c ++ "]"

-- List directory
lsCurrent :: IState -> Repl ()
lsCurrent st = do
    let c = cursor st
    liftIO $ mapM_ putStrLn $ allContent c

ls :: String -> Repl ()
ls path = do
    st <- get
    let c = cursor st
    let found = find (\d -> takeFileName d == path) $ subDirs c
    when (isJust found) $ do
        let path' = fromJust found
        dir <- liftIO $ createDir path'
        liftIO $ mapM_ putStrLn $ allContent dir
    when (path == "") $ lsCurrent st
    unless (isJust found || path == "") notFound
        where notFound = liftIO $ putStrLn "Error: Directory does not exist"

-- Change directory
cd :: String -> Repl ()
cd path = do
    st <- get
    let c = cursor st
    let found = find (\d -> takeFileName d == path) $ subDirs c
    when (isJust found) $ do
        let path' = fromJust found
        dir <- liftIO $ createDir path'
        put (IState (dir:fs st) dir)
    when (path == "..") $
            if null (tail $ fs st) then topOfFs
            else void $ put (IState (tail $ fs st) (head $ tail $ fs st))
    unless (isJust found || path == "..") notFound
        where notFound = liftIO $ putStrLn "Error: Directory does not exist"
              topOfFs  = liftIO $ putStrLn "Error: Top of filestructure already"

-- mock execute function
exec = putStrLn