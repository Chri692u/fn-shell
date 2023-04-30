{-# LANGUAGE LambdaCase #-}
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

-- Options --
settings :: [(String, String -> Shell ())]
settings = [
      ("help", help) -- :help
    , ("quit", quit) -- :quit
    , ("cat", cat . words) -- :cat
    , ("pwd", pwd) -- :pwd
    , ("ls" , ls) -- :ls
    , ("cd" , cd) -- :cd
    ]

-- Commands -- 
help :: String -> Shell ()
help _ = void $ liftIO $ mapM_ putStrLn
            [":help     -- List all commands"
            ,":quit     -- Leaves the shell"
            ,":cat F(s) -- Print file(s)"
            ,":pwd      -- Print working directory"
            ,":ls ?D    -- List files in directory"
            ,":cd D/..  -- Change directory to D or to ../pwd\n  where ?: optional argument, F: file in directory, D: directory"]



-- Quit the shell
quit :: a -> Shell ()
quit _ = liftIO (putStrLn "Leaving (fn shell).") >> abort

cat :: [String] -> Shell ()
cat args = do
    st <- get
    let c = head $ cursor st
    bools <- liftIO $ mapM (exists c) args
    unless (and bools) notFound
    contents <- liftIO $ mapM L.readFile args
    liftIO $ mapM_ (putStrLn . L.unpack) contents
        where notFound = liftIO $ putStrLn "Error: File(s) do not exist."

-- Print current directory
pwd :: String -> Shell ()
pwd _ = do
    st <- get
    let c = head $ cursor st
    void $ liftIO $ putStrLn (dirPath c)

-- List directory
lsCurrent :: IState -> Shell ()
lsCurrent st = do
    let c = head $ cursor st
    forM_ (dirTree c) $ \case
        Dir x -> unless (dirHidden x) $ liftIO $ print x
        FileNode n -> unless (hidden n) $ liftIO $ print n

ls :: String -> Shell ()
ls path = do
    st <- get
    let c = head $ cursor st
    let found = find (\t -> fsName t == path) (dirTree c)
    when (isJust found) $ do
        let dir = fsDir $ fromJust found
        liftIO $ mapM_ print $ dirTree dir
    when (path == "") $ lsCurrent st
    unless (isJust found || path == "") notFound
        where notFound = liftIO $ putStrLn "Error: Directory does not exist"

-- Change directory
cd :: String -> Shell ()
cd path = do
    st <- get
    let c = head $ cursor st
    let found = find (\t -> fsName t == path) (dirTree c)
    when (isJust found) $ do
        let c' = fsDir $ fromJust found
        put (IState (fs st) (c':cursor st))
    when (path == "..") $
        if null (tail $ cursor st) then topOfFs
        else void $ put (IState (fs st) (tail $ cursor st))
    unless (isJust found || path == "..") notFound
        where notFound = liftIO $ putStrLn "Error: Directory does not exist"
              topOfFs  = liftIO $ putStrLn "Error: Top of filestructure already"

-- mock execute function
exec = putStrLn