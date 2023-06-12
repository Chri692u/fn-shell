{-# LANGUAGE LambdaCase #-}
module Terminal.Commands(settings, exec) where

import Control.Monad.Trans
import Control.Monad.State hiding (when, unless)
import Control.Conditional
import System.Process (callCommand)
import System.Exit (exitSuccess)
import System.Console.Repline
import System.Directory (getCurrentDirectory)
import Data.Foldable (find)
import Data.Maybe
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Terminal.ShellTypes
import Terminal.Utility
import FileSystem.FST
import Language.Parser
import Language.Infer

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
            ,":cd D/..  -- Change directory to D or to ../pwd\n    where ?: optional argument, F: file in directory, D: directory"]

-- Quit the shell
quit :: a -> Shell ()
quit _ = liftIO (putStrLn "Leaving (fn shell).") >> abort

-- Print content of file(s) to screen
cat :: [String] -> Shell ()
cat args = do
    st <- get
    let c = head $ cursor st
    bools <- liftIO $ mapM (exists c) args
    unless (and bools) notFound
    contents <- liftIO $ mapM L.readFile args
    liftIO $ mapM_ (putStrLn . L.unpack) contents
        where notFound = liftIO $ putStrLn "Error: File(s) do not exist."

-- Print the current directory
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

-- Ls a directory visible to the cursor
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
    let tenv = tyctx st
    let c = head $ cursor st
    let found = find (\t -> fsName t == path) (dirTree c)
    when (isJust found) $ do
        let c' = fsDir $ fromJust found
        put $ IState (fs st) (c':cursor st) tenv
    when (path == "..") $
        if null (tail $ cursor st) then topOfFs
        else void $ put $ IState (fs st) (tail $ cursor st) tenv
    unless (isJust found || path == "..") notFound
        where notFound = liftIO $ putStrLn "Error: Directory does not exist"
              topOfFs  = liftIO $ putStrLn "Error: Top of filestructure already"

-- Show output from shell
showOutput :: String -> IState -> Shell ()
showOutput arg st = do
  case judgement (tyctx st) "SHELL" of
    Just val -> liftIO $ print val
    Nothing -> return ()

-- Execute script function
exec :: Bool -> L.Text -> Shell ()
exec update input = do
    st <- get
    let fst = fs st
        c = cursor st
    ast <- hoistErr $ parseModule "Error parsing script: " input
    liftIO $ print ast
    tyctx' <- hoistErr $ inferTop (tyctx st) ast
    let st' = st 
            { fs = fst
            , cursor = c
            , tyctx = tyctx' <> tyctx st
            }
            
    when update (put st')
    
    
    scheme <- hoistErr $ inferExpr (tyctx st') (snd $ head ast)
    liftIO $ print scheme
    liftIO $ print $ tyctx st'
            