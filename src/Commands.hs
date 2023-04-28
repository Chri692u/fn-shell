module Commands(settings) where

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
help _ = void $ liftIO (showHelp allCmds)
    where
        showHelp [] = return ()
        showHelp (x:xs) = case x of
            ":help" -> putStrLn (":help -- " ++ "List all commands") >> showHelp xs
            ":quit" -> putStrLn (":quit -- " ++ "Leaves the shell") >> showHelp xs
            ":cat" -> putStrLn (":cat F(s) -- " ++ "Print file(s)") >> showHelp xs
            ":pwd" -> putStrLn (":pwd -- " ++ "Print working directory") >> showHelp xs
            ":ls" -> putStrLn (":ls ?D -- " ++ "List files in directory") >> showHelp xs
            ":cd" -> putStrLn (":cd D/.. -- " ++ "Change directory to D or to ../pwd") >> end
                where end = putStrLn "  where ?: optional argument, F: file in directory"

-- Quit the shell
quit :: a -> Repl ()
quit _ = liftIO (putStrLn "Leaving (fn shell).") >> abort

cat :: [String] -> Repl ()
cat args = do
    st <- get
    let c = head $ cursor st
    bools <- liftIO $ mapM (exists c) args
    liftIO $ print bools
    if and bools then do
        contents <- liftIO $ mapM L.readFile args
        liftIO $ mapM_ (putStrLn . L.unpack) contents
    else notFound
        where notFound = liftIO $ putStrLn "Error: File(s) do not exist."

-- Print current directory
pwd :: String -> Repl ()
pwd _ = do
    st <- get
    let c = head $ cursor st
    void $ liftIO $ putStrLn $ "[" ++ dirName c ++ "]"

-- List directory
lsCurrent :: IState -> Repl ()
lsCurrent st = do
    let c = head $ cursor st
    liftIO $ putStrLn $ dirName c
    forM_ (dirTree c) $ \t ->
        do liftIO $ putStrLn $ content t

ls :: String -> Repl ()
ls path = do
    st <- get
    let c = head $ cursor st
    exist <- liftIO $ exists c path
    if exist then do
        forM_ (dirTree c) $ \t -> do
            when (nameFs t == path) $ do
                liftIO $ mapM_ (putStrLn . content) (dirTree $ fsDir t)
    else
        if path == "" then do
            lsCurrent st
        else notFound
            where notFound = liftIO $ putStrLn "Error: Directory does not exist"

-- Change directory
cd :: String -> Repl ()
cd path = do
    st <- get
    let c = head $ cursor st
    exist <- liftIO $ exists c path
    if exist then do
        forM_ (dirTree c) $ \t -> do
            when (nameFs t == path) $ do
                let c' = fsDir t : cursor st
                put (IState (fs st) c')
    else
        if path == ".." then do
            if null (tail $ cursor st) then topOfFs else 
                do put (IState (fs st) (tail $ cursor st))
        else notFound
            where notFound = liftIO $ putStrLn "Error: Directory does not exist"
                  topOfFs  = liftIO $ putStrLn "Error: Top of filestructure already"

-- mock execute function
exec = putStrLn