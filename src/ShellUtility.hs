module ShellUtility where

import System.Directory
import ShellTypes (IState)

initMsg :: IO ()
initMsg = do 
    putStrLn "(fn shell) version 0.1.0.0"
    putStrLn ":help - a list of all features"
    putStrLn "Ctrl + D will quit the shell"

allCmds :: [String]
allCmds = [":help", ":quit", ":cat", ":pwd", ":ls", ":cd"]

checkFiles fs = do
    checks <- mapM doesFileExist fs
    return $ foldr (&&) True checks