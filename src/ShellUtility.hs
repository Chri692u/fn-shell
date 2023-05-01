module ShellUtility where

import System.Directory
import ShellTypes (IState)

initMsg :: IO ()
initMsg = do 
    putStrLn "                                  "
    putStrLn ".--------------------------------."
    putStrLn "|   (fn shell) version 0.1.0.0   |"
    putStrLn ".--------------------------------."
    putStrLn ":help - a list of all features  "
    putStrLn "Ctrl + D will quit the shell    "

allCmds :: [String]
allCmds = [":help", ":quit", ":cat", ":pwd", ":ls", ":cd"]