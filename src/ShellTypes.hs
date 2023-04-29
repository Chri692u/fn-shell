module ShellTypes where

import System.Console.Repline
import Control.Monad.State
import System.FilePath

-- Types for the file system
data FileInfo = FileInfo {
        path    :: FilePath
        ,ext    :: String
        ,hidden :: Bool
}

instance Show FileInfo where
    show (FileInfo _ _ True)  = []
    show (FileInfo p e False) = fileStr
        where fileStr = takeFileName p ++ e

data Directory = DirNode FileInfo [FileSystemTree]
instance Show Directory where
    show (DirNode info tree) = "(" ++ show info ++ ")"

data FileSystemTree = FileNode FileInfo | Dir Directory
instance Show FileSystemTree where
    show (FileNode info) = "(" ++ show info ++ ")"
    show (Dir d) = "[" ++ show d ++ "]"

-- Shell state
data IState = IState { 
        fs :: FileSystemTree ,
        cursor :: [Directory]
    }

-- Shell Type
type Repl a = HaskelineT (StateT IState IO) a

fsDir :: FileSystemTree -> Directory
fsDir (Dir dir) = dir

dirPath :: Directory -> FilePath
dirPath (DirNode info _) = path info

dirHidden :: Directory -> Bool
dirHidden (DirNode info _) = hidden info

dirTree :: Directory -> [FileSystemTree]
dirTree (DirNode _ fs) = fs  

--- Get the base name of a directory
fsName :: FileSystemTree ->  String
fsName (Dir d) = takeFileName $ dirPath d
fsName (FileNode info) = takeFileName $ path info

-- Pretty print an element in the file system
content :: FileSystemTree -> String
content (FileNode info) =  "(" ++ path info ++ ")" ++ " "
content (Dir d) = "[" ++ takeFileName  (dirPath d) ++ "]" ++ " "