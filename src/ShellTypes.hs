module ShellTypes where

import System.Console.Repline
import Control.Monad.State

-- Types for the file system
data Directory = DirNode FilePath [FileSystemTree]
data FileSystemTree = FileNode FilePath | Dir Directory

-- Shell state
data IState = IState { 
        fs :: FileSystemTree ,
        cursor :: [Directory]
    }

-- Shell Type
type Repl a = HaskelineT (StateT IState IO) a

-- Get the directory from the file system
fsDir :: FileSystemTree -> Directory
fsDir (Dir dir) = dir

-- Get directory name
dirName :: Directory -> FilePath
dirName (DirNode name _) = name

-- Get the list of files in a directory
dirTree :: Directory -> [FileSystemTree]
dirTree (DirNode _ fs) = fs  