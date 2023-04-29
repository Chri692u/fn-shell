module ShellTypes where

import System.Console.Repline
import Control.Monad.State

-- Types for the file system
data FileInfo = FileInfo {
        path    :: FilePath
        ,ext    :: String
        ,hidden :: Bool
    } deriving (Eq, Show)

data Directory = DirNode FileInfo [FileSystemTree]
data FileSystemTree = FileNode FileInfo | Dir Directory

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

dirTree :: Directory -> [FileSystemTree]
dirTree (DirNode _ fs) = fs  