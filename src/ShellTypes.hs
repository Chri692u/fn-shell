module ShellTypes where

import System.Console.Repline
import Control.Monad.State
import System.FilePath
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BL

-- Types for the file system

--  File info --
data FileInfo = FileInfo {
        path    :: FilePath
        ,ext    :: String
        ,hidden :: Bool
}

instance Show FileInfo where
    show (FileInfo _ _ True)  = []
    show (FileInfo p e False) = fileStr
        where fileStr = takeFileName p ++ e

instance B.Binary FileInfo where
  put (FileInfo p e h) = do
    B.put p
    B.put e
    B.put h
  get = FileInfo <$> B.get <*> B.get <*> B.get

-- Directory type --
data Directory = DirNode FileInfo [FileSystemTree]

instance Show Directory where
    show (DirNode info tree) = "[" ++ show info ++ "]"

instance B.Binary Directory where
  put (DirNode info tree) = do
    B.put info
    B.put tree
  get = DirNode <$> B.get <*> B.get

-- File System Tree type -- 
data FileSystemTree = FileNode FileInfo | Dir Directory

instance Show FileSystemTree where
    show (FileNode info) = "(" ++ show info ++ ")"
    show (Dir d) = "[" ++ show d ++ "]"

instance B.Binary FileSystemTree where
  put (FileNode info) = B.putWord8 0 >> B.put info
  put (Dir d) = B.putWord8 1 >> B.put d
  get = do
    tag <- B.getWord8
    case tag of
      0 -> FileNode <$> B.get
      1 -> Dir <$> B.get
      _ -> fail "Invalid binary tree tag"

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