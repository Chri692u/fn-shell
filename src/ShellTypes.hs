{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

-- Show Instance
instance Show FileInfo where
    show (FileInfo _ _ True)  = []
    show (FileInfo p e False) = fileStr
        where fileStr = takeFileName p

-- Binary Instance
instance B.Binary FileInfo where
  put (FileInfo p e h) = do
    B.put p
    B.put e
    B.put h
  get = FileInfo <$> B.get <*> B.get <*> B.get

-- Directory type --
data Directory = DirNode FileInfo [FileSystemTree]

-- Show Instance
instance Show Directory where
    show (DirNode info tree) = "[" ++ show info ++ "]"

-- Binary Instance
instance B.Binary Directory where
  put (DirNode info tree) = do
    B.put info
    B.put tree
  get = DirNode <$> B.get <*> B.get

-- File System Tree type -- 
data FileSystemTree = FileNode FileInfo | Dir Directory

-- Show Instance
instance Show FileSystemTree where
    show (FileNode info) = "(" ++ show info ++ ")"
    show (Dir d) = "[" ++ show d ++ "]"

-- Binary Instance
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
type Shell a = HaskelineT (StateT IState IO) a

-- Shell Vars
data ShellVar = Extern String FilePath

-- Equality Instance
instance Eq ShellVar where
    (Extern name1 path1) == (Extern name2 path2) = nameOk && pathOk
      where pathOk = path1 == path2
            nameOk = name1 == name2

-- Binary Instance
instance B.Binary ShellVar where
  put (Extern n p) = do
    B.put n
    B.put p
  get = Extern <$> B.get <*> B.get

-- Shell Env
data ShellEnv = ShellEnv {
      os     :: String
      , root :: FilePath
      , vars :: [ShellVar]
    }

-- Equality Instance
instance Eq ShellEnv where
    (ShellEnv os1 root1 vars1) == (ShellEnv os2 root2 vars2) = osOk && rootOk && varsOk
      where osOk   = os1 == os2
            rootOk = root1 == root2
            varsOk = vars1 == vars2

-- Show Instance
instance Show ShellEnv where
  show (ShellEnv os r vs) = os ++ ": " ++ r ++ "\nwith vars: " ++ concatMap show' vs ++ "\n"
    where show' (Extern name _) = " " ++ name

-- Binary Instance
instance B.Binary ShellEnv where
  put (ShellEnv os r vs) = do
    B.put os
    B.put r
    B.put vs
  get = ShellEnv <$> B.get <*> B.get <*> B.get

-- Save shell environment
saveSE :: ShellEnv -> FilePath -> IO ()
saveSE se filePath = BL.writeFile filePath (B.encode se)

-- Load shell environment
loadSE :: FilePath -> IO ShellEnv
loadSE = B.decodeFile

-- Return the directory of a FST directory node
fsDir :: FileSystemTree -> Directory
fsDir (Dir dir) = dir

-- Get FST directory node Path
dirPath :: Directory -> FilePath
dirPath (DirNode info _) = path info

-- Is the directory hidden?
dirHidden :: Directory -> Bool
dirHidden (DirNode info _) = hidden info

-- Return the directories of a FST directory node
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