module FileSystem.FST where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as B

import System.Directory ( doesDirectoryExist, listDirectory )
import System.FilePath

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

parseFile :: FilePath -> FileInfo
parseFile path = FileInfo path ext hidden
    where
        ext = takeExtension path
        hidden = '.' == head (takeFileName path)

-- Get FST directory node Path
dirPath :: Directory -> FilePath
dirPath (DirNode info _) = path info

-- Return the directory of a FST directory node
fsDir :: FileSystemTree -> Directory
fsDir (Dir dir) = dir

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

-- Check if a path exists in a directory
exists :: Directory -> FilePath -> IO Bool
exists (DirNode info tree) path' = case takeFileName (path info) == path' of
    False -> do
        names <- mapM name tree
        return $ elem path' names
            where
                name (FileNode n) = do
                    return $ takeFileName $ path n
                name (Dir d) = do
                    return (takeFileName $ dirPath d)
    True -> do
        return True

initFST :: FilePath -> IO FileSystemTree
initFST path' = do
    isDir <- doesDirectoryExist path'
    if isDir then
        do
            children <- listDirectory path'
            childTrees <- mapM (initFST . (path' </>)) children
            let info = parseFile path'
            return $ Dir $ DirNode info childTrees
        else
            return $ FileNode $ parseFile path'

saveFST :: FileSystemTree -> FilePath -> IO ()
saveFST fs filePath = BL.writeFile filePath (B.encode fs)

loadFST :: FilePath -> IO FileSystemTree
loadFST = B.decodeFile