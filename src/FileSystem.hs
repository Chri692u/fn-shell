module FileSystem(initFs, nameFs, content, exists) where
    
import System.Directory
import System.FilePath
import Control.Monad

import ShellTypes

-- Pretty print an element in the file system
content :: FileSystemTree -> String
content (FileNode path) =  "(" ++ takeFileName path ++ ")" ++ " "
content (Dir d) = "[" ++ takeFileName  (dirName d) ++ "]" ++ " "

--- Get the base name of a directory
nameFs :: FileSystemTree ->  String
nameFs (Dir d) = takeFileName $ dirName d
nameFs (FileNode n) = takeFileName n

-- Check if a path exists in a directory
exists :: Directory -> FilePath -> IO Bool
exists (DirNode n tree) path = case takeFileName n == path of
    False -> do
        names <- mapM name tree
        return $ elem path names
            where
                name (FileNode n) = do
                    return n
                name (Dir d) = do
                    return (takeFileName $ dirName d)
    True -> do
        return True

-- Initialize the file system
initFs :: FilePath -> IO FileSystemTree
initFs path = do
    isDir <- doesDirectoryExist path
    if isDir then 
        do
            children <- listDirectory path
            childTrees <- mapM (initFs . (path </>)) children
            return $ Dir $ DirNode path childTrees
        else
            return $ FileNode path