module FileSystem(initFs, nameFs, dirTree, dirName, content, search) where
    
import System.Directory
import System.FilePath
import Control.Monad

import ShellTypes

indent :: Int -> String
indent 0 = ""
indent n = " " ++ indent (pred n) 

dirName :: Directory -> FilePath
dirName (DirNode name _) = name

dirTree :: Directory -> [FileSystemTree]
dirTree (DirNode _ fs) = fs  

content :: FileSystemTree -> String
content (FileNode path) =  "(" ++ takeFileName path ++ ")" ++ " "
content (Dir d) = "[" ++ takeFileName  (dirName d) ++ "]" ++ " "

nameFs :: FileSystemTree ->  String
nameFs (Dir d) = takeFileName $ dirName d
nameFs (FileNode n) = n

search :: Directory -> FilePath -> IO Bool
search (DirNode n tree) path = case takeFileName n == path of
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