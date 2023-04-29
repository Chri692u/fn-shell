module FileSystem(initFs, nameFs, content, exists) where


import System.Directory ( doesDirectoryExist, listDirectory )
import System.FilePath
import ShellTypes
import ShellParserHelper

-- Pretty print an element in the file system
content :: FileSystemTree -> String
content (FileNode info) =  "(" ++ path info ++ ")" ++ " "
content (Dir d) = "[" ++ takeFileName  (dirPath d) ++ "]" ++ " "

--- Get the base name of a directory
nameFs :: FileSystemTree ->  String
nameFs (Dir d) = takeFileName $ dirPath d
nameFs (FileNode info) = takeFileName $ path info

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

initFs :: FilePath -> IO FileSystemTree
initFs path' = do
    isDir <- doesDirectoryExist path'
    if isDir then
        do
            children <- listDirectory path'
            childTrees <- mapM (initFs . (path' </>)) children
            let info = parseFile path'
            return $ Dir $ DirNode info childTrees
        else
            return $ FileNode $ parseFile path'