module FileSystem(createDir, exists) where
    
import System.Directory ( doesDirectoryExist, listDirectory )
import System.FilePath
import ShellTypes

-- Check if a path exists in a directory
exists :: Directory -> FilePath -> IO Bool
exists dir@(Dir n f d) path = return $ takeFileName n `elem` allContent dir

-- Initialize the file system
createDir :: FilePath -> IO Directory
createDir path = do
    isDir <- doesDirectoryExist path
    if isDir then do
        list <- listDirectory path
        let files = filter (elem '.') list
        let dirs = filter (notElem '.') list
        return $ Dir path dirs files
    else
        return $ Dir path [] []