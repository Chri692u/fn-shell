module FileSystem(initFST, saveFST, loadFST, exists) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as B

import System.Directory ( doesDirectoryExist, listDirectory )
import System.FilePath
import Terminal.ShellTypes
import Terminal.ConfigParser

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