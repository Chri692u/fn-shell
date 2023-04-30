module FileSystem(initFs, saveFs, loadFs, exists) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as B

import System.Directory ( doesDirectoryExist, listDirectory )
import System.FilePath
import ShellTypes
import ShellParserHelper

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

saveFs :: FileSystemTree -> FilePath -> IO ()
saveFs fs filePath = BL.writeFile filePath (B.encode fs)

loadFs :: FilePath -> IO FileSystemTree
loadFs = B.decodeFile