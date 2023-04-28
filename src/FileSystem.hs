module FileSystem(initFs, showFs) where
    
import System.Directory
import System.FilePath
import Control.Monad

import ShellTypes

indent :: Int -> String
indent 0 = ""
indent n = " " ++ indent (pred n) 

content :: FileSystemTree -> [Char]
content (FileNode path) =  "(" ++ takeFileName path ++ ")" ++ " "
content (DirNode name _) = "[" ++ takeFileName  name ++ "]" ++ " "

showFs :: Int -> FileSystemTree -> IO()
showFs indents tree@(FileNode path) = return ()
showFs indents (DirNode name tree) = do
    putStrLn $ indent indents ++ "[" ++ takeFileName name ++ "] -> " ++ concatMap content tree
    forM_ tree $ \t ->
          do showFs (2+indents) t
      

initFs :: FilePath -> IO FileSystemTree
initFs path = do
    isDir <- doesDirectoryExist path
    if isDir then 
        do
            children <- listDirectory path
            childTrees <- mapM (initFs . (path </>)) children
            return $ DirNode path childTrees
        else
            return $ FileNode path