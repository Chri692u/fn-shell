module ShellParserHelper(parseFile) where

import System.FilePath
import ShellTypes

parseFile :: FilePath -> FileInfo
parseFile path = FileInfo path ext hidden
    where
        ext = takeExtension path
        hidden = '.' == head (takeFileName path)

