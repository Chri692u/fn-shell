module ShellTypes where

import qualified Data.Map as M
import System.Console.Repline
import Control.Monad.State
import System.FilePath

type Files = [FilePath]
type Subdirs = [FilePath]
-- Types for the file system
data Directory = Dir FilePath Subdirs Files

-- Shell state
data IState = IState { 
        fs :: [Directory],
        cursor :: Directory
    }

-- Shell Type
type Repl a = HaskelineT (StateT IState IO) a

allContent :: Directory -> [FilePath]
allContent (Dir name dirs files) = dirs ++ files

subDirs :: Directory -> [FilePath]
subDirs (Dir _ dirs _) = dirs

dirName :: Directory ->  String
dirName (Dir n _ _) = takeFileName n