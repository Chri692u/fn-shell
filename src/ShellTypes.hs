module ShellTypes where

import System.Console.Repline
import Control.Monad.State

data Directory = DirNode FilePath [FileSystemTree]
data FileSystemTree = FileNode FilePath | Dir Directory

data IState = IState { 
        fs :: FileSystemTree ,
        cursor :: Directory
    }

type Repl a = HaskelineT (StateT IState IO) a