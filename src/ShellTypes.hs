module ShellTypes where

import System.Console.Repline
import Control.Monad.State

data FileSystemTree = FileNode FilePath | DirNode FilePath [FileSystemTree]

newtype IState = IState { 
        fs :: FileSystemTree
    }

type Repl a = HaskelineT (StateT IState IO) a