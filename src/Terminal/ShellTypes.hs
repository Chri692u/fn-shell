{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Terminal.ShellTypes where

import Control.Monad.State ( StateT, MonadIO(liftIO) )
import System.Console.Repline ( HaskelineT, abort )
import System.FilePath (takeFileName)
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BL

import FileSystem.FST (FileSystemTree, Directory)
import Language.Infer (TypeEnv)

-- Shell state
data IState = IState {
      fs :: FileSystemTree ,
      cursor :: [Directory],
      tyctx :: TypeEnv
    }

-- Shell Type
type Shell a = HaskelineT (StateT IState IO) a

hoistErr :: Show e => Either e a -> Shell a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

-- Shell Vars
data ShellVar = Extern String FilePath

-- Equality Instance
instance Eq ShellVar where
    (Extern name1 path1) == (Extern name2 path2) = nameOk && pathOk
      where pathOk = path1 == path2
            nameOk = name1 == name2

-- Binary Instance
instance B.Binary ShellVar where
  put (Extern n p) = do
    B.put n
    B.put p
  get = Extern <$> B.get <*> B.get

-- Shell Env
data ShellEnv = ShellEnv {
      os     :: String
      , root :: FilePath
      , vars :: [ShellVar]
    }

-- Equality Instance
instance Eq ShellEnv where
    (ShellEnv os1 root1 vars1) == (ShellEnv os2 root2 vars2) = osOk && rootOk && varsOk
      where osOk   = os1 == os2
            rootOk = root1 == root2
            varsOk = vars1 == vars2

-- Show Instance
instance Show ShellEnv where
  show (ShellEnv os r vs) = os ++ ": " ++ r ++ "\nwith vars: " ++ concatMap show' vs ++ "\n"
    where show' (Extern name _) = " " ++ name

-- Binary Instance
instance B.Binary ShellEnv where
  put (ShellEnv os r vs) = do
    B.put os
    B.put r
    B.put vs
  get = ShellEnv <$> B.get <*> B.get <*> B.get

-- Save shell environment
saveSE :: ShellEnv -> FilePath -> IO ()
saveSE se filePath = BL.writeFile filePath (B.encode se)

-- Load shell environment
loadSE :: FilePath -> IO ShellEnv
loadSE = B.decodeFile