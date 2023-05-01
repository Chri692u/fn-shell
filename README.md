# (fn shell)
A functional shell and scripting language

# Installation
*temporary:* download repo and run 'cabal install' to install dependencies, then run 'cabal run' to start the shell.
*temporary note:* make sure the ShellEnv.ini contains a root path that exists on the machine and a "self" var which is the path to the fn-shell repo. The OS options are 'Win64' or 'UNIX'

## Dependencies
1. The Glasgow Haskell Compiler (GHC)
2. The Haskell Cabal (cabal)

## Starting the shell
1. After installing and starting cabal, the shell will start from the environment defined in ShellEnv.ini.
2. The shell will initialize by constructing the file system and the shell environment in binary files (tree.bin & ShellEnv.bin)
3. The shell will start with the cursor at the root, use :help for a list of commands