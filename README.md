# (fn shell)
A functional shell and scripting language built from first principles.

## Installation
*temporary:* download repo and run 'cabal install' to install dependencies, then run 'cabal run' to start the shell.
*temporary note:* make sure the ShellEnv.ini contains a root path that exists on the machine and a "self" var which is the path to the fn-shell repo. The OS options are 'Win64' or 'UNIX'

### Dependencies
1. The Glasgow Haskell Compiler (GHC)
2. The Haskell Cabal (cabal)

### Starting the shell
1. After installing and starting cabal, the shell will start from the environment defined in ShellEnv.ini.
2. The shell will initialize by constructing the file system and the shell environment in binary files (FST.bin & ShellEnv.bin) along with a .history file for the terminal.
3. The shell will start with the cursor at the root, use :help for a list of commands

## The Shell
This will be here when I can be bothered

## The Scripting Language
read above 

# Todo
1. Verify .bin files and parsing work on UNIX
2. Better cursor representation
3. Command parsing and exec function