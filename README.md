# (fn shell)
A functional shell and scripting language built from first principles.

## Installation
*temporary:* download repo and run 'cabal install' to install dependencies, then run 'cabal run' to start the shell.
*temporary note:* make sure the ShellEnv.ini contains a root path that exists on the machine and a "self" var which is the path to the fn-shell repo. The OS options are 'Win64' or 'UNIX'

### Dependencies
1. The Glasgow Haskell Compiler (GHC)
2. The Haskell Cabal (cabal)

### Starting the shell
1. After installing and starting cabal, the shell will start from the environment defined in ShellEnv.ini (refer to the one in the repo)
2. The shell will initialize by constructing the file system and the shell environment in binary files (FST.bin & ShellEnv.bin) along with a .history file for the terminal.
3. The shell will start with the cursor at the root, use :help for a list of commands

## The Shell
The Shell reads a root directory from a root path, this needs to be a "full" path starting from C:/(Or something other than C) on windows and / on UNIX. Then it creates a tree representation of the underlying file system (an FST), which can be manipulated using the scripting language.

The shell manages a cursor which is a representation of the current working directory. You can change the cursor with cd .. and cd *existing dir visible from cursor*. 

The internal shell commands start with ':' and all other input will be parsed as the scripting language.

## The Scripting Language
This is a simple ML-dialect with variables, lambda abstraction and application, letin bindings, fixpoint operator, binary operator. At the top level a program is defined as a list of declarations and finally an expression. For examples, look in script-examples. Right now it is only a parser and type system (syntax directed HM inference).

# todo
1. Del FST op i 2 filer, 1 til FST og 1 til funktioner
2. lav "Typesystem" i Language og del op i TypeEnv, Constrains og Infer
3. lav constrain solver
4. minimal evaluering
5. rename og flyt det sidste kode