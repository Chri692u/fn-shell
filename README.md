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
The Shell reads a root directory from a root path, this needs to be a "full" path starting from C:/(Or something other than C) on windows and / on UNIX. Then it creates a tree representation of the underlying file system (an FST), which can be manipulated using the scripting language.

The shell manages a cursor which is a representation of the current working directory. You can change the cursor with cd .. and cd *existing dir visible from cursor*. 

The internal shell commands start with ':' and all other input will be parsed as the scripting language (*note:* the scripting language is only a parser for the moment).

## The Scripting Language
*temporary note:* This will change when the type system is added.

The meta variables used for the abstract syntax:
-x   ::= identifier
-i   ::= any integer constant
-b   ::= true | false
-lit ::= i | b 

The abstract syntax for the scripting language:

- Program P:
    ```
    P ::= {D₁, D₂, ..., Dₙ} e
    ```

- Declaration D:
    ```
    D ::= x = e
    ```

- Expression e:
    ```
    e ::= x                   (variable)
        | e₁ e₂               (application)
        | λx.e                (abstraction)
        | let x = e₁ in e₂     (let-binding)
        | l                   (literal)
        | if e₁ then e₂ else e₃ (conditional expression)
        | fix e               (fixpoint expression)
        | e₁ ⊙ e₂             (binary operation)
    ```
    where `⊙ ::= + | - | * | / | ==`

# Current new ideas
1. Better cursor representation
2. Type system for the language
3. Constructs to manipulate the FST