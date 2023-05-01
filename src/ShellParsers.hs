module ShellParsers(parseFile, parseEnv) where

import Text.ParserCombinators.Parsec

import System.FilePath
import ShellTypes
import Control.Conditional (when)


-- File path parsing
unixFullPath :: Parser FilePath
unixFullPath = do
    string "root" >> spaces >> char '=' >> spaces
    char '/'
    rel <- many1 $ letter <|> digit <|> char '/'
    return $ "/" ++ rel

winFullPath :: Parser FilePath
winFullPath = do
    string "root" >> spaces >> char '=' >> spaces
    pathStart  <- many1 letter `endBy` char ':'
    char '\\'
    rel <- many1 $ letter <|> digit <|> char '\\'
    newline
    let start = concat pathStart
    return $ start ++ ":\\" ++ rel

-- Shell variable parsing
externParser :: Char -> Parser ShellVar
externParser ch = do
    name <- many1 letter
    spaces >> char '=' >> spaces
    exPath <- many1 $ letter <|> digit <|> char ch
    return (Extern name exPath)

-- Parse shell environment
envParser :: Parser ShellEnv
envParser = do
    string "OS" >> spaces >> char '=' >> spaces
    os <- many1 $ letter <|> digit
    newline
    if os == "Win64" then do
        path <- winFullPath
        string "vars" >> char ':'
        newline
        vars <- many1 $ externParser '\\'
        return $ ShellEnv os path vars
    else  do
    -- Default to UNIX
        path <- unixFullPath
        string "vars" >> char ':'
        newline
        vars <- many1 $ externParser '/'
        return $ ShellEnv os path vars

-- Top level
parseFile :: FilePath -> FileInfo
parseFile path = FileInfo path ext hidden
    where
        ext = takeExtension path
        hidden = '.' == head (takeFileName path)

parseEnv :: String -> Either ParseError ShellEnv
parseEnv = parse envParser "ENV PARSE ERROR: "