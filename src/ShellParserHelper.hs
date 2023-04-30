module ShellParserHelper(parseFile, parseEnv) where

import Text.ParserCombinators.Parsec

import System.FilePath
import ShellTypes

svarParser :: Parser ShellVar
svarParser = do
    name <- many1 letter
    spaces >> char '=' >> spaces
    externPath <- many1 $ letter <|> digit <|> char '\\' <|> char ':' <|> char '-'
    return $ Extern name externPath

envParser :: Parser ShellEnv
envParser = do
    string "root" >> spaces >> char '=' >> spaces
    rootPath <- many1 $ letter <|> digit <|> char '\\' <|> char ':' <|> char '-'
    newline
    string "vars" >> char ':'
    newline
    vars <- many1 svarParser
    eof
    return $ ShellEnv rootPath vars

-- Top level
parseFile :: FilePath -> FileInfo
parseFile path = FileInfo path ext hidden
    where
        ext = takeExtension path
        hidden = '.' == head (takeFileName path)


parseEnv = parse envParser "<ENV PARSE ERROR>"