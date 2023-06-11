module Language.Lexer where

import Text.Parsec
import Text.Parsec.Text.Lazy
import Data.Functor.Identity
import qualified Data.Text.Lazy as L
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Expr as X

type Op a = X.Operator L.Text () Identity a
type Operators a = X.OperatorTable L.Text () Identity a

-- List of reserved names in the language
reservedNames :: [String]
reservedNames = [
    "let",
    "in",
    "fix",
    "rec",
    "if",
    "then",
    "else"
  ]

-- List of reserved operators
reservedOps :: [String]
reservedOps = [
    "->",
    "\\",
    "*",
    "/",
    "+",
    "-",
    "="
  ]

-- Parsec language
lexer :: T.GenTokenParser L.Text () Identity
lexer = T.makeTokenParser $ T.LanguageDef
  {
    T.commentStart    = "{-"
    , T.commentEnd      = "-}"
    , T.commentLine     = "--"
    , T.nestedComments  = True
    , T.identStart      = letter
    , T.identLetter     = alphaNum <|> oneOf "_'"
    , T.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , T.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , T.reservedNames   = reservedNames
    , T.reservedOpNames = reservedOps
    , T.caseSensitive   = True
  }

-- Parsec definitions
reserved :: String -> Parser ()
reserved = T.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

identifier :: Parser String
identifier = T.identifier lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

semi :: Parser String
semi = T.semi lexer

-- content parser
contents :: Parser a -> Parser a
contents p = do
  T.whiteSpace lexer
  r <- p
  eof
  return r