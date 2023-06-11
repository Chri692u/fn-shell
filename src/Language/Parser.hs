{-# LANGUAGE OverloadedStrings #-}

module Language.Parser (parseModule) where

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)
import qualified Text.Parsec.Expr as X
import qualified Text.Parsec.Token as T
import qualified Data.Text.Lazy as L

import Language.Syntax
import Language.Lexer


integer :: Parser Integer
integer = T.integer lexer

variable :: Parser Expr
variable = do
  Var <$> identifier

number :: Parser Expr
number = do
  Lit . LInt . fromIntegral <$> integer

bool :: Parser Expr
bool = (reserved "True" >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

fix :: Parser Expr
fix = do
  reservedOp "fix"
  Fix <$> expr

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many identifier
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  Let x e1 <$> expr

letrecin :: Parser Expr
letrecin = do
  reserved "let"
  reserved "rec"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  Let x e1 <$> expr

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  If cond tr <$> expr

aexp :: Parser Expr
aexp = parens expr
  <|> bool
  <|> number
  <|> ifthen
  <|> fix
  <|> try letrecin
  <|> letin
  <|> lambda
  <|> variable

term :: Parser Expr
term = do
  x <- aexp
  xs <- option [] (many1 (parens expr))
  return (foldl App x xs)

infixOp :: String -> (a -> a -> a) -> X.Assoc -> Op a
infixOp x f = X.Infix (reservedOp x >> return f)

table :: Operators Expr
table = [
    [
      infixOp "*" (Op Mul) X.AssocLeft
    , infixOp "/" (Op Div) X.AssocLeft
    ],
    [
      infixOp "+" (Op Add) X.AssocLeft
    , infixOp "-" (Op Sub) X.AssocLeft
    ],
    [
      infixOp "==" (Op Eql) X.AssocLeft
    ]
  ]

expr :: Parser Expr
expr = X.buildExpressionParser table term

type Binding = (String, Expr)

letdecl :: Parser Binding
letdecl = do
  reserved "let"
  name <- identifier
  args <- many identifier
  reservedOp "="
  body <- expr
  return (name, foldr Lam body args)

letrecdecl :: Parser (String, Expr)
letrecdecl = do
  reserved "let"
  reserved "rec"
  name <- identifier
  args <- many identifier
  reservedOp "="
  body <- expr
  return (name, Fix $ foldr Lam body (name:args))

val :: Parser Binding
val = do
  ex <- expr
  return ("SHELL", ex)

decl :: Parser Binding
decl = try letrecdecl <|> letdecl <|> val

modl ::  Parser [Binding]
modl = many decl

--parseExpr :: L.Text -> Either ParseError Expr
--parseExpr = parse (contents expr) "SCRIPT PARSER ERROR: "

parseModule ::  FilePath -> L.Text -> Either ParseError [(String, Expr)]
parseModule = parse (contents modl)