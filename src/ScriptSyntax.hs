module ScriptSyntax where

type Id = String

data Expr
  = Var Id
  | App Expr Expr
  | Lam Id Expr
  | Let Id Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  | Fix Expr
  | Op Binop Expr Expr
  deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Div | Eql
  deriving (Eq, Ord, Show)

type Decl = (Id, Expr)

data Program = Program [Decl] Expr deriving (Show, Eq)