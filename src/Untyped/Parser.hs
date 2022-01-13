module Untyped.Parser where

import Data.Text (Text)

type Name = Text

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit
  deriving stock (Eq, Show)

data Lit
  = LInt Int
  | LBool Bool
  deriving stock (Eq, Show)
