{-# LANGUAGE DeriveFunctor #-}

module Language.Expr where

import Language.Utils
import Language.Names

data Literal a =
    IntLiteral Integer
  | NumericLiteral Double
  | StringLiteral String
  | BooleanLiteral Bool
  | ArrayLiteral [a]
  deriving Functor


data Expr a =
    Literal a (Literal (Expr a))
  | UnaryMinus (Expr a)
  | IfThenElse (Expr a) (Expr a) (Expr a)
  | Var a Ident
  | App (Expr a) (Expr a)
  | Abs Binder (Expr a)

type PositionedExpr = Expr SourceSpan
