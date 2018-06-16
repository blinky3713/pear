{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Expr where

import Language.Utils
import Language.Names
import Data.Functor

data Literal a =
    IntLiteral Integer
  | NumericLiteral Double
  | StringLiteral String
  | BooleanLiteral Bool
  | ArrayLiteral [a]
  deriving Functor

deriving instance Eq a => Eq (Literal a)
deriving instance Show a => Show (Literal a)

data Expr a =
    Literal a (Literal (Expr a))
  | UnaryMinus (Expr a)
  | IfThenElse (Expr a) (Expr a) (Expr a)
  | Var a Ident
  | App (Expr a) (Expr a)
  | Abs (Binder a) (Expr a)
  deriving Functor

deriving instance Eq a => Eq (Expr a)
deriving instance Show a => Show (Expr a)

forgetExprAnn :: Expr a -> Expr ()
forgetExprAnn = (<$) ()

type PositionedExpr = Expr SourceSpan
