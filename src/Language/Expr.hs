{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Expr where

import Language.Utils
import Language.Names
import Data.Functor
import Language.Types
import Data.Monoid ((<>))

data Literal a =
    IntLiteral Integer
  | NumericLiteral Double
  | StringLiteral String
  | BooleanLiteral Bool
  | ArrayLiteral [a]
  deriving Functor

deriving instance Eq a => Eq (Literal a)
deriving instance Show a => Show (Literal a)

-- TODO: For now, only BVar can be parsed
data Binder a =
    BVar a Ident
  | BWildcard a
  | BLit (Literal a)
  deriving Functor

deriving instance Eq a => Eq (Binder a)
deriving instance Show a => Show (Binder a)

type BindGroup a = ([Expl a], [[Impl a]])
type Expl a = (Ident, Scheme, [Alt a])
type Impl a = (Ident, [Alt a])
type Alt a = ([Binder a], Expr a)

data Expr a =
    Literal a (Literal (Expr a))
  | UnaryMinus (Expr a)
  | IfThenElse (Expr a) (Expr a) (Expr a)
  | Var a Ident
  | App (Expr a) (Expr a)
  | Abs (Binder a) (Expr a)
  | Let (BindGroup a) (Expr a)
  deriving Functor

deriving instance Eq a => Eq (Expr a)
deriving instance Show a => Show (Expr a)

forgetExprAnn :: Expr a -> Expr ()
forgetExprAnn = (<$) ()

-- TODO make a comonad
extractAnn :: Monoid a => Expr a -> a
extractAnn (Literal a b) = a
extractAnn (UnaryMinus a) = extractAnn a
extractAnn (IfThenElse i t e) = extractAnn i <> extractAnn t <> extractAnn e
extractAnn (App e1 e2) = extractAnn e1 <> extractAnn e2
extractAnn (Abs b e) = extractAnn e
extractAnn (Let bg e) = extractAnn e

type PositionedExpr = Expr SourceSpan
