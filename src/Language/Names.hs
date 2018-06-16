{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.Names where

import Language.Lexer
import Language.Utils
import qualified Text.Parsec as P

data Ident
  = Ident String
--  | GenIdent (Maybe String) Integer
--  | UnusedIdent
  deriving (Show, Eq, Ord)


data Name
  = IdentName Ident
  deriving Eq

-- | Parse an identifier.
parseIdent :: TokenParser Ident
parseIdent = Ident <$> identifier

data Binder a =
    VarBinder a Ident
--  | OpBinder SourceSpan OpName
--  | NamedBinder SourceSpan Ident Binder
--  | PositionedBinder SourceSpan Binder
  deriving Functor

deriving instance Eq a => Eq (Binder a)
deriving instance Show a => Show (Binder a)

parseVarOrNamedBinder :: TokenParser (Binder SourceSpan)
parseVarOrNamedBinder = withSourceSpanF $ do
  name <- parseIdent
  return (`VarBinder` name)

parseBinderNoParens  :: TokenParser (Binder SourceSpan)
parseBinderNoParens = parseVarOrNamedBinder
