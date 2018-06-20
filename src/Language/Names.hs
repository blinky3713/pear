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

