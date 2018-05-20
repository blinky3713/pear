module Pear.Language.Names where

import Pear.Language.Lexer

newtype OpName = OpName String deriving Show

data Ident
  = Ident String
--  | GenIdent (Maybe String) Integer
--  | UnusedIdent
  deriving (Show, Eq, Ord)

data Name
  = IdentName Ident
  | ValOpName OpName

-- | Parse an operator.
parseOperator :: TokenParser OpName
parseOperator = OpName <$> symbol

-- | Parse an identifier.
parseIdent :: TokenParser Ident
parseIdent = Ident <$> identifier
