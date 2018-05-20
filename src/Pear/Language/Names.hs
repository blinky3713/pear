module Pear.Language.Names where

import Pear.Language.Lexer
import Pear.Language.Utils
import qualified Text.Parsec as P

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

data Binder =
    VarBinder SourceSpan Ident
--  | OpBinder SourceSpan OpName
--  | NamedBinder SourceSpan Ident Binder
--  | PositionedBinder SourceSpan Binder
  deriving (Show)

parseVarOrNamedBinder :: TokenParser Binder
parseVarOrNamedBinder = withSourceSpanF $ do
  name <- parseIdent
  return (`VarBinder` name)

parseBinderNoParens  :: TokenParser Binder
parseBinderNoParens = parseVarOrNamedBinder

