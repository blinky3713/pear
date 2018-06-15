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

-- | Parse an identifier.
parseIdent :: TokenParser Ident
parseIdent = Ident <$> identifier

data Binder =
    VarBinder SourceSpan Ident
--  | OpBinder SourceSpan OpName
--  | NamedBinder SourceSpan Ident Binder
--  | PositionedBinder SourceSpan Binder

parseVarOrNamedBinder :: TokenParser Binder
parseVarOrNamedBinder = withSourceSpanF $ do
  name <- parseIdent
  return (`VarBinder` name)

parseBinderNoParens  :: TokenParser Binder
parseBinderNoParens = parseVarOrNamedBinder
