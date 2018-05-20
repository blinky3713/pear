module Pear.Language.Declaration where

import qualified Text.Parsec as P
import Pear.Language.Expr
import Pear.Language.Lexer
import Pear.Language.Names
import Pear.Language.Utils
import Control.Monad

data ValueDeclarationData a = ValueDeclarationData
  { valdeclSourceAnn :: SourceSpan
  , valdeclIdent :: Ident
  , valdeclBinders :: [Binder]
  , valdeclExpression :: a
  } deriving (Show)

data Declaration =
  ValueDeclaration !(ValueDeclarationData Expr)
  deriving (Show)

parseValueWithIdentAndBinders :: Ident -> [Binder] -> TokenParser (Declaration)
parseValueWithIdentAndBinders ident bs = withSourceSpanF $ do
  equals
  value <- parseValue
  return $ \sa -> ValueDeclaration $ ValueDeclarationData sa ident bs value

parseValueDeclaration :: TokenParser Declaration
parseValueDeclaration = do
  ident <- parseIdent
  binders <- P.many parseBinderNoParens
  parseValueWithIdentAndBinders ident binders

parseDecl :: [PositionedToken] -> Either P.ParseError Declaration
parseDecl = P.parse parseValueDeclaration ""

parseDecl' s = pearLexer s >>= parseDecl
