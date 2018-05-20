module Pear.Language.AST where

import Text.Parsec
import Pear.Language.Names
import Pear.Language.Utils

data Binder =
    VarBinder SourceSpan Ident
  | OpBinder SourceSpan OpName
  | ParensInBinder Binder
  | NamedBinder SourceSpan Ident Binder
  | PositionedBinder SourceSpan Binder
  deriving (Show)

data ValueDeclarationData a = ValueDeclarationData
  { valdeclSourceAnn :: String -- TODO ?
  , valdeclIdent :: Ident
  , valdeclBinders :: [Binder]
  , valdeclExpression :: a
  } deriving (Show)

data Declaration =
  ValueDeclaration
