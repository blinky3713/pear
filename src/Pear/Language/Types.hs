module Pear.Language.Types where

data Type =
    TypeVar String
  | TypeApp Type Type
  | TypeConstructor String
  deriving (Show, Eq)

tyBoolean :: Type
tyBoolean = TypeConstructor "Boolean"

tyString :: Type
tyString = TypeConstructor "String"

tyInt :: Type
tyInt = TypeConstructor "Int"

tyNumber :: Type
tyNumber = TypeConstructor "Number"

tyArray :: Type
tyArray = TypeConstructor "Array"

tyFunction :: Type
tyFunction = TypeConstructor "Function"
