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

prettyPrintType :: Type -> String
prettyPrintType (TypeVar n) = n
prettyPrintType ts@(TypeConstructor _)
  | ts == tyNumber = "Number"
  | ts == tyString = "String"
  | ts == tyInt = "Int"
  | ts == tyBoolean = "Bool"
prettyPrintType (TypeApp tyFunction (TypeApp a b)) = prettyPrintType a ++ " -> " ++ prettyPrintType b
prettyPrintType (TypeApp (TypeConstructor "Array") a) = "[" ++ prettyPrintType a ++ "]"
