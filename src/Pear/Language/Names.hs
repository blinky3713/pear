module Pear.Language.Names where

newtype OpName = OpName String deriving Show

data Ident
  = Ident String
  | GenIdent (Maybe String) Integer
  | UnusedIdent
  deriving (Show, Eq, Ord)

data Name
  = IdentName Ident
  | ValOpName OpName
