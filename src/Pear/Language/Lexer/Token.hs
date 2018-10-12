module Pear.Language.Lexer.Token where

import Data.Functor.Identity (Identity)
import qualified Text.Parsec as P
import qualified Text.ParserCombinators.Parsec.Language as L
import Text.Parsec.Char (letter, alphaNum)

--------------------------------------------------------------------------------
-- | Language Definition for Lexer
--------------------------------------------------------------------------------

languageDef :: L.LanguageDef st
languageDef =
  L.emptyDef { L.commentStart    = "{-"
             , L.commentEnd      = "-}"
             , L.commentLine     = "--"
             , L.identStart      = letter
             , L.identLetter     = alphaNum
             , L.reservedNames   = [ "if"
                                   , "then"
                                   , "else"
                                   , "let"
                                   , "in"
                                   , "true"
                                   , "false"
                                   ]
             }

--------------------------------------------------------------------------------
-- | Token
--------------------------------------------------------------------------------

data Token =
    TInt Integer
  | TNumber Double
  | TStringLit String
  | TBoolLit Bool
  -- an identifier is anything which is not a symbol or a name
  | TIdentifier String
  -- symbols are used for infix operators
  | TSymbol String
  -- TODO: why are names needed
  | TName String
  | RBrace
  | LBrace
  | RParen
  | LParen
  | Comma
  | Equals
  | RArrow
  deriving (Eq, Show)

data PositionedToken =
  PositionedToken { ptStart :: P.SourcePos
                  , ptEnd :: P.SourcePos
                  , ptToken :: Token
                  } deriving (Show)

prettyPrintToken :: Token -> String
prettyPrintToken t = case t of
  TInt n -> show n
  TNumber n -> show n
  TStringLit s -> s
  TBoolLit b -> show b
  TIdentifier n -> n
  TSymbol s -> s
  TName n -> n
  RBrace -> "["
  LBrace -> "]"
  Comma -> ","
  Equals -> "="
  RArrow -> "->"

type TokenParser a = P.ParsecT [PositionedToken] () Identity a

token :: (Token -> Maybe a) -> TokenParser a
token f = P.token (prettyPrintToken . ptToken) ptStart (f . ptToken)

match :: Token -> TokenParser ()
match tok = token (\tok' -> if tok == tok' then Just () else Nothing) P.<?> (prettyPrintToken tok)

stringLiteral :: TokenParser String
stringLiteral = token go P.<?> "string literal"
  where
    go (TStringLit s) = Just s
    go _ = Nothing

intLiteral :: TokenParser Integer
intLiteral = token go P.<?> "int literal"
  where
    go (TInt n) = Just n
    go _ = Nothing

numberLiteral :: TokenParser Double
numberLiteral = token go P.<?> "number literal"
  where
    go (TNumber n) = Just n
    go _ = Nothing

boolLiteral :: TokenParser Bool
boolLiteral = token go P.<?> "bool literal"
  where
    go (TBoolLit b) = Just b
    go _ = Nothing

identifier :: TokenParser String
identifier = token go P.<?> "identifier"
  where
    go (TIdentifier s) | s `notElem` L.reservedNames languageDef = Just s
    go _ = Nothing

symbol :: TokenParser String
symbol = token go P.<?> "symbol"
  where
    go (TSymbol s) = Just s
    go _ = Nothing

symbol' :: String -> TokenParser ()
symbol' s = token go P.<?> show s
  where
  go (TSymbol s') | s == s'   = Just ()
  go _ = Nothing

reserved :: String -> TokenParser ()
reserved s = token go P.<?> show s
  where
  go (TName s') | s == s' = Just ()
  go (TSymbol s') | s == s' = Just ()
  go _ = Nothing

squares :: TokenParser a -> TokenParser a
squares = P.between lbrace rbrace
  where
    lbrace = match LBrace
    rbrace = match RBrace

parens :: TokenParser a -> TokenParser a
parens = P.between lparen rparen
  where
    lparen = match LParen
    rparen = match RParen

comma :: TokenParser ()
comma = match Comma

rarrow :: TokenParser ()
rarrow = match RArrow

equals :: TokenParser ()
equals = match Equals
