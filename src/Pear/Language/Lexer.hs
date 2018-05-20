module Pear.Language.Lexer where

import Data.Functor.Identity
import Text.Parsec.Char (letter, alphaNum)
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec as P
import qualified Text.Parsec.Combinator as PC
import Pear.Language.Utils
import qualified Text.ParserCombinators.Parsec.Language as L

languageDef :: L.LanguageDef st
languageDef =
  L.emptyDef { L.commentStart    = "/*"
             , L.commentEnd      = "*/"
             , L.commentLine     = "//"
             , L.identStart      = letter
             , L.identLetter     = alphaNum
             , L.reservedNames   = [ "if"
                                   , "then"
                                   , "else"
                                   , "true"
                                   , "false"
                                   ]
             , L.reservedOpNames = ["+", "-", "*", "/", ":="
                                   , "<", ">", "&&", "||", "not"
                                   ]
             }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef

data Token =
    TInt Integer
  | TNumber Double
  | TStringLit String
  | TBoolLit Bool
  | TIdentifier String
  | TSymbol String
  | TName String
  | RBrace
  | LBrace
  | Comma
  deriving (Eq, Show)

data PositionedToken =
  PositionedToken { ptStart :: P.SourcePos
                  , ptEnd :: P.SourcePos
                  , ptToken :: Token
                  } deriving (Show)

type Lexer a = P.ParsecT String () Identity a

annotate :: Lexer Token -> Lexer PositionedToken
annotate p = do
  spos <- P.getPosition
  a <- p
  epos <- P.getPosition
  pure $ PositionedToken spos epos a

parseToken :: Lexer Token
parseToken = P.choice $
    [ P.try $ TIdentifier <$> Token.identifier lexer
    , TStringLit <$> Token.stringLiteral lexer
    , Token.lexeme lexer $ TSymbol <$> P.many1 (P.satisfy isSymbolChar)
    , Token.lexeme lexer parseNatOrFloat
    , Token.lexeme lexer parseName
    , P.string "[" *> pure LBrace
    , P.string "]" *> pure RBrace
    , P.string "," *> pure Comma
    ]
  where

    isSymbolChar :: Char -> Bool
    isSymbolChar c = c `elem` "-:*+/&<=>|"

    parseName :: Lexer Token
    parseName = TName <$> ((:) <$> P.lower <*> P.many P.alphaNum)

    parseNatOrFloat :: Lexer Token
    parseNatOrFloat = do
      eRes <- Token.naturalOrFloat lexer
      pure $ case eRes of
        Left n -> TInt n
        Right f -> TNumber f

pearLexer :: String -> Either P.ParseError [PositionedToken]
pearLexer = P.parse (PC.many1 ps) ""
  where
    ps :: Lexer PositionedToken
    ps = annotate parseToken

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
  Comma-> ","

--------------------------------------------------------------------------------
-- | TokenParser
--------------------------------------------------------------------------------

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

identLiteral :: TokenParser String
identLiteral = token go P.<?> "identifier"
  where
    go (TIdentifier s) | s `notElem` (L.reservedNames languageDef) = Just s
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

comma :: TokenParser ()
comma = match Comma
