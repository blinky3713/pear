module Pear.Language.Lexer.Lexer where

import Data.Functor.Identity
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec as P
import qualified Text.Parsec.Combinator as PC
import Pear.Language.Utils
import Pear.Language.Lexer.Token

type Lexer a = P.ParsecT String () Identity a

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef

annotate :: Lexer Token -> Lexer PositionedToken
annotate p = do
  spos <- P.getPosition
  a <- p
  epos <- P.getPosition
  pure $ PositionedToken spos epos a

parseToken :: Lexer Token
parseToken = P.choice $
    [ P.try $ TIdentifier <$> Token.identifier lexer
    , P.try $ TStringLit <$> Token.stringLiteral lexer
    , P.try $ Token.lexeme lexer parseNatOrFloat
    , P.try $ Token.lexeme lexer parseName
    , P.try $ Token.lexeme lexer $ P.string "[" *> pure LBrace
    , P.try $ Token.lexeme lexer $ P.string "]" *> pure RBrace
    , P.try $ Token.lexeme lexer $ P.string "(" *> pure LParen
    , P.try $ Token.lexeme lexer $ P.string ")" *> pure RParen
    , P.try $ Token.lexeme lexer $ P.string "," *> pure Comma
    , P.try $ Token.lexeme lexer $ P.string "=" *> pure Equals
    , P.try $ Token.lexeme lexer $ P.string "->" *> pure RArrow
    , Token.lexeme lexer $ TSymbol <$> P.many1 (P.satisfy isSymbolChar)
    ]
  where

    isSymbolChar :: Char -> Bool
    isSymbolChar c = c `elem` "-:*+/&<=>\\|%{}"

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
