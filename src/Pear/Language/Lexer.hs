module Pear.Language.Lexer where

import Data.Functor.Identity
import Text.Parsec.Char (letter, alphaNum)
import qualified Text.Parsec.Token as Token
import Text.Parsec as P
import Text.Parsec.Combinator as PC


import Text.ParserCombinators.Parsec.Language as L

languageDef :: LanguageDef st
languageDef =
  emptyDef { L.commentStart    = "/*"
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

data TToken =
    TInt Integer
  | TNumber Double
  | TStringLit String
  | TIdentifier String
  | TOperator TOperator
  | TFlowWord TFlowWord
  deriving (Eq, Show)

data TOperator =
  OAdd | OSub | OMul | ODiv | OAssign | OLT | OGT | OConj | ODisj | ONeg
  deriving (Eq, Show)

parseOperator :: P.ParsecT String u Identity TOperator
parseOperator = Token.lexeme lexer $
  PC.choice [ P.string "+" *> pure OAdd
            , P.string "-" *> pure OSub
            , P.string "*" *> pure OMul
            , P.string "/" *> pure ODiv
            , P.string ":=" *> pure OAssign
            , P.string "<" *> pure OLT
            , P.string ">" *> pure OGT
            , P.string "&&" *> pure OConj
            , P.string "||" *> pure ODisj
            , P.string "not" *> pure ONeg
            ]

data TFlowWord =
  FIf | FThen | FElse | FTrue | FFalse
  deriving (Eq, Show)

parseFlowWord :: P.ParsecT String u Identity TFlowWord
parseFlowWord = Token.lexeme lexer $
  PC.choice [ P.string "if" *> pure FIf
            , P.string "then" *> pure FThen
            , P.string "else" *> pure FElse
            , P.string "true" *> pure FTrue
            , P.string "false" *> pure FFalse
            ]

data SourceSpan =
  SourceSpan { ssStart :: P.SourcePos
             , ssEnd :: P.SourcePos
             } deriving  (Show)

data Annotated a = Annotated SourceSpan a

deriving instance Show a => Show (Annotated a)

type PearToken = Annotated TToken

annotate :: P.ParsecT String u Identity a -> P.ParsecT String u Identity (Annotated a)
annotate p = Token.lexeme lexer $ do
  spos <- P.getPosition
  a <- p
  epos <- P.getPosition
  pure $ Annotated (SourceSpan spos epos) a

identifier :: P.ParsecT String u Identity (Annotated TToken)
identifier = annotate $ fmap TIdentifier (Token.identifier lexer)

int :: P.ParsecT String u Identity (Annotated TToken)
int = annotate $ fmap TInt (Token.integer lexer)

number :: P.ParsecT String u Identity (Annotated TToken)
number = annotate $ fmap TNumber (Token.float lexer)

stringLit :: P.ParsecT String u Identity (Annotated TToken)
stringLit = annotate $ fmap TStringLit (Token.identifier lexer)

op :: P.ParsecT String u Identity (Annotated TToken)
op = annotate $ TOperator <$> parseOperator

flow :: P.ParsecT String u Identity (Annotated TToken)
flow = annotate $ TFlowWord <$> parseFlowWord

pearLexer :: String -> Either ParseError [Annotated TToken]
pearLexer = P.parse (PC.many1 ps) ""
  where
    ps :: P.ParsecT String u Identity (Annotated TToken)
    ps = PC.choice [ identifier
                   , int
                   , number
                   , stringLit
                   , op
                   , flow
                   ]
