module Pear.Language.Expr where

import Pear.Language.Lexer
import Pear.Language.Utils
import Text.Parsec as P
import Text.Parsec.Expr as E


data Literal a =
    IntLiteral Integer
  | NumericLiteral Double
  | StringLiteral String
  | BooleanLiteral Bool
  | ArrayLiteral [a]
  deriving (Show)


parseIntLiteral :: TokenParser (Literal a)
parseIntLiteral = IntLiteral <$> intLiteral

parseNumericLiteral :: TokenParser (Literal a)
parseNumericLiteral = NumericLiteral <$> numberLiteral

parseStringLiteral :: TokenParser (Literal a)
parseStringLiteral = StringLiteral <$> stringLiteral

parseBooleanLiteral :: TokenParser (Literal a)
parseBooleanLiteral = BooleanLiteral <$> boolLiteral

parseArrayLiteral :: TokenParser a -> TokenParser (Literal a)
parseArrayLiteral p = ArrayLiteral <$> squares (P.sepBy p comma)

newtype OpName = OpName String deriving Show

data Expr =
    Literal SourceSpan (Literal Expr)
  | UnaryMinus SourceSpan Expr
  | BinaryNoParens Expr Expr Expr
  | Op SourceSpan OpName
  | IfThenElse Expr Expr Expr
  deriving (Show)

parseIfThenElse :: TokenParser Expr
parseIfThenElse = do
  reserved "if"
  c <- parseValue
  reserved "then"
  t <- parseValue
  reserved "else"
  e <- parseValue
  pure $ IfThenElse c t e

parseValueAtom :: TokenParser Expr
parseValueAtom = P.choice
  [ withSourceSpan Literal $ parseIntLiteral
  , withSourceSpan Literal $ parseNumericLiteral
  , withSourceSpan Literal $ parseNumericLiteral
  , withSourceSpan Literal $ parseStringLiteral
  , withSourceSpan Literal $ parseBooleanLiteral
  , withSourceSpan Literal $ parseArrayLiteral parseValue
  , parseIfThenElse
  ]

-- | Parse an expression in backticks or an operator
parseInfixExpr :: TokenParser Expr
parseInfixExpr = withSourceSpan Op parseOperator
  where
    parseOperator :: TokenParser OpName
    parseOperator = OpName <$> symbol

-- | Parse an expression
parseValue :: TokenParser Expr
parseValue =
    E.buildExpressionParser operators parseValueAtom P.<?> "expression"
  where
    operators = [ [ E.Prefix (withSourceSpan (\ss _ -> UnaryMinus ss) (symbol' "-"))
                  ]
                , [ E.Infix (P.try (parseInfixExpr P.<?> "infix expression") >>= \ident ->
                      return (BinaryNoParens ident)) E.AssocRight
                  ]
                ]

parseExpr :: [PositionedToken] -> Either ParseError Expr
parseExpr = P.parse parseValue ""


parseExpr' s = pearLexer s >>= parseExpr

--------------------------------------------------------------------------------

-- | Read source position information
withSourceSpan
  :: (SourceSpan -> a -> b)
  -> P.Parsec [PositionedToken] u a
  -> P.Parsec [PositionedToken] u b
withSourceSpan f p = do
  start <- P.getPosition
  x <- p
  end <- P.getPosition
  let sp = SourceSpan start end
  return $ f sp x
