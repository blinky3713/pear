module Pear.Language.Expr where

import Pear.Language.Lexer
import Pear.Language.Utils
import Text.Parsec as P
import Text.Parsec.Expr as E
import Pear.Language.Names

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
parseBooleanLiteral =
  (reserved "true" *> pure (BooleanLiteral True)) P.<|> (reserved "false" *> pure (BooleanLiteral False))

parseArrayLiteral :: TokenParser a -> TokenParser (Literal a)
parseArrayLiteral p = ArrayLiteral <$> squares (P.sepBy p comma)

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
  [ P.try parseIfThenElse
  , withSourceSpan Literal $ parseArrayLiteral parseValue
  , withSourceSpan Literal $ parseIntLiteral
  , withSourceSpan Literal $ parseNumericLiteral
  , P.try $ withSourceSpan Literal $ parseBooleanLiteral
  , withSourceSpan Literal $ parseStringLiteral
  ]

-- | Parse an expression in backticks or an operator
parseInfixExpr :: TokenParser Expr
parseInfixExpr = withSourceSpan Op parseOperator

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
