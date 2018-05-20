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
  | Var SourceSpan Ident
  | App Expr Expr
  | Abs Binder Expr
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

parseAbs :: TokenParser Expr
parseAbs = do
    symbol' "\\"
    args <- P.many1 (Abs <$> parseBinderNoParens)
    rarrow
    value <- parseValue
    return $ toFunction args value
  where
    toFunction :: [Expr -> Expr] -> Expr -> Expr
    toFunction args value = foldr ($) value args

parseIdentifierAndValue :: TokenParser Expr
parseIdentifierAndValue = do
  (ss, name) <- withSourceSpan (,) identifier
  return (Var ss (Ident name))

parseValueAtom :: TokenParser Expr
parseValueAtom = P.choice
  [ P.try parseIfThenElse
  , parseAbs
  , parseIdentifierAndValue
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
    E.buildExpressionParser operators (buildPostfixParser postfixTable indexersAndAccessors) P.<?> "expression"
  where
    postfixTable = [ \v -> P.try (flip App <$> indexersAndAccessors) <*> pure v]
    operators = [ [ E.Prefix (withSourceSpan (\ss _ -> UnaryMinus ss) (symbol' "-"))
                  ]
                , [ E.Infix (P.try (parseInfixExpr P.<?> "infix expression") >>= \ident ->
                      return (BinaryNoParens ident)) E.AssocRight
                  ]
                ]
indexersAndAccessors :: TokenParser Expr
indexersAndAccessors = buildPostfixParser [] parseValueAtom

parseExpr :: [PositionedToken] -> Either ParseError Expr
parseExpr = P.parse parseValue ""

parseExpr' s = pearLexer s >>= parseExpr

--------------------------------------------------------------------------------
