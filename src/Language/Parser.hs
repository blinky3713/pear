module Language.Parser where

import Language.Lexer
import Language.Expr
import Language.Names
import Language.Utils
import Text.Parsec as P
import Text.Parsec.Expr as E

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

parseIfThenElse :: TokenParser PositionedExpr
parseIfThenElse = do
  reserved "if"
  c <- parseValue
  reserved "then"
  t <- parseValue
  reserved "else"
  e <- parseValue
  pure $ IfThenElse c t e

parseAbs :: TokenParser PositionedExpr
parseAbs = do
    symbol' "\\"
    args <- P.many1 (Abs <$> parseBinderNoParens)
    rarrow
    value <- parseValue
    return $ toFunction args value
  where
    toFunction :: [Expr a -> Expr a] -> Expr a -> Expr a
    toFunction args value = foldr ($) value args

parseIdentifierAndValue :: TokenParser PositionedExpr
parseIdentifierAndValue = do
  (ss, name) <- withSourceSpan (,) identifier
  return $ Var ss (Ident name)

parseValueAtom :: TokenParser PositionedExpr
parseValueAtom = P.choice
  [ P.try parseIfThenElse
  , parens parseValueAtom
  , parseAbs
  , parseIdentifierAndValue
  , withSourceSpan Literal $ parseArrayLiteral parseValue
  , withSourceSpan Literal $ parseIntLiteral
  , withSourceSpan Literal $ parseNumericLiteral
  , P.try $ withSourceSpan Literal $ parseBooleanLiteral
  , withSourceSpan Literal $ parseStringLiteral
  ]

-- | Parse an expression
parseValue :: TokenParser PositionedExpr
parseValue =
    E.buildExpressionParser operators (buildPostfixParser postfixTable indexersAndAccessors) P.<?> "expression"
  where
    postfixTable = [ \v -> P.try (flip App <$> indexersAndAccessors) <*> pure v]
    operators = [ [ E.Prefix (withSourceSpan (\ss _ -> UnaryMinus) (symbol' "-"))
                  ]
                ]
indexersAndAccessors :: TokenParser PositionedExpr
indexersAndAccessors = buildPostfixParser [] parseValueAtom

parseExpr :: [Positioned Token] -> Either ParseError PositionedExpr
parseExpr = P.parse parseValue ""

parseExprFromStr :: String -> Either ParseError PositionedExpr
parseExprFromStr s = pearLexer s >>= parseExpr

--------------------------------------------------------------------------------

parseVarOrNamedBinder :: TokenParser (Binder SourceSpan)
parseVarOrNamedBinder = withSourceSpanF $ do
  name <- parseIdent
  return (`BVar` name)

parseBinderNoParens  :: TokenParser (Binder SourceSpan)
parseBinderNoParens = parseVarOrNamedBinder

