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

newtype OpName = OpName String

data Expr =
    Literal SourceSpan (Literal Expr)
  | UnaryMinus SourceSpan Expr
  | BinaryNoParens Expr Expr Expr
  | Op SourceSpan OpName
  | IfThenElse Expr Expr Expr
{-
parseValueAtom :: TokenParser Expr
parseValueAtom = withSourceSpan PositionedValue $ P.choice
                 [ parseAnonymousArgument
                 , withSourceSpan' Literal $ parseNumericLiteral
                 , withSourceSpan' Literal $ parseCharLiteral
                 , withSourceSpan' Literal $ parseStringLiteral
                 , withSourceSpan' Literal $ parseBooleanLiteral
                 , withSourceSpan' Literal $ parseArrayLiteral parseValue
                 , withSourceSpan' Literal $ parseObjectLiteral parseIdentifierAndValue
                 , parseAbs
                 , P.try parseConstructor
                 , P.try parseVar
                 , parseCase
                 , parseIfThenElse
                 , parseDo
                 , parseAdo
                 , parseLet
                 , P.try $ Parens <$> parens parseValue
                 , withSourceSpan' Op $ parseQualified (parens parseOperator)
                 , parseHole


parseValue :: P.ParsecT [TToken] u m Expr
parseValue =
    E.buildExpressionParser operators [] P.<?> "expression"
  where
    operators = [ [ E.Prefix (UnaryMinus <$> symbol "-")
                  ]
              --  , [ P.Infix (P.try (indented *> parseInfixExpr P.<?> "infix expression") >>= \ident ->
              --        return (BinaryNoParens ident)) P.AssocRight
              --    ]
                ]

{-
-- | Parse an expression
parseValue :: TokenParser Expr
parseValue =
  P.buildExpressionParser operators
    (buildPostfixParser postfixTable indexersAndAccessors)
    P.<?> "expression"
  where
  postfixTable = [ \v -> P.try (flip App <$> (indented *> indexersAndAccessors)) <*> pure v
                 , \v -> flip (TypedValue True) <$> (indented *> doubleColon *> parsePolyType) <*> pure v
                 ]
  operators = [ [ P.Prefix (indented *> withSourceSpan' (\ss _ -> UnaryMinus ss) (symbol' "-"))
                ]
              , [ P.Infix (P.try (indented *> parseInfixExpr P.<?> "infix expression") >>= \ident ->
                    return (BinaryNoParens ident)) P.AssocRight
                ]
              ]
-}
-}
