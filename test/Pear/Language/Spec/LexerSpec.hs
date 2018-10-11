module Pear.Language.Spec.LexerSpec where

import Text.ParserCombinators.Parsec.Error (errorMessages, messageString)
import Pear.Language.Lexer
import Test.Hspec

spec :: Spec
spec = describe "Lexer Spec" $ do

  it "can lex a basic function" $
    let lambdaString = "\\x -> f x"
        lambda = [ TSymbol "\\"
                 , TIdentifier "x"
                 , RArrow
                 , TIdentifier "f"
                 , TIdentifier "x"
                 ]
    in compareLexed lambdaString lambda

  it "can lex an assignment statement" $
    let assignmentString = "x = 1"
        assignment = [ TIdentifier "x"
                     , Equals
                     , TInt 1
                     ]
    in compareLexed assignmentString assignment

  it "can lex a binary operator statement" $
    let expressionString = "(x + y) % \"hello\""
        expression = [ LParen
                     , TIdentifier "x"
                     , TSymbol "+"
                     , TIdentifier "y"
                     , RParen
                     , TSymbol "%"
                     , TStringLit "hello"
                     ]
    in compareLexed expressionString expression

  it "can lex a let statement" $
    let letStatementString = "let x = y \n in false"
        letStatement = [ TName "let"
                       , TIdentifier "x"
                       , Equals
                       , TIdentifier "y"
                       , TName "in"
                       , TName "false"
                       ]
    in compareLexed letStatementString letStatement

  it "can lex a monadic bind" $
    let bindString = "m >>= \\a -> f a"
        bind' = [ TIdentifier "m"
                , TSymbol ">>="
                , TSymbol "\\"
                , TIdentifier "a"
                , RArrow
                , TIdentifier "f"
                , TIdentifier "a"
                ]
    in compareLexed bindString bind'

  it "can lex comments" $
    let commentString1 = "{- hello -}"
        comment1 = [TSymbol "{-", TIdentifier "hello", TSymbol "-}"]
        commentString2 = "-- hello"
        comment2 = [TSymbol "--", TIdentifier "hello"]
    in compareLexed commentString1 comment1 >> compareLexed commentString2 comment2

compareLexed :: String -> [Token] -> IO ()
compareLexed input expected = do
  let lexed = pearLexer input
  case lexed of
    Left err -> fail $ unlines . map messageString $ errorMessages err
    Right l -> projectTokens l `shouldBe` expected
  where
    projectTokens :: [PositionedToken] -> [Token]
    projectTokens = map ptToken
