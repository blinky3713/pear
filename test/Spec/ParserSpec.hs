module Spec.ParserSpec where

import Language.Parser
import Language.Expr
import Language.Names
import Test.Hspec
import Data.List (intercalate)

spec :: Spec
spec =
  describe "Parser Tests" $ do

    it "can parse a function application" $ do
      let textExpr = "f x"
          parsedExpr = fmap forgetExprAnn (parseExprFromStr textExpr)
      parsedExpr `shouldBe` Right (App (Var () (Ident "f")) (Var () (Ident "x")))

    it "can parse abstraction" $ do
      let textExpr = "\\x -> f x"
          parsedExpr = fmap forgetExprAnn (parseExprFromStr textExpr)
          desired = Abs (VarBinder () (Ident "x"))
                      (App (Var () (Ident "f")) (Var () (Ident "x")))
      parsedExpr `shouldBe` Right desired

    it "can parse if then else" $ do
      let (s, e) = testExpr3
          parsedExpr = fmap forgetExprAnn (parseExprFromStr s)
      parsedExpr `shouldBe` Right e

    it "can parse array literals" $ do
      let (s, e) = testExpr4
          parsedExpr = fmap forgetExprAnn (parseExprFromStr s)
      parsedExpr `shouldBe` Right e

    it "can parse numeric literals" $ do
      let parsedExpr1 = fmap forgetExprAnn (parseExprFromStr "1.0")
          parsedExpr2 = fmap forgetExprAnn (parseExprFromStr "3")
          parsedExpr3 = fmap forgetExprAnn (parseExprFromStr "-3")
      parsedExpr1 `shouldBe` Right (Literal () (NumericLiteral 1.0))
      parsedExpr2 `shouldBe` Right (Literal () (IntLiteral 3))
      parsedExpr3 `shouldBe` Right (UnaryMinus (Literal () (IntLiteral 3)))

testExpr1 :: (String, Expr ())
testExpr1 =
  let testExpr = "f x"
      desired = App (Var () (Ident "f")) (Var () (Ident "x"))
  in (testExpr, desired)

testExpr2 :: (String, Expr ())
testExpr2 =
  let testExpr = "\\x -> f x"
      desired = Abs (VarBinder () (Ident "x"))
                  (App (Var () (Ident "f")) (Var () (Ident "x")))
  in (testExpr, desired)

testExpr3 :: (String, Expr ())
testExpr3 =
  let (s2, e2) = testExpr2
      (s1, e1) = testExpr1
      testExpr = "if p then " ++ s1 ++ " else " ++ s2
  in (testExpr, IfThenElse (Var () (Ident "p")) e1 e2)

testExpr4 :: (String, Expr ())
testExpr4 =
  let (s,e) = testExpr1
  in ("[" ++ intercalate ", " (replicate 3 s) ++ "]", Literal () (ArrayLiteral [e,e,e]))
