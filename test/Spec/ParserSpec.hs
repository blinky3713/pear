module Spec.ParserSpec where

import Language.Parser
import Language.Expr
import Language.Names
import Test.Hspec

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
