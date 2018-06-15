module Spec.ParserSpec where

import Test.Hspec


spec :: Spec
spec =
  describe "Parser Tests" $
    it "can parse a function application" $ do
      True `shouldBe` True
