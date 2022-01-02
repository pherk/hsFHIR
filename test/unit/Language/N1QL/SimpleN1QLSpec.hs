{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.N1QL.SimpleN1QLSpec where

import           Language.N1QL.SimpleN1QL.Ast
import           Language.N1QL.SimpleN1QL.Pretty
import Test.Hspec

spec :: Spec
spec = do
  describe "generateAST" $ do
    it "" $ do
      True `shouldBe` True
