{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.N1QL.SimpleN1QL.PrettySpec where

import           Language.N1QL.SimpleN1QL
import           Language.N1QL.SimpleN1QL.Ast
import           Language.N1QL.SimpleN1QL.Pretty
import RIO
import Test.Hspec


spec :: Spec
spec = do
  describe "Literals" $ do
    it "string" $ do
      prettyLiteral (LString "test") `shouldBe` "\"test\""
    it "integer" $ do
      prettyLiteral (LNumberI 123) `shouldBe` "123"
    it "float" $ do
      prettyLiteral (LNumberF 123.123) `shouldBe` "123.123"
    it "true" $ do
      prettyLiteral (LTrue) `shouldBe` "TRUE"
    it "false" $ do
      prettyLiteral (LFalse) `shouldBe` "FALSE"
    it "Null" $ do
      prettyLiteral (LNull) `shouldBe` "NULL"
    it "Missing" $ do
      prettyLiteral (LMissing) `shouldBe` "MISSING"

  describe "Expr" $ do
    it "+" $ do
      prettyExpr (ArithmeticTermE (ATPlus (LiteralE (LNumberI 2)) (LiteralE (LNumberI 3)))) `shouldBe` "2 + 3"
    it "-" $ do
      prettyExpr (ArithmeticTermE (ATMinus (LiteralE (LNumberI 2)) (LiteralE (LNumberI 3)))) `shouldBe` "2 - 3"
    it "*" $ do
      prettyExpr (ArithmeticTermE (ATMul  (LiteralE (LNumberI 2)) (LiteralE (LNumberI 3)))) `shouldBe` "2 * 3"
    it "%" $ do
      prettyExpr (ArithmeticTermE (ATDiv   (LiteralE (LNumberI 2)) (LiteralE (LNumberI 3)))) `shouldBe` "2 DIV 3"
    it "-" $ do
      prettyExpr (ArithmeticTermE (ATUMinus (LiteralE (LNumberI 2)))) `shouldBe` "-2"
    it "(expr)" $ do
      prettyExpr (ParenE (ArithmeticTermE (ATUMinus (LiteralE (LNumberI 2))))) `shouldBe` "(-2)"
    it "`f` IN [x,y,z]" $ do
      prettyExpr (CollectionE $ CIn (IdentifierE (EId "f")) False (ConstructionE $ 
               CEArray $ Just (Elements (LiteralE (LString "x")) (Just (Elements (LiteralE (LString "y")) (Just (Elements (LiteralE (LString "z")) Nothing)))))))
                `shouldBe` "`f` IN [\"x\",\"y\",\"z\"]"
    it "orderByCls" $ do
      prettyOB (Just $ OrderByCls [
                   OrderingTerm (IdentifierE (EId "x")) Nothing Nothing
                 , OrderingTerm (IdentifierE (EId "y")) Nothing Nothing
                 , OrderingTerm (IdentifierE (EId "z")) Nothing Nothing
                 ])
                `shouldBe` "ORDER BY `x`,`y`,`z`"
  describe "N1QL" $ do
    let pn1ql e un l = prettyN1QL (mkN1QL bucket (Just e) un l)

    it "rt=Patient" $ do
      pn1ql rte [] 5 `shouldBe` "SELECT `nabu`.* FROM nabu    WHERE `nabu`.`resourceType` = \"Patient\"    LIMIT 5 "
    it "unnest name" $ do
      pn1ql rte name 0 `shouldBe` "SELECT `nabu`.* FROM nabu    UNNEST `nabu`.name nna  WHERE `nabu`.`resourceType` = \"Patient\"     "
    it "Patient:family,given" $ do
      pn1ql rfg name 0 `shouldBe` 
              "SELECT `nabu`.* FROM nabu    UNNEST `nabu`.name nna  WHERE `nabu`.`resourceType` = \"Patient\" AND `nna`.family LIKE \"Abbo%\" AND ANY t IN tokens(`nna`.given) SATISFIES t LIKE \"Ros%\" END     "
  describe "N1QL" $ do
    let pn1ql e un l = prettyN1QL (mkN1QL bucket (Just e) un l)

    it "rt=Patient" $ do
      pn1ql rte [] 5 `shouldBe` "SELECT `nabu`.* FROM nabu    WHERE `nabu`.`resourceType` = \"Patient\"    LIMIT 5 "
    it "unnest name" $ do
      pn1ql rte name 0 `shouldBe` "SELECT `nabu`.* FROM nabu    UNNEST `nabu`.name nna  WHERE `nabu`.`resourceType` = \"Patient\"     "
    it "Patient:family,given" $ do
      pn1ql rfg name 0 `shouldBe` 
              "SELECT `nabu`.* FROM nabu    UNNEST `nabu`.name nna  WHERE `nabu`.`resourceType` = \"Patient\" AND `nna`.family LIKE \"Abbo%\" AND ANY t IN tokens(`nna`.given) SATISFIES t LIKE \"Ros%\" END     "
