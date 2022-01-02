{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.N1QL.TreeSpec where

import           Language.N1QL.Tree
import           Ntwo.DAO.Types                 ( DAOIndexType(..) )
import           RIO
import Test.Hspec

small :: Tree Int
small = 
  Node 1 [
        Node 2 []
      , Node 3 [
              Node 4 []
            , Node 5 []
            ]
      ]

big :: Tree Int
big = 
  Node 1 [
        Node 2 []
      , Node 7 []
      , Node 3 [
              Node 4 [Node 9 [], Node 10 []]
            , Node 5 []
            ]
      , Node 6 []
      , Node 8 []
      ]
showPC :: Int -> String
showPC = show

spec :: Spec
spec = do
  describe "Tests" $ do
    let t = Node 1 []
    it "init" $ do
        t `shouldBe` Node 1 []
    it "isLeaf: true" $ do
        isLeaf t `shouldBe` True
    it "isLeaf: false" $ do
        isLeaf small `shouldBe` False
    it "isInner: true" $ do
        isInner small `shouldBe` True
    it "isInner: false" $ do
        isInner t `shouldBe` False
    it "value" $ do
        value t `shouldBe` 1
    it "replace" $ do
        replace 1 t `shouldBe` Node 1 []
    it "getChildren" $ do
        getChildren small `shouldBe` [Node 2 [],Node 3 [Node 4 [],Node 5 []]]
    it "formatTree empty" $ do
        formatTree (showPC) Nil `shouldBe` ""
    it "formatTree root" $ do
        formatTree (showPC) (Node 1[]) `shouldBe`
                              "---1\n"
    it "formatTree big" $ do
        formatTree (showPC) big `shouldBe`
                     "---1\n   |\n   +---2\n   |\n   +---7\n   |\n   +---3\n   |   |\n   |   +---4\n   |   |   |\n   |   |   +---9\n   |   |   |\n   |   |   +---10\n   |   |\n   |   +---5\n   |\n   +---6\n   |\n   +---8\n"

    it "mapTree small" $ do
        mapTree (+1) small `shouldBe` Node 2 [Node 3 [],Node 4 [Node 5 [],Node 6 []]]
    it "mapTree big" $ do
        mapTree (+1) big `shouldBe` 
                 Node 2 [Node 3 [],Node 8 [],Node 4 [Node 5 [Node 10 [],Node 11 []],Node 6 []],Node 7 [],Node 9 []]
