{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.N1QL.PathsSpec where

import           Language.N1QL.Paths
import qualified Language.N1QL.Tree             as T
import           Language.N1QL.Zipper
import           Ntwo.DAO.Types                 ( DAOIndexType(..) )
import           RIO
import Test.Hspec

nabu = (PC "nabu" [] DITBucket)
rt   = (PC "resourceType" [ValOp "=" ["Patient"]] DITSimple)
rid  = (PC "_id" [ValOp "=" ["12345"]] DITSimple)
name = (PC "name" [] DITArray)
family = (PC "family" [ValOp "=" ["Abbo"]] DITSimple)
given = (PC "given" [ValOp "=" ["Rosa"]] DITArray)
birthDate1 = (PC "birthDate" [ValOp ">=" ["2020-01-01"]] DITSimple)
birthDate2 = (PC "birthDate" [ValOp "<=" ["2020-12-31"]] DITSimple)
gender = (PC "gender" [ValOp "=" ["male"]] DITSimple)

small :: T.Tree PC
small = 
  T.Node nabu [
        T.Node rt []
      , T.Node name [
              T.Node family []
            , T.Node given []
            ]
      ]

big :: T.Tree PC
big = 
  T.Node nabu [
        T.Node rt []
      , T.Node rid []
      , T.Node name [
              T.Node family []
            , T.Node given []
            ]
      , T.Node birthDate1 []
      , T.Node gender []
      ]

spec :: Spec
spec = do
  describe "Path Table" $ do
    it "init" $ do
      root "nabu" `shouldBe` T.Node nabu []
      value ((root "nabu"),[]) `shouldBe` (nabu)

    describe "insert into Nil" $ do
      it "insert single pc in Nil" $ do
        addP T.Nil [nabu] `shouldBe` (T.Node nabu [])
      it "insert path in Nil" $ do
        addP T.Nil [nabu,rt] `shouldBe` (T.Node nabu [T.Node rt []])
    describe "insert with root" $ do
      it "insert equal path" $ do
        addP (root "nabu") [nabu] `shouldBe` T.Node nabu []
      it "insert new path 1" $ do
        addP (root "nabu") [name] `shouldBe` T.Node nabu [T.Node name []]
      it "insert new path 2" $ do
        addP (root "nabu") [name,family] `shouldBe` T.Node nabu [T.Node name [T.Node family []]]
      it "insert new root path 1" $ do
        addP (root "nabu") [nabu,name] `shouldBe` T.Node nabu [T.Node name []]
      it "insert new root path 2" $ do
        addP (root "nabu") [nabu,name,family] `shouldBe` T.Node nabu [T.Node name [T.Node family []]]

    describe "insert with root" $ do
      it "insert second path" $ do
        addP (addP (root "nabu") [nabu,name,family] ) [nabu,name,given]  `shouldBe`
                         T.Node nabu [
                               T.Node name [
                                    T.Node given []
                                  , T.Node family []
                                  ]
                             ]

    describe "add values" $ do
      it "birthDate" $ do
        addP (addP (root "nabu") [nabu,birthDate1]) [nabu,birthDate2] `shouldBe`
                         T.Node nabu [T.Node (PC "birthDate" [ValOp ">=" ["2020-01-01"],ValOp "<=" ["2020-12-31"]] DITSimple) []]



    describe "complete paths" $ do
      it "small" $ do
        paths small `shouldBe` [
            [nabu,rt]
          , [nabu,name,family]
          , [nabu,name,given]
          ]
      it "big" $ do
        paths big `shouldBe` [
            [nabu,rt]
          , [nabu,rid]
          , [nabu,name,family]
          , [nabu,name,given]
          , [nabu,birthDate1]
          , [nabu,gender]
          ]

    describe "transform" $ do
      it "small" $ do
        T.mapTree transform small `shouldBe`
                T.Node (IBucket "nabu") [
                          T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                        , T.Node (IUnnest "name" "na") [
                              T.Node (ISimple Nothing "family" [ValOp "=" ["Abbo"]]) []
                            , T.Node (IArray Nothing "given" [ValOp "=" ["Rosa"]]) []
                            ]
                        ]
 
