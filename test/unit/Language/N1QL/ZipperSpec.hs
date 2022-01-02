{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.N1QL.ZipperSpec where

import qualified Language.N1QL.Tree             as T
import           Language.N1QL.Zipper
import           Ntwo.DAO.Types                 ( DAOIndexType(..) )
import           RIO
import Test.Hspec

small :: T.Tree Int
small = 
  T.Node 1 [
        T.Node 2 []
      , T.Node 3 [
              T.Node 4 []
            , T.Node 5 []
            ]
      ]

big :: T.Tree Int
big = 
  T.Node 1 [
        T.Node 2 []
      , T.Node 7 []
      , T.Node 3 [
              T.Node 4 []
            , T.Node 5 []
            ]
      , T.Node 6 []
      , T.Node 8 []
      ]
showPC :: Int -> String
showPC = show

spec :: Spec
spec = do
  describe "Zipper" $ do
    let z = (T.Node 1 [],[]) :: Zipper Int
    let zs= (small,[]) :: Zipper Int
    let zb= (big  ,[]) :: Zipper Int
    it "init" $ do
        z `shouldBe` (T.Node 1 [], [])
    it "isLeaf: true" $ do
        isLeaf z `shouldBe` True
    it "isLeaf: false" $ do
        isLeaf zs `shouldBe` False
    it "isInner: true" $ do
        isInner zs `shouldBe` True
    it "isInner: false" $ do
        isInner z `shouldBe` False
    it "isRoot: true" $ do
        isRoot z `shouldBe` True
    it "isRoot: true" $ do
        isRoot zs `shouldBe` True
    it "isRoot: false" $ do
        isRoot (visitChild 0 zs) `shouldBe` False
    it "value" $ do
        value z `shouldBe` 1
    it "replace" $ do
        replace 1 z `shouldBe` (T.Node 1 [],[])
    it "delete root" $ do
        delete z `shouldBe` (T.Nil,[])
    it "delete focus" $ do
        topMost (delete (visitChild 0 zs)) `shouldBe`
                           (T.Node 1 [
                              T.Node 3 [
                                  T.Node 4 []
                                , T.Node 5 []
                                ]
                              ]
                           , []
                           )
    it "delete focus 2 big" $ do
        topMost (delete (visitChild 2 zb)) `shouldBe`
                           (T.Node 1
                               [
                                 T.Node 2 []
                               , T.Node 7 []
                               , T.Node 6 []
                               , T.Node 8 []
                               ]
                           ,
                             []
                           )
    it "delete focus 2 1 big" $ do
        topMost (delete (visitChild 1 (visitChild 2 zb))) `shouldBe`
                           (T.Node 1
                               [ 
                                 T.Node 2 []
                               , T.Node 7 []
                               , T.Node 3
                                    [T.Node 4 []]
                               , T.Node 6 []
                               , T.Node 8 []
                               ]
                           ,
                             []
                           )
    it "visit child 0" $ do
        visitChild 0 zs `shouldBe`  
                           (T.Node 2 []
                           , [Crumb 1 [] 
                                [T.Node 3 [ T.Node 4 [] , T.Node 5 [] ] ]
                             ]
                           )
    it "visit child 1" $ do
        visitChild 1 zs `shouldBe`  
                           (T.Node 3 [
                                T.Node 4 []
                              , T.Node 5 []
                              ]
                           , [Crumb 1 [
                                 T.Node 2 []
                                    ]
                                 []
                             ]
                           )
    it "visit child 2 1" $ do
        visitChild 1 (visitChild 2 zb) `shouldBe`  
                           (T.Node 5 []
                           , [
                               Crumb 3 [T.Node 4 []] []
                             , Crumb 1 [T.Node 2 [],T.Node 7 []] [T.Node 6 [],T.Node 8 []]
                             ]
                           )
    it "up $ visit child 2 1" $ do
        up (visitChild 1 (visitChild 2 zb)) `shouldBe`  
                           (T.Node 3 [
                                T.Node 4 []
                              , T.Node 5 []
                              ]
                           , [Crumb 1
                                   [
                                     T.Node 2 []
                                   , T.Node 7 []
                                   ]
                                   [ T.Node 6 []
                                   , T.Node 8 []
                                   ]
                             ]
                           )
    it "topMost $ visit child 2 1" $ do
        topMost (visitChild 1 (visitChild 2 zb)) `shouldBe` zb 
    it "matchChild 'name'" $ do
        matchChild 3 zs `shouldBe`  
                           Just (T.Node 3 [
                                    T.Node 4 []
                                  , T.Node 5 []
                                  ]
                                , [Crumb 1
                                     [T.Node 2 []]
                                     [] 
                                  ]
                                )
    it "matchChild 'name' big" $ do
        matchChild 3 zb `shouldBe`  
                           Just (T.Node 3
                                  [
                                    T.Node 4 []
                                  , T.Node 5 []
                                  ]
                                , [Crumb 1
                                    [
                                      T.Node 2 []
                                    , T.Node 7 []
                                    ]
                                    [
                                      T.Node 6 []
                                    , T.Node 8 []
                                    ]
                                  ]
                                )

