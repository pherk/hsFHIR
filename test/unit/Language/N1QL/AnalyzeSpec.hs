{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.N1QL.AnalyzeSpec where

import           Data.FHIR.Interface
import           Data.FHIR.Search
import           Language.N1QL.Analyze
import           Language.N1QL.Paths
import           Language.N1QL.SearchConfig
import qualified Language.N1QL.Tree             as T
import           Language.N1QL.Zipper
import           Ntwo.DAO.Types                 ( DAOIndexType(..) )
import           Ntwo.Query
import           Ntwo.Query.QueryString
import           Ntwo.Query.RestAPI
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
  describe "Query Tree" $ do
    it "mkPath" $ do
      mkPath FdtString ["name","family"] [Just (ValOp "=" ["Vausi"])] [DITArray,DITSimple] `shouldBe` 
           [[(PC "name" [] DITArray),(PC "family" [(ValOp "=" ["Vausi"])] DITSimple)]]

    it "fromPC Date =" $ do
      fromPC FHIR_Patient testSC 
             (QPair QtProp (Key (Just "birthdate")) (QDate Nothing "2020-01-01" 8))
         `shouldBe` [[PC "birthDate" [ValOp "=" ["2020-01-01"]] DITSimple]]

    it "fromPC Date gt" $ do
      fromPC FHIR_Patient testSC 
             (QPair QtProp (Key (Just "birthdate")) (QDate (Just "gt") "2020-01-01" 8))
         `shouldBe` [[PC "birthDate" [ValOp ">" ["2020-01-01"]] DITSimple]]

    it "fromPC Date lt" $ do
      fromPC FHIR_Patient testSC 
             (QPair QtProp (Key (Just "birthdate")) (QDate (Just "lt") "2020-01-01" 8))
         `shouldBe` [[PC "birthDate" [ValOp "<" ["2020-01-01"]] DITSimple]]

    it "fromPC _lastUpdated=gt2020-01-01" $ do
      fromPC FHIR_Any testSC
             (QPair QtProp (Key (Just "_lastUpdated")) (QDate (Just "gt") "2020-01-01" 8))
         `shouldBe` [[PC "meta" [] DITPC,PC "lastUpdated" [ValOp ">" ["2020-01-01"]] DITSimple]]
    it "fromPC _tag=review" $ do
      fromPC FHIR_Any testSC
             (QPair QtProp (Key (Just "_tag")) (QToken Nothing (Just "review") Nothing))
         `shouldBe` [[PC "meta" [] DITPC,PC "tag" [] DITCC,PC "code" [ValOp "=" ["review"]] DITSimple]]
    it "fromPC String =" $ do
      fromPC FHIR_Patient testSC 
             (QPair QtProp (Key (Just "family")) (QString "Vausi"))
         `shouldBe` [[PC "name" [] DITArray,PC "family" [ValOp "=" ["Vausi"]] DITSimple]]

    it "fromPC String ==" $ do
      fromPC FHIR_Patient testSC 
             (QPair QtProp (KeyMod "family" (Just "exact") ) (QString "Vausi"))
         `shouldBe` [[PC "name" [] DITArray,PC "family" [ValOp "=" ["Vausi"]] DITSimple]]
    it "fromPCs Identifier sys|code" $ do
      fromPCs FHIR_Patient testSC 
             [(QPair QtProp (Key (Just "identifier")) (QToken (Just "ORBIS_PID") (Just "123") Nothing))]
         `shouldBe` [
                      [PC "identifier" [] DITArray,PC "system" [ValOp "=" ["ORBIS_PID"]] DITSimple]
                    , [PC "identifier" [] DITArray,PC "value" [ValOp "=" ["123"]] DITSimple]
                    ]
    it "fromPCs Identifier sys|code|value" $ do
      fromPCs FHIR_Patient testSC 
             [(QPair QtProp (KeyMod "identifier" (Just "of-type")) (QToken (Just "ORBIS_PID") (Just "123") (Just "456")))]
         `shouldBe` [
                      [PC "identifier" [] DITArray,PC "type" [] DITPC,PC "coding" [] DITArray,PC "system" [ValOp "=" ["ORBIS_PID"]] DITSimple]
                    , [PC "identifier" [] DITArray,PC "type" [] DITPC,PC "coding" [] DITArray,PC "code" [ValOp "=" ["123"]] DITSimple]
                    , [PC "identifier" [] DITArray,PC "value" [ValOp "=" ["456"]] DITSimple]
                    ]
    it "fromPCs two Dates (range)" $ do
      fromPCs FHIR_Patient testSC 
             [
               (QPair QtProp (Key (Just "birthdate")) (QDate (Just "ge") "2020-01-01" 8))
             , (QPair QtProp (Key (Just "birthdate")) (QDate (Just "le") "2020-12-31" 8))
             ]
         `shouldBe`
             [[PC "birthDate" [ValOp ">=" ["2020-01-01"]] DITSimple],[PC "birthDate" [ValOp "<=" ["2020-12-31"]] DITSimple]]
    it "pc2Paths * _lastUpdated=gt2020-01-01" $ do
      pc2Paths "nabu" [] testSC
             [(QPair QtProp (Key (Just "_lastUpdated")) (QDate (Just "gt") "2020-01-01" 8))]
         `shouldBe`
             T.Node (PC "nabu" [] DITBucket) [
                   T.Node (PC "meta" [] DITPC) [T.Node (PC "lastUpdated" [ValOp ">" ["2020-01-01"]] DITSimple) []]]
    it "pc2Paths Patient _lastUpdated=gt2020-01-01" $ do
      pc2Paths "nabu" [FHIR_Patient] testSC
             [(QPair QtProp (Key (Just "_lastUpdated")) (QDate (Just "gt") "2020-01-01" 8))]
         `shouldBe`
             T.Node (PC "nabu" [] DITBucket) [
                   T.Node (PC "resourceType" [ValOp "=" ["Patient"]] DITSimple) []
                 , T.Node (PC "meta" [] DITPC) [T.Node (PC "lastUpdated" [ValOp ">" ["2020-01-01"]] DITSimple) []]]
    it "pc2Paths Patient _tag=review" $ do
      pc2Paths "nabu" [FHIR_Patient] testSC
             [(QPair QtProp (Key (Just "_tag")) (QToken Nothing (Just "review") Nothing))]
         `shouldBe`
             T.Node (PC "nabu" [] DITBucket) [
                   T.Node (PC "resourceType" [ValOp "=" ["Patient"]] DITSimple) []
                 , T.Node (PC "meta" [] DITPC) [
                       T.Node (PC "tag" [] DITCC) [
                           T.Node (PC "code" [ValOp "=" ["review"]] DITSimple) []
                         ]
                     ]
                 ]
    it "pc2Paths two Dates (range)" $ do
      pc2Paths "nabu" [FHIR_Patient] testSC 
             [
               (QPair QtProp (Key (Just "birthdate")) (QDate (Just "ge") "2020-01-01" 8))
             , (QPair QtProp (Key (Just "birthdate")) (QDate (Just "le") "2020-12-31" 8))
             ]
         `shouldBe`
             T.Node (PC "nabu" [] DITBucket) [
                   T.Node (PC "resourceType" [ValOp "=" ["Patient"]] DITSimple) []
                 , T.Node (PC "birthDate" [ValOp ">=" ["2020-01-01"],ValOp "<=" ["2020-12-31"]] DITSimple) []
                 ]
    it "pc2Paths Identifier sys|code|value" $ do
      pc2Paths "nabu" [FHIR_Patient] testSC 
             [(QPair QtProp (KeyMod "identifier" (Just "of-type")) (QToken (Just "ORBIS_PID") (Just "123") (Just "456")))]
         `shouldBe` T.Node (PC "nabu" [] DITBucket) [
                        T.Node (PC "resourceType" [ValOp "=" ["Patient"]] DITSimple) []
                      , T.Node (PC "identifier" [] DITArray) [
                              T.Node (PC "type" [] DITPC) [
                                    T.Node (PC "coding" [] DITArray) [
                                          T.Node (PC "system" [ValOp "=" ["ORBIS_PID"]] DITSimple) []
                                        , T.Node (PC "code" [ValOp "=" ["123"]] DITSimple) []
                                        ]
                                  ]
                            , T.Node (PC "value" [ValOp "=" ["456"]] DITSimple) []
                            ]
                      ]
    describe "fromSortKeys" $ do
      it "[birthdate]" $ do
        fromSortKeys FHIR_Patient testSC ["birthdate"] `shouldBe` [[PC "birthDate" [] DITSimple]]
      it "[family]" $ do
        fromSortKeys FHIR_Patient testSC ["family"] `shouldBe` [[PC "name" [] DITArray,PC "family" [] DITSimple]]
      it "[given]" $ do
        fromSortKeys FHIR_Patient testSC ["given"] `shouldBe` [[PC "name" [] DITArray,PC "given" [] DITArray]]
      it "[family,given,birthdate]" $ do
        fromSortKeys FHIR_Patient testSC ["family","given","birthdate"] `shouldBe`
            [[PC "name" [] DITArray,PC "family" [] DITSimple],[PC "name" [] DITArray,PC "given" [] DITArray],[PC "birthDate" [] DITSimple]]
    describe "sortPaths" $ do
      it "[birthdate]" $ do
        sortPaths "nabu" FHIR_Patient testSC ["birthdate"] `shouldBe` 
            T.Node (PC "nabu" [] DITBucket) [T.Node (PC "birthDate" [] DITSimple) []]
      it "[family]" $ do
        sortPaths "nabu" FHIR_Patient testSC ["family"] `shouldBe`
            T.Node (PC "nabu" [] DITBucket) [T.Node (PC "name" [] DITArray) [T.Node (PC "family" [] DITSimple) []]]
      it "[given]" $ do
        sortPaths "nabu" FHIR_Patient testSC ["given"] `shouldBe`
            T.Node (PC "nabu" [] DITBucket) [T.Node (PC "name" [] DITArray) [T.Node (PC "given" [] DITArray) []]]
      it "[family,given,birthdate]" $ do
        sortPaths "nabu" FHIR_Patient testSC ["family","given","birthdate"] `shouldBe`
            T.Node (PC "nabu" [] DITBucket) [
                  T.Node (PC "birthDate" [] DITSimple) []
                , T.Node (PC "name" [] DITArray) [T.Node (PC "given" [] DITArray) []]
                , T.Node (PC "name" [] DITArray) [T.Node (PC "family" [] DITSimple) []]
                ]
    describe "sortExpr" $ do
      it "[birthdate]" $ do
        sortExpr "nabu" [FHIR_Patient] testSC 
            [QPair QtControl (Key (Just "_sort")) (QToken Nothing (Just "birthdate") Nothing)]
                                                 `shouldBe` 
            [T.Node ISort [T.Node (ISortPath Nothing Nothing) [T.Node (ISimple Nothing "birthDate" []) []]]]
      it "[-birthdate]" $ do
        sortExpr "nabu" [FHIR_Patient] testSC 
            [QPair QtControl (Key (Just "_sort")) (QToken Nothing (Just "-birthdate") Nothing)]
                                                 `shouldBe` 
            [T.Node ISort [T.Node (ISortPath (Just "desc") Nothing) [T.Node (ISimple Nothing "birthDate" []) []]]]
      it "[family]" $ do
        sortExpr "nabu" [FHIR_Patient] testSC
            [QPair QtControl (Key (Just "_sort")) (QToken Nothing (Just "family") Nothing)]
                                                 `shouldBe` 
            [T.Node ISort [T.Node (ISortPath Nothing Nothing) [T.Node (IUnnest "name" "na") [T.Node (ISimple Nothing "family" []) []]]]]
      it "[given]" $ do
        sortExpr "nabu" [FHIR_Patient] testSC
            [QPair QtControl (Key (Just "_sort")) (QToken Nothing (Just "given") Nothing)]
                                                 `shouldBe` 
            [T.Node ISort [T.Node (ISortPath Nothing Nothing) [T.Node (IUnnest "name" "na") [T.Node (IArray Nothing "given" []) []]]]]
      it "[family,given,birthdate]" $ do
        sortExpr "nabu" [FHIR_Patient] testSC 
            [QPair QtControl (Key (Just "_sort")) (QToken Nothing (Just "family,given,birthdate") Nothing)]
                                                 `shouldBe` 
            [T.Node ISort [
                  T.Node (ISortPath Nothing Nothing) [T.Node (IUnnest "name" "na") [T.Node (ISimple Nothing "family" []) []]]
                , T.Node (ISortPath Nothing Nothing) [T.Node (IUnnest "name" "na") [T.Node (IArray Nothing "given" []) []]]
                , T.Node (ISortPath Nothing Nothing) [T.Node (ISimple Nothing "birthDate" []) []]
                ]
            ]


    it "bucket" $ do
      root "nabu" `shouldBe` T.Node nabu []

    describe "from QueryString" $ do
      it "" $ do
        root "nabu" `shouldBe` T.Node nabu []
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
      it "transform ValOp list" $ do
        fmap transform (T.Node (PC "nabu" [] DITBucket) [
                   T.Node (PC "resourceType" [ValOp "=" ["Patient"]] DITSimple) []
                 , T.Node (PC "birthDate" [ValOp ">=" ["2020-01-01"],ValOp "<=" ["2020-12-31"]] DITSimple) []
                 ])
              `shouldBe`
                   T.Node (IBucket "nabu") [
                         T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                       , T.Node (ISimple Nothing "birthDate" [ValOp ">=" ["2020-01-01"],ValOp "<=" ["2020-12-31"]]) []
                       ]
      it "identifier" $ do
        T.getChildren (fmap transform (pc2Paths "nabu" [FHIR_Patient] testSC 
             [(QPair QtProp (KeyMod "identifier" (Just "of-type")) (QToken (Just "ORBIS_PID") (Just "123") (Just "456")))]))
              `shouldBe`
                   [
                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                   , T.Node (IUnnest "identifier" "id") [
                              T.Node (IPath "type") [
                                     T.Node (IUnnest "coding" "co") [
                                           T.Node (ISimple Nothing "system" [ValOp "=" ["ORBIS_PID"]]) []
                                         , T.Node (ISimple Nothing "code" [ValOp "=" ["123"]]) []
                                         ]
                                     ]
                            , T.Node (ISimple Nothing "value" [ValOp "=" ["456"]]) []]]


 
    describe "analyze SearchParam" $ do
        let ty = FHIR_Patient
        let mkTypeIA qs = RestTypeIA $ TypeIA { tcInteraction = IASearch
                                     , tcPath = []
                                     , tcRawQs = ""
                                     , tcQuery = qs
                                     , tcType = ty
                                     , tcOp = Nothing
                                     , tcBody = Nothing
                                     } 
        let mkSystemIA qs = RestServerIA $ ServerIA { scInteraction = IASearchAll
                                     , scPath = []
                                     , scRawQs = ""
                                     , scQuery = qs
                                     , scOp = Nothing
                                     , scBody = Nothing
                                     } 
        let analyzeWT s = analyze (mkTypeIA s) testSC
        let analyzeWoT s = analyze (mkSystemIA s) testSC

        it "_type=Patient&birthdate=2020-01-01" $ do
            analyzeWoT "_type=Patient&birthdate=2020-01-01" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "=" ["2020-01-01"]]) []
                                   ]]
        it "_type=Patient,Practitioner&birthdate=2020-01-01" $ do
            analyzeWoT "_type=Patient,Practitioner&birthdate=2020-01-01" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient","Practitioner"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "=" ["2020-01-01"]]) []
                                   ]]
        it "_type=Patient,Practitioner&_lastUpdated=gt2020-01-01" $ do
            analyzeWoT "_type=Patient,Practitioner&_lastUpdated=gt2020-01-01" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient","Practitioner"]]) []
                                   , T.Node (IPath "meta") [
                                         T.Node (ISimple Nothing "lastUpdated" [ValOp ">" ["2020-01-01"]]) []
                                       ] 
                                   ]]
        it "_type=Patient,Practitioner&_tag=review" $ do
            analyzeWoT "_type=Patient,Practitioner&_tag=review" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient","Practitioner"]]) []
                                   , T.Node (IPath "meta") [
                                         T.Node (IUnnest "tag" "ta") [
                                             T.Node (ISimple Nothing "code" [ValOp "=" ["review"]]) []
                                           ] 
                                        ]
                                   ]]
        it "_type=Patient,Practitioner&birthdate=2020-01-01" $ do
            analyzeWoT "_type=Patient,Practitioner&birthdate=2020-01-01" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient","Practitioner"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "=" ["2020-01-01"]]) []
                                   ]]
        it "_type=Patient&birthdate=ge2020-01-01&birthdate=le2020-12-31" $ do
            analyzeWoT "_type=Patient&birthdate=ge2020-01-01&birthdate=le2020-12-31" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp ">=" ["2020-01-01"],ValOp "<=" ["2020-12-31"]]) []
                                   ]]
        it "birthdate=2016-02-12" $ do
            analyzeWT "birthdate=2016-02-12" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "=" ["2016-02-12"]]) []
                                   ]]
        it "birthdate=2016-02" $ do
            analyzeWT "birthdate=2016-02" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "%" ["2016-02"]]) []
                                   ]]
        it "birthdate=2016" $ do
            analyzeWT "birthdate=2016" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "%" ["2016"]]) []
                                   ]]
        it "birthdate:missing=true" $ do
            analyzeWT "birthdate:missing=true" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "?" ["true"]]) []
                                   ]]
        it "family:exact" $ do
            analyzeWT "family:exact=Vausi" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (IUnnest "name" "na") [
                                                T.Node (ISimple Nothing "family" [ValOp "==" ["Vausi"]]) []
                                              ]
                                   ]]
        it "family" $ do
            analyzeWT "family=Vausi" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (IUnnest "name" "na") [
                                                T.Node (ISimple Nothing "family" [ValOp "%" ["Vausi"]]) []
                                              ]
                                   ]]
        it "family&name-use=official" $ do
            analyzeWT "family=Vausi&name-use=official" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (IUnnest "name" "na") [
                                                T.Node (ISimple Nothing "family" [ValOp "%" ["Vausi"]]) []
                                              , T.Node (ISimple Nothing "use" [ValOp "=" ["official"]]) []
                                              ]
                                   ]]
        it "family&family" $ do
            analyzeWT "family=Vausi&family=Polausi" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (IUnnest "name" "na") [
                                                T.Node (ISimple Nothing "family" [ValOp "%" ["Vausi"]
                                                                                 ,ValOp "%" ["Polausi"]]) []
                                              ]
                                   ]]
--                        ([FHIR_Patient], ([ QPair QtProp (Key (Just "family")) (QString "Vausi")
--                                          , QPair QtProp (Key (Just "family")) (QString "Polausi")
--                                          ],[],[],[],[]))
        it "family&given" $ do
            analyzeWT "family=Vausi&given=Rosa" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (IUnnest "name" "na") [
                                                T.Node (ISimple Nothing "family" [ValOp "%" ["Vausi"]]) []
                                              , T.Node (IArray Nothing "given" [ValOp "%" ["Rosa"]]) []
                                              ]
                                   ]]
        it "identifier system|code" $ do
            analyzeWT "identifier=ORBIS_PID|123" `shouldBe`
                          T.Node (ISelect "nabu")
                                 [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (IUnnest "identifier" "id") [
                                                T.Node (ISimple Nothing "system" [ValOp "=" ["ORBIS_PID"]]) []
                                              , T.Node (ISimple Nothing "value" [ValOp "=" ["123"]]) []
                                              ]
                                   ]]
        it "identifier :of-type system|code|value" $ do
            analyzeWT "identifier:of-type=http://terminology.hl7.org/CodeSystem/v2-0203|MR|123" `shouldBe`
                          T.Node (ISelect "nabu")
                                 [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (IUnnest "identifier" "id") [
                                                T.Node (IPath "type") [
                                                    T.Node (IUnnest "coding" "co") [
                                                          T.Node (ISimple Nothing "system"
                                                               [ValOp "=" ["http://terminology.hl7.org/CodeSystem/v2-0203"]]) []
                                                        , T.Node (ISimple Nothing "code" [ValOp "=" ["MR"]]) []
                                                        ]
                                                    ]
                                              , T.Node (ISimple Nothing "value"  [ValOp "=" ["123"]]) []
                                              ]
                                   ]]

        it "language system|code" $ do
            analyzeWT "language=urn:ietf:bcp:47|de" `shouldBe`
                          T.Node (ISelect "nabu")
                                 [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (IUnnest "communication" "co") [
                                                T.Node (IPath "language") [
                                                    T.Node (IUnnest "coding" "co") [
                                                        T.Node (ISimple Nothing "system" [ValOp "=" ["urn:ietf:bcp:47"]]) []
                                                      , T.Node (ISimple Nothing "value" [ValOp "=" ["de"]]) []
                                                      ]
                                                  ]
                                              ]
                                   ]]
        it "period start|end" $ do
            analyzeWoT "_type=Task&recipient-date=2020-01-01" `shouldBe` 
                          T.Node (ISelect "nabu")
                                 [T.Node (IWhere) [
                                       T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Task"]]) []
                                     , T.Node (IPath "restriction") [
                                                 T.Node (IPath "period") [
                                                      T.Node (ISimple Nothing "start" [ValOp "<=" ["2020-01-01"]]) []
                                                    , T.Node (ISimple Nothing "end" [ValOp ">=" ["2020-01-01"]]) []
                                                    ]
                                               ] 
                                     ]
                                 ]
    describe "_sort, _elements, _count, page" $ do
        let ty = FHIR_Patient
        let mkTypeIA qs = RestTypeIA $ TypeIA { tcInteraction = IASearch
                                     , tcPath = []
                                     , tcRawQs = ""
                                     , tcQuery = qs
                                     , tcType = ty
                                     , tcOp = Nothing
                                     , tcBody = Nothing
                                     } 
        let mkSystemIA qs = RestServerIA $ ServerIA { scInteraction = IASearchAll
                                     , scPath = []
                                     , scRawQs = ""
                                     , scQuery = qs
                                     , scOp = Nothing
                                     , scBody = Nothing
                                     } 
        let analyzeWT s = analyze (mkTypeIA s) testSC
        let analyzeWoT s = analyze (mkSystemIA s) testSC

        it "_sort: simple prop" $ do
            analyzeWoT "_type=Patient&birthdate=2020-01-01&_sort=birthdate" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [ T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "=" ["2020-01-01"]]) []
                                   ]
                               , T.Node (ISort) [
                                     T.Node (ISortPath Nothing Nothing) [
                                                T.Node (ISimple Nothing "birthDate" []) []
                                            ]
                                   ]
                               ]
        it "_sort: simple prop DESC" $ do
            analyzeWoT "_type=Patient&birthdate=2020-01-01&_sort=-birthdate" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [ T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "=" ["2020-01-01"]]) []
                                   ]
                               , T.Node (ISort) [
                                     T.Node (ISortPath (Just "desc") Nothing) [
                                             T.Node (ISimple Nothing "birthDate" []) []
                                            ]
                                   ]
                               ]
        it "_sort: multiple prop" $ do
            analyzeWoT "_type=Patient&birthdate=2020-01-01&_sort=family,given,birthdate" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [ T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "=" ["2020-01-01"]]) []
                                   ]
                               , T.Node (ISort) [
                                     T.Node (ISortPath Nothing Nothing) [
                                           T.Node (IUnnest "name" "na") [
                                                 T.Node (ISimple Nothing "family" []) []]]
                                   , T.Node (ISortPath Nothing Nothing) [
                                           T.Node (IUnnest "name" "na") [
                                                 T.Node (IArray Nothing "given" []) []]]
                                   , T.Node (ISortPath Nothing Nothing) [T.Node (ISimple Nothing "birthDate" []) []]
                                   ]
                               ]
        it "_elements: id, text for Patient" $ do
            analyzeWoT "_type=Patient&_elements=id,text&birthdate=2020-01-01" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [T.Node (IResult) [
                                     T.Node (ISimple Nothing "resourceType" []) []
                                   , T.Node (ISimple Nothing "id" []) []
                                   , T.Node (ISimple Nothing "text" []) []
                                   ]
                               , T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "=" ["2020-01-01"]]) []
                                   ]
                               ]

        it "_count=0" $ do
            analyzeWoT "_type=Patient&_count=0&birthdate=2020-01-01" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [
                                 T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "=" ["2020-01-01"]]) []
                                   ]
                               , T.Node (ISlice) [T.Node (ILimit 0) []]
                               ]
        it "_count=30" $ do
            analyzeWoT "_type=Patient&_count=30&birthdate=2020-01-01" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [
                                 T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "=" ["2020-01-01"]]) []
                                   ]
                               , T.Node (ISlice) [T.Node (ILimit 30) []]
                               ]
        it "page=3" $ do
            analyzeWoT "_type=Patient&birthdate=2020-01-01&page=3" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [
                                 T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "=" ["2020-01-01"]]) []
                                   ]
                               , T.Node (ISlice) [T.Node (IOffset 25 51) []]
                               ]
        -- page starts with 1
        it "_count=30&page=3" $ do
            analyzeWoT "_type=Patient&_count=30&birthdate=2020-01-01&page=3" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [
                                 T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "=" ["2020-01-01"]]) []
                                   ]
                               , T.Node (ISlice) [T.Node (IOffset 30 61) []]
                               ]

        it "_count=0&page=3" $ do
            analyzeWoT "_type=Patient&_count=0&birthdate=2020-01-01&page=3" `shouldBe`
                          T.Node (ISelect "nabu") 
                               [
                                 T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (ISimple Nothing "birthDate" [ValOp "=" ["2020-01-01"]]) []
                                   ]
                               , T.Node (ISlice) [T.Node (ILimit 0) []]
                               ]

-- SELECT nabu.* FROM nabu UNNEST identifier id UNNEST id.type.coding idt UNNEST nabu.name n WHERE `nabu`.`resourceType` = 'Patient' and n.family='Larson43' and id.`value`='999-36-6953' and idt.code='SS'
-- SELECT `nabu`.* FROM `nabu` UNNEST `nabu`.identifier AS id WHERE `nabu`.`resourceType` = 'Patient' AND `id`.`value` = '446053' AND `id`.`system` = 'http://uk-koeln.de/#orbis-pnr';

