{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.N1QL.CompileSpec where

import           Data.FHIR.Interface
import           Data.FHIR.Search
import           Language.N1QL.Analyze
import           Language.N1QL.Compile
import           Language.N1QL.Paths
import           Language.N1QL.SearchConfig
import qualified Language.N1QL.Tree             as T
import           Language.N1QL.Zipper
import           Ntwo.DAO.Types                 ( DAOIndexType(..) )
import           Ntwo.Query
import           Ntwo.Query.QueryString
import           Ntwo.Query.RestAPI
import           RIO
import qualified RIO.List                       as L
import qualified RIO.Map                        as M
import qualified RIO.Set                        as S
import Test.Hspec

rt         = T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
rid        = T.Node (ISimple Nothing "_id"       [ValOp "="  ["12345"]]) []
family     = T.Node (ISimple Nothing "family"    [ValOp "="  ["Abbo"]]) []
given      = T.Node (ISimple Nothing "given"     [ValOp "="  ["Rosa"]]) []
birthDate1 = T.Node (ISimple Nothing "birthDate" [ValOp ">=" ["2020-01-01"]]) []
birthDate2 = T.Node (ISimple Nothing "birthDate" [ValOp "<=" ["2020-12-31"]]) []
gender     = T.Node (ISimple Nothing "gender"    [ValOp "="  ["female"]]) []

small :: T.Tree Instruction
small = 
  T.Node (ISelect "nabu") 
      [T.Node (IWhere) [
        rt
      , T.Node (IUnnest "name" "na") [
              T.Node (ISimple Nothing "family" [ValOp "=" ["Vausi"]]) []
            , T.Node (IArray Nothing "given" [ValOp "=" ["Rosa"]]) []
            ]
      ]]

big :: T.Tree Instruction
big = 
  T.Node (ISelect "nabu") 
      [T.Node (IWhere) [
        rt
      , rid
      , T.Node (IUnnest "name" "na") [
              family
            , given
            ]
      , birthDate1
      , gender
      ]]

comm :: T.Tree Instruction
comm =
  T.Node (ISelect "nabu")
      [T.Node (IWhere) [
          T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
        , T.Node (IUnnest "communication" "co") [
            T.Node (IPath "language") [
                T.Node (IUnnest "coding" "co") [
                    T.Node (INoIndex Nothing "system" [ValOp "=" ["urn:ietf:bcp:47"]]) []
                  , T.Node (INoIndex Nothing "value" [ValOp "=" ["de"]]) []
                  ]
                ]
            ]
        ]]
sort :: T.Tree Instruction
sort =
  T.Node (ISort) [
        T.Node (ISortPath Nothing Nothing) [
              T.Node (IUnnest "name" "na") [
                    T.Node (ISimple Nothing "family" []) []]]
      , T.Node (ISortPath Nothing Nothing) [
              T.Node (IUnnest "name" "na") [
                    T.Node (IArray Nothing "given" []) []]]
      , T.Node (ISortPath Nothing Nothing) [T.Node (ISimple Nothing "birthDate" []) []]
      ]

spec :: Spec
spec = do
    describe "var table (unnests)" $ do
        it "no var" $ do
          L.foldl (toVars) (M.empty,1,S.empty) (T.Node (ISelect "nabu") []) `shouldBe` (M.empty,1,S.empty)
        it "one var" $ do
          L.foldl (toVars) (M.empty,1,S.empty) small `shouldBe` (M.fromList [("name","na")],1,S.fromList ["na"])
        it "two vars" $ do
          L.foldl (toVars) (M.empty,1,S.empty) comm `shouldBe` (M.fromList [("coding","co1"),("communication","co")],2,S.fromList ["co","co1"])

    describe "compile" $ do
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
        let compileWT s = compile $ analyze (mkTypeIA s) testSC
        let compileWoT s = compile $ analyze (mkSystemIA s) testSC
        let (vars,_,_)  = L.foldl toVars  (M.empty,1,S.empty) sort
{-
        it "orderBy" $ do
          orderByExpr (Just sort) vars `shouldBe` Nothing
            Just (OrderByCls [
                  OrderingTerm (NestedE (NField (FieldUI (IdentifierE (EId "na")) (UId "family")))) Nothing Nothing
                , OrderingTerm (NestedE (NField (FieldUI (IdentifierE (EId "na")) (UId "given")))) Nothing Nothing
                , OrderingTerm (NestedE (NField (FieldUI (IdentifierE (EId "nabu")) (UId "birthDate")))) Nothing Nothing
                ])
-}        
        it "_type=Patient&birthdate=2020-01-01" $ do
            compileWoT "_type=Patient&birthdate=2020-01-01" `shouldBe`
--              "SELECT `nabu`.* FROM nabu    WHERE `nabu`.`resourceType` = \"Patient\" AND `nabu`.`birthDate` = \"2020-01-01\"    "
                Right "SELECT `nabu`.* FROM nabu    WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate = \"2020-01-01\"     "
        it "_type=Patient,Practitioner&birthdate=2020-01-01" $ do
            compileWoT "_type=Patient,Practitioner&birthdate=2020-01-01" `shouldBe`
--              "SELECT `nabu`.* FROM nabu    WHERE `nabu`.`resourceType` IN [\"Patient\",\"Practitioner\"] AND `nabu`.`birthDate` = \"2020-01-01\"     "
                Right "SELECT `nabu`.* FROM nabu    WHERE `nabu`.resourceType IN [\"Patient\",\"Practitioner\"] AND `nabu`.birthDate = \"2020-01-01\"     "
        it "_type=Patient&birthdate=ge2020-01-01&birthdate=le2020-12-31" $ do
            compileWoT "_type=Patient&birthdate=ge2020-01-01&birthdate=le2020-12-31" `shouldBe`
--              "SELECT `nabu`.* FROM nabu    WHERE `nabu`.`resourceType` = \"Patient\" AND `nabu`.`birthDate` >= \"2020-01-01\" AND `nabu`.`birthDate` <= \"2020-12-31\"     " 
                Right "SELECT `nabu`.* FROM nabu    WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate >= \"2020-01-01\" AND `nabu`.birthDate <= \"2020-12-31\"     "
        it "birthdate:missing=true" $ do
            compileWT "birthdate:missing=true" `shouldBe`
--              "SELECT `nabu`.* FROM nabu    WHERE `nabu`.`resourceType` = \"Patient\" AND IS MISSING `nabu`.`birthDate`     "
                Right "SELECT `nabu`.* FROM nabu    WHERE `nabu`.resourceType = \"Patient\" AND IS MISSING `nabu`.birthDate     "
        it "birthdate=2020-01-01" $ do
            compileWT "birthdate=2020-01-01" `shouldBe`
                Right "SELECT `nabu`.* FROM nabu    WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate = \"2020-01-01\"     "
        it "birthdate=2020-01" $ do
            compileWT "birthdate=2020-01" `shouldBe`
                Right "SELECT `nabu`.* FROM nabu    WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate LIKE \"2020-01%\"     "
        it "birthdate=2020" $ do
            compileWT "birthdate=2020" `shouldBe`
                Right "SELECT `nabu`.* FROM nabu    WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate LIKE \"2020%\"     "
        it "family:exact" $ do
            compileWT "family:exact=Abbott774" `shouldBe` 
                Right "SELECT `nabu`.* FROM nabu    UNNEST `name` `na`  WHERE `nabu`.resourceType = \"Patient\" AND `na`.family == \"Abbott774\"     "
        it "family" $ do
            compileWT "family=Abbo" `shouldBe` 
                Right "SELECT `nabu`.* FROM nabu    UNNEST `name` `na`  WHERE `nabu`.resourceType = \"Patient\" AND `na`.family LIKE \"Abbo%\"     "
        it "family, name-use" $ do
            compileWT "family=Abbo&name-use=official" `shouldBe` 
                Right "SELECT `nabu`.* FROM nabu    UNNEST `name` `na`  WHERE `nabu`.resourceType = \"Patient\" AND (`na`.family LIKE \"Abbo%\" AND `na`.`use` = \"official\")     "
        it "family&given" $ do
            compileWT "family=Vausi&given=Rosa" `shouldBe`
                Right "SELECT `nabu`.* FROM nabu    UNNEST `name` `na`  WHERE `nabu`.resourceType = \"Patient\" AND `na`.family LIKE \"Vausi%\" AND ANY t IN TOKENS(`na`.given) SATISFIES `t` LIKE \"Rosa%\" END     "
        it "identifier system|code" $ do
            compileWT "identifier=ORBIS_PID|123" `shouldBe` 
                Right "SELECT `nabu`.* FROM nabu    UNNEST `identifier` `id`  WHERE `nabu`.resourceType = \"Patient\" AND `id`.system = \"ORBIS_PID\" AND `id`.`value` = \"123\"     "
{-
                          T.Node (ISelect "nabu"]
                                 [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (IUnnest "identifier" "id") [
                                                T.Node (INoIndex Nothing "system" [ValOp "=" ["ORBIS_PID"]]) []
                                              , T.Node (INoIndex Nothing "value" [ValOp "=" ["123"]]) []
                                              ]
                                   ]]
-}
        it "identifier :of-type system|code|value" $ do
            compileWT "identifier:of-type=http://terminology.hl7.org/CodeSystem/v2-0203|MR|123" `shouldBe`
                Right "SELECT `nabu`.* FROM nabu    UNNEST `identifier` `id`  UNNEST `id`.type.coding `co`  WHERE `nabu`.resourceType = \"Patient\" AND `co`.system = \"http://terminology.hl7.org/CodeSystem/v2-0203\" AND `co`.code = \"MR\" AND `id`.`value` = \"123\"     "
{-
                          T.Node (ISelect "nabu"]
                                 [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (IUnnest "identifier" "id") [
                                                T.Node (IPath "type") [
                                                    T.Node (IUnnest "coding" "co") [
                                                         T.Node (INoIndex Nothing "system"
                                                               [ValOp "=" ["http://terminology.hl7.org/CodeSystem/v2-0203"]]) []
                                                       , T.Node (INoIndex Nothing "code" [ValOp "=" ["MR"]]) []
                                                       ]
                                                  ]
                                              , T.Node (INoIndex Nothing "value"  [ValOp "=" ["123"]]) []
                                              ]
                                   ]]

-}
        it "language system|code" $ do
            compileWT "language=urn:ietf:bcp:47|de" `shouldBe` 
                Right "SELECT `nabu`.* FROM nabu    UNNEST `communication` `co`  UNNEST `co`.language.coding `co1`  WHERE `nabu`.resourceType = \"Patient\" AND `co1`.system = \"urn:ietf:bcp:47\" AND `co1`.`value` = \"de\"     "
{-
                          T.Node (ISelect "nabu"]
                                 [T.Node (IWhere) [
                                     T.Node (ISimple Nothing "resourceType" [ValOp "=" ["Patient"]]) []
                                   , T.Node (IUnnest "communication" "co") [
                                                T.Node (IPath "language") [
                                                    T.Node (IUnnest "coding" "co") [
                                                        T.Node (INoIndex Nothing "system" [ValOp "=" ["urn:ietf:bcp:47"]]) []
                                                      , T.Node (INoIndex Nothing "value" [ValOp "=" ["de"]]) []
                                                      ]
                                                  ]
                                              ]
                                   ]]
-}
        it "period start/end" $ do
            compileWoT "_type=Task&recipient-date=2020-01-01" `shouldBe`
                 Right "SELECT `nabu`.* FROM nabu    WHERE `nabu`.resourceType = \"Task\" AND `restriction`.period.start <= \"2020-01-01\" AND `restriction`.period.end >= \"2020-01-01\"     "
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
        let compileWT s = compile $ analyze (mkTypeIA s) testSC
        let compileWoT s = compile $ analyze (mkSystemIA s) testSC

        it "_sort simple prop" $ do
            compileWoT "_type=Patient&birthdate=2020-01-01&_sort=birthdate" `shouldBe`
                Right "SELECT `nabu`.* FROM nabu    WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate = \"2020-01-01\"   ORDER BY `nabu`.birthDate  "
        it "_sort simple prop DESC" $ do
            compileWoT "_type=Patient&birthdate=2020-01-01&_sort=-birthdate" `shouldBe`
                Right "SELECT `nabu`.* FROM nabu    WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate = \"2020-01-01\"   ORDER BY `nabu`.birthDate DESC  "
        it "_sort array" $ do
            compileWoT "_type=Patient&birthdate=2020-01-01&_sort=family" `shouldBe`
                Right "SELECT `nabu`.* FROM nabu    UNNEST `name` `na`  WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate = \"2020-01-01\"   ORDER BY `na`.family  "
        it "_sort array,array,prop" $ do
            compileWoT "_type=Patient&birthdate=2020-01-01&_sort=family,given,birthdate" `shouldBe`
                Right "SELECT `nabu`.* FROM nabu    UNNEST `name` `na`  WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate = \"2020-01-01\"   ORDER BY `na`.family,`na`.given,`nabu`.birthDate  "
        it "_elements id, text" $ do
            compileWoT "_type=Patient&_elements=id,text&birthdate=2020-01-01&_sort=family,given,birthdate" `shouldBe`
                Right "SELECT `nabu`.resourceType,`nabu`.id,`nabu`.text FROM nabu    UNNEST `name` `na`  WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate = \"2020-01-01\"   ORDER BY `na`.family,`na`.given,`nabu`.birthDate  "
        it "_elements id, family (fail)" $ do
            compileWoT "_type=Patient&_elements=id,family&birthdate=2020-01-01" `shouldBe`
                Right "SELECT `nabu`.resourceType,`nabu`.id FROM nabu    WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate = \"2020-01-01\"     "
        it "_count=0" $ do
            compileWoT "_type=Patient&_count=0&birthdate=2020-01-01" `shouldBe`
                Right "SELECT COUNT(*) FROM nabu    WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate = \"2020-01-01\"     "
        it "_count=30" $ do
            compileWoT "_type=Patient&birthdate=2020-01-01&_count=30" `shouldBe`
                Right "SELECT `nabu`.* FROM nabu    WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate = \"2020-01-01\"    LIMIT 30 "
        it "page=1 (limit default 25)" $ do
            compileWoT "_type=Patient&birthdate=2020-01-01&page=1" `shouldBe`
                Right "SELECT `nabu`.* FROM nabu    WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate = \"2020-01-01\"    LIMIT 25 OFFSET 1"
        it "page=3 (limit default 25)" $ do
            compileWoT "_type=Patient&birthdate=2020-01-01&page=3" `shouldBe`
                Right "SELECT `nabu`.* FROM nabu    WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate = \"2020-01-01\"    LIMIT 25 OFFSET 51"
        it "_count=30&page=3" $ do
            compileWoT "_type=Patient&birthdate=2020-01-01&_count=30&page=3" `shouldBe`
                Right "SELECT `nabu`.* FROM nabu    WHERE `nabu`.resourceType = \"Patient\" AND `nabu`.birthDate = \"2020-01-01\"    LIMIT 30 OFFSET 61"


-- SELECT nabu.* FROM nabu UNNEST identifier id UNNEST id.type.coding idt UNNEST nabu.name n WHERE `nabu`.`resourceType` = 'Patient' and n.family='Larson43' and id.`value`='999-36-6953' and idt.code='SS'
-- SELECT `nabu`.* FROM `nabu` UNNEST `nabu`.identifier AS id WHERE `nabu`.`resourceType` = 'Patient' AND `id`.`value` = '446053' AND `id`.`system` = 'http://uk-koeln.de/#orbis-pnr';
-- SELECT * FROM nabu UNNEST communication cc UNNEST cc.language.coding c WHERE nabu.resourceType='Patient' and c.`system` = 'urn:ietf:bcp:47';

