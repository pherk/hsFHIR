{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.FHIR.Datatypes.InternalSpec where

import qualified Data.Aeson                   as J
import qualified Data.ByteString.Lazy.UTF8    as BLU
import           Data.FHIR.Datatypes.Internal
import           Data.FHIR.Datatypes.XML
import           Data.FHIR.Datatypes.XhtmlDiv
import qualified RIO.HashMap                  as HM
import Test.Hspec

spec :: Spec
spec = do
  describe "Address" $ do
    it "decode" $ do
      J.decode (BLU.fromString "{\"use\":\"home\",\"type\":\"postal\",\"line\":[\"Kerpenerstr. 62\"],\"city\":\"Köln\",\"state\":\"NRW\",\"postalCode\":\"50729\",\"country\":\"DEU\"}")
        `shouldBe` Just Address {
            addressId = Nothing
          , addressExtension= []
          , addressUse = Just AuHome
          , addressType = Just AtPostal
          , addressText = Nothing
          , addressLine = ["Kerpenerstr. 62"]
          , addressCity = Just "Köln"
          , addressDistrict = Nothing
          , addressState = Just "NRW"
          , addressPostalCode = Just "50729"
          , addressCountry = Just "DEU"
--    ISO 3166 3 letter code  
          , addressPeriod = Nothing
          }
    it "decode patient address" $ do
--      J.decode (BLU.fromString "{\"extension\":[{\"url\":\"http://hl7.org/fhir/StructureDefinition/geolocation\",\"extension\":[{\"url\":\"latitude\",\"valueDecimal\":41.76252653772401},{\"url\":\"longitude\",\"valueDecimal\":-70.02685367164186}]}],\"line\":[\"626 Jerde Lane Apt 51\"],\"city\":\"Harwich\",\"state\":\"Massachusetts\",\"country\":\"US\"}")
      J.decode (BLU.fromString "{\"line\":[\"626 Jerde Lane Apt 51\"],\"city\":\"Harwich\",\"state\":\"Massachusetts\",\"country\":\"US\"}")
        `shouldBe` Just Address {
            addressId = Nothing
          , addressExtension = []
          , addressUse = Nothing
          , addressType = Nothing
          , addressText = Nothing
          , addressLine = ["626 Jerde Lane Apt 51"]
          , addressCity = Just "Harwich"
          , addressDistrict = Nothing
          , addressState = Just "Massachusetts"
          , addressPostalCode = Nothing
          , addressCountry = Just "US"
          , addressPeriod = Nothing
          }
    it "encode" $ do
      J.encode Address {
          addressId = Nothing
        , addressExtension= []
        , addressUse = Just AuHome
        , addressType = Just AtPostal
        , addressText = Nothing
        , addressLine = ["Kerpenerstr. 62"]
        , addressCity = Just "Köln"
        , addressDistrict = Nothing
        , addressState = Just "NRW"
        , addressPostalCode = Just "50729"
        , addressCountry = Just "DEU"
--    ISO 3166 3 letter code  
        , addressPeriod = Nothing
        } `shouldBe` "{\"state\":\"NRW\",\"country\":\"DEU\",\"postalCode\":\"50729\",\"use\":\"home\",\"line\":[\"Kerpenerstr. 62\"],\"city\":\"K\195\182ln\",\"type\":\"postal\"}"

  describe "Backbone" $ do
    it "decode" $ do
      J.decode (BLU.fromString "{\"modifierExtension\":[],\"extension\":[],\"id\":\"123456\"}")
      `shouldBe` Just BackboneElement{
          backboneElementAttribs=[]
        , backboneElementId = Just "123456"
        , backboneElementExtension= []
        , backboneElementModifierExtension= []
        } 
    it "encode" $ do
      J.encode BackboneElement{
          backboneElementAttribs=[]
        , backboneElementId = Just "123456"
        , backboneElementExtension= []
        , backboneElementModifierExtension= []
        } `shouldBe` "{\"id\":\"123456\"}"


  describe "CodeableConcept" $ do
    it "decode" $ do
      J.decode (BLU.fromString "{\"text\":\"text text\",\"coding\":[{\"system\":\"orbis-pid\",\"code\":\"ORBISPID\"}]}")
      `shouldBe` Just CodeableConcept {
          codeableConceptAttribs= []
        , codeableConceptId= Nothing
        , codeableConceptExtension= []
        , codeableConceptCoding = [mkCoding "pid"]
        , codeableConceptText = Just "text text"
        }
    it "decode patient maritalStatus" $ do
      J.decode (BLU.fromString "{\"coding\":[{\"system\":\"http://terminology.hl7.org/CodeSystem/v3-MaritalStatus\",\"code\":\"S\",\"display\":\"Never Married\"}],\"text\":\"Never Married\"}")
      `shouldBe` Just CodeableConcept {
          codeableConceptAttribs= []
        , codeableConceptId= Nothing
        , codeableConceptExtension= []
        , codeableConceptCoding = [Coding {
                                     codingAttribs = []
                                   , codingId = Nothing
                                   , codingExtension= []
                                   , codingSystem = Just "http://terminology.hl7.org/CodeSystem/v3-MaritalStatus"
                                   , codingVersion = Nothing
                                   , codingCode = Just "S"
                                   , codingDisplay = Just "Never Married"
                                   , codingUserSelected = Nothing
                                   }]
        , codeableConceptText = Just "Never Married"}
    it "encode" $ do
      J.encode CodeableConcept {
          codeableConceptAttribs= []
        , codeableConceptId= Nothing
        , codeableConceptExtension= []
        , codeableConceptCoding = [mkCoding "pid"]
        , codeableConceptText = Just "text text"
        } `shouldBe` "{\"text\":\"text text\",\"coding\":[{\"system\":\"orbis-pid\",\"code\":\"ORBISPID\"}]}"

  describe "Coding" $ do
    it "decode" $ do
      J.decode (BLU.fromString "{\"display\":\"Dutch\",\"system\":\"urn:ietf:bcp:47\",\"version\":\"v7\",\"code\":\"nl-NL\"}")
      `shouldBe` Just Coding{
          codingAttribs = []
        , codingId = Nothing
        , codingExtension= []
        , codingSystem = Just "urn:ietf:bcp:47"
        , codingVersion = Just "v7"
        , codingCode = Just "nl-NL" 
        , codingDisplay = Just "Dutch" 
        , codingUserSelected = Nothing
        }
    it "encode" $ do
      J.encode Coding{
          codingAttribs = []
        , codingId = Nothing
        , codingExtension= []
        , codingSystem = Just "urn:ietf:bcp:47"
        , codingVersion = Just "v7"
        , codingCode = Just "nl-NL" 
        , codingDisplay = Just "Dutch" 
        , codingUserSelected = Nothing
        } `shouldBe` "{\"display\":\"Dutch\",\"system\":\"urn:ietf:bcp:47\",\"version\":\"v7\",\"code\":\"nl-NL\"}"

  describe "ContactPoint" $ do
    it "decode" $ do
      J.decode (BLU.fromString "{\"system\":\"phone\",\"use\":\"work\",\"value\":\"0221-478-42160\",\"rank\":1}")
      `shouldBe` Just ContactPoint{
          contactPointAttribs = []
        , contactPointId = Nothing
        , contactPointExtension= []
        , contactPointSystem = Just CpsPhone
        , contactPointValue = Just "0221-478-42160"
        , contactPointUse = Just CpuWork
        , contactPointRank = Just 1
        , contactPointPeriod = Nothing
        }
    it "decode patient telecom" $ do
      J.decode (BLU.fromString "{\"system\":\"phone\",\"value\":\"555-871-7175\",\"use\":\"home\"}")
      `shouldBe` Just ContactPoint{
          contactPointAttribs = []
        , contactPointId = Nothing
        , contactPointExtension = []
        , contactPointSystem = Just CpsPhone
        , contactPointValue = Just "555-871-7175"
        , contactPointUse = Just CpuHome
        , contactPointRank = Nothing
        , contactPointPeriod = Nothing
        }
    it "encode" $ do
      J.encode ContactPoint{
          contactPointAttribs = []
        , contactPointId = Nothing
        , contactPointExtension= []
        , contactPointSystem = Just CpsPhone
        , contactPointValue = Just "0221-478-42160"
        , contactPointUse = Just CpuWork
        , contactPointRank = Just 1
        , contactPointPeriod = Nothing
        } `shouldBe` "{\"system\":\"phone\",\"use\":\"work\",\"value\":\"0221-478-42160\",\"rank\":1}"

  describe "Extension" $ do
    it "decode ext code" $ do
      J.decode (BLU.fromString "{\"url\":\"http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex\",\"valueCode\":\"M\"}")
      `shouldBe` Just Extension {
          extensionUrl = "http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex"
        , extensionId  = Nothing
        , extensionExtension = []
        , extensionValue = Just $ ExtensionValueCode "M"
        }
    it "decode ext decimal" $ do
      J.decode (BLU.fromString "{\"url\":\"http://synthetichealth.github.io/synthea/quality-adjusted-life-years\",\"valueDecimal\":7.9263829061269355}")
      `shouldBe` Just Extension {
          extensionUrl = "http://synthetichealth.github.io/synthea/quality-adjusted-life-years"
        , extensionId  = Nothing
        , extensionExtension = []
        , extensionValue = Just $ ExtensionValueDecimal 7.9263829061269355
        }
    it "decode ext string" $ do
      J.decode (BLU.fromString "{\"url\":\"http://hl7.org/fhir/StructureDefinition/patient-mothersMaidenName\",\"valueString\":\"Carry843 O'Connell601\"}")
      `shouldBe` Just Extension {
          extensionUrl = "http://hl7.org/fhir/StructureDefinition/patient-mothersMaidenName"
        , extensionId  = Nothing
        , extensionExtension = []
        , extensionValue = Just $ ExtensionValueString "Carry843 O'Connell601"
        }
    it "decode complex ext" $ do
--      J.decode (BLU.fromString "\"extension\":[{\"url\":\"http://hl7.org/fhir/us/core/StructureDefinition/us-core-race\",\"extension\":[{\"url\":\"ombCategory\",\"valueCoding\":{\"system\":\"urn:oid:2.16.840.1.113883.6.238\",\"code\":\"2106-3\",\"display\":\"White\"}},{\"url\":\"text\",\"valueString\":\"White\"}]},{\"url\":\"http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity\",\"extension\":[{\"url\":\"ombCategory\",\"valueCoding\":{\"system\":\"urn:oid:2.16.840.1.113883.6.238\",\"code\":\"2186-5\",\"display\":\"Not Hispanic or Latino\"}},{\"url\":\"text\",\"valueString\":\"Not Hispanic or Latino\"}]},{ \"url\":\"http://hl7.org/fhir/StructureDefinition/patient-birthPlace\",\"valueAddress\":{\"city\":\"Dighton\",\"state\":\"Massachusetts\",\"country\":\"US\"}},{\"url\":\"http://synthetichealth.github.io/synthea/disability-adjusted-life-years\",\"valueDecimal\":0.07361709387306445},{\"url\":\"http://synthetichealth.github.io/synthea/quality-adjusted-life-years\",\"valueDecimal\":7.9263829061269355}]")
      J.decode (BLU.fromString "{\"url\":\"http://hl7.org/fhir/us/core/StructureDefinition/us-core-race\",\"extension\":[{\"url\":\"ombCategory\",\"valueCoding\":{\"system\":\"urn:oid:2.16.840.1.113883.6.238\",\"code\":\"2106-3\",\"display\":\"White\"}},{\"url\":\"text\",\"valueString\":\"White\"}]}")
       `shouldBe` Just Extension {
          extensionUrl = "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race"
        , extensionId = Nothing
        , extensionExtension = [
              Extension {
                  extensionUrl = "ombCategory"
                , extensionId = Nothing
                , extensionExtension = []
                , extensionValue = Just $
                        ExtensionValueCoding Coding {
                              codingAttribs = []
                            , codingId = Nothing
                            , codingExtension = []
                            , codingSystem = Just "urn:oid:2.16.840.1.113883.6.238"
                            , codingVersion = Nothing
                            , codingCode = Just "2106-3"
                            , codingDisplay = Just "White"
                            , codingUserSelected = Nothing
                            }
                }
            , Extension {
                  extensionUrl = "text"
                , extensionId = Nothing
                , extensionExtension = []
                , extensionValue = Just $ ExtensionValueString "White"
                }
            ]
        , extensionValue = Nothing
        }

    it "encode extension code" $ do
      J.encode Extension {
          extensionUrl = "http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex"
        , extensionId  = Nothing
        , extensionExtension = []
        , extensionValue = Just $ ExtensionValueCode "M"
        } `shouldBe` "{\"url\":\"http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex\",\"valueCode\":\"M\"}"

  describe "HumanName" $ do
    it "decode" $ do
      J.decode (BLU.fromString "{\"suffix\":[\"Jr.\"],\"text\":\"Dummy, Detlef, *2000-01-01\",\"period\":{\"start\":\"2021-04-01T09:00:00\"},\"prefix\":[\"Dr.med.\"],\"use\":\"official\",\"family\":\"Dummy\",\"given\":[\"Detlef\",\"Ralph\"]}")
      `shouldBe` Just HumanName {
          humanNameAttribs= []
        , humanNameId= Nothing
        , humanNameExtension= []
        , humanNameUse = Just NuOfficial
        , humanNameText = Just "Dummy, Detlef, *2000-01-01"
        , humanNameFamily = Just "Dummy"
        , humanNameGiven = ["Detlef","Ralph"]
        , humanNamePrefix = ["Dr.med."]
        , humanNameSuffix = ["Jr."]
        , humanNamePeriod = Just $ mkPeriod
        }
    it "decode patient name" $ do
      J.decode (BLU.fromString "{\"use\":\"official\",\"family\":\"Kiehn525\",\"given\":[\"Drew592\"]}")
      `shouldBe` Just HumanName {
          humanNameAttribs= []
        , humanNameId= Nothing
        , humanNameExtension= []
        , humanNameUse = Just NuOfficial
        , humanNameText = Nothing
        , humanNameFamily = Just "Kiehn525"
        , humanNameGiven = ["Drew592"]
        , humanNamePrefix = []
        , humanNameSuffix = []
        , humanNamePeriod = Nothing
        }
    it "encode" $ do
      J.encode HumanName {
          humanNameAttribs= []
        , humanNameId= Nothing
        , humanNameExtension= []
        , humanNameUse = Just NuOfficial
        , humanNameText = Just "Dummy, Detlef, *2000-01-01"
        , humanNameFamily = Just "Dummy"
        , humanNameGiven = ["Detlef","Ralph"]
        , humanNamePrefix = ["Dr.med."]
        , humanNameSuffix = ["Jr."]
        , humanNamePeriod = Just $ mkPeriod
        } `shouldBe` "{\"suffix\":[\"Jr.\"],\"text\":\"Dummy, Detlef, *2000-01-01\",\"period\":{\"start\":\"2021-04-01T09:00:00\"},\"prefix\":[\"Dr.med.\"],\"use\":\"official\",\"family\":\"Dummy\",\"given\":[\"Detlef\",\"Ralph\"]}"


  describe "Identifier" $ do
    it "decode" $ do
      J.decode (BLU.fromString "{\"system\":\"http://uk-koeln.de/ORBISPID\",\"use\":\"usual\",\"value\":\"6000001\",\"type\":{\"text\":\"pid\",\"coding\":[{\"system\":\"orbis-pid\",\"code\":\"ORBISPID\"}]}}")
      `shouldBe` Just Identifier{
          identifierAttribs= []
        , identifierId= Nothing
        , identifierExtension= []
        , identifierUse = Just IuUsual
        , identifierType = Just $ mkCodeableConcept "pid"
        , identifierSystem = Just "http://uk-koeln.de/ORBISPID"
        , identifierValue  = Just "6000001"
        , identifierPeriod  = Nothing
        , identifierAssigner = Nothing
        } 
    it "decode patient id" $ do
      J.decode (BLU.fromString "{\"type\":{\"coding\":[{\"system\":\"http://terminology.hl7.org/CodeSystem/v2-0203\",\"code\":\"MR\",\"display\":\"Medical Record Number\"}],\"text\":\"Medical Record Number\"},\"system\":\"http://hospital.smarthealthit.org\",\"value\":\"a0c9540e-c790-40f0-ba64-29a1fd6bea6a\"}")
      `shouldBe` Just Identifier{
          identifierAttribs= []
        , identifierId= Nothing
        , identifierExtension= []
        , identifierUse = Nothing
        , identifierType = Just (CodeableConcept {codeableConceptAttribs = [], codeableConceptId = Nothing, codeableConceptExtension = []
                                , codeableConceptCoding = [
                                    Coding {codingAttribs = []
                                           , codingId = Nothing
                                           , codingExtension = []
                                           , codingSystem = Just "http://terminology.hl7.org/CodeSystem/v2-0203"
                                           , codingVersion = Nothing
                                           , codingCode = Just "MR"
                                           , codingDisplay = Just "Medical Record Number"
                                           , codingUserSelected = Nothing
                                           }
                                    ]
                                , codeableConceptText = Just "Medical Record Number"
                                })
        , identifierSystem = Just "http://hospital.smarthealthit.org"
        , identifierValue = Just "a0c9540e-c790-40f0-ba64-29a1fd6bea6a"
        , identifierPeriod = Nothing
        , identifierAssigner = Nothing
        } 
    it "encode" $ do
      J.encode Identifier{
          identifierAttribs= []
        , identifierId= Nothing
        , identifierExtension= []
        , identifierUse = Just IuUsual
        , identifierType = Just $ mkCodeableConcept "pid"
        , identifierSystem = Just "http://uk-koeln.de/ORBISPID"
        , identifierValue  = Just "6000001"
        , identifierPeriod  = Nothing
        , identifierAssigner = Nothing
        } `shouldBe` 
            "{\"system\":\"http://uk-koeln.de/ORBISPID\",\"use\":\"usual\",\"value\":\"6000001\",\"type\":{\"text\":\"pid\",\"coding\":[{\"system\":\"orbis-pid\",\"code\":\"ORBISPID\"}]}}"

  describe "Meta" $ do
    it "decode" $ do
      J.decode (BLU.fromString "{\"lastUpdated\":\"2021-06-10T12:59:59\",\"versionId\":\"0\"}")
      `shouldBe` Just Meta{
          metaAttribs=[]
        , metaId= Nothing
        , metaExtension= []
        , metaVersionId=Just "0"
        , metaLastUpdated = Just "2021-06-10T12:59:59"
        , metaSource = Nothing
        , metaProfile = []
        , metaSecurity = []
        , metaTag = []
        }
    it "encode" $ do
      J.encode Meta{
          metaAttribs=[]
        , metaId= Nothing
        , metaExtension= []
        , metaVersionId=Just "0"
        , metaLastUpdated = Just "2021-06-10T12:59:59"
        , metaSource = Nothing
        , metaProfile = []
        , metaSecurity = []
        , metaTag = []
        } `shouldBe` "{\"lastUpdated\":\"2021-06-10T12:59:59\",\"versionId\":\"0\"}"

  describe "Narrative" $ do
    it "decode text" $ do
      J.decode (BLU.fromString "{\"status\":\"generated\",\"div\":\"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">this is text</div>\"}")
      `shouldBe` Just Narrative{
          narrativeAttribs = []
        , narrativeId = Nothing
        , narrativeExtension= []
        , narrativeStatus = NsGenerated
        , narrativeXhtmlDiv = XhtmlDiv (Div [] [FText "<div xmlns=\"http://www.w3.org/1999/xhtml\">this is text</div>"])
        }
    it "decode patient text" $ do
      J.decode (BLU.fromString "{\"status\":\"generated\",\"div\":\"<div xmlns=\\\"http://www.w3.org/1999/xhtml\\\">Generated by <a href=\\\"https://github.com/synthetichealth/synthea\\\">Synthea</a>.Version identifier: v2.4.0-272-gbd747730\\n .   Person seed: -6471729838080839651  Population seed: 1563971124564</div>\"}")
      `shouldBe` Just Narrative{
          narrativeAttribs = []
        , narrativeId = Nothing
        , narrativeExtension= []
        , narrativeStatus = NsGenerated
        , narrativeXhtmlDiv = XhtmlDiv (Div [] [FText "<div xmlns=\"http://www.w3.org/1999/xhtml\">Generated by <a href=\"https://github.com/synthetichealth/synthea\">Synthea</a>.Version identifier: v2.4.0-272-gbd747730\n .   Person seed: -6471729838080839651  Population seed: 1563971124564</div>"])
        }
    it "encode" $ do
      J.encode Narrative{
          narrativeAttribs = []
        , narrativeId = Nothing
        , narrativeExtension= []
        , narrativeStatus = NsGenerated
        , narrativeXhtmlDiv = mkXhtmlDiv "this is text"
        } `shouldBe` "{\"status\":\"generated\",\"div\":{\"div\":\"Div [] [FText \\\"this is text\\\"]\"}}"

  describe "Period" $ do
    it "decode" $ do
      J.decode (BLU.fromString "{\"start\":\"2021-04-01T09:00:00\"}")
      `shouldBe` Just Period{
          periodAttribs = []
        , periodId = Nothing
        , periodExtension= []
        , periodStart = Just "2021-04-01T09:00:00" 
        , periodEnd = Nothing
        }
    it "encode" $ do
      J.encode Period{
          periodAttribs = []
        , periodId = Nothing
        , periodExtension= []
        , periodStart = Just "2021-04-01T09:00:00" 
        , periodEnd = Nothing
        } `shouldBe` "{\"start\":\"2021-04-01T09:00:00\"}"

  describe "Reference" $ do
    it "decode" $ do
      J.decode (BLU.fromString "{\"display\":\"UKK Kinderklinik\",\"reference\":\"nabu/organizations/kikl\",\"id\":\"123\"}")
      `shouldBe` Just Reference{
          referenceAttribs = HM.fromList [("id","123")]
        , referenceExtension= []
        , referenceReference  = Just "nabu/organizations/kikl"
        , referenceType       = Nothing
        , referenceIdentifier = Nothing
        , referenceDisplay    = Just "UKK Kinderklinik"
        }
    it "encode" $ do
      J.encode Reference{
          referenceAttribs = HM.fromList [("id","123")]
        , referenceExtension= []
        , referenceReference  = Just "nabu/organizations/kikl"
        , referenceType       = Nothing
        , referenceIdentifier = Nothing
        , referenceDisplay    = Just "UKK Kinderklinik"
        } `shouldBe` 
             "{\"display\":\"UKK Kinderklinik\",\"reference\":\"nabu/organizations/kikl\",\"id\":\"123\"}"


-- \"extension\":[ { \"url\":\"http://hl7.org/fhir/us/core/StructureDefinition/us-core-race\"
--                 , \"extension\":[{\"url\":\"ombCategory\"
--                               ,\"valueCoding\":{\"system\":\"urn:oid:2.16.840.1.113883.6.238\",\"code\":\"2106-3\",\"display\":\"White\"}},{\"url\":\"text\",\"valueString\":\"White\"}]}
--               , { \"url\":\"http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity\"
--                 , \"extension\":[ { \"url\":\"ombCategory\"
--                                   , \"valueCoding\":{\"system\":\"urn:oid:2.16.840.1.113883.6.238\",\"code\":\"2186-5\",\"display\":\"Not Hispanic or Latino\"}}
--                                 , { \"url\":\"text\"
--                                   , \"valueString\":\"Not Hispanic or Latino\"}
--                                 ]
--                 }
--               , { \"url\":\"http://hl7.org/fhir/StructureDefinition/patient-birthPlace\"
--                 , \"valueAddress\":{\"city\":\"Dighton\",\"state\":\"Massachusetts\",\"country\":\"US\"}}
--               , { \"url\":\"http://synthetichealth.github.io/synthea/disability-adjusted-life-years\"
--                 , \"valueDecimal\":0.07361709387306445}
--               , { \"url\":\"http://synthetichealth.github.io/synthea/quality-adjusted-life-years\"
--                 , \"valueDecimal\":7.9263829061269355}]
