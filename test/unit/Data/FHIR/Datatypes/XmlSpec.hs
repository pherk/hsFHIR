{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.FHIR.Datatypes.XmlSpec where

import qualified Data.Aeson                   as J
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8    as BLU
import           Data.FHIR.Datatypes.Internal
import           Data.FHIR.Datatypes.XML
import           Data.FHIR.Datatypes.XhtmlDiv
import qualified Xmlbf.Xeno                   as X
import qualified Xmlbf                        as Xmlbf
import           RIO
import qualified RIO.HashMap                  as HM
import Data.Text.Encoding                     as TE
import Test.Hspec

--decode :: Text -> Resource
decode t = Xmlbf.runParser Xmlbf.fromXml $ fromRight [] $ X.fromRawXml (TE.encodeUtf8 t)

bsEncode :: [Xmlbf.Node] -> ByteString
bsEncode = BL.toStrict . BB.toLazyByteString . Xmlbf.encode


spec :: Spec
spec = do
  describe "Address" $ do
    it "decode" $ do
      decode "<use value=\"home\"/><type value=\"postal\"/><line value=\"Kerpenerstr. 62\"/><city value=\"Köln\"/><state value=\"NRW\"/><postalCode value=\"50729\"/><country value=\"DEU\"/>"
        `shouldBe` Right Address {
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
--      J.decode "{\"extension\":[{\"url\":\"http://hl7.org/fhir/StructureDefinition/geolocation\",\"extension\":[{\"url\":\"latitude\",\"valueDecimal\":41.76252653772401},{\"url\":\"longitude\",\"valueDecimal\":-70.02685367164186}]}],\"line\":[\"626 Jerde Lane Apt 51\"],\"city\":\"Harwich\",\"state\":\"Massachusetts\",\"country\":\"US\"}"
      decode "<line value=\"626 Jerde Lane Apt 51\"/><city value=\"Harwich\"/><state value=\"Massachusetts\"/><country value=\"US\"/>"
        `shouldBe` Right Address {
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
      bsEncode (Xmlbf.toXml Address {
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
        }) `shouldBe` "<use value=\"home\"></use><type value=\"postal\"></type><line value=\"Kerpenerstr. 62\"></line><city value=\"K\195\182ln\"></city><state value=\"NRW\"></state><postalCode value=\"50729\"></postalCode><country value=\"DEU\"></country>"


  describe "Backbone" $ do
    it "decode" $ do
      decode "<id value=\"123456\"/>"
      `shouldBe` Right BackboneElement{
          backboneElementAttribs=[]
        , backboneElementId = Just "123456"
        , backboneElementExtension= []
        , backboneElementModifierExtension= []
        } 
    it "encode" $ do
      bsEncode (Xmlbf.toXml BackboneElement{
          backboneElementAttribs=[]
        , backboneElementId = Just "123456"
        , backboneElementExtension= []
        , backboneElementModifierExtension= []
        }) `shouldBe` "<id value=\"123456\"></id>"


  describe "CodeableConcept" $ do
    it "decode" $ do
      decode "<coding><system value=\"orbis-pid\"></system><code value=\"ORBISPID\"></code></coding><text value=\"text text\"></text>"
      `shouldBe` Right CodeableConcept {
          codeableConceptAttribs= []
        , codeableConceptId= Nothing
        , codeableConceptExtension= []
        , codeableConceptCoding = [mkCoding "pid"]
        , codeableConceptText = Just "text text"
        }
    it "decode patient maritalStatus" $ do
      decode "<coding><system value=\"http://terminology.hl7.org/CodeSystem/v3-MaritalStatus\"/><code value=\"S\"/><display value=\"Never Married\"/></coding><text value=\"Never Married\"/>"
      `shouldBe` Right CodeableConcept {
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
      bsEncode (Xmlbf.toXml CodeableConcept {
          codeableConceptAttribs= []
        , codeableConceptId= Nothing
        , codeableConceptExtension= []
        , codeableConceptCoding = [mkCoding "pid"]
        , codeableConceptText = Just "text text"
        }) `shouldBe` "<coding><system value=\"orbis-pid\"></system><code value=\"ORBISPID\"></code></coding><text value=\"text text\"></text>"

  describe "Coding" $ do
    it "decode" $ do
      decode "<system value=\"urn:ietf:bcp:47\"></system><version value=\"v7\"></version><code value=\"nl-NL\"></code><display value=\"Dutch\"></display>"
      `shouldBe` Right Coding{
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
      bsEncode (Xmlbf.toXml Coding{
          codingAttribs = []
        , codingId = Nothing
        , codingExtension= []
        , codingSystem = Just "urn:ietf:bcp:47"
        , codingVersion = Just "v7"
        , codingCode = Just "nl-NL" 
        , codingDisplay = Just "Dutch" 
        , codingUserSelected = Nothing
        }) `shouldBe` "<system value=\"urn:ietf:bcp:47\"></system><version value=\"v7\"></version><code value=\"nl-NL\"></code><display value=\"Dutch\"></display>"

  describe "ContactPoint" $ do
    it "decode" $ do
      decode "<system value=\"phone\"></system><value value=\"0221-478-42160\"></value><use value=\"work\"></use><rank value=\"1\"></rank>"
      `shouldBe` Right ContactPoint{
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
      decode "<system value=\"phone\"/><value value=\"555-871-7175\"/><use value=\"home\"/>"
      `shouldBe` Right ContactPoint{
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
      bsEncode (Xmlbf.toXml ContactPoint{
          contactPointAttribs = []
        , contactPointId = Nothing
        , contactPointExtension= []
        , contactPointSystem = Just CpsPhone
        , contactPointValue = Just "0221-478-42160"
        , contactPointUse = Just CpuWork
        , contactPointRank = Just 1
        , contactPointPeriod = Nothing
        }) `shouldBe` "<system value=\"phone\"></system><value value=\"0221-478-42160\"></value><use value=\"work\"></use><rank value=\"1\"></rank>"

  describe "Extension" $ do
    it "decode ext code" $ do
      decode "<valueCode value=\"M\"/>"
      `shouldBe` Right Extension {
          extensionUrl = ""
        , extensionId  = Nothing
        , extensionExtension = []
        , extensionValue = Just $ ExtensionValueCode "M"
        }
    it "decode ext decimal" $ do
      decode "<valueDecimal value=\"7.9263829061269355\"/>"
      `shouldBe` Right Extension {
          extensionUrl = ""
        , extensionId  = Nothing
        , extensionExtension = []
        , extensionValue = Just $ ExtensionValueDecimal 7.9263829061269355
        }
    it "decode ext string" $ do
      decode "<valueString value=\"Carry843 O'Connell601\"/>"
      `shouldBe` Right Extension {
          extensionUrl = ""
        , extensionId  = Nothing
        , extensionExtension = []
        , extensionValue = Just $ ExtensionValueString "Carry843 O'Connell601"
        }
    it "decode complex ext" $ do
--      decode "\"extension\":[{\"url value=\"http://hl7.org/fhir/us/core/StructureDefinition/us-core-race\"/><extension\":[{\"url value=\"ombCategory\"/><valueCoding\":{\"system value=\"urn:oid:2.16.840.1.113883.6.238\"/><code value=\"2106-3\"/><display value=\"White\"}},{\"url value=\"text\"/><valueString value=\"White\"}]},{\"url value=\"http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity\"/><extension\":[{\"url value=\"ombCategory\"/><valueCoding\":{\"system value=\"urn:oid:2.16.840.1.113883.6.238\"/><code value=\"2186-5\"/><display value=\"Not Hispanic or Latino\"}},{\"url value=\"text\"/><valueString value=\"Not Hispanic or Latino\"}]},{ \"url value=\"http://hl7.org/fhir/StructureDefinition/patient-birthPlace\"/><valueAddress\":{\"city value=\"Dighton\"/><state value=\"Massachusetts\"/><country value=\"US\"}},{\"url value=\"http://synthetichealth.github.io/synthea/disability-adjusted-life-years\"/><valueDecimal\":0.07361709387306445},{\"url value=\"http://synthetichealth.github.io/synthea/quality-adjusted-life-years\"/><valueDecimal\":7.9263829061269355}]"
      decode "<extension url=\"http://hl7.org/fhir/us/core/StructureDefinition/us-core-race\"><extension url=\"ombCategory\"><valueCoding><system value=\"urn:oid:2.16.840.1.113883.6.238\"/><code value=\"2106-3\"/><display value=\"White\"/></valueCoding></extension><extension url=\"text\"><valueString value=\"White\"/></extension></extension>"
       `shouldBe` Right Extension {
          extensionUrl = ""
        , extensionId = Nothing
        , extensionExtension = [Extension{
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
            }]
        , extensionValue = Nothing
        }

    it "encode extension code" $ do
      bsEncode (Xmlbf.toXml Extension {
          extensionUrl = "http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex"
        , extensionId  = Nothing
        , extensionExtension = []
        , extensionValue = Just $ ExtensionValueCode "M"
        }) `shouldBe` "<valueCode value=\"M\"></valueCode>"

  describe "HumanName" $ do
    it "decode" $ do
      decode "<use value=\"official\"></use><text value=\"Dummy, Detlef, *2000-01-01\"></text><family value=\"Dummy\"></family><given value=\"Detlef\"></given><given value=\"Ralph\"></given><prefix value=\"Dr.med.\"></prefix><suffix value=\"Jr.\"></suffix><period><start value=\"2021-04-01T09:00:00\"></start><end value=\"2021-04-01T09:00:00\"></end></period>"
      `shouldBe` Right HumanName {
          humanNameAttribs= []
        , humanNameId= Nothing
        , humanNameExtension= []
        , humanNameUse = Just NuOfficial
        , humanNameText = Just "Dummy, Detlef, *2000-01-01"
        , humanNameFamily = Just "Dummy"
        , humanNameGiven = ["Detlef","Ralph"]
        , humanNamePrefix = ["Dr.med."]
        , humanNameSuffix = ["Jr."]
        , humanNamePeriod = Just (Period {
                  periodAttribs = []
                , periodId = Nothing
                , periodExtension = []
                , periodStart = Just "2021-04-01T09:00:00"
                , periodEnd = Just "2021-04-01T09:00:00"})
        }
    it "decode patient name" $ do
      decode "<use value=\"official\"/><family value=\"Kiehn525\"/><given value=\"Drew592\"/>"
      `shouldBe` Right HumanName {
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
      bsEncode (Xmlbf.toXml HumanName {
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
        }) `shouldBe` "<use value=\"official\"></use><text value=\"Dummy, Detlef, *2000-01-01\"></text><family value=\"Dummy\"></family><given value=\"Detlef\"></given><given value=\"Ralph\"></given><prefix value=\"Dr.med.\"></prefix><suffix value=\"Jr.\"></suffix><period><start value=\"2021-04-01T09:00:00\"></start></period>"


  describe "Identifier" $ do
    it "decode" $ do
      decode "<use value=\"usual\"></use><type><coding><system value=\"orbis-pid\"></system><code value=\"ORBISPID\"></code></coding><text value=\"pid\"></text></type><system value=\"http://uk-koeln.de/ORBISPID\"></system><value value=\"6000001\"></value>"
      `shouldBe` Right Identifier{
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
      decode "<type><coding><system value=\"http://terminology.hl7.org/CodeSystem/v2-0203\"/><code value=\"MR\"/><display value=\"Medical Record Number\"/></coding><text value=\"Medical Record Number\"/></type><system value=\"http://hospital.smarthealthit.org\"/><value value=\"a0c9540e-c790-40f0-ba64-29a1fd6bea6a\"/>"
      `shouldBe` Right Identifier{
          identifierAttribs= []
        , identifierId= Nothing
        , identifierExtension= []
        , identifierUse = Nothing
        , identifierType = Just CodeableConcept {codeableConceptAttribs = [], codeableConceptId = Nothing, codeableConceptExtension = []
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
                                }
        , identifierSystem = Just "http://hospital.smarthealthit.org"
        , identifierValue = Just "a0c9540e-c790-40f0-ba64-29a1fd6bea6a"
        , identifierPeriod = Nothing
        , identifierAssigner = Nothing
        } 
    it "encode" $ do
      bsEncode (Xmlbf.toXml Identifier{
          identifierAttribs= []
        , identifierId= Nothing
        , identifierExtension= []
        , identifierUse = Just IuUsual
        , identifierType = Just $ mkCodeableConcept "pid"
        , identifierSystem = Just "http://uk-koeln.de/ORBISPID"
        , identifierValue  = Just "6000001"
        , identifierPeriod  = Nothing
        , identifierAssigner = Nothing
        }) `shouldBe` "<use value=\"usual\"></use><type><coding><system value=\"orbis-pid\"></system><code value=\"ORBISPID\"></code></coding><text value=\"pid\"></text></type><system value=\"http://uk-koeln.de/ORBISPID\"></system><value value=\"6000001\"></value>"

  describe "Meta" $ do
    it "decode" $ do
      decode "<versionId value=\"0\"></versionId><lastUpdated value=\"2021-06-10T12:59:59\"></lastUpdated>"
      `shouldBe` Right Meta{
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
      bsEncode (Xmlbf.toXml Meta{
          metaAttribs=[]
        , metaId= Nothing
        , metaExtension= []
        , metaVersionId=Just "0"
        , metaLastUpdated = Just "2021-06-10T12:59:59"
        , metaSource = Nothing
        , metaProfile = []
        , metaSecurity = []
        , metaTag = []
        }) `shouldBe` "<versionId value=\"0\"></versionId><lastUpdated value=\"2021-06-10T12:59:59\"></lastUpdated>"

  describe "Narrative" $ do
    it "decode" $ do
      decode "<status value=\"generated\"></status><div xmlns=\"http://www.w3.org/1999/xhtml\">this is text</div>"
      `shouldBe` Right Narrative{
          narrativeAttribs = []
        , narrativeId = Nothing
        , narrativeExtension= []
        , narrativeStatus = NsGenerated
        , narrativeXhtmlDiv = XhtmlDiv (Div [] [FText "this is text"])
        }
    it "decode patient text" $ do
      decode "<status value=\"generated\"></status><div xmlns=\"http://www.w3.org/1999/xhtml\">Generated by <a href=\"https://github.com/synthetichealth/synthea\">Synthea</a>.Version identifier: v2.4.0-272-gbd747730.\\n   Person seed: -6471729838080839651  Population seed: 1563971124564</div>"
      `shouldBe` Right Narrative{
          narrativeAttribs = []
        , narrativeId = Nothing
        , narrativeExtension= []
        , narrativeStatus = NsGenerated
        , narrativeXhtmlDiv = XhtmlDiv (Div [] [FText "Generated by ",FI (IA (Anchor [] "Synthea")),FText ".Version identifier: v2.4.0-272-gbd747730.\\n   Person seed: -6471729838080839651  Population seed: 1563971124564"])
        }
    it "encode" $ do
      bsEncode (Xmlbf.toXml Narrative{
          narrativeAttribs = []
        , narrativeId = Nothing
        , narrativeExtension= []
        , narrativeStatus = NsGenerated
        , narrativeXhtmlDiv = XhtmlDiv (Div [] [FText "this is text"])
        }) `shouldBe` "<status value=\"generated\"></status><div xmlns=\"http://www.w3.org/1999/xhtml\">this is text</div>"


  describe "Period" $ do
    it "decode" $ do
      decode "<start value=\"2021-04-01T09:00:00\"></start><end value=\"2021-04-01T09:00:00\"></end>"
      `shouldBe` Right Period{
          periodAttribs = []
        , periodId = Nothing
        , periodExtension= []
        , periodStart = Just "2021-04-01T09:00:00" 
        , periodEnd = Just "2021-04-01T09:00:00" 
        }
    it "encode" $ do
      bsEncode (Xmlbf.toXml Period{
          periodAttribs = []
        , periodId = Nothing
        , periodExtension= []
        , periodStart = Just "2021-04-01T09:00:00" 
        , periodEnd = Nothing
        }) `shouldBe` "<start value=\"2021-04-01T09:00:00\"></start>"

  describe "Reference" $ do
    it "decode" $ do
      decode "<reference value=\"nabu/organizations/kikl\"></reference><display value=\"UKK Kinderklinik\"></display>"
      `shouldBe` Right Reference{
          referenceAttribs = HM.empty
        , referenceExtension= []
        , referenceReference  = Just "nabu/organizations/kikl"
        , referenceType       = Nothing
        , referenceIdentifier = Nothing
        , referenceDisplay    = Just "UKK Kinderklinik"
        }
    it "encode" $ do
      bsEncode (Xmlbf.toXml Reference{
          referenceAttribs = HM.fromList [("id","123")]
        , referenceExtension= []
        , referenceReference  = Just "nabu/organizations/kikl"
        , referenceType       = Nothing
        , referenceIdentifier = Nothing
        , referenceDisplay    = Just "UKK Kinderklinik"
        }) `shouldBe` "<reference value=\"nabu/organizations/kikl\"></reference><display value=\"UKK Kinderklinik\"></display>"


-- \"extension\":[ { \"url value=\"http://hl7.org/fhir/us/core/StructureDefinition/us-core-race\"
--                 , \"extension\":[{\"url value=\"ombCategory\"
--                               ,\"valueCoding\":{\"system value=\"urn:oid:2.16.840.1.113883.6.238\"/><code value=\"2106-3\"/><display value=\"White\"}},{\"url value=\"text\"/><valueString value=\"White\"}]}
--               , { \"url value=\"http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity\"
--                 , \"extension\":[ { \"url value=\"ombCategory\"
--                                   , \"valueCoding\":{\"system value=\"urn:oid:2.16.840.1.113883.6.238\"/><code value=\"2186-5\"/><display value=\"Not Hispanic or Latino\"}}
--                                 , { \"url value=\"text\"
--                                   , \"valueString value=\"Not Hispanic or Latino\"}
--                                 ]
--                 }
--               , { \"url value=\"http://hl7.org/fhir/StructureDefinition/patient-birthPlace\"
--                 , \"valueAddress\":{\"city value=\"Dighton\"/><state value=\"Massachusetts\"/><country value=\"US\"}}
--               , { \"url value=\"http://synthetichealth.github.io/synthea/disability-adjusted-life-years\"
--                 , \"valueDecimal\":0.07361709387306445}
--               , { \"url value=\"http://synthetichealth.github.io/synthea/quality-adjusted-life-years\"
--                 , \"valueDecimal\":7.9263829061269355}]
