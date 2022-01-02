{-# LANGUAGE NoImplicitPrelude  #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators     #-}

--
-- FHIR 4.0.0 AllergyIntolerance
--

module Data.FHIR.Resources.AllergyIntolerance where

import Data.Aeson
import Data.Aeson.Types hiding (parseJSON)

import qualified Data.HashMap.Strict as HM
import GHC.TypeLits

import RIO                  hiding(fromString)
import qualified RIO.Vector as V
import           Data.FHIR.Datatypes
import           Data.FHIR.Datatypes.XML
import           Data.FHIR.Datatypes.XmlUtils
import           Data.FHIR.Resources.DomainResource
import qualified Xmlbf  as Xmlbf

data AllergyIntoleranceType
    = AITAllergy
    | AITIntolerance
  deriving (Eq, Show)

instance ToJSON AllergyIntoleranceType where
    toJSON AITAllergy = String "allergy"
    toJSON AITIntolerance = String "intolerance"
instance FromJSON AllergyIntoleranceType where
    parseJSON "allergy" = return AITAllergy
    parseJSON "intolerance" = return AITIntolerance

toAllergyIntoleranceType AITAllergy = "allergy"
toAllergyIntoleranceType AITIntolerance = "intolerance"
fromAllergyIntoleranceType "allergy" = AITAllergy
fromAllergyIntoleranceType "intolerance" = AITIntolerance


data AllergyIntoleranceCategory
    = AICFood
    | AICMedication
    | AICEnvironment
    | AICBiologic
  deriving (Eq, Show)

instance ToJSON AllergyIntoleranceCategory where
    toJSON AICFood = String "food"
    toJSON AICMedication = String "medication"
    toJSON AICEnvironment = String "environment"
    toJSON AICBiologic = String "biologic"
instance FromJSON AllergyIntoleranceCategory where
    parseJSON "food" = return AICFood
    parseJSON "medication" = return AICMedication
    parseJSON "environment" = return AICEnvironment
    parseJSON "biologic" = return AICBiologic

toAllergyIntoleranceCategory AICFood = "food"
toAllergyIntoleranceCategory AICMedication = "medication"
toAllergyIntoleranceCategory AICEnvironment = "environment"
toAllergyIntoleranceCategory AICBiologic = "biologic"
fromAllergyIntoleranceCategory "food" = AICFood
fromAllergyIntoleranceCategory "medication" = AICMedication
fromAllergyIntoleranceCategory "environment" = AICEnvironment
fromAllergyIntoleranceCategory "biologic" = AICBiologic


data AllergyIntoleranceCriticality
    = AICLow
    | AICHigh
    | AICUnableToAssess
  deriving (Eq, Show)

instance ToJSON AllergyIntoleranceCriticality where
    toJSON AICLow = String "low"
    toJSON AICHigh = String "high"
    toJSON AICUnableToAssess = String "unable-to-assess"
instance FromJSON AllergyIntoleranceCriticality where
    parseJSON "low" = return AICLow
    parseJSON "high" = return AICHigh
    parseJSON "unable-to-assess" = return AICUnableToAssess

toAllergyIntoleranceCriticality AICLow = "low"
toAllergyIntoleranceCriticality AICHigh = "high"
toAllergyIntoleranceCriticality AICUnableToAssess = "unable-to-assess"
fromAllergyIntoleranceCriticality "low" = AICLow
fromAllergyIntoleranceCriticality "high" = AICHigh
fromAllergyIntoleranceCriticality "unable-to-assess" = AICUnableToAssess


data AllergyIntoleranceOnset
    = AllergyIntoleranceOnsetDateTime DateTime
    | AllergyIntoleranceOnsetAge Age
    | AllergyIntoleranceOnsetPeriod Period
    | AllergyIntoleranceOnsetRange Range
    | AllergyIntoleranceOnsetString Text
    deriving (Eq, Show)

data AllergyIntolerance = AllergyIntolerance {
    allergyIntoleranceId :: Maybe Id
  , allergyIntoleranceMeta :: Maybe Meta
  , allergyIntoleranceImplicitRules :: Maybe Uri
  , allergyIntoleranceLanguage :: Maybe Language
  , allergyIntoleranceText :: Maybe Narrative
--    allergyIntoleranceContained :: [ResourceContainer]
  , allergyIntoleranceExtension :: [Extension]
  , allergyIntoleranceModifierExtension :: [Extension]
  , allergyIntoleranceIdentifier :: [Identifier]
  , allergyIntoleranceClinicalStatus :: Maybe CodeableConcept
  , allergyIntoleranceVerificationStatus :: Maybe CodeableConcept
  , allergyIntoleranceType :: Maybe AllergyIntoleranceType
  , allergyIntoleranceCategory :: [AllergyIntoleranceCategory]
  , allergyIntoleranceCriticality :: Maybe AllergyIntoleranceCriticality
  , allergyIntoleranceCode :: Maybe CodeableConcept
  , allergyIntolerancePatient :: Reference
  , allergyIntoleranceEncounter :: Maybe Reference
  , allergyIntoleranceOnset :: Maybe AllergyIntoleranceOnset
  , allergyIntoleranceRecordedDate :: Maybe DateTime
  , allergyIntoleranceRecorder :: Maybe Reference
  , allergyIntoleranceAsserter :: Maybe Reference
  , allergyIntoleranceLastOccurrence :: Maybe DateTime
  , allergyIntoleranceNote :: [Annotation]
  , allergyIntoleranceReaction :: [AllergyIntoleranceReaction]
  }
--

instance ToJSON AllergyIntolerance where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "AllergyIntolerance")
    ,  "id" .= toJSON (allergyIntoleranceId p)
    ,  "meta" .= toJSON (allergyIntoleranceMeta p)
    ,  "implicitRules" .= toJSON (allergyIntoleranceImplicitRules p)
    ,  "language" .= toJSON (allergyIntoleranceLanguage p)
    ,  "text" .= toJSON (allergyIntoleranceText p)
--    , "contained" .= toJSON (allergyIntoleranceContained p)
    ,  "extension" .= toJSON (allergyIntoleranceExtension p)
    ,  "modifierExtension" .= toJSON (allergyIntoleranceModifierExtension p)
    ,  "identifier" .= toJSON (allergyIntoleranceIdentifier p)
    ,  "clinicalStatus" .= toJSON (allergyIntoleranceClinicalStatus p)
    ,  "verificationStatus" .= toJSON (allergyIntoleranceVerificationStatus p)
    ,  "type" .= toJSON (allergyIntoleranceType p)
    ,  "category" .= toJSON (allergyIntoleranceCategory p)
    ,  "criticality" .= toJSON (allergyIntoleranceCriticality p)
    ,  "code" .= toJSON (allergyIntoleranceCode p)
    ,  "patient" .= toJSON (allergyIntolerancePatient p)
    ,  "encounter" .= toJSON (allergyIntoleranceEncounter p)
    , toOnsetJSON (allergyIntoleranceOnset p)
    ,  "recordedDate" .= toJSON (allergyIntoleranceRecordedDate p)
    ,  "recorder" .= toJSON (allergyIntoleranceRecorder p)
    ,  "asserter" .= toJSON (allergyIntoleranceAsserter p)
    ,  "lastOccurrence" .= toJSON (allergyIntoleranceLastOccurrence p)
    ,  "note" .= toJSON (allergyIntoleranceNote p)
    ,  "reaction" .= toJSON (allergyIntoleranceReaction p)
    ]
    where 
      toOnsetJSON (     Nothing   ) = ("onset", Null)
      toOnsetJSON (Just (AllergyIntoleranceOnsetDateTime c)) = ("onset", toJSON c)
      toOnsetJSON (Just (AllergyIntoleranceOnsetAge c)) = ("onset", toJSON c)
      toOnsetJSON (Just (AllergyIntoleranceOnsetPeriod c)) = ("onset", toJSON c)
      toOnsetJSON (Just (AllergyIntoleranceOnsetRange c)) = ("onset", toJSON c)
      toOnsetJSON (Just (AllergyIntoleranceOnsetString c)) = ("onset", toJSON c)
instance FromJSON AllergyIntolerance where
  parseJSON = withObject "AllergyIntolerance" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "AllergyIntolerance" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        clinicalStatus <- o .:? "clinicalStatus"
        verificationStatus <- o .:? "verificationStatus"
        ty <- o .:? "type"
        category <- o .:? "category" .!= []
        criticality <- o .:? "criticality"
        code <- o .:? "code"
        patient <- o .:  "patient"
        encounter <- o .:? "encounter"
        onset <- parseOnset o
        recordedDate <- o .:? "recordedDate"
        recorder <- o .:? "recorder"
        asserter <- o .:? "asserter"
        lastOccurrence <- o .:? "lastOccurrence"
        note <- o .:? "note" .!= []
        reaction <- o .:? "reaction" .!= []
        return AllergyIntolerance{
            allergyIntoleranceId = id
          , allergyIntoleranceMeta = meta
          , allergyIntoleranceImplicitRules = implicitRules
          , allergyIntoleranceLanguage = language
          , allergyIntoleranceText = text
--          , allergyIntoleranceContained = contained
          , allergyIntoleranceExtension = extension
          , allergyIntoleranceModifierExtension = modifierExtension
          , allergyIntoleranceIdentifier = identifier
          , allergyIntoleranceClinicalStatus = clinicalStatus
          , allergyIntoleranceVerificationStatus = verificationStatus
          , allergyIntoleranceType = ty
          , allergyIntoleranceCategory = category
          , allergyIntoleranceCriticality = criticality
          , allergyIntoleranceCode = code
          , allergyIntolerancePatient = patient
          , allergyIntoleranceEncounter = encounter
          , allergyIntoleranceOnset = onset
          , allergyIntoleranceRecordedDate = recordedDate
          , allergyIntoleranceRecorder = recorder
          , allergyIntoleranceAsserter = asserter
          , allergyIntoleranceLastOccurrence = lastOccurrence
          , allergyIntoleranceNote = note
          , allergyIntoleranceReaction = reaction
          }
      _ -> fail "not a AllergyIntolerance"
    where 
      parseOnset o = parseOnsetDateTime o <|> parseOnsetAge o <|> parseOnsetPeriod o <|> parseOnsetRange o <|> parseOnsetString o
      parseOnsetDateTime o = do
                has <- o .: "onsetDateTime"
                return $ Just (AllergyIntoleranceOnsetDateTime has)
      parseOnsetAge o = do
                has <- o .: "onsetAge"
                return $ Just (AllergyIntoleranceOnsetAge has)
      parseOnsetPeriod o = do
                has <- o .: "onsetPeriod"
                return $ Just (AllergyIntoleranceOnsetPeriod has)
      parseOnsetRange o = do
                has <- o .: "onsetRange"
                return $ Just (AllergyIntoleranceOnsetRange has)
      parseOnsetString o = do
                has <- o .: "onsetString"
                return $ Just (AllergyIntoleranceOnsetString has)
instance Xmlbf.ToXml AllergyIntolerance where
  toXml p = Xmlbf.element "AllergyIntolerance" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (allergyIntoleranceId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (allergyIntoleranceMeta p))
             , OptVal   "implicitRules" (fmap toUri (allergyIntoleranceImplicitRules p))
             , OptVal   "language" (fmap toLanguage (allergyIntoleranceLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (allergyIntoleranceText p))
--             , PropList "contained" (fmap Xmlbf.toXml (allergyIntoleranceContained p))
             , PropList "extension" (fmap Xmlbf.toXml (allergyIntoleranceExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (allergyIntoleranceModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (allergyIntoleranceIdentifier p))
             , OptProp  "clinicalStatus" (fmap Xmlbf.toXml (allergyIntoleranceClinicalStatus p))
             , OptProp  "verificationStatus" (fmap Xmlbf.toXml (allergyIntoleranceVerificationStatus p))
             , OptVal   "type" (fmap toAllergyIntoleranceType (allergyIntoleranceType p))
             , ValList  "category" (fmap toAllergyIntoleranceCategory (allergyIntoleranceCategory p))
             , OptVal   "criticality" (fmap toAllergyIntoleranceCriticality (allergyIntoleranceCriticality p))
             , OptProp  "code" (fmap Xmlbf.toXml (allergyIntoleranceCode p))
             , Prop     "patient" (HM.empty, Xmlbf.toXml (allergyIntolerancePatient p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (allergyIntoleranceEncounter p))
             , toOnsetXml (allergyIntoleranceOnset p)
             , OptVal   "recordedDate" (fmap toDateTime (allergyIntoleranceRecordedDate p))
             , OptProp  "recorder" (fmap Xmlbf.toXml (allergyIntoleranceRecorder p))
             , OptProp  "asserter" (fmap Xmlbf.toXml (allergyIntoleranceAsserter p))
             , OptVal   "lastOccurrence" (fmap toDateTime (allergyIntoleranceLastOccurrence p))
             , PropList "note" (fmap Xmlbf.toXml (allergyIntoleranceNote p))
             , PropList "reaction" (fmap Xmlbf.toXml (allergyIntoleranceReaction p))
             ]
          toOnsetXml ( Nothing   ) = (OptVal "onset" Nothing)
          toOnsetXml (Just (AllergyIntoleranceOnsetDateTime p)) = Val   "onsetDateTime" (toDateTime p)
          toOnsetXml (Just (AllergyIntoleranceOnsetAge p)) = Prop  "onsetAge" (HM.empty, Xmlbf.toXml p)
          toOnsetXml (Just (AllergyIntoleranceOnsetPeriod p)) = Prop  "onsetPeriod" (HM.empty, Xmlbf.toXml p)
          toOnsetXml (Just (AllergyIntoleranceOnsetRange p)) = Prop  "onsetRange" (HM.empty, Xmlbf.toXml p)
          toOnsetXml (Just (AllergyIntoleranceOnsetString p)) = Val   "onsetString" (toString p)
instance Xmlbf.FromXml AllergyIntolerance where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" Xmlbf.fromXml
--    contained <- many     $ Xmlbf.pElement "contained" Xmlbf.fromXml
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    identifier <- many     $ Xmlbf.pElement "identifier" Xmlbf.fromXml
    clinicalStatus <- optional $ Xmlbf.pElement "clinicalStatus" Xmlbf.fromXml
    verificationStatus <- optional $ Xmlbf.pElement "verificationStatus" Xmlbf.fromXml
    ty <- optional $ Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    category <- many     $ Xmlbf.pElement "category" (Xmlbf.pAttr "value")
    criticality <- optional $ Xmlbf.pElement "criticality" (Xmlbf.pAttr "value")
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    patient <-            Xmlbf.pElement "patient" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    onset <- fromOnsetXml
    recordedDate <- optional $ Xmlbf.pElement "recordedDate" (Xmlbf.pAttr "value")
    recorder <- optional $ Xmlbf.pElement "recorder" Xmlbf.fromXml
    asserter <- optional $ Xmlbf.pElement "asserter" Xmlbf.fromXml
    lastOccurrence <- optional $ Xmlbf.pElement "lastOccurrence" (Xmlbf.pAttr "value")
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    reaction <- many     $ Xmlbf.pElement "reaction" Xmlbf.fromXml
    return AllergyIntolerance {
            allergyIntoleranceId = fmap fromId id
          , allergyIntoleranceMeta = meta
          , allergyIntoleranceImplicitRules = fmap fromUri implicitRules
          , allergyIntoleranceLanguage = fmap fromLanguage language
          , allergyIntoleranceText = text
--          , allergyIntoleranceContained = contained
          , allergyIntoleranceExtension = extension
          , allergyIntoleranceModifierExtension = modifierExtension
          , allergyIntoleranceIdentifier = identifier
          , allergyIntoleranceClinicalStatus = clinicalStatus
          , allergyIntoleranceVerificationStatus = verificationStatus
          , allergyIntoleranceType = fmap fromAllergyIntoleranceType ty
          , allergyIntoleranceCategory = fmap fromAllergyIntoleranceCategory category
          , allergyIntoleranceCriticality = fmap fromAllergyIntoleranceCriticality criticality
          , allergyIntoleranceCode = code
          , allergyIntolerancePatient = patient
          , allergyIntoleranceEncounter = encounter
          , allergyIntoleranceOnset = onset
          , allergyIntoleranceRecordedDate = fmap fromDateTime recordedDate
          , allergyIntoleranceRecorder = recorder
          , allergyIntoleranceAsserter = asserter
          , allergyIntoleranceLastOccurrence = fmap fromDateTime lastOccurrence
          , allergyIntoleranceNote = note
          , allergyIntoleranceReaction = reaction
          }

    where 
      fromOnsetXml = parseOnsetDateTime <|> parseOnsetAge <|> parseOnsetPeriod <|> parseOnsetRange <|> parseOnsetString <|> pure Nothing
      parseOnsetDateTime = do
                has <- Xmlbf.pElement "onsetDateTime" (Xmlbf.pAttr "value")
                return $ Just (AllergyIntoleranceOnsetDateTime (     toDateTime has))
      parseOnsetAge = do
                has <- Xmlbf.pElement "onsetAge" Xmlbf.fromXml
                return $ Just (AllergyIntoleranceOnsetAge (                      has))
      parseOnsetPeriod = do
                has <- Xmlbf.pElement "onsetPeriod" Xmlbf.fromXml
                return $ Just (AllergyIntoleranceOnsetPeriod (                      has))
      parseOnsetRange = do
                has <- Xmlbf.pElement "onsetRange" Xmlbf.fromXml
                return $ Just (AllergyIntoleranceOnsetRange (                      has))
      parseOnsetString = do
                has <- Xmlbf.pElement "onsetString" (Xmlbf.pAttr "value")
                return $ Just (AllergyIntoleranceOnsetString (     toString has))


data AllergyIntoleranceReactionSeverity
    = AIRSMild
    | AIRSModerate
    | AIRSSevere
  deriving (Eq, Show)

instance ToJSON AllergyIntoleranceReactionSeverity where
    toJSON AIRSMild = String "mild"
    toJSON AIRSModerate = String "moderate"
    toJSON AIRSSevere = String "severe"
instance FromJSON AllergyIntoleranceReactionSeverity where
    parseJSON "mild" = return AIRSMild
    parseJSON "moderate" = return AIRSModerate
    parseJSON "severe" = return AIRSSevere

toAllergyIntoleranceReactionSeverity AIRSMild = "mild"
toAllergyIntoleranceReactionSeverity AIRSModerate = "moderate"
toAllergyIntoleranceReactionSeverity AIRSSevere = "severe"
fromAllergyIntoleranceReactionSeverity "mild" = AIRSMild
fromAllergyIntoleranceReactionSeverity "moderate" = AIRSModerate
fromAllergyIntoleranceReactionSeverity "severe" = AIRSSevere


data AllergyIntoleranceReaction = AllergyIntoleranceReaction {
    allergyIntoleranceReactionAttrId :: Maybe Text
  , allergyIntoleranceReactionExtension :: [Extension]
  , allergyIntoleranceReactionModifierExtension :: [Extension]
  , allergyIntoleranceReactionSubstance :: Maybe CodeableConcept
  , allergyIntoleranceReactionManifestation :: [CodeableConcept]
  , allergyIntoleranceReactionDescription :: Maybe Text
  , allergyIntoleranceReactionOnset :: Maybe DateTime
  , allergyIntoleranceReactionSeverity :: Maybe AllergyIntoleranceReactionSeverity
  , allergyIntoleranceReactionExposureRoute :: Maybe CodeableConcept
  , allergyIntoleranceReactionNote :: [Annotation]
  }
--

instance ToJSON AllergyIntoleranceReaction where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (allergyIntoleranceReactionAttrId p)
    ,  "extension" .= toJSON (allergyIntoleranceReactionExtension p)
    ,  "modifierExtension" .= toJSON (allergyIntoleranceReactionModifierExtension p)
    ,  "substance" .= toJSON (allergyIntoleranceReactionSubstance p)
    ,  "manifestation" .= toJSON (allergyIntoleranceReactionManifestation p)
    ,  "description" .= toJSON (allergyIntoleranceReactionDescription p)
    ,  "onset" .= toJSON (allergyIntoleranceReactionOnset p)
    ,  "severity" .= toJSON (allergyIntoleranceReactionSeverity p)
    ,  "exposureRoute" .= toJSON (allergyIntoleranceReactionExposureRoute p)
    ,  "note" .= toJSON (allergyIntoleranceReactionNote p)
    ]
instance FromJSON AllergyIntoleranceReaction where
  parseJSON = withObject "AllergyIntoleranceReaction" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        substance <- o .:? "substance"
        manifestation <- o .:? "manifestation" .!= []
        description <- o .:? "description"
        onset <- o .:? "onset"
        severity <- o .:? "severity"
        exposureRoute <- o .:? "exposureRoute"
        note <- o .:? "note" .!= []
        return AllergyIntoleranceReaction{
            allergyIntoleranceReactionAttrId = id
          , allergyIntoleranceReactionExtension = extension
          , allergyIntoleranceReactionModifierExtension = modifierExtension
          , allergyIntoleranceReactionSubstance = substance
          , allergyIntoleranceReactionManifestation = manifestation
          , allergyIntoleranceReactionDescription = description
          , allergyIntoleranceReactionOnset = onset
          , allergyIntoleranceReactionSeverity = severity
          , allergyIntoleranceReactionExposureRoute = exposureRoute
          , allergyIntoleranceReactionNote = note
          }
instance Xmlbf.ToXml AllergyIntoleranceReaction where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (allergyIntoleranceReactionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (allergyIntoleranceReactionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (allergyIntoleranceReactionModifierExtension p))
             , OptProp  "substance" (fmap Xmlbf.toXml (allergyIntoleranceReactionSubstance p))
             , PropList "manifestation" (fmap Xmlbf.toXml (allergyIntoleranceReactionManifestation p))
             , OptVal   "description" (fmap toString (allergyIntoleranceReactionDescription p))
             , OptVal   "onset" (fmap toDateTime (allergyIntoleranceReactionOnset p))
             , OptVal   "severity" (fmap toAllergyIntoleranceReactionSeverity (allergyIntoleranceReactionSeverity p))
             , OptProp  "exposureRoute" (fmap Xmlbf.toXml (allergyIntoleranceReactionExposureRoute p))
             , PropList "note" (fmap Xmlbf.toXml (allergyIntoleranceReactionNote p))
             ]
instance Xmlbf.FromXml AllergyIntoleranceReaction where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    substance <- optional $ Xmlbf.pElement "substance" Xmlbf.fromXml
    manifestation <- many     $ Xmlbf.pElement "manifestation" Xmlbf.fromXml
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    onset <- optional $ Xmlbf.pElement "onset" (Xmlbf.pAttr "value")
    severity <- optional $ Xmlbf.pElement "severity" (Xmlbf.pAttr "value")
    exposureRoute <- optional $ Xmlbf.pElement "exposureRoute" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    return AllergyIntoleranceReaction {
            allergyIntoleranceReactionAttrId = id
          , allergyIntoleranceReactionExtension = extension
          , allergyIntoleranceReactionModifierExtension = modifierExtension
          , allergyIntoleranceReactionSubstance = substance
          , allergyIntoleranceReactionManifestation = manifestation
          , allergyIntoleranceReactionDescription = fmap fromString description
          , allergyIntoleranceReactionOnset = fmap fromDateTime onset
          , allergyIntoleranceReactionSeverity = fmap fromAllergyIntoleranceReactionSeverity severity
          , allergyIntoleranceReactionExposureRoute = exposureRoute
          , allergyIntoleranceReactionNote = note
          }




