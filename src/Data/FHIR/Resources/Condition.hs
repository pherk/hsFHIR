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
-- FHIR 4.0.0 Condition
--

module Data.FHIR.Resources.Condition where

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

data ConditionOnset
    = ConditionOnsetDateTime DateTime
    | ConditionOnsetAge Age
    | ConditionOnsetPeriod Period
    | ConditionOnsetRange Range
    | ConditionOnsetString Text
    deriving (Eq, Show)

data ConditionAbatement
    = ConditionAbatementDateTime DateTime
    | ConditionAbatementAge Age
    | ConditionAbatementPeriod Period
    | ConditionAbatementRange Range
    | ConditionAbatementString Text
    deriving (Eq, Show)

data Condition = Condition {
    conditionId :: Maybe Id
  , conditionMeta :: Maybe Meta
  , conditionImplicitRules :: Maybe Uri
  , conditionLanguage :: Maybe Language
  , conditionText :: Maybe Narrative
--    conditionContained :: [ResourceContainer]
  , conditionExtension :: [Extension]
  , conditionModifierExtension :: [Extension]
  , conditionIdentifier :: [Identifier]
  , conditionClinicalStatus :: Maybe CodeableConcept
  , conditionVerificationStatus :: Maybe CodeableConcept
  , conditionCategory :: [CodeableConcept]
  , conditionSeverity :: Maybe CodeableConcept
  , conditionCode :: Maybe CodeableConcept
  , conditionBodySite :: [CodeableConcept]
  , conditionSubject :: Reference
  , conditionEncounter :: Maybe Reference
  , conditionOnset :: Maybe ConditionOnset
  , conditionAbatement :: Maybe ConditionAbatement
  , conditionRecordedDate :: Maybe DateTime
  , conditionRecorder :: Maybe Reference
  , conditionAsserter :: Maybe Reference
  , conditionStage :: [ConditionStage]
  , conditionEvidence :: [ConditionEvidence]
  , conditionNote :: [Annotation]
  }
--

instance ToJSON Condition where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Condition")
    ,  "id" .= toJSON (conditionId p)
    ,  "meta" .= toJSON (conditionMeta p)
    ,  "implicitRules" .= toJSON (conditionImplicitRules p)
    ,  "language" .= toJSON (conditionLanguage p)
    ,  "text" .= toJSON (conditionText p)
--    , "contained" .= toJSON (conditionContained p)
    ,  "extension" .= toJSON (conditionExtension p)
    ,  "modifierExtension" .= toJSON (conditionModifierExtension p)
    ,  "identifier" .= toJSON (conditionIdentifier p)
    ,  "clinicalStatus" .= toJSON (conditionClinicalStatus p)
    ,  "verificationStatus" .= toJSON (conditionVerificationStatus p)
    ,  "category" .= toJSON (conditionCategory p)
    ,  "severity" .= toJSON (conditionSeverity p)
    ,  "code" .= toJSON (conditionCode p)
    ,  "bodySite" .= toJSON (conditionBodySite p)
    ,  "subject" .= toJSON (conditionSubject p)
    ,  "encounter" .= toJSON (conditionEncounter p)
    , toOnsetJSON (conditionOnset p)
    , toAbatementJSON (conditionAbatement p)
    ,  "recordedDate" .= toJSON (conditionRecordedDate p)
    ,  "recorder" .= toJSON (conditionRecorder p)
    ,  "asserter" .= toJSON (conditionAsserter p)
    ,  "stage" .= toJSON (conditionStage p)
    ,  "evidence" .= toJSON (conditionEvidence p)
    ,  "note" .= toJSON (conditionNote p)
    ]
    where 
      toOnsetJSON (     Nothing   ) = ("onset", Null)
      toOnsetJSON (Just (ConditionOnsetDateTime c)) = ("onset", toJSON c)
      toOnsetJSON (Just (ConditionOnsetAge c)) = ("onset", toJSON c)
      toOnsetJSON (Just (ConditionOnsetPeriod c)) = ("onset", toJSON c)
      toOnsetJSON (Just (ConditionOnsetRange c)) = ("onset", toJSON c)
      toOnsetJSON (Just (ConditionOnsetString c)) = ("onset", toJSON c)
      toAbatementJSON (     Nothing   ) = ("abatement", Null)
      toAbatementJSON (Just (ConditionAbatementDateTime c)) = ("abatement", toJSON c)
      toAbatementJSON (Just (ConditionAbatementAge c)) = ("abatement", toJSON c)
      toAbatementJSON (Just (ConditionAbatementPeriod c)) = ("abatement", toJSON c)
      toAbatementJSON (Just (ConditionAbatementRange c)) = ("abatement", toJSON c)
      toAbatementJSON (Just (ConditionAbatementString c)) = ("abatement", toJSON c)
instance FromJSON Condition where
  parseJSON = withObject "Condition" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Condition" -> do
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
        category <- o .:? "category" .!= []
        severity <- o .:? "severity"
        code <- o .:? "code"
        bodySite <- o .:? "bodySite" .!= []
        subject <- o .:  "subject"
        encounter <- o .:? "encounter"
        onset <- parseOnset o
        abatement <- parseAbatement o
        recordedDate <- o .:? "recordedDate"
        recorder <- o .:? "recorder"
        asserter <- o .:? "asserter"
        stage <- o .:? "stage" .!= []
        evidence <- o .:? "evidence" .!= []
        note <- o .:? "note" .!= []
        return Condition{
            conditionId = id
          , conditionMeta = meta
          , conditionImplicitRules = implicitRules
          , conditionLanguage = language
          , conditionText = text
--          , conditionContained = contained
          , conditionExtension = extension
          , conditionModifierExtension = modifierExtension
          , conditionIdentifier = identifier
          , conditionClinicalStatus = clinicalStatus
          , conditionVerificationStatus = verificationStatus
          , conditionCategory = category
          , conditionSeverity = severity
          , conditionCode = code
          , conditionBodySite = bodySite
          , conditionSubject = subject
          , conditionEncounter = encounter
          , conditionOnset = onset
          , conditionAbatement = abatement
          , conditionRecordedDate = recordedDate
          , conditionRecorder = recorder
          , conditionAsserter = asserter
          , conditionStage = stage
          , conditionEvidence = evidence
          , conditionNote = note
          }
      _ -> fail "not a Condition"
    where 
      parseOnset o = parseOnsetDateTime o <|> parseOnsetAge o <|> parseOnsetPeriod o <|> parseOnsetRange o <|> parseOnsetString o
      parseOnsetDateTime o = do
                has <- o .: "onsetDateTime"
                return $ Just (ConditionOnsetDateTime has)
      parseOnsetAge o = do
                has <- o .: "onsetAge"
                return $ Just (ConditionOnsetAge has)
      parseOnsetPeriod o = do
                has <- o .: "onsetPeriod"
                return $ Just (ConditionOnsetPeriod has)
      parseOnsetRange o = do
                has <- o .: "onsetRange"
                return $ Just (ConditionOnsetRange has)
      parseOnsetString o = do
                has <- o .: "onsetString"
                return $ Just (ConditionOnsetString has)
      parseAbatement o = parseAbatementDateTime o <|> parseAbatementAge o <|> parseAbatementPeriod o <|> parseAbatementRange o <|> parseAbatementString o
      parseAbatementDateTime o = do
                has <- o .: "abatementDateTime"
                return $ Just (ConditionAbatementDateTime has)
      parseAbatementAge o = do
                has <- o .: "abatementAge"
                return $ Just (ConditionAbatementAge has)
      parseAbatementPeriod o = do
                has <- o .: "abatementPeriod"
                return $ Just (ConditionAbatementPeriod has)
      parseAbatementRange o = do
                has <- o .: "abatementRange"
                return $ Just (ConditionAbatementRange has)
      parseAbatementString o = do
                has <- o .: "abatementString"
                return $ Just (ConditionAbatementString has)
instance Xmlbf.ToXml Condition where
  toXml p = Xmlbf.element "Condition" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (conditionId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (conditionMeta p))
             , OptVal   "implicitRules" (fmap toUri (conditionImplicitRules p))
             , OptVal   "language" (fmap toLanguage (conditionLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (conditionText p))
--             , PropList "contained" (fmap Xmlbf.toXml (conditionContained p))
             , PropList "extension" (fmap Xmlbf.toXml (conditionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (conditionModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (conditionIdentifier p))
             , OptProp  "clinicalStatus" (fmap Xmlbf.toXml (conditionClinicalStatus p))
             , OptProp  "verificationStatus" (fmap Xmlbf.toXml (conditionVerificationStatus p))
             , PropList "category" (fmap Xmlbf.toXml (conditionCategory p))
             , OptProp  "severity" (fmap Xmlbf.toXml (conditionSeverity p))
             , OptProp  "code" (fmap Xmlbf.toXml (conditionCode p))
             , PropList "bodySite" (fmap Xmlbf.toXml (conditionBodySite p))
             , Prop     "subject" (HM.empty, Xmlbf.toXml (conditionSubject p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (conditionEncounter p))
             , toOnsetXml (conditionOnset p)
             , toAbatementXml (conditionAbatement p)
             , OptVal   "recordedDate" (fmap toDateTime (conditionRecordedDate p))
             , OptProp  "recorder" (fmap Xmlbf.toXml (conditionRecorder p))
             , OptProp  "asserter" (fmap Xmlbf.toXml (conditionAsserter p))
             , PropList "stage" (fmap Xmlbf.toXml (conditionStage p))
             , PropList "evidence" (fmap Xmlbf.toXml (conditionEvidence p))
             , PropList "note" (fmap Xmlbf.toXml (conditionNote p))
             ]
          toOnsetXml ( Nothing   ) = (OptVal "onset" Nothing)
          toOnsetXml (Just (ConditionOnsetDateTime p)) = Val   "onsetDateTime" (toDateTime p)
          toOnsetXml (Just (ConditionOnsetAge p)) = Prop  "onsetAge" (HM.empty, Xmlbf.toXml p)
          toOnsetXml (Just (ConditionOnsetPeriod p)) = Prop  "onsetPeriod" (HM.empty, Xmlbf.toXml p)
          toOnsetXml (Just (ConditionOnsetRange p)) = Prop  "onsetRange" (HM.empty, Xmlbf.toXml p)
          toOnsetXml (Just (ConditionOnsetString p)) = Val   "onsetString" (toString p)
          toAbatementXml ( Nothing   ) = (OptVal "abatement" Nothing)
          toAbatementXml (Just (ConditionAbatementDateTime p)) = Val   "abatementDateTime" (toDateTime p)
          toAbatementXml (Just (ConditionAbatementAge p)) = Prop  "abatementAge" (HM.empty, Xmlbf.toXml p)
          toAbatementXml (Just (ConditionAbatementPeriod p)) = Prop  "abatementPeriod" (HM.empty, Xmlbf.toXml p)
          toAbatementXml (Just (ConditionAbatementRange p)) = Prop  "abatementRange" (HM.empty, Xmlbf.toXml p)
          toAbatementXml (Just (ConditionAbatementString p)) = Val   "abatementString" (toString p)
instance Xmlbf.FromXml Condition where
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
    category <- many     $ Xmlbf.pElement "category" Xmlbf.fromXml
    severity <- optional $ Xmlbf.pElement "severity" Xmlbf.fromXml
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    bodySite <- many     $ Xmlbf.pElement "bodySite" Xmlbf.fromXml
    subject <-            Xmlbf.pElement "subject" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    onset <- fromOnsetXml
    abatement <- fromAbatementXml
    recordedDate <- optional $ Xmlbf.pElement "recordedDate" (Xmlbf.pAttr "value")
    recorder <- optional $ Xmlbf.pElement "recorder" Xmlbf.fromXml
    asserter <- optional $ Xmlbf.pElement "asserter" Xmlbf.fromXml
    stage <- many     $ Xmlbf.pElement "stage" Xmlbf.fromXml
    evidence <- many     $ Xmlbf.pElement "evidence" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    return Condition {
            conditionId = fmap fromId id
          , conditionMeta = meta
          , conditionImplicitRules = fmap fromUri implicitRules
          , conditionLanguage = fmap fromLanguage language
          , conditionText = text
--          , conditionContained = contained
          , conditionExtension = extension
          , conditionModifierExtension = modifierExtension
          , conditionIdentifier = identifier
          , conditionClinicalStatus = clinicalStatus
          , conditionVerificationStatus = verificationStatus
          , conditionCategory = category
          , conditionSeverity = severity
          , conditionCode = code
          , conditionBodySite = bodySite
          , conditionSubject = subject
          , conditionEncounter = encounter
          , conditionOnset = onset
          , conditionAbatement = abatement
          , conditionRecordedDate = fmap fromDateTime recordedDate
          , conditionRecorder = recorder
          , conditionAsserter = asserter
          , conditionStage = stage
          , conditionEvidence = evidence
          , conditionNote = note
          }

    where 
      fromOnsetXml = parseOnsetDateTime <|> parseOnsetAge <|> parseOnsetPeriod <|> parseOnsetRange <|> parseOnsetString <|> pure Nothing
      parseOnsetDateTime = do
                has <- Xmlbf.pElement "onsetDateTime" (Xmlbf.pAttr "value")
                return $ Just (ConditionOnsetDateTime (     toDateTime has))
      parseOnsetAge = do
                has <- Xmlbf.pElement "onsetAge" Xmlbf.fromXml
                return $ Just (ConditionOnsetAge (                      has))
      parseOnsetPeriod = do
                has <- Xmlbf.pElement "onsetPeriod" Xmlbf.fromXml
                return $ Just (ConditionOnsetPeriod (                      has))
      parseOnsetRange = do
                has <- Xmlbf.pElement "onsetRange" Xmlbf.fromXml
                return $ Just (ConditionOnsetRange (                      has))
      parseOnsetString = do
                has <- Xmlbf.pElement "onsetString" (Xmlbf.pAttr "value")
                return $ Just (ConditionOnsetString (     toString has))
      fromAbatementXml = parseAbatementDateTime <|> parseAbatementAge <|> parseAbatementPeriod <|> parseAbatementRange <|> parseAbatementString <|> pure Nothing
      parseAbatementDateTime = do
                has <- Xmlbf.pElement "abatementDateTime" (Xmlbf.pAttr "value")
                return $ Just (ConditionAbatementDateTime (     toDateTime has))
      parseAbatementAge = do
                has <- Xmlbf.pElement "abatementAge" Xmlbf.fromXml
                return $ Just (ConditionAbatementAge (                      has))
      parseAbatementPeriod = do
                has <- Xmlbf.pElement "abatementPeriod" Xmlbf.fromXml
                return $ Just (ConditionAbatementPeriod (                      has))
      parseAbatementRange = do
                has <- Xmlbf.pElement "abatementRange" Xmlbf.fromXml
                return $ Just (ConditionAbatementRange (                      has))
      parseAbatementString = do
                has <- Xmlbf.pElement "abatementString" (Xmlbf.pAttr "value")
                return $ Just (ConditionAbatementString (     toString has))


data ConditionStage = ConditionStage {
    conditionStageAttrId :: Maybe Text
  , conditionStageExtension :: [Extension]
  , conditionStageModifierExtension :: [Extension]
  , conditionStageSummary :: Maybe CodeableConcept
  , conditionStageAssessment :: [Reference]
  , conditionStageType :: Maybe CodeableConcept
  }
--

instance ToJSON ConditionStage where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (conditionStageAttrId p)
    ,  "extension" .= toJSON (conditionStageExtension p)
    ,  "modifierExtension" .= toJSON (conditionStageModifierExtension p)
    ,  "summary" .= toJSON (conditionStageSummary p)
    ,  "assessment" .= toJSON (conditionStageAssessment p)
    ,  "type" .= toJSON (conditionStageType p)
    ]
instance FromJSON ConditionStage where
  parseJSON = withObject "ConditionStage" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        summary <- o .:? "summary"
        assessment <- o .:? "assessment" .!= []
        ty <- o .:? "type"
        return ConditionStage{
            conditionStageAttrId = id
          , conditionStageExtension = extension
          , conditionStageModifierExtension = modifierExtension
          , conditionStageSummary = summary
          , conditionStageAssessment = assessment
          , conditionStageType = ty
          }
instance Xmlbf.ToXml ConditionStage where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (conditionStageAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (conditionStageExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (conditionStageModifierExtension p))
             , OptProp  "summary" (fmap Xmlbf.toXml (conditionStageSummary p))
             , PropList "assessment" (fmap Xmlbf.toXml (conditionStageAssessment p))
             , OptProp  "type" (fmap Xmlbf.toXml (conditionStageType p))
             ]
instance Xmlbf.FromXml ConditionStage where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    summary <- optional $ Xmlbf.pElement "summary" Xmlbf.fromXml
    assessment <- many     $ Xmlbf.pElement "assessment" Xmlbf.fromXml
    ty <- optional $ Xmlbf.pElement "type" Xmlbf.fromXml
    return ConditionStage {
            conditionStageAttrId = id
          , conditionStageExtension = extension
          , conditionStageModifierExtension = modifierExtension
          , conditionStageSummary = summary
          , conditionStageAssessment = assessment
          , conditionStageType = ty
          }



data ConditionEvidence = ConditionEvidence {
    conditionEvidenceAttrId :: Maybe Text
  , conditionEvidenceExtension :: [Extension]
  , conditionEvidenceModifierExtension :: [Extension]
  , conditionEvidenceCode :: [CodeableConcept]
  , conditionEvidenceDetail :: [Reference]
  }
--

instance ToJSON ConditionEvidence where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (conditionEvidenceAttrId p)
    ,  "extension" .= toJSON (conditionEvidenceExtension p)
    ,  "modifierExtension" .= toJSON (conditionEvidenceModifierExtension p)
    ,  "code" .= toJSON (conditionEvidenceCode p)
    ,  "detail" .= toJSON (conditionEvidenceDetail p)
    ]
instance FromJSON ConditionEvidence where
  parseJSON = withObject "ConditionEvidence" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:? "code" .!= []
        detail <- o .:? "detail" .!= []
        return ConditionEvidence{
            conditionEvidenceAttrId = id
          , conditionEvidenceExtension = extension
          , conditionEvidenceModifierExtension = modifierExtension
          , conditionEvidenceCode = code
          , conditionEvidenceDetail = detail
          }
instance Xmlbf.ToXml ConditionEvidence where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (conditionEvidenceAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (conditionEvidenceExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (conditionEvidenceModifierExtension p))
             , PropList "code" (fmap Xmlbf.toXml (conditionEvidenceCode p))
             , PropList "detail" (fmap Xmlbf.toXml (conditionEvidenceDetail p))
             ]
instance Xmlbf.FromXml ConditionEvidence where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <- many     $ Xmlbf.pElement "code" Xmlbf.fromXml
    detail <- many     $ Xmlbf.pElement "detail" Xmlbf.fromXml
    return ConditionEvidence {
            conditionEvidenceAttrId = id
          , conditionEvidenceExtension = extension
          , conditionEvidenceModifierExtension = modifierExtension
          , conditionEvidenceCode = code
          , conditionEvidenceDetail = detail
          }




