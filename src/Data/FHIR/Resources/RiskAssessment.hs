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
-- FHIR 4.0.0 RiskAssessment
--

module Data.FHIR.Resources.RiskAssessment where

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

data RiskAssessmentStatus
    = RASRegistered
    | RASPreliminary
    | RASFinal
    | RASAmended
    | RASCorrected
    | RASCancelled
    | RASEnteredInError
    | RASUnknown
  deriving (Eq, Show)

instance ToJSON RiskAssessmentStatus where
    toJSON RASRegistered = String "registered"
    toJSON RASPreliminary = String "preliminary"
    toJSON RASFinal = String "final"
    toJSON RASAmended = String "amended"
    toJSON RASCorrected = String "corrected"
    toJSON RASCancelled = String "cancelled"
    toJSON RASEnteredInError = String "entered-in-error"
    toJSON RASUnknown = String "unknown"
instance FromJSON RiskAssessmentStatus where
    parseJSON "registered" = return RASRegistered
    parseJSON "preliminary" = return RASPreliminary
    parseJSON "final" = return RASFinal
    parseJSON "amended" = return RASAmended
    parseJSON "corrected" = return RASCorrected
    parseJSON "cancelled" = return RASCancelled
    parseJSON "entered-in-error" = return RASEnteredInError
    parseJSON "unknown" = return RASUnknown

toRiskAssessmentStatus RASRegistered = "registered"
toRiskAssessmentStatus RASPreliminary = "preliminary"
toRiskAssessmentStatus RASFinal = "final"
toRiskAssessmentStatus RASAmended = "amended"
toRiskAssessmentStatus RASCorrected = "corrected"
toRiskAssessmentStatus RASCancelled = "cancelled"
toRiskAssessmentStatus RASEnteredInError = "entered-in-error"
toRiskAssessmentStatus RASUnknown = "unknown"
fromRiskAssessmentStatus "registered" = RASRegistered
fromRiskAssessmentStatus "preliminary" = RASPreliminary
fromRiskAssessmentStatus "final" = RASFinal
fromRiskAssessmentStatus "amended" = RASAmended
fromRiskAssessmentStatus "corrected" = RASCorrected
fromRiskAssessmentStatus "cancelled" = RASCancelled
fromRiskAssessmentStatus "entered-in-error" = RASEnteredInError
fromRiskAssessmentStatus "unknown" = RASUnknown


data RiskAssessmentOccurrence
    = RiskAssessmentOccurrenceDateTime DateTime
    | RiskAssessmentOccurrencePeriod Period
    deriving (Eq, Show)

data RiskAssessment = RiskAssessment {
    riskAssessmentId :: Maybe Id
  , riskAssessmentMeta :: Maybe Meta
  , riskAssessmentImplicitRules :: Maybe Uri
  , riskAssessmentLanguage :: Maybe Language
  , riskAssessmentText :: Maybe Narrative
--    riskAssessmentContained :: [ResourceContainer]
  , riskAssessmentExtension :: [Extension]
  , riskAssessmentModifierExtension :: [Extension]
  , riskAssessmentIdentifier :: [Identifier]
  , riskAssessmentBasedOn :: Maybe Reference
  , riskAssessmentParent :: Maybe Reference
  , riskAssessmentStatus :: RiskAssessmentStatus
  , riskAssessmentMethod :: Maybe CodeableConcept
  , riskAssessmentCode :: Maybe CodeableConcept
  , riskAssessmentSubject :: Reference
  , riskAssessmentEncounter :: Maybe Reference
  , riskAssessmentOccurrenceDateTime :: Maybe DateTime
  , riskAssessmentOccurrencePeriod :: Maybe Period
  , riskAssessmentCondition :: Maybe Reference
  , riskAssessmentPerformer :: Maybe Reference
  , riskAssessmentReasonCode :: [CodeableConcept]
  , riskAssessmentReasonReference :: [Reference]
  , riskAssessmentBasis :: [Reference]
  , riskAssessmentPrediction :: [RiskAssessmentPrediction]
  , riskAssessmentMitigation :: Maybe Text
  , riskAssessmentNote :: [Annotation]
  }
--

instance ToJSON RiskAssessment where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "RiskAssessment")
    ,  "id" .= toJSON (riskAssessmentId p)
    ,  "meta" .= toJSON (riskAssessmentMeta p)
    ,  "implicitRules" .= toJSON (riskAssessmentImplicitRules p)
    ,  "language" .= toJSON (riskAssessmentLanguage p)
    ,  "text" .= toJSON (riskAssessmentText p)
--    , "contained" .= toJSON (riskAssessmentContained p)
    ,  "extension" .= toJSON (riskAssessmentExtension p)
    ,  "modifierExtension" .= toJSON (riskAssessmentModifierExtension p)
    ,  "identifier" .= toJSON (riskAssessmentIdentifier p)
    ,  "basedOn" .= toJSON (riskAssessmentBasedOn p)
    ,  "parent" .= toJSON (riskAssessmentParent p)
    ,  "status" .= toJSON (riskAssessmentStatus p)
    ,  "method" .= toJSON (riskAssessmentMethod p)
    ,  "code" .= toJSON (riskAssessmentCode p)
    ,  "subject" .= toJSON (riskAssessmentSubject p)
    ,  "encounter" .= toJSON (riskAssessmentEncounter p)
    ,  "occurrenceDateTime" .= toJSON (riskAssessmentOccurrenceDateTime p)
    ,  "occurrencePeriod" .= toJSON (riskAssessmentOccurrencePeriod p)
    ,  "condition" .= toJSON (riskAssessmentCondition p)
    ,  "performer" .= toJSON (riskAssessmentPerformer p)
    ,  "reasonCode" .= toJSON (riskAssessmentReasonCode p)
    ,  "reasonReference" .= toJSON (riskAssessmentReasonReference p)
    ,  "basis" .= toJSON (riskAssessmentBasis p)
    ,  "prediction" .= toJSON (riskAssessmentPrediction p)
    ,  "mitigation" .= toJSON (riskAssessmentMitigation p)
    ,  "note" .= toJSON (riskAssessmentNote p)
    ]
instance FromJSON RiskAssessment where
  parseJSON = withObject "RiskAssessment" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "RiskAssessment" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        basedOn <- o .:? "basedOn"
        parent <- o .:? "parent"
        status <- o .:  "status"
        method <- o .:? "method"
        code <- o .:? "code"
        subject <- o .:  "subject"
        encounter <- o .:? "encounter"
        occurrenceDateTime <- o .:? "occurrenceDateTime"
        occurrencePeriod <- o .:? "occurrencePeriod"
        condition <- o .:? "condition"
        performer <- o .:? "performer"
        reasonCode <- o .:? "reasonCode" .!= []
        reasonReference <- o .:? "reasonReference" .!= []
        basis <- o .:? "basis" .!= []
        prediction <- o .:? "prediction" .!= []
        mitigation <- o .:? "mitigation"
        note <- o .:? "note" .!= []
        return RiskAssessment{
            riskAssessmentId = id
          , riskAssessmentMeta = meta
          , riskAssessmentImplicitRules = implicitRules
          , riskAssessmentLanguage = language
          , riskAssessmentText = text
--          , riskAssessmentContained = contained
          , riskAssessmentExtension = extension
          , riskAssessmentModifierExtension = modifierExtension
          , riskAssessmentIdentifier = identifier
          , riskAssessmentBasedOn = basedOn
          , riskAssessmentParent = parent
          , riskAssessmentStatus = status
          , riskAssessmentMethod = method
          , riskAssessmentCode = code
          , riskAssessmentSubject = subject
          , riskAssessmentEncounter = encounter
          , riskAssessmentOccurrenceDateTime = occurrenceDateTime
          , riskAssessmentOccurrencePeriod = occurrencePeriod
          , riskAssessmentCondition = condition
          , riskAssessmentPerformer = performer
          , riskAssessmentReasonCode = reasonCode
          , riskAssessmentReasonReference = reasonReference
          , riskAssessmentBasis = basis
          , riskAssessmentPrediction = prediction
          , riskAssessmentMitigation = mitigation
          , riskAssessmentNote = note
          }
      _ -> fail "not a RiskAssessment"
instance Xmlbf.ToXml RiskAssessment where
  toXml p = Xmlbf.element "RiskAssessment" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (riskAssessmentId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (riskAssessmentMeta p))
             , OptVal   "implicitRules" (fmap toUri (riskAssessmentImplicitRules p))
             , OptVal   "language" (fmap toLanguage (riskAssessmentLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (riskAssessmentText p))
--             , PropList "contained" (fmap Xmlbf.toXml (riskAssessmentContained p))
             , PropList "extension" (fmap Xmlbf.toXml (riskAssessmentExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (riskAssessmentModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (riskAssessmentIdentifier p))
             , OptProp  "basedOn" (fmap Xmlbf.toXml (riskAssessmentBasedOn p))
             , OptProp  "parent" (fmap Xmlbf.toXml (riskAssessmentParent p))
             , Val      "status" (     toRiskAssessmentStatus (riskAssessmentStatus p))
             , OptProp  "method" (fmap Xmlbf.toXml (riskAssessmentMethod p))
             , OptProp  "code" (fmap Xmlbf.toXml (riskAssessmentCode p))
             , Prop     "subject" (HM.empty, Xmlbf.toXml (riskAssessmentSubject p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (riskAssessmentEncounter p))
             , OptVal   "occurrenceDateTime" (fmap toDateTime (riskAssessmentOccurrenceDateTime p))
             , OptProp  "occurrencePeriod" (fmap Xmlbf.toXml (riskAssessmentOccurrencePeriod p))
             , OptProp  "condition" (fmap Xmlbf.toXml (riskAssessmentCondition p))
             , OptProp  "performer" (fmap Xmlbf.toXml (riskAssessmentPerformer p))
             , PropList "reasonCode" (fmap Xmlbf.toXml (riskAssessmentReasonCode p))
             , PropList "reasonReference" (fmap Xmlbf.toXml (riskAssessmentReasonReference p))
             , PropList "basis" (fmap Xmlbf.toXml (riskAssessmentBasis p))
             , PropList "prediction" (fmap Xmlbf.toXml (riskAssessmentPrediction p))
             , OptVal   "mitigation" (fmap toString (riskAssessmentMitigation p))
             , PropList "note" (fmap Xmlbf.toXml (riskAssessmentNote p))
             ]
instance Xmlbf.FromXml RiskAssessment where
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
    basedOn <- optional $ Xmlbf.pElement "basedOn" Xmlbf.fromXml
    parent <- optional $ Xmlbf.pElement "parent" Xmlbf.fromXml
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    method <- optional $ Xmlbf.pElement "method" Xmlbf.fromXml
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    subject <-            Xmlbf.pElement "subject" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    occurrenceDateTime <- optional $ Xmlbf.pElement "occurrenceDateTime" (Xmlbf.pAttr "value")
    occurrencePeriod <- optional $ Xmlbf.pElement "occurrencePeriod" Xmlbf.fromXml
    condition <- optional $ Xmlbf.pElement "condition" Xmlbf.fromXml
    performer <- optional $ Xmlbf.pElement "performer" Xmlbf.fromXml
    reasonCode <- many     $ Xmlbf.pElement "reasonCode" Xmlbf.fromXml
    reasonReference <- many     $ Xmlbf.pElement "reasonReference" Xmlbf.fromXml
    basis <- many     $ Xmlbf.pElement "basis" Xmlbf.fromXml
    prediction <- many     $ Xmlbf.pElement "prediction" Xmlbf.fromXml
    mitigation <- optional $ Xmlbf.pElement "mitigation" (Xmlbf.pAttr "value")
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    return RiskAssessment {
            riskAssessmentId = fmap fromId id
          , riskAssessmentMeta = meta
          , riskAssessmentImplicitRules = fmap fromUri implicitRules
          , riskAssessmentLanguage = fmap fromLanguage language
          , riskAssessmentText = text
--          , riskAssessmentContained = contained
          , riskAssessmentExtension = extension
          , riskAssessmentModifierExtension = modifierExtension
          , riskAssessmentIdentifier = identifier
          , riskAssessmentBasedOn = basedOn
          , riskAssessmentParent = parent
          , riskAssessmentStatus =      fromRiskAssessmentStatus status
          , riskAssessmentMethod = method
          , riskAssessmentCode = code
          , riskAssessmentSubject = subject
          , riskAssessmentEncounter = encounter
          , riskAssessmentOccurrenceDateTime = fmap fromDateTime occurrenceDateTime
          , riskAssessmentOccurrencePeriod = occurrencePeriod
          , riskAssessmentCondition = condition
          , riskAssessmentPerformer = performer
          , riskAssessmentReasonCode = reasonCode
          , riskAssessmentReasonReference = reasonReference
          , riskAssessmentBasis = basis
          , riskAssessmentPrediction = prediction
          , riskAssessmentMitigation = fmap fromString mitigation
          , riskAssessmentNote = note
          }



data RiskAssessmentPredictionProbability
    = RiskAssessmentPredictionProbabilityDecimal Decimal
    | RiskAssessmentPredictionProbabilityRange Range
    deriving (Eq, Show)

data RiskAssessmentPredictionWhen
    = RiskAssessmentPredictionWhenPeriod Period
    | RiskAssessmentPredictionWhenRange Range
    deriving (Eq, Show)

data RiskAssessmentPrediction = RiskAssessmentPrediction {
    riskAssessmentPredictionAttrId :: Maybe Text
  , riskAssessmentPredictionExtension :: [Extension]
  , riskAssessmentPredictionModifierExtension :: [Extension]
  , riskAssessmentPredictionOutcome :: Maybe CodeableConcept
  , riskAssessmentPredictionProbabilityDecimal :: Maybe Decimal
  , riskAssessmentPredictionProbabilityRange :: Maybe Range
  , riskAssessmentPredictionQualitativeRisk :: Maybe CodeableConcept
  , riskAssessmentPredictionRelativeRisk :: Maybe Decimal
  , riskAssessmentPredictionWhenPeriod :: Maybe Period
  , riskAssessmentPredictionWhenRange :: Maybe Range
  , riskAssessmentPredictionRationale :: Maybe Text
  }
--

instance ToJSON RiskAssessmentPrediction where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (riskAssessmentPredictionAttrId p)
    ,  "extension" .= toJSON (riskAssessmentPredictionExtension p)
    ,  "modifierExtension" .= toJSON (riskAssessmentPredictionModifierExtension p)
    ,  "outcome" .= toJSON (riskAssessmentPredictionOutcome p)
    ,  "probabilityDecimal" .= toJSON (riskAssessmentPredictionProbabilityDecimal p)
    ,  "probabilityRange" .= toJSON (riskAssessmentPredictionProbabilityRange p)
    ,  "qualitativeRisk" .= toJSON (riskAssessmentPredictionQualitativeRisk p)
    ,  "relativeRisk" .= toJSON (riskAssessmentPredictionRelativeRisk p)
    ,  "whenPeriod" .= toJSON (riskAssessmentPredictionWhenPeriod p)
    ,  "whenRange" .= toJSON (riskAssessmentPredictionWhenRange p)
    ,  "rationale" .= toJSON (riskAssessmentPredictionRationale p)
    ]
instance FromJSON RiskAssessmentPrediction where
  parseJSON = withObject "RiskAssessmentPrediction" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        outcome <- o .:? "outcome"
        probabilityDecimal <- o .:? "probabilityDecimal"
        probabilityRange <- o .:? "probabilityRange"
        qualitativeRisk <- o .:? "qualitativeRisk"
        relativeRisk <- o .:? "relativeRisk"
        whenPeriod <- o .:? "whenPeriod"
        whenRange <- o .:? "whenRange"
        rationale <- o .:? "rationale"
        return RiskAssessmentPrediction{
            riskAssessmentPredictionAttrId = id
          , riskAssessmentPredictionExtension = extension
          , riskAssessmentPredictionModifierExtension = modifierExtension
          , riskAssessmentPredictionOutcome = outcome
          , riskAssessmentPredictionProbabilityDecimal = probabilityDecimal
          , riskAssessmentPredictionProbabilityRange = probabilityRange
          , riskAssessmentPredictionQualitativeRisk = qualitativeRisk
          , riskAssessmentPredictionRelativeRisk = relativeRisk
          , riskAssessmentPredictionWhenPeriod = whenPeriod
          , riskAssessmentPredictionWhenRange = whenRange
          , riskAssessmentPredictionRationale = rationale
          }
instance Xmlbf.ToXml RiskAssessmentPrediction where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (riskAssessmentPredictionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (riskAssessmentPredictionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (riskAssessmentPredictionModifierExtension p))
             , OptProp  "outcome" (fmap Xmlbf.toXml (riskAssessmentPredictionOutcome p))
             , OptVal   "probabilityDecimal" (fmap toDecimal (riskAssessmentPredictionProbabilityDecimal p))
             , OptProp  "probabilityRange" (fmap Xmlbf.toXml (riskAssessmentPredictionProbabilityRange p))
             , OptProp  "qualitativeRisk" (fmap Xmlbf.toXml (riskAssessmentPredictionQualitativeRisk p))
             , OptVal   "relativeRisk" (fmap toDecimal (riskAssessmentPredictionRelativeRisk p))
             , OptProp  "whenPeriod" (fmap Xmlbf.toXml (riskAssessmentPredictionWhenPeriod p))
             , OptProp  "whenRange" (fmap Xmlbf.toXml (riskAssessmentPredictionWhenRange p))
             , OptVal   "rationale" (fmap toString (riskAssessmentPredictionRationale p))
             ]
instance Xmlbf.FromXml RiskAssessmentPrediction where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    outcome <- optional $ Xmlbf.pElement "outcome" Xmlbf.fromXml
    probabilityDecimal <- optional $ Xmlbf.pElement "probabilityDecimal" (Xmlbf.pAttr "value")
    probabilityRange <- optional $ Xmlbf.pElement "probabilityRange" Xmlbf.fromXml
    qualitativeRisk <- optional $ Xmlbf.pElement "qualitativeRisk" Xmlbf.fromXml
    relativeRisk <- optional $ Xmlbf.pElement "relativeRisk" (Xmlbf.pAttr "value")
    whenPeriod <- optional $ Xmlbf.pElement "whenPeriod" Xmlbf.fromXml
    whenRange <- optional $ Xmlbf.pElement "whenRange" Xmlbf.fromXml
    rationale <- optional $ Xmlbf.pElement "rationale" (Xmlbf.pAttr "value")
    return RiskAssessmentPrediction {
            riskAssessmentPredictionAttrId = id
          , riskAssessmentPredictionExtension = extension
          , riskAssessmentPredictionModifierExtension = modifierExtension
          , riskAssessmentPredictionOutcome = outcome
          , riskAssessmentPredictionProbabilityDecimal = fmap fromDecimal probabilityDecimal
          , riskAssessmentPredictionProbabilityRange = probabilityRange
          , riskAssessmentPredictionQualitativeRisk = qualitativeRisk
          , riskAssessmentPredictionRelativeRisk = fmap fromDecimal relativeRisk
          , riskAssessmentPredictionWhenPeriod = whenPeriod
          , riskAssessmentPredictionWhenRange = whenRange
          , riskAssessmentPredictionRationale = fmap fromString rationale
          }




