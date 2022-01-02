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
-- FHIR 4.0.0 AdverseEvent
--

module Data.FHIR.Resources.AdverseEvent where

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

data AdverseEventActuality
    = AEAActual
    | AEAPotential
  deriving (Eq, Show)

instance ToJSON AdverseEventActuality where
    toJSON AEAActual = String "actual"
    toJSON AEAPotential = String "potential"
instance FromJSON AdverseEventActuality where
    parseJSON "actual" = return AEAActual
    parseJSON "potential" = return AEAPotential

toAdverseEventActuality AEAActual = "actual"
toAdverseEventActuality AEAPotential = "potential"
fromAdverseEventActuality "actual" = AEAActual
fromAdverseEventActuality "potential" = AEAPotential


data AdverseEvent = AdverseEvent {
    adverseEventId :: Maybe Id
  , adverseEventMeta :: Maybe Meta
  , adverseEventImplicitRules :: Maybe Uri
  , adverseEventLanguage :: Maybe Language
  , adverseEventText :: Maybe Narrative
--    adverseEventContained :: [ResourceContainer]
  , adverseEventExtension :: [Extension]
  , adverseEventModifierExtension :: [Extension]
  , adverseEventIdentifier :: Maybe Identifier
  , adverseEventActuality :: AdverseEventActuality
  , adverseEventCategory :: [CodeableConcept]
  , adverseEventEvent :: Maybe CodeableConcept
  , adverseEventSubject :: Reference
  , adverseEventEncounter :: Maybe Reference
  , adverseEventDate :: Maybe DateTime
  , adverseEventDetected :: Maybe DateTime
  , adverseEventRecordedDate :: Maybe DateTime
  , adverseEventResultingCondition :: [Reference]
  , adverseEventLocation :: Maybe Reference
  , adverseEventSeriousness :: Maybe CodeableConcept
  , adverseEventSeverity :: Maybe CodeableConcept
  , adverseEventOutcome :: Maybe CodeableConcept
  , adverseEventRecorder :: Maybe Reference
  , adverseEventContributor :: [Reference]
  , adverseEventSuspectEntity :: [AdverseEventSuspectEntity]
  , adverseEventSubjectMedicalHistory :: [Reference]
  , adverseEventReferenceDocument :: [Reference]
  , adverseEventStudy :: [Reference]
  }
--

instance ToJSON AdverseEvent where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "AdverseEvent")
    ,  "id" .= toJSON (adverseEventId p)
    ,  "meta" .= toJSON (adverseEventMeta p)
    ,  "implicitRules" .= toJSON (adverseEventImplicitRules p)
    ,  "language" .= toJSON (adverseEventLanguage p)
    ,  "text" .= toJSON (adverseEventText p)
--    , "contained" .= toJSON (adverseEventContained p)
    ,  "extension" .= toJSON (adverseEventExtension p)
    ,  "modifierExtension" .= toJSON (adverseEventModifierExtension p)
    ,  "identifier" .= toJSON (adverseEventIdentifier p)
    ,  "actuality" .= toJSON (adverseEventActuality p)
    ,  "category" .= toJSON (adverseEventCategory p)
    ,  "event" .= toJSON (adverseEventEvent p)
    ,  "subject" .= toJSON (adverseEventSubject p)
    ,  "encounter" .= toJSON (adverseEventEncounter p)
    ,  "date" .= toJSON (adverseEventDate p)
    ,  "detected" .= toJSON (adverseEventDetected p)
    ,  "recordedDate" .= toJSON (adverseEventRecordedDate p)
    ,  "resultingCondition" .= toJSON (adverseEventResultingCondition p)
    ,  "location" .= toJSON (adverseEventLocation p)
    ,  "seriousness" .= toJSON (adverseEventSeriousness p)
    ,  "severity" .= toJSON (adverseEventSeverity p)
    ,  "outcome" .= toJSON (adverseEventOutcome p)
    ,  "recorder" .= toJSON (adverseEventRecorder p)
    ,  "contributor" .= toJSON (adverseEventContributor p)
    ,  "suspectEntity" .= toJSON (adverseEventSuspectEntity p)
    ,  "subjectMedicalHistory" .= toJSON (adverseEventSubjectMedicalHistory p)
    ,  "referenceDocument" .= toJSON (adverseEventReferenceDocument p)
    ,  "study" .= toJSON (adverseEventStudy p)
    ]
instance FromJSON AdverseEvent where
  parseJSON = withObject "AdverseEvent" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "AdverseEvent" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier"
        actuality <- o .:  "actuality"
        category <- o .:? "category" .!= []
        event <- o .:? "event"
        subject <- o .:  "subject"
        encounter <- o .:? "encounter"
        date <- o .:? "date"
        detected <- o .:? "detected"
        recordedDate <- o .:? "recordedDate"
        resultingCondition <- o .:? "resultingCondition" .!= []
        location <- o .:? "location"
        seriousness <- o .:? "seriousness"
        severity <- o .:? "severity"
        outcome <- o .:? "outcome"
        recorder <- o .:? "recorder"
        contributor <- o .:? "contributor" .!= []
        suspectEntity <- o .:? "suspectEntity" .!= []
        subjectMedicalHistory <- o .:? "subjectMedicalHistory" .!= []
        referenceDocument <- o .:? "referenceDocument" .!= []
        study <- o .:? "study" .!= []
        return AdverseEvent{
            adverseEventId = id
          , adverseEventMeta = meta
          , adverseEventImplicitRules = implicitRules
          , adverseEventLanguage = language
          , adverseEventText = text
--          , adverseEventContained = contained
          , adverseEventExtension = extension
          , adverseEventModifierExtension = modifierExtension
          , adverseEventIdentifier = identifier
          , adverseEventActuality = actuality
          , adverseEventCategory = category
          , adverseEventEvent = event
          , adverseEventSubject = subject
          , adverseEventEncounter = encounter
          , adverseEventDate = date
          , adverseEventDetected = detected
          , adverseEventRecordedDate = recordedDate
          , adverseEventResultingCondition = resultingCondition
          , adverseEventLocation = location
          , adverseEventSeriousness = seriousness
          , adverseEventSeverity = severity
          , adverseEventOutcome = outcome
          , adverseEventRecorder = recorder
          , adverseEventContributor = contributor
          , adverseEventSuspectEntity = suspectEntity
          , adverseEventSubjectMedicalHistory = subjectMedicalHistory
          , adverseEventReferenceDocument = referenceDocument
          , adverseEventStudy = study
          }
      _ -> fail "not a AdverseEvent"
instance Xmlbf.ToXml AdverseEvent where
  toXml p = Xmlbf.element "AdverseEvent" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (adverseEventId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (adverseEventMeta p))
             , OptVal   "implicitRules" (fmap toUri (adverseEventImplicitRules p))
             , OptVal   "language" (fmap toLanguage (adverseEventLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (adverseEventText p))
--             , PropList "contained" (fmap Xmlbf.toXml (adverseEventContained p))
             , PropList "extension" (fmap Xmlbf.toXml (adverseEventExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (adverseEventModifierExtension p))
             , OptProp  "identifier" (fmap Xmlbf.toXml (adverseEventIdentifier p))
             , Val      "actuality" (     toAdverseEventActuality (adverseEventActuality p))
             , PropList "category" (fmap Xmlbf.toXml (adverseEventCategory p))
             , OptProp  "event" (fmap Xmlbf.toXml (adverseEventEvent p))
             , Prop     "subject" (HM.empty, Xmlbf.toXml (adverseEventSubject p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (adverseEventEncounter p))
             , OptVal   "date" (fmap toDateTime (adverseEventDate p))
             , OptVal   "detected" (fmap toDateTime (adverseEventDetected p))
             , OptVal   "recordedDate" (fmap toDateTime (adverseEventRecordedDate p))
             , PropList "resultingCondition" (fmap Xmlbf.toXml (adverseEventResultingCondition p))
             , OptProp  "location" (fmap Xmlbf.toXml (adverseEventLocation p))
             , OptProp  "seriousness" (fmap Xmlbf.toXml (adverseEventSeriousness p))
             , OptProp  "severity" (fmap Xmlbf.toXml (adverseEventSeverity p))
             , OptProp  "outcome" (fmap Xmlbf.toXml (adverseEventOutcome p))
             , OptProp  "recorder" (fmap Xmlbf.toXml (adverseEventRecorder p))
             , PropList "contributor" (fmap Xmlbf.toXml (adverseEventContributor p))
             , PropList "suspectEntity" (fmap Xmlbf.toXml (adverseEventSuspectEntity p))
             , PropList "subjectMedicalHistory" (fmap Xmlbf.toXml (adverseEventSubjectMedicalHistory p))
             , PropList "referenceDocument" (fmap Xmlbf.toXml (adverseEventReferenceDocument p))
             , PropList "study" (fmap Xmlbf.toXml (adverseEventStudy p))
             ]
instance Xmlbf.FromXml AdverseEvent where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" Xmlbf.fromXml
--    contained <- many     $ Xmlbf.pElement "contained" Xmlbf.fromXml
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    identifier <- optional $ Xmlbf.pElement "identifier" Xmlbf.fromXml
    actuality <-            Xmlbf.pElement "actuality" (Xmlbf.pAttr "value")
    category <- many     $ Xmlbf.pElement "category" Xmlbf.fromXml
    event <- optional $ Xmlbf.pElement "event" Xmlbf.fromXml
    subject <-            Xmlbf.pElement "subject" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    date <- optional $ Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    detected <- optional $ Xmlbf.pElement "detected" (Xmlbf.pAttr "value")
    recordedDate <- optional $ Xmlbf.pElement "recordedDate" (Xmlbf.pAttr "value")
    resultingCondition <- many     $ Xmlbf.pElement "resultingCondition" Xmlbf.fromXml
    location <- optional $ Xmlbf.pElement "location" Xmlbf.fromXml
    seriousness <- optional $ Xmlbf.pElement "seriousness" Xmlbf.fromXml
    severity <- optional $ Xmlbf.pElement "severity" Xmlbf.fromXml
    outcome <- optional $ Xmlbf.pElement "outcome" Xmlbf.fromXml
    recorder <- optional $ Xmlbf.pElement "recorder" Xmlbf.fromXml
    contributor <- many     $ Xmlbf.pElement "contributor" Xmlbf.fromXml
    suspectEntity <- many     $ Xmlbf.pElement "suspectEntity" Xmlbf.fromXml
    subjectMedicalHistory <- many     $ Xmlbf.pElement "subjectMedicalHistory" Xmlbf.fromXml
    referenceDocument <- many     $ Xmlbf.pElement "referenceDocument" Xmlbf.fromXml
    study <- many     $ Xmlbf.pElement "study" Xmlbf.fromXml
    return AdverseEvent {
            adverseEventId = fmap fromId id
          , adverseEventMeta = meta
          , adverseEventImplicitRules = fmap fromUri implicitRules
          , adverseEventLanguage = fmap fromLanguage language
          , adverseEventText = text
--          , adverseEventContained = contained
          , adverseEventExtension = extension
          , adverseEventModifierExtension = modifierExtension
          , adverseEventIdentifier = identifier
          , adverseEventActuality =      fromAdverseEventActuality actuality
          , adverseEventCategory = category
          , adverseEventEvent = event
          , adverseEventSubject = subject
          , adverseEventEncounter = encounter
          , adverseEventDate = fmap fromDateTime date
          , adverseEventDetected = fmap fromDateTime detected
          , adverseEventRecordedDate = fmap fromDateTime recordedDate
          , adverseEventResultingCondition = resultingCondition
          , adverseEventLocation = location
          , adverseEventSeriousness = seriousness
          , adverseEventSeverity = severity
          , adverseEventOutcome = outcome
          , adverseEventRecorder = recorder
          , adverseEventContributor = contributor
          , adverseEventSuspectEntity = suspectEntity
          , adverseEventSubjectMedicalHistory = subjectMedicalHistory
          , adverseEventReferenceDocument = referenceDocument
          , adverseEventStudy = study
          }



data AdverseEventSuspectEntity = AdverseEventSuspectEntity {
    adverseEventSuspectEntityAttrId :: Maybe Text
  , adverseEventSuspectEntityExtension :: [Extension]
  , adverseEventSuspectEntityModifierExtension :: [Extension]
  , adverseEventSuspectEntityInstance :: Reference
  , adverseEventSuspectEntityCausality :: [AdverseEventCausality]
  }
--

instance ToJSON AdverseEventSuspectEntity where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (adverseEventSuspectEntityAttrId p)
    ,  "extension" .= toJSON (adverseEventSuspectEntityExtension p)
    ,  "modifierExtension" .= toJSON (adverseEventSuspectEntityModifierExtension p)
    ,  "instance" .= toJSON (adverseEventSuspectEntityInstance p)
    ,  "causality" .= toJSON (adverseEventSuspectEntityCausality p)
    ]
instance FromJSON AdverseEventSuspectEntity where
  parseJSON = withObject "AdverseEventSuspectEntity" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ins <- o .:  "instance"
        causality <- o .:? "causality" .!= []
        return AdverseEventSuspectEntity{
            adverseEventSuspectEntityAttrId = id
          , adverseEventSuspectEntityExtension = extension
          , adverseEventSuspectEntityModifierExtension = modifierExtension
          , adverseEventSuspectEntityInstance = ins
          , adverseEventSuspectEntityCausality = causality
          }
instance Xmlbf.ToXml AdverseEventSuspectEntity where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (adverseEventSuspectEntityAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (adverseEventSuspectEntityExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (adverseEventSuspectEntityModifierExtension p))
             , Prop     "instance" (HM.empty, Xmlbf.toXml (adverseEventSuspectEntityInstance p))
             , PropList "causality" (fmap Xmlbf.toXml (adverseEventSuspectEntityCausality p))
             ]
instance Xmlbf.FromXml AdverseEventSuspectEntity where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ins <-            Xmlbf.pElement "instance" Xmlbf.fromXml
    causality <- many     $ Xmlbf.pElement "causality" Xmlbf.fromXml
    return AdverseEventSuspectEntity {
            adverseEventSuspectEntityAttrId = id
          , adverseEventSuspectEntityExtension = extension
          , adverseEventSuspectEntityModifierExtension = modifierExtension
          , adverseEventSuspectEntityInstance = ins
          , adverseEventSuspectEntityCausality = causality
          }



data AdverseEventCausality = AdverseEventCausality {
    adverseEventCausalityAttrId :: Maybe Text
  , adverseEventCausalityExtension :: [Extension]
  , adverseEventCausalityModifierExtension :: [Extension]
  , adverseEventCausalityAssessment :: Maybe CodeableConcept
  , adverseEventCausalityProductRelatedness :: Maybe Text
  , adverseEventCausalityAuthor :: Maybe Reference
  , adverseEventCausalityMethod :: Maybe CodeableConcept
  }
--

instance ToJSON AdverseEventCausality where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (adverseEventCausalityAttrId p)
    ,  "extension" .= toJSON (adverseEventCausalityExtension p)
    ,  "modifierExtension" .= toJSON (adverseEventCausalityModifierExtension p)
    ,  "assessment" .= toJSON (adverseEventCausalityAssessment p)
    ,  "productRelatedness" .= toJSON (adverseEventCausalityProductRelatedness p)
    ,  "author" .= toJSON (adverseEventCausalityAuthor p)
    ,  "method" .= toJSON (adverseEventCausalityMethod p)
    ]
instance FromJSON AdverseEventCausality where
  parseJSON = withObject "AdverseEventCausality" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        assessment <- o .:? "assessment"
        productRelatedness <- o .:? "productRelatedness"
        author <- o .:? "author"
        method <- o .:? "method"
        return AdverseEventCausality{
            adverseEventCausalityAttrId = id
          , adverseEventCausalityExtension = extension
          , adverseEventCausalityModifierExtension = modifierExtension
          , adverseEventCausalityAssessment = assessment
          , adverseEventCausalityProductRelatedness = productRelatedness
          , adverseEventCausalityAuthor = author
          , adverseEventCausalityMethod = method
          }
instance Xmlbf.ToXml AdverseEventCausality where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (adverseEventCausalityAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (adverseEventCausalityExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (adverseEventCausalityModifierExtension p))
             , OptProp  "assessment" (fmap Xmlbf.toXml (adverseEventCausalityAssessment p))
             , OptVal   "productRelatedness" (fmap toString (adverseEventCausalityProductRelatedness p))
             , OptProp  "author" (fmap Xmlbf.toXml (adverseEventCausalityAuthor p))
             , OptProp  "method" (fmap Xmlbf.toXml (adverseEventCausalityMethod p))
             ]
instance Xmlbf.FromXml AdverseEventCausality where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    assessment <- optional $ Xmlbf.pElement "assessment" Xmlbf.fromXml
    productRelatedness <- optional $ Xmlbf.pElement "productRelatedness" (Xmlbf.pAttr "value")
    author <- optional $ Xmlbf.pElement "author" Xmlbf.fromXml
    method <- optional $ Xmlbf.pElement "method" Xmlbf.fromXml
    return AdverseEventCausality {
            adverseEventCausalityAttrId = id
          , adverseEventCausalityExtension = extension
          , adverseEventCausalityModifierExtension = modifierExtension
          , adverseEventCausalityAssessment = assessment
          , adverseEventCausalityProductRelatedness = fmap fromString productRelatedness
          , adverseEventCausalityAuthor = author
          , adverseEventCausalityMethod = method
          }




