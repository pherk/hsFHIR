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
-- FHIR 4.0.0 MedicationRequest
--

module Data.FHIR.Resources.MedicationRequest where

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

data MedicationRequestStatus
    = MRSActive
    | MRSOnHold
    | MRSCancelled
    | MRSCompleted
    | MRSEnteredInError
    | MRSStopped
    | MRSDraft
    | MRSUnknown
  deriving (Eq, Show)

instance ToJSON MedicationRequestStatus where
    toJSON MRSActive = String "active"
    toJSON MRSOnHold = String "on-hold"
    toJSON MRSCancelled = String "cancelled"
    toJSON MRSCompleted = String "completed"
    toJSON MRSEnteredInError = String "entered-in-error"
    toJSON MRSStopped = String "stopped"
    toJSON MRSDraft = String "draft"
    toJSON MRSUnknown = String "unknown"
instance FromJSON MedicationRequestStatus where
    parseJSON "active" = return MRSActive
    parseJSON "on-hold" = return MRSOnHold
    parseJSON "cancelled" = return MRSCancelled
    parseJSON "completed" = return MRSCompleted
    parseJSON "entered-in-error" = return MRSEnteredInError
    parseJSON "stopped" = return MRSStopped
    parseJSON "draft" = return MRSDraft
    parseJSON "unknown" = return MRSUnknown

toMedicationRequestStatus MRSActive = "active"
toMedicationRequestStatus MRSOnHold = "on-hold"
toMedicationRequestStatus MRSCancelled = "cancelled"
toMedicationRequestStatus MRSCompleted = "completed"
toMedicationRequestStatus MRSEnteredInError = "entered-in-error"
toMedicationRequestStatus MRSStopped = "stopped"
toMedicationRequestStatus MRSDraft = "draft"
toMedicationRequestStatus MRSUnknown = "unknown"
fromMedicationRequestStatus "active" = MRSActive
fromMedicationRequestStatus "on-hold" = MRSOnHold
fromMedicationRequestStatus "cancelled" = MRSCancelled
fromMedicationRequestStatus "completed" = MRSCompleted
fromMedicationRequestStatus "entered-in-error" = MRSEnteredInError
fromMedicationRequestStatus "stopped" = MRSStopped
fromMedicationRequestStatus "draft" = MRSDraft
fromMedicationRequestStatus "unknown" = MRSUnknown


data MedicationRequestIntent
    = MRIProposal
    | MRIPlan
    | MRIOrder
    | MRIOriginalOrder
    | MRIReflexOrder
    | MRIFillerOrder
    | MRIInstanceOrder
    | MRIOption
  deriving (Eq, Show)

instance ToJSON MedicationRequestIntent where
    toJSON MRIProposal = String "proposal"
    toJSON MRIPlan = String "plan"
    toJSON MRIOrder = String "order"
    toJSON MRIOriginalOrder = String "original-order"
    toJSON MRIReflexOrder = String "reflex-order"
    toJSON MRIFillerOrder = String "filler-order"
    toJSON MRIInstanceOrder = String "instance-order"
    toJSON MRIOption = String "option"
instance FromJSON MedicationRequestIntent where
    parseJSON "proposal" = return MRIProposal
    parseJSON "plan" = return MRIPlan
    parseJSON "order" = return MRIOrder
    parseJSON "original-order" = return MRIOriginalOrder
    parseJSON "reflex-order" = return MRIReflexOrder
    parseJSON "filler-order" = return MRIFillerOrder
    parseJSON "instance-order" = return MRIInstanceOrder
    parseJSON "option" = return MRIOption

toMedicationRequestIntent MRIProposal = "proposal"
toMedicationRequestIntent MRIPlan = "plan"
toMedicationRequestIntent MRIOrder = "order"
toMedicationRequestIntent MRIOriginalOrder = "original-order"
toMedicationRequestIntent MRIReflexOrder = "reflex-order"
toMedicationRequestIntent MRIFillerOrder = "filler-order"
toMedicationRequestIntent MRIInstanceOrder = "instance-order"
toMedicationRequestIntent MRIOption = "option"
fromMedicationRequestIntent "proposal" = MRIProposal
fromMedicationRequestIntent "plan" = MRIPlan
fromMedicationRequestIntent "order" = MRIOrder
fromMedicationRequestIntent "original-order" = MRIOriginalOrder
fromMedicationRequestIntent "reflex-order" = MRIReflexOrder
fromMedicationRequestIntent "filler-order" = MRIFillerOrder
fromMedicationRequestIntent "instance-order" = MRIInstanceOrder
fromMedicationRequestIntent "option" = MRIOption


data MedicationRequestPriority
    = MRPRoutine
    | MRPUrgent
    | MRPAsap
    | MRPStat
  deriving (Eq, Show)

instance ToJSON MedicationRequestPriority where
    toJSON MRPRoutine = String "routine"
    toJSON MRPUrgent = String "urgent"
    toJSON MRPAsap = String "asap"
    toJSON MRPStat = String "stat"
instance FromJSON MedicationRequestPriority where
    parseJSON "routine" = return MRPRoutine
    parseJSON "urgent" = return MRPUrgent
    parseJSON "asap" = return MRPAsap
    parseJSON "stat" = return MRPStat

toMedicationRequestPriority MRPRoutine = "routine"
toMedicationRequestPriority MRPUrgent = "urgent"
toMedicationRequestPriority MRPAsap = "asap"
toMedicationRequestPriority MRPStat = "stat"
fromMedicationRequestPriority "routine" = MRPRoutine
fromMedicationRequestPriority "urgent" = MRPUrgent
fromMedicationRequestPriority "asap" = MRPAsap
fromMedicationRequestPriority "stat" = MRPStat


data MedicationRequestReported
    = MedicationRequestReportedBoolean Boolean
    | MedicationRequestReportedReference Reference
    deriving (Eq, Show)

data MedicationRequestMedication
    = MedicationRequestMedicationCodeableConcept CodeableConcept
    | MedicationRequestMedicationReference Reference
    deriving (Eq, Show)

data MedicationRequest = MedicationRequest {
    medicationRequestId :: Maybe Id
  , medicationRequestMeta :: Maybe Meta
  , medicationRequestImplicitRules :: Maybe Uri
  , medicationRequestLanguage :: Maybe Language
  , medicationRequestText :: Maybe Narrative
--    medicationRequestContained :: [ResourceContainer]
  , medicationRequestExtension :: [Extension]
  , medicationRequestModifierExtension :: [Extension]
  , medicationRequestIdentifier :: [Identifier]
  , medicationRequestStatus :: MedicationRequestStatus
  , medicationRequestStatusReason :: Maybe CodeableConcept
  , medicationRequestIntent :: MedicationRequestIntent
  , medicationRequestCategory :: [CodeableConcept]
  , medicationRequestPriority :: Maybe MedicationRequestPriority
  , medicationRequestDoNotPerform :: Maybe Boolean
  , medicationRequestReported :: Maybe MedicationRequestReported
  , medicationRequestMedication :: MedicationRequestMedication
  , medicationRequestSubject :: Reference
  , medicationRequestEncounter :: Maybe Reference
  , medicationRequestSupportingInformation :: [Reference]
  , medicationRequestAuthoredOn :: Maybe DateTime
  , medicationRequestRequester :: Maybe Reference
  , medicationRequestPerformer :: Maybe Reference
  , medicationRequestPerformerType :: Maybe CodeableConcept
  , medicationRequestRecorder :: Maybe Reference
  , medicationRequestReasonCode :: [CodeableConcept]
  , medicationRequestReasonReference :: [Reference]
  , medicationRequestInstantiatesCanonical :: [Canonical]
  , medicationRequestInstantiatesUri :: [Uri]
  , medicationRequestBasedOn :: [Reference]
  , medicationRequestGroupIdentifier :: Maybe Identifier
  , medicationRequestCourseOfTherapyType :: Maybe CodeableConcept
  , medicationRequestInsurance :: [Reference]
  , medicationRequestNote :: [Annotation]
  , medicationRequestDosageInstruction :: [Dosage]
  , medicationRequestDispenseRequest :: Maybe MedicationRequestDispenseRequest
  , medicationRequestSubstitution :: Maybe MedicationRequestSubstitution
  , medicationRequestPriorPrescription :: Maybe Reference
  , medicationRequestDetectedIssue :: [Reference]
  , medicationRequestEventHistory :: [Reference]
  }
  deriving (Eq, Show)
--

instance ToJSON MedicationRequest where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "MedicationRequest")
    ,  "id" .= toJSON (medicationRequestId p)
    ,  "meta" .= toJSON (medicationRequestMeta p)
    ,  "implicitRules" .= toJSON (medicationRequestImplicitRules p)
    ,  "language" .= toJSON (medicationRequestLanguage p)
    ,  "text" .= toJSON (medicationRequestText p)
--    , "contained" .= toJSON (medicationRequestContained p)
    ,  "extension" .= toJSON (medicationRequestExtension p)
    ,  "modifierExtension" .= toJSON (medicationRequestModifierExtension p)
    ,  "identifier" .= toJSON (medicationRequestIdentifier p)
    ,  "status" .= toJSON (medicationRequestStatus p)
    ,  "statusReason" .= toJSON (medicationRequestStatusReason p)
    ,  "intent" .= toJSON (medicationRequestIntent p)
    ,  "category" .= toJSON (medicationRequestCategory p)
    ,  "priority" .= toJSON (medicationRequestPriority p)
    ,  "doNotPerform" .= toJSON (medicationRequestDoNotPerform p)
    , toReportedJSON (medicationRequestReported p)
    , toMedicationJSON (medicationRequestMedication p)
    ,  "subject" .= toJSON (medicationRequestSubject p)
    ,  "encounter" .= toJSON (medicationRequestEncounter p)
    ,  "supportingInformation" .= toJSON (medicationRequestSupportingInformation p)
    ,  "authoredOn" .= toJSON (medicationRequestAuthoredOn p)
    ,  "requester" .= toJSON (medicationRequestRequester p)
    ,  "performer" .= toJSON (medicationRequestPerformer p)
    ,  "performerType" .= toJSON (medicationRequestPerformerType p)
    ,  "recorder" .= toJSON (medicationRequestRecorder p)
    ,  "reasonCode" .= toJSON (medicationRequestReasonCode p)
    ,  "reasonReference" .= toJSON (medicationRequestReasonReference p)
    ,  "instantiatesCanonical" .= toJSON (medicationRequestInstantiatesCanonical p)
    ,  "instantiatesUri" .= toJSON (medicationRequestInstantiatesUri p)
    ,  "basedOn" .= toJSON (medicationRequestBasedOn p)
    ,  "groupIdentifier" .= toJSON (medicationRequestGroupIdentifier p)
    ,  "courseOfTherapyType" .= toJSON (medicationRequestCourseOfTherapyType p)
    ,  "insurance" .= toJSON (medicationRequestInsurance p)
    ,  "note" .= toJSON (medicationRequestNote p)
    ,  "dosageInstruction" .= toJSON (medicationRequestDosageInstruction p)
    ,  "dispenseRequest" .= toJSON (medicationRequestDispenseRequest p)
    ,  "substitution" .= toJSON (medicationRequestSubstitution p)
    ,  "priorPrescription" .= toJSON (medicationRequestPriorPrescription p)
    ,  "detectedIssue" .= toJSON (medicationRequestDetectedIssue p)
    ,  "eventHistory" .= toJSON (medicationRequestEventHistory p)
    ]
    where 
      toReportedJSON (     Nothing   ) = ("reported", Null)
      toReportedJSON (Just (MedicationRequestReportedBoolean c)) = ("reported", toJSON c)
      toReportedJSON (Just (MedicationRequestReportedReference c)) = ("reported", toJSON c)
      toMedicationJSON (     (MedicationRequestMedicationCodeableConcept c)) = ("medication", toJSON c)
      toMedicationJSON (     (MedicationRequestMedicationReference c)) = ("medication", toJSON c)
instance FromJSON MedicationRequest where
  parseJSON = withObject "MedicationRequest" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "MedicationRequest" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        status <- o .:  "status"
        statusReason <- o .:? "statusReason"
        intent <- o .:  "intent"
        category <- o .:? "category" .!= []
        priority <- o .:? "priority"
        doNotPerform <- o .:? "doNotPerform"
        reported <- parseReported o
        medication <- parseMedication o
        subject <- o .:  "subject"
        encounter <- o .:? "encounter"
        supportingInformation <- o .:? "supportingInformation" .!= []
        authoredOn <- o .:? "authoredOn"
        requester <- o .:? "requester"
        performer <- o .:? "performer"
        performerType <- o .:? "performerType"
        recorder <- o .:? "recorder"
        reasonCode <- o .:? "reasonCode" .!= []
        reasonReference <- o .:? "reasonReference" .!= []
        instantiatesCanonical <- o .:? "instantiatesCanonical" .!= []
        instantiatesUri <- o .:? "instantiatesUri" .!= []
        basedOn <- o .:? "basedOn" .!= []
        groupIdentifier <- o .:? "groupIdentifier"
        courseOfTherapyType <- o .:? "courseOfTherapyType"
        insurance <- o .:? "insurance" .!= []
        note <- o .:? "note" .!= []
        dosageInstruction <- o .:? "dosageInstruction" .!= []
        dispenseRequest <- o .:? "dispenseRequest"
        substitution <- o .:? "substitution"
        priorPrescription <- o .:? "priorPrescription"
        detectedIssue <- o .:? "detectedIssue" .!= []
        eventHistory <- o .:? "eventHistory" .!= []
        return MedicationRequest{
            medicationRequestId = id
          , medicationRequestMeta = meta
          , medicationRequestImplicitRules = implicitRules
          , medicationRequestLanguage = language
          , medicationRequestText = text
--          , medicationRequestContained = contained
          , medicationRequestExtension = extension
          , medicationRequestModifierExtension = modifierExtension
          , medicationRequestIdentifier = identifier
          , medicationRequestStatus = status
          , medicationRequestStatusReason = statusReason
          , medicationRequestIntent = intent
          , medicationRequestCategory = category
          , medicationRequestPriority = priority
          , medicationRequestDoNotPerform = doNotPerform
          , medicationRequestReported = reported
          , medicationRequestMedication = medication
          , medicationRequestSubject = subject
          , medicationRequestEncounter = encounter
          , medicationRequestSupportingInformation = supportingInformation
          , medicationRequestAuthoredOn = authoredOn
          , medicationRequestRequester = requester
          , medicationRequestPerformer = performer
          , medicationRequestPerformerType = performerType
          , medicationRequestRecorder = recorder
          , medicationRequestReasonCode = reasonCode
          , medicationRequestReasonReference = reasonReference
          , medicationRequestInstantiatesCanonical = instantiatesCanonical
          , medicationRequestInstantiatesUri = instantiatesUri
          , medicationRequestBasedOn = basedOn
          , medicationRequestGroupIdentifier = groupIdentifier
          , medicationRequestCourseOfTherapyType = courseOfTherapyType
          , medicationRequestInsurance = insurance
          , medicationRequestNote = note
          , medicationRequestDosageInstruction = dosageInstruction
          , medicationRequestDispenseRequest = dispenseRequest
          , medicationRequestSubstitution = substitution
          , medicationRequestPriorPrescription = priorPrescription
          , medicationRequestDetectedIssue = detectedIssue
          , medicationRequestEventHistory = eventHistory
          }
      _ -> fail "not a MedicationRequest"
    where 
      parseReported o = parseReportedBoolean o <|> parseReportedReference o
      parseReportedBoolean o = do
                has <- o .: "reportedBoolean"
                return $ Just (MedicationRequestReportedBoolean has)
      parseReportedReference o = do
                has <- o .: "reportedReference"
                return $ Just (MedicationRequestReportedReference has)
      parseMedication o = parseMedicationCodeableConcept o <|> parseMedicationReference o
      parseMedicationCodeableConcept o = do
                has <- o .: "medicationCodeableConcept"
                return $ MedicationRequestMedicationCodeableConcept has
      parseMedicationReference o = do
                has <- o .: "medicationReference"
                return $ MedicationRequestMedicationReference has
instance Xmlbf.ToXml MedicationRequest where
  toXml p = Xmlbf.element "MedicationRequest" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (medicationRequestId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (medicationRequestMeta p))
             , OptVal   "implicitRules" (fmap toUri (medicationRequestImplicitRules p))
             , OptVal   "language" (fmap toLanguage (medicationRequestLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (medicationRequestText p))
--             , PropList "contained" (fmap Xmlbf.toXml (medicationRequestContained p))
             , PropList "extension" (fmap Xmlbf.toXml (medicationRequestExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (medicationRequestModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (medicationRequestIdentifier p))
             , Val      "status" (     toMedicationRequestStatus (medicationRequestStatus p))
             , OptProp  "statusReason" (fmap Xmlbf.toXml (medicationRequestStatusReason p))
             , Val      "intent" (     toMedicationRequestIntent (medicationRequestIntent p))
             , PropList "category" (fmap Xmlbf.toXml (medicationRequestCategory p))
             , OptVal   "priority" (fmap toMedicationRequestPriority (medicationRequestPriority p))
             , OptVal   "doNotPerform" (fmap toBoolean (medicationRequestDoNotPerform p))
             , toReportedXml (medicationRequestReported p)
             , toMedicationXml (medicationRequestMedication p)
             , Prop     "subject" (HM.empty, Xmlbf.toXml (medicationRequestSubject p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (medicationRequestEncounter p))
             , PropList "supportingInformation" (fmap Xmlbf.toXml (medicationRequestSupportingInformation p))
             , OptVal   "authoredOn" (fmap toDateTime (medicationRequestAuthoredOn p))
             , OptProp  "requester" (fmap Xmlbf.toXml (medicationRequestRequester p))
             , OptProp  "performer" (fmap Xmlbf.toXml (medicationRequestPerformer p))
             , OptProp  "performerType" (fmap Xmlbf.toXml (medicationRequestPerformerType p))
             , OptProp  "recorder" (fmap Xmlbf.toXml (medicationRequestRecorder p))
             , PropList "reasonCode" (fmap Xmlbf.toXml (medicationRequestReasonCode p))
             , PropList "reasonReference" (fmap Xmlbf.toXml (medicationRequestReasonReference p))
             , ValList  "instantiatesCanonical" (fmap toCanonical (medicationRequestInstantiatesCanonical p))
             , ValList  "instantiatesUri" (fmap toUri (medicationRequestInstantiatesUri p))
             , PropList "basedOn" (fmap Xmlbf.toXml (medicationRequestBasedOn p))
             , OptProp  "groupIdentifier" (fmap Xmlbf.toXml (medicationRequestGroupIdentifier p))
             , OptProp  "courseOfTherapyType" (fmap Xmlbf.toXml (medicationRequestCourseOfTherapyType p))
             , PropList "insurance" (fmap Xmlbf.toXml (medicationRequestInsurance p))
             , PropList "note" (fmap Xmlbf.toXml (medicationRequestNote p))
             , PropList "dosageInstruction" (fmap Xmlbf.toXml (medicationRequestDosageInstruction p))
             , OptProp  "dispenseRequest" (fmap Xmlbf.toXml (medicationRequestDispenseRequest p))
             , OptProp  "substitution" (fmap Xmlbf.toXml (medicationRequestSubstitution p))
             , OptProp  "priorPrescription" (fmap Xmlbf.toXml (medicationRequestPriorPrescription p))
             , PropList "detectedIssue" (fmap Xmlbf.toXml (medicationRequestDetectedIssue p))
             , PropList "eventHistory" (fmap Xmlbf.toXml (medicationRequestEventHistory p))
             ]
          toReportedXml ( Nothing   ) = (OptVal "reported" Nothing)
          toReportedXml (Just (MedicationRequestReportedBoolean p)) = Val   "reportedBoolean" (toBoolean p)
          toReportedXml (Just (MedicationRequestReportedReference p)) = Prop  "reportedReference" (HM.empty, Xmlbf.toXml p)
          toMedicationXml (     (MedicationRequestMedicationCodeableConcept p)) = Prop     "medicationCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toMedicationXml (     (MedicationRequestMedicationReference p)) = Prop     "medicationReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml MedicationRequest where
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
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    statusReason <- optional $ Xmlbf.pElement "statusReason" Xmlbf.fromXml
    intent <-            Xmlbf.pElement "intent" (Xmlbf.pAttr "value")
    category <- many     $ Xmlbf.pElement "category" Xmlbf.fromXml
    priority <- optional $ Xmlbf.pElement "priority" (Xmlbf.pAttr "value")
    doNotPerform <- optional $ Xmlbf.pElement "doNotPerform" (Xmlbf.pAttr "value")
    reported <- fromReportedXml
    medication <- fromMedicationXml
    subject <-            Xmlbf.pElement "subject" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    supportingInformation <- many     $ Xmlbf.pElement "supportingInformation" Xmlbf.fromXml
    authoredOn <- optional $ Xmlbf.pElement "authoredOn" (Xmlbf.pAttr "value")
    requester <- optional $ Xmlbf.pElement "requester" Xmlbf.fromXml
    performer <- optional $ Xmlbf.pElement "performer" Xmlbf.fromXml
    performerType <- optional $ Xmlbf.pElement "performerType" Xmlbf.fromXml
    recorder <- optional $ Xmlbf.pElement "recorder" Xmlbf.fromXml
    reasonCode <- many     $ Xmlbf.pElement "reasonCode" Xmlbf.fromXml
    reasonReference <- many     $ Xmlbf.pElement "reasonReference" Xmlbf.fromXml
    instantiatesCanonical <- many     $ Xmlbf.pElement "instantiatesCanonical" (Xmlbf.pAttr "value")
    instantiatesUri <- many     $ Xmlbf.pElement "instantiatesUri" (Xmlbf.pAttr "value")
    basedOn <- many     $ Xmlbf.pElement "basedOn" Xmlbf.fromXml
    groupIdentifier <- optional $ Xmlbf.pElement "groupIdentifier" Xmlbf.fromXml
    courseOfTherapyType <- optional $ Xmlbf.pElement "courseOfTherapyType" Xmlbf.fromXml
    insurance <- many     $ Xmlbf.pElement "insurance" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    dosageInstruction <- many     $ Xmlbf.pElement "dosageInstruction" Xmlbf.fromXml
    dispenseRequest <- optional $ Xmlbf.pElement "dispenseRequest" Xmlbf.fromXml
    substitution <- optional $ Xmlbf.pElement "substitution" Xmlbf.fromXml
    priorPrescription <- optional $ Xmlbf.pElement "priorPrescription" Xmlbf.fromXml
    detectedIssue <- many     $ Xmlbf.pElement "detectedIssue" Xmlbf.fromXml
    eventHistory <- many     $ Xmlbf.pElement "eventHistory" Xmlbf.fromXml
    return MedicationRequest {
            medicationRequestId = fmap fromId id
          , medicationRequestMeta = meta
          , medicationRequestImplicitRules = fmap fromUri implicitRules
          , medicationRequestLanguage = fmap fromLanguage language
          , medicationRequestText = text
--          , medicationRequestContained = contained
          , medicationRequestExtension = extension
          , medicationRequestModifierExtension = modifierExtension
          , medicationRequestIdentifier = identifier
          , medicationRequestStatus =      fromMedicationRequestStatus status
          , medicationRequestStatusReason = statusReason
          , medicationRequestIntent =      fromMedicationRequestIntent intent
          , medicationRequestCategory = category
          , medicationRequestPriority = fmap fromMedicationRequestPriority priority
          , medicationRequestDoNotPerform = fmap fromBoolean doNotPerform
          , medicationRequestReported = reported
          , medicationRequestMedication = medication
          , medicationRequestSubject = subject
          , medicationRequestEncounter = encounter
          , medicationRequestSupportingInformation = supportingInformation
          , medicationRequestAuthoredOn = fmap fromDateTime authoredOn
          , medicationRequestRequester = requester
          , medicationRequestPerformer = performer
          , medicationRequestPerformerType = performerType
          , medicationRequestRecorder = recorder
          , medicationRequestReasonCode = reasonCode
          , medicationRequestReasonReference = reasonReference
          , medicationRequestInstantiatesCanonical = fmap fromCanonical instantiatesCanonical
          , medicationRequestInstantiatesUri = fmap fromUri instantiatesUri
          , medicationRequestBasedOn = basedOn
          , medicationRequestGroupIdentifier = groupIdentifier
          , medicationRequestCourseOfTherapyType = courseOfTherapyType
          , medicationRequestInsurance = insurance
          , medicationRequestNote = note
          , medicationRequestDosageInstruction = dosageInstruction
          , medicationRequestDispenseRequest = dispenseRequest
          , medicationRequestSubstitution = substitution
          , medicationRequestPriorPrescription = priorPrescription
          , medicationRequestDetectedIssue = detectedIssue
          , medicationRequestEventHistory = eventHistory
          }

    where 
      fromReportedXml = parseReportedBoolean <|> parseReportedReference <|> pure Nothing
      parseReportedBoolean = do
                has <- Xmlbf.pElement "reportedBoolean" (Xmlbf.pAttr "value")
                return $ Just (MedicationRequestReportedBoolean (     fromBoolean has))
      parseReportedReference = do
                has <- Xmlbf.pElement "reportedReference" Xmlbf.fromXml
                return $ Just (MedicationRequestReportedReference (                      has))
      fromMedicationXml = parseMedicationCodeableConcept <|> parseMedicationReference
      parseMedicationCodeableConcept = do
                has <- Xmlbf.pElement "medicationCodeableConcept" Xmlbf.fromXml
                return $ MedicationRequestMedicationCodeableConcept (                      has)
      parseMedicationReference = do
                has <- Xmlbf.pElement "medicationReference" Xmlbf.fromXml
                return $ MedicationRequestMedicationReference (                      has)


data MedicationRequestInitialFill = MedicationRequestInitialFill {
    medicationRequestInitialFillAttrId :: Maybe Text
  , medicationRequestInitialFillExtension :: [Extension]
  , medicationRequestInitialFillModifierExtension :: [Extension]
  , medicationRequestInitialFillQuantity :: Maybe Quantity
  , medicationRequestInitialFillDuration :: Maybe Duration
  }
  deriving (Eq, Show)
--

instance ToJSON MedicationRequestInitialFill where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (medicationRequestInitialFillAttrId p)
    ,  "extension" .= toJSON (medicationRequestInitialFillExtension p)
    ,  "modifierExtension" .= toJSON (medicationRequestInitialFillModifierExtension p)
    ,  "quantity" .= toJSON (medicationRequestInitialFillQuantity p)
    ,  "duration" .= toJSON (medicationRequestInitialFillDuration p)
    ]
instance FromJSON MedicationRequestInitialFill where
  parseJSON = withObject "MedicationRequestInitialFill" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        quantity <- o .:? "quantity"
        duration <- o .:? "duration"
        return MedicationRequestInitialFill{
            medicationRequestInitialFillAttrId = id
          , medicationRequestInitialFillExtension = extension
          , medicationRequestInitialFillModifierExtension = modifierExtension
          , medicationRequestInitialFillQuantity = quantity
          , medicationRequestInitialFillDuration = duration
          }
instance Xmlbf.ToXml MedicationRequestInitialFill where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (medicationRequestInitialFillAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (medicationRequestInitialFillExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (medicationRequestInitialFillModifierExtension p))
             , OptProp  "quantity" (fmap Xmlbf.toXml (medicationRequestInitialFillQuantity p))
             , OptProp  "duration" (fmap Xmlbf.toXml (medicationRequestInitialFillDuration p))
             ]
instance Xmlbf.FromXml MedicationRequestInitialFill where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    quantity <- optional $ Xmlbf.pElement "quantity" Xmlbf.fromXml
    duration <- optional $ Xmlbf.pElement "duration" Xmlbf.fromXml
    return MedicationRequestInitialFill {
            medicationRequestInitialFillAttrId = id
          , medicationRequestInitialFillExtension = extension
          , medicationRequestInitialFillModifierExtension = modifierExtension
          , medicationRequestInitialFillQuantity = quantity
          , medicationRequestInitialFillDuration = duration
          }



data MedicationRequestSubstitutionAllowed
    = MedicationRequestSubstitutionAllowedBoolean Boolean
    | MedicationRequestSubstitutionAllowedCodeableConcept CodeableConcept
    deriving (Eq, Show)

data MedicationRequestSubstitution = MedicationRequestSubstitution {
    medicationRequestSubstitutionAttrId :: Maybe Text
  , medicationRequestSubstitutionExtension :: [Extension]
  , medicationRequestSubstitutionModifierExtension :: [Extension]
  , medicationRequestSubstitutionAllowedBoolean :: Boolean
  , medicationRequestSubstitutionAllowedCodeableConcept :: CodeableConcept
  , medicationRequestSubstitutionReason :: Maybe CodeableConcept
  }
  deriving (Eq, Show)
--

instance ToJSON MedicationRequestSubstitution where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (medicationRequestSubstitutionAttrId p)
    ,  "extension" .= toJSON (medicationRequestSubstitutionExtension p)
    ,  "modifierExtension" .= toJSON (medicationRequestSubstitutionModifierExtension p)
    ,  "allowedBoolean" .= toJSON (medicationRequestSubstitutionAllowedBoolean p)
    ,  "allowedCodeableConcept" .= toJSON (medicationRequestSubstitutionAllowedCodeableConcept p)
    ,  "reason" .= toJSON (medicationRequestSubstitutionReason p)
    ]
instance FromJSON MedicationRequestSubstitution where
  parseJSON = withObject "MedicationRequestSubstitution" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        allowedBoolean <- o .:  "allowedBoolean"
        allowedCodeableConcept <- o .:  "allowedCodeableConcept"
        reason <- o .:? "reason"
        return MedicationRequestSubstitution{
            medicationRequestSubstitutionAttrId = id
          , medicationRequestSubstitutionExtension = extension
          , medicationRequestSubstitutionModifierExtension = modifierExtension
          , medicationRequestSubstitutionAllowedBoolean = allowedBoolean
          , medicationRequestSubstitutionAllowedCodeableConcept = allowedCodeableConcept
          , medicationRequestSubstitutionReason = reason
          }
instance Xmlbf.ToXml MedicationRequestSubstitution where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (medicationRequestSubstitutionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (medicationRequestSubstitutionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (medicationRequestSubstitutionModifierExtension p))
             , Val      "allowedBoolean" (     toBoolean (medicationRequestSubstitutionAllowedBoolean p))
             , Prop     "allowedCodeableConcept" (HM.empty, Xmlbf.toXml (medicationRequestSubstitutionAllowedCodeableConcept p))
             , OptProp  "reason" (fmap Xmlbf.toXml (medicationRequestSubstitutionReason p))
             ]
instance Xmlbf.FromXml MedicationRequestSubstitution where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    allowedBoolean <-            Xmlbf.pElement "allowedBoolean" (Xmlbf.pAttr "value")
    allowedCodeableConcept <-            Xmlbf.pElement "allowedCodeableConcept" Xmlbf.fromXml
    reason <- optional $ Xmlbf.pElement "reason" Xmlbf.fromXml
    return MedicationRequestSubstitution {
            medicationRequestSubstitutionAttrId = id
          , medicationRequestSubstitutionExtension = extension
          , medicationRequestSubstitutionModifierExtension = modifierExtension
          , medicationRequestSubstitutionAllowedBoolean =      fromBoolean allowedBoolean
          , medicationRequestSubstitutionAllowedCodeableConcept = allowedCodeableConcept
          , medicationRequestSubstitutionReason = reason
          }



data MedicationRequestDispenseRequest = MedicationRequestDispenseRequest {
    medicationRequestDispenseRequestAttrId :: Maybe Text
  , medicationRequestDispenseRequestExtension :: [Extension]
  , medicationRequestDispenseRequestModifierExtension :: [Extension]
  , medicationRequestDispenseRequestInitialFill :: Maybe MedicationRequestInitialFill
  , medicationRequestDispenseRequestDispenseInterval :: Maybe Duration
  , medicationRequestDispenseRequestValidityPeriod :: Maybe Period
  , medicationRequestDispenseRequestNumberOfRepeatsAllowed :: Maybe UnsignedInt
  , medicationRequestDispenseRequestQuantity :: Maybe Quantity
  , medicationRequestDispenseRequestExpectedSupplyDuration :: Maybe Duration
  , medicationRequestDispenseRequestPerformer :: Maybe Reference
  }
  deriving (Eq, Show)
--

instance ToJSON MedicationRequestDispenseRequest where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (medicationRequestDispenseRequestAttrId p)
    ,  "extension" .= toJSON (medicationRequestDispenseRequestExtension p)
    ,  "modifierExtension" .= toJSON (medicationRequestDispenseRequestModifierExtension p)
    ,  "initialFill" .= toJSON (medicationRequestDispenseRequestInitialFill p)
    ,  "dispenseInterval" .= toJSON (medicationRequestDispenseRequestDispenseInterval p)
    ,  "validityPeriod" .= toJSON (medicationRequestDispenseRequestValidityPeriod p)
    ,  "numberOfRepeatsAllowed" .= toJSON (medicationRequestDispenseRequestNumberOfRepeatsAllowed p)
    ,  "quantity" .= toJSON (medicationRequestDispenseRequestQuantity p)
    ,  "expectedSupplyDuration" .= toJSON (medicationRequestDispenseRequestExpectedSupplyDuration p)
    ,  "performer" .= toJSON (medicationRequestDispenseRequestPerformer p)
    ]
instance FromJSON MedicationRequestDispenseRequest where
  parseJSON = withObject "MedicationRequestDispenseRequest" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        initialFill <- o .:? "initialFill"
        dispenseInterval <- o .:? "dispenseInterval"
        validityPeriod <- o .:? "validityPeriod"
        numberOfRepeatsAllowed <- o .:? "numberOfRepeatsAllowed"
        quantity <- o .:? "quantity"
        expectedSupplyDuration <- o .:? "expectedSupplyDuration"
        performer <- o .:? "performer"
        return MedicationRequestDispenseRequest{
            medicationRequestDispenseRequestAttrId = id
          , medicationRequestDispenseRequestExtension = extension
          , medicationRequestDispenseRequestModifierExtension = modifierExtension
          , medicationRequestDispenseRequestInitialFill = initialFill
          , medicationRequestDispenseRequestDispenseInterval = dispenseInterval
          , medicationRequestDispenseRequestValidityPeriod = validityPeriod
          , medicationRequestDispenseRequestNumberOfRepeatsAllowed = numberOfRepeatsAllowed
          , medicationRequestDispenseRequestQuantity = quantity
          , medicationRequestDispenseRequestExpectedSupplyDuration = expectedSupplyDuration
          , medicationRequestDispenseRequestPerformer = performer
          }
instance Xmlbf.ToXml MedicationRequestDispenseRequest where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (medicationRequestDispenseRequestAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (medicationRequestDispenseRequestExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (medicationRequestDispenseRequestModifierExtension p))
             , OptProp  "initialFill" (fmap Xmlbf.toXml (medicationRequestDispenseRequestInitialFill p))
             , OptProp  "dispenseInterval" (fmap Xmlbf.toXml (medicationRequestDispenseRequestDispenseInterval p))
             , OptProp  "validityPeriod" (fmap Xmlbf.toXml (medicationRequestDispenseRequestValidityPeriod p))
             , OptVal   "numberOfRepeatsAllowed" (fmap toUnsignedInt (medicationRequestDispenseRequestNumberOfRepeatsAllowed p))
             , OptProp  "quantity" (fmap Xmlbf.toXml (medicationRequestDispenseRequestQuantity p))
             , OptProp  "expectedSupplyDuration" (fmap Xmlbf.toXml (medicationRequestDispenseRequestExpectedSupplyDuration p))
             , OptProp  "performer" (fmap Xmlbf.toXml (medicationRequestDispenseRequestPerformer p))
             ]
instance Xmlbf.FromXml MedicationRequestDispenseRequest where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    initialFill <- optional $ Xmlbf.pElement "initialFill" Xmlbf.fromXml
    dispenseInterval <- optional $ Xmlbf.pElement "dispenseInterval" Xmlbf.fromXml
    validityPeriod <- optional $ Xmlbf.pElement "validityPeriod" Xmlbf.fromXml
    numberOfRepeatsAllowed <- optional $ Xmlbf.pElement "numberOfRepeatsAllowed" (Xmlbf.pAttr "value")
    quantity <- optional $ Xmlbf.pElement "quantity" Xmlbf.fromXml
    expectedSupplyDuration <- optional $ Xmlbf.pElement "expectedSupplyDuration" Xmlbf.fromXml
    performer <- optional $ Xmlbf.pElement "performer" Xmlbf.fromXml
    return MedicationRequestDispenseRequest {
            medicationRequestDispenseRequestAttrId = id
          , medicationRequestDispenseRequestExtension = extension
          , medicationRequestDispenseRequestModifierExtension = modifierExtension
          , medicationRequestDispenseRequestInitialFill = initialFill
          , medicationRequestDispenseRequestDispenseInterval = dispenseInterval
          , medicationRequestDispenseRequestValidityPeriod = validityPeriod
          , medicationRequestDispenseRequestNumberOfRepeatsAllowed = fmap fromUnsignedInt numberOfRepeatsAllowed
          , medicationRequestDispenseRequestQuantity = quantity
          , medicationRequestDispenseRequestExpectedSupplyDuration = expectedSupplyDuration
          , medicationRequestDispenseRequestPerformer = performer
          }




