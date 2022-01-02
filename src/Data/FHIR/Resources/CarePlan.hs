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
-- FHIR 4.0.0 CarePlan
--

module Data.FHIR.Resources.CarePlan where

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

data CarePlanStatus
    = CPSDraft
    | CPSActive
    | CPSOnHold
    | CPSRevoked
    | CPSCompleted
    | CPSEnteredInError
    | CPSUnknown
  deriving (Eq, Show)

instance ToJSON CarePlanStatus where
    toJSON CPSDraft = String "draft"
    toJSON CPSActive = String "active"
    toJSON CPSOnHold = String "on-hold"
    toJSON CPSRevoked = String "revoked"
    toJSON CPSCompleted = String "completed"
    toJSON CPSEnteredInError = String "entered-in-error"
    toJSON CPSUnknown = String "unknown"
instance FromJSON CarePlanStatus where
    parseJSON "draft" = return CPSDraft
    parseJSON "active" = return CPSActive
    parseJSON "on-hold" = return CPSOnHold
    parseJSON "revoked" = return CPSRevoked
    parseJSON "completed" = return CPSCompleted
    parseJSON "entered-in-error" = return CPSEnteredInError
    parseJSON "unknown" = return CPSUnknown

toCarePlanStatus CPSDraft = "draft"
toCarePlanStatus CPSActive = "active"
toCarePlanStatus CPSOnHold = "on-hold"
toCarePlanStatus CPSRevoked = "revoked"
toCarePlanStatus CPSCompleted = "completed"
toCarePlanStatus CPSEnteredInError = "entered-in-error"
toCarePlanStatus CPSUnknown = "unknown"
fromCarePlanStatus "draft" = CPSDraft
fromCarePlanStatus "active" = CPSActive
fromCarePlanStatus "on-hold" = CPSOnHold
fromCarePlanStatus "revoked" = CPSRevoked
fromCarePlanStatus "completed" = CPSCompleted
fromCarePlanStatus "entered-in-error" = CPSEnteredInError
fromCarePlanStatus "unknown" = CPSUnknown


data CarePlanIntent
    = CPIProposal
    | CPIPlan
    | CPIOrder
    | CPIOption
  deriving (Eq, Show)

instance ToJSON CarePlanIntent where
    toJSON CPIProposal = String "proposal"
    toJSON CPIPlan = String "plan"
    toJSON CPIOrder = String "order"
    toJSON CPIOption = String "option"
instance FromJSON CarePlanIntent where
    parseJSON "proposal" = return CPIProposal
    parseJSON "plan" = return CPIPlan
    parseJSON "order" = return CPIOrder
    parseJSON "option" = return CPIOption

toCarePlanIntent CPIProposal = "proposal"
toCarePlanIntent CPIPlan = "plan"
toCarePlanIntent CPIOrder = "order"
toCarePlanIntent CPIOption = "option"
fromCarePlanIntent "proposal" = CPIProposal
fromCarePlanIntent "plan" = CPIPlan
fromCarePlanIntent "order" = CPIOrder
fromCarePlanIntent "option" = CPIOption


data CarePlan = CarePlan {
    carePlanId :: Maybe Id
  , carePlanMeta :: Maybe Meta
  , carePlanImplicitRules :: Maybe Uri
  , carePlanLanguage :: Maybe Language
  , carePlanText :: Maybe Narrative
--    carePlanContained :: [ResourceContainer]
  , carePlanExtension :: [Extension]
  , carePlanModifierExtension :: [Extension]
  , carePlanIdentifier :: [Identifier]
  , carePlanInstantiatesCanonical :: [Canonical]
  , carePlanInstantiatesUri :: [Uri]
  , carePlanBasedOn :: [Reference]
  , carePlanReplaces :: [Reference]
  , carePlanPartOf :: [Reference]
  , carePlanStatus :: CarePlanStatus
  , carePlanIntent :: CarePlanIntent
  , carePlanCategory :: [CodeableConcept]
  , carePlanTitle :: Maybe Text
  , carePlanDescription :: Maybe Text
  , carePlanSubject :: Reference
  , carePlanEncounter :: Maybe Reference
  , carePlanPeriod :: Maybe Period
  , carePlanCreated :: Maybe DateTime
  , carePlanAuthor :: Maybe Reference
  , carePlanContributor :: [Reference]
  , carePlanCareTeam :: [Reference]
  , carePlanAddresses :: [Reference]
  , carePlanSupportingInfo :: [Reference]
  , carePlanGoal :: [Reference]
  , carePlanActivity :: [CarePlanActivity]
  , carePlanNote :: [Annotation]
  }
--

instance ToJSON CarePlan where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "CarePlan")
    ,  "id" .= toJSON (carePlanId p)
    ,  "meta" .= toJSON (carePlanMeta p)
    ,  "implicitRules" .= toJSON (carePlanImplicitRules p)
    ,  "language" .= toJSON (carePlanLanguage p)
    ,  "text" .= toJSON (carePlanText p)
--    , "contained" .= toJSON (carePlanContained p)
    ,  "extension" .= toJSON (carePlanExtension p)
    ,  "modifierExtension" .= toJSON (carePlanModifierExtension p)
    ,  "identifier" .= toJSON (carePlanIdentifier p)
    ,  "instantiatesCanonical" .= toJSON (carePlanInstantiatesCanonical p)
    ,  "instantiatesUri" .= toJSON (carePlanInstantiatesUri p)
    ,  "basedOn" .= toJSON (carePlanBasedOn p)
    ,  "replaces" .= toJSON (carePlanReplaces p)
    ,  "partOf" .= toJSON (carePlanPartOf p)
    ,  "status" .= toJSON (carePlanStatus p)
    ,  "intent" .= toJSON (carePlanIntent p)
    ,  "category" .= toJSON (carePlanCategory p)
    ,  "title" .= toJSON (carePlanTitle p)
    ,  "description" .= toJSON (carePlanDescription p)
    ,  "subject" .= toJSON (carePlanSubject p)
    ,  "encounter" .= toJSON (carePlanEncounter p)
    ,  "period" .= toJSON (carePlanPeriod p)
    ,  "created" .= toJSON (carePlanCreated p)
    ,  "author" .= toJSON (carePlanAuthor p)
    ,  "contributor" .= toJSON (carePlanContributor p)
    ,  "careTeam" .= toJSON (carePlanCareTeam p)
    ,  "addresses" .= toJSON (carePlanAddresses p)
    ,  "supportingInfo" .= toJSON (carePlanSupportingInfo p)
    ,  "goal" .= toJSON (carePlanGoal p)
    ,  "activity" .= toJSON (carePlanActivity p)
    ,  "note" .= toJSON (carePlanNote p)
    ]
instance FromJSON CarePlan where
  parseJSON = withObject "CarePlan" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "CarePlan" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        instantiatesCanonical <- o .:? "instantiatesCanonical" .!= []
        instantiatesUri <- o .:? "instantiatesUri" .!= []
        basedOn <- o .:? "basedOn" .!= []
        replaces <- o .:? "replaces" .!= []
        partOf <- o .:? "partOf" .!= []
        status <- o .:  "status"
        intent <- o .:  "intent"
        category <- o .:? "category" .!= []
        title <- o .:? "title"
        description <- o .:? "description"
        subject <- o .:  "subject"
        encounter <- o .:? "encounter"
        period <- o .:? "period"
        created <- o .:? "created"
        author <- o .:? "author"
        contributor <- o .:? "contributor" .!= []
        careTeam <- o .:? "careTeam" .!= []
        addresses <- o .:? "addresses" .!= []
        supportingInfo <- o .:? "supportingInfo" .!= []
        goal <- o .:? "goal" .!= []
        activity <- o .:? "activity" .!= []
        note <- o .:? "note" .!= []
        return CarePlan{
            carePlanId = id
          , carePlanMeta = meta
          , carePlanImplicitRules = implicitRules
          , carePlanLanguage = language
          , carePlanText = text
--          , carePlanContained = contained
          , carePlanExtension = extension
          , carePlanModifierExtension = modifierExtension
          , carePlanIdentifier = identifier
          , carePlanInstantiatesCanonical = instantiatesCanonical
          , carePlanInstantiatesUri = instantiatesUri
          , carePlanBasedOn = basedOn
          , carePlanReplaces = replaces
          , carePlanPartOf = partOf
          , carePlanStatus = status
          , carePlanIntent = intent
          , carePlanCategory = category
          , carePlanTitle = title
          , carePlanDescription = description
          , carePlanSubject = subject
          , carePlanEncounter = encounter
          , carePlanPeriod = period
          , carePlanCreated = created
          , carePlanAuthor = author
          , carePlanContributor = contributor
          , carePlanCareTeam = careTeam
          , carePlanAddresses = addresses
          , carePlanSupportingInfo = supportingInfo
          , carePlanGoal = goal
          , carePlanActivity = activity
          , carePlanNote = note
          }
      _ -> fail "not a CarePlan"
instance Xmlbf.ToXml CarePlan where
  toXml p = Xmlbf.element "CarePlan" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (carePlanId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (carePlanMeta p))
             , OptVal   "implicitRules" (fmap toUri (carePlanImplicitRules p))
             , OptVal   "language" (fmap toLanguage (carePlanLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (carePlanText p))
--             , PropList "contained" (fmap Xmlbf.toXml (carePlanContained p))
             , PropList "extension" (fmap Xmlbf.toXml (carePlanExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (carePlanModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (carePlanIdentifier p))
             , ValList  "instantiatesCanonical" (fmap toCanonical (carePlanInstantiatesCanonical p))
             , ValList  "instantiatesUri" (fmap toUri (carePlanInstantiatesUri p))
             , PropList "basedOn" (fmap Xmlbf.toXml (carePlanBasedOn p))
             , PropList "replaces" (fmap Xmlbf.toXml (carePlanReplaces p))
             , PropList "partOf" (fmap Xmlbf.toXml (carePlanPartOf p))
             , Val      "status" (     toCarePlanStatus (carePlanStatus p))
             , Val      "intent" (     toCarePlanIntent (carePlanIntent p))
             , PropList "category" (fmap Xmlbf.toXml (carePlanCategory p))
             , OptVal   "title" (fmap toString (carePlanTitle p))
             , OptVal   "description" (fmap toString (carePlanDescription p))
             , Prop     "subject" (HM.empty, Xmlbf.toXml (carePlanSubject p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (carePlanEncounter p))
             , OptProp  "period" (fmap Xmlbf.toXml (carePlanPeriod p))
             , OptVal   "created" (fmap toDateTime (carePlanCreated p))
             , OptProp  "author" (fmap Xmlbf.toXml (carePlanAuthor p))
             , PropList "contributor" (fmap Xmlbf.toXml (carePlanContributor p))
             , PropList "careTeam" (fmap Xmlbf.toXml (carePlanCareTeam p))
             , PropList "addresses" (fmap Xmlbf.toXml (carePlanAddresses p))
             , PropList "supportingInfo" (fmap Xmlbf.toXml (carePlanSupportingInfo p))
             , PropList "goal" (fmap Xmlbf.toXml (carePlanGoal p))
             , PropList "activity" (fmap Xmlbf.toXml (carePlanActivity p))
             , PropList "note" (fmap Xmlbf.toXml (carePlanNote p))
             ]
instance Xmlbf.FromXml CarePlan where
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
    instantiatesCanonical <- many     $ Xmlbf.pElement "instantiatesCanonical" (Xmlbf.pAttr "value")
    instantiatesUri <- many     $ Xmlbf.pElement "instantiatesUri" (Xmlbf.pAttr "value")
    basedOn <- many     $ Xmlbf.pElement "basedOn" Xmlbf.fromXml
    replaces <- many     $ Xmlbf.pElement "replaces" Xmlbf.fromXml
    partOf <- many     $ Xmlbf.pElement "partOf" Xmlbf.fromXml
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    intent <-            Xmlbf.pElement "intent" (Xmlbf.pAttr "value")
    category <- many     $ Xmlbf.pElement "category" Xmlbf.fromXml
    title <- optional $ Xmlbf.pElement "title" (Xmlbf.pAttr "value")
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    subject <-            Xmlbf.pElement "subject" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    created <- optional $ Xmlbf.pElement "created" (Xmlbf.pAttr "value")
    author <- optional $ Xmlbf.pElement "author" Xmlbf.fromXml
    contributor <- many     $ Xmlbf.pElement "contributor" Xmlbf.fromXml
    careTeam <- many     $ Xmlbf.pElement "careTeam" Xmlbf.fromXml
    addresses <- many     $ Xmlbf.pElement "addresses" Xmlbf.fromXml
    supportingInfo <- many     $ Xmlbf.pElement "supportingInfo" Xmlbf.fromXml
    goal <- many     $ Xmlbf.pElement "goal" Xmlbf.fromXml
    activity <- many     $ Xmlbf.pElement "activity" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    return CarePlan {
            carePlanId = fmap fromId id
          , carePlanMeta = meta
          , carePlanImplicitRules = fmap fromUri implicitRules
          , carePlanLanguage = fmap fromLanguage language
          , carePlanText = text
--          , carePlanContained = contained
          , carePlanExtension = extension
          , carePlanModifierExtension = modifierExtension
          , carePlanIdentifier = identifier
          , carePlanInstantiatesCanonical = fmap fromCanonical instantiatesCanonical
          , carePlanInstantiatesUri = fmap fromUri instantiatesUri
          , carePlanBasedOn = basedOn
          , carePlanReplaces = replaces
          , carePlanPartOf = partOf
          , carePlanStatus =      fromCarePlanStatus status
          , carePlanIntent =      fromCarePlanIntent intent
          , carePlanCategory = category
          , carePlanTitle = fmap fromString title
          , carePlanDescription = fmap fromString description
          , carePlanSubject = subject
          , carePlanEncounter = encounter
          , carePlanPeriod = period
          , carePlanCreated = fmap fromDateTime created
          , carePlanAuthor = author
          , carePlanContributor = contributor
          , carePlanCareTeam = careTeam
          , carePlanAddresses = addresses
          , carePlanSupportingInfo = supportingInfo
          , carePlanGoal = goal
          , carePlanActivity = activity
          , carePlanNote = note
          }



data CarePlanActivity = CarePlanActivity {
    carePlanActivityAttrId :: Maybe Text
  , carePlanActivityExtension :: [Extension]
  , carePlanActivityModifierExtension :: [Extension]
  , carePlanActivityOutcomeCodeableConcept :: [CodeableConcept]
  , carePlanActivityOutcomeReference :: [Reference]
  , carePlanActivityProgress :: [Annotation]
  , carePlanActivityReference :: Maybe Reference
  , carePlanActivityDetail :: Maybe CarePlanDetail
  }
--

instance ToJSON CarePlanActivity where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (carePlanActivityAttrId p)
    ,  "extension" .= toJSON (carePlanActivityExtension p)
    ,  "modifierExtension" .= toJSON (carePlanActivityModifierExtension p)
    ,  "outcomeCodeableConcept" .= toJSON (carePlanActivityOutcomeCodeableConcept p)
    ,  "outcomeReference" .= toJSON (carePlanActivityOutcomeReference p)
    ,  "progress" .= toJSON (carePlanActivityProgress p)
    ,  "reference" .= toJSON (carePlanActivityReference p)
    ,  "detail" .= toJSON (carePlanActivityDetail p)
    ]
instance FromJSON CarePlanActivity where
  parseJSON = withObject "CarePlanActivity" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        outcomeCodeableConcept <- o .:? "outcomeCodeableConcept" .!= []
        outcomeReference <- o .:? "outcomeReference" .!= []
        progress <- o .:? "progress" .!= []
        reference <- o .:? "reference"
        detail <- o .:? "detail"
        return CarePlanActivity{
            carePlanActivityAttrId = id
          , carePlanActivityExtension = extension
          , carePlanActivityModifierExtension = modifierExtension
          , carePlanActivityOutcomeCodeableConcept = outcomeCodeableConcept
          , carePlanActivityOutcomeReference = outcomeReference
          , carePlanActivityProgress = progress
          , carePlanActivityReference = reference
          , carePlanActivityDetail = detail
          }
instance Xmlbf.ToXml CarePlanActivity where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (carePlanActivityAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (carePlanActivityExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (carePlanActivityModifierExtension p))
             , PropList "outcomeCodeableConcept" (fmap Xmlbf.toXml (carePlanActivityOutcomeCodeableConcept p))
             , PropList "outcomeReference" (fmap Xmlbf.toXml (carePlanActivityOutcomeReference p))
             , PropList "progress" (fmap Xmlbf.toXml (carePlanActivityProgress p))
             , OptProp  "reference" (fmap Xmlbf.toXml (carePlanActivityReference p))
             , OptProp  "detail" (fmap Xmlbf.toXml (carePlanActivityDetail p))
             ]
instance Xmlbf.FromXml CarePlanActivity where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    outcomeCodeableConcept <- many     $ Xmlbf.pElement "outcomeCodeableConcept" Xmlbf.fromXml
    outcomeReference <- many     $ Xmlbf.pElement "outcomeReference" Xmlbf.fromXml
    progress <- many     $ Xmlbf.pElement "progress" Xmlbf.fromXml
    reference <- optional $ Xmlbf.pElement "reference" Xmlbf.fromXml
    detail <- optional $ Xmlbf.pElement "detail" Xmlbf.fromXml
    return CarePlanActivity {
            carePlanActivityAttrId = id
          , carePlanActivityExtension = extension
          , carePlanActivityModifierExtension = modifierExtension
          , carePlanActivityOutcomeCodeableConcept = outcomeCodeableConcept
          , carePlanActivityOutcomeReference = outcomeReference
          , carePlanActivityProgress = progress
          , carePlanActivityReference = reference
          , carePlanActivityDetail = detail
          }



data CarePlanDetailKind
    = CPDKAppointment
    | CPDKCommunicationRequest
    | CPDKDeviceRequest
    | CPDKMedicationRequest
    | CPDKNutritionOrder
    | CPDKTask
    | CPDKServiceRequest
    | CPDKVisionPrescription
  deriving (Eq, Show)

instance ToJSON CarePlanDetailKind where
    toJSON CPDKAppointment = String "Appointment"
    toJSON CPDKCommunicationRequest = String "CommunicationRequest"
    toJSON CPDKDeviceRequest = String "DeviceRequest"
    toJSON CPDKMedicationRequest = String "MedicationRequest"
    toJSON CPDKNutritionOrder = String "NutritionOrder"
    toJSON CPDKTask = String "Task"
    toJSON CPDKServiceRequest = String "ServiceRequest"
    toJSON CPDKVisionPrescription = String "VisionPrescription"
instance FromJSON CarePlanDetailKind where
    parseJSON "Appointment" = return CPDKAppointment
    parseJSON "CommunicationRequest" = return CPDKCommunicationRequest
    parseJSON "DeviceRequest" = return CPDKDeviceRequest
    parseJSON "MedicationRequest" = return CPDKMedicationRequest
    parseJSON "NutritionOrder" = return CPDKNutritionOrder
    parseJSON "Task" = return CPDKTask
    parseJSON "ServiceRequest" = return CPDKServiceRequest
    parseJSON "VisionPrescription" = return CPDKVisionPrescription

toCarePlanDetailKind CPDKAppointment = "Appointment"
toCarePlanDetailKind CPDKCommunicationRequest = "CommunicationRequest"
toCarePlanDetailKind CPDKDeviceRequest = "DeviceRequest"
toCarePlanDetailKind CPDKMedicationRequest = "MedicationRequest"
toCarePlanDetailKind CPDKNutritionOrder = "NutritionOrder"
toCarePlanDetailKind CPDKTask = "Task"
toCarePlanDetailKind CPDKServiceRequest = "ServiceRequest"
toCarePlanDetailKind CPDKVisionPrescription = "VisionPrescription"
fromCarePlanDetailKind "Appointment" = CPDKAppointment
fromCarePlanDetailKind "CommunicationRequest" = CPDKCommunicationRequest
fromCarePlanDetailKind "DeviceRequest" = CPDKDeviceRequest
fromCarePlanDetailKind "MedicationRequest" = CPDKMedicationRequest
fromCarePlanDetailKind "NutritionOrder" = CPDKNutritionOrder
fromCarePlanDetailKind "Task" = CPDKTask
fromCarePlanDetailKind "ServiceRequest" = CPDKServiceRequest
fromCarePlanDetailKind "VisionPrescription" = CPDKVisionPrescription


data CarePlanDetailStatus
    = CPDSNotStarted
    | CPDSScheduled
    | CPDSInProgress
    | CPDSOnHold
    | CPDSCompleted
    | CPDSCancelled
    | CPDSStopped
    | CPDSUnknown
    | CPDSEnteredInError
  deriving (Eq, Show)

instance ToJSON CarePlanDetailStatus where
    toJSON CPDSNotStarted = String "not-started"
    toJSON CPDSScheduled = String "scheduled"
    toJSON CPDSInProgress = String "in-progress"
    toJSON CPDSOnHold = String "on-hold"
    toJSON CPDSCompleted = String "completed"
    toJSON CPDSCancelled = String "cancelled"
    toJSON CPDSStopped = String "stopped"
    toJSON CPDSUnknown = String "unknown"
    toJSON CPDSEnteredInError = String "entered-in-error"
instance FromJSON CarePlanDetailStatus where
    parseJSON "not-started" = return CPDSNotStarted
    parseJSON "scheduled" = return CPDSScheduled
    parseJSON "in-progress" = return CPDSInProgress
    parseJSON "on-hold" = return CPDSOnHold
    parseJSON "completed" = return CPDSCompleted
    parseJSON "cancelled" = return CPDSCancelled
    parseJSON "stopped" = return CPDSStopped
    parseJSON "unknown" = return CPDSUnknown
    parseJSON "entered-in-error" = return CPDSEnteredInError

toCarePlanDetailStatus CPDSNotStarted = "not-started"
toCarePlanDetailStatus CPDSScheduled = "scheduled"
toCarePlanDetailStatus CPDSInProgress = "in-progress"
toCarePlanDetailStatus CPDSOnHold = "on-hold"
toCarePlanDetailStatus CPDSCompleted = "completed"
toCarePlanDetailStatus CPDSCancelled = "cancelled"
toCarePlanDetailStatus CPDSStopped = "stopped"
toCarePlanDetailStatus CPDSUnknown = "unknown"
toCarePlanDetailStatus CPDSEnteredInError = "entered-in-error"
fromCarePlanDetailStatus "not-started" = CPDSNotStarted
fromCarePlanDetailStatus "scheduled" = CPDSScheduled
fromCarePlanDetailStatus "in-progress" = CPDSInProgress
fromCarePlanDetailStatus "on-hold" = CPDSOnHold
fromCarePlanDetailStatus "completed" = CPDSCompleted
fromCarePlanDetailStatus "cancelled" = CPDSCancelled
fromCarePlanDetailStatus "stopped" = CPDSStopped
fromCarePlanDetailStatus "unknown" = CPDSUnknown
fromCarePlanDetailStatus "entered-in-error" = CPDSEnteredInError


data CarePlanDetailScheduled
    = CarePlanDetailScheduledTiming Timing
    | CarePlanDetailScheduledPeriod Period
    | CarePlanDetailScheduledString Text
    deriving (Eq, Show)

data CarePlanDetailProduct
    = CarePlanDetailProductCodeableConcept CodeableConcept
    | CarePlanDetailProductReference Reference
    deriving (Eq, Show)

data CarePlanDetail = CarePlanDetail {
    carePlanDetailAttrId :: Maybe Text
  , carePlanDetailExtension :: [Extension]
  , carePlanDetailModifierExtension :: [Extension]
  , carePlanDetailKind :: Maybe CarePlanDetailKind
  , carePlanDetailInstantiatesCanonical :: [Canonical]
  , carePlanDetailInstantiatesUri :: [Uri]
  , carePlanDetailCode :: Maybe CodeableConcept
  , carePlanDetailReasonCode :: [CodeableConcept]
  , carePlanDetailReasonReference :: [Reference]
  , carePlanDetailGoal :: [Reference]
  , carePlanDetailStatus :: CarePlanDetailStatus
  , carePlanDetailStatusReason :: Maybe CodeableConcept
  , carePlanDetailDoNotPerform :: Maybe Boolean
  , carePlanDetailScheduledTiming :: Maybe Timing
  , carePlanDetailScheduledPeriod :: Maybe Period
  , carePlanDetailScheduledString :: Maybe Text
  , carePlanDetailLocation :: Maybe Reference
  , carePlanDetailPerformer :: [Reference]
  , carePlanDetailProductCodeableConcept :: Maybe CodeableConcept
  , carePlanDetailProductReference :: Maybe Reference
  , carePlanDetailDailyAmount :: Maybe Quantity
  , carePlanDetailQuantity :: Maybe Quantity
  , carePlanDetailDescription :: Maybe Text
  }
--

instance ToJSON CarePlanDetail where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (carePlanDetailAttrId p)
    ,  "extension" .= toJSON (carePlanDetailExtension p)
    ,  "modifierExtension" .= toJSON (carePlanDetailModifierExtension p)
    ,  "kind" .= toJSON (carePlanDetailKind p)
    ,  "instantiatesCanonical" .= toJSON (carePlanDetailInstantiatesCanonical p)
    ,  "instantiatesUri" .= toJSON (carePlanDetailInstantiatesUri p)
    ,  "code" .= toJSON (carePlanDetailCode p)
    ,  "reasonCode" .= toJSON (carePlanDetailReasonCode p)
    ,  "reasonReference" .= toJSON (carePlanDetailReasonReference p)
    ,  "goal" .= toJSON (carePlanDetailGoal p)
    ,  "status" .= toJSON (carePlanDetailStatus p)
    ,  "statusReason" .= toJSON (carePlanDetailStatusReason p)
    ,  "doNotPerform" .= toJSON (carePlanDetailDoNotPerform p)
    ,  "scheduledTiming" .= toJSON (carePlanDetailScheduledTiming p)
    ,  "scheduledPeriod" .= toJSON (carePlanDetailScheduledPeriod p)
    ,  "scheduledString" .= toJSON (carePlanDetailScheduledString p)
    ,  "location" .= toJSON (carePlanDetailLocation p)
    ,  "performer" .= toJSON (carePlanDetailPerformer p)
    ,  "productCodeableConcept" .= toJSON (carePlanDetailProductCodeableConcept p)
    ,  "productReference" .= toJSON (carePlanDetailProductReference p)
    ,  "dailyAmount" .= toJSON (carePlanDetailDailyAmount p)
    ,  "quantity" .= toJSON (carePlanDetailQuantity p)
    ,  "description" .= toJSON (carePlanDetailDescription p)
    ]
instance FromJSON CarePlanDetail where
  parseJSON = withObject "CarePlanDetail" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        kind <- o .:? "kind"
        instantiatesCanonical <- o .:? "instantiatesCanonical" .!= []
        instantiatesUri <- o .:? "instantiatesUri" .!= []
        code <- o .:? "code"
        reasonCode <- o .:? "reasonCode" .!= []
        reasonReference <- o .:? "reasonReference" .!= []
        goal <- o .:? "goal" .!= []
        status <- o .:  "status"
        statusReason <- o .:? "statusReason"
        doNotPerform <- o .:? "doNotPerform"
        scheduledTiming <- o .:? "scheduledTiming"
        scheduledPeriod <- o .:? "scheduledPeriod"
        scheduledString <- o .:? "scheduledString"
        location <- o .:? "location"
        performer <- o .:? "performer" .!= []
        productCodeableConcept <- o .:? "productCodeableConcept"
        productReference <- o .:? "productReference"
        dailyAmount <- o .:? "dailyAmount"
        quantity <- o .:? "quantity"
        description <- o .:? "description"
        return CarePlanDetail{
            carePlanDetailAttrId = id
          , carePlanDetailExtension = extension
          , carePlanDetailModifierExtension = modifierExtension
          , carePlanDetailKind = kind
          , carePlanDetailInstantiatesCanonical = instantiatesCanonical
          , carePlanDetailInstantiatesUri = instantiatesUri
          , carePlanDetailCode = code
          , carePlanDetailReasonCode = reasonCode
          , carePlanDetailReasonReference = reasonReference
          , carePlanDetailGoal = goal
          , carePlanDetailStatus = status
          , carePlanDetailStatusReason = statusReason
          , carePlanDetailDoNotPerform = doNotPerform
          , carePlanDetailScheduledTiming = scheduledTiming
          , carePlanDetailScheduledPeriod = scheduledPeriod
          , carePlanDetailScheduledString = scheduledString
          , carePlanDetailLocation = location
          , carePlanDetailPerformer = performer
          , carePlanDetailProductCodeableConcept = productCodeableConcept
          , carePlanDetailProductReference = productReference
          , carePlanDetailDailyAmount = dailyAmount
          , carePlanDetailQuantity = quantity
          , carePlanDetailDescription = description
          }
instance Xmlbf.ToXml CarePlanDetail where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (carePlanDetailAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (carePlanDetailExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (carePlanDetailModifierExtension p))
             , OptVal   "kind" (fmap toCarePlanDetailKind (carePlanDetailKind p))
             , ValList  "instantiatesCanonical" (fmap toCanonical (carePlanDetailInstantiatesCanonical p))
             , ValList  "instantiatesUri" (fmap toUri (carePlanDetailInstantiatesUri p))
             , OptProp  "code" (fmap Xmlbf.toXml (carePlanDetailCode p))
             , PropList "reasonCode" (fmap Xmlbf.toXml (carePlanDetailReasonCode p))
             , PropList "reasonReference" (fmap Xmlbf.toXml (carePlanDetailReasonReference p))
             , PropList "goal" (fmap Xmlbf.toXml (carePlanDetailGoal p))
             , Val      "status" (     toCarePlanDetailStatus (carePlanDetailStatus p))
             , OptProp  "statusReason" (fmap Xmlbf.toXml (carePlanDetailStatusReason p))
             , OptVal   "doNotPerform" (fmap toBoolean (carePlanDetailDoNotPerform p))
             , OptProp  "scheduledTiming" (fmap Xmlbf.toXml (carePlanDetailScheduledTiming p))
             , OptProp  "scheduledPeriod" (fmap Xmlbf.toXml (carePlanDetailScheduledPeriod p))
             , OptVal   "scheduledString" (fmap toString (carePlanDetailScheduledString p))
             , OptProp  "location" (fmap Xmlbf.toXml (carePlanDetailLocation p))
             , PropList "performer" (fmap Xmlbf.toXml (carePlanDetailPerformer p))
             , OptProp  "productCodeableConcept" (fmap Xmlbf.toXml (carePlanDetailProductCodeableConcept p))
             , OptProp  "productReference" (fmap Xmlbf.toXml (carePlanDetailProductReference p))
             , OptProp  "dailyAmount" (fmap Xmlbf.toXml (carePlanDetailDailyAmount p))
             , OptProp  "quantity" (fmap Xmlbf.toXml (carePlanDetailQuantity p))
             , OptVal   "description" (fmap toString (carePlanDetailDescription p))
             ]
instance Xmlbf.FromXml CarePlanDetail where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    kind <- optional $ Xmlbf.pElement "kind" (Xmlbf.pAttr "value")
    instantiatesCanonical <- many     $ Xmlbf.pElement "instantiatesCanonical" (Xmlbf.pAttr "value")
    instantiatesUri <- many     $ Xmlbf.pElement "instantiatesUri" (Xmlbf.pAttr "value")
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    reasonCode <- many     $ Xmlbf.pElement "reasonCode" Xmlbf.fromXml
    reasonReference <- many     $ Xmlbf.pElement "reasonReference" Xmlbf.fromXml
    goal <- many     $ Xmlbf.pElement "goal" Xmlbf.fromXml
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    statusReason <- optional $ Xmlbf.pElement "statusReason" Xmlbf.fromXml
    doNotPerform <- optional $ Xmlbf.pElement "doNotPerform" (Xmlbf.pAttr "value")
    scheduledTiming <- optional $ Xmlbf.pElement "scheduledTiming" Xmlbf.fromXml
    scheduledPeriod <- optional $ Xmlbf.pElement "scheduledPeriod" Xmlbf.fromXml
    scheduledString <- optional $ Xmlbf.pElement "scheduledString" (Xmlbf.pAttr "value")
    location <- optional $ Xmlbf.pElement "location" Xmlbf.fromXml
    performer <- many     $ Xmlbf.pElement "performer" Xmlbf.fromXml
    productCodeableConcept <- optional $ Xmlbf.pElement "productCodeableConcept" Xmlbf.fromXml
    productReference <- optional $ Xmlbf.pElement "productReference" Xmlbf.fromXml
    dailyAmount <- optional $ Xmlbf.pElement "dailyAmount" Xmlbf.fromXml
    quantity <- optional $ Xmlbf.pElement "quantity" Xmlbf.fromXml
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    return CarePlanDetail {
            carePlanDetailAttrId = id
          , carePlanDetailExtension = extension
          , carePlanDetailModifierExtension = modifierExtension
          , carePlanDetailKind = fmap fromCarePlanDetailKind kind
          , carePlanDetailInstantiatesCanonical = fmap fromCanonical instantiatesCanonical
          , carePlanDetailInstantiatesUri = fmap fromUri instantiatesUri
          , carePlanDetailCode = code
          , carePlanDetailReasonCode = reasonCode
          , carePlanDetailReasonReference = reasonReference
          , carePlanDetailGoal = goal
          , carePlanDetailStatus =      fromCarePlanDetailStatus status
          , carePlanDetailStatusReason = statusReason
          , carePlanDetailDoNotPerform = fmap fromBoolean doNotPerform
          , carePlanDetailScheduledTiming = scheduledTiming
          , carePlanDetailScheduledPeriod = scheduledPeriod
          , carePlanDetailScheduledString = fmap fromString scheduledString
          , carePlanDetailLocation = location
          , carePlanDetailPerformer = performer
          , carePlanDetailProductCodeableConcept = productCodeableConcept
          , carePlanDetailProductReference = productReference
          , carePlanDetailDailyAmount = dailyAmount
          , carePlanDetailQuantity = quantity
          , carePlanDetailDescription = fmap fromString description
          }




