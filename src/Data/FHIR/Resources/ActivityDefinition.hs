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
-- FHIR 4.0.0 ActivityDefinition
--

module Data.FHIR.Resources.ActivityDefinition where

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

data ActivityDefinitionKind
    = ADKAppointment
    | ADKAppointmentResponse
    | ADKCarePlan
    | ADKClaim
    | ADKCommunicationRequest
    | ADKContract
    | ADKDeviceRequest
    | ADKEnrollmentRequest
    | ADKImmunizationRecommendation
    | ADKMedicationRequest
    | ADKNutritionOrder
    | ADKServiceRequest
    | ADKSupplyRequest
    | ADKTask
    | ADKVisionPrescription
  deriving (Eq, Show)

instance ToJSON ActivityDefinitionKind where
    toJSON ADKAppointment = String "Appointment"
    toJSON ADKAppointmentResponse = String "AppointmentResponse"
    toJSON ADKCarePlan = String "CarePlan"
    toJSON ADKClaim = String "Claim"
    toJSON ADKCommunicationRequest = String "CommunicationRequest"
    toJSON ADKContract = String "Contract"
    toJSON ADKDeviceRequest = String "DeviceRequest"
    toJSON ADKEnrollmentRequest = String "EnrollmentRequest"
    toJSON ADKImmunizationRecommendation = String "ImmunizationRecommendation"
    toJSON ADKMedicationRequest = String "MedicationRequest"
    toJSON ADKNutritionOrder = String "NutritionOrder"
    toJSON ADKServiceRequest = String "ServiceRequest"
    toJSON ADKSupplyRequest = String "SupplyRequest"
    toJSON ADKTask = String "Task"
    toJSON ADKVisionPrescription = String "VisionPrescription"
instance FromJSON ActivityDefinitionKind where
    parseJSON "Appointment" = return ADKAppointment
    parseJSON "AppointmentResponse" = return ADKAppointmentResponse
    parseJSON "CarePlan" = return ADKCarePlan
    parseJSON "Claim" = return ADKClaim
    parseJSON "CommunicationRequest" = return ADKCommunicationRequest
    parseJSON "Contract" = return ADKContract
    parseJSON "DeviceRequest" = return ADKDeviceRequest
    parseJSON "EnrollmentRequest" = return ADKEnrollmentRequest
    parseJSON "ImmunizationRecommendation" = return ADKImmunizationRecommendation
    parseJSON "MedicationRequest" = return ADKMedicationRequest
    parseJSON "NutritionOrder" = return ADKNutritionOrder
    parseJSON "ServiceRequest" = return ADKServiceRequest
    parseJSON "SupplyRequest" = return ADKSupplyRequest
    parseJSON "Task" = return ADKTask
    parseJSON "VisionPrescription" = return ADKVisionPrescription

toActivityDefinitionKind ADKAppointment = "Appointment"
toActivityDefinitionKind ADKAppointmentResponse = "AppointmentResponse"
toActivityDefinitionKind ADKCarePlan = "CarePlan"
toActivityDefinitionKind ADKClaim = "Claim"
toActivityDefinitionKind ADKCommunicationRequest = "CommunicationRequest"
toActivityDefinitionKind ADKContract = "Contract"
toActivityDefinitionKind ADKDeviceRequest = "DeviceRequest"
toActivityDefinitionKind ADKEnrollmentRequest = "EnrollmentRequest"
toActivityDefinitionKind ADKImmunizationRecommendation = "ImmunizationRecommendation"
toActivityDefinitionKind ADKMedicationRequest = "MedicationRequest"
toActivityDefinitionKind ADKNutritionOrder = "NutritionOrder"
toActivityDefinitionKind ADKServiceRequest = "ServiceRequest"
toActivityDefinitionKind ADKSupplyRequest = "SupplyRequest"
toActivityDefinitionKind ADKTask = "Task"
toActivityDefinitionKind ADKVisionPrescription = "VisionPrescription"
fromActivityDefinitionKind "Appointment" = ADKAppointment
fromActivityDefinitionKind "AppointmentResponse" = ADKAppointmentResponse
fromActivityDefinitionKind "CarePlan" = ADKCarePlan
fromActivityDefinitionKind "Claim" = ADKClaim
fromActivityDefinitionKind "CommunicationRequest" = ADKCommunicationRequest
fromActivityDefinitionKind "Contract" = ADKContract
fromActivityDefinitionKind "DeviceRequest" = ADKDeviceRequest
fromActivityDefinitionKind "EnrollmentRequest" = ADKEnrollmentRequest
fromActivityDefinitionKind "ImmunizationRecommendation" = ADKImmunizationRecommendation
fromActivityDefinitionKind "MedicationRequest" = ADKMedicationRequest
fromActivityDefinitionKind "NutritionOrder" = ADKNutritionOrder
fromActivityDefinitionKind "ServiceRequest" = ADKServiceRequest
fromActivityDefinitionKind "SupplyRequest" = ADKSupplyRequest
fromActivityDefinitionKind "Task" = ADKTask
fromActivityDefinitionKind "VisionPrescription" = ADKVisionPrescription


data ActivityDefinitionIntent
    = ADIProposal
    | ADIPlan
    | ADIDirective
    | ADIOrder
    | ADIOriginalOrder
    | ADIReflexOrder
    | ADIFillerOrder
    | ADIInstanceOrder
    | ADIOption
  deriving (Eq, Show)

instance ToJSON ActivityDefinitionIntent where
    toJSON ADIProposal = String "proposal"
    toJSON ADIPlan = String "plan"
    toJSON ADIDirective = String "directive"
    toJSON ADIOrder = String "order"
    toJSON ADIOriginalOrder = String "original-order"
    toJSON ADIReflexOrder = String "reflex-order"
    toJSON ADIFillerOrder = String "filler-order"
    toJSON ADIInstanceOrder = String "instance-order"
    toJSON ADIOption = String "option"
instance FromJSON ActivityDefinitionIntent where
    parseJSON "proposal" = return ADIProposal
    parseJSON "plan" = return ADIPlan
    parseJSON "directive" = return ADIDirective
    parseJSON "order" = return ADIOrder
    parseJSON "original-order" = return ADIOriginalOrder
    parseJSON "reflex-order" = return ADIReflexOrder
    parseJSON "filler-order" = return ADIFillerOrder
    parseJSON "instance-order" = return ADIInstanceOrder
    parseJSON "option" = return ADIOption

toActivityDefinitionIntent ADIProposal = "proposal"
toActivityDefinitionIntent ADIPlan = "plan"
toActivityDefinitionIntent ADIDirective = "directive"
toActivityDefinitionIntent ADIOrder = "order"
toActivityDefinitionIntent ADIOriginalOrder = "original-order"
toActivityDefinitionIntent ADIReflexOrder = "reflex-order"
toActivityDefinitionIntent ADIFillerOrder = "filler-order"
toActivityDefinitionIntent ADIInstanceOrder = "instance-order"
toActivityDefinitionIntent ADIOption = "option"
fromActivityDefinitionIntent "proposal" = ADIProposal
fromActivityDefinitionIntent "plan" = ADIPlan
fromActivityDefinitionIntent "directive" = ADIDirective
fromActivityDefinitionIntent "order" = ADIOrder
fromActivityDefinitionIntent "original-order" = ADIOriginalOrder
fromActivityDefinitionIntent "reflex-order" = ADIReflexOrder
fromActivityDefinitionIntent "filler-order" = ADIFillerOrder
fromActivityDefinitionIntent "instance-order" = ADIInstanceOrder
fromActivityDefinitionIntent "option" = ADIOption


data ActivityDefinitionPriority
    = ADPRoutine
    | ADPUrgent
    | ADPAsap
    | ADPStat
  deriving (Eq, Show)

instance ToJSON ActivityDefinitionPriority where
    toJSON ADPRoutine = String "routine"
    toJSON ADPUrgent = String "urgent"
    toJSON ADPAsap = String "asap"
    toJSON ADPStat = String "stat"
instance FromJSON ActivityDefinitionPriority where
    parseJSON "routine" = return ADPRoutine
    parseJSON "urgent" = return ADPUrgent
    parseJSON "asap" = return ADPAsap
    parseJSON "stat" = return ADPStat

toActivityDefinitionPriority ADPRoutine = "routine"
toActivityDefinitionPriority ADPUrgent = "urgent"
toActivityDefinitionPriority ADPAsap = "asap"
toActivityDefinitionPriority ADPStat = "stat"
fromActivityDefinitionPriority "routine" = ADPRoutine
fromActivityDefinitionPriority "urgent" = ADPUrgent
fromActivityDefinitionPriority "asap" = ADPAsap
fromActivityDefinitionPriority "stat" = ADPStat


data ActivityDefinitionSubject
    = ActivityDefinitionSubjectCodeableConcept CodeableConcept
    | ActivityDefinitionSubjectReference Reference
    deriving (Eq, Show)

data ActivityDefinitionTiming
    = ActivityDefinitionTimingTiming Timing
    | ActivityDefinitionTimingDateTime DateTime
    | ActivityDefinitionTimingAge Age
    | ActivityDefinitionTimingPeriod Period
    | ActivityDefinitionTimingRange Range
    | ActivityDefinitionTimingDuration Duration
    deriving (Eq, Show)

data ActivityDefinitionProduct
    = ActivityDefinitionProductReference Reference
    | ActivityDefinitionProductCodeableConcept CodeableConcept
    deriving (Eq, Show)

data ActivityDefinition = ActivityDefinition {
    activityDefinitionId :: Maybe Id
  , activityDefinitionMeta :: Maybe Meta
  , activityDefinitionImplicitRules :: Maybe Uri
  , activityDefinitionLanguage :: Maybe Language
  , activityDefinitionText :: Maybe Narrative
--    activityDefinitionContained :: [ResourceContainer]
  , activityDefinitionExtension :: [Extension]
  , activityDefinitionModifierExtension :: [Extension]
  , activityDefinitionUrl :: Maybe Uri
  , activityDefinitionIdentifier :: [Identifier]
  , activityDefinitionVersion :: Maybe Text
  , activityDefinitionName :: Maybe Text
  , activityDefinitionTitle :: Maybe Text
  , activityDefinitionSubtitle :: Maybe Text
  , activityDefinitionStatus :: PublicationStatus
  , activityDefinitionExperimental :: Maybe Boolean
  , activityDefinitionSubject :: Maybe ActivityDefinitionSubject
  , activityDefinitionDate :: Maybe DateTime
  , activityDefinitionPublisher :: Maybe Text
  , activityDefinitionContact :: [ContactDetail]
  , activityDefinitionDescription :: Maybe Markdown
  , activityDefinitionUseContext :: [UsageContext]
  , activityDefinitionJurisdiction :: [CodeableConcept]
  , activityDefinitionPurpose :: Maybe Markdown
  , activityDefinitionUsage :: Maybe Text
  , activityDefinitionCopyright :: Maybe Markdown
  , activityDefinitionApprovalDate :: Maybe Date
  , activityDefinitionLastReviewDate :: Maybe Date
  , activityDefinitionEffectivePeriod :: Maybe Period
  , activityDefinitionTopic :: [CodeableConcept]
  , activityDefinitionAuthor :: [ContactDetail]
  , activityDefinitionEditor :: [ContactDetail]
  , activityDefinitionReviewer :: [ContactDetail]
  , activityDefinitionEndorser :: [ContactDetail]
  , activityDefinitionRelatedArtifact :: [RelatedArtifact]
  , activityDefinitionLibrary :: [Canonical]
  , activityDefinitionKind :: Maybe ActivityDefinitionKind
  , activityDefinitionProfile :: Maybe Canonical
  , activityDefinitionCode :: Maybe CodeableConcept
  , activityDefinitionIntent :: Maybe ActivityDefinitionIntent
  , activityDefinitionPriority :: Maybe ActivityDefinitionPriority
  , activityDefinitionDoNotPerform :: Maybe Boolean
  , activityDefinitionTiming :: Maybe ActivityDefinitionTiming
  , activityDefinitionLocation :: Maybe Reference
  , activityDefinitionParticipant :: [ActivityDefinitionParticipant]
  , activityDefinitionProduct :: Maybe ActivityDefinitionProduct
  , activityDefinitionQuantity :: Maybe Quantity
  , activityDefinitionDosage :: [Dosage]
  , activityDefinitionBodySite :: [CodeableConcept]
  , activityDefinitionSpecimenRequirement :: [Reference]
  , activityDefinitionObservationRequirement :: [Reference]
  , activityDefinitionObservationResultRequirement :: [Reference]
  , activityDefinitionTransform :: Maybe Canonical
  , activityDefinitionDynamicValue :: [ActivityDefinitionDynamicValue]
  } deriving (Eq, Show)
--

instance ToJSON ActivityDefinition where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "ActivityDefinition")
    ,  "id" .= toJSON (activityDefinitionId p)
    ,  "meta" .= toJSON (activityDefinitionMeta p)
    ,  "implicitRules" .= toJSON (activityDefinitionImplicitRules p)
    ,  "language" .= toJSON (activityDefinitionLanguage p)
    ,  "text" .= toJSON (activityDefinitionText p)
--    , "contained" .= toJSON (activityDefinitionContained p)
    ,  "extension" .= toJSON (activityDefinitionExtension p)
    ,  "modifierExtension" .= toJSON (activityDefinitionModifierExtension p)
    ,  "url" .= toJSON (activityDefinitionUrl p)
    ,  "identifier" .= toJSON (activityDefinitionIdentifier p)
    ,  "version" .= toJSON (activityDefinitionVersion p)
    ,  "name" .= toJSON (activityDefinitionName p)
    ,  "title" .= toJSON (activityDefinitionTitle p)
    ,  "subtitle" .= toJSON (activityDefinitionSubtitle p)
    ,  "status" .= toJSON (activityDefinitionStatus p)
    ,  "experimental" .= toJSON (activityDefinitionExperimental p)
    , toSubjectJSON (activityDefinitionSubject p)
    ,  "date" .= toJSON (activityDefinitionDate p)
    ,  "publisher" .= toJSON (activityDefinitionPublisher p)
    ,  "contact" .= toJSON (activityDefinitionContact p)
    ,  "description" .= toJSON (activityDefinitionDescription p)
    ,  "useContext" .= toJSON (activityDefinitionUseContext p)
    ,  "jurisdiction" .= toJSON (activityDefinitionJurisdiction p)
    ,  "purpose" .= toJSON (activityDefinitionPurpose p)
    ,  "usage" .= toJSON (activityDefinitionUsage p)
    ,  "copyright" .= toJSON (activityDefinitionCopyright p)
    ,  "approvalDate" .= toJSON (activityDefinitionApprovalDate p)
    ,  "lastReviewDate" .= toJSON (activityDefinitionLastReviewDate p)
    ,  "effectivePeriod" .= toJSON (activityDefinitionEffectivePeriod p)
    ,  "topic" .= toJSON (activityDefinitionTopic p)
    ,  "author" .= toJSON (activityDefinitionAuthor p)
    ,  "editor" .= toJSON (activityDefinitionEditor p)
    ,  "reviewer" .= toJSON (activityDefinitionReviewer p)
    ,  "endorser" .= toJSON (activityDefinitionEndorser p)
    ,  "relatedArtifact" .= toJSON (activityDefinitionRelatedArtifact p)
    ,  "library" .= toJSON (activityDefinitionLibrary p)
    ,  "kind" .= toJSON (activityDefinitionKind p)
    ,  "profile" .= toJSON (activityDefinitionProfile p)
    ,  "code" .= toJSON (activityDefinitionCode p)
    ,  "intent" .= toJSON (activityDefinitionIntent p)
    ,  "priority" .= toJSON (activityDefinitionPriority p)
    ,  "doNotPerform" .= toJSON (activityDefinitionDoNotPerform p)
    , toTimingJSON (activityDefinitionTiming p)
    ,  "location" .= toJSON (activityDefinitionLocation p)
    ,  "participant" .= toJSON (activityDefinitionParticipant p)
    , toProductJSON (activityDefinitionProduct p)
    ,  "quantity" .= toJSON (activityDefinitionQuantity p)
    ,  "dosage" .= toJSON (activityDefinitionDosage p)
    ,  "bodySite" .= toJSON (activityDefinitionBodySite p)
    ,  "specimenRequirement" .= toJSON (activityDefinitionSpecimenRequirement p)
    ,  "observationRequirement" .= toJSON (activityDefinitionObservationRequirement p)
    ,  "observationResultRequirement" .= toJSON (activityDefinitionObservationResultRequirement p)
    ,  "transform" .= toJSON (activityDefinitionTransform p)
    ,  "dynamicValue" .= toJSON (activityDefinitionDynamicValue p)
    ]
    where 
      toSubjectJSON (     Nothing   ) = ("subject", Null)
      toSubjectJSON (Just (ActivityDefinitionSubjectCodeableConcept c)) = ("subjectCodeableConcept", toJSON c)
      toSubjectJSON (Just (ActivityDefinitionSubjectReference c)) = ("subjectReference", toJSON c)
      toTimingJSON (     Nothing   ) = ("timing", Null)
      toTimingJSON (Just (ActivityDefinitionTimingTiming c)) = ("timingTiming", toJSON c)
      toTimingJSON (Just (ActivityDefinitionTimingDateTime c)) = ("timingDateTime", toJSON c)
      toTimingJSON (Just (ActivityDefinitionTimingAge c)) = ("timingAge", toJSON c)
      toTimingJSON (Just (ActivityDefinitionTimingPeriod c)) = ("timingPeriod", toJSON c)
      toTimingJSON (Just (ActivityDefinitionTimingRange c)) = ("timingRange", toJSON c)
      toTimingJSON (Just (ActivityDefinitionTimingDuration c)) = ("timingDuration", toJSON c)
      toProductJSON (     Nothing   ) = ("product", Null)
      toProductJSON (Just (ActivityDefinitionProductReference c)) = ("productReference", toJSON c)
      toProductJSON (Just (ActivityDefinitionProductCodeableConcept c)) = ("productCodeableConcept", toJSON c)
instance FromJSON ActivityDefinition where
  parseJSON = withObject "ActivityDefinition" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "ActivityDefinition" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        url <- o .:? "url"
        identifier <- o .:? "identifier" .!= []
        version <- o .:? "version"
        name <- o .:? "name"
        title <- o .:? "title"
        subtitle <- o .:? "subtitle"
        status <- o .:  "status"
        experimental <- o .:? "experimental"
        subject <- parseSubject o
        date <- o .:? "date"
        publisher <- o .:? "publisher"
        contact <- o .:? "contact" .!= []
        description <- o .:? "description"
        useContext <- o .:? "useContext" .!= []
        jurisdiction <- o .:? "jurisdiction" .!= []
        purpose <- o .:? "purpose"
        usage <- o .:? "usage"
        copyright <- o .:? "copyright"
        approvalDate <- o .:? "approvalDate"
        lastReviewDate <- o .:? "lastReviewDate"
        effectivePeriod <- o .:? "effectivePeriod"
        topic <- o .:? "topic" .!= []
        author <- o .:? "author" .!= []
        editor <- o .:? "editor" .!= []
        reviewer <- o .:? "reviewer" .!= []
        endorser <- o .:? "endorser" .!= []
        relatedArtifact <- o .:? "relatedArtifact" .!= []
        library <- o .:? "library" .!= []
        kind <- o .:? "kind"
        profile <- o .:? "profile"
        code <- o .:? "code"
        intent <- o .:? "intent"
        priority <- o .:? "priority"
        doNotPerform <- o .:? "doNotPerform"
        timing <- parseTiming o
        location <- o .:? "location"
        participant <- o .:? "participant" .!= []
        product <- parseProduct o
        quantity <- o .:? "quantity"
        dosage <- o .:? "dosage" .!= []
        bodySite <- o .:? "bodySite" .!= []
        specimenRequirement <- o .:? "specimenRequirement" .!= []
        observationRequirement <- o .:? "observationRequirement" .!= []
        observationResultRequirement <- o .:? "observationResultRequirement" .!= []
        transform <- o .:? "transform"
        dynamicValue <- o .:? "dynamicValue" .!= []
        return ActivityDefinition{
            activityDefinitionId = id
          , activityDefinitionMeta = meta
          , activityDefinitionImplicitRules = implicitRules
          , activityDefinitionLanguage = language
          , activityDefinitionText = text
--          , activityDefinitionContained = contained
          , activityDefinitionExtension = extension
          , activityDefinitionModifierExtension = modifierExtension
          , activityDefinitionUrl = url
          , activityDefinitionIdentifier = identifier
          , activityDefinitionVersion = version
          , activityDefinitionName = name
          , activityDefinitionTitle = title
          , activityDefinitionSubtitle = subtitle
          , activityDefinitionStatus = status
          , activityDefinitionExperimental = experimental
          , activityDefinitionSubject = subject
          , activityDefinitionDate = date
          , activityDefinitionPublisher = publisher
          , activityDefinitionContact = contact
          , activityDefinitionDescription = description
          , activityDefinitionUseContext = useContext
          , activityDefinitionJurisdiction = jurisdiction
          , activityDefinitionPurpose = purpose
          , activityDefinitionUsage = usage
          , activityDefinitionCopyright = copyright
          , activityDefinitionApprovalDate = approvalDate
          , activityDefinitionLastReviewDate = lastReviewDate
          , activityDefinitionEffectivePeriod = effectivePeriod
          , activityDefinitionTopic = topic
          , activityDefinitionAuthor = author
          , activityDefinitionEditor = editor
          , activityDefinitionReviewer = reviewer
          , activityDefinitionEndorser = endorser
          , activityDefinitionRelatedArtifact = relatedArtifact
          , activityDefinitionLibrary = library
          , activityDefinitionKind = kind
          , activityDefinitionProfile = profile
          , activityDefinitionCode = code
          , activityDefinitionIntent = intent
          , activityDefinitionPriority = priority
          , activityDefinitionDoNotPerform = doNotPerform
          , activityDefinitionTiming = timing
          , activityDefinitionLocation = location
          , activityDefinitionParticipant = participant
          , activityDefinitionProduct = product
          , activityDefinitionQuantity = quantity
          , activityDefinitionDosage = dosage
          , activityDefinitionBodySite = bodySite
          , activityDefinitionSpecimenRequirement = specimenRequirement
          , activityDefinitionObservationRequirement = observationRequirement
          , activityDefinitionObservationResultRequirement = observationResultRequirement
          , activityDefinitionTransform = transform
          , activityDefinitionDynamicValue = dynamicValue
          }
      _ -> fail "not a ActivityDefinition"
    where 
      parseSubject o = parseSubjectCodeableConcept o <|> parseSubjectReference o
      parseSubjectCodeableConcept o = do
                has <- o .: "subjectCodeableConcept"
                return $ Just (ActivityDefinitionSubjectCodeableConcept has)
      parseSubjectReference o = do
                has <- o .: "subjectReference"
                return $ Just (ActivityDefinitionSubjectReference has)
      parseTiming o = parseTimingTiming o <|> parseTimingDateTime o <|> parseTimingAge o <|> parseTimingPeriod o <|> parseTimingRange o <|> parseTimingDuration o
      parseTimingTiming o = do
                has <- o .: "timingTiming"
                return $ Just (ActivityDefinitionTimingTiming has)
      parseTimingDateTime o = do
                has <- o .: "timingDateTime"
                return $ Just (ActivityDefinitionTimingDateTime has)
      parseTimingAge o = do
                has <- o .: "timingAge"
                return $ Just (ActivityDefinitionTimingAge has)
      parseTimingPeriod o = do
                has <- o .: "timingPeriod"
                return $ Just (ActivityDefinitionTimingPeriod has)
      parseTimingRange o = do
                has <- o .: "timingRange"
                return $ Just (ActivityDefinitionTimingRange has)
      parseTimingDuration o = do
                has <- o .: "timingDuration"
                return $ Just (ActivityDefinitionTimingDuration has)
      parseProduct o = parseProductReference o <|> parseProductCodeableConcept o
      parseProductReference o = do
                has <- o .: "productReference"
                return $ Just (ActivityDefinitionProductReference has)
      parseProductCodeableConcept o = do
                has <- o .: "productCodeableConcept"
                return $ Just (ActivityDefinitionProductCodeableConcept has)
instance Xmlbf.ToXml ActivityDefinition where
  toXml p = Xmlbf.element "ActivityDefinition" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (activityDefinitionId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (activityDefinitionMeta p))
             , OptVal   "implicitRules" (fmap toUri (activityDefinitionImplicitRules p))
             , OptVal   "language" (fmap toLanguage (activityDefinitionLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (activityDefinitionText p))
--             , PropList "contained" (fmap Xmlbf.toXml (activityDefinitionContained p))
             , PropList "extension" (fmap Xmlbf.toXml (activityDefinitionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (activityDefinitionModifierExtension p))
             , OptVal   "url" (fmap toUri (activityDefinitionUrl p))
             , PropList "identifier" (fmap Xmlbf.toXml (activityDefinitionIdentifier p))
             , OptVal   "version" (fmap toString (activityDefinitionVersion p))
             , OptVal   "name" (fmap toString (activityDefinitionName p))
             , OptVal   "title" (fmap toString (activityDefinitionTitle p))
             , OptVal   "subtitle" (fmap toString (activityDefinitionSubtitle p))
             , Val      "status" (     toPublicationStatus (activityDefinitionStatus p))
             , OptVal   "experimental" (fmap toBoolean (activityDefinitionExperimental p))
             , toSubjectXml (activityDefinitionSubject p)
             , OptVal   "date" (fmap toDateTime (activityDefinitionDate p))
             , OptVal   "publisher" (fmap toString (activityDefinitionPublisher p))
             , PropList "contact" (fmap Xmlbf.toXml (activityDefinitionContact p))
             , OptVal   "description" (fmap toMarkdown (activityDefinitionDescription p))
             , PropList "useContext" (fmap Xmlbf.toXml (activityDefinitionUseContext p))
             , PropList "jurisdiction" (fmap Xmlbf.toXml (activityDefinitionJurisdiction p))
             , OptVal   "purpose" (fmap toMarkdown (activityDefinitionPurpose p))
             , OptVal   "usage" (fmap toString (activityDefinitionUsage p))
             , OptVal   "copyright" (fmap toMarkdown (activityDefinitionCopyright p))
             , OptVal   "approvalDate" (fmap toDate (activityDefinitionApprovalDate p))
             , OptVal   "lastReviewDate" (fmap toDate (activityDefinitionLastReviewDate p))
             , OptProp  "effectivePeriod" (fmap Xmlbf.toXml (activityDefinitionEffectivePeriod p))
             , PropList "topic" (fmap Xmlbf.toXml (activityDefinitionTopic p))
             , PropList "author" (fmap Xmlbf.toXml (activityDefinitionAuthor p))
             , PropList "editor" (fmap Xmlbf.toXml (activityDefinitionEditor p))
             , PropList "reviewer" (fmap Xmlbf.toXml (activityDefinitionReviewer p))
             , PropList "endorser" (fmap Xmlbf.toXml (activityDefinitionEndorser p))
             , PropList "relatedArtifact" (fmap Xmlbf.toXml (activityDefinitionRelatedArtifact p))
             , ValList  "library" (fmap toCanonical (activityDefinitionLibrary p))
             , OptVal   "kind" (fmap toActivityDefinitionKind (activityDefinitionKind p))
             , OptVal   "profile" (fmap toCanonical (activityDefinitionProfile p))
             , OptProp  "code" (fmap Xmlbf.toXml (activityDefinitionCode p))
             , OptVal   "intent" (fmap toActivityDefinitionIntent (activityDefinitionIntent p))
             , OptVal   "priority" (fmap toActivityDefinitionPriority (activityDefinitionPriority p))
             , OptVal   "doNotPerform" (fmap toBoolean (activityDefinitionDoNotPerform p))
             , toTimingXml (activityDefinitionTiming p)
             , OptProp  "location" (fmap Xmlbf.toXml (activityDefinitionLocation p))
             , PropList "participant" (fmap Xmlbf.toXml (activityDefinitionParticipant p))
             , toProductXml (activityDefinitionProduct p)
             , OptProp  "quantity" (fmap Xmlbf.toXml (activityDefinitionQuantity p))
             , PropList "dosage" (fmap Xmlbf.toXml (activityDefinitionDosage p))
             , PropList "bodySite" (fmap Xmlbf.toXml (activityDefinitionBodySite p))
             , PropList "specimenRequirement" (fmap Xmlbf.toXml (activityDefinitionSpecimenRequirement p))
             , PropList "observationRequirement" (fmap Xmlbf.toXml (activityDefinitionObservationRequirement p))
             , PropList "observationResultRequirement" (fmap Xmlbf.toXml (activityDefinitionObservationResultRequirement p))
             , OptVal   "transform" (fmap toCanonical (activityDefinitionTransform p))
             , PropList "dynamicValue" (fmap Xmlbf.toXml (activityDefinitionDynamicValue p))
             ]
          toSubjectXml ( Nothing   ) = (OptVal "subject" Nothing)
          toSubjectXml (Just (ActivityDefinitionSubjectCodeableConcept p)) = Prop  "subjectCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toSubjectXml (Just (ActivityDefinitionSubjectReference p)) = Prop  "subjectReference" (HM.empty, Xmlbf.toXml p)
          toTimingXml ( Nothing   ) = (OptVal "timing" Nothing)
          toTimingXml (Just (ActivityDefinitionTimingTiming p)) = Prop  "timingTiming" (HM.empty, Xmlbf.toXml p)
          toTimingXml (Just (ActivityDefinitionTimingDateTime p)) = Val   "timingDateTime" (toDateTime p)
          toTimingXml (Just (ActivityDefinitionTimingAge p)) = Prop  "timingAge" (HM.empty, Xmlbf.toXml p)
          toTimingXml (Just (ActivityDefinitionTimingPeriod p)) = Prop  "timingPeriod" (HM.empty, Xmlbf.toXml p)
          toTimingXml (Just (ActivityDefinitionTimingRange p)) = Prop  "timingRange" (HM.empty, Xmlbf.toXml p)
          toTimingXml (Just (ActivityDefinitionTimingDuration p)) = Prop  "timingDuration" (HM.empty, Xmlbf.toXml p)
          toProductXml ( Nothing   ) = (OptVal "product" Nothing)
          toProductXml (Just (ActivityDefinitionProductReference p)) = Prop  "productReference" (HM.empty, Xmlbf.toXml p)
          toProductXml (Just (ActivityDefinitionProductCodeableConcept p)) = Prop  "productCodeableConcept" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml ActivityDefinition where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" Xmlbf.fromXml
--    contained <- many     $ Xmlbf.pElement "contained" Xmlbf.fromXml
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    url <- optional $ Xmlbf.pElement "url" (Xmlbf.pAttr "value")
    identifier <- many     $ Xmlbf.pElement "identifier" Xmlbf.fromXml
    version <- optional $ Xmlbf.pElement "version" (Xmlbf.pAttr "value")
    name <- optional $ Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    title <- optional $ Xmlbf.pElement "title" (Xmlbf.pAttr "value")
    subtitle <- optional $ Xmlbf.pElement "subtitle" (Xmlbf.pAttr "value")
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    experimental <- optional $ Xmlbf.pElement "experimental" (Xmlbf.pAttr "value")
    subject <- fromSubjectXml
    date <- optional $ Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    publisher <- optional $ Xmlbf.pElement "publisher" (Xmlbf.pAttr "value")
    contact <- many     $ Xmlbf.pElement "contact" Xmlbf.fromXml
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    useContext <- many     $ Xmlbf.pElement "useContext" Xmlbf.fromXml
    jurisdiction <- many     $ Xmlbf.pElement "jurisdiction" Xmlbf.fromXml
    purpose <- optional $ Xmlbf.pElement "purpose" (Xmlbf.pAttr "value")
    usage <- optional $ Xmlbf.pElement "usage" (Xmlbf.pAttr "value")
    copyright <- optional $ Xmlbf.pElement "copyright" (Xmlbf.pAttr "value")
    approvalDate <- optional $ Xmlbf.pElement "approvalDate" (Xmlbf.pAttr "value")
    lastReviewDate <- optional $ Xmlbf.pElement "lastReviewDate" (Xmlbf.pAttr "value")
    effectivePeriod <- optional $ Xmlbf.pElement "effectivePeriod" Xmlbf.fromXml
    topic <- many     $ Xmlbf.pElement "topic" Xmlbf.fromXml
    author <- many     $ Xmlbf.pElement "author" Xmlbf.fromXml
    editor <- many     $ Xmlbf.pElement "editor" Xmlbf.fromXml
    reviewer <- many     $ Xmlbf.pElement "reviewer" Xmlbf.fromXml
    endorser <- many     $ Xmlbf.pElement "endorser" Xmlbf.fromXml
    relatedArtifact <- many     $ Xmlbf.pElement "relatedArtifact" Xmlbf.fromXml
    library <- many     $ Xmlbf.pElement "library" (Xmlbf.pAttr "value")
    kind <- optional $ Xmlbf.pElement "kind" (Xmlbf.pAttr "value")
    profile <- optional $ Xmlbf.pElement "profile" (Xmlbf.pAttr "value")
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    intent <- optional $ Xmlbf.pElement "intent" (Xmlbf.pAttr "value")
    priority <- optional $ Xmlbf.pElement "priority" (Xmlbf.pAttr "value")
    doNotPerform <- optional $ Xmlbf.pElement "doNotPerform" (Xmlbf.pAttr "value")
    timing <- fromTimingXml
    location <- optional $ Xmlbf.pElement "location" Xmlbf.fromXml
    participant <- many     $ Xmlbf.pElement "participant" Xmlbf.fromXml
    product <- fromProductXml
    quantity <- optional $ Xmlbf.pElement "quantity" Xmlbf.fromXml
    dosage <- many     $ Xmlbf.pElement "dosage" Xmlbf.fromXml
    bodySite <- many     $ Xmlbf.pElement "bodySite" Xmlbf.fromXml
    specimenRequirement <- many     $ Xmlbf.pElement "specimenRequirement" Xmlbf.fromXml
    observationRequirement <- many     $ Xmlbf.pElement "observationRequirement" Xmlbf.fromXml
    observationResultRequirement <- many     $ Xmlbf.pElement "observationResultRequirement" Xmlbf.fromXml
    transform <- optional $ Xmlbf.pElement "transform" (Xmlbf.pAttr "value")
    dynamicValue <- many     $ Xmlbf.pElement "dynamicValue" Xmlbf.fromXml
    return ActivityDefinition {
            activityDefinitionId = fmap fromId id
          , activityDefinitionMeta = meta
          , activityDefinitionImplicitRules = fmap fromUri implicitRules
          , activityDefinitionLanguage = fmap fromLanguage language
          , activityDefinitionText = text
--          , activityDefinitionContained = contained
          , activityDefinitionExtension = extension
          , activityDefinitionModifierExtension = modifierExtension
          , activityDefinitionUrl = fmap fromUri url
          , activityDefinitionIdentifier = identifier
          , activityDefinitionVersion = fmap fromString version
          , activityDefinitionName = fmap fromString name
          , activityDefinitionTitle = fmap fromString title
          , activityDefinitionSubtitle = fmap fromString subtitle
          , activityDefinitionStatus =      fromPublicationStatus status
          , activityDefinitionExperimental = fmap fromBoolean experimental
          , activityDefinitionSubject = subject
          , activityDefinitionDate = fmap fromDateTime date
          , activityDefinitionPublisher = fmap fromString publisher
          , activityDefinitionContact = contact
          , activityDefinitionDescription = fmap fromMarkdown description
          , activityDefinitionUseContext = useContext
          , activityDefinitionJurisdiction = jurisdiction
          , activityDefinitionPurpose = fmap fromMarkdown purpose
          , activityDefinitionUsage = fmap fromString usage
          , activityDefinitionCopyright = fmap fromMarkdown copyright
          , activityDefinitionApprovalDate = fmap fromDate approvalDate
          , activityDefinitionLastReviewDate = fmap fromDate lastReviewDate
          , activityDefinitionEffectivePeriod = effectivePeriod
          , activityDefinitionTopic = topic
          , activityDefinitionAuthor = author
          , activityDefinitionEditor = editor
          , activityDefinitionReviewer = reviewer
          , activityDefinitionEndorser = endorser
          , activityDefinitionRelatedArtifact = relatedArtifact
          , activityDefinitionLibrary = fmap fromCanonical library
          , activityDefinitionKind = fmap fromActivityDefinitionKind kind
          , activityDefinitionProfile = fmap fromCanonical profile
          , activityDefinitionCode = code
          , activityDefinitionIntent = fmap fromActivityDefinitionIntent intent
          , activityDefinitionPriority = fmap fromActivityDefinitionPriority priority
          , activityDefinitionDoNotPerform = fmap fromBoolean doNotPerform
          , activityDefinitionTiming = timing
          , activityDefinitionLocation = location
          , activityDefinitionParticipant = participant
          , activityDefinitionProduct = product
          , activityDefinitionQuantity = quantity
          , activityDefinitionDosage = dosage
          , activityDefinitionBodySite = bodySite
          , activityDefinitionSpecimenRequirement = specimenRequirement
          , activityDefinitionObservationRequirement = observationRequirement
          , activityDefinitionObservationResultRequirement = observationResultRequirement
          , activityDefinitionTransform = fmap fromCanonical transform
          , activityDefinitionDynamicValue = dynamicValue
          }

    where 
      fromSubjectXml = parseSubjectCodeableConcept <|> parseSubjectReference <|> pure Nothing
      parseSubjectCodeableConcept = do
                has <- Xmlbf.pElement "subjectCodeableConcept" Xmlbf.fromXml
                return $ Just (ActivityDefinitionSubjectCodeableConcept (                      has))
      parseSubjectReference = do
                has <- Xmlbf.pElement "subjectReference" Xmlbf.fromXml
                return $ Just (ActivityDefinitionSubjectReference (                      has))
      fromTimingXml = parseTimingTiming <|> parseTimingDateTime <|> parseTimingAge <|> parseTimingPeriod <|> parseTimingRange <|> parseTimingDuration <|> pure Nothing
      parseTimingTiming = do
                has <- Xmlbf.pElement "timingTiming" Xmlbf.fromXml
                return $ Just (ActivityDefinitionTimingTiming (                      has))
      parseTimingDateTime = do
                has <- Xmlbf.pElement "timingDateTime" (Xmlbf.pAttr "value")
                return $ Just (ActivityDefinitionTimingDateTime (     fromDateTime has))
      parseTimingAge = do
                has <- Xmlbf.pElement "timingAge" Xmlbf.fromXml
                return $ Just (ActivityDefinitionTimingAge (                      has))
      parseTimingPeriod = do
                has <- Xmlbf.pElement "timingPeriod" Xmlbf.fromXml
                return $ Just (ActivityDefinitionTimingPeriod (                      has))
      parseTimingRange = do
                has <- Xmlbf.pElement "timingRange" Xmlbf.fromXml
                return $ Just (ActivityDefinitionTimingRange (                      has))
      parseTimingDuration = do
                has <- Xmlbf.pElement "timingDuration" Xmlbf.fromXml
                return $ Just (ActivityDefinitionTimingDuration (                      has))
      fromProductXml = parseProductReference <|> parseProductCodeableConcept <|> pure Nothing
      parseProductReference = do
                has <- Xmlbf.pElement "productReference" Xmlbf.fromXml
                return $ Just (ActivityDefinitionProductReference (                      has))
      parseProductCodeableConcept = do
                has <- Xmlbf.pElement "productCodeableConcept" Xmlbf.fromXml
                return $ Just (ActivityDefinitionProductCodeableConcept (                      has))


data ActivityDefinitionDynamicValue = ActivityDefinitionDynamicValue {
    activityDefinitionDynamicValueAttrId :: Maybe Text
  , activityDefinitionDynamicValueExtension :: [Extension]
  , activityDefinitionDynamicValueModifierExtension :: [Extension]
  , activityDefinitionDynamicValuePath :: Text
  , activityDefinitionDynamicValueExpression :: Expression
  } deriving (Eq, Show)
--

instance ToJSON ActivityDefinitionDynamicValue where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (activityDefinitionDynamicValueAttrId p)
    ,  "extension" .= toJSON (activityDefinitionDynamicValueExtension p)
    ,  "modifierExtension" .= toJSON (activityDefinitionDynamicValueModifierExtension p)
    ,  "path" .= toJSON (activityDefinitionDynamicValuePath p)
    ,  "expression" .= toJSON (activityDefinitionDynamicValueExpression p)
    ]
instance FromJSON ActivityDefinitionDynamicValue where
  parseJSON = withObject "ActivityDefinitionDynamicValue" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        path <- o .:  "path"
        expression <- o .:  "expression"
        return ActivityDefinitionDynamicValue{
            activityDefinitionDynamicValueAttrId = id
          , activityDefinitionDynamicValueExtension = extension
          , activityDefinitionDynamicValueModifierExtension = modifierExtension
          , activityDefinitionDynamicValuePath = path
          , activityDefinitionDynamicValueExpression = expression
          }
instance Xmlbf.ToXml ActivityDefinitionDynamicValue where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (activityDefinitionDynamicValueAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (activityDefinitionDynamicValueExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (activityDefinitionDynamicValueModifierExtension p))
             , Val      "path" (     toString (activityDefinitionDynamicValuePath p))
             , Prop     "expression" (HM.empty, Xmlbf.toXml (activityDefinitionDynamicValueExpression p))
             ]
instance Xmlbf.FromXml ActivityDefinitionDynamicValue where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    path <-            Xmlbf.pElement "path" (Xmlbf.pAttr "value")
    expression <-            Xmlbf.pElement "expression" Xmlbf.fromXml
    return ActivityDefinitionDynamicValue {
            activityDefinitionDynamicValueAttrId = id
          , activityDefinitionDynamicValueExtension = extension
          , activityDefinitionDynamicValueModifierExtension = modifierExtension
          , activityDefinitionDynamicValuePath =      fromString path
          , activityDefinitionDynamicValueExpression = expression
          }



data ActivityDefinitionParticipantType
    = ADPTPatient
    | ADPTPractitioner
    | ADPTRelatedPerson
    | ADPTDevice
  deriving (Eq, Show)

instance ToJSON ActivityDefinitionParticipantType where
    toJSON ADPTPatient = String "patient"
    toJSON ADPTPractitioner = String "practitioner"
    toJSON ADPTRelatedPerson = String "related-person"
    toJSON ADPTDevice = String "device"
instance FromJSON ActivityDefinitionParticipantType where
    parseJSON "patient" = return ADPTPatient
    parseJSON "practitioner" = return ADPTPractitioner
    parseJSON "related-person" = return ADPTRelatedPerson
    parseJSON "device" = return ADPTDevice

toActivityDefinitionParticipantType ADPTPatient = "patient"
toActivityDefinitionParticipantType ADPTPractitioner = "practitioner"
toActivityDefinitionParticipantType ADPTRelatedPerson = "related-person"
toActivityDefinitionParticipantType ADPTDevice = "device"
fromActivityDefinitionParticipantType "patient" = ADPTPatient
fromActivityDefinitionParticipantType "practitioner" = ADPTPractitioner
fromActivityDefinitionParticipantType "related-person" = ADPTRelatedPerson
fromActivityDefinitionParticipantType "device" = ADPTDevice


data ActivityDefinitionParticipant = ActivityDefinitionParticipant {
    activityDefinitionParticipantAttrId :: Maybe Text
  , activityDefinitionParticipantExtension :: [Extension]
  , activityDefinitionParticipantModifierExtension :: [Extension]
  , activityDefinitionParticipantType :: ActivityDefinitionParticipantType
  , activityDefinitionParticipantRole :: Maybe CodeableConcept
  } deriving (Eq, Show)
--

instance ToJSON ActivityDefinitionParticipant where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (activityDefinitionParticipantAttrId p)
    ,  "extension" .= toJSON (activityDefinitionParticipantExtension p)
    ,  "modifierExtension" .= toJSON (activityDefinitionParticipantModifierExtension p)
    ,  "type" .= toJSON (activityDefinitionParticipantType p)
    ,  "role" .= toJSON (activityDefinitionParticipantRole p)
    ]
instance FromJSON ActivityDefinitionParticipant where
  parseJSON = withObject "ActivityDefinitionParticipant" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:  "type"
        role <- o .:? "role"
        return ActivityDefinitionParticipant{
            activityDefinitionParticipantAttrId = id
          , activityDefinitionParticipantExtension = extension
          , activityDefinitionParticipantModifierExtension = modifierExtension
          , activityDefinitionParticipantType = ty
          , activityDefinitionParticipantRole = role
          }
instance Xmlbf.ToXml ActivityDefinitionParticipant where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (activityDefinitionParticipantAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (activityDefinitionParticipantExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (activityDefinitionParticipantModifierExtension p))
             , Val      "type" (     toActivityDefinitionParticipantType (activityDefinitionParticipantType p))
             , OptProp  "role" (fmap Xmlbf.toXml (activityDefinitionParticipantRole p))
             ]
instance Xmlbf.FromXml ActivityDefinitionParticipant where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    role <- optional $ Xmlbf.pElement "role" Xmlbf.fromXml
    return ActivityDefinitionParticipant {
            activityDefinitionParticipantAttrId = id
          , activityDefinitionParticipantExtension = extension
          , activityDefinitionParticipantModifierExtension = modifierExtension
          , activityDefinitionParticipantType =      fromActivityDefinitionParticipantType ty
          , activityDefinitionParticipantRole = role
          }




