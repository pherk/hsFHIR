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
-- FHIR 4.0.0 Task
--

module Data.FHIR.Resources.Task where

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

data TaskStatus
    = TSDraft
    | TSRequested
    | TSReceived
    | TSAccepted
    | TSRejected
    | TSReady
    | TSCancelled
    | TSInProgress
    | TSOnHold
    | TSFailed
    | TSCompleted
    | TSEnteredInError
  deriving (Eq, Show)

instance ToJSON TaskStatus where
    toJSON TSDraft = String "draft"
    toJSON TSRequested = String "requested"
    toJSON TSReceived = String "received"
    toJSON TSAccepted = String "accepted"
    toJSON TSRejected = String "rejected"
    toJSON TSReady = String "ready"
    toJSON TSCancelled = String "cancelled"
    toJSON TSInProgress = String "in-progress"
    toJSON TSOnHold = String "on-hold"
    toJSON TSFailed = String "failed"
    toJSON TSCompleted = String "completed"
    toJSON TSEnteredInError = String "entered-in-error"
instance FromJSON TaskStatus where
    parseJSON "draft" = return TSDraft
    parseJSON "requested" = return TSRequested
    parseJSON "received" = return TSReceived
    parseJSON "accepted" = return TSAccepted
    parseJSON "rejected" = return TSRejected
    parseJSON "ready" = return TSReady
    parseJSON "cancelled" = return TSCancelled
    parseJSON "in-progress" = return TSInProgress
    parseJSON "on-hold" = return TSOnHold
    parseJSON "failed" = return TSFailed
    parseJSON "completed" = return TSCompleted
    parseJSON "entered-in-error" = return TSEnteredInError

toTaskStatus TSDraft = "draft"
toTaskStatus TSRequested = "requested"
toTaskStatus TSReceived = "received"
toTaskStatus TSAccepted = "accepted"
toTaskStatus TSRejected = "rejected"
toTaskStatus TSReady = "ready"
toTaskStatus TSCancelled = "cancelled"
toTaskStatus TSInProgress = "in-progress"
toTaskStatus TSOnHold = "on-hold"
toTaskStatus TSFailed = "failed"
toTaskStatus TSCompleted = "completed"
toTaskStatus TSEnteredInError = "entered-in-error"
fromTaskStatus "draft" = TSDraft
fromTaskStatus "requested" = TSRequested
fromTaskStatus "received" = TSReceived
fromTaskStatus "accepted" = TSAccepted
fromTaskStatus "rejected" = TSRejected
fromTaskStatus "ready" = TSReady
fromTaskStatus "cancelled" = TSCancelled
fromTaskStatus "in-progress" = TSInProgress
fromTaskStatus "on-hold" = TSOnHold
fromTaskStatus "failed" = TSFailed
fromTaskStatus "completed" = TSCompleted
fromTaskStatus "entered-in-error" = TSEnteredInError


data TaskIntent
    = TIUnknown
    | TIProposal
    | TIPlan
    | TIOrder
    | TIOriginalOrder
    | TIReflexOrder
    | TIFillerOrder
    | TIInstanceOrder
    | TIOption
  deriving (Eq, Show)

instance ToJSON TaskIntent where
    toJSON TIUnknown = String "unknown"
    toJSON TIProposal = String "proposal"
    toJSON TIPlan = String "plan"
    toJSON TIOrder = String "order"
    toJSON TIOriginalOrder = String "original-order"
    toJSON TIReflexOrder = String "reflex-order"
    toJSON TIFillerOrder = String "filler-order"
    toJSON TIInstanceOrder = String "instance-order"
    toJSON TIOption = String "option"
instance FromJSON TaskIntent where
    parseJSON "unknown" = return TIUnknown
    parseJSON "proposal" = return TIProposal
    parseJSON "plan" = return TIPlan
    parseJSON "order" = return TIOrder
    parseJSON "original-order" = return TIOriginalOrder
    parseJSON "reflex-order" = return TIReflexOrder
    parseJSON "filler-order" = return TIFillerOrder
    parseJSON "instance-order" = return TIInstanceOrder
    parseJSON "option" = return TIOption

toTaskIntent TIUnknown = "unknown"
toTaskIntent TIProposal = "proposal"
toTaskIntent TIPlan = "plan"
toTaskIntent TIOrder = "order"
toTaskIntent TIOriginalOrder = "original-order"
toTaskIntent TIReflexOrder = "reflex-order"
toTaskIntent TIFillerOrder = "filler-order"
toTaskIntent TIInstanceOrder = "instance-order"
toTaskIntent TIOption = "option"
fromTaskIntent "unknown" = TIUnknown
fromTaskIntent "proposal" = TIProposal
fromTaskIntent "plan" = TIPlan
fromTaskIntent "order" = TIOrder
fromTaskIntent "original-order" = TIOriginalOrder
fromTaskIntent "reflex-order" = TIReflexOrder
fromTaskIntent "filler-order" = TIFillerOrder
fromTaskIntent "instance-order" = TIInstanceOrder
fromTaskIntent "option" = TIOption


data TaskPriority
    = TPRoutine
    | TPUrgent
    | TPAsap
    | TPStat
  deriving (Eq, Show)

instance ToJSON TaskPriority where
    toJSON TPRoutine = String "routine"
    toJSON TPUrgent = String "urgent"
    toJSON TPAsap = String "asap"
    toJSON TPStat = String "stat"
instance FromJSON TaskPriority where
    parseJSON "routine" = return TPRoutine
    parseJSON "urgent" = return TPUrgent
    parseJSON "asap" = return TPAsap
    parseJSON "stat" = return TPStat

toTaskPriority TPRoutine = "routine"
toTaskPriority TPUrgent = "urgent"
toTaskPriority TPAsap = "asap"
toTaskPriority TPStat = "stat"
fromTaskPriority "routine" = TPRoutine
fromTaskPriority "urgent" = TPUrgent
fromTaskPriority "asap" = TPAsap
fromTaskPriority "stat" = TPStat


data Task = Task {
    taskId :: Maybe Id
  , taskMeta :: Maybe Meta
  , taskImplicitRules :: Maybe Uri
  , taskLanguage :: Maybe Language
  , taskText :: Maybe Narrative
--    taskContained :: [ResourceContainer]
  , taskExtension :: [Extension]
  , taskModifierExtension :: [Extension]
  , taskIdentifier :: [Identifier]
  , taskInstantiatesCanonical :: Maybe Canonical
  , taskInstantiatesUri :: Maybe Uri
  , taskBasedOn :: [Reference]
  , taskGroupIdentifier :: Maybe Identifier
  , taskPartOf :: [Reference]
  , taskStatus :: TaskStatus
  , taskStatusReason :: Maybe CodeableConcept
  , taskBusinessStatus :: Maybe CodeableConcept
  , taskIntent :: TaskIntent
  , taskPriority :: Maybe TaskPriority
  , taskCode :: Maybe CodeableConcept
  , taskDescription :: Maybe Text
  , taskFocus :: Maybe Reference
  , taskFor :: Maybe Reference
  , taskEncounter :: Maybe Reference
  , taskExecutionPeriod :: Maybe Period
  , taskAuthoredOn :: Maybe DateTime
  , taskLastModified :: Maybe DateTime
  , taskRequester :: Maybe Reference
  , taskPerformerType :: [CodeableConcept]
  , taskOwner :: Maybe Reference
  , taskLocation :: Maybe Reference
  , taskReasonCode :: Maybe CodeableConcept
  , taskReasonReference :: Maybe Reference
  , taskInsurance :: [Reference]
  , taskNote :: [Annotation]
  , taskRelevantHistory :: [Reference]
  , taskRestriction :: Maybe TaskRestriction
  , taskInput :: [TaskInput]
  , taskOutput :: [TaskOutput]
  } deriving (Eq, Show)
--

instance ToJSON Task where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Task")
    ,  "id" .= toJSON (taskId p)
    ,  "meta" .= toJSON (taskMeta p)
    ,  "implicitRules" .= toJSON (taskImplicitRules p)
    ,  "language" .= toJSON (taskLanguage p)
    ,  "text" .= toJSON (taskText p)
--    , "contained" .= toJSON (taskContained p)
    ,  "extension" .= toJSON (taskExtension p)
    ,  "modifierExtension" .= toJSON (taskModifierExtension p)
    ,  "identifier" .= toJSON (taskIdentifier p)
    ,  "instantiatesCanonical" .= toJSON (taskInstantiatesCanonical p)
    ,  "instantiatesUri" .= toJSON (taskInstantiatesUri p)
    ,  "basedOn" .= toJSON (taskBasedOn p)
    ,  "groupIdentifier" .= toJSON (taskGroupIdentifier p)
    ,  "partOf" .= toJSON (taskPartOf p)
    ,  "status" .= toJSON (taskStatus p)
    ,  "statusReason" .= toJSON (taskStatusReason p)
    ,  "businessStatus" .= toJSON (taskBusinessStatus p)
    ,  "intent" .= toJSON (taskIntent p)
    ,  "priority" .= toJSON (taskPriority p)
    ,  "code" .= toJSON (taskCode p)
    ,  "description" .= toJSON (taskDescription p)
    ,  "focus" .= toJSON (taskFocus p)
    ,  "for" .= toJSON (taskFor p)
    ,  "encounter" .= toJSON (taskEncounter p)
    ,  "executionPeriod" .= toJSON (taskExecutionPeriod p)
    ,  "authoredOn" .= toJSON (taskAuthoredOn p)
    ,  "lastModified" .= toJSON (taskLastModified p)
    ,  "requester" .= toJSON (taskRequester p)
    ,  "performerType" .= toJSON (taskPerformerType p)
    ,  "owner" .= toJSON (taskOwner p)
    ,  "location" .= toJSON (taskLocation p)
    ,  "reasonCode" .= toJSON (taskReasonCode p)
    ,  "reasonReference" .= toJSON (taskReasonReference p)
    ,  "insurance" .= toJSON (taskInsurance p)
    ,  "note" .= toJSON (taskNote p)
    ,  "relevantHistory" .= toJSON (taskRelevantHistory p)
    ,  "restriction" .= toJSON (taskRestriction p)
    ,  "input" .= toJSON (taskInput p)
    ,  "output" .= toJSON (taskOutput p)
    ]
instance FromJSON Task where
  parseJSON = withObject "Task" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Task" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        instantiatesCanonical <- o .:? "instantiatesCanonical"
        instantiatesUri <- o .:? "instantiatesUri"
        basedOn <- o .:? "basedOn" .!= []
        groupIdentifier <- o .:? "groupIdentifier"
        partOf <- o .:? "partOf" .!= []
        status <- o .:  "status"
        statusReason <- o .:? "statusReason"
        businessStatus <- o .:? "businessStatus"
        intent <- o .:  "intent"
        priority <- o .:? "priority"
        code <- o .:? "code"
        description <- o .:? "description"
        focus <- o .:? "focus"
        for <- o .:? "for"
        encounter <- o .:? "encounter"
        executionPeriod <- o .:? "executionPeriod"
        authoredOn <- o .:? "authoredOn"
        lastModified <- o .:? "lastModified"
        requester <- o .:? "requester"
        performerType <- o .:? "performerType" .!= []
        owner <- o .:? "owner"
        location <- o .:? "location"
        reasonCode <- o .:? "reasonCode"
        reasonReference <- o .:? "reasonReference"
        insurance <- o .:? "insurance" .!= []
        note <- o .:? "note" .!= []
        relevantHistory <- o .:? "relevantHistory" .!= []
        restriction <- o .:? "restriction"
        input <- o .:? "input" .!= []
        output <- o .:? "output" .!= []
        return Task{
            taskId = id
          , taskMeta = meta
          , taskImplicitRules = implicitRules
          , taskLanguage = language
          , taskText = text
--          , taskContained = contained
          , taskExtension = extension
          , taskModifierExtension = modifierExtension
          , taskIdentifier = identifier
          , taskInstantiatesCanonical = instantiatesCanonical
          , taskInstantiatesUri = instantiatesUri
          , taskBasedOn = basedOn
          , taskGroupIdentifier = groupIdentifier
          , taskPartOf = partOf
          , taskStatus = status
          , taskStatusReason = statusReason
          , taskBusinessStatus = businessStatus
          , taskIntent = intent
          , taskPriority = priority
          , taskCode = code
          , taskDescription = description
          , taskFocus = focus
          , taskFor = for
          , taskEncounter = encounter
          , taskExecutionPeriod = executionPeriod
          , taskAuthoredOn = authoredOn
          , taskLastModified = lastModified
          , taskRequester = requester
          , taskPerformerType = performerType
          , taskOwner = owner
          , taskLocation = location
          , taskReasonCode = reasonCode
          , taskReasonReference = reasonReference
          , taskInsurance = insurance
          , taskNote = note
          , taskRelevantHistory = relevantHistory
          , taskRestriction = restriction
          , taskInput = input
          , taskOutput = output
          }
      _ -> fail "not a Task"
instance Xmlbf.ToXml Task where
  toXml p = Xmlbf.element "Task" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (taskId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (taskMeta p))
             , OptVal   "implicitRules" (fmap toUri (taskImplicitRules p))
             , OptVal   "language" (fmap toLanguage (taskLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (taskText p))
--             , PropList "contained" (fmap Xmlbf.toXml (taskContained p))
             , PropList "extension" (fmap Xmlbf.toXml (taskExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (taskModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (taskIdentifier p))
             , OptVal   "instantiatesCanonical" (fmap toCanonical (taskInstantiatesCanonical p))
             , OptVal   "instantiatesUri" (fmap toUri (taskInstantiatesUri p))
             , PropList "basedOn" (fmap Xmlbf.toXml (taskBasedOn p))
             , OptProp  "groupIdentifier" (fmap Xmlbf.toXml (taskGroupIdentifier p))
             , PropList "partOf" (fmap Xmlbf.toXml (taskPartOf p))
             , Val      "status" (     toTaskStatus (taskStatus p))
             , OptProp  "statusReason" (fmap Xmlbf.toXml (taskStatusReason p))
             , OptProp  "businessStatus" (fmap Xmlbf.toXml (taskBusinessStatus p))
             , Val      "intent" (     toTaskIntent (taskIntent p))
             , OptVal   "priority" (fmap toTaskPriority (taskPriority p))
             , OptProp  "code" (fmap Xmlbf.toXml (taskCode p))
             , OptVal   "description" (fmap toString (taskDescription p))
             , OptProp  "focus" (fmap Xmlbf.toXml (taskFocus p))
             , OptProp  "for" (fmap Xmlbf.toXml (taskFor p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (taskEncounter p))
             , OptProp  "executionPeriod" (fmap Xmlbf.toXml (taskExecutionPeriod p))
             , OptVal   "authoredOn" (fmap toDateTime (taskAuthoredOn p))
             , OptVal   "lastModified" (fmap toDateTime (taskLastModified p))
             , OptProp  "requester" (fmap Xmlbf.toXml (taskRequester p))
             , PropList "performerType" (fmap Xmlbf.toXml (taskPerformerType p))
             , OptProp  "owner" (fmap Xmlbf.toXml (taskOwner p))
             , OptProp  "location" (fmap Xmlbf.toXml (taskLocation p))
             , OptProp  "reasonCode" (fmap Xmlbf.toXml (taskReasonCode p))
             , OptProp  "reasonReference" (fmap Xmlbf.toXml (taskReasonReference p))
             , PropList "insurance" (fmap Xmlbf.toXml (taskInsurance p))
             , PropList "note" (fmap Xmlbf.toXml (taskNote p))
             , PropList "relevantHistory" (fmap Xmlbf.toXml (taskRelevantHistory p))
             , OptProp  "restriction" (fmap Xmlbf.toXml (taskRestriction p))
             , PropList "input" (fmap Xmlbf.toXml (taskInput p))
             , PropList "output" (fmap Xmlbf.toXml (taskOutput p))
             ]
instance Xmlbf.FromXml Task where
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
    instantiatesCanonical <- optional $ Xmlbf.pElement "instantiatesCanonical" (Xmlbf.pAttr "value")
    instantiatesUri <- optional $ Xmlbf.pElement "instantiatesUri" (Xmlbf.pAttr "value")
    basedOn <- many     $ Xmlbf.pElement "basedOn" Xmlbf.fromXml
    groupIdentifier <- optional $ Xmlbf.pElement "groupIdentifier" Xmlbf.fromXml
    partOf <- many     $ Xmlbf.pElement "partOf" Xmlbf.fromXml
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    statusReason <- optional $ Xmlbf.pElement "statusReason" Xmlbf.fromXml
    businessStatus <- optional $ Xmlbf.pElement "businessStatus" Xmlbf.fromXml
    intent <-            Xmlbf.pElement "intent" (Xmlbf.pAttr "value")
    priority <- optional $ Xmlbf.pElement "priority" (Xmlbf.pAttr "value")
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    focus <- optional $ Xmlbf.pElement "focus" Xmlbf.fromXml
    for <- optional $ Xmlbf.pElement "for" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    executionPeriod <- optional $ Xmlbf.pElement "executionPeriod" Xmlbf.fromXml
    authoredOn <- optional $ Xmlbf.pElement "authoredOn" (Xmlbf.pAttr "value")
    lastModified <- optional $ Xmlbf.pElement "lastModified" (Xmlbf.pAttr "value")
    requester <- optional $ Xmlbf.pElement "requester" Xmlbf.fromXml
    performerType <- many     $ Xmlbf.pElement "performerType" Xmlbf.fromXml
    owner <- optional $ Xmlbf.pElement "owner" Xmlbf.fromXml
    location <- optional $ Xmlbf.pElement "location" Xmlbf.fromXml
    reasonCode <- optional $ Xmlbf.pElement "reasonCode" Xmlbf.fromXml
    reasonReference <- optional $ Xmlbf.pElement "reasonReference" Xmlbf.fromXml
    insurance <- many     $ Xmlbf.pElement "insurance" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    relevantHistory <- many     $ Xmlbf.pElement "relevantHistory" Xmlbf.fromXml
    restriction <- optional $ Xmlbf.pElement "restriction" Xmlbf.fromXml
    input <- many     $ Xmlbf.pElement "input" Xmlbf.fromXml
    output <- many     $ Xmlbf.pElement "output" Xmlbf.fromXml
    return Task {
            taskId = fmap fromId id
          , taskMeta = meta
          , taskImplicitRules = fmap fromUri implicitRules
          , taskLanguage = fmap fromLanguage language
          , taskText = text
--          , taskContained = contained
          , taskExtension = extension
          , taskModifierExtension = modifierExtension
          , taskIdentifier = identifier
          , taskInstantiatesCanonical = fmap fromCanonical instantiatesCanonical
          , taskInstantiatesUri = fmap fromUri instantiatesUri
          , taskBasedOn = basedOn
          , taskGroupIdentifier = groupIdentifier
          , taskPartOf = partOf
          , taskStatus =      fromTaskStatus status
          , taskStatusReason = statusReason
          , taskBusinessStatus = businessStatus
          , taskIntent =      fromTaskIntent intent
          , taskPriority = fmap fromTaskPriority priority
          , taskCode = code
          , taskDescription = fmap fromString description
          , taskFocus = focus
          , taskFor = for
          , taskEncounter = encounter
          , taskExecutionPeriod = executionPeriod
          , taskAuthoredOn = fmap fromDateTime authoredOn
          , taskLastModified = fmap fromDateTime lastModified
          , taskRequester = requester
          , taskPerformerType = performerType
          , taskOwner = owner
          , taskLocation = location
          , taskReasonCode = reasonCode
          , taskReasonReference = reasonReference
          , taskInsurance = insurance
          , taskNote = note
          , taskRelevantHistory = relevantHistory
          , taskRestriction = restriction
          , taskInput = input
          , taskOutput = output
          }



data TaskOutputValue
    = TaskOutputValueBase64Binary Base64Binary
    | TaskOutputValueBoolean Boolean
    | TaskOutputValueCanonical Canonical
    | TaskOutputValueCode Code
    | TaskOutputValueDate Date
    | TaskOutputValueDateTime DateTime
    | TaskOutputValueDecimal Decimal
    | TaskOutputValueId Id
    | TaskOutputValueInstant Instant
    | TaskOutputValueInteger Integer
    | TaskOutputValueMarkdown Markdown
    | TaskOutputValueOid Oid
    | TaskOutputValuePositiveInt PositiveInt
    | TaskOutputValueString Text
    | TaskOutputValueTime Time
    | TaskOutputValueUnsignedInt UnsignedInt
    | TaskOutputValueUri Uri
    | TaskOutputValueUrl Url
    | TaskOutputValueUuid Uuid
    | TaskOutputValueAddress Address
    | TaskOutputValueAge Age
    | TaskOutputValueAnnotation Annotation
    | TaskOutputValueAttachment Attachment
    | TaskOutputValueCodeableConcept CodeableConcept
    | TaskOutputValueCoding Coding
    | TaskOutputValueContactPoint ContactPoint
    | TaskOutputValueCount Count
    | TaskOutputValueDistance Distance
    | TaskOutputValueDuration Duration
    | TaskOutputValueHumanName HumanName
    | TaskOutputValueIdentifier Identifier
    | TaskOutputValueMoney Money
    | TaskOutputValuePeriod Period
    | TaskOutputValueQuantity Quantity
    | TaskOutputValueRange Range
    | TaskOutputValueRatio Ratio
    | TaskOutputValueReference Reference
    | TaskOutputValueSampledData SampledData
    | TaskOutputValueSignature Signature
    | TaskOutputValueTiming Timing
    | TaskOutputValueContactDetail ContactDetail
    | TaskOutputValueContributor Contributor
    | TaskOutputValueDataRequirement DataRequirement
    | TaskOutputValueExpression Expression
    | TaskOutputValueParameterDefinition ParameterDefinition
    | TaskOutputValueRelatedArtifact RelatedArtifact
    | TaskOutputValueTriggerDefinition TriggerDefinition
    | TaskOutputValueUsageContext UsageContext
    | TaskOutputValueDosage Dosage
    deriving (Eq, Show)

data TaskOutput = TaskOutput {
    taskOutputAttrId :: Maybe Text
  , taskOutputExtension :: [Extension]
  , taskOutputModifierExtension :: [Extension]
  , taskOutputType :: CodeableConcept
  , taskOutputValueBase64Binary :: Base64Binary
  , taskOutputValueBoolean :: Boolean
  , taskOutputValueCanonical :: Canonical
  , taskOutputValueCode :: Code
  , taskOutputValueDate :: Date
  , taskOutputValueDateTime :: DateTime
  , taskOutputValueDecimal :: Decimal
  , taskOutputValueId :: Id
  , taskOutputValueInstant :: Instant
  , taskOutputValueInteger :: Integer
  , taskOutputValueMarkdown :: Markdown
  , taskOutputValueOid :: Oid
  , taskOutputValuePositiveInt :: PositiveInt
  , taskOutputValueString :: Text
  , taskOutputValueTime :: Time
  , taskOutputValueUnsignedInt :: UnsignedInt
  , taskOutputValueUri :: Uri
  , taskOutputValueUrl :: Url
  , taskOutputValueUuid :: Uuid
  , taskOutputValueAddress :: Address
  , taskOutputValueAge :: Age
  , taskOutputValueAnnotation :: Annotation
  , taskOutputValueAttachment :: Attachment
  , taskOutputValueCodeableConcept :: CodeableConcept
  , taskOutputValueCoding :: Coding
  , taskOutputValueContactPoint :: ContactPoint
  , taskOutputValueCount :: Count
  , taskOutputValueDistance :: Distance
  , taskOutputValueDuration :: Duration
  , taskOutputValueHumanName :: HumanName
  , taskOutputValueIdentifier :: Identifier
  , taskOutputValueMoney :: Money
  , taskOutputValuePeriod :: Period
  , taskOutputValueQuantity :: Quantity
  , taskOutputValueRange :: Range
  , taskOutputValueRatio :: Ratio
  , taskOutputValueReference :: Reference
  , taskOutputValueSampledData :: SampledData
  , taskOutputValueSignature :: Signature
  , taskOutputValueTiming :: Timing
  , taskOutputValueContactDetail :: ContactDetail
  , taskOutputValueContributor :: Contributor
  , taskOutputValueDataRequirement :: DataRequirement
  , taskOutputValueExpression :: Expression
  , taskOutputValueParameterDefinition :: ParameterDefinition
  , taskOutputValueRelatedArtifact :: RelatedArtifact
  , taskOutputValueTriggerDefinition :: TriggerDefinition
  , taskOutputValueUsageContext :: UsageContext
  , taskOutputValueDosage :: Dosage
  } deriving (Eq, Show)
--

instance ToJSON TaskOutput where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (taskOutputAttrId p)
    ,  "extension" .= toJSON (taskOutputExtension p)
    ,  "modifierExtension" .= toJSON (taskOutputModifierExtension p)
    ,  "type" .= toJSON (taskOutputType p)
    ,  "valueBase64Binary" .= toJSON (taskOutputValueBase64Binary p)
    ,  "valueBoolean" .= toJSON (taskOutputValueBoolean p)
    ,  "valueCanonical" .= toJSON (taskOutputValueCanonical p)
    ,  "valueCode" .= toJSON (taskOutputValueCode p)
    ,  "valueDate" .= toJSON (taskOutputValueDate p)
    ,  "valueDateTime" .= toJSON (taskOutputValueDateTime p)
    ,  "valueDecimal" .= toJSON (taskOutputValueDecimal p)
    ,  "valueId" .= toJSON (taskOutputValueId p)
    ,  "valueInstant" .= toJSON (taskOutputValueInstant p)
    ,  "valueInteger" .= toJSON (taskOutputValueInteger p)
    ,  "valueMarkdown" .= toJSON (taskOutputValueMarkdown p)
    ,  "valueOid" .= toJSON (taskOutputValueOid p)
    ,  "valuePositiveInt" .= toJSON (taskOutputValuePositiveInt p)
    ,  "valueString" .= toJSON (taskOutputValueString p)
    ,  "valueTime" .= toJSON (taskOutputValueTime p)
    ,  "valueUnsignedInt" .= toJSON (taskOutputValueUnsignedInt p)
    ,  "valueUri" .= toJSON (taskOutputValueUri p)
    ,  "valueUrl" .= toJSON (taskOutputValueUrl p)
    ,  "valueUuid" .= toJSON (taskOutputValueUuid p)
    ,  "valueAddress" .= toJSON (taskOutputValueAddress p)
    ,  "valueAge" .= toJSON (taskOutputValueAge p)
    ,  "valueAnnotation" .= toJSON (taskOutputValueAnnotation p)
    ,  "valueAttachment" .= toJSON (taskOutputValueAttachment p)
    ,  "valueCodeableConcept" .= toJSON (taskOutputValueCodeableConcept p)
    ,  "valueCoding" .= toJSON (taskOutputValueCoding p)
    ,  "valueContactPoint" .= toJSON (taskOutputValueContactPoint p)
    ,  "valueCount" .= toJSON (taskOutputValueCount p)
    ,  "valueDistance" .= toJSON (taskOutputValueDistance p)
    ,  "valueDuration" .= toJSON (taskOutputValueDuration p)
    ,  "valueHumanName" .= toJSON (taskOutputValueHumanName p)
    ,  "valueIdentifier" .= toJSON (taskOutputValueIdentifier p)
    ,  "valueMoney" .= toJSON (taskOutputValueMoney p)
    ,  "valuePeriod" .= toJSON (taskOutputValuePeriod p)
    ,  "valueQuantity" .= toJSON (taskOutputValueQuantity p)
    ,  "valueRange" .= toJSON (taskOutputValueRange p)
    ,  "valueRatio" .= toJSON (taskOutputValueRatio p)
    ,  "valueReference" .= toJSON (taskOutputValueReference p)
    ,  "valueSampledData" .= toJSON (taskOutputValueSampledData p)
    ,  "valueSignature" .= toJSON (taskOutputValueSignature p)
    ,  "valueTiming" .= toJSON (taskOutputValueTiming p)
    ,  "valueContactDetail" .= toJSON (taskOutputValueContactDetail p)
    ,  "valueContributor" .= toJSON (taskOutputValueContributor p)
    ,  "valueDataRequirement" .= toJSON (taskOutputValueDataRequirement p)
    ,  "valueExpression" .= toJSON (taskOutputValueExpression p)
    ,  "valueParameterDefinition" .= toJSON (taskOutputValueParameterDefinition p)
    ,  "valueRelatedArtifact" .= toJSON (taskOutputValueRelatedArtifact p)
    ,  "valueTriggerDefinition" .= toJSON (taskOutputValueTriggerDefinition p)
    ,  "valueUsageContext" .= toJSON (taskOutputValueUsageContext p)
    ,  "valueDosage" .= toJSON (taskOutputValueDosage p)
    ]
instance FromJSON TaskOutput where
  parseJSON = withObject "TaskOutput" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:  "type"
        valueBase64Binary <- o .:  "valueBase64Binary"
        valueBoolean <- o .:  "valueBoolean"
        valueCanonical <- o .:  "valueCanonical"
        valueCode <- o .:  "valueCode"
        valueDate <- o .:  "valueDate"
        valueDateTime <- o .:  "valueDateTime"
        valueDecimal <- o .:  "valueDecimal"
        valueId <- o .:  "valueId"
        valueInstant <- o .:  "valueInstant"
        valueInteger <- o .:  "valueInteger"
        valueMarkdown <- o .:  "valueMarkdown"
        valueOid <- o .:  "valueOid"
        valuePositiveInt <- o .:  "valuePositiveInt"
        valueString <- o .:  "valueString"
        valueTime <- o .:  "valueTime"
        valueUnsignedInt <- o .:  "valueUnsignedInt"
        valueUri <- o .:  "valueUri"
        valueUrl <- o .:  "valueUrl"
        valueUuid <- o .:  "valueUuid"
        valueAddress <- o .:  "valueAddress"
        valueAge <- o .:  "valueAge"
        valueAnnotation <- o .:  "valueAnnotation"
        valueAttachment <- o .:  "valueAttachment"
        valueCodeableConcept <- o .:  "valueCodeableConcept"
        valueCoding <- o .:  "valueCoding"
        valueContactPoint <- o .:  "valueContactPoint"
        valueCount <- o .:  "valueCount"
        valueDistance <- o .:  "valueDistance"
        valueDuration <- o .:  "valueDuration"
        valueHumanName <- o .:  "valueHumanName"
        valueIdentifier <- o .:  "valueIdentifier"
        valueMoney <- o .:  "valueMoney"
        valuePeriod <- o .:  "valuePeriod"
        valueQuantity <- o .:  "valueQuantity"
        valueRange <- o .:  "valueRange"
        valueRatio <- o .:  "valueRatio"
        valueReference <- o .:  "valueReference"
        valueSampledData <- o .:  "valueSampledData"
        valueSignature <- o .:  "valueSignature"
        valueTiming <- o .:  "valueTiming"
        valueContactDetail <- o .:  "valueContactDetail"
        valueContributor <- o .:  "valueContributor"
        valueDataRequirement <- o .:  "valueDataRequirement"
        valueExpression <- o .:  "valueExpression"
        valueParameterDefinition <- o .:  "valueParameterDefinition"
        valueRelatedArtifact <- o .:  "valueRelatedArtifact"
        valueTriggerDefinition <- o .:  "valueTriggerDefinition"
        valueUsageContext <- o .:  "valueUsageContext"
        valueDosage <- o .:  "valueDosage"
        return TaskOutput{
            taskOutputAttrId = id
          , taskOutputExtension = extension
          , taskOutputModifierExtension = modifierExtension
          , taskOutputType = ty
          , taskOutputValueBase64Binary = valueBase64Binary
          , taskOutputValueBoolean = valueBoolean
          , taskOutputValueCanonical = valueCanonical
          , taskOutputValueCode = valueCode
          , taskOutputValueDate = valueDate
          , taskOutputValueDateTime = valueDateTime
          , taskOutputValueDecimal = valueDecimal
          , taskOutputValueId = valueId
          , taskOutputValueInstant = valueInstant
          , taskOutputValueInteger = valueInteger
          , taskOutputValueMarkdown = valueMarkdown
          , taskOutputValueOid = valueOid
          , taskOutputValuePositiveInt = valuePositiveInt
          , taskOutputValueString = valueString
          , taskOutputValueTime = valueTime
          , taskOutputValueUnsignedInt = valueUnsignedInt
          , taskOutputValueUri = valueUri
          , taskOutputValueUrl = valueUrl
          , taskOutputValueUuid = valueUuid
          , taskOutputValueAddress = valueAddress
          , taskOutputValueAge = valueAge
          , taskOutputValueAnnotation = valueAnnotation
          , taskOutputValueAttachment = valueAttachment
          , taskOutputValueCodeableConcept = valueCodeableConcept
          , taskOutputValueCoding = valueCoding
          , taskOutputValueContactPoint = valueContactPoint
          , taskOutputValueCount = valueCount
          , taskOutputValueDistance = valueDistance
          , taskOutputValueDuration = valueDuration
          , taskOutputValueHumanName = valueHumanName
          , taskOutputValueIdentifier = valueIdentifier
          , taskOutputValueMoney = valueMoney
          , taskOutputValuePeriod = valuePeriod
          , taskOutputValueQuantity = valueQuantity
          , taskOutputValueRange = valueRange
          , taskOutputValueRatio = valueRatio
          , taskOutputValueReference = valueReference
          , taskOutputValueSampledData = valueSampledData
          , taskOutputValueSignature = valueSignature
          , taskOutputValueTiming = valueTiming
          , taskOutputValueContactDetail = valueContactDetail
          , taskOutputValueContributor = valueContributor
          , taskOutputValueDataRequirement = valueDataRequirement
          , taskOutputValueExpression = valueExpression
          , taskOutputValueParameterDefinition = valueParameterDefinition
          , taskOutputValueRelatedArtifact = valueRelatedArtifact
          , taskOutputValueTriggerDefinition = valueTriggerDefinition
          , taskOutputValueUsageContext = valueUsageContext
          , taskOutputValueDosage = valueDosage
          }
instance Xmlbf.ToXml TaskOutput where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (taskOutputAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (taskOutputExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (taskOutputModifierExtension p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (taskOutputType p))
             , Val      "valueBase64Binary" (     toBase64Binary (taskOutputValueBase64Binary p))
             , Val      "valueBoolean" (     toBoolean (taskOutputValueBoolean p))
             , Val      "valueCanonical" (     toCanonical (taskOutputValueCanonical p))
             , Val      "valueCode" (     toCode (taskOutputValueCode p))
             , Val      "valueDate" (     toDate (taskOutputValueDate p))
             , Val      "valueDateTime" (     toDateTime (taskOutputValueDateTime p))
             , Val      "valueDecimal" (     toDecimal (taskOutputValueDecimal p))
             , Val      "valueId" (     toId (taskOutputValueId p))
             , Val      "valueInstant" (     toInstant (taskOutputValueInstant p))
             , Val      "valueInteger" (     toInt (taskOutputValueInteger p))
             , Val      "valueMarkdown" (     toMarkdown (taskOutputValueMarkdown p))
             , Val      "valueOid" (     toOid (taskOutputValueOid p))
             , Val      "valuePositiveInt" (     toPositiveInt (taskOutputValuePositiveInt p))
             , Val      "valueString" (     toString (taskOutputValueString p))
             , Val      "valueTime" (     toTime (taskOutputValueTime p))
             , Val      "valueUnsignedInt" (     toUnsignedInt (taskOutputValueUnsignedInt p))
             , Val      "valueUri" (     toUri (taskOutputValueUri p))
             , Val      "valueUrl" (     toUrl (taskOutputValueUrl p))
             , Val      "valueUuid" (     toUuid (taskOutputValueUuid p))
             , Prop     "valueAddress" (HM.empty, Xmlbf.toXml (taskOutputValueAddress p))
             , Prop     "valueAge" (HM.empty, Xmlbf.toXml (taskOutputValueAge p))
             , Prop     "valueAnnotation" (HM.empty, Xmlbf.toXml (taskOutputValueAnnotation p))
             , Prop     "valueAttachment" (HM.empty, Xmlbf.toXml (taskOutputValueAttachment p))
             , Prop     "valueCodeableConcept" (HM.empty, Xmlbf.toXml (taskOutputValueCodeableConcept p))
             , Prop     "valueCoding" (HM.empty, Xmlbf.toXml (taskOutputValueCoding p))
             , Prop     "valueContactPoint" (HM.empty, Xmlbf.toXml (taskOutputValueContactPoint p))
             , Prop     "valueCount" (HM.empty, Xmlbf.toXml (taskOutputValueCount p))
             , Prop     "valueDistance" (HM.empty, Xmlbf.toXml (taskOutputValueDistance p))
             , Prop     "valueDuration" (HM.empty, Xmlbf.toXml (taskOutputValueDuration p))
             , Prop     "valueHumanName" (HM.empty, Xmlbf.toXml (taskOutputValueHumanName p))
             , Prop     "valueIdentifier" (HM.empty, Xmlbf.toXml (taskOutputValueIdentifier p))
             , Prop     "valueMoney" (HM.empty, Xmlbf.toXml (taskOutputValueMoney p))
             , Prop     "valuePeriod" (HM.empty, Xmlbf.toXml (taskOutputValuePeriod p))
             , Prop     "valueQuantity" (HM.empty, Xmlbf.toXml (taskOutputValueQuantity p))
             , Prop     "valueRange" (HM.empty, Xmlbf.toXml (taskOutputValueRange p))
             , Prop     "valueRatio" (HM.empty, Xmlbf.toXml (taskOutputValueRatio p))
             , Prop     "valueReference" (HM.empty, Xmlbf.toXml (taskOutputValueReference p))
             , Prop     "valueSampledData" (HM.empty, Xmlbf.toXml (taskOutputValueSampledData p))
             , Prop     "valueSignature" (HM.empty, Xmlbf.toXml (taskOutputValueSignature p))
             , Prop     "valueTiming" (HM.empty, Xmlbf.toXml (taskOutputValueTiming p))
             , Prop     "valueContactDetail" (HM.empty, Xmlbf.toXml (taskOutputValueContactDetail p))
             , Prop     "valueContributor" (HM.empty, Xmlbf.toXml (taskOutputValueContributor p))
             , Prop     "valueDataRequirement" (HM.empty, Xmlbf.toXml (taskOutputValueDataRequirement p))
             , Prop     "valueExpression" (HM.empty, Xmlbf.toXml (taskOutputValueExpression p))
             , Prop     "valueParameterDefinition" (HM.empty, Xmlbf.toXml (taskOutputValueParameterDefinition p))
             , Prop     "valueRelatedArtifact" (HM.empty, Xmlbf.toXml (taskOutputValueRelatedArtifact p))
             , Prop     "valueTriggerDefinition" (HM.empty, Xmlbf.toXml (taskOutputValueTriggerDefinition p))
             , Prop     "valueUsageContext" (HM.empty, Xmlbf.toXml (taskOutputValueUsageContext p))
             , Prop     "valueDosage" (HM.empty, Xmlbf.toXml (taskOutputValueDosage p))
             ]
instance Xmlbf.FromXml TaskOutput where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" Xmlbf.fromXml
    valueBase64Binary <-            Xmlbf.pElement "valueBase64Binary" (Xmlbf.pAttr "value")
    valueBoolean <-            Xmlbf.pElement "valueBoolean" (Xmlbf.pAttr "value")
    valueCanonical <-            Xmlbf.pElement "valueCanonical" (Xmlbf.pAttr "value")
    valueCode <-            Xmlbf.pElement "valueCode" (Xmlbf.pAttr "value")
    valueDate <-            Xmlbf.pElement "valueDate" (Xmlbf.pAttr "value")
    valueDateTime <-            Xmlbf.pElement "valueDateTime" (Xmlbf.pAttr "value")
    valueDecimal <-            Xmlbf.pElement "valueDecimal" (Xmlbf.pAttr "value")
    valueId <-            Xmlbf.pElement "valueId" (Xmlbf.pAttr "value")
    valueInstant <-            Xmlbf.pElement "valueInstant" (Xmlbf.pAttr "value")
    valueInteger <-            Xmlbf.pElement "valueInteger" (Xmlbf.pAttr "value")
    valueMarkdown <-            Xmlbf.pElement "valueMarkdown" (Xmlbf.pAttr "value")
    valueOid <-            Xmlbf.pElement "valueOid" (Xmlbf.pAttr "value")
    valuePositiveInt <-            Xmlbf.pElement "valuePositiveInt" (Xmlbf.pAttr "value")
    valueString <-            Xmlbf.pElement "valueString" (Xmlbf.pAttr "value")
    valueTime <-            Xmlbf.pElement "valueTime" (Xmlbf.pAttr "value")
    valueUnsignedInt <-            Xmlbf.pElement "valueUnsignedInt" (Xmlbf.pAttr "value")
    valueUri <-            Xmlbf.pElement "valueUri" (Xmlbf.pAttr "value")
    valueUrl <-            Xmlbf.pElement "valueUrl" (Xmlbf.pAttr "value")
    valueUuid <-            Xmlbf.pElement "valueUuid" (Xmlbf.pAttr "value")
    valueAddress <-            Xmlbf.pElement "valueAddress" Xmlbf.fromXml
    valueAge <-            Xmlbf.pElement "valueAge" Xmlbf.fromXml
    valueAnnotation <-            Xmlbf.pElement "valueAnnotation" Xmlbf.fromXml
    valueAttachment <-            Xmlbf.pElement "valueAttachment" Xmlbf.fromXml
    valueCodeableConcept <-            Xmlbf.pElement "valueCodeableConcept" Xmlbf.fromXml
    valueCoding <-            Xmlbf.pElement "valueCoding" Xmlbf.fromXml
    valueContactPoint <-            Xmlbf.pElement "valueContactPoint" Xmlbf.fromXml
    valueCount <-            Xmlbf.pElement "valueCount" Xmlbf.fromXml
    valueDistance <-            Xmlbf.pElement "valueDistance" Xmlbf.fromXml
    valueDuration <-            Xmlbf.pElement "valueDuration" Xmlbf.fromXml
    valueHumanName <-            Xmlbf.pElement "valueHumanName" Xmlbf.fromXml
    valueIdentifier <-            Xmlbf.pElement "valueIdentifier" Xmlbf.fromXml
    valueMoney <-            Xmlbf.pElement "valueMoney" Xmlbf.fromXml
    valuePeriod <-            Xmlbf.pElement "valuePeriod" Xmlbf.fromXml
    valueQuantity <-            Xmlbf.pElement "valueQuantity" Xmlbf.fromXml
    valueRange <-            Xmlbf.pElement "valueRange" Xmlbf.fromXml
    valueRatio <-            Xmlbf.pElement "valueRatio" Xmlbf.fromXml
    valueReference <-            Xmlbf.pElement "valueReference" Xmlbf.fromXml
    valueSampledData <-            Xmlbf.pElement "valueSampledData" Xmlbf.fromXml
    valueSignature <-            Xmlbf.pElement "valueSignature" Xmlbf.fromXml
    valueTiming <-            Xmlbf.pElement "valueTiming" Xmlbf.fromXml
    valueContactDetail <-            Xmlbf.pElement "valueContactDetail" Xmlbf.fromXml
    valueContributor <-            Xmlbf.pElement "valueContributor" Xmlbf.fromXml
    valueDataRequirement <-            Xmlbf.pElement "valueDataRequirement" Xmlbf.fromXml
    valueExpression <-            Xmlbf.pElement "valueExpression" Xmlbf.fromXml
    valueParameterDefinition <-            Xmlbf.pElement "valueParameterDefinition" Xmlbf.fromXml
    valueRelatedArtifact <-            Xmlbf.pElement "valueRelatedArtifact" Xmlbf.fromXml
    valueTriggerDefinition <-            Xmlbf.pElement "valueTriggerDefinition" Xmlbf.fromXml
    valueUsageContext <-            Xmlbf.pElement "valueUsageContext" Xmlbf.fromXml
    valueDosage <-            Xmlbf.pElement "valueDosage" Xmlbf.fromXml
    return TaskOutput {
            taskOutputAttrId = id
          , taskOutputExtension = extension
          , taskOutputModifierExtension = modifierExtension
          , taskOutputType = ty
          , taskOutputValueBase64Binary =      fromBase64Binary valueBase64Binary
          , taskOutputValueBoolean =      fromBoolean valueBoolean
          , taskOutputValueCanonical =      fromCanonical valueCanonical
          , taskOutputValueCode =      fromCode valueCode
          , taskOutputValueDate =      fromDate valueDate
          , taskOutputValueDateTime =      fromDateTime valueDateTime
          , taskOutputValueDecimal =      fromDecimal valueDecimal
          , taskOutputValueId =      fromId valueId
          , taskOutputValueInstant =      fromInstant valueInstant
          , taskOutputValueInteger =      fromInt valueInteger
          , taskOutputValueMarkdown =      fromMarkdown valueMarkdown
          , taskOutputValueOid =      fromOid valueOid
          , taskOutputValuePositiveInt =      fromPositiveInt valuePositiveInt
          , taskOutputValueString =      fromString valueString
          , taskOutputValueTime =      fromTime valueTime
          , taskOutputValueUnsignedInt =      fromUnsignedInt valueUnsignedInt
          , taskOutputValueUri =      fromUri valueUri
          , taskOutputValueUrl =      fromUrl valueUrl
          , taskOutputValueUuid =      fromUuid valueUuid
          , taskOutputValueAddress = valueAddress
          , taskOutputValueAge = valueAge
          , taskOutputValueAnnotation = valueAnnotation
          , taskOutputValueAttachment = valueAttachment
          , taskOutputValueCodeableConcept = valueCodeableConcept
          , taskOutputValueCoding = valueCoding
          , taskOutputValueContactPoint = valueContactPoint
          , taskOutputValueCount = valueCount
          , taskOutputValueDistance = valueDistance
          , taskOutputValueDuration = valueDuration
          , taskOutputValueHumanName = valueHumanName
          , taskOutputValueIdentifier = valueIdentifier
          , taskOutputValueMoney = valueMoney
          , taskOutputValuePeriod = valuePeriod
          , taskOutputValueQuantity = valueQuantity
          , taskOutputValueRange = valueRange
          , taskOutputValueRatio = valueRatio
          , taskOutputValueReference = valueReference
          , taskOutputValueSampledData = valueSampledData
          , taskOutputValueSignature = valueSignature
          , taskOutputValueTiming = valueTiming
          , taskOutputValueContactDetail = valueContactDetail
          , taskOutputValueContributor = valueContributor
          , taskOutputValueDataRequirement = valueDataRequirement
          , taskOutputValueExpression = valueExpression
          , taskOutputValueParameterDefinition = valueParameterDefinition
          , taskOutputValueRelatedArtifact = valueRelatedArtifact
          , taskOutputValueTriggerDefinition = valueTriggerDefinition
          , taskOutputValueUsageContext = valueUsageContext
          , taskOutputValueDosage = valueDosage
          }



data TaskInputValue
    = TaskInputValueBase64Binary Base64Binary
    | TaskInputValueBoolean Boolean
    | TaskInputValueCanonical Canonical
    | TaskInputValueCode Code
    | TaskInputValueDate Date
    | TaskInputValueDateTime DateTime
    | TaskInputValueDecimal Decimal
    | TaskInputValueId Id
    | TaskInputValueInstant Instant
    | TaskInputValueInteger Integer
    | TaskInputValueMarkdown Markdown
    | TaskInputValueOid Oid
    | TaskInputValuePositiveInt PositiveInt
    | TaskInputValueString Text
    | TaskInputValueTime Time
    | TaskInputValueUnsignedInt UnsignedInt
    | TaskInputValueUri Uri
    | TaskInputValueUrl Url
    | TaskInputValueUuid Uuid
    | TaskInputValueAddress Address
    | TaskInputValueAge Age
    | TaskInputValueAnnotation Annotation
    | TaskInputValueAttachment Attachment
    | TaskInputValueCodeableConcept CodeableConcept
    | TaskInputValueCoding Coding
    | TaskInputValueContactPoint ContactPoint
    | TaskInputValueCount Count
    | TaskInputValueDistance Distance
    | TaskInputValueDuration Duration
    | TaskInputValueHumanName HumanName
    | TaskInputValueIdentifier Identifier
    | TaskInputValueMoney Money
    | TaskInputValuePeriod Period
    | TaskInputValueQuantity Quantity
    | TaskInputValueRange Range
    | TaskInputValueRatio Ratio
    | TaskInputValueReference Reference
    | TaskInputValueSampledData SampledData
    | TaskInputValueSignature Signature
    | TaskInputValueTiming Timing
    | TaskInputValueContactDetail ContactDetail
    | TaskInputValueContributor Contributor
    | TaskInputValueDataRequirement DataRequirement
    | TaskInputValueExpression Expression
    | TaskInputValueParameterDefinition ParameterDefinition
    | TaskInputValueRelatedArtifact RelatedArtifact
    | TaskInputValueTriggerDefinition TriggerDefinition
    | TaskInputValueUsageContext UsageContext
    | TaskInputValueDosage Dosage
    deriving (Eq, Show)

data TaskInput = TaskInput {
    taskInputAttrId :: Maybe Text
  , taskInputExtension :: [Extension]
  , taskInputModifierExtension :: [Extension]
  , taskInputType :: CodeableConcept
  , taskInputValueBase64Binary :: Base64Binary
  , taskInputValueBoolean :: Boolean
  , taskInputValueCanonical :: Canonical
  , taskInputValueCode :: Code
  , taskInputValueDate :: Date
  , taskInputValueDateTime :: DateTime
  , taskInputValueDecimal :: Decimal
  , taskInputValueId :: Id
  , taskInputValueInstant :: Instant
  , taskInputValueInteger :: Integer
  , taskInputValueMarkdown :: Markdown
  , taskInputValueOid :: Oid
  , taskInputValuePositiveInt :: PositiveInt
  , taskInputValueString :: Text
  , taskInputValueTime :: Time
  , taskInputValueUnsignedInt :: UnsignedInt
  , taskInputValueUri :: Uri
  , taskInputValueUrl :: Url
  , taskInputValueUuid :: Uuid
  , taskInputValueAddress :: Address
  , taskInputValueAge :: Age
  , taskInputValueAnnotation :: Annotation
  , taskInputValueAttachment :: Attachment
  , taskInputValueCodeableConcept :: CodeableConcept
  , taskInputValueCoding :: Coding
  , taskInputValueContactPoint :: ContactPoint
  , taskInputValueCount :: Count
  , taskInputValueDistance :: Distance
  , taskInputValueDuration :: Duration
  , taskInputValueHumanName :: HumanName
  , taskInputValueIdentifier :: Identifier
  , taskInputValueMoney :: Money
  , taskInputValuePeriod :: Period
  , taskInputValueQuantity :: Quantity
  , taskInputValueRange :: Range
  , taskInputValueRatio :: Ratio
  , taskInputValueReference :: Reference
  , taskInputValueSampledData :: SampledData
  , taskInputValueSignature :: Signature
  , taskInputValueTiming :: Timing
  , taskInputValueContactDetail :: ContactDetail
  , taskInputValueContributor :: Contributor
  , taskInputValueDataRequirement :: DataRequirement
  , taskInputValueExpression :: Expression
  , taskInputValueParameterDefinition :: ParameterDefinition
  , taskInputValueRelatedArtifact :: RelatedArtifact
  , taskInputValueTriggerDefinition :: TriggerDefinition
  , taskInputValueUsageContext :: UsageContext
  , taskInputValueDosage :: Dosage
  } deriving (Eq, Show)
--

instance ToJSON TaskInput where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (taskInputAttrId p)
    ,  "extension" .= toJSON (taskInputExtension p)
    ,  "modifierExtension" .= toJSON (taskInputModifierExtension p)
    ,  "type" .= toJSON (taskInputType p)
    ,  "valueBase64Binary" .= toJSON (taskInputValueBase64Binary p)
    ,  "valueBoolean" .= toJSON (taskInputValueBoolean p)
    ,  "valueCanonical" .= toJSON (taskInputValueCanonical p)
    ,  "valueCode" .= toJSON (taskInputValueCode p)
    ,  "valueDate" .= toJSON (taskInputValueDate p)
    ,  "valueDateTime" .= toJSON (taskInputValueDateTime p)
    ,  "valueDecimal" .= toJSON (taskInputValueDecimal p)
    ,  "valueId" .= toJSON (taskInputValueId p)
    ,  "valueInstant" .= toJSON (taskInputValueInstant p)
    ,  "valueInteger" .= toJSON (taskInputValueInteger p)
    ,  "valueMarkdown" .= toJSON (taskInputValueMarkdown p)
    ,  "valueOid" .= toJSON (taskInputValueOid p)
    ,  "valuePositiveInt" .= toJSON (taskInputValuePositiveInt p)
    ,  "valueString" .= toJSON (taskInputValueString p)
    ,  "valueTime" .= toJSON (taskInputValueTime p)
    ,  "valueUnsignedInt" .= toJSON (taskInputValueUnsignedInt p)
    ,  "valueUri" .= toJSON (taskInputValueUri p)
    ,  "valueUrl" .= toJSON (taskInputValueUrl p)
    ,  "valueUuid" .= toJSON (taskInputValueUuid p)
    ,  "valueAddress" .= toJSON (taskInputValueAddress p)
    ,  "valueAge" .= toJSON (taskInputValueAge p)
    ,  "valueAnnotation" .= toJSON (taskInputValueAnnotation p)
    ,  "valueAttachment" .= toJSON (taskInputValueAttachment p)
    ,  "valueCodeableConcept" .= toJSON (taskInputValueCodeableConcept p)
    ,  "valueCoding" .= toJSON (taskInputValueCoding p)
    ,  "valueContactPoint" .= toJSON (taskInputValueContactPoint p)
    ,  "valueCount" .= toJSON (taskInputValueCount p)
    ,  "valueDistance" .= toJSON (taskInputValueDistance p)
    ,  "valueDuration" .= toJSON (taskInputValueDuration p)
    ,  "valueHumanName" .= toJSON (taskInputValueHumanName p)
    ,  "valueIdentifier" .= toJSON (taskInputValueIdentifier p)
    ,  "valueMoney" .= toJSON (taskInputValueMoney p)
    ,  "valuePeriod" .= toJSON (taskInputValuePeriod p)
    ,  "valueQuantity" .= toJSON (taskInputValueQuantity p)
    ,  "valueRange" .= toJSON (taskInputValueRange p)
    ,  "valueRatio" .= toJSON (taskInputValueRatio p)
    ,  "valueReference" .= toJSON (taskInputValueReference p)
    ,  "valueSampledData" .= toJSON (taskInputValueSampledData p)
    ,  "valueSignature" .= toJSON (taskInputValueSignature p)
    ,  "valueTiming" .= toJSON (taskInputValueTiming p)
    ,  "valueContactDetail" .= toJSON (taskInputValueContactDetail p)
    ,  "valueContributor" .= toJSON (taskInputValueContributor p)
    ,  "valueDataRequirement" .= toJSON (taskInputValueDataRequirement p)
    ,  "valueExpression" .= toJSON (taskInputValueExpression p)
    ,  "valueParameterDefinition" .= toJSON (taskInputValueParameterDefinition p)
    ,  "valueRelatedArtifact" .= toJSON (taskInputValueRelatedArtifact p)
    ,  "valueTriggerDefinition" .= toJSON (taskInputValueTriggerDefinition p)
    ,  "valueUsageContext" .= toJSON (taskInputValueUsageContext p)
    ,  "valueDosage" .= toJSON (taskInputValueDosage p)
    ]
instance FromJSON TaskInput where
  parseJSON = withObject "TaskInput" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:  "type"
        valueBase64Binary <- o .:  "valueBase64Binary"
        valueBoolean <- o .:  "valueBoolean"
        valueCanonical <- o .:  "valueCanonical"
        valueCode <- o .:  "valueCode"
        valueDate <- o .:  "valueDate"
        valueDateTime <- o .:  "valueDateTime"
        valueDecimal <- o .:  "valueDecimal"
        valueId <- o .:  "valueId"
        valueInstant <- o .:  "valueInstant"
        valueInteger <- o .:  "valueInteger"
        valueMarkdown <- o .:  "valueMarkdown"
        valueOid <- o .:  "valueOid"
        valuePositiveInt <- o .:  "valuePositiveInt"
        valueString <- o .:  "valueString"
        valueTime <- o .:  "valueTime"
        valueUnsignedInt <- o .:  "valueUnsignedInt"
        valueUri <- o .:  "valueUri"
        valueUrl <- o .:  "valueUrl"
        valueUuid <- o .:  "valueUuid"
        valueAddress <- o .:  "valueAddress"
        valueAge <- o .:  "valueAge"
        valueAnnotation <- o .:  "valueAnnotation"
        valueAttachment <- o .:  "valueAttachment"
        valueCodeableConcept <- o .:  "valueCodeableConcept"
        valueCoding <- o .:  "valueCoding"
        valueContactPoint <- o .:  "valueContactPoint"
        valueCount <- o .:  "valueCount"
        valueDistance <- o .:  "valueDistance"
        valueDuration <- o .:  "valueDuration"
        valueHumanName <- o .:  "valueHumanName"
        valueIdentifier <- o .:  "valueIdentifier"
        valueMoney <- o .:  "valueMoney"
        valuePeriod <- o .:  "valuePeriod"
        valueQuantity <- o .:  "valueQuantity"
        valueRange <- o .:  "valueRange"
        valueRatio <- o .:  "valueRatio"
        valueReference <- o .:  "valueReference"
        valueSampledData <- o .:  "valueSampledData"
        valueSignature <- o .:  "valueSignature"
        valueTiming <- o .:  "valueTiming"
        valueContactDetail <- o .:  "valueContactDetail"
        valueContributor <- o .:  "valueContributor"
        valueDataRequirement <- o .:  "valueDataRequirement"
        valueExpression <- o .:  "valueExpression"
        valueParameterDefinition <- o .:  "valueParameterDefinition"
        valueRelatedArtifact <- o .:  "valueRelatedArtifact"
        valueTriggerDefinition <- o .:  "valueTriggerDefinition"
        valueUsageContext <- o .:  "valueUsageContext"
        valueDosage <- o .:  "valueDosage"
        return TaskInput{
            taskInputAttrId = id
          , taskInputExtension = extension
          , taskInputModifierExtension = modifierExtension
          , taskInputType = ty
          , taskInputValueBase64Binary = valueBase64Binary
          , taskInputValueBoolean = valueBoolean
          , taskInputValueCanonical = valueCanonical
          , taskInputValueCode = valueCode
          , taskInputValueDate = valueDate
          , taskInputValueDateTime = valueDateTime
          , taskInputValueDecimal = valueDecimal
          , taskInputValueId = valueId
          , taskInputValueInstant = valueInstant
          , taskInputValueInteger = valueInteger
          , taskInputValueMarkdown = valueMarkdown
          , taskInputValueOid = valueOid
          , taskInputValuePositiveInt = valuePositiveInt
          , taskInputValueString = valueString
          , taskInputValueTime = valueTime
          , taskInputValueUnsignedInt = valueUnsignedInt
          , taskInputValueUri = valueUri
          , taskInputValueUrl = valueUrl
          , taskInputValueUuid = valueUuid
          , taskInputValueAddress = valueAddress
          , taskInputValueAge = valueAge
          , taskInputValueAnnotation = valueAnnotation
          , taskInputValueAttachment = valueAttachment
          , taskInputValueCodeableConcept = valueCodeableConcept
          , taskInputValueCoding = valueCoding
          , taskInputValueContactPoint = valueContactPoint
          , taskInputValueCount = valueCount
          , taskInputValueDistance = valueDistance
          , taskInputValueDuration = valueDuration
          , taskInputValueHumanName = valueHumanName
          , taskInputValueIdentifier = valueIdentifier
          , taskInputValueMoney = valueMoney
          , taskInputValuePeriod = valuePeriod
          , taskInputValueQuantity = valueQuantity
          , taskInputValueRange = valueRange
          , taskInputValueRatio = valueRatio
          , taskInputValueReference = valueReference
          , taskInputValueSampledData = valueSampledData
          , taskInputValueSignature = valueSignature
          , taskInputValueTiming = valueTiming
          , taskInputValueContactDetail = valueContactDetail
          , taskInputValueContributor = valueContributor
          , taskInputValueDataRequirement = valueDataRequirement
          , taskInputValueExpression = valueExpression
          , taskInputValueParameterDefinition = valueParameterDefinition
          , taskInputValueRelatedArtifact = valueRelatedArtifact
          , taskInputValueTriggerDefinition = valueTriggerDefinition
          , taskInputValueUsageContext = valueUsageContext
          , taskInputValueDosage = valueDosage
          }
instance Xmlbf.ToXml TaskInput where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (taskInputAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (taskInputExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (taskInputModifierExtension p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (taskInputType p))
             , Val      "valueBase64Binary" (     toBase64Binary (taskInputValueBase64Binary p))
             , Val      "valueBoolean" (     toBoolean (taskInputValueBoolean p))
             , Val      "valueCanonical" (     toCanonical (taskInputValueCanonical p))
             , Val      "valueCode" (     toCode (taskInputValueCode p))
             , Val      "valueDate" (     toDate (taskInputValueDate p))
             , Val      "valueDateTime" (     toDateTime (taskInputValueDateTime p))
             , Val      "valueDecimal" (     toDecimal (taskInputValueDecimal p))
             , Val      "valueId" (     toId (taskInputValueId p))
             , Val      "valueInstant" (     toInstant (taskInputValueInstant p))
             , Val      "valueInteger" (     toInt (taskInputValueInteger p))
             , Val      "valueMarkdown" (     toMarkdown (taskInputValueMarkdown p))
             , Val      "valueOid" (     toOid (taskInputValueOid p))
             , Val      "valuePositiveInt" (     toPositiveInt (taskInputValuePositiveInt p))
             , Val      "valueString" (     toString (taskInputValueString p))
             , Val      "valueTime" (     toTime (taskInputValueTime p))
             , Val      "valueUnsignedInt" (     toUnsignedInt (taskInputValueUnsignedInt p))
             , Val      "valueUri" (     toUri (taskInputValueUri p))
             , Val      "valueUrl" (     toUrl (taskInputValueUrl p))
             , Val      "valueUuid" (     toUuid (taskInputValueUuid p))
             , Prop     "valueAddress" (HM.empty, Xmlbf.toXml (taskInputValueAddress p))
             , Prop     "valueAge" (HM.empty, Xmlbf.toXml (taskInputValueAge p))
             , Prop     "valueAnnotation" (HM.empty, Xmlbf.toXml (taskInputValueAnnotation p))
             , Prop     "valueAttachment" (HM.empty, Xmlbf.toXml (taskInputValueAttachment p))
             , Prop     "valueCodeableConcept" (HM.empty, Xmlbf.toXml (taskInputValueCodeableConcept p))
             , Prop     "valueCoding" (HM.empty, Xmlbf.toXml (taskInputValueCoding p))
             , Prop     "valueContactPoint" (HM.empty, Xmlbf.toXml (taskInputValueContactPoint p))
             , Prop     "valueCount" (HM.empty, Xmlbf.toXml (taskInputValueCount p))
             , Prop     "valueDistance" (HM.empty, Xmlbf.toXml (taskInputValueDistance p))
             , Prop     "valueDuration" (HM.empty, Xmlbf.toXml (taskInputValueDuration p))
             , Prop     "valueHumanName" (HM.empty, Xmlbf.toXml (taskInputValueHumanName p))
             , Prop     "valueIdentifier" (HM.empty, Xmlbf.toXml (taskInputValueIdentifier p))
             , Prop     "valueMoney" (HM.empty, Xmlbf.toXml (taskInputValueMoney p))
             , Prop     "valuePeriod" (HM.empty, Xmlbf.toXml (taskInputValuePeriod p))
             , Prop     "valueQuantity" (HM.empty, Xmlbf.toXml (taskInputValueQuantity p))
             , Prop     "valueRange" (HM.empty, Xmlbf.toXml (taskInputValueRange p))
             , Prop     "valueRatio" (HM.empty, Xmlbf.toXml (taskInputValueRatio p))
             , Prop     "valueReference" (HM.empty, Xmlbf.toXml (taskInputValueReference p))
             , Prop     "valueSampledData" (HM.empty, Xmlbf.toXml (taskInputValueSampledData p))
             , Prop     "valueSignature" (HM.empty, Xmlbf.toXml (taskInputValueSignature p))
             , Prop     "valueTiming" (HM.empty, Xmlbf.toXml (taskInputValueTiming p))
             , Prop     "valueContactDetail" (HM.empty, Xmlbf.toXml (taskInputValueContactDetail p))
             , Prop     "valueContributor" (HM.empty, Xmlbf.toXml (taskInputValueContributor p))
             , Prop     "valueDataRequirement" (HM.empty, Xmlbf.toXml (taskInputValueDataRequirement p))
             , Prop     "valueExpression" (HM.empty, Xmlbf.toXml (taskInputValueExpression p))
             , Prop     "valueParameterDefinition" (HM.empty, Xmlbf.toXml (taskInputValueParameterDefinition p))
             , Prop     "valueRelatedArtifact" (HM.empty, Xmlbf.toXml (taskInputValueRelatedArtifact p))
             , Prop     "valueTriggerDefinition" (HM.empty, Xmlbf.toXml (taskInputValueTriggerDefinition p))
             , Prop     "valueUsageContext" (HM.empty, Xmlbf.toXml (taskInputValueUsageContext p))
             , Prop     "valueDosage" (HM.empty, Xmlbf.toXml (taskInputValueDosage p))
             ]
instance Xmlbf.FromXml TaskInput where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" Xmlbf.fromXml
    valueBase64Binary <-            Xmlbf.pElement "valueBase64Binary" (Xmlbf.pAttr "value")
    valueBoolean <-            Xmlbf.pElement "valueBoolean" (Xmlbf.pAttr "value")
    valueCanonical <-            Xmlbf.pElement "valueCanonical" (Xmlbf.pAttr "value")
    valueCode <-            Xmlbf.pElement "valueCode" (Xmlbf.pAttr "value")
    valueDate <-            Xmlbf.pElement "valueDate" (Xmlbf.pAttr "value")
    valueDateTime <-            Xmlbf.pElement "valueDateTime" (Xmlbf.pAttr "value")
    valueDecimal <-            Xmlbf.pElement "valueDecimal" (Xmlbf.pAttr "value")
    valueId <-            Xmlbf.pElement "valueId" (Xmlbf.pAttr "value")
    valueInstant <-            Xmlbf.pElement "valueInstant" (Xmlbf.pAttr "value")
    valueInteger <-            Xmlbf.pElement "valueInteger" (Xmlbf.pAttr "value")
    valueMarkdown <-            Xmlbf.pElement "valueMarkdown" (Xmlbf.pAttr "value")
    valueOid <-            Xmlbf.pElement "valueOid" (Xmlbf.pAttr "value")
    valuePositiveInt <-            Xmlbf.pElement "valuePositiveInt" (Xmlbf.pAttr "value")
    valueString <-            Xmlbf.pElement "valueString" (Xmlbf.pAttr "value")
    valueTime <-            Xmlbf.pElement "valueTime" (Xmlbf.pAttr "value")
    valueUnsignedInt <-            Xmlbf.pElement "valueUnsignedInt" (Xmlbf.pAttr "value")
    valueUri <-            Xmlbf.pElement "valueUri" (Xmlbf.pAttr "value")
    valueUrl <-            Xmlbf.pElement "valueUrl" (Xmlbf.pAttr "value")
    valueUuid <-            Xmlbf.pElement "valueUuid" (Xmlbf.pAttr "value")
    valueAddress <-            Xmlbf.pElement "valueAddress" Xmlbf.fromXml
    valueAge <-            Xmlbf.pElement "valueAge" Xmlbf.fromXml
    valueAnnotation <-            Xmlbf.pElement "valueAnnotation" Xmlbf.fromXml
    valueAttachment <-            Xmlbf.pElement "valueAttachment" Xmlbf.fromXml
    valueCodeableConcept <-            Xmlbf.pElement "valueCodeableConcept" Xmlbf.fromXml
    valueCoding <-            Xmlbf.pElement "valueCoding" Xmlbf.fromXml
    valueContactPoint <-            Xmlbf.pElement "valueContactPoint" Xmlbf.fromXml
    valueCount <-            Xmlbf.pElement "valueCount" Xmlbf.fromXml
    valueDistance <-            Xmlbf.pElement "valueDistance" Xmlbf.fromXml
    valueDuration <-            Xmlbf.pElement "valueDuration" Xmlbf.fromXml
    valueHumanName <-            Xmlbf.pElement "valueHumanName" Xmlbf.fromXml
    valueIdentifier <-            Xmlbf.pElement "valueIdentifier" Xmlbf.fromXml
    valueMoney <-            Xmlbf.pElement "valueMoney" Xmlbf.fromXml
    valuePeriod <-            Xmlbf.pElement "valuePeriod" Xmlbf.fromXml
    valueQuantity <-            Xmlbf.pElement "valueQuantity" Xmlbf.fromXml
    valueRange <-            Xmlbf.pElement "valueRange" Xmlbf.fromXml
    valueRatio <-            Xmlbf.pElement "valueRatio" Xmlbf.fromXml
    valueReference <-            Xmlbf.pElement "valueReference" Xmlbf.fromXml
    valueSampledData <-            Xmlbf.pElement "valueSampledData" Xmlbf.fromXml
    valueSignature <-            Xmlbf.pElement "valueSignature" Xmlbf.fromXml
    valueTiming <-            Xmlbf.pElement "valueTiming" Xmlbf.fromXml
    valueContactDetail <-            Xmlbf.pElement "valueContactDetail" Xmlbf.fromXml
    valueContributor <-            Xmlbf.pElement "valueContributor" Xmlbf.fromXml
    valueDataRequirement <-            Xmlbf.pElement "valueDataRequirement" Xmlbf.fromXml
    valueExpression <-            Xmlbf.pElement "valueExpression" Xmlbf.fromXml
    valueParameterDefinition <-            Xmlbf.pElement "valueParameterDefinition" Xmlbf.fromXml
    valueRelatedArtifact <-            Xmlbf.pElement "valueRelatedArtifact" Xmlbf.fromXml
    valueTriggerDefinition <-            Xmlbf.pElement "valueTriggerDefinition" Xmlbf.fromXml
    valueUsageContext <-            Xmlbf.pElement "valueUsageContext" Xmlbf.fromXml
    valueDosage <-            Xmlbf.pElement "valueDosage" Xmlbf.fromXml
    return TaskInput {
            taskInputAttrId = id
          , taskInputExtension = extension
          , taskInputModifierExtension = modifierExtension
          , taskInputType = ty
          , taskInputValueBase64Binary =      fromBase64Binary valueBase64Binary
          , taskInputValueBoolean =      fromBoolean valueBoolean
          , taskInputValueCanonical =      fromCanonical valueCanonical
          , taskInputValueCode =      fromCode valueCode
          , taskInputValueDate =      fromDate valueDate
          , taskInputValueDateTime =      fromDateTime valueDateTime
          , taskInputValueDecimal =      fromDecimal valueDecimal
          , taskInputValueId =      fromId valueId
          , taskInputValueInstant =      fromInstant valueInstant
          , taskInputValueInteger =      fromInt valueInteger
          , taskInputValueMarkdown =      fromMarkdown valueMarkdown
          , taskInputValueOid =      fromOid valueOid
          , taskInputValuePositiveInt =      fromPositiveInt valuePositiveInt
          , taskInputValueString =      fromString valueString
          , taskInputValueTime =      fromTime valueTime
          , taskInputValueUnsignedInt =      fromUnsignedInt valueUnsignedInt
          , taskInputValueUri =      fromUri valueUri
          , taskInputValueUrl =      fromUrl valueUrl
          , taskInputValueUuid =      fromUuid valueUuid
          , taskInputValueAddress = valueAddress
          , taskInputValueAge = valueAge
          , taskInputValueAnnotation = valueAnnotation
          , taskInputValueAttachment = valueAttachment
          , taskInputValueCodeableConcept = valueCodeableConcept
          , taskInputValueCoding = valueCoding
          , taskInputValueContactPoint = valueContactPoint
          , taskInputValueCount = valueCount
          , taskInputValueDistance = valueDistance
          , taskInputValueDuration = valueDuration
          , taskInputValueHumanName = valueHumanName
          , taskInputValueIdentifier = valueIdentifier
          , taskInputValueMoney = valueMoney
          , taskInputValuePeriod = valuePeriod
          , taskInputValueQuantity = valueQuantity
          , taskInputValueRange = valueRange
          , taskInputValueRatio = valueRatio
          , taskInputValueReference = valueReference
          , taskInputValueSampledData = valueSampledData
          , taskInputValueSignature = valueSignature
          , taskInputValueTiming = valueTiming
          , taskInputValueContactDetail = valueContactDetail
          , taskInputValueContributor = valueContributor
          , taskInputValueDataRequirement = valueDataRequirement
          , taskInputValueExpression = valueExpression
          , taskInputValueParameterDefinition = valueParameterDefinition
          , taskInputValueRelatedArtifact = valueRelatedArtifact
          , taskInputValueTriggerDefinition = valueTriggerDefinition
          , taskInputValueUsageContext = valueUsageContext
          , taskInputValueDosage = valueDosage
          }



data TaskRestriction = TaskRestriction {
    taskRestrictionAttrId :: Maybe Text
  , taskRestrictionExtension :: [Extension]
  , taskRestrictionModifierExtension :: [Extension]
  , taskRestrictionRepetitions :: Maybe PositiveInt
  , taskRestrictionPeriod :: Maybe Period
  , taskRestrictionRecipient :: [Reference]
  } deriving (Eq, Show)
--

instance ToJSON TaskRestriction where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (taskRestrictionAttrId p)
    ,  "extension" .= toJSON (taskRestrictionExtension p)
    ,  "modifierExtension" .= toJSON (taskRestrictionModifierExtension p)
    ,  "repetitions" .= toJSON (taskRestrictionRepetitions p)
    ,  "period" .= toJSON (taskRestrictionPeriod p)
    ,  "recipient" .= toJSON (taskRestrictionRecipient p)
    ]
instance FromJSON TaskRestriction where
  parseJSON = withObject "TaskRestriction" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        repetitions <- o .:? "repetitions"
        period <- o .:? "period"
        recipient <- o .:? "recipient" .!= []
        return TaskRestriction{
            taskRestrictionAttrId = id
          , taskRestrictionExtension = extension
          , taskRestrictionModifierExtension = modifierExtension
          , taskRestrictionRepetitions = repetitions
          , taskRestrictionPeriod = period
          , taskRestrictionRecipient = recipient
          }
instance Xmlbf.ToXml TaskRestriction where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (taskRestrictionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (taskRestrictionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (taskRestrictionModifierExtension p))
             , OptVal   "repetitions" (fmap toPositiveInt (taskRestrictionRepetitions p))
             , OptProp  "period" (fmap Xmlbf.toXml (taskRestrictionPeriod p))
             , PropList "recipient" (fmap Xmlbf.toXml (taskRestrictionRecipient p))
             ]
instance Xmlbf.FromXml TaskRestriction where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    repetitions <- optional $ Xmlbf.pElement "repetitions" (Xmlbf.pAttr "value")
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    recipient <- many     $ Xmlbf.pElement "recipient" Xmlbf.fromXml
    return TaskRestriction {
            taskRestrictionAttrId = id
          , taskRestrictionExtension = extension
          , taskRestrictionModifierExtension = modifierExtension
          , taskRestrictionRepetitions = fmap fromPositiveInt repetitions
          , taskRestrictionPeriod = period
          , taskRestrictionRecipient = recipient
          }




