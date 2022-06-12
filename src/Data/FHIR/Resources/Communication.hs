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
-- FHIR 4.0.0 Communication
--

module Data.FHIR.Resources.Communication where

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

data CommunicationStatus
    = CSPreparation
    | CSInProgress
    | CSNotDone
    | CSOnHold
    | CSStopped
    | CSCompleted
    | CSEnteredInError
    | CSUnknown
  deriving (Eq, Show)

instance ToJSON CommunicationStatus where
    toJSON CSPreparation = String "preparation"
    toJSON CSInProgress = String "in-progress"
    toJSON CSNotDone = String "not-done"
    toJSON CSOnHold = String "on-hold"
    toJSON CSStopped = String "stopped"
    toJSON CSCompleted = String "completed"
    toJSON CSEnteredInError = String "entered-in-error"
    toJSON CSUnknown = String "unknown"
instance FromJSON CommunicationStatus where
    parseJSON "preparation" = return CSPreparation
    parseJSON "in-progress" = return CSInProgress
    parseJSON "not-done" = return CSNotDone
    parseJSON "on-hold" = return CSOnHold
    parseJSON "stopped" = return CSStopped
    parseJSON "completed" = return CSCompleted
    parseJSON "entered-in-error" = return CSEnteredInError
    parseJSON "unknown" = return CSUnknown

toCommunicationStatus CSPreparation = "preparation"
toCommunicationStatus CSInProgress = "in-progress"
toCommunicationStatus CSNotDone = "not-done"
toCommunicationStatus CSOnHold = "on-hold"
toCommunicationStatus CSStopped = "stopped"
toCommunicationStatus CSCompleted = "completed"
toCommunicationStatus CSEnteredInError = "entered-in-error"
toCommunicationStatus CSUnknown = "unknown"
fromCommunicationStatus "preparation" = CSPreparation
fromCommunicationStatus "in-progress" = CSInProgress
fromCommunicationStatus "not-done" = CSNotDone
fromCommunicationStatus "on-hold" = CSOnHold
fromCommunicationStatus "stopped" = CSStopped
fromCommunicationStatus "completed" = CSCompleted
fromCommunicationStatus "entered-in-error" = CSEnteredInError
fromCommunicationStatus "unknown" = CSUnknown


data CommunicationPriority
    = CPRoutine
    | CPUrgent
    | CPAsap
    | CPStat
  deriving (Eq, Show)

instance ToJSON CommunicationPriority where
    toJSON CPRoutine = String "routine"
    toJSON CPUrgent = String "urgent"
    toJSON CPAsap = String "asap"
    toJSON CPStat = String "stat"
instance FromJSON CommunicationPriority where
    parseJSON "routine" = return CPRoutine
    parseJSON "urgent" = return CPUrgent
    parseJSON "asap" = return CPAsap
    parseJSON "stat" = return CPStat

toCommunicationPriority CPRoutine = "routine"
toCommunicationPriority CPUrgent = "urgent"
toCommunicationPriority CPAsap = "asap"
toCommunicationPriority CPStat = "stat"
fromCommunicationPriority "routine" = CPRoutine
fromCommunicationPriority "urgent" = CPUrgent
fromCommunicationPriority "asap" = CPAsap
fromCommunicationPriority "stat" = CPStat


data Communication = Communication {
    communicationId :: Maybe Id
  , communicationMeta :: Maybe Meta
  , communicationImplicitRules :: Maybe Uri
  , communicationLanguage :: Maybe Language
  , communicationText :: Maybe Narrative
--    communicationContained :: [ResourceContainer]
  , communicationExtension :: [Extension]
  , communicationModifierExtension :: [Extension]
  , communicationIdentifier :: [Identifier]
  , communicationInstantiatesCanonical :: [Canonical]
  , communicationInstantiatesUri :: [Uri]
  , communicationBasedOn :: [Reference]
  , communicationPartOf :: [Reference]
  , communicationInResponseTo :: [Reference]
  , communicationStatus :: CommunicationStatus
  , communicationStatusReason :: Maybe CodeableConcept
  , communicationCategory :: [CodeableConcept]
  , communicationPriority :: Maybe CommunicationPriority
  , communicationMedium :: [CodeableConcept]
  , communicationSubject :: Maybe Reference
  , communicationTopic :: Maybe CodeableConcept
  , communicationAbout :: [Reference]
  , communicationEncounter :: Maybe Reference
  , communicationSent :: Maybe DateTime
  , communicationReceived :: Maybe DateTime
  , communicationRecipient :: [Reference]
  , communicationSender :: Maybe Reference
  , communicationReasonCode :: [CodeableConcept]
  , communicationReasonReference :: [Reference]
  , communicationPayload :: [CommunicationPayload]
  , communicationNote :: [Annotation]
  }
  deriving (Eq, Show)
--

instance ToJSON Communication where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Communication")
    ,  "id" .= toJSON (communicationId p)
    ,  "meta" .= toJSON (communicationMeta p)
    ,  "implicitRules" .= toJSON (communicationImplicitRules p)
    ,  "language" .= toJSON (communicationLanguage p)
    ,  "text" .= toJSON (communicationText p)
--    , "contained" .= toJSON (communicationContained p)
    ,  "extension" .= toJSON (communicationExtension p)
    ,  "modifierExtension" .= toJSON (communicationModifierExtension p)
    ,  "identifier" .= toJSON (communicationIdentifier p)
    ,  "instantiatesCanonical" .= toJSON (communicationInstantiatesCanonical p)
    ,  "instantiatesUri" .= toJSON (communicationInstantiatesUri p)
    ,  "basedOn" .= toJSON (communicationBasedOn p)
    ,  "partOf" .= toJSON (communicationPartOf p)
    ,  "inResponseTo" .= toJSON (communicationInResponseTo p)
    ,  "status" .= toJSON (communicationStatus p)
    ,  "statusReason" .= toJSON (communicationStatusReason p)
    ,  "category" .= toJSON (communicationCategory p)
    ,  "priority" .= toJSON (communicationPriority p)
    ,  "medium" .= toJSON (communicationMedium p)
    ,  "subject" .= toJSON (communicationSubject p)
    ,  "topic" .= toJSON (communicationTopic p)
    ,  "about" .= toJSON (communicationAbout p)
    ,  "encounter" .= toJSON (communicationEncounter p)
    ,  "sent" .= toJSON (communicationSent p)
    ,  "received" .= toJSON (communicationReceived p)
    ,  "recipient" .= toJSON (communicationRecipient p)
    ,  "sender" .= toJSON (communicationSender p)
    ,  "reasonCode" .= toJSON (communicationReasonCode p)
    ,  "reasonReference" .= toJSON (communicationReasonReference p)
    ,  "payload" .= toJSON (communicationPayload p)
    ,  "note" .= toJSON (communicationNote p)
    ]
instance FromJSON Communication where
  parseJSON = withObject "Communication" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Communication" -> do
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
        partOf <- o .:? "partOf" .!= []
        inResponseTo <- o .:? "inResponseTo" .!= []
        status <- o .:  "status"
        statusReason <- o .:? "statusReason"
        category <- o .:? "category" .!= []
        priority <- o .:? "priority"
        medium <- o .:? "medium" .!= []
        subject <- o .:? "subject"
        topic <- o .:? "topic"
        about <- o .:? "about" .!= []
        encounter <- o .:? "encounter"
        sent <- o .:? "sent"
        received <- o .:? "received"
        recipient <- o .:? "recipient" .!= []
        sender <- o .:? "sender"
        reasonCode <- o .:? "reasonCode" .!= []
        reasonReference <- o .:? "reasonReference" .!= []
        payload <- o .:? "payload" .!= []
        note <- o .:? "note" .!= []
        return Communication{
            communicationId = id
          , communicationMeta = meta
          , communicationImplicitRules = implicitRules
          , communicationLanguage = language
          , communicationText = text
--          , communicationContained = contained
          , communicationExtension = extension
          , communicationModifierExtension = modifierExtension
          , communicationIdentifier = identifier
          , communicationInstantiatesCanonical = instantiatesCanonical
          , communicationInstantiatesUri = instantiatesUri
          , communicationBasedOn = basedOn
          , communicationPartOf = partOf
          , communicationInResponseTo = inResponseTo
          , communicationStatus = status
          , communicationStatusReason = statusReason
          , communicationCategory = category
          , communicationPriority = priority
          , communicationMedium = medium
          , communicationSubject = subject
          , communicationTopic = topic
          , communicationAbout = about
          , communicationEncounter = encounter
          , communicationSent = sent
          , communicationReceived = received
          , communicationRecipient = recipient
          , communicationSender = sender
          , communicationReasonCode = reasonCode
          , communicationReasonReference = reasonReference
          , communicationPayload = payload
          , communicationNote = note
          }
      _ -> fail "not a Communication"
instance Xmlbf.ToXml Communication where
  toXml p = Xmlbf.element "Communication" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (communicationId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (communicationMeta p))
             , OptVal   "implicitRules" (fmap toUri (communicationImplicitRules p))
             , OptVal   "language" (fmap toLanguage (communicationLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (communicationText p))
--             , PropList "contained" (fmap Xmlbf.toXml (communicationContained p))
             , PropList "extension" (fmap Xmlbf.toXml (communicationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (communicationModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (communicationIdentifier p))
             , ValList  "instantiatesCanonical" (fmap toCanonical (communicationInstantiatesCanonical p))
             , ValList  "instantiatesUri" (fmap toUri (communicationInstantiatesUri p))
             , PropList "basedOn" (fmap Xmlbf.toXml (communicationBasedOn p))
             , PropList "partOf" (fmap Xmlbf.toXml (communicationPartOf p))
             , PropList "inResponseTo" (fmap Xmlbf.toXml (communicationInResponseTo p))
             , Val      "status" (     toCommunicationStatus (communicationStatus p))
             , OptProp  "statusReason" (fmap Xmlbf.toXml (communicationStatusReason p))
             , PropList "category" (fmap Xmlbf.toXml (communicationCategory p))
             , OptVal   "priority" (fmap toCommunicationPriority (communicationPriority p))
             , PropList "medium" (fmap Xmlbf.toXml (communicationMedium p))
             , OptProp  "subject" (fmap Xmlbf.toXml (communicationSubject p))
             , OptProp  "topic" (fmap Xmlbf.toXml (communicationTopic p))
             , PropList "about" (fmap Xmlbf.toXml (communicationAbout p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (communicationEncounter p))
             , OptVal   "sent" (fmap toDateTime (communicationSent p))
             , OptVal   "received" (fmap toDateTime (communicationReceived p))
             , PropList "recipient" (fmap Xmlbf.toXml (communicationRecipient p))
             , OptProp  "sender" (fmap Xmlbf.toXml (communicationSender p))
             , PropList "reasonCode" (fmap Xmlbf.toXml (communicationReasonCode p))
             , PropList "reasonReference" (fmap Xmlbf.toXml (communicationReasonReference p))
             , PropList "payload" (fmap Xmlbf.toXml (communicationPayload p))
             , PropList "note" (fmap Xmlbf.toXml (communicationNote p))
             ]
instance Xmlbf.FromXml Communication where
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
    partOf <- many     $ Xmlbf.pElement "partOf" Xmlbf.fromXml
    inResponseTo <- many     $ Xmlbf.pElement "inResponseTo" Xmlbf.fromXml
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    statusReason <- optional $ Xmlbf.pElement "statusReason" Xmlbf.fromXml
    category <- many     $ Xmlbf.pElement "category" Xmlbf.fromXml
    priority <- optional $ Xmlbf.pElement "priority" (Xmlbf.pAttr "value")
    medium <- many     $ Xmlbf.pElement "medium" Xmlbf.fromXml
    subject <- optional $ Xmlbf.pElement "subject" Xmlbf.fromXml
    topic <- optional $ Xmlbf.pElement "topic" Xmlbf.fromXml
    about <- many     $ Xmlbf.pElement "about" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    sent <- optional $ Xmlbf.pElement "sent" (Xmlbf.pAttr "value")
    received <- optional $ Xmlbf.pElement "received" (Xmlbf.pAttr "value")
    recipient <- many     $ Xmlbf.pElement "recipient" Xmlbf.fromXml
    sender <- optional $ Xmlbf.pElement "sender" Xmlbf.fromXml
    reasonCode <- many     $ Xmlbf.pElement "reasonCode" Xmlbf.fromXml
    reasonReference <- many     $ Xmlbf.pElement "reasonReference" Xmlbf.fromXml
    payload <- many     $ Xmlbf.pElement "payload" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    return Communication {
            communicationId = fmap fromId id
          , communicationMeta = meta
          , communicationImplicitRules = fmap fromUri implicitRules
          , communicationLanguage = fmap fromLanguage language
          , communicationText = text
--          , communicationContained = contained
          , communicationExtension = extension
          , communicationModifierExtension = modifierExtension
          , communicationIdentifier = identifier
          , communicationInstantiatesCanonical = fmap fromCanonical instantiatesCanonical
          , communicationInstantiatesUri = fmap fromUri instantiatesUri
          , communicationBasedOn = basedOn
          , communicationPartOf = partOf
          , communicationInResponseTo = inResponseTo
          , communicationStatus =      fromCommunicationStatus status
          , communicationStatusReason = statusReason
          , communicationCategory = category
          , communicationPriority = fmap fromCommunicationPriority priority
          , communicationMedium = medium
          , communicationSubject = subject
          , communicationTopic = topic
          , communicationAbout = about
          , communicationEncounter = encounter
          , communicationSent = fmap fromDateTime sent
          , communicationReceived = fmap fromDateTime received
          , communicationRecipient = recipient
          , communicationSender = sender
          , communicationReasonCode = reasonCode
          , communicationReasonReference = reasonReference
          , communicationPayload = payload
          , communicationNote = note
          }



data CommunicationPayloadContent
    = CommunicationPayloadContentString Text
    | CommunicationPayloadContentAttachment Attachment
    | CommunicationPayloadContentReference Reference
    deriving (Eq, Show)

data CommunicationPayload = CommunicationPayload {
    communicationPayloadAttrId :: Maybe Text
  , communicationPayloadExtension :: [Extension]
  , communicationPayloadModifierExtension :: [Extension]
  , communicationPayloadContentString :: Text
  , communicationPayloadContentAttachment :: Attachment
  , communicationPayloadContentReference :: Reference
  }
  deriving (Eq, Show)
--

instance ToJSON CommunicationPayload where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (communicationPayloadAttrId p)
    ,  "extension" .= toJSON (communicationPayloadExtension p)
    ,  "modifierExtension" .= toJSON (communicationPayloadModifierExtension p)
    ,  "contentString" .= toJSON (communicationPayloadContentString p)
    ,  "contentAttachment" .= toJSON (communicationPayloadContentAttachment p)
    ,  "contentReference" .= toJSON (communicationPayloadContentReference p)
    ]
instance FromJSON CommunicationPayload where
  parseJSON = withObject "CommunicationPayload" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        contentString <- o .:  "contentString"
        contentAttachment <- o .:  "contentAttachment"
        contentReference <- o .:  "contentReference"
        return CommunicationPayload{
            communicationPayloadAttrId = id
          , communicationPayloadExtension = extension
          , communicationPayloadModifierExtension = modifierExtension
          , communicationPayloadContentString = contentString
          , communicationPayloadContentAttachment = contentAttachment
          , communicationPayloadContentReference = contentReference
          }
instance Xmlbf.ToXml CommunicationPayload where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (communicationPayloadAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (communicationPayloadExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (communicationPayloadModifierExtension p))
             , Val      "contentString" (     toString (communicationPayloadContentString p))
             , Prop     "contentAttachment" (HM.empty, Xmlbf.toXml (communicationPayloadContentAttachment p))
             , Prop     "contentReference" (HM.empty, Xmlbf.toXml (communicationPayloadContentReference p))
             ]
instance Xmlbf.FromXml CommunicationPayload where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    contentString <-            Xmlbf.pElement "contentString" (Xmlbf.pAttr "value")
    contentAttachment <-            Xmlbf.pElement "contentAttachment" Xmlbf.fromXml
    contentReference <-            Xmlbf.pElement "contentReference" Xmlbf.fromXml
    return CommunicationPayload {
            communicationPayloadAttrId = id
          , communicationPayloadExtension = extension
          , communicationPayloadModifierExtension = modifierExtension
          , communicationPayloadContentString =      fromString contentString
          , communicationPayloadContentAttachment = contentAttachment
          , communicationPayloadContentReference = contentReference
          }



data CommunicationRequestStatus
    = CRSDraft
    | CRSActive
    | CRSOnHold
    | CRSRevoked
    | CRSCompleted
    | CRSEnteredInError
    | CRSUnknown
  deriving (Eq, Show)

instance ToJSON CommunicationRequestStatus where
    toJSON CRSDraft = String "draft"
    toJSON CRSActive = String "active"
    toJSON CRSOnHold = String "on-hold"
    toJSON CRSRevoked = String "revoked"
    toJSON CRSCompleted = String "completed"
    toJSON CRSEnteredInError = String "entered-in-error"
    toJSON CRSUnknown = String "unknown"
instance FromJSON CommunicationRequestStatus where
    parseJSON "draft" = return CRSDraft
    parseJSON "active" = return CRSActive
    parseJSON "on-hold" = return CRSOnHold
    parseJSON "revoked" = return CRSRevoked
    parseJSON "completed" = return CRSCompleted
    parseJSON "entered-in-error" = return CRSEnteredInError
    parseJSON "unknown" = return CRSUnknown

toCommunicationRequestStatus CRSDraft = "draft"
toCommunicationRequestStatus CRSActive = "active"
toCommunicationRequestStatus CRSOnHold = "on-hold"
toCommunicationRequestStatus CRSRevoked = "revoked"
toCommunicationRequestStatus CRSCompleted = "completed"
toCommunicationRequestStatus CRSEnteredInError = "entered-in-error"
toCommunicationRequestStatus CRSUnknown = "unknown"
fromCommunicationRequestStatus "draft" = CRSDraft
fromCommunicationRequestStatus "active" = CRSActive
fromCommunicationRequestStatus "on-hold" = CRSOnHold
fromCommunicationRequestStatus "revoked" = CRSRevoked
fromCommunicationRequestStatus "completed" = CRSCompleted
fromCommunicationRequestStatus "entered-in-error" = CRSEnteredInError
fromCommunicationRequestStatus "unknown" = CRSUnknown


data CommunicationRequestPriority
    = CRPRoutine
    | CRPUrgent
    | CRPAsap
    | CRPStat
  deriving (Eq, Show)

instance ToJSON CommunicationRequestPriority where
    toJSON CRPRoutine = String "routine"
    toJSON CRPUrgent = String "urgent"
    toJSON CRPAsap = String "asap"
    toJSON CRPStat = String "stat"
instance FromJSON CommunicationRequestPriority where
    parseJSON "routine" = return CRPRoutine
    parseJSON "urgent" = return CRPUrgent
    parseJSON "asap" = return CRPAsap
    parseJSON "stat" = return CRPStat

toCommunicationRequestPriority CRPRoutine = "routine"
toCommunicationRequestPriority CRPUrgent = "urgent"
toCommunicationRequestPriority CRPAsap = "asap"
toCommunicationRequestPriority CRPStat = "stat"
fromCommunicationRequestPriority "routine" = CRPRoutine
fromCommunicationRequestPriority "urgent" = CRPUrgent
fromCommunicationRequestPriority "asap" = CRPAsap
fromCommunicationRequestPriority "stat" = CRPStat


data CommunicationRequestOccurrence
    = CommunicationRequestOccurrenceDateTime DateTime
    | CommunicationRequestOccurrencePeriod Period
    deriving (Eq, Show)

data CommunicationRequest = CommunicationRequest {
    communicationRequestId :: Maybe Id
  , communicationRequestMeta :: Maybe Meta
  , communicationRequestImplicitRules :: Maybe Uri
  , communicationRequestLanguage :: Maybe Language
  , communicationRequestText :: Maybe Narrative
--    communicationRequestContained :: [ResourceContainer]
  , communicationRequestExtension :: [Extension]
  , communicationRequestModifierExtension :: [Extension]
  , communicationRequestIdentifier :: [Identifier]
  , communicationRequestBasedOn :: [Reference]
  , communicationRequestReplaces :: [Reference]
  , communicationRequestGroupIdentifier :: Maybe Identifier
  , communicationRequestStatus :: CommunicationRequestStatus
  , communicationRequestStatusReason :: Maybe CodeableConcept
  , communicationRequestCategory :: [CodeableConcept]
  , communicationRequestPriority :: Maybe CommunicationRequestPriority
  , communicationRequestDoNotPerform :: Maybe Boolean
  , communicationRequestMedium :: [CodeableConcept]
  , communicationRequestSubject :: Maybe Reference
  , communicationRequestAbout :: [Reference]
  , communicationRequestEncounter :: Maybe Reference
  , communicationRequestPayload :: [CommunicationRequestPayload]
  , communicationRequestOccurrenceDateTime :: Maybe DateTime
  , communicationRequestOccurrencePeriod :: Maybe Period
  , communicationRequestAuthoredOn :: Maybe DateTime
  , communicationRequestRequester :: Maybe Reference
  , communicationRequestRecipient :: [Reference]
  , communicationRequestSender :: Maybe Reference
  , communicationRequestReasonCode :: [CodeableConcept]
  , communicationRequestReasonReference :: [Reference]
  , communicationRequestNote :: [Annotation]
  }
  deriving (Eq, Show)
--

instance ToJSON CommunicationRequest where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "CommunicationRequest")
    ,  "id" .= toJSON (communicationRequestId p)
    ,  "meta" .= toJSON (communicationRequestMeta p)
    ,  "implicitRules" .= toJSON (communicationRequestImplicitRules p)
    ,  "language" .= toJSON (communicationRequestLanguage p)
    ,  "text" .= toJSON (communicationRequestText p)
--    , "contained" .= toJSON (communicationRequestContained p)
    ,  "extension" .= toJSON (communicationRequestExtension p)
    ,  "modifierExtension" .= toJSON (communicationRequestModifierExtension p)
    ,  "identifier" .= toJSON (communicationRequestIdentifier p)
    ,  "basedOn" .= toJSON (communicationRequestBasedOn p)
    ,  "replaces" .= toJSON (communicationRequestReplaces p)
    ,  "groupIdentifier" .= toJSON (communicationRequestGroupIdentifier p)
    ,  "status" .= toJSON (communicationRequestStatus p)
    ,  "statusReason" .= toJSON (communicationRequestStatusReason p)
    ,  "category" .= toJSON (communicationRequestCategory p)
    ,  "priority" .= toJSON (communicationRequestPriority p)
    ,  "doNotPerform" .= toJSON (communicationRequestDoNotPerform p)
    ,  "medium" .= toJSON (communicationRequestMedium p)
    ,  "subject" .= toJSON (communicationRequestSubject p)
    ,  "about" .= toJSON (communicationRequestAbout p)
    ,  "encounter" .= toJSON (communicationRequestEncounter p)
    ,  "payload" .= toJSON (communicationRequestPayload p)
    ,  "occurrenceDateTime" .= toJSON (communicationRequestOccurrenceDateTime p)
    ,  "occurrencePeriod" .= toJSON (communicationRequestOccurrencePeriod p)
    ,  "authoredOn" .= toJSON (communicationRequestAuthoredOn p)
    ,  "requester" .= toJSON (communicationRequestRequester p)
    ,  "recipient" .= toJSON (communicationRequestRecipient p)
    ,  "sender" .= toJSON (communicationRequestSender p)
    ,  "reasonCode" .= toJSON (communicationRequestReasonCode p)
    ,  "reasonReference" .= toJSON (communicationRequestReasonReference p)
    ,  "note" .= toJSON (communicationRequestNote p)
    ]
instance FromJSON CommunicationRequest where
  parseJSON = withObject "CommunicationRequest" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "CommunicationRequest" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        basedOn <- o .:? "basedOn" .!= []
        replaces <- o .:? "replaces" .!= []
        groupIdentifier <- o .:? "groupIdentifier"
        status <- o .:  "status"
        statusReason <- o .:? "statusReason"
        category <- o .:? "category" .!= []
        priority <- o .:? "priority"
        doNotPerform <- o .:? "doNotPerform"
        medium <- o .:? "medium" .!= []
        subject <- o .:? "subject"
        about <- o .:? "about" .!= []
        encounter <- o .:? "encounter"
        payload <- o .:? "payload" .!= []
        occurrenceDateTime <- o .:? "occurrenceDateTime"
        occurrencePeriod <- o .:? "occurrencePeriod"
        authoredOn <- o .:? "authoredOn"
        requester <- o .:? "requester"
        recipient <- o .:? "recipient" .!= []
        sender <- o .:? "sender"
        reasonCode <- o .:? "reasonCode" .!= []
        reasonReference <- o .:? "reasonReference" .!= []
        note <- o .:? "note" .!= []
        return CommunicationRequest{
            communicationRequestId = id
          , communicationRequestMeta = meta
          , communicationRequestImplicitRules = implicitRules
          , communicationRequestLanguage = language
          , communicationRequestText = text
--          , communicationRequestContained = contained
          , communicationRequestExtension = extension
          , communicationRequestModifierExtension = modifierExtension
          , communicationRequestIdentifier = identifier
          , communicationRequestBasedOn = basedOn
          , communicationRequestReplaces = replaces
          , communicationRequestGroupIdentifier = groupIdentifier
          , communicationRequestStatus = status
          , communicationRequestStatusReason = statusReason
          , communicationRequestCategory = category
          , communicationRequestPriority = priority
          , communicationRequestDoNotPerform = doNotPerform
          , communicationRequestMedium = medium
          , communicationRequestSubject = subject
          , communicationRequestAbout = about
          , communicationRequestEncounter = encounter
          , communicationRequestPayload = payload
          , communicationRequestOccurrenceDateTime = occurrenceDateTime
          , communicationRequestOccurrencePeriod = occurrencePeriod
          , communicationRequestAuthoredOn = authoredOn
          , communicationRequestRequester = requester
          , communicationRequestRecipient = recipient
          , communicationRequestSender = sender
          , communicationRequestReasonCode = reasonCode
          , communicationRequestReasonReference = reasonReference
          , communicationRequestNote = note
          }
      _ -> fail "not a CommunicationRequest"
instance Xmlbf.ToXml CommunicationRequest where
  toXml p = Xmlbf.element "CommunicationRequest" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (communicationRequestId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (communicationRequestMeta p))
             , OptVal   "implicitRules" (fmap toUri (communicationRequestImplicitRules p))
             , OptVal   "language" (fmap toLanguage (communicationRequestLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (communicationRequestText p))
--             , PropList "contained" (fmap Xmlbf.toXml (communicationRequestContained p))
             , PropList "extension" (fmap Xmlbf.toXml (communicationRequestExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (communicationRequestModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (communicationRequestIdentifier p))
             , PropList "basedOn" (fmap Xmlbf.toXml (communicationRequestBasedOn p))
             , PropList "replaces" (fmap Xmlbf.toXml (communicationRequestReplaces p))
             , OptProp  "groupIdentifier" (fmap Xmlbf.toXml (communicationRequestGroupIdentifier p))
             , Val      "status" (     toCommunicationRequestStatus (communicationRequestStatus p))
             , OptProp  "statusReason" (fmap Xmlbf.toXml (communicationRequestStatusReason p))
             , PropList "category" (fmap Xmlbf.toXml (communicationRequestCategory p))
             , OptVal   "priority" (fmap toCommunicationRequestPriority (communicationRequestPriority p))
             , OptVal   "doNotPerform" (fmap toBoolean (communicationRequestDoNotPerform p))
             , PropList "medium" (fmap Xmlbf.toXml (communicationRequestMedium p))
             , OptProp  "subject" (fmap Xmlbf.toXml (communicationRequestSubject p))
             , PropList "about" (fmap Xmlbf.toXml (communicationRequestAbout p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (communicationRequestEncounter p))
             , PropList "payload" (fmap Xmlbf.toXml (communicationRequestPayload p))
             , OptVal   "occurrenceDateTime" (fmap toDateTime (communicationRequestOccurrenceDateTime p))
             , OptProp  "occurrencePeriod" (fmap Xmlbf.toXml (communicationRequestOccurrencePeriod p))
             , OptVal   "authoredOn" (fmap toDateTime (communicationRequestAuthoredOn p))
             , OptProp  "requester" (fmap Xmlbf.toXml (communicationRequestRequester p))
             , PropList "recipient" (fmap Xmlbf.toXml (communicationRequestRecipient p))
             , OptProp  "sender" (fmap Xmlbf.toXml (communicationRequestSender p))
             , PropList "reasonCode" (fmap Xmlbf.toXml (communicationRequestReasonCode p))
             , PropList "reasonReference" (fmap Xmlbf.toXml (communicationRequestReasonReference p))
             , PropList "note" (fmap Xmlbf.toXml (communicationRequestNote p))
             ]
instance Xmlbf.FromXml CommunicationRequest where
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
    basedOn <- many     $ Xmlbf.pElement "basedOn" Xmlbf.fromXml
    replaces <- many     $ Xmlbf.pElement "replaces" Xmlbf.fromXml
    groupIdentifier <- optional $ Xmlbf.pElement "groupIdentifier" Xmlbf.fromXml
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    statusReason <- optional $ Xmlbf.pElement "statusReason" Xmlbf.fromXml
    category <- many     $ Xmlbf.pElement "category" Xmlbf.fromXml
    priority <- optional $ Xmlbf.pElement "priority" (Xmlbf.pAttr "value")
    doNotPerform <- optional $ Xmlbf.pElement "doNotPerform" (Xmlbf.pAttr "value")
    medium <- many     $ Xmlbf.pElement "medium" Xmlbf.fromXml
    subject <- optional $ Xmlbf.pElement "subject" Xmlbf.fromXml
    about <- many     $ Xmlbf.pElement "about" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    payload <- many     $ Xmlbf.pElement "payload" Xmlbf.fromXml
    occurrenceDateTime <- optional $ Xmlbf.pElement "occurrenceDateTime" (Xmlbf.pAttr "value")
    occurrencePeriod <- optional $ Xmlbf.pElement "occurrencePeriod" Xmlbf.fromXml
    authoredOn <- optional $ Xmlbf.pElement "authoredOn" (Xmlbf.pAttr "value")
    requester <- optional $ Xmlbf.pElement "requester" Xmlbf.fromXml
    recipient <- many     $ Xmlbf.pElement "recipient" Xmlbf.fromXml
    sender <- optional $ Xmlbf.pElement "sender" Xmlbf.fromXml
    reasonCode <- many     $ Xmlbf.pElement "reasonCode" Xmlbf.fromXml
    reasonReference <- many     $ Xmlbf.pElement "reasonReference" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    return CommunicationRequest {
            communicationRequestId = fmap fromId id
          , communicationRequestMeta = meta
          , communicationRequestImplicitRules = fmap fromUri implicitRules
          , communicationRequestLanguage = fmap fromLanguage language
          , communicationRequestText = text
--          , communicationRequestContained = contained
          , communicationRequestExtension = extension
          , communicationRequestModifierExtension = modifierExtension
          , communicationRequestIdentifier = identifier
          , communicationRequestBasedOn = basedOn
          , communicationRequestReplaces = replaces
          , communicationRequestGroupIdentifier = groupIdentifier
          , communicationRequestStatus =      fromCommunicationRequestStatus status
          , communicationRequestStatusReason = statusReason
          , communicationRequestCategory = category
          , communicationRequestPriority = fmap fromCommunicationRequestPriority priority
          , communicationRequestDoNotPerform = fmap fromBoolean doNotPerform
          , communicationRequestMedium = medium
          , communicationRequestSubject = subject
          , communicationRequestAbout = about
          , communicationRequestEncounter = encounter
          , communicationRequestPayload = payload
          , communicationRequestOccurrenceDateTime = fmap fromDateTime occurrenceDateTime
          , communicationRequestOccurrencePeriod = occurrencePeriod
          , communicationRequestAuthoredOn = fmap fromDateTime authoredOn
          , communicationRequestRequester = requester
          , communicationRequestRecipient = recipient
          , communicationRequestSender = sender
          , communicationRequestReasonCode = reasonCode
          , communicationRequestReasonReference = reasonReference
          , communicationRequestNote = note
          }



data CommunicationRequestPayloadContent
    = CommunicationRequestPayloadContentString Text
    | CommunicationRequestPayloadContentAttachment Attachment
    | CommunicationRequestPayloadContentReference Reference
    deriving (Eq, Show)

data CommunicationRequestPayload = CommunicationRequestPayload {
    communicationRequestPayloadAttrId :: Maybe Text
  , communicationRequestPayloadExtension :: [Extension]
  , communicationRequestPayloadModifierExtension :: [Extension]
  , communicationRequestPayloadContentString :: Text
  , communicationRequestPayloadContentAttachment :: Attachment
  , communicationRequestPayloadContentReference :: Reference
  }
  deriving (Eq, Show)
--

instance ToJSON CommunicationRequestPayload where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (communicationRequestPayloadAttrId p)
    ,  "extension" .= toJSON (communicationRequestPayloadExtension p)
    ,  "modifierExtension" .= toJSON (communicationRequestPayloadModifierExtension p)
    ,  "contentString" .= toJSON (communicationRequestPayloadContentString p)
    ,  "contentAttachment" .= toJSON (communicationRequestPayloadContentAttachment p)
    ,  "contentReference" .= toJSON (communicationRequestPayloadContentReference p)
    ]
instance FromJSON CommunicationRequestPayload where
  parseJSON = withObject "CommunicationRequestPayload" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        contentString <- o .:  "contentString"
        contentAttachment <- o .:  "contentAttachment"
        contentReference <- o .:  "contentReference"
        return CommunicationRequestPayload{
            communicationRequestPayloadAttrId = id
          , communicationRequestPayloadExtension = extension
          , communicationRequestPayloadModifierExtension = modifierExtension
          , communicationRequestPayloadContentString = contentString
          , communicationRequestPayloadContentAttachment = contentAttachment
          , communicationRequestPayloadContentReference = contentReference
          }
instance Xmlbf.ToXml CommunicationRequestPayload where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (communicationRequestPayloadAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (communicationRequestPayloadExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (communicationRequestPayloadModifierExtension p))
             , Val      "contentString" (     toString (communicationRequestPayloadContentString p))
             , Prop     "contentAttachment" (HM.empty, Xmlbf.toXml (communicationRequestPayloadContentAttachment p))
             , Prop     "contentReference" (HM.empty, Xmlbf.toXml (communicationRequestPayloadContentReference p))
             ]
instance Xmlbf.FromXml CommunicationRequestPayload where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    contentString <-            Xmlbf.pElement "contentString" (Xmlbf.pAttr "value")
    contentAttachment <-            Xmlbf.pElement "contentAttachment" Xmlbf.fromXml
    contentReference <-            Xmlbf.pElement "contentReference" Xmlbf.fromXml
    return CommunicationRequestPayload {
            communicationRequestPayloadAttrId = id
          , communicationRequestPayloadExtension = extension
          , communicationRequestPayloadModifierExtension = modifierExtension
          , communicationRequestPayloadContentString =      fromString contentString
          , communicationRequestPayloadContentAttachment = contentAttachment
          , communicationRequestPayloadContentReference = contentReference
          }




