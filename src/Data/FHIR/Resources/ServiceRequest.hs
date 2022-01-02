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
-- FHIR 4.0.0 ServiceRequest
--

module Data.FHIR.Resources.ServiceRequest where

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

data ServiceRequestStatus
    = SRSDraft
    | SRSActive
    | SRSOnHold
    | SRSRevoked
    | SRSCompleted
    | SRSEnteredInError
    | SRSUnknown
  deriving (Eq, Show)

instance ToJSON ServiceRequestStatus where
    toJSON SRSDraft = String "draft"
    toJSON SRSActive = String "active"
    toJSON SRSOnHold = String "on-hold"
    toJSON SRSRevoked = String "revoked"
    toJSON SRSCompleted = String "completed"
    toJSON SRSEnteredInError = String "entered-in-error"
    toJSON SRSUnknown = String "unknown"
instance FromJSON ServiceRequestStatus where
    parseJSON "draft" = return SRSDraft
    parseJSON "active" = return SRSActive
    parseJSON "on-hold" = return SRSOnHold
    parseJSON "revoked" = return SRSRevoked
    parseJSON "completed" = return SRSCompleted
    parseJSON "entered-in-error" = return SRSEnteredInError
    parseJSON "unknown" = return SRSUnknown

toServiceRequestStatus SRSDraft = "draft"
toServiceRequestStatus SRSActive = "active"
toServiceRequestStatus SRSOnHold = "on-hold"
toServiceRequestStatus SRSRevoked = "revoked"
toServiceRequestStatus SRSCompleted = "completed"
toServiceRequestStatus SRSEnteredInError = "entered-in-error"
toServiceRequestStatus SRSUnknown = "unknown"
fromServiceRequestStatus "draft" = SRSDraft
fromServiceRequestStatus "active" = SRSActive
fromServiceRequestStatus "on-hold" = SRSOnHold
fromServiceRequestStatus "revoked" = SRSRevoked
fromServiceRequestStatus "completed" = SRSCompleted
fromServiceRequestStatus "entered-in-error" = SRSEnteredInError
fromServiceRequestStatus "unknown" = SRSUnknown


data ServiceRequestIntent
    = SRIProposal
    | SRIPlan
    | SRIDirective
    | SRIOrder
    | SRIOriginalOrder
    | SRIReflexOrder
    | SRIFillerOrder
    | SRIInstanceOrder
    | SRIOption
  deriving (Eq, Show)

instance ToJSON ServiceRequestIntent where
    toJSON SRIProposal = String "proposal"
    toJSON SRIPlan = String "plan"
    toJSON SRIDirective = String "directive"
    toJSON SRIOrder = String "order"
    toJSON SRIOriginalOrder = String "original-order"
    toJSON SRIReflexOrder = String "reflex-order"
    toJSON SRIFillerOrder = String "filler-order"
    toJSON SRIInstanceOrder = String "instance-order"
    toJSON SRIOption = String "option"
instance FromJSON ServiceRequestIntent where
    parseJSON "proposal" = return SRIProposal
    parseJSON "plan" = return SRIPlan
    parseJSON "directive" = return SRIDirective
    parseJSON "order" = return SRIOrder
    parseJSON "original-order" = return SRIOriginalOrder
    parseJSON "reflex-order" = return SRIReflexOrder
    parseJSON "filler-order" = return SRIFillerOrder
    parseJSON "instance-order" = return SRIInstanceOrder
    parseJSON "option" = return SRIOption

toServiceRequestIntent SRIProposal = "proposal"
toServiceRequestIntent SRIPlan = "plan"
toServiceRequestIntent SRIDirective = "directive"
toServiceRequestIntent SRIOrder = "order"
toServiceRequestIntent SRIOriginalOrder = "original-order"
toServiceRequestIntent SRIReflexOrder = "reflex-order"
toServiceRequestIntent SRIFillerOrder = "filler-order"
toServiceRequestIntent SRIInstanceOrder = "instance-order"
toServiceRequestIntent SRIOption = "option"
fromServiceRequestIntent "proposal" = SRIProposal
fromServiceRequestIntent "plan" = SRIPlan
fromServiceRequestIntent "directive" = SRIDirective
fromServiceRequestIntent "order" = SRIOrder
fromServiceRequestIntent "original-order" = SRIOriginalOrder
fromServiceRequestIntent "reflex-order" = SRIReflexOrder
fromServiceRequestIntent "filler-order" = SRIFillerOrder
fromServiceRequestIntent "instance-order" = SRIInstanceOrder
fromServiceRequestIntent "option" = SRIOption


data ServiceRequestPriority
    = SRPRoutine
    | SRPUrgent
    | SRPAsap
    | SRPStat
  deriving (Eq, Show)

instance ToJSON ServiceRequestPriority where
    toJSON SRPRoutine = String "routine"
    toJSON SRPUrgent = String "urgent"
    toJSON SRPAsap = String "asap"
    toJSON SRPStat = String "stat"
instance FromJSON ServiceRequestPriority where
    parseJSON "routine" = return SRPRoutine
    parseJSON "urgent" = return SRPUrgent
    parseJSON "asap" = return SRPAsap
    parseJSON "stat" = return SRPStat

toServiceRequestPriority SRPRoutine = "routine"
toServiceRequestPriority SRPUrgent = "urgent"
toServiceRequestPriority SRPAsap = "asap"
toServiceRequestPriority SRPStat = "stat"
fromServiceRequestPriority "routine" = SRPRoutine
fromServiceRequestPriority "urgent" = SRPUrgent
fromServiceRequestPriority "asap" = SRPAsap
fromServiceRequestPriority "stat" = SRPStat


data ServiceRequestQuantity
    = ServiceRequestQuantityQuantity Quantity
    | ServiceRequestQuantityRatio Ratio
    | ServiceRequestQuantityRange Range
    deriving (Eq, Show)

data ServiceRequestOccurrence
    = ServiceRequestOccurrenceDateTime DateTime
    | ServiceRequestOccurrencePeriod Period
    | ServiceRequestOccurrenceTiming Timing
    deriving (Eq, Show)

data ServiceRequestAsNeeded
    = ServiceRequestAsNeededBoolean Boolean
    | ServiceRequestAsNeededCodeableConcept CodeableConcept
    deriving (Eq, Show)

data ServiceRequest = ServiceRequest {
    serviceRequestId :: Maybe Id
  , serviceRequestMeta :: Maybe Meta
  , serviceRequestImplicitRules :: Maybe Uri
  , serviceRequestLanguage :: Maybe Language
  , serviceRequestText :: Maybe Narrative
--    serviceRequestContained :: [ResourceContainer]
  , serviceRequestExtension :: [Extension]
  , serviceRequestModifierExtension :: [Extension]
  , serviceRequestIdentifier :: [Identifier]
  , serviceRequestInstantiatesCanonical :: [Canonical]
  , serviceRequestInstantiatesUri :: [Uri]
  , serviceRequestBasedOn :: [Reference]
  , serviceRequestReplaces :: [Reference]
  , serviceRequestRequisition :: Maybe Identifier
  , serviceRequestStatus :: ServiceRequestStatus
  , serviceRequestIntent :: ServiceRequestIntent
  , serviceRequestCategory :: [CodeableConcept]
  , serviceRequestPriority :: Maybe ServiceRequestPriority
  , serviceRequestDoNotPerform :: Maybe Boolean
  , serviceRequestCode :: Maybe CodeableConcept
  , serviceRequestOrderDetail :: [CodeableConcept]
  , serviceRequestQuantity :: Maybe ServiceRequestQuantity
  , serviceRequestSubject :: Reference
  , serviceRequestEncounter :: Maybe Reference
  , serviceRequestOccurrence :: Maybe ServiceRequestOccurrence
  , serviceRequestAsNeeded :: Maybe ServiceRequestAsNeeded
  , serviceRequestAuthoredOn :: Maybe DateTime
  , serviceRequestRequester :: Maybe Reference
  , serviceRequestPerformerType :: Maybe CodeableConcept
  , serviceRequestPerformer :: [Reference]
  , serviceRequestLocationCode :: [CodeableConcept]
  , serviceRequestLocationReference :: [Reference]
  , serviceRequestReasonCode :: [CodeableConcept]
  , serviceRequestReasonReference :: [Reference]
  , serviceRequestInsurance :: [Reference]
  , serviceRequestSupportingInfo :: [Reference]
  , serviceRequestSpecimen :: [Reference]
  , serviceRequestBodySite :: [CodeableConcept]
  , serviceRequestNote :: [Annotation]
  , serviceRequestPatientInstruction :: Maybe Text
  , serviceRequestRelevantHistory :: [Reference]
  } deriving (Eq, Show)
--

instance ToJSON ServiceRequest where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "ServiceRequest")
    ,  "id" .= toJSON (serviceRequestId p)
    ,  "meta" .= toJSON (serviceRequestMeta p)
    ,  "implicitRules" .= toJSON (serviceRequestImplicitRules p)
    ,  "language" .= toJSON (serviceRequestLanguage p)
    ,  "text" .= toJSON (serviceRequestText p)
--    , "contained" .= toJSON (serviceRequestContained p)
    ,  "extension" .= toJSON (serviceRequestExtension p)
    ,  "modifierExtension" .= toJSON (serviceRequestModifierExtension p)
    ,  "identifier" .= toJSON (serviceRequestIdentifier p)
    ,  "instantiatesCanonical" .= toJSON (serviceRequestInstantiatesCanonical p)
    ,  "instantiatesUri" .= toJSON (serviceRequestInstantiatesUri p)
    ,  "basedOn" .= toJSON (serviceRequestBasedOn p)
    ,  "replaces" .= toJSON (serviceRequestReplaces p)
    ,  "requisition" .= toJSON (serviceRequestRequisition p)
    ,  "status" .= toJSON (serviceRequestStatus p)
    ,  "intent" .= toJSON (serviceRequestIntent p)
    ,  "category" .= toJSON (serviceRequestCategory p)
    ,  "priority" .= toJSON (serviceRequestPriority p)
    ,  "doNotPerform" .= toJSON (serviceRequestDoNotPerform p)
    ,  "code" .= toJSON (serviceRequestCode p)
    ,  "orderDetail" .= toJSON (serviceRequestOrderDetail p)
    , toQuantityJSON (serviceRequestQuantity p)
    ,  "subject" .= toJSON (serviceRequestSubject p)
    ,  "encounter" .= toJSON (serviceRequestEncounter p)
    , toOccurrenceJSON (serviceRequestOccurrence p)
    , toAsNeededJSON (serviceRequestAsNeeded p)
    ,  "authoredOn" .= toJSON (serviceRequestAuthoredOn p)
    ,  "requester" .= toJSON (serviceRequestRequester p)
    ,  "performerType" .= toJSON (serviceRequestPerformerType p)
    ,  "performer" .= toJSON (serviceRequestPerformer p)
    ,  "locationCode" .= toJSON (serviceRequestLocationCode p)
    ,  "locationReference" .= toJSON (serviceRequestLocationReference p)
    ,  "reasonCode" .= toJSON (serviceRequestReasonCode p)
    ,  "reasonReference" .= toJSON (serviceRequestReasonReference p)
    ,  "insurance" .= toJSON (serviceRequestInsurance p)
    ,  "supportingInfo" .= toJSON (serviceRequestSupportingInfo p)
    ,  "specimen" .= toJSON (serviceRequestSpecimen p)
    ,  "bodySite" .= toJSON (serviceRequestBodySite p)
    ,  "note" .= toJSON (serviceRequestNote p)
    ,  "patientInstruction" .= toJSON (serviceRequestPatientInstruction p)
    ,  "relevantHistory" .= toJSON (serviceRequestRelevantHistory p)
    ]
    where 
      toQuantityJSON (     Nothing   ) = ("quantity", Null)
      toQuantityJSON (Just (ServiceRequestQuantityQuantity c)) = ("quantityQuantity", toJSON c)
      toQuantityJSON (Just (ServiceRequestQuantityRatio c)) = ("quantityRatio", toJSON c)
      toQuantityJSON (Just (ServiceRequestQuantityRange c)) = ("quantityRange", toJSON c)
      toOccurrenceJSON (     Nothing   ) = ("occurrence", Null)
      toOccurrenceJSON (Just (ServiceRequestOccurrenceDateTime c)) = ("occurrenceDateTime", toJSON c)
      toOccurrenceJSON (Just (ServiceRequestOccurrencePeriod c)) = ("occurrencePeriod", toJSON c)
      toOccurrenceJSON (Just (ServiceRequestOccurrenceTiming c)) = ("occurrenceTiming", toJSON c)
      toAsNeededJSON (     Nothing   ) = ("asNeeded", Null)
      toAsNeededJSON (Just (ServiceRequestAsNeededBoolean c)) = ("asNeededBoolean", toJSON c)
      toAsNeededJSON (Just (ServiceRequestAsNeededCodeableConcept c)) = ("asNeededCodeableConcept", toJSON c)
instance FromJSON ServiceRequest where
  parseJSON = withObject "ServiceRequest" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "ServiceRequest" -> do
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
        requisition <- o .:? "requisition"
        status <- o .:  "status"
        intent <- o .:  "intent"
        category <- o .:? "category" .!= []
        priority <- o .:? "priority"
        doNotPerform <- o .:? "doNotPerform"
        code <- o .:? "code"
        orderDetail <- o .:? "orderDetail" .!= []
        quantity <- parseQuantity o
        subject <- o .:  "subject"
        encounter <- o .:? "encounter"
        occurrence <- parseOccurrence o
        asNeeded <- parseAsNeeded o
        authoredOn <- o .:? "authoredOn"
        requester <- o .:? "requester"
        performerType <- o .:? "performerType"
        performer <- o .:? "performer" .!= []
        locationCode <- o .:? "locationCode" .!= []
        locationReference <- o .:? "locationReference" .!= []
        reasonCode <- o .:? "reasonCode" .!= []
        reasonReference <- o .:? "reasonReference" .!= []
        insurance <- o .:? "insurance" .!= []
        supportingInfo <- o .:? "supportingInfo" .!= []
        specimen <- o .:? "specimen" .!= []
        bodySite <- o .:? "bodySite" .!= []
        note <- o .:? "note" .!= []
        patientInstruction <- o .:? "patientInstruction"
        relevantHistory <- o .:? "relevantHistory" .!= []
        return ServiceRequest{
            serviceRequestId = id
          , serviceRequestMeta = meta
          , serviceRequestImplicitRules = implicitRules
          , serviceRequestLanguage = language
          , serviceRequestText = text
--          , serviceRequestContained = contained
          , serviceRequestExtension = extension
          , serviceRequestModifierExtension = modifierExtension
          , serviceRequestIdentifier = identifier
          , serviceRequestInstantiatesCanonical = instantiatesCanonical
          , serviceRequestInstantiatesUri = instantiatesUri
          , serviceRequestBasedOn = basedOn
          , serviceRequestReplaces = replaces
          , serviceRequestRequisition = requisition
          , serviceRequestStatus = status
          , serviceRequestIntent = intent
          , serviceRequestCategory = category
          , serviceRequestPriority = priority
          , serviceRequestDoNotPerform = doNotPerform
          , serviceRequestCode = code
          , serviceRequestOrderDetail = orderDetail
          , serviceRequestQuantity = quantity
          , serviceRequestSubject = subject
          , serviceRequestEncounter = encounter
          , serviceRequestOccurrence = occurrence
          , serviceRequestAsNeeded = asNeeded
          , serviceRequestAuthoredOn = authoredOn
          , serviceRequestRequester = requester
          , serviceRequestPerformerType = performerType
          , serviceRequestPerformer = performer
          , serviceRequestLocationCode = locationCode
          , serviceRequestLocationReference = locationReference
          , serviceRequestReasonCode = reasonCode
          , serviceRequestReasonReference = reasonReference
          , serviceRequestInsurance = insurance
          , serviceRequestSupportingInfo = supportingInfo
          , serviceRequestSpecimen = specimen
          , serviceRequestBodySite = bodySite
          , serviceRequestNote = note
          , serviceRequestPatientInstruction = patientInstruction
          , serviceRequestRelevantHistory = relevantHistory
          }
      _ -> fail "not a ServiceRequest"
    where 
      parseQuantity o = parseQuantityQuantity o <|> parseQuantityRatio o <|> parseQuantityRange o
      parseQuantityQuantity o = do
                has <- o .: "quantityQuantity"
                return $ Just (ServiceRequestQuantityQuantity has)
      parseQuantityRatio o = do
                has <- o .: "quantityRatio"
                return $ Just (ServiceRequestQuantityRatio has)
      parseQuantityRange o = do
                has <- o .: "quantityRange"
                return $ Just (ServiceRequestQuantityRange has)
      parseOccurrence o = parseOccurrenceDateTime o <|> parseOccurrencePeriod o <|> parseOccurrenceTiming o
      parseOccurrenceDateTime o = do
                has <- o .: "occurrenceDateTime"
                return $ Just (ServiceRequestOccurrenceDateTime has)
      parseOccurrencePeriod o = do
                has <- o .: "occurrencePeriod"
                return $ Just (ServiceRequestOccurrencePeriod has)
      parseOccurrenceTiming o = do
                has <- o .: "occurrenceTiming"
                return $ Just (ServiceRequestOccurrenceTiming has)
      parseAsNeeded o = parseAsNeededBoolean o <|> parseAsNeededCodeableConcept o
      parseAsNeededBoolean o = do
                has <- o .: "asNeededBoolean"
                return $ Just (ServiceRequestAsNeededBoolean has)
      parseAsNeededCodeableConcept o = do
                has <- o .: "asNeededCodeableConcept"
                return $ Just (ServiceRequestAsNeededCodeableConcept has)
instance Xmlbf.ToXml ServiceRequest where
  toXml p = Xmlbf.element "ServiceRequest" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (serviceRequestId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (serviceRequestMeta p))
             , OptVal   "implicitRules" (fmap toUri (serviceRequestImplicitRules p))
             , OptVal   "language" (fmap toLanguage (serviceRequestLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (serviceRequestText p))
--             , PropList "contained" (fmap Xmlbf.toXml (serviceRequestContained p))
             , PropList "extension" (fmap Xmlbf.toXml (serviceRequestExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (serviceRequestModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (serviceRequestIdentifier p))
             , ValList  "instantiatesCanonical" (fmap toCanonical (serviceRequestInstantiatesCanonical p))
             , ValList  "instantiatesUri" (fmap toUri (serviceRequestInstantiatesUri p))
             , PropList "basedOn" (fmap Xmlbf.toXml (serviceRequestBasedOn p))
             , PropList "replaces" (fmap Xmlbf.toXml (serviceRequestReplaces p))
             , OptProp  "requisition" (fmap Xmlbf.toXml (serviceRequestRequisition p))
             , Val      "status" (     toServiceRequestStatus (serviceRequestStatus p))
             , Val      "intent" (     toServiceRequestIntent (serviceRequestIntent p))
             , PropList "category" (fmap Xmlbf.toXml (serviceRequestCategory p))
             , OptVal   "priority" (fmap toServiceRequestPriority (serviceRequestPriority p))
             , OptVal   "doNotPerform" (fmap toBoolean (serviceRequestDoNotPerform p))
             , OptProp  "code" (fmap Xmlbf.toXml (serviceRequestCode p))
             , PropList "orderDetail" (fmap Xmlbf.toXml (serviceRequestOrderDetail p))
             , toQuantityXml (serviceRequestQuantity p)
             , Prop     "subject" (HM.empty, Xmlbf.toXml (serviceRequestSubject p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (serviceRequestEncounter p))
             , toOccurrenceXml (serviceRequestOccurrence p)
             , toAsNeededXml (serviceRequestAsNeeded p)
             , OptVal   "authoredOn" (fmap toDateTime (serviceRequestAuthoredOn p))
             , OptProp  "requester" (fmap Xmlbf.toXml (serviceRequestRequester p))
             , OptProp  "performerType" (fmap Xmlbf.toXml (serviceRequestPerformerType p))
             , PropList "performer" (fmap Xmlbf.toXml (serviceRequestPerformer p))
             , PropList "locationCode" (fmap Xmlbf.toXml (serviceRequestLocationCode p))
             , PropList "locationReference" (fmap Xmlbf.toXml (serviceRequestLocationReference p))
             , PropList "reasonCode" (fmap Xmlbf.toXml (serviceRequestReasonCode p))
             , PropList "reasonReference" (fmap Xmlbf.toXml (serviceRequestReasonReference p))
             , PropList "insurance" (fmap Xmlbf.toXml (serviceRequestInsurance p))
             , PropList "supportingInfo" (fmap Xmlbf.toXml (serviceRequestSupportingInfo p))
             , PropList "specimen" (fmap Xmlbf.toXml (serviceRequestSpecimen p))
             , PropList "bodySite" (fmap Xmlbf.toXml (serviceRequestBodySite p))
             , PropList "note" (fmap Xmlbf.toXml (serviceRequestNote p))
             , OptVal   "patientInstruction" (fmap toString (serviceRequestPatientInstruction p))
             , PropList "relevantHistory" (fmap Xmlbf.toXml (serviceRequestRelevantHistory p))
             ]
          toQuantityXml ( Nothing   ) = (OptVal "quantity" Nothing)
          toQuantityXml (Just (ServiceRequestQuantityQuantity p)) = Prop  "quantityQuantity" (HM.empty, Xmlbf.toXml p)
          toQuantityXml (Just (ServiceRequestQuantityRatio p)) = Prop  "quantityRatio" (HM.empty, Xmlbf.toXml p)
          toQuantityXml (Just (ServiceRequestQuantityRange p)) = Prop  "quantityRange" (HM.empty, Xmlbf.toXml p)
          toOccurrenceXml ( Nothing   ) = (OptVal "occurrence" Nothing)
          toOccurrenceXml (Just (ServiceRequestOccurrenceDateTime p)) = Val   "occurrenceDateTime" (toDateTime p)
          toOccurrenceXml (Just (ServiceRequestOccurrencePeriod p)) = Prop  "occurrencePeriod" (HM.empty, Xmlbf.toXml p)
          toOccurrenceXml (Just (ServiceRequestOccurrenceTiming p)) = Prop  "occurrenceTiming" (HM.empty, Xmlbf.toXml p)
          toAsNeededXml ( Nothing   ) = (OptVal "asNeeded" Nothing)
          toAsNeededXml (Just (ServiceRequestAsNeededBoolean p)) = Val   "asNeededBoolean" (toBoolean p)
          toAsNeededXml (Just (ServiceRequestAsNeededCodeableConcept p)) = Prop  "asNeededCodeableConcept" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml ServiceRequest where
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
    requisition <- optional $ Xmlbf.pElement "requisition" Xmlbf.fromXml
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    intent <-            Xmlbf.pElement "intent" (Xmlbf.pAttr "value")
    category <- many     $ Xmlbf.pElement "category" Xmlbf.fromXml
    priority <- optional $ Xmlbf.pElement "priority" (Xmlbf.pAttr "value")
    doNotPerform <- optional $ Xmlbf.pElement "doNotPerform" (Xmlbf.pAttr "value")
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    orderDetail <- many     $ Xmlbf.pElement "orderDetail" Xmlbf.fromXml
    quantity <- fromQuantityXml
    subject <-            Xmlbf.pElement "subject" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    occurrence <- fromOccurrenceXml
    asNeeded <- fromAsNeededXml
    authoredOn <- optional $ Xmlbf.pElement "authoredOn" (Xmlbf.pAttr "value")
    requester <- optional $ Xmlbf.pElement "requester" Xmlbf.fromXml
    performerType <- optional $ Xmlbf.pElement "performerType" Xmlbf.fromXml
    performer <- many     $ Xmlbf.pElement "performer" Xmlbf.fromXml
    locationCode <- many     $ Xmlbf.pElement "locationCode" Xmlbf.fromXml
    locationReference <- many     $ Xmlbf.pElement "locationReference" Xmlbf.fromXml
    reasonCode <- many     $ Xmlbf.pElement "reasonCode" Xmlbf.fromXml
    reasonReference <- many     $ Xmlbf.pElement "reasonReference" Xmlbf.fromXml
    insurance <- many     $ Xmlbf.pElement "insurance" Xmlbf.fromXml
    supportingInfo <- many     $ Xmlbf.pElement "supportingInfo" Xmlbf.fromXml
    specimen <- many     $ Xmlbf.pElement "specimen" Xmlbf.fromXml
    bodySite <- many     $ Xmlbf.pElement "bodySite" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    patientInstruction <- optional $ Xmlbf.pElement "patientInstruction" (Xmlbf.pAttr "value")
    relevantHistory <- many     $ Xmlbf.pElement "relevantHistory" Xmlbf.fromXml
    return ServiceRequest {
            serviceRequestId = fmap fromId id
          , serviceRequestMeta = meta
          , serviceRequestImplicitRules = fmap fromUri implicitRules
          , serviceRequestLanguage = fmap fromLanguage language
          , serviceRequestText = text
--          , serviceRequestContained = contained
          , serviceRequestExtension = extension
          , serviceRequestModifierExtension = modifierExtension
          , serviceRequestIdentifier = identifier
          , serviceRequestInstantiatesCanonical = fmap fromCanonical instantiatesCanonical
          , serviceRequestInstantiatesUri = fmap fromUri instantiatesUri
          , serviceRequestBasedOn = basedOn
          , serviceRequestReplaces = replaces
          , serviceRequestRequisition = requisition
          , serviceRequestStatus =      fromServiceRequestStatus status
          , serviceRequestIntent =      fromServiceRequestIntent intent
          , serviceRequestCategory = category
          , serviceRequestPriority = fmap fromServiceRequestPriority priority
          , serviceRequestDoNotPerform = fmap fromBoolean doNotPerform
          , serviceRequestCode = code
          , serviceRequestOrderDetail = orderDetail
          , serviceRequestQuantity = quantity
          , serviceRequestSubject = subject
          , serviceRequestEncounter = encounter
          , serviceRequestOccurrence = occurrence
          , serviceRequestAsNeeded = asNeeded
          , serviceRequestAuthoredOn = fmap fromDateTime authoredOn
          , serviceRequestRequester = requester
          , serviceRequestPerformerType = performerType
          , serviceRequestPerformer = performer
          , serviceRequestLocationCode = locationCode
          , serviceRequestLocationReference = locationReference
          , serviceRequestReasonCode = reasonCode
          , serviceRequestReasonReference = reasonReference
          , serviceRequestInsurance = insurance
          , serviceRequestSupportingInfo = supportingInfo
          , serviceRequestSpecimen = specimen
          , serviceRequestBodySite = bodySite
          , serviceRequestNote = note
          , serviceRequestPatientInstruction = fmap fromString patientInstruction
          , serviceRequestRelevantHistory = relevantHistory
          }

    where 
      fromQuantityXml = parseQuantityQuantity <|> parseQuantityRatio <|> parseQuantityRange <|> pure Nothing
      parseQuantityQuantity = do
                has <- Xmlbf.pElement "quantityQuantity" Xmlbf.fromXml
                return $ Just (ServiceRequestQuantityQuantity (                      has))
      parseQuantityRatio = do
                has <- Xmlbf.pElement "quantityRatio" Xmlbf.fromXml
                return $ Just (ServiceRequestQuantityRatio (                      has))
      parseQuantityRange = do
                has <- Xmlbf.pElement "quantityRange" Xmlbf.fromXml
                return $ Just (ServiceRequestQuantityRange (                      has))
      fromOccurrenceXml = parseOccurrenceDateTime <|> parseOccurrencePeriod <|> parseOccurrenceTiming <|> pure Nothing
      parseOccurrenceDateTime = do
                has <- Xmlbf.pElement "occurrenceDateTime" (Xmlbf.pAttr "value")
                return $ Just (ServiceRequestOccurrenceDateTime (     fromDateTime has))
      parseOccurrencePeriod = do
                has <- Xmlbf.pElement "occurrencePeriod" Xmlbf.fromXml
                return $ Just (ServiceRequestOccurrencePeriod (                      has))
      parseOccurrenceTiming = do
                has <- Xmlbf.pElement "occurrenceTiming" Xmlbf.fromXml
                return $ Just (ServiceRequestOccurrenceTiming (                      has))
      fromAsNeededXml = parseAsNeededBoolean <|> parseAsNeededCodeableConcept <|> pure Nothing
      parseAsNeededBoolean = do
                has <- Xmlbf.pElement "asNeededBoolean" (Xmlbf.pAttr "value")
                return $ Just (ServiceRequestAsNeededBoolean (     fromBoolean has))
      parseAsNeededCodeableConcept = do
                has <- Xmlbf.pElement "asNeededCodeableConcept" Xmlbf.fromXml
                return $ Just (ServiceRequestAsNeededCodeableConcept (                      has))



