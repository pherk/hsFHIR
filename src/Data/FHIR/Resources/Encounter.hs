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
-- FHIR 4.0.0 Encounter
--

module Data.FHIR.Resources.Encounter where

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

data EncounterStatus
    = ESPlanned
    | ESArrived
    | ESTriaged
    | ESInProgress
    | ESOnleave
    | ESFinished
    | ESCancelled
    | ESEnteredInError
    | ESUnknown
  deriving (Eq, Show)

instance ToJSON EncounterStatus where
    toJSON ESPlanned = String "planned"
    toJSON ESArrived = String "arrived"
    toJSON ESTriaged = String "triaged"
    toJSON ESInProgress = String "in-progress"
    toJSON ESOnleave = String "onleave"
    toJSON ESFinished = String "finished"
    toJSON ESCancelled = String "cancelled"
    toJSON ESEnteredInError = String "entered-in-error"
    toJSON ESUnknown = String "unknown"
instance FromJSON EncounterStatus where
    parseJSON "planned" = return ESPlanned
    parseJSON "arrived" = return ESArrived
    parseJSON "triaged" = return ESTriaged
    parseJSON "in-progress" = return ESInProgress
    parseJSON "onleave" = return ESOnleave
    parseJSON "finished" = return ESFinished
    parseJSON "cancelled" = return ESCancelled
    parseJSON "entered-in-error" = return ESEnteredInError
    parseJSON "unknown" = return ESUnknown

toEncounterStatus ESPlanned = "planned"
toEncounterStatus ESArrived = "arrived"
toEncounterStatus ESTriaged = "triaged"
toEncounterStatus ESInProgress = "in-progress"
toEncounterStatus ESOnleave = "onleave"
toEncounterStatus ESFinished = "finished"
toEncounterStatus ESCancelled = "cancelled"
toEncounterStatus ESEnteredInError = "entered-in-error"
toEncounterStatus ESUnknown = "unknown"
fromEncounterStatus "planned" = ESPlanned
fromEncounterStatus "arrived" = ESArrived
fromEncounterStatus "triaged" = ESTriaged
fromEncounterStatus "in-progress" = ESInProgress
fromEncounterStatus "onleave" = ESOnleave
fromEncounterStatus "finished" = ESFinished
fromEncounterStatus "cancelled" = ESCancelled
fromEncounterStatus "entered-in-error" = ESEnteredInError
fromEncounterStatus "unknown" = ESUnknown


data Encounter = Encounter {
    encounterId :: Maybe Id
  , encounterMeta :: Maybe Meta
  , encounterImplicitRules :: Maybe Uri
  , encounterLanguage :: Maybe Language
  , encounterText :: Maybe Narrative
--    encounterContained :: [ResourceContainer]
  , encounterExtension :: [Extension]
  , encounterModifierExtension :: [Extension]
  , encounterIdentifier :: [Identifier]
  , encounterStatus :: EncounterStatus
  , encounterStatusHistory :: [EncounterStatusHistory]
  , encounterClass :: Coding
  , encounterClassHistory :: [EncounterClassHistory]
  , encounterType :: [CodeableConcept]
  , encounterServiceType :: Maybe CodeableConcept
  , encounterPriority :: Maybe CodeableConcept
  , encounterSubject :: Maybe Reference
  , encounterEpisodeOfCare :: [Reference]
  , encounterBasedOn :: [Reference]
  , encounterParticipant :: [EncounterParticipant]
  , encounterAppointment :: [Reference]
  , encounterPeriod :: Maybe Period
  , encounterLength :: Maybe Duration
  , encounterReasonCode :: [CodeableConcept]
  , encounterReasonReference :: [Reference]
  , encounterDiagnosis :: [EncounterDiagnosis]
  , encounterAccount :: [Reference]
  , encounterHospitalization :: Maybe EncounterHospitalization
  , encounterLocation :: [EncounterLocation]
  , encounterServiceProvider :: Maybe Reference
  , encounterPartOf :: Maybe Reference
  } deriving (Eq, Show)
--

instance ToJSON Encounter where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Encounter")
    ,  "id" .= toJSON (encounterId p)
    ,  "meta" .= toJSON (encounterMeta p)
    ,  "implicitRules" .= toJSON (encounterImplicitRules p)
    ,  "language" .= toJSON (encounterLanguage p)
    ,  "text" .= toJSON (encounterText p)
--    , "contained" .= toJSON (encounterContained p)
    ,  "extension" .= toJSON (encounterExtension p)
    ,  "modifierExtension" .= toJSON (encounterModifierExtension p)
    ,  "identifier" .= toJSON (encounterIdentifier p)
    ,  "status" .= toJSON (encounterStatus p)
    ,  "statusHistory" .= toJSON (encounterStatusHistory p)
    ,  "class" .= toJSON (encounterClass p)
    ,  "classHistory" .= toJSON (encounterClassHistory p)
    ,  "type" .= toJSON (encounterType p)
    ,  "serviceType" .= toJSON (encounterServiceType p)
    ,  "priority" .= toJSON (encounterPriority p)
    ,  "subject" .= toJSON (encounterSubject p)
    ,  "episodeOfCare" .= toJSON (encounterEpisodeOfCare p)
    ,  "basedOn" .= toJSON (encounterBasedOn p)
    ,  "participant" .= toJSON (encounterParticipant p)
    ,  "appointment" .= toJSON (encounterAppointment p)
    ,  "period" .= toJSON (encounterPeriod p)
    ,  "length" .= toJSON (encounterLength p)
    ,  "reasonCode" .= toJSON (encounterReasonCode p)
    ,  "reasonReference" .= toJSON (encounterReasonReference p)
    ,  "diagnosis" .= toJSON (encounterDiagnosis p)
    ,  "account" .= toJSON (encounterAccount p)
    ,  "hospitalization" .= toJSON (encounterHospitalization p)
    ,  "location" .= toJSON (encounterLocation p)
    ,  "serviceProvider" .= toJSON (encounterServiceProvider p)
    ,  "partOf" .= toJSON (encounterPartOf p)
    ]
instance FromJSON Encounter where
  parseJSON = withObject "Encounter" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Encounter" -> do
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
        statusHistory <- o .:? "statusHistory" .!= []
        cl <- o .:  "class"
        classHistory <- o .:? "classHistory" .!= []
        ty <- o .:? "type" .!= []
        serviceType <- o .:? "serviceType"
        priority <- o .:? "priority"
        subject <- o .:? "subject"
        episodeOfCare <- o .:? "episodeOfCare" .!= []
        basedOn <- o .:? "basedOn" .!= []
        participant <- o .:? "participant" .!= []
        appointment <- o .:? "appointment" .!= []
        period <- o .:? "period"
        length <- o .:? "length"
        reasonCode <- o .:? "reasonCode" .!= []
        reasonReference <- o .:? "reasonReference" .!= []
        diagnosis <- o .:? "diagnosis" .!= []
        account <- o .:? "account" .!= []
        hospitalization <- o .:? "hospitalization"
        location <- o .:? "location" .!= []
        serviceProvider <- o .:? "serviceProvider"
        partOf <- o .:? "partOf"
        return Encounter{
            encounterId = id
          , encounterMeta = meta
          , encounterImplicitRules = implicitRules
          , encounterLanguage = language
          , encounterText = text
--          , encounterContained = contained
          , encounterExtension = extension
          , encounterModifierExtension = modifierExtension
          , encounterIdentifier = identifier
          , encounterStatus = status
          , encounterStatusHistory = statusHistory
          , encounterClass = cl
          , encounterClassHistory = classHistory
          , encounterType = ty
          , encounterServiceType = serviceType
          , encounterPriority = priority
          , encounterSubject = subject
          , encounterEpisodeOfCare = episodeOfCare
          , encounterBasedOn = basedOn
          , encounterParticipant = participant
          , encounterAppointment = appointment
          , encounterPeriod = period
          , encounterLength = length
          , encounterReasonCode = reasonCode
          , encounterReasonReference = reasonReference
          , encounterDiagnosis = diagnosis
          , encounterAccount = account
          , encounterHospitalization = hospitalization
          , encounterLocation = location
          , encounterServiceProvider = serviceProvider
          , encounterPartOf = partOf
          }
      _ -> fail "not a Encounter"
instance Xmlbf.ToXml Encounter where
  toXml p = Xmlbf.element "Encounter" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (encounterId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (encounterMeta p))
             , OptVal   "implicitRules" (fmap toUri (encounterImplicitRules p))
             , OptVal   "language" (fmap toLanguage (encounterLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (encounterText p))
--             , PropList "contained" (fmap Xmlbf.toXml (encounterContained p))
             , PropList "extension" (fmap Xmlbf.toXml (encounterExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (encounterModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (encounterIdentifier p))
             , Val      "status" (     toEncounterStatus (encounterStatus p))
             , PropList "statusHistory" (fmap Xmlbf.toXml (encounterStatusHistory p))
             , Prop     "class" (HM.empty, Xmlbf.toXml (encounterClass p))
             , PropList "classHistory" (fmap Xmlbf.toXml (encounterClassHistory p))
             , PropList "type" (fmap Xmlbf.toXml (encounterType p))
             , OptProp  "serviceType" (fmap Xmlbf.toXml (encounterServiceType p))
             , OptProp  "priority" (fmap Xmlbf.toXml (encounterPriority p))
             , OptProp  "subject" (fmap Xmlbf.toXml (encounterSubject p))
             , PropList "episodeOfCare" (fmap Xmlbf.toXml (encounterEpisodeOfCare p))
             , PropList "basedOn" (fmap Xmlbf.toXml (encounterBasedOn p))
             , PropList "participant" (fmap Xmlbf.toXml (encounterParticipant p))
             , PropList "appointment" (fmap Xmlbf.toXml (encounterAppointment p))
             , OptProp  "period" (fmap Xmlbf.toXml (encounterPeriod p))
             , OptProp  "length" (fmap Xmlbf.toXml (encounterLength p))
             , PropList "reasonCode" (fmap Xmlbf.toXml (encounterReasonCode p))
             , PropList "reasonReference" (fmap Xmlbf.toXml (encounterReasonReference p))
             , PropList "diagnosis" (fmap Xmlbf.toXml (encounterDiagnosis p))
             , PropList "account" (fmap Xmlbf.toXml (encounterAccount p))
             , OptProp  "hospitalization" (fmap Xmlbf.toXml (encounterHospitalization p))
             , PropList "location" (fmap Xmlbf.toXml (encounterLocation p))
             , OptProp  "serviceProvider" (fmap Xmlbf.toXml (encounterServiceProvider p))
             , OptProp  "partOf" (fmap Xmlbf.toXml (encounterPartOf p))
             ]
instance Xmlbf.FromXml Encounter where
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
    statusHistory <- many     $ Xmlbf.pElement "statusHistory" Xmlbf.fromXml
    cl <-            Xmlbf.pElement "class" Xmlbf.fromXml
    classHistory <- many     $ Xmlbf.pElement "classHistory" Xmlbf.fromXml
    ty <- many     $ Xmlbf.pElement "type" Xmlbf.fromXml
    serviceType <- optional $ Xmlbf.pElement "serviceType" Xmlbf.fromXml
    priority <- optional $ Xmlbf.pElement "priority" Xmlbf.fromXml
    subject <- optional $ Xmlbf.pElement "subject" Xmlbf.fromXml
    episodeOfCare <- many     $ Xmlbf.pElement "episodeOfCare" Xmlbf.fromXml
    basedOn <- many     $ Xmlbf.pElement "basedOn" Xmlbf.fromXml
    participant <- many     $ Xmlbf.pElement "participant" Xmlbf.fromXml
    appointment <- many     $ Xmlbf.pElement "appointment" Xmlbf.fromXml
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    length <- optional $ Xmlbf.pElement "length" Xmlbf.fromXml
    reasonCode <- many     $ Xmlbf.pElement "reasonCode" Xmlbf.fromXml
    reasonReference <- many     $ Xmlbf.pElement "reasonReference" Xmlbf.fromXml
    diagnosis <- many     $ Xmlbf.pElement "diagnosis" Xmlbf.fromXml
    account <- many     $ Xmlbf.pElement "account" Xmlbf.fromXml
    hospitalization <- optional $ Xmlbf.pElement "hospitalization" Xmlbf.fromXml
    location <- many     $ Xmlbf.pElement "location" Xmlbf.fromXml
    serviceProvider <- optional $ Xmlbf.pElement "serviceProvider" Xmlbf.fromXml
    partOf <- optional $ Xmlbf.pElement "partOf" Xmlbf.fromXml
    return Encounter {
            encounterId = fmap fromId id
          , encounterMeta = meta
          , encounterImplicitRules = fmap fromUri implicitRules
          , encounterLanguage = fmap fromLanguage language
          , encounterText = text
--          , encounterContained = contained
          , encounterExtension = extension
          , encounterModifierExtension = modifierExtension
          , encounterIdentifier = identifier
          , encounterStatus =      fromEncounterStatus status
          , encounterStatusHistory = statusHistory
          , encounterClass = cl
          , encounterClassHistory = classHistory
          , encounterType = ty
          , encounterServiceType = serviceType
          , encounterPriority = priority
          , encounterSubject = subject
          , encounterEpisodeOfCare = episodeOfCare
          , encounterBasedOn = basedOn
          , encounterParticipant = participant
          , encounterAppointment = appointment
          , encounterPeriod = period
          , encounterLength = length
          , encounterReasonCode = reasonCode
          , encounterReasonReference = reasonReference
          , encounterDiagnosis = diagnosis
          , encounterAccount = account
          , encounterHospitalization = hospitalization
          , encounterLocation = location
          , encounterServiceProvider = serviceProvider
          , encounterPartOf = partOf
          }



data EncounterParticipant = EncounterParticipant {
    encounterParticipantAttrId :: Maybe Text
  , encounterParticipantExtension :: [Extension]
  , encounterParticipantModifierExtension :: [Extension]
  , encounterParticipantType :: [CodeableConcept]
  , encounterParticipantPeriod :: Maybe Period
  , encounterParticipantIndividual :: Maybe Reference
  } deriving (Eq, Show)
--

instance ToJSON EncounterParticipant where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (encounterParticipantAttrId p)
    ,  "extension" .= toJSON (encounterParticipantExtension p)
    ,  "modifierExtension" .= toJSON (encounterParticipantModifierExtension p)
    ,  "type" .= toJSON (encounterParticipantType p)
    ,  "period" .= toJSON (encounterParticipantPeriod p)
    ,  "individual" .= toJSON (encounterParticipantIndividual p)
    ]
instance FromJSON EncounterParticipant where
  parseJSON = withObject "EncounterParticipant" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:? "type" .!= []
        period <- o .:? "period"
        individual <- o .:? "individual"
        return EncounterParticipant{
            encounterParticipantAttrId = id
          , encounterParticipantExtension = extension
          , encounterParticipantModifierExtension = modifierExtension
          , encounterParticipantType = ty
          , encounterParticipantPeriod = period
          , encounterParticipantIndividual = individual
          }
instance Xmlbf.ToXml EncounterParticipant where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (encounterParticipantAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (encounterParticipantExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (encounterParticipantModifierExtension p))
             , PropList "type" (fmap Xmlbf.toXml (encounterParticipantType p))
             , OptProp  "period" (fmap Xmlbf.toXml (encounterParticipantPeriod p))
             , OptProp  "individual" (fmap Xmlbf.toXml (encounterParticipantIndividual p))
             ]
instance Xmlbf.FromXml EncounterParticipant where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <- many     $ Xmlbf.pElement "type" Xmlbf.fromXml
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    individual <- optional $ Xmlbf.pElement "individual" Xmlbf.fromXml
    return EncounterParticipant {
            encounterParticipantAttrId = id
          , encounterParticipantExtension = extension
          , encounterParticipantModifierExtension = modifierExtension
          , encounterParticipantType = ty
          , encounterParticipantPeriod = period
          , encounterParticipantIndividual = individual
          }



data EncounterStatusHistoryStatus
    = ESHSPlanned
    | ESHSArrived
    | ESHSTriaged
    | ESHSInProgress
    | ESHSOnleave
    | ESHSFinished
    | ESHSCancelled
    | ESHSEnteredInError
    | ESHSUnknown
  deriving (Eq, Show)

instance ToJSON EncounterStatusHistoryStatus where
    toJSON ESHSPlanned = String "planned"
    toJSON ESHSArrived = String "arrived"
    toJSON ESHSTriaged = String "triaged"
    toJSON ESHSInProgress = String "in-progress"
    toJSON ESHSOnleave = String "onleave"
    toJSON ESHSFinished = String "finished"
    toJSON ESHSCancelled = String "cancelled"
    toJSON ESHSEnteredInError = String "entered-in-error"
    toJSON ESHSUnknown = String "unknown"
instance FromJSON EncounterStatusHistoryStatus where
    parseJSON "planned" = return ESHSPlanned
    parseJSON "arrived" = return ESHSArrived
    parseJSON "triaged" = return ESHSTriaged
    parseJSON "in-progress" = return ESHSInProgress
    parseJSON "onleave" = return ESHSOnleave
    parseJSON "finished" = return ESHSFinished
    parseJSON "cancelled" = return ESHSCancelled
    parseJSON "entered-in-error" = return ESHSEnteredInError
    parseJSON "unknown" = return ESHSUnknown

toEncounterStatusHistoryStatus ESHSPlanned = "planned"
toEncounterStatusHistoryStatus ESHSArrived = "arrived"
toEncounterStatusHistoryStatus ESHSTriaged = "triaged"
toEncounterStatusHistoryStatus ESHSInProgress = "in-progress"
toEncounterStatusHistoryStatus ESHSOnleave = "onleave"
toEncounterStatusHistoryStatus ESHSFinished = "finished"
toEncounterStatusHistoryStatus ESHSCancelled = "cancelled"
toEncounterStatusHistoryStatus ESHSEnteredInError = "entered-in-error"
toEncounterStatusHistoryStatus ESHSUnknown = "unknown"
fromEncounterStatusHistoryStatus "planned" = ESHSPlanned
fromEncounterStatusHistoryStatus "arrived" = ESHSArrived
fromEncounterStatusHistoryStatus "triaged" = ESHSTriaged
fromEncounterStatusHistoryStatus "in-progress" = ESHSInProgress
fromEncounterStatusHistoryStatus "onleave" = ESHSOnleave
fromEncounterStatusHistoryStatus "finished" = ESHSFinished
fromEncounterStatusHistoryStatus "cancelled" = ESHSCancelled
fromEncounterStatusHistoryStatus "entered-in-error" = ESHSEnteredInError
fromEncounterStatusHistoryStatus "unknown" = ESHSUnknown


data EncounterStatusHistory = EncounterStatusHistory {
    encounterStatusHistoryAttrId :: Maybe Text
  , encounterStatusHistoryExtension :: [Extension]
  , encounterStatusHistoryModifierExtension :: [Extension]
  , encounterStatusHistoryStatus :: EncounterStatusHistoryStatus
  , encounterStatusHistoryPeriod :: Period
  } deriving (Eq, Show)
--

instance ToJSON EncounterStatusHistory where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (encounterStatusHistoryAttrId p)
    ,  "extension" .= toJSON (encounterStatusHistoryExtension p)
    ,  "modifierExtension" .= toJSON (encounterStatusHistoryModifierExtension p)
    ,  "status" .= toJSON (encounterStatusHistoryStatus p)
    ,  "period" .= toJSON (encounterStatusHistoryPeriod p)
    ]
instance FromJSON EncounterStatusHistory where
  parseJSON = withObject "EncounterStatusHistory" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        status <- o .:  "status"
        period <- o .:  "period"
        return EncounterStatusHistory{
            encounterStatusHistoryAttrId = id
          , encounterStatusHistoryExtension = extension
          , encounterStatusHistoryModifierExtension = modifierExtension
          , encounterStatusHistoryStatus = status
          , encounterStatusHistoryPeriod = period
          }
instance Xmlbf.ToXml EncounterStatusHistory where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (encounterStatusHistoryAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (encounterStatusHistoryExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (encounterStatusHistoryModifierExtension p))
             , Val      "status" (     toEncounterStatusHistoryStatus (encounterStatusHistoryStatus p))
             , Prop     "period" (HM.empty, Xmlbf.toXml (encounterStatusHistoryPeriod p))
             ]
instance Xmlbf.FromXml EncounterStatusHistory where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    period <-            Xmlbf.pElement "period" Xmlbf.fromXml
    return EncounterStatusHistory {
            encounterStatusHistoryAttrId = id
          , encounterStatusHistoryExtension = extension
          , encounterStatusHistoryModifierExtension = modifierExtension
          , encounterStatusHistoryStatus =      fromEncounterStatusHistoryStatus status
          , encounterStatusHistoryPeriod = period
          }



data EncounterClassHistory = EncounterClassHistory {
    encounterClassHistoryAttrId :: Maybe Text
  , encounterClassHistoryExtension :: [Extension]
  , encounterClassHistoryModifierExtension :: [Extension]
  , encounterClassHistoryClass :: Coding
  , encounterClassHistoryPeriod :: Period
  } deriving (Eq, Show)
--

instance ToJSON EncounterClassHistory where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (encounterClassHistoryAttrId p)
    ,  "extension" .= toJSON (encounterClassHistoryExtension p)
    ,  "modifierExtension" .= toJSON (encounterClassHistoryModifierExtension p)
    ,  "class" .= toJSON (encounterClassHistoryClass p)
    ,  "period" .= toJSON (encounterClassHistoryPeriod p)
    ]
instance FromJSON EncounterClassHistory where
  parseJSON = withObject "EncounterClassHistory" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        cl <- o .:  "class"
        period <- o .:  "period"
        return EncounterClassHistory{
            encounterClassHistoryAttrId = id
          , encounterClassHistoryExtension = extension
          , encounterClassHistoryModifierExtension = modifierExtension
          , encounterClassHistoryClass = cl
          , encounterClassHistoryPeriod = period
          }
instance Xmlbf.ToXml EncounterClassHistory where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (encounterClassHistoryAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (encounterClassHistoryExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (encounterClassHistoryModifierExtension p))
             , Prop     "class" (HM.empty, Xmlbf.toXml (encounterClassHistoryClass p))
             , Prop     "period" (HM.empty, Xmlbf.toXml (encounterClassHistoryPeriod p))
             ]
instance Xmlbf.FromXml EncounterClassHistory where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    cl <-            Xmlbf.pElement "class" Xmlbf.fromXml
    period <-            Xmlbf.pElement "period" Xmlbf.fromXml
    return EncounterClassHistory {
            encounterClassHistoryAttrId = id
          , encounterClassHistoryExtension = extension
          , encounterClassHistoryModifierExtension = modifierExtension
          , encounterClassHistoryClass = cl
          , encounterClassHistoryPeriod = period
          }



data EncounterDiagnosis = EncounterDiagnosis {
    encounterDiagnosisAttrId :: Maybe Text
  , encounterDiagnosisExtension :: [Extension]
  , encounterDiagnosisModifierExtension :: [Extension]
  , encounterDiagnosisCondition :: Reference
  , encounterDiagnosisUse :: Maybe CodeableConcept
  , encounterDiagnosisRank :: Maybe PositiveInt
  } deriving (Eq, Show)
--

instance ToJSON EncounterDiagnosis where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (encounterDiagnosisAttrId p)
    ,  "extension" .= toJSON (encounterDiagnosisExtension p)
    ,  "modifierExtension" .= toJSON (encounterDiagnosisModifierExtension p)
    ,  "condition" .= toJSON (encounterDiagnosisCondition p)
    ,  "use" .= toJSON (encounterDiagnosisUse p)
    ,  "rank" .= toJSON (encounterDiagnosisRank p)
    ]
instance FromJSON EncounterDiagnosis where
  parseJSON = withObject "EncounterDiagnosis" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        condition <- o .:  "condition"
        use <- o .:? "use"
        rank <- o .:? "rank"
        return EncounterDiagnosis{
            encounterDiagnosisAttrId = id
          , encounterDiagnosisExtension = extension
          , encounterDiagnosisModifierExtension = modifierExtension
          , encounterDiagnosisCondition = condition
          , encounterDiagnosisUse = use
          , encounterDiagnosisRank = rank
          }
instance Xmlbf.ToXml EncounterDiagnosis where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (encounterDiagnosisAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (encounterDiagnosisExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (encounterDiagnosisModifierExtension p))
             , Prop     "condition" (HM.empty, Xmlbf.toXml (encounterDiagnosisCondition p))
             , OptProp  "use" (fmap Xmlbf.toXml (encounterDiagnosisUse p))
             , OptVal   "rank" (fmap toPositiveInt (encounterDiagnosisRank p))
             ]
instance Xmlbf.FromXml EncounterDiagnosis where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    condition <-            Xmlbf.pElement "condition" Xmlbf.fromXml
    use <- optional $ Xmlbf.pElement "use" Xmlbf.fromXml
    rank <- optional $ Xmlbf.pElement "rank" (Xmlbf.pAttr "value")
    return EncounterDiagnosis {
            encounterDiagnosisAttrId = id
          , encounterDiagnosisExtension = extension
          , encounterDiagnosisModifierExtension = modifierExtension
          , encounterDiagnosisCondition = condition
          , encounterDiagnosisUse = use
          , encounterDiagnosisRank = fmap fromPositiveInt rank
          }



data EncounterLocationStatus
    = ELSPlanned
    | ELSActive
    | ELSReserved
    | ELSCompleted
  deriving (Eq, Show)

instance ToJSON EncounterLocationStatus where
    toJSON ELSPlanned = String "planned"
    toJSON ELSActive = String "active"
    toJSON ELSReserved = String "reserved"
    toJSON ELSCompleted = String "completed"
instance FromJSON EncounterLocationStatus where
    parseJSON "planned" = return ELSPlanned
    parseJSON "active" = return ELSActive
    parseJSON "reserved" = return ELSReserved
    parseJSON "completed" = return ELSCompleted

toEncounterLocationStatus ELSPlanned = "planned"
toEncounterLocationStatus ELSActive = "active"
toEncounterLocationStatus ELSReserved = "reserved"
toEncounterLocationStatus ELSCompleted = "completed"
fromEncounterLocationStatus "planned" = ELSPlanned
fromEncounterLocationStatus "active" = ELSActive
fromEncounterLocationStatus "reserved" = ELSReserved
fromEncounterLocationStatus "completed" = ELSCompleted


data EncounterLocation = EncounterLocation {
    encounterLocationAttrId :: Maybe Text
  , encounterLocationExtension :: [Extension]
  , encounterLocationModifierExtension :: [Extension]
  , encounterLocationLocation :: Reference
  , encounterLocationStatus :: Maybe EncounterLocationStatus
  , encounterLocationPhysicalType :: Maybe CodeableConcept
  , encounterLocationPeriod :: Maybe Period
  } deriving (Eq, Show)
--

instance ToJSON EncounterLocation where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (encounterLocationAttrId p)
    ,  "extension" .= toJSON (encounterLocationExtension p)
    ,  "modifierExtension" .= toJSON (encounterLocationModifierExtension p)
    ,  "location" .= toJSON (encounterLocationLocation p)
    ,  "status" .= toJSON (encounterLocationStatus p)
    ,  "physicalType" .= toJSON (encounterLocationPhysicalType p)
    ,  "period" .= toJSON (encounterLocationPeriod p)
    ]
instance FromJSON EncounterLocation where
  parseJSON = withObject "EncounterLocation" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        location <- o .:  "location"
        status <- o .:? "status"
        physicalType <- o .:? "physicalType"
        period <- o .:? "period"
        return EncounterLocation{
            encounterLocationAttrId = id
          , encounterLocationExtension = extension
          , encounterLocationModifierExtension = modifierExtension
          , encounterLocationLocation = location
          , encounterLocationStatus = status
          , encounterLocationPhysicalType = physicalType
          , encounterLocationPeriod = period
          }
instance Xmlbf.ToXml EncounterLocation where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (encounterLocationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (encounterLocationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (encounterLocationModifierExtension p))
             , Prop     "location" (HM.empty, Xmlbf.toXml (encounterLocationLocation p))
             , OptVal   "status" (fmap toEncounterLocationStatus (encounterLocationStatus p))
             , OptProp  "physicalType" (fmap Xmlbf.toXml (encounterLocationPhysicalType p))
             , OptProp  "period" (fmap Xmlbf.toXml (encounterLocationPeriod p))
             ]
instance Xmlbf.FromXml EncounterLocation where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    location <-            Xmlbf.pElement "location" Xmlbf.fromXml
    status <- optional $ Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    physicalType <- optional $ Xmlbf.pElement "physicalType" Xmlbf.fromXml
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    return EncounterLocation {
            encounterLocationAttrId = id
          , encounterLocationExtension = extension
          , encounterLocationModifierExtension = modifierExtension
          , encounterLocationLocation = location
          , encounterLocationStatus = fmap fromEncounterLocationStatus status
          , encounterLocationPhysicalType = physicalType
          , encounterLocationPeriod = period
          }



data EncounterHospitalization = EncounterHospitalization {
    encounterHospitalizationAttrId :: Maybe Text
  , encounterHospitalizationExtension :: [Extension]
  , encounterHospitalizationModifierExtension :: [Extension]
  , encounterHospitalizationPreAdmissionIdentifier :: Maybe Identifier
  , encounterHospitalizationOrigin :: Maybe Reference
  , encounterHospitalizationAdmitSource :: Maybe CodeableConcept
  , encounterHospitalizationReAdmission :: Maybe CodeableConcept
  , encounterHospitalizationDietPreference :: [CodeableConcept]
  , encounterHospitalizationSpecialCourtesy :: [CodeableConcept]
  , encounterHospitalizationSpecialArrangement :: [CodeableConcept]
  , encounterHospitalizationDestination :: Maybe Reference
  , encounterHospitalizationDischargeDisposition :: Maybe CodeableConcept
  } deriving (Eq, Show)
--

instance ToJSON EncounterHospitalization where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (encounterHospitalizationAttrId p)
    ,  "extension" .= toJSON (encounterHospitalizationExtension p)
    ,  "modifierExtension" .= toJSON (encounterHospitalizationModifierExtension p)
    ,  "preAdmissionIdentifier" .= toJSON (encounterHospitalizationPreAdmissionIdentifier p)
    ,  "origin" .= toJSON (encounterHospitalizationOrigin p)
    ,  "admitSource" .= toJSON (encounterHospitalizationAdmitSource p)
    ,  "reAdmission" .= toJSON (encounterHospitalizationReAdmission p)
    ,  "dietPreference" .= toJSON (encounterHospitalizationDietPreference p)
    ,  "specialCourtesy" .= toJSON (encounterHospitalizationSpecialCourtesy p)
    ,  "specialArrangement" .= toJSON (encounterHospitalizationSpecialArrangement p)
    ,  "destination" .= toJSON (encounterHospitalizationDestination p)
    ,  "dischargeDisposition" .= toJSON (encounterHospitalizationDischargeDisposition p)
    ]
instance FromJSON EncounterHospitalization where
  parseJSON = withObject "EncounterHospitalization" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        preAdmissionIdentifier <- o .:? "preAdmissionIdentifier"
        origin <- o .:? "origin"
        admitSource <- o .:? "admitSource"
        reAdmission <- o .:? "reAdmission"
        dietPreference <- o .:? "dietPreference" .!= []
        specialCourtesy <- o .:? "specialCourtesy" .!= []
        specialArrangement <- o .:? "specialArrangement" .!= []
        destination <- o .:? "destination"
        dischargeDisposition <- o .:? "dischargeDisposition"
        return EncounterHospitalization{
            encounterHospitalizationAttrId = id
          , encounterHospitalizationExtension = extension
          , encounterHospitalizationModifierExtension = modifierExtension
          , encounterHospitalizationPreAdmissionIdentifier = preAdmissionIdentifier
          , encounterHospitalizationOrigin = origin
          , encounterHospitalizationAdmitSource = admitSource
          , encounterHospitalizationReAdmission = reAdmission
          , encounterHospitalizationDietPreference = dietPreference
          , encounterHospitalizationSpecialCourtesy = specialCourtesy
          , encounterHospitalizationSpecialArrangement = specialArrangement
          , encounterHospitalizationDestination = destination
          , encounterHospitalizationDischargeDisposition = dischargeDisposition
          }
instance Xmlbf.ToXml EncounterHospitalization where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (encounterHospitalizationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (encounterHospitalizationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (encounterHospitalizationModifierExtension p))
             , OptProp  "preAdmissionIdentifier" (fmap Xmlbf.toXml (encounterHospitalizationPreAdmissionIdentifier p))
             , OptProp  "origin" (fmap Xmlbf.toXml (encounterHospitalizationOrigin p))
             , OptProp  "admitSource" (fmap Xmlbf.toXml (encounterHospitalizationAdmitSource p))
             , OptProp  "reAdmission" (fmap Xmlbf.toXml (encounterHospitalizationReAdmission p))
             , PropList "dietPreference" (fmap Xmlbf.toXml (encounterHospitalizationDietPreference p))
             , PropList "specialCourtesy" (fmap Xmlbf.toXml (encounterHospitalizationSpecialCourtesy p))
             , PropList "specialArrangement" (fmap Xmlbf.toXml (encounterHospitalizationSpecialArrangement p))
             , OptProp  "destination" (fmap Xmlbf.toXml (encounterHospitalizationDestination p))
             , OptProp  "dischargeDisposition" (fmap Xmlbf.toXml (encounterHospitalizationDischargeDisposition p))
             ]
instance Xmlbf.FromXml EncounterHospitalization where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    preAdmissionIdentifier <- optional $ Xmlbf.pElement "preAdmissionIdentifier" Xmlbf.fromXml
    origin <- optional $ Xmlbf.pElement "origin" Xmlbf.fromXml
    admitSource <- optional $ Xmlbf.pElement "admitSource" Xmlbf.fromXml
    reAdmission <- optional $ Xmlbf.pElement "reAdmission" Xmlbf.fromXml
    dietPreference <- many     $ Xmlbf.pElement "dietPreference" Xmlbf.fromXml
    specialCourtesy <- many     $ Xmlbf.pElement "specialCourtesy" Xmlbf.fromXml
    specialArrangement <- many     $ Xmlbf.pElement "specialArrangement" Xmlbf.fromXml
    destination <- optional $ Xmlbf.pElement "destination" Xmlbf.fromXml
    dischargeDisposition <- optional $ Xmlbf.pElement "dischargeDisposition" Xmlbf.fromXml
    return EncounterHospitalization {
            encounterHospitalizationAttrId = id
          , encounterHospitalizationExtension = extension
          , encounterHospitalizationModifierExtension = modifierExtension
          , encounterHospitalizationPreAdmissionIdentifier = preAdmissionIdentifier
          , encounterHospitalizationOrigin = origin
          , encounterHospitalizationAdmitSource = admitSource
          , encounterHospitalizationReAdmission = reAdmission
          , encounterHospitalizationDietPreference = dietPreference
          , encounterHospitalizationSpecialCourtesy = specialCourtesy
          , encounterHospitalizationSpecialArrangement = specialArrangement
          , encounterHospitalizationDestination = destination
          , encounterHospitalizationDischargeDisposition = dischargeDisposition
          }




