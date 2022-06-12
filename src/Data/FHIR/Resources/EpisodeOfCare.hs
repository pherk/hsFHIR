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
-- FHIR 4.0.0 EpisodeOfCare
--

module Data.FHIR.Resources.EpisodeOfCare where

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

data EpisodeOfCareStatus
    = EOCSPlanned
    | EOCSWaitlist
    | EOCSActive
    | EOCSOnhold
    | EOCSFinished
    | EOCSCancelled
    | EOCSEnteredInError
  deriving (Eq, Show)

instance ToJSON EpisodeOfCareStatus where
    toJSON EOCSPlanned = String "planned"
    toJSON EOCSWaitlist = String "waitlist"
    toJSON EOCSActive = String "active"
    toJSON EOCSOnhold = String "onhold"
    toJSON EOCSFinished = String "finished"
    toJSON EOCSCancelled = String "cancelled"
    toJSON EOCSEnteredInError = String "entered-in-error"
instance FromJSON EpisodeOfCareStatus where
    parseJSON "planned" = return EOCSPlanned
    parseJSON "waitlist" = return EOCSWaitlist
    parseJSON "active" = return EOCSActive
    parseJSON "onhold" = return EOCSOnhold
    parseJSON "finished" = return EOCSFinished
    parseJSON "cancelled" = return EOCSCancelled
    parseJSON "entered-in-error" = return EOCSEnteredInError

toEpisodeOfCareStatus EOCSPlanned = "planned"
toEpisodeOfCareStatus EOCSWaitlist = "waitlist"
toEpisodeOfCareStatus EOCSActive = "active"
toEpisodeOfCareStatus EOCSOnhold = "onhold"
toEpisodeOfCareStatus EOCSFinished = "finished"
toEpisodeOfCareStatus EOCSCancelled = "cancelled"
toEpisodeOfCareStatus EOCSEnteredInError = "entered-in-error"
fromEpisodeOfCareStatus "planned" = EOCSPlanned
fromEpisodeOfCareStatus "waitlist" = EOCSWaitlist
fromEpisodeOfCareStatus "active" = EOCSActive
fromEpisodeOfCareStatus "onhold" = EOCSOnhold
fromEpisodeOfCareStatus "finished" = EOCSFinished
fromEpisodeOfCareStatus "cancelled" = EOCSCancelled
fromEpisodeOfCareStatus "entered-in-error" = EOCSEnteredInError


data EpisodeOfCare = EpisodeOfCare {
    episodeOfCareId :: Maybe Id
  , episodeOfCareMeta :: Maybe Meta
  , episodeOfCareImplicitRules :: Maybe Uri
  , episodeOfCareLanguage :: Maybe Language
  , episodeOfCareText :: Maybe Narrative
--    episodeOfCareContained :: [ResourceContainer]
  , episodeOfCareExtension :: [Extension]
  , episodeOfCareModifierExtension :: [Extension]
  , episodeOfCareIdentifier :: [Identifier]
  , episodeOfCareStatus :: EpisodeOfCareStatus
  , episodeOfCareStatusHistory :: [EpisodeOfCareStatusHistory]
  , episodeOfCareType :: [CodeableConcept]
  , episodeOfCareDiagnosis :: [EpisodeOfCareDiagnosis]
  , episodeOfCarePatient :: Reference
  , episodeOfCareManagingOrganization :: Maybe Reference
  , episodeOfCarePeriod :: Maybe Period
  , episodeOfCareReferralRequest :: [Reference]
  , episodeOfCareCareManager :: Maybe Reference
  , episodeOfCareTeam :: [Reference]
  , episodeOfCareAccount :: [Reference]
  }
  deriving (Eq, Show)
--

instance ToJSON EpisodeOfCare where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "EpisodeOfCare")
    ,  "id" .= toJSON (episodeOfCareId p)
    ,  "meta" .= toJSON (episodeOfCareMeta p)
    ,  "implicitRules" .= toJSON (episodeOfCareImplicitRules p)
    ,  "language" .= toJSON (episodeOfCareLanguage p)
    ,  "text" .= toJSON (episodeOfCareText p)
--    , "contained" .= toJSON (episodeOfCareContained p)
    ,  "extension" .= toJSON (episodeOfCareExtension p)
    ,  "modifierExtension" .= toJSON (episodeOfCareModifierExtension p)
    ,  "identifier" .= toJSON (episodeOfCareIdentifier p)
    ,  "status" .= toJSON (episodeOfCareStatus p)
    ,  "statusHistory" .= toJSON (episodeOfCareStatusHistory p)
    ,  "type" .= toJSON (episodeOfCareType p)
    ,  "diagnosis" .= toJSON (episodeOfCareDiagnosis p)
    ,  "patient" .= toJSON (episodeOfCarePatient p)
    ,  "managingOrganization" .= toJSON (episodeOfCareManagingOrganization p)
    ,  "period" .= toJSON (episodeOfCarePeriod p)
    ,  "referralRequest" .= toJSON (episodeOfCareReferralRequest p)
    ,  "careManager" .= toJSON (episodeOfCareCareManager p)
    ,  "team" .= toJSON (episodeOfCareTeam p)
    ,  "account" .= toJSON (episodeOfCareAccount p)
    ]
instance FromJSON EpisodeOfCare where
  parseJSON = withObject "EpisodeOfCare" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "EpisodeOfCare" -> do
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
        ty <- o .:? "type" .!= []
        diagnosis <- o .:? "diagnosis" .!= []
        patient <- o .:  "patient"
        managingOrganization <- o .:? "managingOrganization"
        period <- o .:? "period"
        referralRequest <- o .:? "referralRequest" .!= []
        careManager <- o .:? "careManager"
        team <- o .:? "team" .!= []
        account <- o .:? "account" .!= []
        return EpisodeOfCare{
            episodeOfCareId = id
          , episodeOfCareMeta = meta
          , episodeOfCareImplicitRules = implicitRules
          , episodeOfCareLanguage = language
          , episodeOfCareText = text
--          , episodeOfCareContained = contained
          , episodeOfCareExtension = extension
          , episodeOfCareModifierExtension = modifierExtension
          , episodeOfCareIdentifier = identifier
          , episodeOfCareStatus = status
          , episodeOfCareStatusHistory = statusHistory
          , episodeOfCareType = ty
          , episodeOfCareDiagnosis = diagnosis
          , episodeOfCarePatient = patient
          , episodeOfCareManagingOrganization = managingOrganization
          , episodeOfCarePeriod = period
          , episodeOfCareReferralRequest = referralRequest
          , episodeOfCareCareManager = careManager
          , episodeOfCareTeam = team
          , episodeOfCareAccount = account
          }
      _ -> fail "not a EpisodeOfCare"
instance Xmlbf.ToXml EpisodeOfCare where
  toXml p = Xmlbf.element "EpisodeOfCare" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (episodeOfCareId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (episodeOfCareMeta p))
             , OptVal   "implicitRules" (fmap toUri (episodeOfCareImplicitRules p))
             , OptVal   "language" (fmap toLanguage (episodeOfCareLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (episodeOfCareText p))
--             , PropList "contained" (fmap Xmlbf.toXml (episodeOfCareContained p))
             , PropList "extension" (fmap Xmlbf.toXml (episodeOfCareExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (episodeOfCareModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (episodeOfCareIdentifier p))
             , Val      "status" (     toEpisodeOfCareStatus (episodeOfCareStatus p))
             , PropList "statusHistory" (fmap Xmlbf.toXml (episodeOfCareStatusHistory p))
             , PropList "type" (fmap Xmlbf.toXml (episodeOfCareType p))
             , PropList "diagnosis" (fmap Xmlbf.toXml (episodeOfCareDiagnosis p))
             , Prop     "patient" (HM.empty, Xmlbf.toXml (episodeOfCarePatient p))
             , OptProp  "managingOrganization" (fmap Xmlbf.toXml (episodeOfCareManagingOrganization p))
             , OptProp  "period" (fmap Xmlbf.toXml (episodeOfCarePeriod p))
             , PropList "referralRequest" (fmap Xmlbf.toXml (episodeOfCareReferralRequest p))
             , OptProp  "careManager" (fmap Xmlbf.toXml (episodeOfCareCareManager p))
             , PropList "team" (fmap Xmlbf.toXml (episodeOfCareTeam p))
             , PropList "account" (fmap Xmlbf.toXml (episodeOfCareAccount p))
             ]
instance Xmlbf.FromXml EpisodeOfCare where
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
    ty <- many     $ Xmlbf.pElement "type" Xmlbf.fromXml
    diagnosis <- many     $ Xmlbf.pElement "diagnosis" Xmlbf.fromXml
    patient <-            Xmlbf.pElement "patient" Xmlbf.fromXml
    managingOrganization <- optional $ Xmlbf.pElement "managingOrganization" Xmlbf.fromXml
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    referralRequest <- many     $ Xmlbf.pElement "referralRequest" Xmlbf.fromXml
    careManager <- optional $ Xmlbf.pElement "careManager" Xmlbf.fromXml
    team <- many     $ Xmlbf.pElement "team" Xmlbf.fromXml
    account <- many     $ Xmlbf.pElement "account" Xmlbf.fromXml
    return EpisodeOfCare {
            episodeOfCareId = fmap fromId id
          , episodeOfCareMeta = meta
          , episodeOfCareImplicitRules = fmap fromUri implicitRules
          , episodeOfCareLanguage = fmap fromLanguage language
          , episodeOfCareText = text
--          , episodeOfCareContained = contained
          , episodeOfCareExtension = extension
          , episodeOfCareModifierExtension = modifierExtension
          , episodeOfCareIdentifier = identifier
          , episodeOfCareStatus =      fromEpisodeOfCareStatus status
          , episodeOfCareStatusHistory = statusHistory
          , episodeOfCareType = ty
          , episodeOfCareDiagnosis = diagnosis
          , episodeOfCarePatient = patient
          , episodeOfCareManagingOrganization = managingOrganization
          , episodeOfCarePeriod = period
          , episodeOfCareReferralRequest = referralRequest
          , episodeOfCareCareManager = careManager
          , episodeOfCareTeam = team
          , episodeOfCareAccount = account
          }



data EpisodeOfCareStatusHistoryStatus
    = EOCSHSPlanned
    | EOCSHSWaitlist
    | EOCSHSActive
    | EOCSHSOnhold
    | EOCSHSFinished
    | EOCSHSCancelled
    | EOCSHSEnteredInError
  deriving (Eq, Show)

instance ToJSON EpisodeOfCareStatusHistoryStatus where
    toJSON EOCSHSPlanned = String "planned"
    toJSON EOCSHSWaitlist = String "waitlist"
    toJSON EOCSHSActive = String "active"
    toJSON EOCSHSOnhold = String "onhold"
    toJSON EOCSHSFinished = String "finished"
    toJSON EOCSHSCancelled = String "cancelled"
    toJSON EOCSHSEnteredInError = String "entered-in-error"
instance FromJSON EpisodeOfCareStatusHistoryStatus where
    parseJSON "planned" = return EOCSHSPlanned
    parseJSON "waitlist" = return EOCSHSWaitlist
    parseJSON "active" = return EOCSHSActive
    parseJSON "onhold" = return EOCSHSOnhold
    parseJSON "finished" = return EOCSHSFinished
    parseJSON "cancelled" = return EOCSHSCancelled
    parseJSON "entered-in-error" = return EOCSHSEnteredInError

toEpisodeOfCareStatusHistoryStatus EOCSHSPlanned = "planned"
toEpisodeOfCareStatusHistoryStatus EOCSHSWaitlist = "waitlist"
toEpisodeOfCareStatusHistoryStatus EOCSHSActive = "active"
toEpisodeOfCareStatusHistoryStatus EOCSHSOnhold = "onhold"
toEpisodeOfCareStatusHistoryStatus EOCSHSFinished = "finished"
toEpisodeOfCareStatusHistoryStatus EOCSHSCancelled = "cancelled"
toEpisodeOfCareStatusHistoryStatus EOCSHSEnteredInError = "entered-in-error"
fromEpisodeOfCareStatusHistoryStatus "planned" = EOCSHSPlanned
fromEpisodeOfCareStatusHistoryStatus "waitlist" = EOCSHSWaitlist
fromEpisodeOfCareStatusHistoryStatus "active" = EOCSHSActive
fromEpisodeOfCareStatusHistoryStatus "onhold" = EOCSHSOnhold
fromEpisodeOfCareStatusHistoryStatus "finished" = EOCSHSFinished
fromEpisodeOfCareStatusHistoryStatus "cancelled" = EOCSHSCancelled
fromEpisodeOfCareStatusHistoryStatus "entered-in-error" = EOCSHSEnteredInError


data EpisodeOfCareStatusHistory = EpisodeOfCareStatusHistory {
    episodeOfCareStatusHistoryAttrId :: Maybe Text
  , episodeOfCareStatusHistoryExtension :: [Extension]
  , episodeOfCareStatusHistoryModifierExtension :: [Extension]
  , episodeOfCareStatusHistoryStatus :: EpisodeOfCareStatusHistoryStatus
  , episodeOfCareStatusHistoryPeriod :: Period
  }
  deriving (Eq, Show)
--

instance ToJSON EpisodeOfCareStatusHistory where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (episodeOfCareStatusHistoryAttrId p)
    ,  "extension" .= toJSON (episodeOfCareStatusHistoryExtension p)
    ,  "modifierExtension" .= toJSON (episodeOfCareStatusHistoryModifierExtension p)
    ,  "status" .= toJSON (episodeOfCareStatusHistoryStatus p)
    ,  "period" .= toJSON (episodeOfCareStatusHistoryPeriod p)
    ]
instance FromJSON EpisodeOfCareStatusHistory where
  parseJSON = withObject "EpisodeOfCareStatusHistory" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        status <- o .:  "status"
        period <- o .:  "period"
        return EpisodeOfCareStatusHistory{
            episodeOfCareStatusHistoryAttrId = id
          , episodeOfCareStatusHistoryExtension = extension
          , episodeOfCareStatusHistoryModifierExtension = modifierExtension
          , episodeOfCareStatusHistoryStatus = status
          , episodeOfCareStatusHistoryPeriod = period
          }
instance Xmlbf.ToXml EpisodeOfCareStatusHistory where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (episodeOfCareStatusHistoryAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (episodeOfCareStatusHistoryExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (episodeOfCareStatusHistoryModifierExtension p))
             , Val      "status" (     toEpisodeOfCareStatusHistoryStatus (episodeOfCareStatusHistoryStatus p))
             , Prop     "period" (HM.empty, Xmlbf.toXml (episodeOfCareStatusHistoryPeriod p))
             ]
instance Xmlbf.FromXml EpisodeOfCareStatusHistory where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    period <-            Xmlbf.pElement "period" Xmlbf.fromXml
    return EpisodeOfCareStatusHistory {
            episodeOfCareStatusHistoryAttrId = id
          , episodeOfCareStatusHistoryExtension = extension
          , episodeOfCareStatusHistoryModifierExtension = modifierExtension
          , episodeOfCareStatusHistoryStatus =      fromEpisodeOfCareStatusHistoryStatus status
          , episodeOfCareStatusHistoryPeriod = period
          }



data EpisodeOfCareDiagnosis = EpisodeOfCareDiagnosis {
    episodeOfCareDiagnosisAttrId :: Maybe Text
  , episodeOfCareDiagnosisExtension :: [Extension]
  , episodeOfCareDiagnosisModifierExtension :: [Extension]
  , episodeOfCareDiagnosisCondition :: Reference
  , episodeOfCareDiagnosisRole :: Maybe CodeableConcept
  , episodeOfCareDiagnosisRank :: Maybe PositiveInt
  }
  deriving (Eq, Show)
--

instance ToJSON EpisodeOfCareDiagnosis where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (episodeOfCareDiagnosisAttrId p)
    ,  "extension" .= toJSON (episodeOfCareDiagnosisExtension p)
    ,  "modifierExtension" .= toJSON (episodeOfCareDiagnosisModifierExtension p)
    ,  "condition" .= toJSON (episodeOfCareDiagnosisCondition p)
    ,  "role" .= toJSON (episodeOfCareDiagnosisRole p)
    ,  "rank" .= toJSON (episodeOfCareDiagnosisRank p)
    ]
instance FromJSON EpisodeOfCareDiagnosis where
  parseJSON = withObject "EpisodeOfCareDiagnosis" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        condition <- o .:  "condition"
        role <- o .:? "role"
        rank <- o .:? "rank"
        return EpisodeOfCareDiagnosis{
            episodeOfCareDiagnosisAttrId = id
          , episodeOfCareDiagnosisExtension = extension
          , episodeOfCareDiagnosisModifierExtension = modifierExtension
          , episodeOfCareDiagnosisCondition = condition
          , episodeOfCareDiagnosisRole = role
          , episodeOfCareDiagnosisRank = rank
          }
instance Xmlbf.ToXml EpisodeOfCareDiagnosis where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (episodeOfCareDiagnosisAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (episodeOfCareDiagnosisExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (episodeOfCareDiagnosisModifierExtension p))
             , Prop     "condition" (HM.empty, Xmlbf.toXml (episodeOfCareDiagnosisCondition p))
             , OptProp  "role" (fmap Xmlbf.toXml (episodeOfCareDiagnosisRole p))
             , OptVal   "rank" (fmap toPositiveInt (episodeOfCareDiagnosisRank p))
             ]
instance Xmlbf.FromXml EpisodeOfCareDiagnosis where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    condition <-            Xmlbf.pElement "condition" Xmlbf.fromXml
    role <- optional $ Xmlbf.pElement "role" Xmlbf.fromXml
    rank <- optional $ Xmlbf.pElement "rank" (Xmlbf.pAttr "value")
    return EpisodeOfCareDiagnosis {
            episodeOfCareDiagnosisAttrId = id
          , episodeOfCareDiagnosisExtension = extension
          , episodeOfCareDiagnosisModifierExtension = modifierExtension
          , episodeOfCareDiagnosisCondition = condition
          , episodeOfCareDiagnosisRole = role
          , episodeOfCareDiagnosisRank = fmap fromPositiveInt rank
          }




