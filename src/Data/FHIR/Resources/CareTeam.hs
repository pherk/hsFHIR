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
-- FHIR 4.0.0 CareTeam
--

module Data.FHIR.Resources.CareTeam where

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

data CareTeamStatus
    = CTSProposed
    | CTSActive
    | CTSSuspended
    | CTSInactive
    | CTSEnteredInError
  deriving (Eq, Show)

instance ToJSON CareTeamStatus where
    toJSON CTSProposed = String "proposed"
    toJSON CTSActive = String "active"
    toJSON CTSSuspended = String "suspended"
    toJSON CTSInactive = String "inactive"
    toJSON CTSEnteredInError = String "entered-in-error"
instance FromJSON CareTeamStatus where
    parseJSON "proposed" = return CTSProposed
    parseJSON "active" = return CTSActive
    parseJSON "suspended" = return CTSSuspended
    parseJSON "inactive" = return CTSInactive
    parseJSON "entered-in-error" = return CTSEnteredInError

toCareTeamStatus CTSProposed = "proposed"
toCareTeamStatus CTSActive = "active"
toCareTeamStatus CTSSuspended = "suspended"
toCareTeamStatus CTSInactive = "inactive"
toCareTeamStatus CTSEnteredInError = "entered-in-error"
fromCareTeamStatus "proposed" = CTSProposed
fromCareTeamStatus "active" = CTSActive
fromCareTeamStatus "suspended" = CTSSuspended
fromCareTeamStatus "inactive" = CTSInactive
fromCareTeamStatus "entered-in-error" = CTSEnteredInError


data CareTeam = CareTeam {
    careTeamId :: Maybe Id
  , careTeamMeta :: Maybe Meta
  , careTeamImplicitRules :: Maybe Uri
  , careTeamLanguage :: Maybe Language
  , careTeamText :: Maybe Narrative
--    careTeamContained :: [ResourceContainer]
  , careTeamExtension :: [Extension]
  , careTeamModifierExtension :: [Extension]
  , careTeamIdentifier :: [Identifier]
  , careTeamStatus :: Maybe CareTeamStatus
  , careTeamCategory :: [CodeableConcept]
  , careTeamName :: Maybe Text
  , careTeamSubject :: Maybe Reference
  , careTeamEncounter :: Maybe Reference
  , careTeamPeriod :: Maybe Period
  , careTeamParticipant :: [CareTeamParticipant]
  , careTeamReasonCode :: [CodeableConcept]
  , careTeamReasonReference :: [Reference]
  , careTeamManagingOrganization :: [Reference]
  , careTeamTelecom :: [ContactPoint]
  , careTeamNote :: [Annotation]
  }
--

instance ToJSON CareTeam where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "CareTeam")
    ,  "id" .= toJSON (careTeamId p)
    ,  "meta" .= toJSON (careTeamMeta p)
    ,  "implicitRules" .= toJSON (careTeamImplicitRules p)
    ,  "language" .= toJSON (careTeamLanguage p)
    ,  "text" .= toJSON (careTeamText p)
--    , "contained" .= toJSON (careTeamContained p)
    ,  "extension" .= toJSON (careTeamExtension p)
    ,  "modifierExtension" .= toJSON (careTeamModifierExtension p)
    ,  "identifier" .= toJSON (careTeamIdentifier p)
    ,  "status" .= toJSON (careTeamStatus p)
    ,  "category" .= toJSON (careTeamCategory p)
    ,  "name" .= toJSON (careTeamName p)
    ,  "subject" .= toJSON (careTeamSubject p)
    ,  "encounter" .= toJSON (careTeamEncounter p)
    ,  "period" .= toJSON (careTeamPeriod p)
    ,  "participant" .= toJSON (careTeamParticipant p)
    ,  "reasonCode" .= toJSON (careTeamReasonCode p)
    ,  "reasonReference" .= toJSON (careTeamReasonReference p)
    ,  "managingOrganization" .= toJSON (careTeamManagingOrganization p)
    ,  "telecom" .= toJSON (careTeamTelecom p)
    ,  "note" .= toJSON (careTeamNote p)
    ]
instance FromJSON CareTeam where
  parseJSON = withObject "CareTeam" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "CareTeam" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        status <- o .:? "status"
        category <- o .:? "category" .!= []
        name <- o .:? "name"
        subject <- o .:? "subject"
        encounter <- o .:? "encounter"
        period <- o .:? "period"
        participant <- o .:? "participant" .!= []
        reasonCode <- o .:? "reasonCode" .!= []
        reasonReference <- o .:? "reasonReference" .!= []
        managingOrganization <- o .:? "managingOrganization" .!= []
        telecom <- o .:? "telecom" .!= []
        note <- o .:? "note" .!= []
        return CareTeam{
            careTeamId = id
          , careTeamMeta = meta
          , careTeamImplicitRules = implicitRules
          , careTeamLanguage = language
          , careTeamText = text
--          , careTeamContained = contained
          , careTeamExtension = extension
          , careTeamModifierExtension = modifierExtension
          , careTeamIdentifier = identifier
          , careTeamStatus = status
          , careTeamCategory = category
          , careTeamName = name
          , careTeamSubject = subject
          , careTeamEncounter = encounter
          , careTeamPeriod = period
          , careTeamParticipant = participant
          , careTeamReasonCode = reasonCode
          , careTeamReasonReference = reasonReference
          , careTeamManagingOrganization = managingOrganization
          , careTeamTelecom = telecom
          , careTeamNote = note
          }
      _ -> fail "not a CareTeam"
instance Xmlbf.ToXml CareTeam where
  toXml p = Xmlbf.element "CareTeam" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (careTeamId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (careTeamMeta p))
             , OptVal   "implicitRules" (fmap toUri (careTeamImplicitRules p))
             , OptVal   "language" (fmap toLanguage (careTeamLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (careTeamText p))
--             , PropList "contained" (fmap Xmlbf.toXml (careTeamContained p))
             , PropList "extension" (fmap Xmlbf.toXml (careTeamExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (careTeamModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (careTeamIdentifier p))
             , OptVal   "status" (fmap toCareTeamStatus (careTeamStatus p))
             , PropList "category" (fmap Xmlbf.toXml (careTeamCategory p))
             , OptVal   "name" (fmap toString (careTeamName p))
             , OptProp  "subject" (fmap Xmlbf.toXml (careTeamSubject p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (careTeamEncounter p))
             , OptProp  "period" (fmap Xmlbf.toXml (careTeamPeriod p))
             , PropList "participant" (fmap Xmlbf.toXml (careTeamParticipant p))
             , PropList "reasonCode" (fmap Xmlbf.toXml (careTeamReasonCode p))
             , PropList "reasonReference" (fmap Xmlbf.toXml (careTeamReasonReference p))
             , PropList "managingOrganization" (fmap Xmlbf.toXml (careTeamManagingOrganization p))
             , PropList "telecom" (fmap Xmlbf.toXml (careTeamTelecom p))
             , PropList "note" (fmap Xmlbf.toXml (careTeamNote p))
             ]
instance Xmlbf.FromXml CareTeam where
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
    status <- optional $ Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    category <- many     $ Xmlbf.pElement "category" Xmlbf.fromXml
    name <- optional $ Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    subject <- optional $ Xmlbf.pElement "subject" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    participant <- many     $ Xmlbf.pElement "participant" Xmlbf.fromXml
    reasonCode <- many     $ Xmlbf.pElement "reasonCode" Xmlbf.fromXml
    reasonReference <- many     $ Xmlbf.pElement "reasonReference" Xmlbf.fromXml
    managingOrganization <- many     $ Xmlbf.pElement "managingOrganization" Xmlbf.fromXml
    telecom <- many     $ Xmlbf.pElement "telecom" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    return CareTeam {
            careTeamId = fmap fromId id
          , careTeamMeta = meta
          , careTeamImplicitRules = fmap fromUri implicitRules
          , careTeamLanguage = fmap fromLanguage language
          , careTeamText = text
--          , careTeamContained = contained
          , careTeamExtension = extension
          , careTeamModifierExtension = modifierExtension
          , careTeamIdentifier = identifier
          , careTeamStatus = fmap fromCareTeamStatus status
          , careTeamCategory = category
          , careTeamName = fmap fromString name
          , careTeamSubject = subject
          , careTeamEncounter = encounter
          , careTeamPeriod = period
          , careTeamParticipant = participant
          , careTeamReasonCode = reasonCode
          , careTeamReasonReference = reasonReference
          , careTeamManagingOrganization = managingOrganization
          , careTeamTelecom = telecom
          , careTeamNote = note
          }



data CareTeamParticipant = CareTeamParticipant {
    careTeamParticipantAttrId :: Maybe Text
  , careTeamParticipantExtension :: [Extension]
  , careTeamParticipantModifierExtension :: [Extension]
  , careTeamParticipantRole :: [CodeableConcept]
  , careTeamParticipantMember :: Maybe Reference
  , careTeamParticipantOnBehalfOf :: Maybe Reference
  , careTeamParticipantPeriod :: Maybe Period
  }
--

instance ToJSON CareTeamParticipant where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (careTeamParticipantAttrId p)
    ,  "extension" .= toJSON (careTeamParticipantExtension p)
    ,  "modifierExtension" .= toJSON (careTeamParticipantModifierExtension p)
    ,  "role" .= toJSON (careTeamParticipantRole p)
    ,  "member" .= toJSON (careTeamParticipantMember p)
    ,  "onBehalfOf" .= toJSON (careTeamParticipantOnBehalfOf p)
    ,  "period" .= toJSON (careTeamParticipantPeriod p)
    ]
instance FromJSON CareTeamParticipant where
  parseJSON = withObject "CareTeamParticipant" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        role <- o .:? "role" .!= []
        member <- o .:? "member"
        onBehalfOf <- o .:? "onBehalfOf"
        period <- o .:? "period"
        return CareTeamParticipant{
            careTeamParticipantAttrId = id
          , careTeamParticipantExtension = extension
          , careTeamParticipantModifierExtension = modifierExtension
          , careTeamParticipantRole = role
          , careTeamParticipantMember = member
          , careTeamParticipantOnBehalfOf = onBehalfOf
          , careTeamParticipantPeriod = period
          }
instance Xmlbf.ToXml CareTeamParticipant where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (careTeamParticipantAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (careTeamParticipantExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (careTeamParticipantModifierExtension p))
             , PropList "role" (fmap Xmlbf.toXml (careTeamParticipantRole p))
             , OptProp  "member" (fmap Xmlbf.toXml (careTeamParticipantMember p))
             , OptProp  "onBehalfOf" (fmap Xmlbf.toXml (careTeamParticipantOnBehalfOf p))
             , OptProp  "period" (fmap Xmlbf.toXml (careTeamParticipantPeriod p))
             ]
instance Xmlbf.FromXml CareTeamParticipant where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    role <- many     $ Xmlbf.pElement "role" Xmlbf.fromXml
    member <- optional $ Xmlbf.pElement "member" Xmlbf.fromXml
    onBehalfOf <- optional $ Xmlbf.pElement "onBehalfOf" Xmlbf.fromXml
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    return CareTeamParticipant {
            careTeamParticipantAttrId = id
          , careTeamParticipantExtension = extension
          , careTeamParticipantModifierExtension = modifierExtension
          , careTeamParticipantRole = role
          , careTeamParticipantMember = member
          , careTeamParticipantOnBehalfOf = onBehalfOf
          , careTeamParticipantPeriod = period
          }




