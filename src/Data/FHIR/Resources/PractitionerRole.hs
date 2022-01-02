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
-- FHIR 4.0.0 PractitionerRole
--

module Data.FHIR.Resources.PractitionerRole where

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

data PractitionerRole = PractitionerRole {
    practitionerRoleId :: Maybe Id
  , practitionerRoleMeta :: Maybe Meta
  , practitionerRoleImplicitRules :: Maybe Uri
  , practitionerRoleLanguage :: Maybe Language
  , practitionerRoleText :: Maybe Narrative
--    practitionerRoleContained :: [ResourceContainer]
  , practitionerRoleExtension :: [Extension]
  , practitionerRoleModifierExtension :: [Extension]
  , practitionerRoleIdentifier :: [Identifier]
  , practitionerRoleActive :: Maybe Boolean
  , practitionerRolePeriod :: Maybe Period
  , practitionerRolePractitioner :: Maybe Reference
  , practitionerRoleOrganization :: Maybe Reference
  , practitionerRoleCode :: [CodeableConcept]
  , practitionerRoleSpecialty :: [CodeableConcept]
  , practitionerRoleLocation :: [Reference]
  , practitionerRoleHealthcareService :: [Reference]
  , practitionerRoleTelecom :: [ContactPoint]
  , practitionerRoleAvailableTime :: [PractitionerRoleAvailableTime]
  , practitionerRoleNotAvailable :: [PractitionerRoleNotAvailable]
  , practitionerRoleAvailabilityExceptions :: Maybe Text
  , practitionerRoleEndpoint :: [Reference]
  }
--

instance ToJSON PractitionerRole where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "PractitionerRole")
    ,  "id" .= toJSON (practitionerRoleId p)
    ,  "meta" .= toJSON (practitionerRoleMeta p)
    ,  "implicitRules" .= toJSON (practitionerRoleImplicitRules p)
    ,  "language" .= toJSON (practitionerRoleLanguage p)
    ,  "text" .= toJSON (practitionerRoleText p)
--    , "contained" .= toJSON (practitionerRoleContained p)
    ,  "extension" .= toJSON (practitionerRoleExtension p)
    ,  "modifierExtension" .= toJSON (practitionerRoleModifierExtension p)
    ,  "identifier" .= toJSON (practitionerRoleIdentifier p)
    ,  "active" .= toJSON (practitionerRoleActive p)
    ,  "period" .= toJSON (practitionerRolePeriod p)
    ,  "practitioner" .= toJSON (practitionerRolePractitioner p)
    ,  "organization" .= toJSON (practitionerRoleOrganization p)
    ,  "code" .= toJSON (practitionerRoleCode p)
    ,  "specialty" .= toJSON (practitionerRoleSpecialty p)
    ,  "location" .= toJSON (practitionerRoleLocation p)
    ,  "healthcareService" .= toJSON (practitionerRoleHealthcareService p)
    ,  "telecom" .= toJSON (practitionerRoleTelecom p)
    ,  "availableTime" .= toJSON (practitionerRoleAvailableTime p)
    ,  "notAvailable" .= toJSON (practitionerRoleNotAvailable p)
    ,  "availabilityExceptions" .= toJSON (practitionerRoleAvailabilityExceptions p)
    ,  "endpoint" .= toJSON (practitionerRoleEndpoint p)
    ]
instance FromJSON PractitionerRole where
  parseJSON = withObject "PractitionerRole" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "PractitionerRole" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        active <- o .:? "active"
        period <- o .:? "period"
        practitioner <- o .:? "practitioner"
        organization <- o .:? "organization"
        code <- o .:? "code" .!= []
        specialty <- o .:? "specialty" .!= []
        location <- o .:? "location" .!= []
        healthcareService <- o .:? "healthcareService" .!= []
        telecom <- o .:? "telecom" .!= []
        availableTime <- o .:? "availableTime" .!= []
        notAvailable <- o .:? "notAvailable" .!= []
        availabilityExceptions <- o .:? "availabilityExceptions"
        endpoint <- o .:? "endpoint" .!= []
        return PractitionerRole{
            practitionerRoleId = id
          , practitionerRoleMeta = meta
          , practitionerRoleImplicitRules = implicitRules
          , practitionerRoleLanguage = language
          , practitionerRoleText = text
--          , practitionerRoleContained = contained
          , practitionerRoleExtension = extension
          , practitionerRoleModifierExtension = modifierExtension
          , practitionerRoleIdentifier = identifier
          , practitionerRoleActive = active
          , practitionerRolePeriod = period
          , practitionerRolePractitioner = practitioner
          , practitionerRoleOrganization = organization
          , practitionerRoleCode = code
          , practitionerRoleSpecialty = specialty
          , practitionerRoleLocation = location
          , practitionerRoleHealthcareService = healthcareService
          , practitionerRoleTelecom = telecom
          , practitionerRoleAvailableTime = availableTime
          , practitionerRoleNotAvailable = notAvailable
          , practitionerRoleAvailabilityExceptions = availabilityExceptions
          , practitionerRoleEndpoint = endpoint
          }
      _ -> fail "not a PractitionerRole"
instance Xmlbf.ToXml PractitionerRole where
  toXml p = Xmlbf.element "PractitionerRole" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (practitionerRoleId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (practitionerRoleMeta p))
             , OptVal   "implicitRules" (fmap toUri (practitionerRoleImplicitRules p))
             , OptVal   "language" (fmap toLanguage (practitionerRoleLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (practitionerRoleText p))
--             , PropList "contained" (fmap Xmlbf.toXml (practitionerRoleContained p))
             , PropList "extension" (fmap Xmlbf.toXml (practitionerRoleExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (practitionerRoleModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (practitionerRoleIdentifier p))
             , OptVal   "active" (fmap toBoolean (practitionerRoleActive p))
             , OptProp  "period" (fmap Xmlbf.toXml (practitionerRolePeriod p))
             , OptProp  "practitioner" (fmap Xmlbf.toXml (practitionerRolePractitioner p))
             , OptProp  "organization" (fmap Xmlbf.toXml (practitionerRoleOrganization p))
             , PropList "code" (fmap Xmlbf.toXml (practitionerRoleCode p))
             , PropList "specialty" (fmap Xmlbf.toXml (practitionerRoleSpecialty p))
             , PropList "location" (fmap Xmlbf.toXml (practitionerRoleLocation p))
             , PropList "healthcareService" (fmap Xmlbf.toXml (practitionerRoleHealthcareService p))
             , PropList "telecom" (fmap Xmlbf.toXml (practitionerRoleTelecom p))
             , PropList "availableTime" (fmap Xmlbf.toXml (practitionerRoleAvailableTime p))
             , PropList "notAvailable" (fmap Xmlbf.toXml (practitionerRoleNotAvailable p))
             , OptVal   "availabilityExceptions" (fmap toString (practitionerRoleAvailabilityExceptions p))
             , PropList "endpoint" (fmap Xmlbf.toXml (practitionerRoleEndpoint p))
             ]
instance Xmlbf.FromXml PractitionerRole where
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
    active <- optional $ Xmlbf.pElement "active" (Xmlbf.pAttr "value")
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    practitioner <- optional $ Xmlbf.pElement "practitioner" Xmlbf.fromXml
    organization <- optional $ Xmlbf.pElement "organization" Xmlbf.fromXml
    code <- many     $ Xmlbf.pElement "code" Xmlbf.fromXml
    specialty <- many     $ Xmlbf.pElement "specialty" Xmlbf.fromXml
    location <- many     $ Xmlbf.pElement "location" Xmlbf.fromXml
    healthcareService <- many     $ Xmlbf.pElement "healthcareService" Xmlbf.fromXml
    telecom <- many     $ Xmlbf.pElement "telecom" Xmlbf.fromXml
    availableTime <- many     $ Xmlbf.pElement "availableTime" Xmlbf.fromXml
    notAvailable <- many     $ Xmlbf.pElement "notAvailable" Xmlbf.fromXml
    availabilityExceptions <- optional $ Xmlbf.pElement "availabilityExceptions" (Xmlbf.pAttr "value")
    endpoint <- many     $ Xmlbf.pElement "endpoint" Xmlbf.fromXml
    return PractitionerRole {
            practitionerRoleId = fmap fromId id
          , practitionerRoleMeta = meta
          , practitionerRoleImplicitRules = fmap fromUri implicitRules
          , practitionerRoleLanguage = fmap fromLanguage language
          , practitionerRoleText = text
--          , practitionerRoleContained = contained
          , practitionerRoleExtension = extension
          , practitionerRoleModifierExtension = modifierExtension
          , practitionerRoleIdentifier = identifier
          , practitionerRoleActive = fmap fromBoolean active
          , practitionerRolePeriod = period
          , practitionerRolePractitioner = practitioner
          , practitionerRoleOrganization = organization
          , practitionerRoleCode = code
          , practitionerRoleSpecialty = specialty
          , practitionerRoleLocation = location
          , practitionerRoleHealthcareService = healthcareService
          , practitionerRoleTelecom = telecom
          , practitionerRoleAvailableTime = availableTime
          , practitionerRoleNotAvailable = notAvailable
          , practitionerRoleAvailabilityExceptions = fmap fromString availabilityExceptions
          , practitionerRoleEndpoint = endpoint
          }



data PractitionerRoleAvailableTimeDaysOfWeek
    = PRATDOWMon
    | PRATDOWTue
    | PRATDOWWed
    | PRATDOWThu
    | PRATDOWFri
    | PRATDOWSat
    | PRATDOWSun
  deriving (Eq, Show)

instance ToJSON PractitionerRoleAvailableTimeDaysOfWeek where
    toJSON PRATDOWMon = String "mon"
    toJSON PRATDOWTue = String "tue"
    toJSON PRATDOWWed = String "wed"
    toJSON PRATDOWThu = String "thu"
    toJSON PRATDOWFri = String "fri"
    toJSON PRATDOWSat = String "sat"
    toJSON PRATDOWSun = String "sun"
instance FromJSON PractitionerRoleAvailableTimeDaysOfWeek where
    parseJSON "mon" = return PRATDOWMon
    parseJSON "tue" = return PRATDOWTue
    parseJSON "wed" = return PRATDOWWed
    parseJSON "thu" = return PRATDOWThu
    parseJSON "fri" = return PRATDOWFri
    parseJSON "sat" = return PRATDOWSat
    parseJSON "sun" = return PRATDOWSun

toPractitionerRoleAvailableTimeDaysOfWeek PRATDOWMon = "mon"
toPractitionerRoleAvailableTimeDaysOfWeek PRATDOWTue = "tue"
toPractitionerRoleAvailableTimeDaysOfWeek PRATDOWWed = "wed"
toPractitionerRoleAvailableTimeDaysOfWeek PRATDOWThu = "thu"
toPractitionerRoleAvailableTimeDaysOfWeek PRATDOWFri = "fri"
toPractitionerRoleAvailableTimeDaysOfWeek PRATDOWSat = "sat"
toPractitionerRoleAvailableTimeDaysOfWeek PRATDOWSun = "sun"
fromPractitionerRoleAvailableTimeDaysOfWeek "mon" = PRATDOWMon
fromPractitionerRoleAvailableTimeDaysOfWeek "tue" = PRATDOWTue
fromPractitionerRoleAvailableTimeDaysOfWeek "wed" = PRATDOWWed
fromPractitionerRoleAvailableTimeDaysOfWeek "thu" = PRATDOWThu
fromPractitionerRoleAvailableTimeDaysOfWeek "fri" = PRATDOWFri
fromPractitionerRoleAvailableTimeDaysOfWeek "sat" = PRATDOWSat
fromPractitionerRoleAvailableTimeDaysOfWeek "sun" = PRATDOWSun


data PractitionerRoleAvailableTime = PractitionerRoleAvailableTime {
    practitionerRoleAvailableTimeAttrId :: Maybe Text
  , practitionerRoleAvailableTimeExtension :: [Extension]
  , practitionerRoleAvailableTimeModifierExtension :: [Extension]
  , practitionerRoleAvailableTimeDaysOfWeek :: [PractitionerRoleAvailableTimeDaysOfWeek]
  , practitionerRoleAvailableTimeAllDay :: Maybe Boolean
  , practitionerRoleAvailableTimeAvailableStartTime :: Maybe Time
  , practitionerRoleAvailableTimeAvailableEndTime :: Maybe Time
  }
--

instance ToJSON PractitionerRoleAvailableTime where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (practitionerRoleAvailableTimeAttrId p)
    ,  "extension" .= toJSON (practitionerRoleAvailableTimeExtension p)
    ,  "modifierExtension" .= toJSON (practitionerRoleAvailableTimeModifierExtension p)
    ,  "daysOfWeek" .= toJSON (practitionerRoleAvailableTimeDaysOfWeek p)
    ,  "allDay" .= toJSON (practitionerRoleAvailableTimeAllDay p)
    ,  "availableStartTime" .= toJSON (practitionerRoleAvailableTimeAvailableStartTime p)
    ,  "availableEndTime" .= toJSON (practitionerRoleAvailableTimeAvailableEndTime p)
    ]
instance FromJSON PractitionerRoleAvailableTime where
  parseJSON = withObject "PractitionerRoleAvailableTime" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        daysOfWeek <- o .:? "daysOfWeek" .!= []
        allDay <- o .:? "allDay"
        availableStartTime <- o .:? "availableStartTime"
        availableEndTime <- o .:? "availableEndTime"
        return PractitionerRoleAvailableTime{
            practitionerRoleAvailableTimeAttrId = id
          , practitionerRoleAvailableTimeExtension = extension
          , practitionerRoleAvailableTimeModifierExtension = modifierExtension
          , practitionerRoleAvailableTimeDaysOfWeek = daysOfWeek
          , practitionerRoleAvailableTimeAllDay = allDay
          , practitionerRoleAvailableTimeAvailableStartTime = availableStartTime
          , practitionerRoleAvailableTimeAvailableEndTime = availableEndTime
          }
instance Xmlbf.ToXml PractitionerRoleAvailableTime where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (practitionerRoleAvailableTimeAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (practitionerRoleAvailableTimeExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (practitionerRoleAvailableTimeModifierExtension p))
             , ValList  "daysOfWeek" (fmap toPractitionerRoleAvailableTimeDaysOfWeek (practitionerRoleAvailableTimeDaysOfWeek p))
             , OptVal   "allDay" (fmap toBoolean (practitionerRoleAvailableTimeAllDay p))
             , OptVal   "availableStartTime" (fmap toTime (practitionerRoleAvailableTimeAvailableStartTime p))
             , OptVal   "availableEndTime" (fmap toTime (practitionerRoleAvailableTimeAvailableEndTime p))
             ]
instance Xmlbf.FromXml PractitionerRoleAvailableTime where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    daysOfWeek <- many     $ Xmlbf.pElement "daysOfWeek" (Xmlbf.pAttr "value")
    allDay <- optional $ Xmlbf.pElement "allDay" (Xmlbf.pAttr "value")
    availableStartTime <- optional $ Xmlbf.pElement "availableStartTime" (Xmlbf.pAttr "value")
    availableEndTime <- optional $ Xmlbf.pElement "availableEndTime" (Xmlbf.pAttr "value")
    return PractitionerRoleAvailableTime {
            practitionerRoleAvailableTimeAttrId = id
          , practitionerRoleAvailableTimeExtension = extension
          , practitionerRoleAvailableTimeModifierExtension = modifierExtension
          , practitionerRoleAvailableTimeDaysOfWeek = fmap fromPractitionerRoleAvailableTimeDaysOfWeek daysOfWeek
          , practitionerRoleAvailableTimeAllDay = fmap fromBoolean allDay
          , practitionerRoleAvailableTimeAvailableStartTime = fmap fromTime availableStartTime
          , practitionerRoleAvailableTimeAvailableEndTime = fmap fromTime availableEndTime
          }



data PractitionerRoleNotAvailable = PractitionerRoleNotAvailable {
    practitionerRoleNotAvailableAttrId :: Maybe Text
  , practitionerRoleNotAvailableExtension :: [Extension]
  , practitionerRoleNotAvailableModifierExtension :: [Extension]
  , practitionerRoleNotAvailableDescription :: Text
  , practitionerRoleNotAvailableDuring :: Maybe Period
  }
--

instance ToJSON PractitionerRoleNotAvailable where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (practitionerRoleNotAvailableAttrId p)
    ,  "extension" .= toJSON (practitionerRoleNotAvailableExtension p)
    ,  "modifierExtension" .= toJSON (practitionerRoleNotAvailableModifierExtension p)
    ,  "description" .= toJSON (practitionerRoleNotAvailableDescription p)
    ,  "during" .= toJSON (practitionerRoleNotAvailableDuring p)
    ]
instance FromJSON PractitionerRoleNotAvailable where
  parseJSON = withObject "PractitionerRoleNotAvailable" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        description <- o .:  "description"
        during <- o .:? "during"
        return PractitionerRoleNotAvailable{
            practitionerRoleNotAvailableAttrId = id
          , practitionerRoleNotAvailableExtension = extension
          , practitionerRoleNotAvailableModifierExtension = modifierExtension
          , practitionerRoleNotAvailableDescription = description
          , practitionerRoleNotAvailableDuring = during
          }
instance Xmlbf.ToXml PractitionerRoleNotAvailable where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (practitionerRoleNotAvailableAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (practitionerRoleNotAvailableExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (practitionerRoleNotAvailableModifierExtension p))
             , Val      "description" (     toString (practitionerRoleNotAvailableDescription p))
             , OptProp  "during" (fmap Xmlbf.toXml (practitionerRoleNotAvailableDuring p))
             ]
instance Xmlbf.FromXml PractitionerRoleNotAvailable where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    description <-            Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    during <- optional $ Xmlbf.pElement "during" Xmlbf.fromXml
    return PractitionerRoleNotAvailable {
            practitionerRoleNotAvailableAttrId = id
          , practitionerRoleNotAvailableExtension = extension
          , practitionerRoleNotAvailableModifierExtension = modifierExtension
          , practitionerRoleNotAvailableDescription =      fromString description
          , practitionerRoleNotAvailableDuring = during
          }




