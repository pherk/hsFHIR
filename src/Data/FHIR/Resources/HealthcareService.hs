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
-- FHIR 4.0.0 HealthcareService
--

module Data.FHIR.Resources.HealthcareService where

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

data HealthcareService = HealthcareService {
    healthcareServiceId :: Maybe Id
  , healthcareServiceMeta :: Maybe Meta
  , healthcareServiceImplicitRules :: Maybe Uri
  , healthcareServiceLanguage :: Maybe Language
  , healthcareServiceText :: Maybe Narrative
--    healthcareServiceContained :: [ResourceContainer]
  , healthcareServiceExtension :: [Extension]
  , healthcareServiceModifierExtension :: [Extension]
  , healthcareServiceIdentifier :: [Identifier]
  , healthcareServiceActive :: Maybe Boolean
  , healthcareServiceProvidedBy :: Maybe Reference
  , healthcareServiceCategory :: [CodeableConcept]
  , healthcareServiceType :: [CodeableConcept]
  , healthcareServiceSpecialty :: [CodeableConcept]
  , healthcareServiceLocation :: [Reference]
  , healthcareServiceName :: Maybe Text
  , healthcareServiceComment :: Maybe Text
  , healthcareServiceExtraDetails :: Maybe Markdown
  , healthcareServicePhoto :: Maybe Attachment
  , healthcareServiceTelecom :: [ContactPoint]
  , healthcareServiceCoverageArea :: [Reference]
  , healthcareServiceServiceProvisionCode :: [CodeableConcept]
  , healthcareServiceEligibility :: [HealthcareServiceEligibility]
  , healthcareServiceProgram :: [CodeableConcept]
  , healthcareServiceCharacteristic :: [CodeableConcept]
  , healthcareServiceCommunication :: [CodeableConcept]
  , healthcareServiceReferralMethod :: [CodeableConcept]
  , healthcareServiceAppointmentRequired :: Maybe Boolean
  , healthcareServiceAvailableTime :: [HealthcareServiceAvailableTime]
  , healthcareServiceNotAvailable :: [HealthcareServiceNotAvailable]
  , healthcareServiceAvailabilityExceptions :: Maybe Text
  , healthcareServiceEndpoint :: [Reference]
  }
--

instance ToJSON HealthcareService where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "HealthcareService")
    ,  "id" .= toJSON (healthcareServiceId p)
    ,  "meta" .= toJSON (healthcareServiceMeta p)
    ,  "implicitRules" .= toJSON (healthcareServiceImplicitRules p)
    ,  "language" .= toJSON (healthcareServiceLanguage p)
    ,  "text" .= toJSON (healthcareServiceText p)
--    , "contained" .= toJSON (healthcareServiceContained p)
    ,  "extension" .= toJSON (healthcareServiceExtension p)
    ,  "modifierExtension" .= toJSON (healthcareServiceModifierExtension p)
    ,  "identifier" .= toJSON (healthcareServiceIdentifier p)
    ,  "active" .= toJSON (healthcareServiceActive p)
    ,  "providedBy" .= toJSON (healthcareServiceProvidedBy p)
    ,  "category" .= toJSON (healthcareServiceCategory p)
    ,  "type" .= toJSON (healthcareServiceType p)
    ,  "specialty" .= toJSON (healthcareServiceSpecialty p)
    ,  "location" .= toJSON (healthcareServiceLocation p)
    ,  "name" .= toJSON (healthcareServiceName p)
    ,  "comment" .= toJSON (healthcareServiceComment p)
    ,  "extraDetails" .= toJSON (healthcareServiceExtraDetails p)
    ,  "photo" .= toJSON (healthcareServicePhoto p)
    ,  "telecom" .= toJSON (healthcareServiceTelecom p)
    ,  "coverageArea" .= toJSON (healthcareServiceCoverageArea p)
    ,  "serviceProvisionCode" .= toJSON (healthcareServiceServiceProvisionCode p)
    ,  "eligibility" .= toJSON (healthcareServiceEligibility p)
    ,  "program" .= toJSON (healthcareServiceProgram p)
    ,  "characteristic" .= toJSON (healthcareServiceCharacteristic p)
    ,  "communication" .= toJSON (healthcareServiceCommunication p)
    ,  "referralMethod" .= toJSON (healthcareServiceReferralMethod p)
    ,  "appointmentRequired" .= toJSON (healthcareServiceAppointmentRequired p)
    ,  "availableTime" .= toJSON (healthcareServiceAvailableTime p)
    ,  "notAvailable" .= toJSON (healthcareServiceNotAvailable p)
    ,  "availabilityExceptions" .= toJSON (healthcareServiceAvailabilityExceptions p)
    ,  "endpoint" .= toJSON (healthcareServiceEndpoint p)
    ]
instance FromJSON HealthcareService where
  parseJSON = withObject "HealthcareService" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "HealthcareService" -> do
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
        providedBy <- o .:? "providedBy"
        category <- o .:? "category" .!= []
        ty <- o .:? "type" .!= []
        specialty <- o .:? "specialty" .!= []
        location <- o .:? "location" .!= []
        name <- o .:? "name"
        comment <- o .:? "comment"
        extraDetails <- o .:? "extraDetails"
        photo <- o .:? "photo"
        telecom <- o .:? "telecom" .!= []
        coverageArea <- o .:? "coverageArea" .!= []
        serviceProvisionCode <- o .:? "serviceProvisionCode" .!= []
        eligibility <- o .:? "eligibility" .!= []
        program <- o .:? "program" .!= []
        characteristic <- o .:? "characteristic" .!= []
        communication <- o .:? "communication" .!= []
        referralMethod <- o .:? "referralMethod" .!= []
        appointmentRequired <- o .:? "appointmentRequired"
        availableTime <- o .:? "availableTime" .!= []
        notAvailable <- o .:? "notAvailable" .!= []
        availabilityExceptions <- o .:? "availabilityExceptions"
        endpoint <- o .:? "endpoint" .!= []
        return HealthcareService{
            healthcareServiceId = id
          , healthcareServiceMeta = meta
          , healthcareServiceImplicitRules = implicitRules
          , healthcareServiceLanguage = language
          , healthcareServiceText = text
--          , healthcareServiceContained = contained
          , healthcareServiceExtension = extension
          , healthcareServiceModifierExtension = modifierExtension
          , healthcareServiceIdentifier = identifier
          , healthcareServiceActive = active
          , healthcareServiceProvidedBy = providedBy
          , healthcareServiceCategory = category
          , healthcareServiceType = ty
          , healthcareServiceSpecialty = specialty
          , healthcareServiceLocation = location
          , healthcareServiceName = name
          , healthcareServiceComment = comment
          , healthcareServiceExtraDetails = extraDetails
          , healthcareServicePhoto = photo
          , healthcareServiceTelecom = telecom
          , healthcareServiceCoverageArea = coverageArea
          , healthcareServiceServiceProvisionCode = serviceProvisionCode
          , healthcareServiceEligibility = eligibility
          , healthcareServiceProgram = program
          , healthcareServiceCharacteristic = characteristic
          , healthcareServiceCommunication = communication
          , healthcareServiceReferralMethod = referralMethod
          , healthcareServiceAppointmentRequired = appointmentRequired
          , healthcareServiceAvailableTime = availableTime
          , healthcareServiceNotAvailable = notAvailable
          , healthcareServiceAvailabilityExceptions = availabilityExceptions
          , healthcareServiceEndpoint = endpoint
          }
      _ -> fail "not a HealthcareService"
instance Xmlbf.ToXml HealthcareService where
  toXml p = Xmlbf.element "HealthcareService" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (healthcareServiceId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (healthcareServiceMeta p))
             , OptVal   "implicitRules" (fmap toUri (healthcareServiceImplicitRules p))
             , OptVal   "language" (fmap toLanguage (healthcareServiceLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (healthcareServiceText p))
--             , PropList "contained" (fmap Xmlbf.toXml (healthcareServiceContained p))
             , PropList "extension" (fmap Xmlbf.toXml (healthcareServiceExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (healthcareServiceModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (healthcareServiceIdentifier p))
             , OptVal   "active" (fmap toBoolean (healthcareServiceActive p))
             , OptProp  "providedBy" (fmap Xmlbf.toXml (healthcareServiceProvidedBy p))
             , PropList "category" (fmap Xmlbf.toXml (healthcareServiceCategory p))
             , PropList "type" (fmap Xmlbf.toXml (healthcareServiceType p))
             , PropList "specialty" (fmap Xmlbf.toXml (healthcareServiceSpecialty p))
             , PropList "location" (fmap Xmlbf.toXml (healthcareServiceLocation p))
             , OptVal   "name" (fmap toString (healthcareServiceName p))
             , OptVal   "comment" (fmap toString (healthcareServiceComment p))
             , OptVal   "extraDetails" (fmap toMarkdown (healthcareServiceExtraDetails p))
             , OptProp  "photo" (fmap Xmlbf.toXml (healthcareServicePhoto p))
             , PropList "telecom" (fmap Xmlbf.toXml (healthcareServiceTelecom p))
             , PropList "coverageArea" (fmap Xmlbf.toXml (healthcareServiceCoverageArea p))
             , PropList "serviceProvisionCode" (fmap Xmlbf.toXml (healthcareServiceServiceProvisionCode p))
             , PropList "eligibility" (fmap Xmlbf.toXml (healthcareServiceEligibility p))
             , PropList "program" (fmap Xmlbf.toXml (healthcareServiceProgram p))
             , PropList "characteristic" (fmap Xmlbf.toXml (healthcareServiceCharacteristic p))
             , PropList "communication" (fmap Xmlbf.toXml (healthcareServiceCommunication p))
             , PropList "referralMethod" (fmap Xmlbf.toXml (healthcareServiceReferralMethod p))
             , OptVal   "appointmentRequired" (fmap toBoolean (healthcareServiceAppointmentRequired p))
             , PropList "availableTime" (fmap Xmlbf.toXml (healthcareServiceAvailableTime p))
             , PropList "notAvailable" (fmap Xmlbf.toXml (healthcareServiceNotAvailable p))
             , OptVal   "availabilityExceptions" (fmap toString (healthcareServiceAvailabilityExceptions p))
             , PropList "endpoint" (fmap Xmlbf.toXml (healthcareServiceEndpoint p))
             ]
instance Xmlbf.FromXml HealthcareService where
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
    providedBy <- optional $ Xmlbf.pElement "providedBy" Xmlbf.fromXml
    category <- many     $ Xmlbf.pElement "category" Xmlbf.fromXml
    ty <- many     $ Xmlbf.pElement "type" Xmlbf.fromXml
    specialty <- many     $ Xmlbf.pElement "specialty" Xmlbf.fromXml
    location <- many     $ Xmlbf.pElement "location" Xmlbf.fromXml
    name <- optional $ Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    comment <- optional $ Xmlbf.pElement "comment" (Xmlbf.pAttr "value")
    extraDetails <- optional $ Xmlbf.pElement "extraDetails" (Xmlbf.pAttr "value")
    photo <- optional $ Xmlbf.pElement "photo" Xmlbf.fromXml
    telecom <- many     $ Xmlbf.pElement "telecom" Xmlbf.fromXml
    coverageArea <- many     $ Xmlbf.pElement "coverageArea" Xmlbf.fromXml
    serviceProvisionCode <- many     $ Xmlbf.pElement "serviceProvisionCode" Xmlbf.fromXml
    eligibility <- many     $ Xmlbf.pElement "eligibility" Xmlbf.fromXml
    program <- many     $ Xmlbf.pElement "program" Xmlbf.fromXml
    characteristic <- many     $ Xmlbf.pElement "characteristic" Xmlbf.fromXml
    communication <- many     $ Xmlbf.pElement "communication" Xmlbf.fromXml
    referralMethod <- many     $ Xmlbf.pElement "referralMethod" Xmlbf.fromXml
    appointmentRequired <- optional $ Xmlbf.pElement "appointmentRequired" (Xmlbf.pAttr "value")
    availableTime <- many     $ Xmlbf.pElement "availableTime" Xmlbf.fromXml
    notAvailable <- many     $ Xmlbf.pElement "notAvailable" Xmlbf.fromXml
    availabilityExceptions <- optional $ Xmlbf.pElement "availabilityExceptions" (Xmlbf.pAttr "value")
    endpoint <- many     $ Xmlbf.pElement "endpoint" Xmlbf.fromXml
    return HealthcareService {
            healthcareServiceId = fmap fromId id
          , healthcareServiceMeta = meta
          , healthcareServiceImplicitRules = fmap fromUri implicitRules
          , healthcareServiceLanguage = fmap fromLanguage language
          , healthcareServiceText = text
--          , healthcareServiceContained = contained
          , healthcareServiceExtension = extension
          , healthcareServiceModifierExtension = modifierExtension
          , healthcareServiceIdentifier = identifier
          , healthcareServiceActive = fmap fromBoolean active
          , healthcareServiceProvidedBy = providedBy
          , healthcareServiceCategory = category
          , healthcareServiceType = ty
          , healthcareServiceSpecialty = specialty
          , healthcareServiceLocation = location
          , healthcareServiceName = fmap fromString name
          , healthcareServiceComment = fmap fromString comment
          , healthcareServiceExtraDetails = fmap fromMarkdown extraDetails
          , healthcareServicePhoto = photo
          , healthcareServiceTelecom = telecom
          , healthcareServiceCoverageArea = coverageArea
          , healthcareServiceServiceProvisionCode = serviceProvisionCode
          , healthcareServiceEligibility = eligibility
          , healthcareServiceProgram = program
          , healthcareServiceCharacteristic = characteristic
          , healthcareServiceCommunication = communication
          , healthcareServiceReferralMethod = referralMethod
          , healthcareServiceAppointmentRequired = fmap fromBoolean appointmentRequired
          , healthcareServiceAvailableTime = availableTime
          , healthcareServiceNotAvailable = notAvailable
          , healthcareServiceAvailabilityExceptions = fmap fromString availabilityExceptions
          , healthcareServiceEndpoint = endpoint
          }



data HealthcareServiceEligibility = HealthcareServiceEligibility {
    healthcareServiceEligibilityAttrId :: Maybe Text
  , healthcareServiceEligibilityExtension :: [Extension]
  , healthcareServiceEligibilityModifierExtension :: [Extension]
  , healthcareServiceEligibilityCode :: Maybe CodeableConcept
  , healthcareServiceEligibilityComment :: Maybe Markdown
  }
--

instance ToJSON HealthcareServiceEligibility where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (healthcareServiceEligibilityAttrId p)
    ,  "extension" .= toJSON (healthcareServiceEligibilityExtension p)
    ,  "modifierExtension" .= toJSON (healthcareServiceEligibilityModifierExtension p)
    ,  "code" .= toJSON (healthcareServiceEligibilityCode p)
    ,  "comment" .= toJSON (healthcareServiceEligibilityComment p)
    ]
instance FromJSON HealthcareServiceEligibility where
  parseJSON = withObject "HealthcareServiceEligibility" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:? "code"
        comment <- o .:? "comment"
        return HealthcareServiceEligibility{
            healthcareServiceEligibilityAttrId = id
          , healthcareServiceEligibilityExtension = extension
          , healthcareServiceEligibilityModifierExtension = modifierExtension
          , healthcareServiceEligibilityCode = code
          , healthcareServiceEligibilityComment = comment
          }
instance Xmlbf.ToXml HealthcareServiceEligibility where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (healthcareServiceEligibilityAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (healthcareServiceEligibilityExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (healthcareServiceEligibilityModifierExtension p))
             , OptProp  "code" (fmap Xmlbf.toXml (healthcareServiceEligibilityCode p))
             , OptVal   "comment" (fmap toMarkdown (healthcareServiceEligibilityComment p))
             ]
instance Xmlbf.FromXml HealthcareServiceEligibility where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    comment <- optional $ Xmlbf.pElement "comment" (Xmlbf.pAttr "value")
    return HealthcareServiceEligibility {
            healthcareServiceEligibilityAttrId = id
          , healthcareServiceEligibilityExtension = extension
          , healthcareServiceEligibilityModifierExtension = modifierExtension
          , healthcareServiceEligibilityCode = code
          , healthcareServiceEligibilityComment = fmap fromMarkdown comment
          }



data HealthcareServiceNotAvailable = HealthcareServiceNotAvailable {
    healthcareServiceNotAvailableAttrId :: Maybe Text
  , healthcareServiceNotAvailableExtension :: [Extension]
  , healthcareServiceNotAvailableModifierExtension :: [Extension]
  , healthcareServiceNotAvailableDescription :: Text
  , healthcareServiceNotAvailableDuring :: Maybe Period
  }
--

instance ToJSON HealthcareServiceNotAvailable where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (healthcareServiceNotAvailableAttrId p)
    ,  "extension" .= toJSON (healthcareServiceNotAvailableExtension p)
    ,  "modifierExtension" .= toJSON (healthcareServiceNotAvailableModifierExtension p)
    ,  "description" .= toJSON (healthcareServiceNotAvailableDescription p)
    ,  "during" .= toJSON (healthcareServiceNotAvailableDuring p)
    ]
instance FromJSON HealthcareServiceNotAvailable where
  parseJSON = withObject "HealthcareServiceNotAvailable" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        description <- o .:  "description"
        during <- o .:? "during"
        return HealthcareServiceNotAvailable{
            healthcareServiceNotAvailableAttrId = id
          , healthcareServiceNotAvailableExtension = extension
          , healthcareServiceNotAvailableModifierExtension = modifierExtension
          , healthcareServiceNotAvailableDescription = description
          , healthcareServiceNotAvailableDuring = during
          }
instance Xmlbf.ToXml HealthcareServiceNotAvailable where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (healthcareServiceNotAvailableAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (healthcareServiceNotAvailableExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (healthcareServiceNotAvailableModifierExtension p))
             , Val      "description" (     toString (healthcareServiceNotAvailableDescription p))
             , OptProp  "during" (fmap Xmlbf.toXml (healthcareServiceNotAvailableDuring p))
             ]
instance Xmlbf.FromXml HealthcareServiceNotAvailable where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    description <-            Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    during <- optional $ Xmlbf.pElement "during" Xmlbf.fromXml
    return HealthcareServiceNotAvailable {
            healthcareServiceNotAvailableAttrId = id
          , healthcareServiceNotAvailableExtension = extension
          , healthcareServiceNotAvailableModifierExtension = modifierExtension
          , healthcareServiceNotAvailableDescription =      fromString description
          , healthcareServiceNotAvailableDuring = during
          }



data HealthcareServiceAvailableTimeDaysOfWeek
    = HSATDOWMon
    | HSATDOWTue
    | HSATDOWWed
    | HSATDOWThu
    | HSATDOWFri
    | HSATDOWSat
    | HSATDOWSun
  deriving (Eq, Show)

instance ToJSON HealthcareServiceAvailableTimeDaysOfWeek where
    toJSON HSATDOWMon = String "mon"
    toJSON HSATDOWTue = String "tue"
    toJSON HSATDOWWed = String "wed"
    toJSON HSATDOWThu = String "thu"
    toJSON HSATDOWFri = String "fri"
    toJSON HSATDOWSat = String "sat"
    toJSON HSATDOWSun = String "sun"
instance FromJSON HealthcareServiceAvailableTimeDaysOfWeek where
    parseJSON "mon" = return HSATDOWMon
    parseJSON "tue" = return HSATDOWTue
    parseJSON "wed" = return HSATDOWWed
    parseJSON "thu" = return HSATDOWThu
    parseJSON "fri" = return HSATDOWFri
    parseJSON "sat" = return HSATDOWSat
    parseJSON "sun" = return HSATDOWSun

toHealthcareServiceAvailableTimeDaysOfWeek HSATDOWMon = "mon"
toHealthcareServiceAvailableTimeDaysOfWeek HSATDOWTue = "tue"
toHealthcareServiceAvailableTimeDaysOfWeek HSATDOWWed = "wed"
toHealthcareServiceAvailableTimeDaysOfWeek HSATDOWThu = "thu"
toHealthcareServiceAvailableTimeDaysOfWeek HSATDOWFri = "fri"
toHealthcareServiceAvailableTimeDaysOfWeek HSATDOWSat = "sat"
toHealthcareServiceAvailableTimeDaysOfWeek HSATDOWSun = "sun"
fromHealthcareServiceAvailableTimeDaysOfWeek "mon" = HSATDOWMon
fromHealthcareServiceAvailableTimeDaysOfWeek "tue" = HSATDOWTue
fromHealthcareServiceAvailableTimeDaysOfWeek "wed" = HSATDOWWed
fromHealthcareServiceAvailableTimeDaysOfWeek "thu" = HSATDOWThu
fromHealthcareServiceAvailableTimeDaysOfWeek "fri" = HSATDOWFri
fromHealthcareServiceAvailableTimeDaysOfWeek "sat" = HSATDOWSat
fromHealthcareServiceAvailableTimeDaysOfWeek "sun" = HSATDOWSun


data HealthcareServiceAvailableTime = HealthcareServiceAvailableTime {
    healthcareServiceAvailableTimeAttrId :: Maybe Text
  , healthcareServiceAvailableTimeExtension :: [Extension]
  , healthcareServiceAvailableTimeModifierExtension :: [Extension]
  , healthcareServiceAvailableTimeDaysOfWeek :: [HealthcareServiceAvailableTimeDaysOfWeek]
  , healthcareServiceAvailableTimeAllDay :: Maybe Boolean
  , healthcareServiceAvailableTimeAvailableStartTime :: Maybe Time
  , healthcareServiceAvailableTimeAvailableEndTime :: Maybe Time
  }
--

instance ToJSON HealthcareServiceAvailableTime where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (healthcareServiceAvailableTimeAttrId p)
    ,  "extension" .= toJSON (healthcareServiceAvailableTimeExtension p)
    ,  "modifierExtension" .= toJSON (healthcareServiceAvailableTimeModifierExtension p)
    ,  "daysOfWeek" .= toJSON (healthcareServiceAvailableTimeDaysOfWeek p)
    ,  "allDay" .= toJSON (healthcareServiceAvailableTimeAllDay p)
    ,  "availableStartTime" .= toJSON (healthcareServiceAvailableTimeAvailableStartTime p)
    ,  "availableEndTime" .= toJSON (healthcareServiceAvailableTimeAvailableEndTime p)
    ]
instance FromJSON HealthcareServiceAvailableTime where
  parseJSON = withObject "HealthcareServiceAvailableTime" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        daysOfWeek <- o .:? "daysOfWeek" .!= []
        allDay <- o .:? "allDay"
        availableStartTime <- o .:? "availableStartTime"
        availableEndTime <- o .:? "availableEndTime"
        return HealthcareServiceAvailableTime{
            healthcareServiceAvailableTimeAttrId = id
          , healthcareServiceAvailableTimeExtension = extension
          , healthcareServiceAvailableTimeModifierExtension = modifierExtension
          , healthcareServiceAvailableTimeDaysOfWeek = daysOfWeek
          , healthcareServiceAvailableTimeAllDay = allDay
          , healthcareServiceAvailableTimeAvailableStartTime = availableStartTime
          , healthcareServiceAvailableTimeAvailableEndTime = availableEndTime
          }
instance Xmlbf.ToXml HealthcareServiceAvailableTime where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (healthcareServiceAvailableTimeAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (healthcareServiceAvailableTimeExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (healthcareServiceAvailableTimeModifierExtension p))
             , ValList  "daysOfWeek" (fmap toHealthcareServiceAvailableTimeDaysOfWeek (healthcareServiceAvailableTimeDaysOfWeek p))
             , OptVal   "allDay" (fmap toBoolean (healthcareServiceAvailableTimeAllDay p))
             , OptVal   "availableStartTime" (fmap toTime (healthcareServiceAvailableTimeAvailableStartTime p))
             , OptVal   "availableEndTime" (fmap toTime (healthcareServiceAvailableTimeAvailableEndTime p))
             ]
instance Xmlbf.FromXml HealthcareServiceAvailableTime where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    daysOfWeek <- many     $ Xmlbf.pElement "daysOfWeek" (Xmlbf.pAttr "value")
    allDay <- optional $ Xmlbf.pElement "allDay" (Xmlbf.pAttr "value")
    availableStartTime <- optional $ Xmlbf.pElement "availableStartTime" (Xmlbf.pAttr "value")
    availableEndTime <- optional $ Xmlbf.pElement "availableEndTime" (Xmlbf.pAttr "value")
    return HealthcareServiceAvailableTime {
            healthcareServiceAvailableTimeAttrId = id
          , healthcareServiceAvailableTimeExtension = extension
          , healthcareServiceAvailableTimeModifierExtension = modifierExtension
          , healthcareServiceAvailableTimeDaysOfWeek = fmap fromHealthcareServiceAvailableTimeDaysOfWeek daysOfWeek
          , healthcareServiceAvailableTimeAllDay = fmap fromBoolean allDay
          , healthcareServiceAvailableTimeAvailableStartTime = fmap fromTime availableStartTime
          , healthcareServiceAvailableTimeAvailableEndTime = fmap fromTime availableEndTime
          }




