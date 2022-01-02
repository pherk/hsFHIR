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
-- FHIR 4.0.0 Location
--

module Data.FHIR.Resources.Location where

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

data LocationStatus
    = LSActive
    | LSSuspended
    | LSInactive
  deriving (Eq, Show)

instance ToJSON LocationStatus where
    toJSON LSActive = String "active"
    toJSON LSSuspended = String "suspended"
    toJSON LSInactive = String "inactive"
instance FromJSON LocationStatus where
    parseJSON "active" = return LSActive
    parseJSON "suspended" = return LSSuspended
    parseJSON "inactive" = return LSInactive

toLocationStatus LSActive = "active"
toLocationStatus LSSuspended = "suspended"
toLocationStatus LSInactive = "inactive"
fromLocationStatus "active" = LSActive
fromLocationStatus "suspended" = LSSuspended
fromLocationStatus "inactive" = LSInactive


data LocationMode
    = LMInstance
    | LMKind
  deriving (Eq, Show)

instance ToJSON LocationMode where
    toJSON LMInstance = String "instance"
    toJSON LMKind = String "kind"
instance FromJSON LocationMode where
    parseJSON "instance" = return LMInstance
    parseJSON "kind" = return LMKind

toLocationMode LMInstance = "instance"
toLocationMode LMKind = "kind"
fromLocationMode "instance" = LMInstance
fromLocationMode "kind" = LMKind


data Location = Location {
    locationId :: Maybe Id
  , locationMeta :: Maybe Meta
  , locationImplicitRules :: Maybe Uri
  , locationLanguage :: Maybe Language
  , locationText :: Maybe Narrative
--    locationContained :: [ResourceContainer]
  , locationExtension :: [Extension]
  , locationModifierExtension :: [Extension]
  , locationIdentifier :: [Identifier]
  , locationStatus :: Maybe LocationStatus
  , locationOperationalStatus :: Maybe Coding
  , locationName :: Maybe Text
  , locationAlias :: [Text]
  , locationDescription :: Maybe Text
  , locationMode :: Maybe LocationMode
  , locationType :: [CodeableConcept]
  , locationTelecom :: [ContactPoint]
  , locationAddress :: Maybe Address
  , locationPhysicalType :: Maybe CodeableConcept
  , locationPosition :: Maybe LocationPosition
  , locationManagingOrganization :: Maybe Reference
  , locationPartOf :: Maybe Reference
  , locationHoursOfOperation :: [LocationHoursOfOperation]
  , locationAvailabilityExceptions :: Maybe Text
  , locationEndpoint :: [Reference]
  }
--

instance ToJSON Location where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Location")
    ,  "id" .= toJSON (locationId p)
    ,  "meta" .= toJSON (locationMeta p)
    ,  "implicitRules" .= toJSON (locationImplicitRules p)
    ,  "language" .= toJSON (locationLanguage p)
    ,  "text" .= toJSON (locationText p)
--    , "contained" .= toJSON (locationContained p)
    ,  "extension" .= toJSON (locationExtension p)
    ,  "modifierExtension" .= toJSON (locationModifierExtension p)
    ,  "identifier" .= toJSON (locationIdentifier p)
    ,  "status" .= toJSON (locationStatus p)
    ,  "operationalStatus" .= toJSON (locationOperationalStatus p)
    ,  "name" .= toJSON (locationName p)
    ,  "alias" .= toJSON (locationAlias p)
    ,  "description" .= toJSON (locationDescription p)
    ,  "mode" .= toJSON (locationMode p)
    ,  "type" .= toJSON (locationType p)
    ,  "telecom" .= toJSON (locationTelecom p)
    ,  "address" .= toJSON (locationAddress p)
    ,  "physicalType" .= toJSON (locationPhysicalType p)
    ,  "position" .= toJSON (locationPosition p)
    ,  "managingOrganization" .= toJSON (locationManagingOrganization p)
    ,  "partOf" .= toJSON (locationPartOf p)
    ,  "hoursOfOperation" .= toJSON (locationHoursOfOperation p)
    ,  "availabilityExceptions" .= toJSON (locationAvailabilityExceptions p)
    ,  "endpoint" .= toJSON (locationEndpoint p)
    ]
instance FromJSON Location where
  parseJSON = withObject "Location" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Location" -> do
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
        operationalStatus <- o .:? "operationalStatus"
        name <- o .:? "name"
        alias <- o .:? "alias" .!= []
        description <- o .:? "description"
        mode <- o .:? "mode"
        ty <- o .:? "type" .!= []
        telecom <- o .:? "telecom" .!= []
        address <- o .:? "address"
        physicalType <- o .:? "physicalType"
        position <- o .:? "position"
        managingOrganization <- o .:? "managingOrganization"
        partOf <- o .:? "partOf"
        hoursOfOperation <- o .:? "hoursOfOperation" .!= []
        availabilityExceptions <- o .:? "availabilityExceptions"
        endpoint <- o .:? "endpoint" .!= []
        return Location{
            locationId = id
          , locationMeta = meta
          , locationImplicitRules = implicitRules
          , locationLanguage = language
          , locationText = text
--          , locationContained = contained
          , locationExtension = extension
          , locationModifierExtension = modifierExtension
          , locationIdentifier = identifier
          , locationStatus = status
          , locationOperationalStatus = operationalStatus
          , locationName = name
          , locationAlias = alias
          , locationDescription = description
          , locationMode = mode
          , locationType = ty
          , locationTelecom = telecom
          , locationAddress = address
          , locationPhysicalType = physicalType
          , locationPosition = position
          , locationManagingOrganization = managingOrganization
          , locationPartOf = partOf
          , locationHoursOfOperation = hoursOfOperation
          , locationAvailabilityExceptions = availabilityExceptions
          , locationEndpoint = endpoint
          }
      _ -> fail "not a Location"
instance Xmlbf.ToXml Location where
  toXml p = Xmlbf.element "Location" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (locationId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (locationMeta p))
             , OptVal   "implicitRules" (fmap toUri (locationImplicitRules p))
             , OptVal   "language" (fmap toLanguage (locationLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (locationText p))
--             , PropList "contained" (fmap Xmlbf.toXml (locationContained p))
             , PropList "extension" (fmap Xmlbf.toXml (locationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (locationModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (locationIdentifier p))
             , OptVal   "status" (fmap toLocationStatus (locationStatus p))
             , OptProp  "operationalStatus" (fmap Xmlbf.toXml (locationOperationalStatus p))
             , OptVal   "name" (fmap toString (locationName p))
             , ValList  "alias" (fmap toString (locationAlias p))
             , OptVal   "description" (fmap toString (locationDescription p))
             , OptVal   "mode" (fmap toLocationMode (locationMode p))
             , PropList "type" (fmap Xmlbf.toXml (locationType p))
             , PropList "telecom" (fmap Xmlbf.toXml (locationTelecom p))
             , OptProp  "address" (fmap Xmlbf.toXml (locationAddress p))
             , OptProp  "physicalType" (fmap Xmlbf.toXml (locationPhysicalType p))
             , OptProp  "position" (fmap Xmlbf.toXml (locationPosition p))
             , OptProp  "managingOrganization" (fmap Xmlbf.toXml (locationManagingOrganization p))
             , OptProp  "partOf" (fmap Xmlbf.toXml (locationPartOf p))
             , PropList "hoursOfOperation" (fmap Xmlbf.toXml (locationHoursOfOperation p))
             , OptVal   "availabilityExceptions" (fmap toString (locationAvailabilityExceptions p))
             , PropList "endpoint" (fmap Xmlbf.toXml (locationEndpoint p))
             ]
instance Xmlbf.FromXml Location where
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
    operationalStatus <- optional $ Xmlbf.pElement "operationalStatus" Xmlbf.fromXml
    name <- optional $ Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    alias <- many     $ Xmlbf.pElement "alias" (Xmlbf.pAttr "value")
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    mode <- optional $ Xmlbf.pElement "mode" (Xmlbf.pAttr "value")
    ty <- many     $ Xmlbf.pElement "type" Xmlbf.fromXml
    telecom <- many     $ Xmlbf.pElement "telecom" Xmlbf.fromXml
    address <- optional $ Xmlbf.pElement "address" Xmlbf.fromXml
    physicalType <- optional $ Xmlbf.pElement "physicalType" Xmlbf.fromXml
    position <- optional $ Xmlbf.pElement "position" Xmlbf.fromXml
    managingOrganization <- optional $ Xmlbf.pElement "managingOrganization" Xmlbf.fromXml
    partOf <- optional $ Xmlbf.pElement "partOf" Xmlbf.fromXml
    hoursOfOperation <- many     $ Xmlbf.pElement "hoursOfOperation" Xmlbf.fromXml
    availabilityExceptions <- optional $ Xmlbf.pElement "availabilityExceptions" (Xmlbf.pAttr "value")
    endpoint <- many     $ Xmlbf.pElement "endpoint" Xmlbf.fromXml
    return Location {
            locationId = fmap fromId id
          , locationMeta = meta
          , locationImplicitRules = fmap fromUri implicitRules
          , locationLanguage = fmap fromLanguage language
          , locationText = text
--          , locationContained = contained
          , locationExtension = extension
          , locationModifierExtension = modifierExtension
          , locationIdentifier = identifier
          , locationStatus = fmap fromLocationStatus status
          , locationOperationalStatus = operationalStatus
          , locationName = fmap fromString name
          , locationAlias = fmap fromString alias
          , locationDescription = fmap fromString description
          , locationMode = fmap fromLocationMode mode
          , locationType = ty
          , locationTelecom = telecom
          , locationAddress = address
          , locationPhysicalType = physicalType
          , locationPosition = position
          , locationManagingOrganization = managingOrganization
          , locationPartOf = partOf
          , locationHoursOfOperation = hoursOfOperation
          , locationAvailabilityExceptions = fmap fromString availabilityExceptions
          , locationEndpoint = endpoint
          }



data LocationHoursOfOperationDaysOfWeek
    = LHOODOWMon
    | LHOODOWTue
    | LHOODOWWed
    | LHOODOWThu
    | LHOODOWFri
    | LHOODOWSat
    | LHOODOWSun
  deriving (Eq, Show)

instance ToJSON LocationHoursOfOperationDaysOfWeek where
    toJSON LHOODOWMon = String "mon"
    toJSON LHOODOWTue = String "tue"
    toJSON LHOODOWWed = String "wed"
    toJSON LHOODOWThu = String "thu"
    toJSON LHOODOWFri = String "fri"
    toJSON LHOODOWSat = String "sat"
    toJSON LHOODOWSun = String "sun"
instance FromJSON LocationHoursOfOperationDaysOfWeek where
    parseJSON "mon" = return LHOODOWMon
    parseJSON "tue" = return LHOODOWTue
    parseJSON "wed" = return LHOODOWWed
    parseJSON "thu" = return LHOODOWThu
    parseJSON "fri" = return LHOODOWFri
    parseJSON "sat" = return LHOODOWSat
    parseJSON "sun" = return LHOODOWSun

toLocationHoursOfOperationDaysOfWeek LHOODOWMon = "mon"
toLocationHoursOfOperationDaysOfWeek LHOODOWTue = "tue"
toLocationHoursOfOperationDaysOfWeek LHOODOWWed = "wed"
toLocationHoursOfOperationDaysOfWeek LHOODOWThu = "thu"
toLocationHoursOfOperationDaysOfWeek LHOODOWFri = "fri"
toLocationHoursOfOperationDaysOfWeek LHOODOWSat = "sat"
toLocationHoursOfOperationDaysOfWeek LHOODOWSun = "sun"
fromLocationHoursOfOperationDaysOfWeek "mon" = LHOODOWMon
fromLocationHoursOfOperationDaysOfWeek "tue" = LHOODOWTue
fromLocationHoursOfOperationDaysOfWeek "wed" = LHOODOWWed
fromLocationHoursOfOperationDaysOfWeek "thu" = LHOODOWThu
fromLocationHoursOfOperationDaysOfWeek "fri" = LHOODOWFri
fromLocationHoursOfOperationDaysOfWeek "sat" = LHOODOWSat
fromLocationHoursOfOperationDaysOfWeek "sun" = LHOODOWSun


data LocationHoursOfOperation = LocationHoursOfOperation {
    locationHoursOfOperationAttrId :: Maybe Text
  , locationHoursOfOperationExtension :: [Extension]
  , locationHoursOfOperationModifierExtension :: [Extension]
  , locationHoursOfOperationDaysOfWeek :: [LocationHoursOfOperationDaysOfWeek]
  , locationHoursOfOperationAllDay :: Maybe Boolean
  , locationHoursOfOperationOpeningTime :: Maybe Time
  , locationHoursOfOperationClosingTime :: Maybe Time
  }
--

instance ToJSON LocationHoursOfOperation where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (locationHoursOfOperationAttrId p)
    ,  "extension" .= toJSON (locationHoursOfOperationExtension p)
    ,  "modifierExtension" .= toJSON (locationHoursOfOperationModifierExtension p)
    ,  "daysOfWeek" .= toJSON (locationHoursOfOperationDaysOfWeek p)
    ,  "allDay" .= toJSON (locationHoursOfOperationAllDay p)
    ,  "openingTime" .= toJSON (locationHoursOfOperationOpeningTime p)
    ,  "closingTime" .= toJSON (locationHoursOfOperationClosingTime p)
    ]
instance FromJSON LocationHoursOfOperation where
  parseJSON = withObject "LocationHoursOfOperation" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        daysOfWeek <- o .:? "daysOfWeek" .!= []
        allDay <- o .:? "allDay"
        openingTime <- o .:? "openingTime"
        closingTime <- o .:? "closingTime"
        return LocationHoursOfOperation{
            locationHoursOfOperationAttrId = id
          , locationHoursOfOperationExtension = extension
          , locationHoursOfOperationModifierExtension = modifierExtension
          , locationHoursOfOperationDaysOfWeek = daysOfWeek
          , locationHoursOfOperationAllDay = allDay
          , locationHoursOfOperationOpeningTime = openingTime
          , locationHoursOfOperationClosingTime = closingTime
          }
instance Xmlbf.ToXml LocationHoursOfOperation where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (locationHoursOfOperationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (locationHoursOfOperationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (locationHoursOfOperationModifierExtension p))
             , ValList  "daysOfWeek" (fmap toLocationHoursOfOperationDaysOfWeek (locationHoursOfOperationDaysOfWeek p))
             , OptVal   "allDay" (fmap toBoolean (locationHoursOfOperationAllDay p))
             , OptVal   "openingTime" (fmap toTime (locationHoursOfOperationOpeningTime p))
             , OptVal   "closingTime" (fmap toTime (locationHoursOfOperationClosingTime p))
             ]
instance Xmlbf.FromXml LocationHoursOfOperation where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    daysOfWeek <- many     $ Xmlbf.pElement "daysOfWeek" (Xmlbf.pAttr "value")
    allDay <- optional $ Xmlbf.pElement "allDay" (Xmlbf.pAttr "value")
    openingTime <- optional $ Xmlbf.pElement "openingTime" (Xmlbf.pAttr "value")
    closingTime <- optional $ Xmlbf.pElement "closingTime" (Xmlbf.pAttr "value")
    return LocationHoursOfOperation {
            locationHoursOfOperationAttrId = id
          , locationHoursOfOperationExtension = extension
          , locationHoursOfOperationModifierExtension = modifierExtension
          , locationHoursOfOperationDaysOfWeek = fmap fromLocationHoursOfOperationDaysOfWeek daysOfWeek
          , locationHoursOfOperationAllDay = fmap fromBoolean allDay
          , locationHoursOfOperationOpeningTime = fmap fromTime openingTime
          , locationHoursOfOperationClosingTime = fmap fromTime closingTime
          }



data LocationPosition = LocationPosition {
    locationPositionAttrId :: Maybe Text
  , locationPositionExtension :: [Extension]
  , locationPositionModifierExtension :: [Extension]
  , locationPositionLongitude :: Decimal
  , locationPositionLatitude :: Decimal
  , locationPositionAltitude :: Maybe Decimal
  }
--

instance ToJSON LocationPosition where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (locationPositionAttrId p)
    ,  "extension" .= toJSON (locationPositionExtension p)
    ,  "modifierExtension" .= toJSON (locationPositionModifierExtension p)
    ,  "longitude" .= toJSON (locationPositionLongitude p)
    ,  "latitude" .= toJSON (locationPositionLatitude p)
    ,  "altitude" .= toJSON (locationPositionAltitude p)
    ]
instance FromJSON LocationPosition where
  parseJSON = withObject "LocationPosition" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        longitude <- o .:  "longitude"
        latitude <- o .:  "latitude"
        altitude <- o .:? "altitude"
        return LocationPosition{
            locationPositionAttrId = id
          , locationPositionExtension = extension
          , locationPositionModifierExtension = modifierExtension
          , locationPositionLongitude = longitude
          , locationPositionLatitude = latitude
          , locationPositionAltitude = altitude
          }
instance Xmlbf.ToXml LocationPosition where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (locationPositionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (locationPositionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (locationPositionModifierExtension p))
             , Val      "longitude" (     toDecimal (locationPositionLongitude p))
             , Val      "latitude" (     toDecimal (locationPositionLatitude p))
             , OptVal   "altitude" (fmap toDecimal (locationPositionAltitude p))
             ]
instance Xmlbf.FromXml LocationPosition where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    longitude <-            Xmlbf.pElement "longitude" (Xmlbf.pAttr "value")
    latitude <-            Xmlbf.pElement "latitude" (Xmlbf.pAttr "value")
    altitude <- optional $ Xmlbf.pElement "altitude" (Xmlbf.pAttr "value")
    return LocationPosition {
            locationPositionAttrId = id
          , locationPositionExtension = extension
          , locationPositionModifierExtension = modifierExtension
          , locationPositionLongitude =      fromDecimal longitude
          , locationPositionLatitude =      fromDecimal latitude
          , locationPositionAltitude = fmap fromDecimal altitude
          }




