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
-- FHIR 4.0.0 Device
--

module Data.FHIR.Resources.Device where

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

data DeviceStatus
    = DSActive
    | DSInactive
    | DSEnteredInError
    | DSUnknown
  deriving (Eq, Show)

instance ToJSON DeviceStatus where
    toJSON DSActive = String "active"
    toJSON DSInactive = String "inactive"
    toJSON DSEnteredInError = String "entered-in-error"
    toJSON DSUnknown = String "unknown"
instance FromJSON DeviceStatus where
    parseJSON "active" = return DSActive
    parseJSON "inactive" = return DSInactive
    parseJSON "entered-in-error" = return DSEnteredInError
    parseJSON "unknown" = return DSUnknown

toDeviceStatus DSActive = "active"
toDeviceStatus DSInactive = "inactive"
toDeviceStatus DSEnteredInError = "entered-in-error"
toDeviceStatus DSUnknown = "unknown"
fromDeviceStatus "active" = DSActive
fromDeviceStatus "inactive" = DSInactive
fromDeviceStatus "entered-in-error" = DSEnteredInError
fromDeviceStatus "unknown" = DSUnknown


data Device = Device {
    deviceId :: Maybe Id
  , deviceMeta :: Maybe Meta
  , deviceImplicitRules :: Maybe Uri
  , deviceLanguage :: Maybe Language
  , deviceText :: Maybe Narrative
--    deviceContained :: [ResourceContainer]
  , deviceExtension :: [Extension]
  , deviceModifierExtension :: [Extension]
  , deviceIdentifier :: [Identifier]
  , deviceDefinition :: Maybe Reference
  , deviceUdiCarrier :: [DeviceUdiCarrier]
  , deviceStatus :: Maybe DeviceStatus
  , deviceStatusReason :: [CodeableConcept]
  , deviceDistinctIdentifier :: Maybe Text
  , deviceManufacturer :: Maybe Text
  , deviceManufactureDate :: Maybe DateTime
  , deviceExpirationDate :: Maybe DateTime
  , deviceLotNumber :: Maybe Text
  , deviceSerialNumber :: Maybe Text
  , deviceDeviceName :: [DeviceDeviceName]
  , deviceModelNumber :: Maybe Text
  , devicePartNumber :: Maybe Text
  , deviceType :: Maybe CodeableConcept
  , deviceSpecialization :: [DeviceSpecialization]
  , deviceVersion :: [DeviceVersion]
  , deviceProperty :: [DeviceProperty]
  , devicePatient :: Maybe Reference
  , deviceOwner :: Maybe Reference
  , deviceContact :: [ContactPoint]
  , deviceLocation :: Maybe Reference
  , deviceUrl :: Maybe Uri
  , deviceNote :: [Annotation]
  , deviceSafety :: [CodeableConcept]
  , deviceParent :: Maybe Reference
  }
--

instance ToJSON Device where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Device")
    ,  "id" .= toJSON (deviceId p)
    ,  "meta" .= toJSON (deviceMeta p)
    ,  "implicitRules" .= toJSON (deviceImplicitRules p)
    ,  "language" .= toJSON (deviceLanguage p)
    ,  "text" .= toJSON (deviceText p)
--    , "contained" .= toJSON (deviceContained p)
    ,  "extension" .= toJSON (deviceExtension p)
    ,  "modifierExtension" .= toJSON (deviceModifierExtension p)
    ,  "identifier" .= toJSON (deviceIdentifier p)
    ,  "definition" .= toJSON (deviceDefinition p)
    ,  "udiCarrier" .= toJSON (deviceUdiCarrier p)
    ,  "status" .= toJSON (deviceStatus p)
    ,  "statusReason" .= toJSON (deviceStatusReason p)
    ,  "distinctIdentifier" .= toJSON (deviceDistinctIdentifier p)
    ,  "manufacturer" .= toJSON (deviceManufacturer p)
    ,  "manufactureDate" .= toJSON (deviceManufactureDate p)
    ,  "expirationDate" .= toJSON (deviceExpirationDate p)
    ,  "lotNumber" .= toJSON (deviceLotNumber p)
    ,  "serialNumber" .= toJSON (deviceSerialNumber p)
    ,  "deviceName" .= toJSON (deviceDeviceName p)
    ,  "modelNumber" .= toJSON (deviceModelNumber p)
    ,  "partNumber" .= toJSON (devicePartNumber p)
    ,  "type" .= toJSON (deviceType p)
    ,  "specialization" .= toJSON (deviceSpecialization p)
    ,  "version" .= toJSON (deviceVersion p)
    ,  "property" .= toJSON (deviceProperty p)
    ,  "patient" .= toJSON (devicePatient p)
    ,  "owner" .= toJSON (deviceOwner p)
    ,  "contact" .= toJSON (deviceContact p)
    ,  "location" .= toJSON (deviceLocation p)
    ,  "url" .= toJSON (deviceUrl p)
    ,  "note" .= toJSON (deviceNote p)
    ,  "safety" .= toJSON (deviceSafety p)
    ,  "parent" .= toJSON (deviceParent p)
    ]
instance FromJSON Device where
  parseJSON = withObject "Device" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Device" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        definition <- o .:? "definition"
        udiCarrier <- o .:? "udiCarrier" .!= []
        status <- o .:? "status"
        statusReason <- o .:? "statusReason" .!= []
        distinctIdentifier <- o .:? "distinctIdentifier"
        manufacturer <- o .:? "manufacturer"
        manufactureDate <- o .:? "manufactureDate"
        expirationDate <- o .:? "expirationDate"
        lotNumber <- o .:? "lotNumber"
        serialNumber <- o .:? "serialNumber"
        deviceName <- o .:? "deviceName" .!= []
        modelNumber <- o .:? "modelNumber"
        partNumber <- o .:? "partNumber"
        ty <- o .:? "type"
        specialization <- o .:? "specialization" .!= []
        version <- o .:? "version" .!= []
        property <- o .:? "property" .!= []
        patient <- o .:? "patient"
        owner <- o .:? "owner"
        contact <- o .:? "contact" .!= []
        location <- o .:? "location"
        url <- o .:? "url"
        note <- o .:? "note" .!= []
        safety <- o .:? "safety" .!= []
        parent <- o .:? "parent"
        return Device{
            deviceId = id
          , deviceMeta = meta
          , deviceImplicitRules = implicitRules
          , deviceLanguage = language
          , deviceText = text
--          , deviceContained = contained
          , deviceExtension = extension
          , deviceModifierExtension = modifierExtension
          , deviceIdentifier = identifier
          , deviceDefinition = definition
          , deviceUdiCarrier = udiCarrier
          , deviceStatus = status
          , deviceStatusReason = statusReason
          , deviceDistinctIdentifier = distinctIdentifier
          , deviceManufacturer = manufacturer
          , deviceManufactureDate = manufactureDate
          , deviceExpirationDate = expirationDate
          , deviceLotNumber = lotNumber
          , deviceSerialNumber = serialNumber
          , deviceDeviceName = deviceName
          , deviceModelNumber = modelNumber
          , devicePartNumber = partNumber
          , deviceType = ty
          , deviceSpecialization = specialization
          , deviceVersion = version
          , deviceProperty = property
          , devicePatient = patient
          , deviceOwner = owner
          , deviceContact = contact
          , deviceLocation = location
          , deviceUrl = url
          , deviceNote = note
          , deviceSafety = safety
          , deviceParent = parent
          }
      _ -> fail "not a Device"
instance Xmlbf.ToXml Device where
  toXml p = Xmlbf.element "Device" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (deviceId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (deviceMeta p))
             , OptVal   "implicitRules" (fmap toUri (deviceImplicitRules p))
             , OptVal   "language" (fmap toLanguage (deviceLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (deviceText p))
--             , PropList "contained" (fmap Xmlbf.toXml (deviceContained p))
             , PropList "extension" (fmap Xmlbf.toXml (deviceExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (deviceIdentifier p))
             , OptProp  "definition" (fmap Xmlbf.toXml (deviceDefinition p))
             , PropList "udiCarrier" (fmap Xmlbf.toXml (deviceUdiCarrier p))
             , OptVal   "status" (fmap toDeviceStatus (deviceStatus p))
             , PropList "statusReason" (fmap Xmlbf.toXml (deviceStatusReason p))
             , OptVal   "distinctIdentifier" (fmap toString (deviceDistinctIdentifier p))
             , OptVal   "manufacturer" (fmap toString (deviceManufacturer p))
             , OptVal   "manufactureDate" (fmap toDateTime (deviceManufactureDate p))
             , OptVal   "expirationDate" (fmap toDateTime (deviceExpirationDate p))
             , OptVal   "lotNumber" (fmap toString (deviceLotNumber p))
             , OptVal   "serialNumber" (fmap toString (deviceSerialNumber p))
             , PropList "deviceName" (fmap Xmlbf.toXml (deviceDeviceName p))
             , OptVal   "modelNumber" (fmap toString (deviceModelNumber p))
             , OptVal   "partNumber" (fmap toString (devicePartNumber p))
             , OptProp  "type" (fmap Xmlbf.toXml (deviceType p))
             , PropList "specialization" (fmap Xmlbf.toXml (deviceSpecialization p))
             , PropList "version" (fmap Xmlbf.toXml (deviceVersion p))
             , PropList "property" (fmap Xmlbf.toXml (deviceProperty p))
             , OptProp  "patient" (fmap Xmlbf.toXml (devicePatient p))
             , OptProp  "owner" (fmap Xmlbf.toXml (deviceOwner p))
             , PropList "contact" (fmap Xmlbf.toXml (deviceContact p))
             , OptProp  "location" (fmap Xmlbf.toXml (deviceLocation p))
             , OptVal   "url" (fmap toUri (deviceUrl p))
             , PropList "note" (fmap Xmlbf.toXml (deviceNote p))
             , PropList "safety" (fmap Xmlbf.toXml (deviceSafety p))
             , OptProp  "parent" (fmap Xmlbf.toXml (deviceParent p))
             ]
instance Xmlbf.FromXml Device where
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
    definition <- optional $ Xmlbf.pElement "definition" Xmlbf.fromXml
    udiCarrier <- many     $ Xmlbf.pElement "udiCarrier" Xmlbf.fromXml
    status <- optional $ Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    statusReason <- many     $ Xmlbf.pElement "statusReason" Xmlbf.fromXml
    distinctIdentifier <- optional $ Xmlbf.pElement "distinctIdentifier" (Xmlbf.pAttr "value")
    manufacturer <- optional $ Xmlbf.pElement "manufacturer" (Xmlbf.pAttr "value")
    manufactureDate <- optional $ Xmlbf.pElement "manufactureDate" (Xmlbf.pAttr "value")
    expirationDate <- optional $ Xmlbf.pElement "expirationDate" (Xmlbf.pAttr "value")
    lotNumber <- optional $ Xmlbf.pElement "lotNumber" (Xmlbf.pAttr "value")
    serialNumber <- optional $ Xmlbf.pElement "serialNumber" (Xmlbf.pAttr "value")
    deviceName <- many     $ Xmlbf.pElement "deviceName" Xmlbf.fromXml
    modelNumber <- optional $ Xmlbf.pElement "modelNumber" (Xmlbf.pAttr "value")
    partNumber <- optional $ Xmlbf.pElement "partNumber" (Xmlbf.pAttr "value")
    ty <- optional $ Xmlbf.pElement "type" Xmlbf.fromXml
    specialization <- many     $ Xmlbf.pElement "specialization" Xmlbf.fromXml
    version <- many     $ Xmlbf.pElement "version" Xmlbf.fromXml
    property <- many     $ Xmlbf.pElement "property" Xmlbf.fromXml
    patient <- optional $ Xmlbf.pElement "patient" Xmlbf.fromXml
    owner <- optional $ Xmlbf.pElement "owner" Xmlbf.fromXml
    contact <- many     $ Xmlbf.pElement "contact" Xmlbf.fromXml
    location <- optional $ Xmlbf.pElement "location" Xmlbf.fromXml
    url <- optional $ Xmlbf.pElement "url" (Xmlbf.pAttr "value")
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    safety <- many     $ Xmlbf.pElement "safety" Xmlbf.fromXml
    parent <- optional $ Xmlbf.pElement "parent" Xmlbf.fromXml
    return Device {
            deviceId = fmap fromId id
          , deviceMeta = meta
          , deviceImplicitRules = fmap fromUri implicitRules
          , deviceLanguage = fmap fromLanguage language
          , deviceText = text
--          , deviceContained = contained
          , deviceExtension = extension
          , deviceModifierExtension = modifierExtension
          , deviceIdentifier = identifier
          , deviceDefinition = definition
          , deviceUdiCarrier = udiCarrier
          , deviceStatus = fmap fromDeviceStatus status
          , deviceStatusReason = statusReason
          , deviceDistinctIdentifier = fmap fromString distinctIdentifier
          , deviceManufacturer = fmap fromString manufacturer
          , deviceManufactureDate = fmap fromDateTime manufactureDate
          , deviceExpirationDate = fmap fromDateTime expirationDate
          , deviceLotNumber = fmap fromString lotNumber
          , deviceSerialNumber = fmap fromString serialNumber
          , deviceDeviceName = deviceName
          , deviceModelNumber = fmap fromString modelNumber
          , devicePartNumber = fmap fromString partNumber
          , deviceType = ty
          , deviceSpecialization = specialization
          , deviceVersion = version
          , deviceProperty = property
          , devicePatient = patient
          , deviceOwner = owner
          , deviceContact = contact
          , deviceLocation = location
          , deviceUrl = fmap fromUri url
          , deviceNote = note
          , deviceSafety = safety
          , deviceParent = parent
          }



data DeviceDefinitionUdiDeviceIdentifier = DeviceDefinitionUdiDeviceIdentifier {
    deviceDefinitionUdiDeviceIdentifierAttrId :: Maybe Text
  , deviceDefinitionUdiDeviceIdentifierExtension :: [Extension]
  , deviceDefinitionUdiDeviceIdentifierModifierExtension :: [Extension]
  , deviceDefinitionUdiDeviceIdentifierDeviceIdentifier :: Text
  , deviceDefinitionUdiDeviceIdentifierIssuer :: Uri
  , deviceDefinitionUdiDeviceIdentifierJurisdiction :: Uri
  }
--

instance ToJSON DeviceDefinitionUdiDeviceIdentifier where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (deviceDefinitionUdiDeviceIdentifierAttrId p)
    ,  "extension" .= toJSON (deviceDefinitionUdiDeviceIdentifierExtension p)
    ,  "modifierExtension" .= toJSON (deviceDefinitionUdiDeviceIdentifierModifierExtension p)
    ,  "deviceIdentifier" .= toJSON (deviceDefinitionUdiDeviceIdentifierDeviceIdentifier p)
    ,  "issuer" .= toJSON (deviceDefinitionUdiDeviceIdentifierIssuer p)
    ,  "jurisdiction" .= toJSON (deviceDefinitionUdiDeviceIdentifierJurisdiction p)
    ]
instance FromJSON DeviceDefinitionUdiDeviceIdentifier where
  parseJSON = withObject "DeviceDefinitionUdiDeviceIdentifier" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        deviceIdentifier <- o .:  "deviceIdentifier"
        issuer <- o .:  "issuer"
        jurisdiction <- o .:  "jurisdiction"
        return DeviceDefinitionUdiDeviceIdentifier{
            deviceDefinitionUdiDeviceIdentifierAttrId = id
          , deviceDefinitionUdiDeviceIdentifierExtension = extension
          , deviceDefinitionUdiDeviceIdentifierModifierExtension = modifierExtension
          , deviceDefinitionUdiDeviceIdentifierDeviceIdentifier = deviceIdentifier
          , deviceDefinitionUdiDeviceIdentifierIssuer = issuer
          , deviceDefinitionUdiDeviceIdentifierJurisdiction = jurisdiction
          }
instance Xmlbf.ToXml DeviceDefinitionUdiDeviceIdentifier where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (deviceDefinitionUdiDeviceIdentifierAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (deviceDefinitionUdiDeviceIdentifierExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceDefinitionUdiDeviceIdentifierModifierExtension p))
             , Val      "deviceIdentifier" (     toString (deviceDefinitionUdiDeviceIdentifierDeviceIdentifier p))
             , Val      "issuer" (     toUri (deviceDefinitionUdiDeviceIdentifierIssuer p))
             , Val      "jurisdiction" (     toUri (deviceDefinitionUdiDeviceIdentifierJurisdiction p))
             ]
instance Xmlbf.FromXml DeviceDefinitionUdiDeviceIdentifier where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    deviceIdentifier <-            Xmlbf.pElement "deviceIdentifier" (Xmlbf.pAttr "value")
    issuer <-            Xmlbf.pElement "issuer" (Xmlbf.pAttr "value")
    jurisdiction <-            Xmlbf.pElement "jurisdiction" (Xmlbf.pAttr "value")
    return DeviceDefinitionUdiDeviceIdentifier {
            deviceDefinitionUdiDeviceIdentifierAttrId = id
          , deviceDefinitionUdiDeviceIdentifierExtension = extension
          , deviceDefinitionUdiDeviceIdentifierModifierExtension = modifierExtension
          , deviceDefinitionUdiDeviceIdentifierDeviceIdentifier =      fromString deviceIdentifier
          , deviceDefinitionUdiDeviceIdentifierIssuer =      fromUri issuer
          , deviceDefinitionUdiDeviceIdentifierJurisdiction =      fromUri jurisdiction
          }



data DeviceVersion = DeviceVersion {
    deviceVersionAttrId :: Maybe Text
  , deviceVersionExtension :: [Extension]
  , deviceVersionModifierExtension :: [Extension]
  , deviceVersionType :: Maybe CodeableConcept
  , deviceVersionComponent :: Maybe Identifier
  , deviceVersionValue :: Text
  }
--

instance ToJSON DeviceVersion where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (deviceVersionAttrId p)
    ,  "extension" .= toJSON (deviceVersionExtension p)
    ,  "modifierExtension" .= toJSON (deviceVersionModifierExtension p)
    ,  "type" .= toJSON (deviceVersionType p)
    ,  "component" .= toJSON (deviceVersionComponent p)
    ,  "value" .= toJSON (deviceVersionValue p)
    ]
instance FromJSON DeviceVersion where
  parseJSON = withObject "DeviceVersion" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:? "type"
        component <- o .:? "component"
        value <- o .:  "value"
        return DeviceVersion{
            deviceVersionAttrId = id
          , deviceVersionExtension = extension
          , deviceVersionModifierExtension = modifierExtension
          , deviceVersionType = ty
          , deviceVersionComponent = component
          , deviceVersionValue = value
          }
instance Xmlbf.ToXml DeviceVersion where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (deviceVersionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (deviceVersionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceVersionModifierExtension p))
             , OptProp  "type" (fmap Xmlbf.toXml (deviceVersionType p))
             , OptProp  "component" (fmap Xmlbf.toXml (deviceVersionComponent p))
             , Val      "value" (     toString (deviceVersionValue p))
             ]
instance Xmlbf.FromXml DeviceVersion where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <- optional $ Xmlbf.pElement "type" Xmlbf.fromXml
    component <- optional $ Xmlbf.pElement "component" Xmlbf.fromXml
    value <-            Xmlbf.pElement "value" (Xmlbf.pAttr "value")
    return DeviceVersion {
            deviceVersionAttrId = id
          , deviceVersionExtension = extension
          , deviceVersionModifierExtension = modifierExtension
          , deviceVersionType = ty
          , deviceVersionComponent = component
          , deviceVersionValue =      fromString value
          }



data DeviceUdiCarrierEntryType
    = DUCETBarcode
    | DUCETRfid
    | DUCETManual
    | DUCETCard
    | DUCETSelfReported
    | DUCETUnknown
  deriving (Eq, Show)

instance ToJSON DeviceUdiCarrierEntryType where
    toJSON DUCETBarcode = String "barcode"
    toJSON DUCETRfid = String "rfid"
    toJSON DUCETManual = String "manual"
    toJSON DUCETCard = String "card"
    toJSON DUCETSelfReported = String "self-reported"
    toJSON DUCETUnknown = String "unknown"
instance FromJSON DeviceUdiCarrierEntryType where
    parseJSON "barcode" = return DUCETBarcode
    parseJSON "rfid" = return DUCETRfid
    parseJSON "manual" = return DUCETManual
    parseJSON "card" = return DUCETCard
    parseJSON "self-reported" = return DUCETSelfReported
    parseJSON "unknown" = return DUCETUnknown

toDeviceUdiCarrierEntryType DUCETBarcode = "barcode"
toDeviceUdiCarrierEntryType DUCETRfid = "rfid"
toDeviceUdiCarrierEntryType DUCETManual = "manual"
toDeviceUdiCarrierEntryType DUCETCard = "card"
toDeviceUdiCarrierEntryType DUCETSelfReported = "self-reported"
toDeviceUdiCarrierEntryType DUCETUnknown = "unknown"
fromDeviceUdiCarrierEntryType "barcode" = DUCETBarcode
fromDeviceUdiCarrierEntryType "rfid" = DUCETRfid
fromDeviceUdiCarrierEntryType "manual" = DUCETManual
fromDeviceUdiCarrierEntryType "card" = DUCETCard
fromDeviceUdiCarrierEntryType "self-reported" = DUCETSelfReported
fromDeviceUdiCarrierEntryType "unknown" = DUCETUnknown


data DeviceUdiCarrier = DeviceUdiCarrier {
    deviceUdiCarrierAttrId :: Maybe Text
  , deviceUdiCarrierExtension :: [Extension]
  , deviceUdiCarrierModifierExtension :: [Extension]
  , deviceUdiCarrierDeviceIdentifier :: Maybe Text
  , deviceUdiCarrierIssuer :: Maybe Uri
  , deviceUdiCarrierJurisdiction :: Maybe Uri
  , deviceUdiCarrierCarrierAIDC :: Maybe Base64Binary
  , deviceUdiCarrierCarrierHRF :: Maybe Text
  , deviceUdiCarrierEntryType :: Maybe DeviceUdiCarrierEntryType
  }
--

instance ToJSON DeviceUdiCarrier where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (deviceUdiCarrierAttrId p)
    ,  "extension" .= toJSON (deviceUdiCarrierExtension p)
    ,  "modifierExtension" .= toJSON (deviceUdiCarrierModifierExtension p)
    ,  "deviceIdentifier" .= toJSON (deviceUdiCarrierDeviceIdentifier p)
    ,  "issuer" .= toJSON (deviceUdiCarrierIssuer p)
    ,  "jurisdiction" .= toJSON (deviceUdiCarrierJurisdiction p)
    ,  "carrierAIDC" .= toJSON (deviceUdiCarrierCarrierAIDC p)
    ,  "carrierHRF" .= toJSON (deviceUdiCarrierCarrierHRF p)
    ,  "entryType" .= toJSON (deviceUdiCarrierEntryType p)
    ]
instance FromJSON DeviceUdiCarrier where
  parseJSON = withObject "DeviceUdiCarrier" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        deviceIdentifier <- o .:? "deviceIdentifier"
        issuer <- o .:? "issuer"
        jurisdiction <- o .:? "jurisdiction"
        carrierAIDC <- o .:? "carrierAIDC"
        carrierHRF <- o .:? "carrierHRF"
        entryType <- o .:? "entryType"
        return DeviceUdiCarrier{
            deviceUdiCarrierAttrId = id
          , deviceUdiCarrierExtension = extension
          , deviceUdiCarrierModifierExtension = modifierExtension
          , deviceUdiCarrierDeviceIdentifier = deviceIdentifier
          , deviceUdiCarrierIssuer = issuer
          , deviceUdiCarrierJurisdiction = jurisdiction
          , deviceUdiCarrierCarrierAIDC = carrierAIDC
          , deviceUdiCarrierCarrierHRF = carrierHRF
          , deviceUdiCarrierEntryType = entryType
          }
instance Xmlbf.ToXml DeviceUdiCarrier where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (deviceUdiCarrierAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (deviceUdiCarrierExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceUdiCarrierModifierExtension p))
             , OptVal   "deviceIdentifier" (fmap toString (deviceUdiCarrierDeviceIdentifier p))
             , OptVal   "issuer" (fmap toUri (deviceUdiCarrierIssuer p))
             , OptVal   "jurisdiction" (fmap toUri (deviceUdiCarrierJurisdiction p))
             , OptVal   "carrierAIDC" (fmap toBase64Binary (deviceUdiCarrierCarrierAIDC p))
             , OptVal   "carrierHRF" (fmap toString (deviceUdiCarrierCarrierHRF p))
             , OptVal   "entryType" (fmap toDeviceUdiCarrierEntryType (deviceUdiCarrierEntryType p))
             ]
instance Xmlbf.FromXml DeviceUdiCarrier where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    deviceIdentifier <- optional $ Xmlbf.pElement "deviceIdentifier" (Xmlbf.pAttr "value")
    issuer <- optional $ Xmlbf.pElement "issuer" (Xmlbf.pAttr "value")
    jurisdiction <- optional $ Xmlbf.pElement "jurisdiction" (Xmlbf.pAttr "value")
    carrierAIDC <- optional $ Xmlbf.pElement "carrierAIDC" (Xmlbf.pAttr "value")
    carrierHRF <- optional $ Xmlbf.pElement "carrierHRF" (Xmlbf.pAttr "value")
    entryType <- optional $ Xmlbf.pElement "entryType" (Xmlbf.pAttr "value")
    return DeviceUdiCarrier {
            deviceUdiCarrierAttrId = id
          , deviceUdiCarrierExtension = extension
          , deviceUdiCarrierModifierExtension = modifierExtension
          , deviceUdiCarrierDeviceIdentifier = fmap fromString deviceIdentifier
          , deviceUdiCarrierIssuer = fmap fromUri issuer
          , deviceUdiCarrierJurisdiction = fmap fromUri jurisdiction
          , deviceUdiCarrierCarrierAIDC = fmap fromBase64Binary carrierAIDC
          , deviceUdiCarrierCarrierHRF = fmap fromString carrierHRF
          , deviceUdiCarrierEntryType = fmap fromDeviceUdiCarrierEntryType entryType
          }



data DeviceProperty = DeviceProperty {
    devicePropertyAttrId :: Maybe Text
  , devicePropertyExtension :: [Extension]
  , devicePropertyModifierExtension :: [Extension]
  , devicePropertyType :: CodeableConcept
  , devicePropertyValueQuantity :: [Quantity]
  , devicePropertyValueCode :: [CodeableConcept]
  }
--

instance ToJSON DeviceProperty where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (devicePropertyAttrId p)
    ,  "extension" .= toJSON (devicePropertyExtension p)
    ,  "modifierExtension" .= toJSON (devicePropertyModifierExtension p)
    ,  "type" .= toJSON (devicePropertyType p)
    ,  "valueQuantity" .= toJSON (devicePropertyValueQuantity p)
    ,  "valueCode" .= toJSON (devicePropertyValueCode p)
    ]
instance FromJSON DeviceProperty where
  parseJSON = withObject "DeviceProperty" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:  "type"
        valueQuantity <- o .:? "valueQuantity" .!= []
        valueCode <- o .:? "valueCode" .!= []
        return DeviceProperty{
            devicePropertyAttrId = id
          , devicePropertyExtension = extension
          , devicePropertyModifierExtension = modifierExtension
          , devicePropertyType = ty
          , devicePropertyValueQuantity = valueQuantity
          , devicePropertyValueCode = valueCode
          }
instance Xmlbf.ToXml DeviceProperty where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (devicePropertyAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (devicePropertyExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (devicePropertyModifierExtension p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (devicePropertyType p))
             , PropList "valueQuantity" (fmap Xmlbf.toXml (devicePropertyValueQuantity p))
             , PropList "valueCode" (fmap Xmlbf.toXml (devicePropertyValueCode p))
             ]
instance Xmlbf.FromXml DeviceProperty where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" Xmlbf.fromXml
    valueQuantity <- many     $ Xmlbf.pElement "valueQuantity" Xmlbf.fromXml
    valueCode <- many     $ Xmlbf.pElement "valueCode" Xmlbf.fromXml
    return DeviceProperty {
            devicePropertyAttrId = id
          , devicePropertyExtension = extension
          , devicePropertyModifierExtension = modifierExtension
          , devicePropertyType = ty
          , devicePropertyValueQuantity = valueQuantity
          , devicePropertyValueCode = valueCode
          }



data DeviceMetricCalibrationType
    = DMCTUnspecified
    | DMCTOffset
    | DMCTGain
    | DMCTTwoPoint
  deriving (Eq, Show)

instance ToJSON DeviceMetricCalibrationType where
    toJSON DMCTUnspecified = String "unspecified"
    toJSON DMCTOffset = String "offset"
    toJSON DMCTGain = String "gain"
    toJSON DMCTTwoPoint = String "two-point"
instance FromJSON DeviceMetricCalibrationType where
    parseJSON "unspecified" = return DMCTUnspecified
    parseJSON "offset" = return DMCTOffset
    parseJSON "gain" = return DMCTGain
    parseJSON "two-point" = return DMCTTwoPoint

toDeviceMetricCalibrationType DMCTUnspecified = "unspecified"
toDeviceMetricCalibrationType DMCTOffset = "offset"
toDeviceMetricCalibrationType DMCTGain = "gain"
toDeviceMetricCalibrationType DMCTTwoPoint = "two-point"
fromDeviceMetricCalibrationType "unspecified" = DMCTUnspecified
fromDeviceMetricCalibrationType "offset" = DMCTOffset
fromDeviceMetricCalibrationType "gain" = DMCTGain
fromDeviceMetricCalibrationType "two-point" = DMCTTwoPoint


data DeviceMetricCalibrationState
    = DMCSNotCalibrated
    | DMCSCalibrationRequired
    | DMCSCalibrated
    | DMCSUnspecified
  deriving (Eq, Show)

instance ToJSON DeviceMetricCalibrationState where
    toJSON DMCSNotCalibrated = String "not-calibrated"
    toJSON DMCSCalibrationRequired = String "calibration-required"
    toJSON DMCSCalibrated = String "calibrated"
    toJSON DMCSUnspecified = String "unspecified"
instance FromJSON DeviceMetricCalibrationState where
    parseJSON "not-calibrated" = return DMCSNotCalibrated
    parseJSON "calibration-required" = return DMCSCalibrationRequired
    parseJSON "calibrated" = return DMCSCalibrated
    parseJSON "unspecified" = return DMCSUnspecified

toDeviceMetricCalibrationState DMCSNotCalibrated = "not-calibrated"
toDeviceMetricCalibrationState DMCSCalibrationRequired = "calibration-required"
toDeviceMetricCalibrationState DMCSCalibrated = "calibrated"
toDeviceMetricCalibrationState DMCSUnspecified = "unspecified"
fromDeviceMetricCalibrationState "not-calibrated" = DMCSNotCalibrated
fromDeviceMetricCalibrationState "calibration-required" = DMCSCalibrationRequired
fromDeviceMetricCalibrationState "calibrated" = DMCSCalibrated
fromDeviceMetricCalibrationState "unspecified" = DMCSUnspecified


data DeviceMetricCalibration = DeviceMetricCalibration {
    deviceMetricCalibrationAttrId :: Maybe Text
  , deviceMetricCalibrationExtension :: [Extension]
  , deviceMetricCalibrationModifierExtension :: [Extension]
  , deviceMetricCalibrationType :: Maybe DeviceMetricCalibrationType
  , deviceMetricCalibrationState :: Maybe DeviceMetricCalibrationState
  , deviceMetricCalibrationTime :: Maybe Instant
  }
--

instance ToJSON DeviceMetricCalibration where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (deviceMetricCalibrationAttrId p)
    ,  "extension" .= toJSON (deviceMetricCalibrationExtension p)
    ,  "modifierExtension" .= toJSON (deviceMetricCalibrationModifierExtension p)
    ,  "type" .= toJSON (deviceMetricCalibrationType p)
    ,  "state" .= toJSON (deviceMetricCalibrationState p)
    ,  "time" .= toJSON (deviceMetricCalibrationTime p)
    ]
instance FromJSON DeviceMetricCalibration where
  parseJSON = withObject "DeviceMetricCalibration" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:? "type"
        state <- o .:? "state"
        time <- o .:? "time"
        return DeviceMetricCalibration{
            deviceMetricCalibrationAttrId = id
          , deviceMetricCalibrationExtension = extension
          , deviceMetricCalibrationModifierExtension = modifierExtension
          , deviceMetricCalibrationType = ty
          , deviceMetricCalibrationState = state
          , deviceMetricCalibrationTime = time
          }
instance Xmlbf.ToXml DeviceMetricCalibration where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (deviceMetricCalibrationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (deviceMetricCalibrationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceMetricCalibrationModifierExtension p))
             , OptVal   "type" (fmap toDeviceMetricCalibrationType (deviceMetricCalibrationType p))
             , OptVal   "state" (fmap toDeviceMetricCalibrationState (deviceMetricCalibrationState p))
             , OptVal   "time" (fmap toInstant (deviceMetricCalibrationTime p))
             ]
instance Xmlbf.FromXml DeviceMetricCalibration where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <- optional $ Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    state <- optional $ Xmlbf.pElement "state" (Xmlbf.pAttr "value")
    time <- optional $ Xmlbf.pElement "time" (Xmlbf.pAttr "value")
    return DeviceMetricCalibration {
            deviceMetricCalibrationAttrId = id
          , deviceMetricCalibrationExtension = extension
          , deviceMetricCalibrationModifierExtension = modifierExtension
          , deviceMetricCalibrationType = fmap fromDeviceMetricCalibrationType ty
          , deviceMetricCalibrationState = fmap fromDeviceMetricCalibrationState state
          , deviceMetricCalibrationTime = fmap fromInstant time
          }



data DeviceDefinitionCapability = DeviceDefinitionCapability {
    deviceDefinitionCapabilityAttrId :: Maybe Text
  , deviceDefinitionCapabilityExtension :: [Extension]
  , deviceDefinitionCapabilityModifierExtension :: [Extension]
  , deviceDefinitionCapabilityType :: CodeableConcept
  , deviceDefinitionCapabilityDescription :: [CodeableConcept]
  }
--

instance ToJSON DeviceDefinitionCapability where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (deviceDefinitionCapabilityAttrId p)
    ,  "extension" .= toJSON (deviceDefinitionCapabilityExtension p)
    ,  "modifierExtension" .= toJSON (deviceDefinitionCapabilityModifierExtension p)
    ,  "type" .= toJSON (deviceDefinitionCapabilityType p)
    ,  "description" .= toJSON (deviceDefinitionCapabilityDescription p)
    ]
instance FromJSON DeviceDefinitionCapability where
  parseJSON = withObject "DeviceDefinitionCapability" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:  "type"
        description <- o .:? "description" .!= []
        return DeviceDefinitionCapability{
            deviceDefinitionCapabilityAttrId = id
          , deviceDefinitionCapabilityExtension = extension
          , deviceDefinitionCapabilityModifierExtension = modifierExtension
          , deviceDefinitionCapabilityType = ty
          , deviceDefinitionCapabilityDescription = description
          }
instance Xmlbf.ToXml DeviceDefinitionCapability where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (deviceDefinitionCapabilityAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (deviceDefinitionCapabilityExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceDefinitionCapabilityModifierExtension p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (deviceDefinitionCapabilityType p))
             , PropList "description" (fmap Xmlbf.toXml (deviceDefinitionCapabilityDescription p))
             ]
instance Xmlbf.FromXml DeviceDefinitionCapability where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" Xmlbf.fromXml
    description <- many     $ Xmlbf.pElement "description" Xmlbf.fromXml
    return DeviceDefinitionCapability {
            deviceDefinitionCapabilityAttrId = id
          , deviceDefinitionCapabilityExtension = extension
          , deviceDefinitionCapabilityModifierExtension = modifierExtension
          , deviceDefinitionCapabilityType = ty
          , deviceDefinitionCapabilityDescription = description
          }



data DeviceMetricOperationalStatus
    = DMOSOn
    | DMOSOff
    | DMOSStandby
    | DMOSEnteredInError
  deriving (Eq, Show)

instance ToJSON DeviceMetricOperationalStatus where
    toJSON DMOSOn = String "on"
    toJSON DMOSOff = String "off"
    toJSON DMOSStandby = String "standby"
    toJSON DMOSEnteredInError = String "entered-in-error"
instance FromJSON DeviceMetricOperationalStatus where
    parseJSON "on" = return DMOSOn
    parseJSON "off" = return DMOSOff
    parseJSON "standby" = return DMOSStandby
    parseJSON "entered-in-error" = return DMOSEnteredInError

toDeviceMetricOperationalStatus DMOSOn = "on"
toDeviceMetricOperationalStatus DMOSOff = "off"
toDeviceMetricOperationalStatus DMOSStandby = "standby"
toDeviceMetricOperationalStatus DMOSEnteredInError = "entered-in-error"
fromDeviceMetricOperationalStatus "on" = DMOSOn
fromDeviceMetricOperationalStatus "off" = DMOSOff
fromDeviceMetricOperationalStatus "standby" = DMOSStandby
fromDeviceMetricOperationalStatus "entered-in-error" = DMOSEnteredInError


data DeviceMetricColor
    = DMCBlack
    | DMCRed
    | DMCGreen
    | DMCYellow
    | DMCBlue
    | DMCMagenta
    | DMCCyan
    | DMCWhite
  deriving (Eq, Show)

instance ToJSON DeviceMetricColor where
    toJSON DMCBlack = String "black"
    toJSON DMCRed = String "red"
    toJSON DMCGreen = String "green"
    toJSON DMCYellow = String "yellow"
    toJSON DMCBlue = String "blue"
    toJSON DMCMagenta = String "magenta"
    toJSON DMCCyan = String "cyan"
    toJSON DMCWhite = String "white"
instance FromJSON DeviceMetricColor where
    parseJSON "black" = return DMCBlack
    parseJSON "red" = return DMCRed
    parseJSON "green" = return DMCGreen
    parseJSON "yellow" = return DMCYellow
    parseJSON "blue" = return DMCBlue
    parseJSON "magenta" = return DMCMagenta
    parseJSON "cyan" = return DMCCyan
    parseJSON "white" = return DMCWhite

toDeviceMetricColor DMCBlack = "black"
toDeviceMetricColor DMCRed = "red"
toDeviceMetricColor DMCGreen = "green"
toDeviceMetricColor DMCYellow = "yellow"
toDeviceMetricColor DMCBlue = "blue"
toDeviceMetricColor DMCMagenta = "magenta"
toDeviceMetricColor DMCCyan = "cyan"
toDeviceMetricColor DMCWhite = "white"
fromDeviceMetricColor "black" = DMCBlack
fromDeviceMetricColor "red" = DMCRed
fromDeviceMetricColor "green" = DMCGreen
fromDeviceMetricColor "yellow" = DMCYellow
fromDeviceMetricColor "blue" = DMCBlue
fromDeviceMetricColor "magenta" = DMCMagenta
fromDeviceMetricColor "cyan" = DMCCyan
fromDeviceMetricColor "white" = DMCWhite


data DeviceMetricCategory
    = DMCMeasurement
    | DMCSetting
    | DMCCalculation
    | DMCUnspecified
  deriving (Eq, Show)

instance ToJSON DeviceMetricCategory where
    toJSON DMCMeasurement = String "measurement"
    toJSON DMCSetting = String "setting"
    toJSON DMCCalculation = String "calculation"
    toJSON DMCUnspecified = String "unspecified"
instance FromJSON DeviceMetricCategory where
    parseJSON "measurement" = return DMCMeasurement
    parseJSON "setting" = return DMCSetting
    parseJSON "calculation" = return DMCCalculation
    parseJSON "unspecified" = return DMCUnspecified

toDeviceMetricCategory DMCMeasurement = "measurement"
toDeviceMetricCategory DMCSetting = "setting"
toDeviceMetricCategory DMCCalculation = "calculation"
toDeviceMetricCategory DMCUnspecified = "unspecified"
fromDeviceMetricCategory "measurement" = DMCMeasurement
fromDeviceMetricCategory "setting" = DMCSetting
fromDeviceMetricCategory "calculation" = DMCCalculation
fromDeviceMetricCategory "unspecified" = DMCUnspecified


data DeviceMetric = DeviceMetric {
    deviceMetricId :: Maybe Id
  , deviceMetricMeta :: Maybe Meta
  , deviceMetricImplicitRules :: Maybe Uri
  , deviceMetricLanguage :: Maybe Language
  , deviceMetricText :: Maybe Narrative
--    deviceMetricContained :: [ResourceContainer]
  , deviceMetricExtension :: [Extension]
  , deviceMetricModifierExtension :: [Extension]
  , deviceMetricIdentifier :: [Identifier]
  , deviceMetricType :: CodeableConcept
  , deviceMetricUnit :: Maybe CodeableConcept
  , deviceMetricSource :: Maybe Reference
  , deviceMetricParent :: Maybe Reference
  , deviceMetricOperationalStatus :: Maybe DeviceMetricOperationalStatus
  , deviceMetricColor :: Maybe DeviceMetricColor
  , deviceMetricCategory :: DeviceMetricCategory
  , deviceMetricMeasurementPeriod :: Maybe Timing
  , deviceMetricCalibration :: [DeviceMetricCalibration]
  }
--

instance ToJSON DeviceMetric where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "DeviceMetric")
    ,  "id" .= toJSON (deviceMetricId p)
    ,  "meta" .= toJSON (deviceMetricMeta p)
    ,  "implicitRules" .= toJSON (deviceMetricImplicitRules p)
    ,  "language" .= toJSON (deviceMetricLanguage p)
    ,  "text" .= toJSON (deviceMetricText p)
--    , "contained" .= toJSON (deviceMetricContained p)
    ,  "extension" .= toJSON (deviceMetricExtension p)
    ,  "modifierExtension" .= toJSON (deviceMetricModifierExtension p)
    ,  "identifier" .= toJSON (deviceMetricIdentifier p)
    ,  "type" .= toJSON (deviceMetricType p)
    ,  "unit" .= toJSON (deviceMetricUnit p)
    ,  "source" .= toJSON (deviceMetricSource p)
    ,  "parent" .= toJSON (deviceMetricParent p)
    ,  "operationalStatus" .= toJSON (deviceMetricOperationalStatus p)
    ,  "color" .= toJSON (deviceMetricColor p)
    ,  "category" .= toJSON (deviceMetricCategory p)
    ,  "measurementPeriod" .= toJSON (deviceMetricMeasurementPeriod p)
    ,  "calibration" .= toJSON (deviceMetricCalibration p)
    ]
instance FromJSON DeviceMetric where
  parseJSON = withObject "DeviceMetric" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "DeviceMetric" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        ty <- o .:  "type"
        unit <- o .:? "unit"
        source <- o .:? "source"
        parent <- o .:? "parent"
        operationalStatus <- o .:? "operationalStatus"
        color <- o .:? "color"
        category <- o .:  "category"
        measurementPeriod <- o .:? "measurementPeriod"
        calibration <- o .:? "calibration" .!= []
        return DeviceMetric{
            deviceMetricId = id
          , deviceMetricMeta = meta
          , deviceMetricImplicitRules = implicitRules
          , deviceMetricLanguage = language
          , deviceMetricText = text
--          , deviceMetricContained = contained
          , deviceMetricExtension = extension
          , deviceMetricModifierExtension = modifierExtension
          , deviceMetricIdentifier = identifier
          , deviceMetricType = ty
          , deviceMetricUnit = unit
          , deviceMetricSource = source
          , deviceMetricParent = parent
          , deviceMetricOperationalStatus = operationalStatus
          , deviceMetricColor = color
          , deviceMetricCategory = category
          , deviceMetricMeasurementPeriod = measurementPeriod
          , deviceMetricCalibration = calibration
          }
      _ -> fail "not a DeviceMetric"
instance Xmlbf.ToXml DeviceMetric where
  toXml p = Xmlbf.element "DeviceMetric" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (deviceMetricId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (deviceMetricMeta p))
             , OptVal   "implicitRules" (fmap toUri (deviceMetricImplicitRules p))
             , OptVal   "language" (fmap toLanguage (deviceMetricLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (deviceMetricText p))
--             , PropList "contained" (fmap Xmlbf.toXml (deviceMetricContained p))
             , PropList "extension" (fmap Xmlbf.toXml (deviceMetricExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceMetricModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (deviceMetricIdentifier p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (deviceMetricType p))
             , OptProp  "unit" (fmap Xmlbf.toXml (deviceMetricUnit p))
             , OptProp  "source" (fmap Xmlbf.toXml (deviceMetricSource p))
             , OptProp  "parent" (fmap Xmlbf.toXml (deviceMetricParent p))
             , OptVal   "operationalStatus" (fmap toDeviceMetricOperationalStatus (deviceMetricOperationalStatus p))
             , OptVal   "color" (fmap toDeviceMetricColor (deviceMetricColor p))
             , Val      "category" (     toDeviceMetricCategory (deviceMetricCategory p))
             , OptProp  "measurementPeriod" (fmap Xmlbf.toXml (deviceMetricMeasurementPeriod p))
             , PropList "calibration" (fmap Xmlbf.toXml (deviceMetricCalibration p))
             ]
instance Xmlbf.FromXml DeviceMetric where
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
    ty <-            Xmlbf.pElement "type" Xmlbf.fromXml
    unit <- optional $ Xmlbf.pElement "unit" Xmlbf.fromXml
    source <- optional $ Xmlbf.pElement "source" Xmlbf.fromXml
    parent <- optional $ Xmlbf.pElement "parent" Xmlbf.fromXml
    operationalStatus <- optional $ Xmlbf.pElement "operationalStatus" (Xmlbf.pAttr "value")
    color <- optional $ Xmlbf.pElement "color" (Xmlbf.pAttr "value")
    category <-            Xmlbf.pElement "category" (Xmlbf.pAttr "value")
    measurementPeriod <- optional $ Xmlbf.pElement "measurementPeriod" Xmlbf.fromXml
    calibration <- many     $ Xmlbf.pElement "calibration" Xmlbf.fromXml
    return DeviceMetric {
            deviceMetricId = fmap fromId id
          , deviceMetricMeta = meta
          , deviceMetricImplicitRules = fmap fromUri implicitRules
          , deviceMetricLanguage = fmap fromLanguage language
          , deviceMetricText = text
--          , deviceMetricContained = contained
          , deviceMetricExtension = extension
          , deviceMetricModifierExtension = modifierExtension
          , deviceMetricIdentifier = identifier
          , deviceMetricType = ty
          , deviceMetricUnit = unit
          , deviceMetricSource = source
          , deviceMetricParent = parent
          , deviceMetricOperationalStatus = fmap fromDeviceMetricOperationalStatus operationalStatus
          , deviceMetricColor = fmap fromDeviceMetricColor color
          , deviceMetricCategory =      fromDeviceMetricCategory category
          , deviceMetricMeasurementPeriod = measurementPeriod
          , deviceMetricCalibration = calibration
          }



data DeviceRequestParameterValue
    = DeviceRequestParameterValueCodeableConcept CodeableConcept
    | DeviceRequestParameterValueQuantity Quantity
    | DeviceRequestParameterValueRange Range
    | DeviceRequestParameterValueBoolean Boolean
    deriving (Eq, Show)

data DeviceRequestParameter = DeviceRequestParameter {
    deviceRequestParameterAttrId :: Maybe Text
  , deviceRequestParameterExtension :: [Extension]
  , deviceRequestParameterModifierExtension :: [Extension]
  , deviceRequestParameterCode :: Maybe CodeableConcept
  , deviceRequestParameterValueCodeableConcept :: Maybe CodeableConcept
  , deviceRequestParameterValueQuantity :: Maybe Quantity
  , deviceRequestParameterValueRange :: Maybe Range
  , deviceRequestParameterValueBoolean :: Maybe Boolean
  }
--

instance ToJSON DeviceRequestParameter where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (deviceRequestParameterAttrId p)
    ,  "extension" .= toJSON (deviceRequestParameterExtension p)
    ,  "modifierExtension" .= toJSON (deviceRequestParameterModifierExtension p)
    ,  "code" .= toJSON (deviceRequestParameterCode p)
    ,  "valueCodeableConcept" .= toJSON (deviceRequestParameterValueCodeableConcept p)
    ,  "valueQuantity" .= toJSON (deviceRequestParameterValueQuantity p)
    ,  "valueRange" .= toJSON (deviceRequestParameterValueRange p)
    ,  "valueBoolean" .= toJSON (deviceRequestParameterValueBoolean p)
    ]
instance FromJSON DeviceRequestParameter where
  parseJSON = withObject "DeviceRequestParameter" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:? "code"
        valueCodeableConcept <- o .:? "valueCodeableConcept"
        valueQuantity <- o .:? "valueQuantity"
        valueRange <- o .:? "valueRange"
        valueBoolean <- o .:? "valueBoolean"
        return DeviceRequestParameter{
            deviceRequestParameterAttrId = id
          , deviceRequestParameterExtension = extension
          , deviceRequestParameterModifierExtension = modifierExtension
          , deviceRequestParameterCode = code
          , deviceRequestParameterValueCodeableConcept = valueCodeableConcept
          , deviceRequestParameterValueQuantity = valueQuantity
          , deviceRequestParameterValueRange = valueRange
          , deviceRequestParameterValueBoolean = valueBoolean
          }
instance Xmlbf.ToXml DeviceRequestParameter where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (deviceRequestParameterAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (deviceRequestParameterExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceRequestParameterModifierExtension p))
             , OptProp  "code" (fmap Xmlbf.toXml (deviceRequestParameterCode p))
             , OptProp  "valueCodeableConcept" (fmap Xmlbf.toXml (deviceRequestParameterValueCodeableConcept p))
             , OptProp  "valueQuantity" (fmap Xmlbf.toXml (deviceRequestParameterValueQuantity p))
             , OptProp  "valueRange" (fmap Xmlbf.toXml (deviceRequestParameterValueRange p))
             , OptVal   "valueBoolean" (fmap toBoolean (deviceRequestParameterValueBoolean p))
             ]
instance Xmlbf.FromXml DeviceRequestParameter where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    valueCodeableConcept <- optional $ Xmlbf.pElement "valueCodeableConcept" Xmlbf.fromXml
    valueQuantity <- optional $ Xmlbf.pElement "valueQuantity" Xmlbf.fromXml
    valueRange <- optional $ Xmlbf.pElement "valueRange" Xmlbf.fromXml
    valueBoolean <- optional $ Xmlbf.pElement "valueBoolean" (Xmlbf.pAttr "value")
    return DeviceRequestParameter {
            deviceRequestParameterAttrId = id
          , deviceRequestParameterExtension = extension
          , deviceRequestParameterModifierExtension = modifierExtension
          , deviceRequestParameterCode = code
          , deviceRequestParameterValueCodeableConcept = valueCodeableConcept
          , deviceRequestParameterValueQuantity = valueQuantity
          , deviceRequestParameterValueRange = valueRange
          , deviceRequestParameterValueBoolean = fmap fromBoolean valueBoolean
          }



data DeviceRequestStatus
    = DRSDraft
    | DRSActive
    | DRSOnHold
    | DRSRevoked
    | DRSCompleted
    | DRSEnteredInError
    | DRSUnknown
  deriving (Eq, Show)

instance ToJSON DeviceRequestStatus where
    toJSON DRSDraft = String "draft"
    toJSON DRSActive = String "active"
    toJSON DRSOnHold = String "on-hold"
    toJSON DRSRevoked = String "revoked"
    toJSON DRSCompleted = String "completed"
    toJSON DRSEnteredInError = String "entered-in-error"
    toJSON DRSUnknown = String "unknown"
instance FromJSON DeviceRequestStatus where
    parseJSON "draft" = return DRSDraft
    parseJSON "active" = return DRSActive
    parseJSON "on-hold" = return DRSOnHold
    parseJSON "revoked" = return DRSRevoked
    parseJSON "completed" = return DRSCompleted
    parseJSON "entered-in-error" = return DRSEnteredInError
    parseJSON "unknown" = return DRSUnknown

toDeviceRequestStatus DRSDraft = "draft"
toDeviceRequestStatus DRSActive = "active"
toDeviceRequestStatus DRSOnHold = "on-hold"
toDeviceRequestStatus DRSRevoked = "revoked"
toDeviceRequestStatus DRSCompleted = "completed"
toDeviceRequestStatus DRSEnteredInError = "entered-in-error"
toDeviceRequestStatus DRSUnknown = "unknown"
fromDeviceRequestStatus "draft" = DRSDraft
fromDeviceRequestStatus "active" = DRSActive
fromDeviceRequestStatus "on-hold" = DRSOnHold
fromDeviceRequestStatus "revoked" = DRSRevoked
fromDeviceRequestStatus "completed" = DRSCompleted
fromDeviceRequestStatus "entered-in-error" = DRSEnteredInError
fromDeviceRequestStatus "unknown" = DRSUnknown


data DeviceRequestIntent
    = DRIProposal
    | DRIPlan
    | DRIDirective
    | DRIOrder
    | DRIOriginalOrder
    | DRIReflexOrder
    | DRIFillerOrder
    | DRIInstanceOrder
    | DRIOption
  deriving (Eq, Show)

instance ToJSON DeviceRequestIntent where
    toJSON DRIProposal = String "proposal"
    toJSON DRIPlan = String "plan"
    toJSON DRIDirective = String "directive"
    toJSON DRIOrder = String "order"
    toJSON DRIOriginalOrder = String "original-order"
    toJSON DRIReflexOrder = String "reflex-order"
    toJSON DRIFillerOrder = String "filler-order"
    toJSON DRIInstanceOrder = String "instance-order"
    toJSON DRIOption = String "option"
instance FromJSON DeviceRequestIntent where
    parseJSON "proposal" = return DRIProposal
    parseJSON "plan" = return DRIPlan
    parseJSON "directive" = return DRIDirective
    parseJSON "order" = return DRIOrder
    parseJSON "original-order" = return DRIOriginalOrder
    parseJSON "reflex-order" = return DRIReflexOrder
    parseJSON "filler-order" = return DRIFillerOrder
    parseJSON "instance-order" = return DRIInstanceOrder
    parseJSON "option" = return DRIOption

toDeviceRequestIntent DRIProposal = "proposal"
toDeviceRequestIntent DRIPlan = "plan"
toDeviceRequestIntent DRIDirective = "directive"
toDeviceRequestIntent DRIOrder = "order"
toDeviceRequestIntent DRIOriginalOrder = "original-order"
toDeviceRequestIntent DRIReflexOrder = "reflex-order"
toDeviceRequestIntent DRIFillerOrder = "filler-order"
toDeviceRequestIntent DRIInstanceOrder = "instance-order"
toDeviceRequestIntent DRIOption = "option"
fromDeviceRequestIntent "proposal" = DRIProposal
fromDeviceRequestIntent "plan" = DRIPlan
fromDeviceRequestIntent "directive" = DRIDirective
fromDeviceRequestIntent "order" = DRIOrder
fromDeviceRequestIntent "original-order" = DRIOriginalOrder
fromDeviceRequestIntent "reflex-order" = DRIReflexOrder
fromDeviceRequestIntent "filler-order" = DRIFillerOrder
fromDeviceRequestIntent "instance-order" = DRIInstanceOrder
fromDeviceRequestIntent "option" = DRIOption


data DeviceRequestPriority
    = DRPRoutine
    | DRPUrgent
    | DRPAsap
    | DRPStat
  deriving (Eq, Show)

instance ToJSON DeviceRequestPriority where
    toJSON DRPRoutine = String "routine"
    toJSON DRPUrgent = String "urgent"
    toJSON DRPAsap = String "asap"
    toJSON DRPStat = String "stat"
instance FromJSON DeviceRequestPriority where
    parseJSON "routine" = return DRPRoutine
    parseJSON "urgent" = return DRPUrgent
    parseJSON "asap" = return DRPAsap
    parseJSON "stat" = return DRPStat

toDeviceRequestPriority DRPRoutine = "routine"
toDeviceRequestPriority DRPUrgent = "urgent"
toDeviceRequestPriority DRPAsap = "asap"
toDeviceRequestPriority DRPStat = "stat"
fromDeviceRequestPriority "routine" = DRPRoutine
fromDeviceRequestPriority "urgent" = DRPUrgent
fromDeviceRequestPriority "asap" = DRPAsap
fromDeviceRequestPriority "stat" = DRPStat


data DeviceRequestCode
    = DeviceRequestCodeReference Reference
    | DeviceRequestCodeCodeableConcept CodeableConcept
    deriving (Eq, Show)

data DeviceRequestOccurrence
    = DeviceRequestOccurrenceDateTime DateTime
    | DeviceRequestOccurrencePeriod Period
    | DeviceRequestOccurrenceTiming Timing
    deriving (Eq, Show)

data DeviceRequest = DeviceRequest {
    deviceRequestId :: Maybe Id
  , deviceRequestMeta :: Maybe Meta
  , deviceRequestImplicitRules :: Maybe Uri
  , deviceRequestLanguage :: Maybe Language
  , deviceRequestText :: Maybe Narrative
--    deviceRequestContained :: [ResourceContainer]
  , deviceRequestExtension :: [Extension]
  , deviceRequestModifierExtension :: [Extension]
  , deviceRequestIdentifier :: [Identifier]
  , deviceRequestInstantiatesCanonical :: [Canonical]
  , deviceRequestInstantiatesUri :: [Uri]
  , deviceRequestBasedOn :: [Reference]
  , deviceRequestPriorRequest :: [Reference]
  , deviceRequestGroupIdentifier :: Maybe Identifier
  , deviceRequestStatus :: Maybe DeviceRequestStatus
  , deviceRequestIntent :: DeviceRequestIntent
  , deviceRequestPriority :: Maybe DeviceRequestPriority
  , deviceRequestCodeReference :: Reference
  , deviceRequestCodeCodeableConcept :: CodeableConcept
  , deviceRequestParameter :: [DeviceRequestParameter]
  , deviceRequestSubject :: Reference
  , deviceRequestEncounter :: Maybe Reference
  , deviceRequestOccurrenceDateTime :: Maybe DateTime
  , deviceRequestOccurrencePeriod :: Maybe Period
  , deviceRequestOccurrenceTiming :: Maybe Timing
  , deviceRequestAuthoredOn :: Maybe DateTime
  , deviceRequestRequester :: Maybe Reference
  , deviceRequestPerformerType :: Maybe CodeableConcept
  , deviceRequestPerformer :: Maybe Reference
  , deviceRequestReasonCode :: [CodeableConcept]
  , deviceRequestReasonReference :: [Reference]
  , deviceRequestInsurance :: [Reference]
  , deviceRequestSupportingInfo :: [Reference]
  , deviceRequestNote :: [Annotation]
  , deviceRequestRelevantHistory :: [Reference]
  }
--

instance ToJSON DeviceRequest where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "DeviceRequest")
    ,  "id" .= toJSON (deviceRequestId p)
    ,  "meta" .= toJSON (deviceRequestMeta p)
    ,  "implicitRules" .= toJSON (deviceRequestImplicitRules p)
    ,  "language" .= toJSON (deviceRequestLanguage p)
    ,  "text" .= toJSON (deviceRequestText p)
--    , "contained" .= toJSON (deviceRequestContained p)
    ,  "extension" .= toJSON (deviceRequestExtension p)
    ,  "modifierExtension" .= toJSON (deviceRequestModifierExtension p)
    ,  "identifier" .= toJSON (deviceRequestIdentifier p)
    ,  "instantiatesCanonical" .= toJSON (deviceRequestInstantiatesCanonical p)
    ,  "instantiatesUri" .= toJSON (deviceRequestInstantiatesUri p)
    ,  "basedOn" .= toJSON (deviceRequestBasedOn p)
    ,  "priorRequest" .= toJSON (deviceRequestPriorRequest p)
    ,  "groupIdentifier" .= toJSON (deviceRequestGroupIdentifier p)
    ,  "status" .= toJSON (deviceRequestStatus p)
    ,  "intent" .= toJSON (deviceRequestIntent p)
    ,  "priority" .= toJSON (deviceRequestPriority p)
    ,  "codeReference" .= toJSON (deviceRequestCodeReference p)
    ,  "codeCodeableConcept" .= toJSON (deviceRequestCodeCodeableConcept p)
    ,  "parameter" .= toJSON (deviceRequestParameter p)
    ,  "subject" .= toJSON (deviceRequestSubject p)
    ,  "encounter" .= toJSON (deviceRequestEncounter p)
    ,  "occurrenceDateTime" .= toJSON (deviceRequestOccurrenceDateTime p)
    ,  "occurrencePeriod" .= toJSON (deviceRequestOccurrencePeriod p)
    ,  "occurrenceTiming" .= toJSON (deviceRequestOccurrenceTiming p)
    ,  "authoredOn" .= toJSON (deviceRequestAuthoredOn p)
    ,  "requester" .= toJSON (deviceRequestRequester p)
    ,  "performerType" .= toJSON (deviceRequestPerformerType p)
    ,  "performer" .= toJSON (deviceRequestPerformer p)
    ,  "reasonCode" .= toJSON (deviceRequestReasonCode p)
    ,  "reasonReference" .= toJSON (deviceRequestReasonReference p)
    ,  "insurance" .= toJSON (deviceRequestInsurance p)
    ,  "supportingInfo" .= toJSON (deviceRequestSupportingInfo p)
    ,  "note" .= toJSON (deviceRequestNote p)
    ,  "relevantHistory" .= toJSON (deviceRequestRelevantHistory p)
    ]
instance FromJSON DeviceRequest where
  parseJSON = withObject "DeviceRequest" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "DeviceRequest" -> do
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
        priorRequest <- o .:? "priorRequest" .!= []
        groupIdentifier <- o .:? "groupIdentifier"
        status <- o .:? "status"
        intent <- o .:  "intent"
        priority <- o .:? "priority"
        codeReference <- o .:  "codeReference"
        codeCodeableConcept <- o .:  "codeCodeableConcept"
        parameter <- o .:? "parameter" .!= []
        subject <- o .:  "subject"
        encounter <- o .:? "encounter"
        occurrenceDateTime <- o .:? "occurrenceDateTime"
        occurrencePeriod <- o .:? "occurrencePeriod"
        occurrenceTiming <- o .:? "occurrenceTiming"
        authoredOn <- o .:? "authoredOn"
        requester <- o .:? "requester"
        performerType <- o .:? "performerType"
        performer <- o .:? "performer"
        reasonCode <- o .:? "reasonCode" .!= []
        reasonReference <- o .:? "reasonReference" .!= []
        insurance <- o .:? "insurance" .!= []
        supportingInfo <- o .:? "supportingInfo" .!= []
        note <- o .:? "note" .!= []
        relevantHistory <- o .:? "relevantHistory" .!= []
        return DeviceRequest{
            deviceRequestId = id
          , deviceRequestMeta = meta
          , deviceRequestImplicitRules = implicitRules
          , deviceRequestLanguage = language
          , deviceRequestText = text
--          , deviceRequestContained = contained
          , deviceRequestExtension = extension
          , deviceRequestModifierExtension = modifierExtension
          , deviceRequestIdentifier = identifier
          , deviceRequestInstantiatesCanonical = instantiatesCanonical
          , deviceRequestInstantiatesUri = instantiatesUri
          , deviceRequestBasedOn = basedOn
          , deviceRequestPriorRequest = priorRequest
          , deviceRequestGroupIdentifier = groupIdentifier
          , deviceRequestStatus = status
          , deviceRequestIntent = intent
          , deviceRequestPriority = priority
          , deviceRequestCodeReference = codeReference
          , deviceRequestCodeCodeableConcept = codeCodeableConcept
          , deviceRequestParameter = parameter
          , deviceRequestSubject = subject
          , deviceRequestEncounter = encounter
          , deviceRequestOccurrenceDateTime = occurrenceDateTime
          , deviceRequestOccurrencePeriod = occurrencePeriod
          , deviceRequestOccurrenceTiming = occurrenceTiming
          , deviceRequestAuthoredOn = authoredOn
          , deviceRequestRequester = requester
          , deviceRequestPerformerType = performerType
          , deviceRequestPerformer = performer
          , deviceRequestReasonCode = reasonCode
          , deviceRequestReasonReference = reasonReference
          , deviceRequestInsurance = insurance
          , deviceRequestSupportingInfo = supportingInfo
          , deviceRequestNote = note
          , deviceRequestRelevantHistory = relevantHistory
          }
      _ -> fail "not a DeviceRequest"
instance Xmlbf.ToXml DeviceRequest where
  toXml p = Xmlbf.element "DeviceRequest" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (deviceRequestId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (deviceRequestMeta p))
             , OptVal   "implicitRules" (fmap toUri (deviceRequestImplicitRules p))
             , OptVal   "language" (fmap toLanguage (deviceRequestLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (deviceRequestText p))
--             , PropList "contained" (fmap Xmlbf.toXml (deviceRequestContained p))
             , PropList "extension" (fmap Xmlbf.toXml (deviceRequestExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceRequestModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (deviceRequestIdentifier p))
             , ValList  "instantiatesCanonical" (fmap toCanonical (deviceRequestInstantiatesCanonical p))
             , ValList  "instantiatesUri" (fmap toUri (deviceRequestInstantiatesUri p))
             , PropList "basedOn" (fmap Xmlbf.toXml (deviceRequestBasedOn p))
             , PropList "priorRequest" (fmap Xmlbf.toXml (deviceRequestPriorRequest p))
             , OptProp  "groupIdentifier" (fmap Xmlbf.toXml (deviceRequestGroupIdentifier p))
             , OptVal   "status" (fmap toDeviceRequestStatus (deviceRequestStatus p))
             , Val      "intent" (     toDeviceRequestIntent (deviceRequestIntent p))
             , OptVal   "priority" (fmap toDeviceRequestPriority (deviceRequestPriority p))
             , Prop     "codeReference" (HM.empty, Xmlbf.toXml (deviceRequestCodeReference p))
             , Prop     "codeCodeableConcept" (HM.empty, Xmlbf.toXml (deviceRequestCodeCodeableConcept p))
             , PropList "parameter" (fmap Xmlbf.toXml (deviceRequestParameter p))
             , Prop     "subject" (HM.empty, Xmlbf.toXml (deviceRequestSubject p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (deviceRequestEncounter p))
             , OptVal   "occurrenceDateTime" (fmap toDateTime (deviceRequestOccurrenceDateTime p))
             , OptProp  "occurrencePeriod" (fmap Xmlbf.toXml (deviceRequestOccurrencePeriod p))
             , OptProp  "occurrenceTiming" (fmap Xmlbf.toXml (deviceRequestOccurrenceTiming p))
             , OptVal   "authoredOn" (fmap toDateTime (deviceRequestAuthoredOn p))
             , OptProp  "requester" (fmap Xmlbf.toXml (deviceRequestRequester p))
             , OptProp  "performerType" (fmap Xmlbf.toXml (deviceRequestPerformerType p))
             , OptProp  "performer" (fmap Xmlbf.toXml (deviceRequestPerformer p))
             , PropList "reasonCode" (fmap Xmlbf.toXml (deviceRequestReasonCode p))
             , PropList "reasonReference" (fmap Xmlbf.toXml (deviceRequestReasonReference p))
             , PropList "insurance" (fmap Xmlbf.toXml (deviceRequestInsurance p))
             , PropList "supportingInfo" (fmap Xmlbf.toXml (deviceRequestSupportingInfo p))
             , PropList "note" (fmap Xmlbf.toXml (deviceRequestNote p))
             , PropList "relevantHistory" (fmap Xmlbf.toXml (deviceRequestRelevantHistory p))
             ]
instance Xmlbf.FromXml DeviceRequest where
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
    priorRequest <- many     $ Xmlbf.pElement "priorRequest" Xmlbf.fromXml
    groupIdentifier <- optional $ Xmlbf.pElement "groupIdentifier" Xmlbf.fromXml
    status <- optional $ Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    intent <-            Xmlbf.pElement "intent" (Xmlbf.pAttr "value")
    priority <- optional $ Xmlbf.pElement "priority" (Xmlbf.pAttr "value")
    codeReference <-            Xmlbf.pElement "codeReference" Xmlbf.fromXml
    codeCodeableConcept <-            Xmlbf.pElement "codeCodeableConcept" Xmlbf.fromXml
    parameter <- many     $ Xmlbf.pElement "parameter" Xmlbf.fromXml
    subject <-            Xmlbf.pElement "subject" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    occurrenceDateTime <- optional $ Xmlbf.pElement "occurrenceDateTime" (Xmlbf.pAttr "value")
    occurrencePeriod <- optional $ Xmlbf.pElement "occurrencePeriod" Xmlbf.fromXml
    occurrenceTiming <- optional $ Xmlbf.pElement "occurrenceTiming" Xmlbf.fromXml
    authoredOn <- optional $ Xmlbf.pElement "authoredOn" (Xmlbf.pAttr "value")
    requester <- optional $ Xmlbf.pElement "requester" Xmlbf.fromXml
    performerType <- optional $ Xmlbf.pElement "performerType" Xmlbf.fromXml
    performer <- optional $ Xmlbf.pElement "performer" Xmlbf.fromXml
    reasonCode <- many     $ Xmlbf.pElement "reasonCode" Xmlbf.fromXml
    reasonReference <- many     $ Xmlbf.pElement "reasonReference" Xmlbf.fromXml
    insurance <- many     $ Xmlbf.pElement "insurance" Xmlbf.fromXml
    supportingInfo <- many     $ Xmlbf.pElement "supportingInfo" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    relevantHistory <- many     $ Xmlbf.pElement "relevantHistory" Xmlbf.fromXml
    return DeviceRequest {
            deviceRequestId = fmap fromId id
          , deviceRequestMeta = meta
          , deviceRequestImplicitRules = fmap fromUri implicitRules
          , deviceRequestLanguage = fmap fromLanguage language
          , deviceRequestText = text
--          , deviceRequestContained = contained
          , deviceRequestExtension = extension
          , deviceRequestModifierExtension = modifierExtension
          , deviceRequestIdentifier = identifier
          , deviceRequestInstantiatesCanonical = fmap fromCanonical instantiatesCanonical
          , deviceRequestInstantiatesUri = fmap fromUri instantiatesUri
          , deviceRequestBasedOn = basedOn
          , deviceRequestPriorRequest = priorRequest
          , deviceRequestGroupIdentifier = groupIdentifier
          , deviceRequestStatus = fmap fromDeviceRequestStatus status
          , deviceRequestIntent =      fromDeviceRequestIntent intent
          , deviceRequestPriority = fmap fromDeviceRequestPriority priority
          , deviceRequestCodeReference = codeReference
          , deviceRequestCodeCodeableConcept = codeCodeableConcept
          , deviceRequestParameter = parameter
          , deviceRequestSubject = subject
          , deviceRequestEncounter = encounter
          , deviceRequestOccurrenceDateTime = fmap fromDateTime occurrenceDateTime
          , deviceRequestOccurrencePeriod = occurrencePeriod
          , deviceRequestOccurrenceTiming = occurrenceTiming
          , deviceRequestAuthoredOn = fmap fromDateTime authoredOn
          , deviceRequestRequester = requester
          , deviceRequestPerformerType = performerType
          , deviceRequestPerformer = performer
          , deviceRequestReasonCode = reasonCode
          , deviceRequestReasonReference = reasonReference
          , deviceRequestInsurance = insurance
          , deviceRequestSupportingInfo = supportingInfo
          , deviceRequestNote = note
          , deviceRequestRelevantHistory = relevantHistory
          }



data DeviceDefinitionMaterial = DeviceDefinitionMaterial {
    deviceDefinitionMaterialAttrId :: Maybe Text
  , deviceDefinitionMaterialExtension :: [Extension]
  , deviceDefinitionMaterialModifierExtension :: [Extension]
  , deviceDefinitionMaterialSubstance :: CodeableConcept
  , deviceDefinitionMaterialAlternate :: Maybe Boolean
  , deviceDefinitionMaterialAllergenicIndicator :: Maybe Boolean
  }
--

instance ToJSON DeviceDefinitionMaterial where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (deviceDefinitionMaterialAttrId p)
    ,  "extension" .= toJSON (deviceDefinitionMaterialExtension p)
    ,  "modifierExtension" .= toJSON (deviceDefinitionMaterialModifierExtension p)
    ,  "substance" .= toJSON (deviceDefinitionMaterialSubstance p)
    ,  "alternate" .= toJSON (deviceDefinitionMaterialAlternate p)
    ,  "allergenicIndicator" .= toJSON (deviceDefinitionMaterialAllergenicIndicator p)
    ]
instance FromJSON DeviceDefinitionMaterial where
  parseJSON = withObject "DeviceDefinitionMaterial" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        substance <- o .:  "substance"
        alternate <- o .:? "alternate"
        allergenicIndicator <- o .:? "allergenicIndicator"
        return DeviceDefinitionMaterial{
            deviceDefinitionMaterialAttrId = id
          , deviceDefinitionMaterialExtension = extension
          , deviceDefinitionMaterialModifierExtension = modifierExtension
          , deviceDefinitionMaterialSubstance = substance
          , deviceDefinitionMaterialAlternate = alternate
          , deviceDefinitionMaterialAllergenicIndicator = allergenicIndicator
          }
instance Xmlbf.ToXml DeviceDefinitionMaterial where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (deviceDefinitionMaterialAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (deviceDefinitionMaterialExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceDefinitionMaterialModifierExtension p))
             , Prop     "substance" (HM.empty, Xmlbf.toXml (deviceDefinitionMaterialSubstance p))
             , OptVal   "alternate" (fmap toBoolean (deviceDefinitionMaterialAlternate p))
             , OptVal   "allergenicIndicator" (fmap toBoolean (deviceDefinitionMaterialAllergenicIndicator p))
             ]
instance Xmlbf.FromXml DeviceDefinitionMaterial where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    substance <-            Xmlbf.pElement "substance" Xmlbf.fromXml
    alternate <- optional $ Xmlbf.pElement "alternate" (Xmlbf.pAttr "value")
    allergenicIndicator <- optional $ Xmlbf.pElement "allergenicIndicator" (Xmlbf.pAttr "value")
    return DeviceDefinitionMaterial {
            deviceDefinitionMaterialAttrId = id
          , deviceDefinitionMaterialExtension = extension
          , deviceDefinitionMaterialModifierExtension = modifierExtension
          , deviceDefinitionMaterialSubstance = substance
          , deviceDefinitionMaterialAlternate = fmap fromBoolean alternate
          , deviceDefinitionMaterialAllergenicIndicator = fmap fromBoolean allergenicIndicator
          }



data DeviceDeviceNameType
    = DDNTUdiLabelName
    | DDNTUserFriendlyName
    | DDNTPatientReportedName
    | DDNTManufacturerName
    | DDNTModelName
    | DDNTOther
  deriving (Eq, Show)

instance ToJSON DeviceDeviceNameType where
    toJSON DDNTUdiLabelName = String "udi-label-name"
    toJSON DDNTUserFriendlyName = String "user-friendly-name"
    toJSON DDNTPatientReportedName = String "patient-reported-name"
    toJSON DDNTManufacturerName = String "manufacturer-name"
    toJSON DDNTModelName = String "model-name"
    toJSON DDNTOther = String "other"
instance FromJSON DeviceDeviceNameType where
    parseJSON "udi-label-name" = return DDNTUdiLabelName
    parseJSON "user-friendly-name" = return DDNTUserFriendlyName
    parseJSON "patient-reported-name" = return DDNTPatientReportedName
    parseJSON "manufacturer-name" = return DDNTManufacturerName
    parseJSON "model-name" = return DDNTModelName
    parseJSON "other" = return DDNTOther

toDeviceDeviceNameType DDNTUdiLabelName = "udi-label-name"
toDeviceDeviceNameType DDNTUserFriendlyName = "user-friendly-name"
toDeviceDeviceNameType DDNTPatientReportedName = "patient-reported-name"
toDeviceDeviceNameType DDNTManufacturerName = "manufacturer-name"
toDeviceDeviceNameType DDNTModelName = "model-name"
toDeviceDeviceNameType DDNTOther = "other"
fromDeviceDeviceNameType "udi-label-name" = DDNTUdiLabelName
fromDeviceDeviceNameType "user-friendly-name" = DDNTUserFriendlyName
fromDeviceDeviceNameType "patient-reported-name" = DDNTPatientReportedName
fromDeviceDeviceNameType "manufacturer-name" = DDNTManufacturerName
fromDeviceDeviceNameType "model-name" = DDNTModelName
fromDeviceDeviceNameType "other" = DDNTOther


data DeviceDeviceName = DeviceDeviceName {
    deviceDeviceNameAttrId :: Maybe Text
  , deviceDeviceNameExtension :: [Extension]
  , deviceDeviceNameModifierExtension :: [Extension]
  , deviceDeviceNameName :: Text
  , deviceDeviceNameType :: DeviceDeviceNameType
  }
--

instance ToJSON DeviceDeviceName where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (deviceDeviceNameAttrId p)
    ,  "extension" .= toJSON (deviceDeviceNameExtension p)
    ,  "modifierExtension" .= toJSON (deviceDeviceNameModifierExtension p)
    ,  "name" .= toJSON (deviceDeviceNameName p)
    ,  "type" .= toJSON (deviceDeviceNameType p)
    ]
instance FromJSON DeviceDeviceName where
  parseJSON = withObject "DeviceDeviceName" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        name <- o .:  "name"
        ty <- o .:  "type"
        return DeviceDeviceName{
            deviceDeviceNameAttrId = id
          , deviceDeviceNameExtension = extension
          , deviceDeviceNameModifierExtension = modifierExtension
          , deviceDeviceNameName = name
          , deviceDeviceNameType = ty
          }
instance Xmlbf.ToXml DeviceDeviceName where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (deviceDeviceNameAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (deviceDeviceNameExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceDeviceNameModifierExtension p))
             , Val      "name" (     toString (deviceDeviceNameName p))
             , Val      "type" (     toDeviceDeviceNameType (deviceDeviceNameType p))
             ]
instance Xmlbf.FromXml DeviceDeviceName where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    name <-            Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    ty <-            Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    return DeviceDeviceName {
            deviceDeviceNameAttrId = id
          , deviceDeviceNameExtension = extension
          , deviceDeviceNameModifierExtension = modifierExtension
          , deviceDeviceNameName =      fromString name
          , deviceDeviceNameType =      fromDeviceDeviceNameType ty
          }



data DeviceDefinitionDeviceNameType
    = DDDNTUdiLabelName
    | DDDNTUserFriendlyName
    | DDDNTPatientReportedName
    | DDDNTManufacturerName
    | DDDNTModelName
    | DDDNTOther
  deriving (Eq, Show)

instance ToJSON DeviceDefinitionDeviceNameType where
    toJSON DDDNTUdiLabelName = String "udi-label-name"
    toJSON DDDNTUserFriendlyName = String "user-friendly-name"
    toJSON DDDNTPatientReportedName = String "patient-reported-name"
    toJSON DDDNTManufacturerName = String "manufacturer-name"
    toJSON DDDNTModelName = String "model-name"
    toJSON DDDNTOther = String "other"
instance FromJSON DeviceDefinitionDeviceNameType where
    parseJSON "udi-label-name" = return DDDNTUdiLabelName
    parseJSON "user-friendly-name" = return DDDNTUserFriendlyName
    parseJSON "patient-reported-name" = return DDDNTPatientReportedName
    parseJSON "manufacturer-name" = return DDDNTManufacturerName
    parseJSON "model-name" = return DDDNTModelName
    parseJSON "other" = return DDDNTOther

toDeviceDefinitionDeviceNameType DDDNTUdiLabelName = "udi-label-name"
toDeviceDefinitionDeviceNameType DDDNTUserFriendlyName = "user-friendly-name"
toDeviceDefinitionDeviceNameType DDDNTPatientReportedName = "patient-reported-name"
toDeviceDefinitionDeviceNameType DDDNTManufacturerName = "manufacturer-name"
toDeviceDefinitionDeviceNameType DDDNTModelName = "model-name"
toDeviceDefinitionDeviceNameType DDDNTOther = "other"
fromDeviceDefinitionDeviceNameType "udi-label-name" = DDDNTUdiLabelName
fromDeviceDefinitionDeviceNameType "user-friendly-name" = DDDNTUserFriendlyName
fromDeviceDefinitionDeviceNameType "patient-reported-name" = DDDNTPatientReportedName
fromDeviceDefinitionDeviceNameType "manufacturer-name" = DDDNTManufacturerName
fromDeviceDefinitionDeviceNameType "model-name" = DDDNTModelName
fromDeviceDefinitionDeviceNameType "other" = DDDNTOther


data DeviceDefinitionDeviceName = DeviceDefinitionDeviceName {
    deviceDefinitionDeviceNameAttrId :: Maybe Text
  , deviceDefinitionDeviceNameExtension :: [Extension]
  , deviceDefinitionDeviceNameModifierExtension :: [Extension]
  , deviceDefinitionDeviceNameName :: Text
  , deviceDefinitionDeviceNameType :: DeviceDefinitionDeviceNameType
  }
--

instance ToJSON DeviceDefinitionDeviceName where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (deviceDefinitionDeviceNameAttrId p)
    ,  "extension" .= toJSON (deviceDefinitionDeviceNameExtension p)
    ,  "modifierExtension" .= toJSON (deviceDefinitionDeviceNameModifierExtension p)
    ,  "name" .= toJSON (deviceDefinitionDeviceNameName p)
    ,  "type" .= toJSON (deviceDefinitionDeviceNameType p)
    ]
instance FromJSON DeviceDefinitionDeviceName where
  parseJSON = withObject "DeviceDefinitionDeviceName" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        name <- o .:  "name"
        ty <- o .:  "type"
        return DeviceDefinitionDeviceName{
            deviceDefinitionDeviceNameAttrId = id
          , deviceDefinitionDeviceNameExtension = extension
          , deviceDefinitionDeviceNameModifierExtension = modifierExtension
          , deviceDefinitionDeviceNameName = name
          , deviceDefinitionDeviceNameType = ty
          }
instance Xmlbf.ToXml DeviceDefinitionDeviceName where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (deviceDefinitionDeviceNameAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (deviceDefinitionDeviceNameExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceDefinitionDeviceNameModifierExtension p))
             , Val      "name" (     toString (deviceDefinitionDeviceNameName p))
             , Val      "type" (     toDeviceDefinitionDeviceNameType (deviceDefinitionDeviceNameType p))
             ]
instance Xmlbf.FromXml DeviceDefinitionDeviceName where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    name <-            Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    ty <-            Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    return DeviceDefinitionDeviceName {
            deviceDefinitionDeviceNameAttrId = id
          , deviceDefinitionDeviceNameExtension = extension
          , deviceDefinitionDeviceNameModifierExtension = modifierExtension
          , deviceDefinitionDeviceNameName =      fromString name
          , deviceDefinitionDeviceNameType =      fromDeviceDefinitionDeviceNameType ty
          }



data DeviceSpecialization = DeviceSpecialization {
    deviceSpecializationAttrId :: Maybe Text
  , deviceSpecializationExtension :: [Extension]
  , deviceSpecializationModifierExtension :: [Extension]
  , deviceSpecializationSystemType :: CodeableConcept
  , deviceSpecializationVersion :: Maybe Text
  }
--

instance ToJSON DeviceSpecialization where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (deviceSpecializationAttrId p)
    ,  "extension" .= toJSON (deviceSpecializationExtension p)
    ,  "modifierExtension" .= toJSON (deviceSpecializationModifierExtension p)
    ,  "systemType" .= toJSON (deviceSpecializationSystemType p)
    ,  "version" .= toJSON (deviceSpecializationVersion p)
    ]
instance FromJSON DeviceSpecialization where
  parseJSON = withObject "DeviceSpecialization" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        systemType <- o .:  "systemType"
        version <- o .:? "version"
        return DeviceSpecialization{
            deviceSpecializationAttrId = id
          , deviceSpecializationExtension = extension
          , deviceSpecializationModifierExtension = modifierExtension
          , deviceSpecializationSystemType = systemType
          , deviceSpecializationVersion = version
          }
instance Xmlbf.ToXml DeviceSpecialization where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (deviceSpecializationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (deviceSpecializationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceSpecializationModifierExtension p))
             , Prop     "systemType" (HM.empty, Xmlbf.toXml (deviceSpecializationSystemType p))
             , OptVal   "version" (fmap toString (deviceSpecializationVersion p))
             ]
instance Xmlbf.FromXml DeviceSpecialization where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    systemType <-            Xmlbf.pElement "systemType" Xmlbf.fromXml
    version <- optional $ Xmlbf.pElement "version" (Xmlbf.pAttr "value")
    return DeviceSpecialization {
            deviceSpecializationAttrId = id
          , deviceSpecializationExtension = extension
          , deviceSpecializationModifierExtension = modifierExtension
          , deviceSpecializationSystemType = systemType
          , deviceSpecializationVersion = fmap fromString version
          }



data DeviceDefinitionSpecialization = DeviceDefinitionSpecialization {
    deviceDefinitionSpecializationAttrId :: Maybe Text
  , deviceDefinitionSpecializationExtension :: [Extension]
  , deviceDefinitionSpecializationModifierExtension :: [Extension]
  , deviceDefinitionSpecializationSystemType :: Text
  , deviceDefinitionSpecializationVersion :: Maybe Text
  }
--

instance ToJSON DeviceDefinitionSpecialization where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (deviceDefinitionSpecializationAttrId p)
    ,  "extension" .= toJSON (deviceDefinitionSpecializationExtension p)
    ,  "modifierExtension" .= toJSON (deviceDefinitionSpecializationModifierExtension p)
    ,  "systemType" .= toJSON (deviceDefinitionSpecializationSystemType p)
    ,  "version" .= toJSON (deviceDefinitionSpecializationVersion p)
    ]
instance FromJSON DeviceDefinitionSpecialization where
  parseJSON = withObject "DeviceDefinitionSpecialization" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        systemType <- o .:  "systemType"
        version <- o .:? "version"
        return DeviceDefinitionSpecialization{
            deviceDefinitionSpecializationAttrId = id
          , deviceDefinitionSpecializationExtension = extension
          , deviceDefinitionSpecializationModifierExtension = modifierExtension
          , deviceDefinitionSpecializationSystemType = systemType
          , deviceDefinitionSpecializationVersion = version
          }
instance Xmlbf.ToXml DeviceDefinitionSpecialization where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (deviceDefinitionSpecializationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (deviceDefinitionSpecializationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceDefinitionSpecializationModifierExtension p))
             , Val      "systemType" (     toString (deviceDefinitionSpecializationSystemType p))
             , OptVal   "version" (fmap toString (deviceDefinitionSpecializationVersion p))
             ]
instance Xmlbf.FromXml DeviceDefinitionSpecialization where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    systemType <-            Xmlbf.pElement "systemType" (Xmlbf.pAttr "value")
    version <- optional $ Xmlbf.pElement "version" (Xmlbf.pAttr "value")
    return DeviceDefinitionSpecialization {
            deviceDefinitionSpecializationAttrId = id
          , deviceDefinitionSpecializationExtension = extension
          , deviceDefinitionSpecializationModifierExtension = modifierExtension
          , deviceDefinitionSpecializationSystemType =      fromString systemType
          , deviceDefinitionSpecializationVersion = fmap fromString version
          }



data DeviceUseStatementStatus
    = DUSSActive
    | DUSSCompleted
    | DUSSEnteredInError
    | DUSSIntended
    | DUSSStopped
    | DUSSOnHold
  deriving (Eq, Show)

instance ToJSON DeviceUseStatementStatus where
    toJSON DUSSActive = String "active"
    toJSON DUSSCompleted = String "completed"
    toJSON DUSSEnteredInError = String "entered-in-error"
    toJSON DUSSIntended = String "intended"
    toJSON DUSSStopped = String "stopped"
    toJSON DUSSOnHold = String "on-hold"
instance FromJSON DeviceUseStatementStatus where
    parseJSON "active" = return DUSSActive
    parseJSON "completed" = return DUSSCompleted
    parseJSON "entered-in-error" = return DUSSEnteredInError
    parseJSON "intended" = return DUSSIntended
    parseJSON "stopped" = return DUSSStopped
    parseJSON "on-hold" = return DUSSOnHold

toDeviceUseStatementStatus DUSSActive = "active"
toDeviceUseStatementStatus DUSSCompleted = "completed"
toDeviceUseStatementStatus DUSSEnteredInError = "entered-in-error"
toDeviceUseStatementStatus DUSSIntended = "intended"
toDeviceUseStatementStatus DUSSStopped = "stopped"
toDeviceUseStatementStatus DUSSOnHold = "on-hold"
fromDeviceUseStatementStatus "active" = DUSSActive
fromDeviceUseStatementStatus "completed" = DUSSCompleted
fromDeviceUseStatementStatus "entered-in-error" = DUSSEnteredInError
fromDeviceUseStatementStatus "intended" = DUSSIntended
fromDeviceUseStatementStatus "stopped" = DUSSStopped
fromDeviceUseStatementStatus "on-hold" = DUSSOnHold


data DeviceUseStatementTiming
    = DeviceUseStatementTimingTiming Timing
    | DeviceUseStatementTimingPeriod Period
    | DeviceUseStatementTimingDateTime DateTime
    deriving (Eq, Show)

data DeviceUseStatement = DeviceUseStatement {
    deviceUseStatementId :: Maybe Id
  , deviceUseStatementMeta :: Maybe Meta
  , deviceUseStatementImplicitRules :: Maybe Uri
  , deviceUseStatementLanguage :: Maybe Language
  , deviceUseStatementText :: Maybe Narrative
--    deviceUseStatementContained :: [ResourceContainer]
  , deviceUseStatementExtension :: [Extension]
  , deviceUseStatementModifierExtension :: [Extension]
  , deviceUseStatementIdentifier :: [Identifier]
  , deviceUseStatementBasedOn :: [Reference]
  , deviceUseStatementStatus :: DeviceUseStatementStatus
  , deviceUseStatementSubject :: Reference
  , deviceUseStatementDerivedFrom :: [Reference]
  , deviceUseStatementTimingTiming :: Maybe Timing
  , deviceUseStatementTimingPeriod :: Maybe Period
  , deviceUseStatementTimingDateTime :: Maybe DateTime
  , deviceUseStatementRecordedOn :: Maybe DateTime
  , deviceUseStatementSource :: Maybe Reference
  , deviceUseStatementDevice :: Reference
  , deviceUseStatementReasonCode :: [CodeableConcept]
  , deviceUseStatementReasonReference :: [Reference]
  , deviceUseStatementBodySite :: Maybe CodeableConcept
  , deviceUseStatementNote :: [Annotation]
  }
--

instance ToJSON DeviceUseStatement where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "DeviceUseStatement")
    ,  "id" .= toJSON (deviceUseStatementId p)
    ,  "meta" .= toJSON (deviceUseStatementMeta p)
    ,  "implicitRules" .= toJSON (deviceUseStatementImplicitRules p)
    ,  "language" .= toJSON (deviceUseStatementLanguage p)
    ,  "text" .= toJSON (deviceUseStatementText p)
--    , "contained" .= toJSON (deviceUseStatementContained p)
    ,  "extension" .= toJSON (deviceUseStatementExtension p)
    ,  "modifierExtension" .= toJSON (deviceUseStatementModifierExtension p)
    ,  "identifier" .= toJSON (deviceUseStatementIdentifier p)
    ,  "basedOn" .= toJSON (deviceUseStatementBasedOn p)
    ,  "status" .= toJSON (deviceUseStatementStatus p)
    ,  "subject" .= toJSON (deviceUseStatementSubject p)
    ,  "derivedFrom" .= toJSON (deviceUseStatementDerivedFrom p)
    ,  "timingTiming" .= toJSON (deviceUseStatementTimingTiming p)
    ,  "timingPeriod" .= toJSON (deviceUseStatementTimingPeriod p)
    ,  "timingDateTime" .= toJSON (deviceUseStatementTimingDateTime p)
    ,  "recordedOn" .= toJSON (deviceUseStatementRecordedOn p)
    ,  "source" .= toJSON (deviceUseStatementSource p)
    ,  "device" .= toJSON (deviceUseStatementDevice p)
    ,  "reasonCode" .= toJSON (deviceUseStatementReasonCode p)
    ,  "reasonReference" .= toJSON (deviceUseStatementReasonReference p)
    ,  "bodySite" .= toJSON (deviceUseStatementBodySite p)
    ,  "note" .= toJSON (deviceUseStatementNote p)
    ]
instance FromJSON DeviceUseStatement where
  parseJSON = withObject "DeviceUseStatement" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "DeviceUseStatement" -> do
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
        status <- o .:  "status"
        subject <- o .:  "subject"
        derivedFrom <- o .:? "derivedFrom" .!= []
        timingTiming <- o .:? "timingTiming"
        timingPeriod <- o .:? "timingPeriod"
        timingDateTime <- o .:? "timingDateTime"
        recordedOn <- o .:? "recordedOn"
        source <- o .:? "source"
        device <- o .:  "device"
        reasonCode <- o .:? "reasonCode" .!= []
        reasonReference <- o .:? "reasonReference" .!= []
        bodySite <- o .:? "bodySite"
        note <- o .:? "note" .!= []
        return DeviceUseStatement{
            deviceUseStatementId = id
          , deviceUseStatementMeta = meta
          , deviceUseStatementImplicitRules = implicitRules
          , deviceUseStatementLanguage = language
          , deviceUseStatementText = text
--          , deviceUseStatementContained = contained
          , deviceUseStatementExtension = extension
          , deviceUseStatementModifierExtension = modifierExtension
          , deviceUseStatementIdentifier = identifier
          , deviceUseStatementBasedOn = basedOn
          , deviceUseStatementStatus = status
          , deviceUseStatementSubject = subject
          , deviceUseStatementDerivedFrom = derivedFrom
          , deviceUseStatementTimingTiming = timingTiming
          , deviceUseStatementTimingPeriod = timingPeriod
          , deviceUseStatementTimingDateTime = timingDateTime
          , deviceUseStatementRecordedOn = recordedOn
          , deviceUseStatementSource = source
          , deviceUseStatementDevice = device
          , deviceUseStatementReasonCode = reasonCode
          , deviceUseStatementReasonReference = reasonReference
          , deviceUseStatementBodySite = bodySite
          , deviceUseStatementNote = note
          }
      _ -> fail "not a DeviceUseStatement"
instance Xmlbf.ToXml DeviceUseStatement where
  toXml p = Xmlbf.element "DeviceUseStatement" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (deviceUseStatementId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (deviceUseStatementMeta p))
             , OptVal   "implicitRules" (fmap toUri (deviceUseStatementImplicitRules p))
             , OptVal   "language" (fmap toLanguage (deviceUseStatementLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (deviceUseStatementText p))
--             , PropList "contained" (fmap Xmlbf.toXml (deviceUseStatementContained p))
             , PropList "extension" (fmap Xmlbf.toXml (deviceUseStatementExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceUseStatementModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (deviceUseStatementIdentifier p))
             , PropList "basedOn" (fmap Xmlbf.toXml (deviceUseStatementBasedOn p))
             , Val      "status" (     toDeviceUseStatementStatus (deviceUseStatementStatus p))
             , Prop     "subject" (HM.empty, Xmlbf.toXml (deviceUseStatementSubject p))
             , PropList "derivedFrom" (fmap Xmlbf.toXml (deviceUseStatementDerivedFrom p))
             , OptProp  "timingTiming" (fmap Xmlbf.toXml (deviceUseStatementTimingTiming p))
             , OptProp  "timingPeriod" (fmap Xmlbf.toXml (deviceUseStatementTimingPeriod p))
             , OptVal   "timingDateTime" (fmap toDateTime (deviceUseStatementTimingDateTime p))
             , OptVal   "recordedOn" (fmap toDateTime (deviceUseStatementRecordedOn p))
             , OptProp  "source" (fmap Xmlbf.toXml (deviceUseStatementSource p))
             , Prop     "device" (HM.empty, Xmlbf.toXml (deviceUseStatementDevice p))
             , PropList "reasonCode" (fmap Xmlbf.toXml (deviceUseStatementReasonCode p))
             , PropList "reasonReference" (fmap Xmlbf.toXml (deviceUseStatementReasonReference p))
             , OptProp  "bodySite" (fmap Xmlbf.toXml (deviceUseStatementBodySite p))
             , PropList "note" (fmap Xmlbf.toXml (deviceUseStatementNote p))
             ]
instance Xmlbf.FromXml DeviceUseStatement where
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
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    subject <-            Xmlbf.pElement "subject" Xmlbf.fromXml
    derivedFrom <- many     $ Xmlbf.pElement "derivedFrom" Xmlbf.fromXml
    timingTiming <- optional $ Xmlbf.pElement "timingTiming" Xmlbf.fromXml
    timingPeriod <- optional $ Xmlbf.pElement "timingPeriod" Xmlbf.fromXml
    timingDateTime <- optional $ Xmlbf.pElement "timingDateTime" (Xmlbf.pAttr "value")
    recordedOn <- optional $ Xmlbf.pElement "recordedOn" (Xmlbf.pAttr "value")
    source <- optional $ Xmlbf.pElement "source" Xmlbf.fromXml
    device <-            Xmlbf.pElement "device" Xmlbf.fromXml
    reasonCode <- many     $ Xmlbf.pElement "reasonCode" Xmlbf.fromXml
    reasonReference <- many     $ Xmlbf.pElement "reasonReference" Xmlbf.fromXml
    bodySite <- optional $ Xmlbf.pElement "bodySite" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    return DeviceUseStatement {
            deviceUseStatementId = fmap fromId id
          , deviceUseStatementMeta = meta
          , deviceUseStatementImplicitRules = fmap fromUri implicitRules
          , deviceUseStatementLanguage = fmap fromLanguage language
          , deviceUseStatementText = text
--          , deviceUseStatementContained = contained
          , deviceUseStatementExtension = extension
          , deviceUseStatementModifierExtension = modifierExtension
          , deviceUseStatementIdentifier = identifier
          , deviceUseStatementBasedOn = basedOn
          , deviceUseStatementStatus =      fromDeviceUseStatementStatus status
          , deviceUseStatementSubject = subject
          , deviceUseStatementDerivedFrom = derivedFrom
          , deviceUseStatementTimingTiming = timingTiming
          , deviceUseStatementTimingPeriod = timingPeriod
          , deviceUseStatementTimingDateTime = fmap fromDateTime timingDateTime
          , deviceUseStatementRecordedOn = fmap fromDateTime recordedOn
          , deviceUseStatementSource = source
          , deviceUseStatementDevice = device
          , deviceUseStatementReasonCode = reasonCode
          , deviceUseStatementReasonReference = reasonReference
          , deviceUseStatementBodySite = bodySite
          , deviceUseStatementNote = note
          }



data DeviceDefinitionProperty = DeviceDefinitionProperty {
    deviceDefinitionPropertyAttrId :: Maybe Text
  , deviceDefinitionPropertyExtension :: [Extension]
  , deviceDefinitionPropertyModifierExtension :: [Extension]
  , deviceDefinitionPropertyType :: CodeableConcept
  , deviceDefinitionPropertyValueQuantity :: [Quantity]
  , deviceDefinitionPropertyValueCode :: [CodeableConcept]
  }
--

instance ToJSON DeviceDefinitionProperty where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (deviceDefinitionPropertyAttrId p)
    ,  "extension" .= toJSON (deviceDefinitionPropertyExtension p)
    ,  "modifierExtension" .= toJSON (deviceDefinitionPropertyModifierExtension p)
    ,  "type" .= toJSON (deviceDefinitionPropertyType p)
    ,  "valueQuantity" .= toJSON (deviceDefinitionPropertyValueQuantity p)
    ,  "valueCode" .= toJSON (deviceDefinitionPropertyValueCode p)
    ]
instance FromJSON DeviceDefinitionProperty where
  parseJSON = withObject "DeviceDefinitionProperty" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:  "type"
        valueQuantity <- o .:? "valueQuantity" .!= []
        valueCode <- o .:? "valueCode" .!= []
        return DeviceDefinitionProperty{
            deviceDefinitionPropertyAttrId = id
          , deviceDefinitionPropertyExtension = extension
          , deviceDefinitionPropertyModifierExtension = modifierExtension
          , deviceDefinitionPropertyType = ty
          , deviceDefinitionPropertyValueQuantity = valueQuantity
          , deviceDefinitionPropertyValueCode = valueCode
          }
instance Xmlbf.ToXml DeviceDefinitionProperty where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (deviceDefinitionPropertyAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (deviceDefinitionPropertyExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceDefinitionPropertyModifierExtension p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (deviceDefinitionPropertyType p))
             , PropList "valueQuantity" (fmap Xmlbf.toXml (deviceDefinitionPropertyValueQuantity p))
             , PropList "valueCode" (fmap Xmlbf.toXml (deviceDefinitionPropertyValueCode p))
             ]
instance Xmlbf.FromXml DeviceDefinitionProperty where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" Xmlbf.fromXml
    valueQuantity <- many     $ Xmlbf.pElement "valueQuantity" Xmlbf.fromXml
    valueCode <- many     $ Xmlbf.pElement "valueCode" Xmlbf.fromXml
    return DeviceDefinitionProperty {
            deviceDefinitionPropertyAttrId = id
          , deviceDefinitionPropertyExtension = extension
          , deviceDefinitionPropertyModifierExtension = modifierExtension
          , deviceDefinitionPropertyType = ty
          , deviceDefinitionPropertyValueQuantity = valueQuantity
          , deviceDefinitionPropertyValueCode = valueCode
          }



data DeviceDefinitionManufacturer
    = DeviceDefinitionManufacturerString Text
    | DeviceDefinitionManufacturerReference Reference
    deriving (Eq, Show)

data DeviceDefinition = DeviceDefinition {
    deviceDefinitionId :: Maybe Id
  , deviceDefinitionMeta :: Maybe Meta
  , deviceDefinitionImplicitRules :: Maybe Uri
  , deviceDefinitionLanguage :: Maybe Language
  , deviceDefinitionText :: Maybe Narrative
--    deviceDefinitionContained :: [ResourceContainer]
  , deviceDefinitionExtension :: [Extension]
  , deviceDefinitionModifierExtension :: [Extension]
  , deviceDefinitionIdentifier :: [Identifier]
  , deviceDefinitionUdiDeviceIdentifier :: [DeviceDefinitionUdiDeviceIdentifier]
  , deviceDefinitionManufacturerString :: Maybe Text
  , deviceDefinitionManufacturerReference :: Maybe Reference
  , deviceDefinitionDeviceName :: [DeviceDefinitionDeviceName]
  , deviceDefinitionModelNumber :: Maybe Text
  , deviceDefinitionType :: Maybe CodeableConcept
  , deviceDefinitionSpecialization :: [DeviceDefinitionSpecialization]
  , deviceDefinitionVersion :: [Text]
  , deviceDefinitionSafety :: [CodeableConcept]
  , deviceDefinitionShelfLifeStorage :: [ProductShelfLife]
  , deviceDefinitionPhysicalCharacteristics :: Maybe ProdCharacteristic
  , deviceDefinitionLanguageCode :: [CodeableConcept]
  , deviceDefinitionCapability :: [DeviceDefinitionCapability]
  , deviceDefinitionProperty :: [DeviceDefinitionProperty]
  , deviceDefinitionOwner :: Maybe Reference
  , deviceDefinitionContact :: [ContactPoint]
  , deviceDefinitionUrl :: Maybe Uri
  , deviceDefinitionOnlineInformation :: Maybe Uri
  , deviceDefinitionNote :: [Annotation]
  , deviceDefinitionQuantity :: Maybe Quantity
  , deviceDefinitionParentDevice :: Maybe Reference
  , deviceDefinitionMaterial :: [DeviceDefinitionMaterial]
  }
--

instance ToJSON DeviceDefinition where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "DeviceDefinition")
    ,  "id" .= toJSON (deviceDefinitionId p)
    ,  "meta" .= toJSON (deviceDefinitionMeta p)
    ,  "implicitRules" .= toJSON (deviceDefinitionImplicitRules p)
    ,  "language" .= toJSON (deviceDefinitionLanguage p)
    ,  "text" .= toJSON (deviceDefinitionText p)
--    , "contained" .= toJSON (deviceDefinitionContained p)
    ,  "extension" .= toJSON (deviceDefinitionExtension p)
    ,  "modifierExtension" .= toJSON (deviceDefinitionModifierExtension p)
    ,  "identifier" .= toJSON (deviceDefinitionIdentifier p)
    ,  "udiDeviceIdentifier" .= toJSON (deviceDefinitionUdiDeviceIdentifier p)
    ,  "manufacturerString" .= toJSON (deviceDefinitionManufacturerString p)
    ,  "manufacturerReference" .= toJSON (deviceDefinitionManufacturerReference p)
    ,  "deviceName" .= toJSON (deviceDefinitionDeviceName p)
    ,  "modelNumber" .= toJSON (deviceDefinitionModelNumber p)
    ,  "type" .= toJSON (deviceDefinitionType p)
    ,  "specialization" .= toJSON (deviceDefinitionSpecialization p)
    ,  "version" .= toJSON (deviceDefinitionVersion p)
    ,  "safety" .= toJSON (deviceDefinitionSafety p)
    ,  "shelfLifeStorage" .= toJSON (deviceDefinitionShelfLifeStorage p)
    ,  "physicalCharacteristics" .= toJSON (deviceDefinitionPhysicalCharacteristics p)
    ,  "languageCode" .= toJSON (deviceDefinitionLanguageCode p)
    ,  "capability" .= toJSON (deviceDefinitionCapability p)
    ,  "property" .= toJSON (deviceDefinitionProperty p)
    ,  "owner" .= toJSON (deviceDefinitionOwner p)
    ,  "contact" .= toJSON (deviceDefinitionContact p)
    ,  "url" .= toJSON (deviceDefinitionUrl p)
    ,  "onlineInformation" .= toJSON (deviceDefinitionOnlineInformation p)
    ,  "note" .= toJSON (deviceDefinitionNote p)
    ,  "quantity" .= toJSON (deviceDefinitionQuantity p)
    ,  "parentDevice" .= toJSON (deviceDefinitionParentDevice p)
    ,  "material" .= toJSON (deviceDefinitionMaterial p)
    ]
instance FromJSON DeviceDefinition where
  parseJSON = withObject "DeviceDefinition" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "DeviceDefinition" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        udiDeviceIdentifier <- o .:? "udiDeviceIdentifier" .!= []
        manufacturerString <- o .:? "manufacturerString"
        manufacturerReference <- o .:? "manufacturerReference"
        deviceName <- o .:? "deviceName" .!= []
        modelNumber <- o .:? "modelNumber"
        ty <- o .:? "type"
        specialization <- o .:? "specialization" .!= []
        version <- o .:? "version" .!= []
        safety <- o .:? "safety" .!= []
        shelfLifeStorage <- o .:? "shelfLifeStorage" .!= []
        physicalCharacteristics <- o .:? "physicalCharacteristics"
        languageCode <- o .:? "languageCode" .!= []
        capability <- o .:? "capability" .!= []
        property <- o .:? "property" .!= []
        owner <- o .:? "owner"
        contact <- o .:? "contact" .!= []
        url <- o .:? "url"
        onlineInformation <- o .:? "onlineInformation"
        note <- o .:? "note" .!= []
        quantity <- o .:? "quantity"
        parentDevice <- o .:? "parentDevice"
        material <- o .:? "material" .!= []
        return DeviceDefinition{
            deviceDefinitionId = id
          , deviceDefinitionMeta = meta
          , deviceDefinitionImplicitRules = implicitRules
          , deviceDefinitionLanguage = language
          , deviceDefinitionText = text
--          , deviceDefinitionContained = contained
          , deviceDefinitionExtension = extension
          , deviceDefinitionModifierExtension = modifierExtension
          , deviceDefinitionIdentifier = identifier
          , deviceDefinitionUdiDeviceIdentifier = udiDeviceIdentifier
          , deviceDefinitionManufacturerString = manufacturerString
          , deviceDefinitionManufacturerReference = manufacturerReference
          , deviceDefinitionDeviceName = deviceName
          , deviceDefinitionModelNumber = modelNumber
          , deviceDefinitionType = ty
          , deviceDefinitionSpecialization = specialization
          , deviceDefinitionVersion = version
          , deviceDefinitionSafety = safety
          , deviceDefinitionShelfLifeStorage = shelfLifeStorage
          , deviceDefinitionPhysicalCharacteristics = physicalCharacteristics
          , deviceDefinitionLanguageCode = languageCode
          , deviceDefinitionCapability = capability
          , deviceDefinitionProperty = property
          , deviceDefinitionOwner = owner
          , deviceDefinitionContact = contact
          , deviceDefinitionUrl = url
          , deviceDefinitionOnlineInformation = onlineInformation
          , deviceDefinitionNote = note
          , deviceDefinitionQuantity = quantity
          , deviceDefinitionParentDevice = parentDevice
          , deviceDefinitionMaterial = material
          }
      _ -> fail "not a DeviceDefinition"
instance Xmlbf.ToXml DeviceDefinition where
  toXml p = Xmlbf.element "DeviceDefinition" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (deviceDefinitionId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (deviceDefinitionMeta p))
             , OptVal   "implicitRules" (fmap toUri (deviceDefinitionImplicitRules p))
             , OptVal   "language" (fmap toLanguage (deviceDefinitionLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (deviceDefinitionText p))
--             , PropList "contained" (fmap Xmlbf.toXml (deviceDefinitionContained p))
             , PropList "extension" (fmap Xmlbf.toXml (deviceDefinitionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (deviceDefinitionModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (deviceDefinitionIdentifier p))
             , PropList "udiDeviceIdentifier" (fmap Xmlbf.toXml (deviceDefinitionUdiDeviceIdentifier p))
             , OptVal   "manufacturerString" (fmap toString (deviceDefinitionManufacturerString p))
             , OptProp  "manufacturerReference" (fmap Xmlbf.toXml (deviceDefinitionManufacturerReference p))
             , PropList "deviceName" (fmap Xmlbf.toXml (deviceDefinitionDeviceName p))
             , OptVal   "modelNumber" (fmap toString (deviceDefinitionModelNumber p))
             , OptProp  "type" (fmap Xmlbf.toXml (deviceDefinitionType p))
             , PropList "specialization" (fmap Xmlbf.toXml (deviceDefinitionSpecialization p))
             , ValList  "version" (fmap toString (deviceDefinitionVersion p))
             , PropList "safety" (fmap Xmlbf.toXml (deviceDefinitionSafety p))
             , PropList "shelfLifeStorage" (fmap Xmlbf.toXml (deviceDefinitionShelfLifeStorage p))
             , OptProp  "physicalCharacteristics" (fmap Xmlbf.toXml (deviceDefinitionPhysicalCharacteristics p))
             , PropList "languageCode" (fmap Xmlbf.toXml (deviceDefinitionLanguageCode p))
             , PropList "capability" (fmap Xmlbf.toXml (deviceDefinitionCapability p))
             , PropList "property" (fmap Xmlbf.toXml (deviceDefinitionProperty p))
             , OptProp  "owner" (fmap Xmlbf.toXml (deviceDefinitionOwner p))
             , PropList "contact" (fmap Xmlbf.toXml (deviceDefinitionContact p))
             , OptVal   "url" (fmap toUri (deviceDefinitionUrl p))
             , OptVal   "onlineInformation" (fmap toUri (deviceDefinitionOnlineInformation p))
             , PropList "note" (fmap Xmlbf.toXml (deviceDefinitionNote p))
             , OptProp  "quantity" (fmap Xmlbf.toXml (deviceDefinitionQuantity p))
             , OptProp  "parentDevice" (fmap Xmlbf.toXml (deviceDefinitionParentDevice p))
             , PropList "material" (fmap Xmlbf.toXml (deviceDefinitionMaterial p))
             ]
instance Xmlbf.FromXml DeviceDefinition where
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
    udiDeviceIdentifier <- many     $ Xmlbf.pElement "udiDeviceIdentifier" Xmlbf.fromXml
    manufacturerString <- optional $ Xmlbf.pElement "manufacturerString" (Xmlbf.pAttr "value")
    manufacturerReference <- optional $ Xmlbf.pElement "manufacturerReference" Xmlbf.fromXml
    deviceName <- many     $ Xmlbf.pElement "deviceName" Xmlbf.fromXml
    modelNumber <- optional $ Xmlbf.pElement "modelNumber" (Xmlbf.pAttr "value")
    ty <- optional $ Xmlbf.pElement "type" Xmlbf.fromXml
    specialization <- many     $ Xmlbf.pElement "specialization" Xmlbf.fromXml
    version <- many     $ Xmlbf.pElement "version" (Xmlbf.pAttr "value")
    safety <- many     $ Xmlbf.pElement "safety" Xmlbf.fromXml
    shelfLifeStorage <- many     $ Xmlbf.pElement "shelfLifeStorage" Xmlbf.fromXml
    physicalCharacteristics <- optional $ Xmlbf.pElement "physicalCharacteristics" Xmlbf.fromXml
    languageCode <- many     $ Xmlbf.pElement "languageCode" Xmlbf.fromXml
    capability <- many     $ Xmlbf.pElement "capability" Xmlbf.fromXml
    property <- many     $ Xmlbf.pElement "property" Xmlbf.fromXml
    owner <- optional $ Xmlbf.pElement "owner" Xmlbf.fromXml
    contact <- many     $ Xmlbf.pElement "contact" Xmlbf.fromXml
    url <- optional $ Xmlbf.pElement "url" (Xmlbf.pAttr "value")
    onlineInformation <- optional $ Xmlbf.pElement "onlineInformation" (Xmlbf.pAttr "value")
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    quantity <- optional $ Xmlbf.pElement "quantity" Xmlbf.fromXml
    parentDevice <- optional $ Xmlbf.pElement "parentDevice" Xmlbf.fromXml
    material <- many     $ Xmlbf.pElement "material" Xmlbf.fromXml
    return DeviceDefinition {
            deviceDefinitionId = fmap fromId id
          , deviceDefinitionMeta = meta
          , deviceDefinitionImplicitRules = fmap fromUri implicitRules
          , deviceDefinitionLanguage = fmap fromLanguage language
          , deviceDefinitionText = text
--          , deviceDefinitionContained = contained
          , deviceDefinitionExtension = extension
          , deviceDefinitionModifierExtension = modifierExtension
          , deviceDefinitionIdentifier = identifier
          , deviceDefinitionUdiDeviceIdentifier = udiDeviceIdentifier
          , deviceDefinitionManufacturerString = fmap fromString manufacturerString
          , deviceDefinitionManufacturerReference = manufacturerReference
          , deviceDefinitionDeviceName = deviceName
          , deviceDefinitionModelNumber = fmap fromString modelNumber
          , deviceDefinitionType = ty
          , deviceDefinitionSpecialization = specialization
          , deviceDefinitionVersion = fmap fromString version
          , deviceDefinitionSafety = safety
          , deviceDefinitionShelfLifeStorage = shelfLifeStorage
          , deviceDefinitionPhysicalCharacteristics = physicalCharacteristics
          , deviceDefinitionLanguageCode = languageCode
          , deviceDefinitionCapability = capability
          , deviceDefinitionProperty = property
          , deviceDefinitionOwner = owner
          , deviceDefinitionContact = contact
          , deviceDefinitionUrl = fmap fromUri url
          , deviceDefinitionOnlineInformation = fmap fromUri onlineInformation
          , deviceDefinitionNote = note
          , deviceDefinitionQuantity = quantity
          , deviceDefinitionParentDevice = parentDevice
          , deviceDefinitionMaterial = material
          }




