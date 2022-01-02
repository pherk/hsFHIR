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
-- FHIR 4.0.0 Patient
--

module Data.FHIR.Resources.Patient where

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

data PatientDeceased
    = PatientDeceasedBoolean Boolean
    | PatientDeceasedDateTime DateTime
    deriving (Eq, Show)

data PatientMultipleBirth
    = PatientMultipleBirthBoolean Boolean
    | PatientMultipleBirthInteger Integer
    deriving (Eq, Show)

data Patient = Patient {
    patientId :: Maybe Id
  , patientMeta :: Maybe Meta
  , patientImplicitRules :: Maybe Uri
  , patientLanguage :: Maybe Language
  , patientText :: Maybe Narrative
--    patientContained :: [ResourceContainer]
  , patientExtension :: [Extension]
  , patientModifierExtension :: [Extension]
  , patientIdentifier :: [Identifier]
  , patientActive :: Maybe Boolean
  , patientName :: [HumanName]
  , patientTelecom :: [ContactPoint]
  , patientGender :: Maybe AdministrativeGender
  , patientBirthDate :: Maybe Date
  , patientDeceased :: Maybe PatientDeceased
  , patientAddress :: [Address]
  , patientMaritalStatus :: Maybe CodeableConcept
  , patientMultipleBirth :: Maybe PatientMultipleBirth
  , patientPhoto :: [Attachment]
  , patientContact :: [PatientContact]
  , patientCommunication :: [PatientCommunication]
  , patientGeneralPractitioner :: [Reference]
  , patientManagingOrganization :: Maybe Reference
  , patientLink :: [PatientLink]
  }
  deriving (Eq, Show)
--

instance ToJSON Patient where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Patient")
    ,  "id" .= toJSON (patientId p)
    ,  "meta" .= toJSON (patientMeta p)
    ,  "implicitRules" .= toJSON (patientImplicitRules p)
    ,  "language" .= toJSON (patientLanguage p)
    ,  "text" .= toJSON (patientText p)
--    , "contained" .= toJSON (patientContained p)
    ,  "extension" .= toJSON (patientExtension p)
    ,  "modifierExtension" .= toJSON (patientModifierExtension p)
    ,  "identifier" .= toJSON (patientIdentifier p)
    ,  "active" .= toJSON (patientActive p)
    ,  "name" .= toJSON (patientName p)
    ,  "telecom" .= toJSON (patientTelecom p)
    ,  "gender" .= toJSON (patientGender p)
    ,  "birthDate" .= toJSON (patientBirthDate p)
    , toDeceasedJSON (patientDeceased p)
    ,  "address" .= toJSON (patientAddress p)
    ,  "maritalStatus" .= toJSON (patientMaritalStatus p)
    , toMultipleBirthJSON (patientMultipleBirth p)
    ,  "photo" .= toJSON (patientPhoto p)
    ,  "contact" .= toJSON (patientContact p)
    ,  "communication" .= toJSON (patientCommunication p)
    ,  "generalPractitioner" .= toJSON (patientGeneralPractitioner p)
    ,  "managingOrganization" .= toJSON (patientManagingOrganization p)
    ,  "link" .= toJSON (patientLink p)
    ]
    where 
      toDeceasedJSON (     Nothing   ) = ("deceased", Null)
      toDeceasedJSON (Just (PatientDeceasedBoolean c)) = ("deceased", toJSON c)
      toDeceasedJSON (Just (PatientDeceasedDateTime c)) = ("deceased", toJSON c)
      toMultipleBirthJSON (     Nothing   ) = ("multipleBirth", Null)
      toMultipleBirthJSON (Just (PatientMultipleBirthBoolean c)) = ("multipleBirth", toJSON c)
      toMultipleBirthJSON (Just (PatientMultipleBirthInteger c)) = ("multipleBirth", toJSON c)
instance FromJSON Patient where
  parseJSON = withObject "Patient" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Patient" -> do
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
        name <- o .:? "name" .!= []
        telecom <- o .:? "telecom" .!= []
        gender <- o .:? "gender"
        birthDate <- o .:? "birthDate"
        deceased <- parseDeceased o
        address <- o .:? "address" .!= []
        maritalStatus <- o .:? "maritalStatus"
        multipleBirth <- parseMultipleBirth o
        photo <- o .:? "photo" .!= []
        contact <- o .:? "contact" .!= []
        communication <- o .:? "communication" .!= []
        generalPractitioner <- o .:? "generalPractitioner" .!= []
        managingOrganization <- o .:? "managingOrganization"
        link <- o .:? "link" .!= []
        return Patient{
            patientId = id
          , patientMeta = meta
          , patientImplicitRules = implicitRules
          , patientLanguage = language
          , patientText = text
--          , patientContained = contained
          , patientExtension = extension
          , patientModifierExtension = modifierExtension
          , patientIdentifier = identifier
          , patientActive = active
          , patientName = name
          , patientTelecom = telecom
          , patientGender = gender
          , patientBirthDate = birthDate
          , patientDeceased = deceased
          , patientAddress = address
          , patientMaritalStatus = maritalStatus
          , patientMultipleBirth = multipleBirth
          , patientPhoto = photo
          , patientContact = contact
          , patientCommunication = communication
          , patientGeneralPractitioner = generalPractitioner
          , patientManagingOrganization = managingOrganization
          , patientLink = link
          }
      _ -> fail "not a Patient"
    where 
      parseDeceased o = parseDeceasedBoolean o <|> parseDeceasedDateTime o <|> pure Nothing
      parseDeceasedBoolean o = do
                has <- o .: "deceasedBoolean"
                return $ Just (PatientDeceasedBoolean has)
      parseDeceasedDateTime o = do
                has <- o .: "deceasedDateTime"
                return $ Just (PatientDeceasedDateTime has)
      parseMultipleBirth o = parseMultipleBirthBoolean o <|> parseMultipleBirthInteger o <|> pure Nothing
      parseMultipleBirthBoolean o = do
                has <- o .: "multipleBirthBoolean"
                return $ Just (PatientMultipleBirthBoolean has)
      parseMultipleBirthInteger o = do
                has <- o .: "multipleBirthInteger"
                return $ Just (PatientMultipleBirthInteger has)
instance Xmlbf.ToXml Patient where
  toXml p = Xmlbf.element "Patient" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (patientId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (patientMeta p))
             , OptVal   "implicitRules" (fmap toUri (patientImplicitRules p))
             , OptVal   "language" (fmap toLanguage (patientLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (patientText p))
--             , PropList "contained" (fmap Xmlbf.toXml (patientContained p))
             , PropList "extension" (fmap Xmlbf.toXml (patientExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (patientModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (patientIdentifier p))
             , OptVal   "active" (fmap toBoolean (patientActive p))
             , PropList "name" (fmap Xmlbf.toXml (patientName p))
             , PropList "telecom" (fmap Xmlbf.toXml (patientTelecom p))
             , OptVal   "gender" (fmap toAdministrativeGender (patientGender p))
             , OptVal   "birthDate" (fmap toDate (patientBirthDate p))
             , toDeceasedXml (patientDeceased p)
             , PropList "address" (fmap Xmlbf.toXml (patientAddress p))
             , OptProp  "maritalStatus" (fmap Xmlbf.toXml (patientMaritalStatus p))
             , toMultipleBirthXml (patientMultipleBirth p)
             , PropList "photo" (fmap Xmlbf.toXml (patientPhoto p))
             , PropList "contact" (fmap Xmlbf.toXml (patientContact p))
             , PropList "communication" (fmap Xmlbf.toXml (patientCommunication p))
             , PropList "generalPractitioner" (fmap Xmlbf.toXml (patientGeneralPractitioner p))
             , OptProp  "managingOrganization" (fmap Xmlbf.toXml (patientManagingOrganization p))
             , PropList "link" (fmap Xmlbf.toXml (patientLink p))
             ]
          toDeceasedXml ( Nothing   ) = (OptVal "deceased" Nothing)
          toDeceasedXml (Just (PatientDeceasedBoolean p)) = Val   "deceasedBoolean" (toBoolean p)
          toDeceasedXml (Just (PatientDeceasedDateTime p)) = Val   "deceasedDateTime" (toDateTime p)
          toMultipleBirthXml ( Nothing   ) = (OptVal "multipleBirth" Nothing)
          toMultipleBirthXml (Just (PatientMultipleBirthBoolean p)) = Val   "multipleBirthBoolean" (toBoolean p)
          toMultipleBirthXml (Just (PatientMultipleBirthInteger p)) = Val   "multipleBirthInteger" (toInt p)
instance Xmlbf.FromXml Patient where
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
    name <- many     $ Xmlbf.pElement "name" Xmlbf.fromXml
    telecom <- many     $ Xmlbf.pElement "telecom" Xmlbf.fromXml
    gender <- optional $ Xmlbf.pElement "gender" (Xmlbf.pAttr "value")
    birthDate <- optional $ Xmlbf.pElement "birthDate" (Xmlbf.pAttr "value")
    deceased <- fromDeceasedXml
    address <- many     $ Xmlbf.pElement "address" Xmlbf.fromXml
    maritalStatus <- optional $ Xmlbf.pElement "maritalStatus" Xmlbf.fromXml
    multipleBirth <- fromMultipleBirthXml
    photo <- many     $ Xmlbf.pElement "photo" Xmlbf.fromXml
    contact <- many     $ Xmlbf.pElement "contact" Xmlbf.fromXml
    communication <- many     $ Xmlbf.pElement "communication" Xmlbf.fromXml
    generalPractitioner <- many     $ Xmlbf.pElement "generalPractitioner" Xmlbf.fromXml
    managingOrganization <- optional $ Xmlbf.pElement "managingOrganization" Xmlbf.fromXml
    link <- many     $ Xmlbf.pElement "link" Xmlbf.fromXml
    return Patient {
            patientId = fmap fromId id
          , patientMeta = meta
          , patientImplicitRules = fmap fromUri implicitRules
          , patientLanguage = fmap fromLanguage language
          , patientText = text
--          , patientContained = contained
          , patientExtension = extension
          , patientModifierExtension = modifierExtension
          , patientIdentifier = identifier
          , patientActive = fmap fromBoolean active
          , patientName = name
          , patientTelecom = telecom
          , patientGender = fmap fromAdministrativeGender gender
          , patientBirthDate = fmap fromDate birthDate
          , patientDeceased = deceased
          , patientAddress = address
          , patientMaritalStatus = maritalStatus
          , patientMultipleBirth = multipleBirth
          , patientPhoto = photo
          , patientContact = contact
          , patientCommunication = communication
          , patientGeneralPractitioner = generalPractitioner
          , patientManagingOrganization = managingOrganization
          , patientLink = link
          }

    where 
      fromDeceasedXml = parseDeceasedBoolean <|> parseDeceasedDateTime <|> pure Nothing
      parseDeceasedBoolean = do
                has <- Xmlbf.pElement "deceasedBoolean" (Xmlbf.pAttr "value")
                return $ Just (PatientDeceasedBoolean (     fromBoolean has))
      parseDeceasedDateTime = do
                has <- Xmlbf.pElement "deceasedDateTime" (Xmlbf.pAttr "value")
                return $ Just (PatientDeceasedDateTime (     fromDateTime has))
      fromMultipleBirthXml = parseMultipleBirthBoolean <|> parseMultipleBirthInteger <|> pure Nothing
      parseMultipleBirthBoolean = do
                has <- Xmlbf.pElement "multipleBirthBoolean" (Xmlbf.pAttr "value")
                return $ Just (PatientMultipleBirthBoolean (     fromBoolean has))
      parseMultipleBirthInteger = do
                has <- Xmlbf.pElement "multipleBirthInteger" (Xmlbf.pAttr "value")
                return $ Just (PatientMultipleBirthInteger (     fromInt has))


data PatientLink = PatientLink {
    patientLinkAttrId :: Maybe Text
  , patientLinkExtension :: [Extension]
  , patientLinkModifierExtension :: [Extension]
  , patientLinkOther :: Reference
  , patientLinkType :: LinkType
  }
  deriving (Eq, Show)
--

instance ToJSON PatientLink where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (patientLinkAttrId p)
    ,  "extension" .= toJSON (patientLinkExtension p)
    ,  "modifierExtension" .= toJSON (patientLinkModifierExtension p)
    ,  "other" .= toJSON (patientLinkOther p)
    ,  "type" .= toJSON (patientLinkType p)
    ]
instance FromJSON PatientLink where
  parseJSON = withObject "PatientLink" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        other <- o .:  "other"
        ty <- o .:  "type"
        return PatientLink{
            patientLinkAttrId = id
          , patientLinkExtension = extension
          , patientLinkModifierExtension = modifierExtension
          , patientLinkOther = other
          , patientLinkType = ty
          }
instance Xmlbf.ToXml PatientLink where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (patientLinkAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (patientLinkExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (patientLinkModifierExtension p))
             , Prop     "other" (HM.empty, Xmlbf.toXml (patientLinkOther p))
             , Val      "type" (     toLinkType (patientLinkType p))
             ]
instance Xmlbf.FromXml PatientLink where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    other <-            Xmlbf.pElement "other" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    return PatientLink {
            patientLinkAttrId = id
          , patientLinkExtension = extension
          , patientLinkModifierExtension = modifierExtension
          , patientLinkOther = other
          , patientLinkType =      fromLinkType ty
          }



data PatientCommunication = PatientCommunication {
    patientCommunicationAttrId :: Maybe Text
  , patientCommunicationExtension :: [Extension]
  , patientCommunicationModifierExtension :: [Extension]
  , patientCommunicationLanguage :: CodeableConcept
  , patientCommunicationPreferred :: Maybe Boolean
  } deriving (Eq, Show)
--

instance ToJSON PatientCommunication where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (patientCommunicationAttrId p)
    ,  "extension" .= toJSON (patientCommunicationExtension p)
    ,  "modifierExtension" .= toJSON (patientCommunicationModifierExtension p)
    ,  "language" .= toJSON (patientCommunicationLanguage p)
    ,  "preferred" .= toJSON (patientCommunicationPreferred p)
    ]
instance FromJSON PatientCommunication where
  parseJSON = withObject "PatientCommunication" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        language <- o .:  "language"
        preferred <- o .:? "preferred"
        return PatientCommunication{
            patientCommunicationAttrId = id
          , patientCommunicationExtension = extension
          , patientCommunicationModifierExtension = modifierExtension
          , patientCommunicationLanguage = language
          , patientCommunicationPreferred = preferred
          }
instance Xmlbf.ToXml PatientCommunication where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (patientCommunicationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (patientCommunicationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (patientCommunicationModifierExtension p))
             , Prop     "language" (HM.empty, Xmlbf.toXml (patientCommunicationLanguage p))
             , OptVal   "preferred" (fmap toBoolean (patientCommunicationPreferred p))
             ]
instance Xmlbf.FromXml PatientCommunication where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    language <-            Xmlbf.pElement "language" Xmlbf.fromXml
    preferred <- optional $ Xmlbf.pElement "preferred" (Xmlbf.pAttr "value")
    return PatientCommunication {
            patientCommunicationAttrId = id
          , patientCommunicationExtension = extension
          , patientCommunicationModifierExtension = modifierExtension
          , patientCommunicationLanguage = language
          , patientCommunicationPreferred = fmap fromBoolean preferred
          }



data PatientContact = PatientContact {
    patientContactAttrId :: Maybe Text
  , patientContactExtension :: [Extension]
  , patientContactModifierExtension :: [Extension]
  , patientContactRelationship :: [CodeableConcept]
  , patientContactName :: Maybe HumanName
  , patientContactTelecom :: [ContactPoint]
  , patientContactAddress :: Maybe Address
  , patientContactGender :: Maybe AdministrativeGender
  , patientContactOrganization :: Maybe Reference
  , patientContactPeriod :: Maybe Period
  }
  deriving (Eq, Show)
--

instance ToJSON PatientContact where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (patientContactAttrId p)
    ,  "extension" .= toJSON (patientContactExtension p)
    ,  "modifierExtension" .= toJSON (patientContactModifierExtension p)
    ,  "relationship" .= toJSON (patientContactRelationship p)
    ,  "name" .= toJSON (patientContactName p)
    ,  "telecom" .= toJSON (patientContactTelecom p)
    ,  "address" .= toJSON (patientContactAddress p)
    ,  "gender" .= toJSON (patientContactGender p)
    ,  "organization" .= toJSON (patientContactOrganization p)
    ,  "period" .= toJSON (patientContactPeriod p)
    ]
instance FromJSON PatientContact where
  parseJSON = withObject "PatientContact" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        relationship <- o .:? "relationship" .!= []
        name <- o .:? "name"
        telecom <- o .:? "telecom" .!= []
        address <- o .:? "address"
        gender <- o .:? "gender"
        organization <- o .:? "organization"
        period <- o .:? "period"
        return PatientContact{
            patientContactAttrId = id
          , patientContactExtension = extension
          , patientContactModifierExtension = modifierExtension
          , patientContactRelationship = relationship
          , patientContactName = name
          , patientContactTelecom = telecom
          , patientContactAddress = address
          , patientContactGender = gender
          , patientContactOrganization = organization
          , patientContactPeriod = period
          }
instance Xmlbf.ToXml PatientContact where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (patientContactAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (patientContactExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (patientContactModifierExtension p))
             , PropList "relationship" (fmap Xmlbf.toXml (patientContactRelationship p))
             , OptProp  "name" (fmap Xmlbf.toXml (patientContactName p))
             , PropList "telecom" (fmap Xmlbf.toXml (patientContactTelecom p))
             , OptProp  "address" (fmap Xmlbf.toXml (patientContactAddress p))
             , OptVal   "gender" (fmap toAdministrativeGender (patientContactGender p))
             , OptProp  "organization" (fmap Xmlbf.toXml (patientContactOrganization p))
             , OptProp  "period" (fmap Xmlbf.toXml (patientContactPeriod p))
             ]
instance Xmlbf.FromXml PatientContact where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    relationship <- many     $ Xmlbf.pElement "relationship" Xmlbf.fromXml
    name <- optional $ Xmlbf.pElement "name" Xmlbf.fromXml
    telecom <- many     $ Xmlbf.pElement "telecom" Xmlbf.fromXml
    address <- optional $ Xmlbf.pElement "address" Xmlbf.fromXml
    gender <- optional $ Xmlbf.pElement "gender" (Xmlbf.pAttr "value")
    organization <- optional $ Xmlbf.pElement "organization" Xmlbf.fromXml
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    return PatientContact {
            patientContactAttrId = id
          , patientContactExtension = extension
          , patientContactModifierExtension = modifierExtension
          , patientContactRelationship = relationship
          , patientContactName = name
          , patientContactTelecom = telecom
          , patientContactAddress = address
          , patientContactGender = fmap fromAdministrativeGender gender
          , patientContactOrganization = organization
          , patientContactPeriod = period
          }




