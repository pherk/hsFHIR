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
-- FHIR 4.0.0 Organization
--

module Data.FHIR.Resources.Organization where

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

data Organization = Organization {
    organizationId :: Maybe Id
  , organizationMeta :: Maybe Meta
  , organizationImplicitRules :: Maybe Uri
  , organizationLanguage :: Maybe Language
  , organizationText :: Maybe Narrative
--    organizationContained :: [ResourceContainer]
  , organizationExtension :: [Extension]
  , organizationModifierExtension :: [Extension]
  , organizationIdentifier :: [Identifier]
  , organizationActive :: Maybe Boolean
  , organizationType :: [CodeableConcept]
  , organizationName :: Maybe Text
  , organizationAlias :: [Text]
  , organizationTelecom :: [ContactPoint]
  , organizationAddress :: [Address]
  , organizationPartOf :: Maybe Reference
  , organizationContact :: [OrganizationContact]
  , organizationEndpoint :: [Reference]
  }
--

instance ToJSON Organization where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Organization")
    ,  "id" .= toJSON (organizationId p)
    ,  "meta" .= toJSON (organizationMeta p)
    ,  "implicitRules" .= toJSON (organizationImplicitRules p)
    ,  "language" .= toJSON (organizationLanguage p)
    ,  "text" .= toJSON (organizationText p)
--    , "contained" .= toJSON (organizationContained p)
    ,  "extension" .= toJSON (organizationExtension p)
    ,  "modifierExtension" .= toJSON (organizationModifierExtension p)
    ,  "identifier" .= toJSON (organizationIdentifier p)
    ,  "active" .= toJSON (organizationActive p)
    ,  "type" .= toJSON (organizationType p)
    ,  "name" .= toJSON (organizationName p)
    ,  "alias" .= toJSON (organizationAlias p)
    ,  "telecom" .= toJSON (organizationTelecom p)
    ,  "address" .= toJSON (organizationAddress p)
    ,  "partOf" .= toJSON (organizationPartOf p)
    ,  "contact" .= toJSON (organizationContact p)
    ,  "endpoint" .= toJSON (organizationEndpoint p)
    ]
instance FromJSON Organization where
  parseJSON = withObject "Organization" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Organization" -> do
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
        ty <- o .:? "type" .!= []
        name <- o .:? "name"
        alias <- o .:? "alias" .!= []
        telecom <- o .:? "telecom" .!= []
        address <- o .:? "address" .!= []
        partOf <- o .:? "partOf"
        contact <- o .:? "contact" .!= []
        endpoint <- o .:? "endpoint" .!= []
        return Organization{
            organizationId = id
          , organizationMeta = meta
          , organizationImplicitRules = implicitRules
          , organizationLanguage = language
          , organizationText = text
--          , organizationContained = contained
          , organizationExtension = extension
          , organizationModifierExtension = modifierExtension
          , organizationIdentifier = identifier
          , organizationActive = active
          , organizationType = ty
          , organizationName = name
          , organizationAlias = alias
          , organizationTelecom = telecom
          , organizationAddress = address
          , organizationPartOf = partOf
          , organizationContact = contact
          , organizationEndpoint = endpoint
          }
      _ -> fail "not a Organization"
instance Xmlbf.ToXml Organization where
  toXml p = Xmlbf.element "Organization" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (organizationId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (organizationMeta p))
             , OptVal   "implicitRules" (fmap toUri (organizationImplicitRules p))
             , OptVal   "language" (fmap toLanguage (organizationLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (organizationText p))
--             , PropList "contained" (fmap Xmlbf.toXml (organizationContained p))
             , PropList "extension" (fmap Xmlbf.toXml (organizationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (organizationModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (organizationIdentifier p))
             , OptVal   "active" (fmap toBoolean (organizationActive p))
             , PropList "type" (fmap Xmlbf.toXml (organizationType p))
             , OptVal   "name" (fmap toString (organizationName p))
             , ValList  "alias" (fmap toString (organizationAlias p))
             , PropList "telecom" (fmap Xmlbf.toXml (organizationTelecom p))
             , PropList "address" (fmap Xmlbf.toXml (organizationAddress p))
             , OptProp  "partOf" (fmap Xmlbf.toXml (organizationPartOf p))
             , PropList "contact" (fmap Xmlbf.toXml (organizationContact p))
             , PropList "endpoint" (fmap Xmlbf.toXml (organizationEndpoint p))
             ]
instance Xmlbf.FromXml Organization where
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
    ty <- many     $ Xmlbf.pElement "type" Xmlbf.fromXml
    name <- optional $ Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    alias <- many     $ Xmlbf.pElement "alias" (Xmlbf.pAttr "value")
    telecom <- many     $ Xmlbf.pElement "telecom" Xmlbf.fromXml
    address <- many     $ Xmlbf.pElement "address" Xmlbf.fromXml
    partOf <- optional $ Xmlbf.pElement "partOf" Xmlbf.fromXml
    contact <- many     $ Xmlbf.pElement "contact" Xmlbf.fromXml
    endpoint <- many     $ Xmlbf.pElement "endpoint" Xmlbf.fromXml
    return Organization {
            organizationId = fmap fromId id
          , organizationMeta = meta
          , organizationImplicitRules = fmap fromUri implicitRules
          , organizationLanguage = fmap fromLanguage language
          , organizationText = text
--          , organizationContained = contained
          , organizationExtension = extension
          , organizationModifierExtension = modifierExtension
          , organizationIdentifier = identifier
          , organizationActive = fmap fromBoolean active
          , organizationType = ty
          , organizationName = fmap fromString name
          , organizationAlias = fmap fromString alias
          , organizationTelecom = telecom
          , organizationAddress = address
          , organizationPartOf = partOf
          , organizationContact = contact
          , organizationEndpoint = endpoint
          }



data OrganizationAffiliation = OrganizationAffiliation {
    organizationAffiliationId :: Maybe Id
  , organizationAffiliationMeta :: Maybe Meta
  , organizationAffiliationImplicitRules :: Maybe Uri
  , organizationAffiliationLanguage :: Maybe Language
  , organizationAffiliationText :: Maybe Narrative
--    organizationAffiliationContained :: [ResourceContainer]
  , organizationAffiliationExtension :: [Extension]
  , organizationAffiliationModifierExtension :: [Extension]
  , organizationAffiliationIdentifier :: [Identifier]
  , organizationAffiliationActive :: Maybe Boolean
  , organizationAffiliationPeriod :: Maybe Period
  , organizationAffiliationOrganization :: Maybe Reference
  , organizationAffiliationParticipatingOrganization :: Maybe Reference
  , organizationAffiliationNetwork :: [Reference]
  , organizationAffiliationCode :: [CodeableConcept]
  , organizationAffiliationSpecialty :: [CodeableConcept]
  , organizationAffiliationLocation :: [Reference]
  , organizationAffiliationHealthcareService :: [Reference]
  , organizationAffiliationTelecom :: [ContactPoint]
  , organizationAffiliationEndpoint :: [Reference]
  }
--

instance ToJSON OrganizationAffiliation where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "OrganizationAffiliation")
    ,  "id" .= toJSON (organizationAffiliationId p)
    ,  "meta" .= toJSON (organizationAffiliationMeta p)
    ,  "implicitRules" .= toJSON (organizationAffiliationImplicitRules p)
    ,  "language" .= toJSON (organizationAffiliationLanguage p)
    ,  "text" .= toJSON (organizationAffiliationText p)
--    , "contained" .= toJSON (organizationAffiliationContained p)
    ,  "extension" .= toJSON (organizationAffiliationExtension p)
    ,  "modifierExtension" .= toJSON (organizationAffiliationModifierExtension p)
    ,  "identifier" .= toJSON (organizationAffiliationIdentifier p)
    ,  "active" .= toJSON (organizationAffiliationActive p)
    ,  "period" .= toJSON (organizationAffiliationPeriod p)
    ,  "organization" .= toJSON (organizationAffiliationOrganization p)
    ,  "participatingOrganization" .= toJSON (organizationAffiliationParticipatingOrganization p)
    ,  "network" .= toJSON (organizationAffiliationNetwork p)
    ,  "code" .= toJSON (organizationAffiliationCode p)
    ,  "specialty" .= toJSON (organizationAffiliationSpecialty p)
    ,  "location" .= toJSON (organizationAffiliationLocation p)
    ,  "healthcareService" .= toJSON (organizationAffiliationHealthcareService p)
    ,  "telecom" .= toJSON (organizationAffiliationTelecom p)
    ,  "endpoint" .= toJSON (organizationAffiliationEndpoint p)
    ]
instance FromJSON OrganizationAffiliation where
  parseJSON = withObject "OrganizationAffiliation" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "OrganizationAffiliation" -> do
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
        organization <- o .:? "organization"
        participatingOrganization <- o .:? "participatingOrganization"
        network <- o .:? "network" .!= []
        code <- o .:? "code" .!= []
        specialty <- o .:? "specialty" .!= []
        location <- o .:? "location" .!= []
        healthcareService <- o .:? "healthcareService" .!= []
        telecom <- o .:? "telecom" .!= []
        endpoint <- o .:? "endpoint" .!= []
        return OrganizationAffiliation{
            organizationAffiliationId = id
          , organizationAffiliationMeta = meta
          , organizationAffiliationImplicitRules = implicitRules
          , organizationAffiliationLanguage = language
          , organizationAffiliationText = text
--          , organizationAffiliationContained = contained
          , organizationAffiliationExtension = extension
          , organizationAffiliationModifierExtension = modifierExtension
          , organizationAffiliationIdentifier = identifier
          , organizationAffiliationActive = active
          , organizationAffiliationPeriod = period
          , organizationAffiliationOrganization = organization
          , organizationAffiliationParticipatingOrganization = participatingOrganization
          , organizationAffiliationNetwork = network
          , organizationAffiliationCode = code
          , organizationAffiliationSpecialty = specialty
          , organizationAffiliationLocation = location
          , organizationAffiliationHealthcareService = healthcareService
          , organizationAffiliationTelecom = telecom
          , organizationAffiliationEndpoint = endpoint
          }
      _ -> fail "not a OrganizationAffiliation"
instance Xmlbf.ToXml OrganizationAffiliation where
  toXml p = Xmlbf.element "OrganizationAffiliation" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (organizationAffiliationId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (organizationAffiliationMeta p))
             , OptVal   "implicitRules" (fmap toUri (organizationAffiliationImplicitRules p))
             , OptVal   "language" (fmap toLanguage (organizationAffiliationLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (organizationAffiliationText p))
--             , PropList "contained" (fmap Xmlbf.toXml (organizationAffiliationContained p))
             , PropList "extension" (fmap Xmlbf.toXml (organizationAffiliationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (organizationAffiliationModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (organizationAffiliationIdentifier p))
             , OptVal   "active" (fmap toBoolean (organizationAffiliationActive p))
             , OptProp  "period" (fmap Xmlbf.toXml (organizationAffiliationPeriod p))
             , OptProp  "organization" (fmap Xmlbf.toXml (organizationAffiliationOrganization p))
             , OptProp  "participatingOrganization" (fmap Xmlbf.toXml (organizationAffiliationParticipatingOrganization p))
             , PropList "network" (fmap Xmlbf.toXml (organizationAffiliationNetwork p))
             , PropList "code" (fmap Xmlbf.toXml (organizationAffiliationCode p))
             , PropList "specialty" (fmap Xmlbf.toXml (organizationAffiliationSpecialty p))
             , PropList "location" (fmap Xmlbf.toXml (organizationAffiliationLocation p))
             , PropList "healthcareService" (fmap Xmlbf.toXml (organizationAffiliationHealthcareService p))
             , PropList "telecom" (fmap Xmlbf.toXml (organizationAffiliationTelecom p))
             , PropList "endpoint" (fmap Xmlbf.toXml (organizationAffiliationEndpoint p))
             ]
instance Xmlbf.FromXml OrganizationAffiliation where
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
    organization <- optional $ Xmlbf.pElement "organization" Xmlbf.fromXml
    participatingOrganization <- optional $ Xmlbf.pElement "participatingOrganization" Xmlbf.fromXml
    network <- many     $ Xmlbf.pElement "network" Xmlbf.fromXml
    code <- many     $ Xmlbf.pElement "code" Xmlbf.fromXml
    specialty <- many     $ Xmlbf.pElement "specialty" Xmlbf.fromXml
    location <- many     $ Xmlbf.pElement "location" Xmlbf.fromXml
    healthcareService <- many     $ Xmlbf.pElement "healthcareService" Xmlbf.fromXml
    telecom <- many     $ Xmlbf.pElement "telecom" Xmlbf.fromXml
    endpoint <- many     $ Xmlbf.pElement "endpoint" Xmlbf.fromXml
    return OrganizationAffiliation {
            organizationAffiliationId = fmap fromId id
          , organizationAffiliationMeta = meta
          , organizationAffiliationImplicitRules = fmap fromUri implicitRules
          , organizationAffiliationLanguage = fmap fromLanguage language
          , organizationAffiliationText = text
--          , organizationAffiliationContained = contained
          , organizationAffiliationExtension = extension
          , organizationAffiliationModifierExtension = modifierExtension
          , organizationAffiliationIdentifier = identifier
          , organizationAffiliationActive = fmap fromBoolean active
          , organizationAffiliationPeriod = period
          , organizationAffiliationOrganization = organization
          , organizationAffiliationParticipatingOrganization = participatingOrganization
          , organizationAffiliationNetwork = network
          , organizationAffiliationCode = code
          , organizationAffiliationSpecialty = specialty
          , organizationAffiliationLocation = location
          , organizationAffiliationHealthcareService = healthcareService
          , organizationAffiliationTelecom = telecom
          , organizationAffiliationEndpoint = endpoint
          }



data OrganizationContact = OrganizationContact {
    organizationContactAttrId :: Maybe Text
  , organizationContactExtension :: [Extension]
  , organizationContactModifierExtension :: [Extension]
  , organizationContactPurpose :: Maybe CodeableConcept
  , organizationContactName :: Maybe HumanName
  , organizationContactTelecom :: [ContactPoint]
  , organizationContactAddress :: Maybe Address
  }
--

instance ToJSON OrganizationContact where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (organizationContactAttrId p)
    ,  "extension" .= toJSON (organizationContactExtension p)
    ,  "modifierExtension" .= toJSON (organizationContactModifierExtension p)
    ,  "purpose" .= toJSON (organizationContactPurpose p)
    ,  "name" .= toJSON (organizationContactName p)
    ,  "telecom" .= toJSON (organizationContactTelecom p)
    ,  "address" .= toJSON (organizationContactAddress p)
    ]
instance FromJSON OrganizationContact where
  parseJSON = withObject "OrganizationContact" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        purpose <- o .:? "purpose"
        name <- o .:? "name"
        telecom <- o .:? "telecom" .!= []
        address <- o .:? "address"
        return OrganizationContact{
            organizationContactAttrId = id
          , organizationContactExtension = extension
          , organizationContactModifierExtension = modifierExtension
          , organizationContactPurpose = purpose
          , organizationContactName = name
          , organizationContactTelecom = telecom
          , organizationContactAddress = address
          }
instance Xmlbf.ToXml OrganizationContact where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (organizationContactAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (organizationContactExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (organizationContactModifierExtension p))
             , OptProp  "purpose" (fmap Xmlbf.toXml (organizationContactPurpose p))
             , OptProp  "name" (fmap Xmlbf.toXml (organizationContactName p))
             , PropList "telecom" (fmap Xmlbf.toXml (organizationContactTelecom p))
             , OptProp  "address" (fmap Xmlbf.toXml (organizationContactAddress p))
             ]
instance Xmlbf.FromXml OrganizationContact where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    purpose <- optional $ Xmlbf.pElement "purpose" Xmlbf.fromXml
    name <- optional $ Xmlbf.pElement "name" Xmlbf.fromXml
    telecom <- many     $ Xmlbf.pElement "telecom" Xmlbf.fromXml
    address <- optional $ Xmlbf.pElement "address" Xmlbf.fromXml
    return OrganizationContact {
            organizationContactAttrId = id
          , organizationContactExtension = extension
          , organizationContactModifierExtension = modifierExtension
          , organizationContactPurpose = purpose
          , organizationContactName = name
          , organizationContactTelecom = telecom
          , organizationContactAddress = address
          }




