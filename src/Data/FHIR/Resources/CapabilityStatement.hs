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
-- FHIR 4.0.0 CapabilityStatement
--

module Data.FHIR.Resources.CapabilityStatement where

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

data CapabilityStatementKind
    = CSKInstance
    | CSKCapability
    | CSKRequirements
  deriving (Eq, Show)

instance ToJSON CapabilityStatementKind where
    toJSON CSKInstance = String "instance"
    toJSON CSKCapability = String "capability"
    toJSON CSKRequirements = String "requirements"
instance FromJSON CapabilityStatementKind where
    parseJSON "instance" = return CSKInstance
    parseJSON "capability" = return CSKCapability
    parseJSON "requirements" = return CSKRequirements

toCapabilityStatementKind CSKInstance = "instance"
toCapabilityStatementKind CSKCapability = "capability"
toCapabilityStatementKind CSKRequirements = "requirements"
fromCapabilityStatementKind "instance" = CSKInstance
fromCapabilityStatementKind "capability" = CSKCapability
fromCapabilityStatementKind "requirements" = CSKRequirements


data CapabilityStatement = CapabilityStatement {
    capabilityStatementId :: Maybe Id
  , capabilityStatementMeta :: Maybe Meta
  , capabilityStatementImplicitRules :: Maybe Uri
  , capabilityStatementLanguage :: Maybe Language
  , capabilityStatementText :: Maybe Narrative
--    capabilityStatementContained :: [ResourceContainer]
  , capabilityStatementExtension :: [Extension]
  , capabilityStatementModifierExtension :: [Extension]
  , capabilityStatementUrl :: Maybe Uri
  , capabilityStatementVersion :: Maybe Text
  , capabilityStatementName :: Maybe Text
  , capabilityStatementTitle :: Maybe Text
  , capabilityStatementStatus :: PublicationStatus
  , capabilityStatementExperimental :: Maybe Boolean
  , capabilityStatementDate :: DateTime
  , capabilityStatementPublisher :: Maybe Text
  , capabilityStatementContact :: [ContactDetail]
  , capabilityStatementDescription :: Maybe Markdown
  , capabilityStatementUseContext :: [UsageContext]
  , capabilityStatementJurisdiction :: [CodeableConcept]
  , capabilityStatementPurpose :: Maybe Markdown
  , capabilityStatementCopyright :: Maybe Markdown
  , capabilityStatementKind :: CapabilityStatementKind
  , capabilityStatementInstantiates :: [Canonical]
  , capabilityStatementImports :: [Canonical]
  , capabilityStatementSoftware :: Maybe CapabilityStatementSoftware
  , capabilityStatementImplementation :: Maybe CapabilityStatementImplementation
  , capabilityStatementFhirVersion :: FHIRVersion
  , capabilityStatementFormat :: [Code]
  , capabilityStatementPatchFormat :: [Code]
  , capabilityStatementImplementationGuide :: [Canonical]
  , capabilityStatementRest :: [CapabilityStatementRest]
  , capabilityStatementMessaging :: [CapabilityStatementMessaging]
  , capabilityStatementDocument :: [CapabilityStatementDocument]
  } deriving (Eq, Show)
--

instance ToJSON CapabilityStatement where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "CapabilityStatement")
    ,  "id" .= toJSON (capabilityStatementId p)
    ,  "meta" .= toJSON (capabilityStatementMeta p)
    ,  "implicitRules" .= toJSON (capabilityStatementImplicitRules p)
    ,  "language" .= toJSON (capabilityStatementLanguage p)
    ,  "text" .= toJSON (capabilityStatementText p)
--    , "contained" .= toJSON (capabilityStatementContained p)
    ,  "extension" .= toJSON (capabilityStatementExtension p)
    ,  "modifierExtension" .= toJSON (capabilityStatementModifierExtension p)
    ,  "url" .= toJSON (capabilityStatementUrl p)
    ,  "version" .= toJSON (capabilityStatementVersion p)
    ,  "name" .= toJSON (capabilityStatementName p)
    ,  "title" .= toJSON (capabilityStatementTitle p)
    ,  "status" .= toJSON (capabilityStatementStatus p)
    ,  "experimental" .= toJSON (capabilityStatementExperimental p)
    ,  "date" .= toJSON (capabilityStatementDate p)
    ,  "publisher" .= toJSON (capabilityStatementPublisher p)
    ,  "contact" .= toJSON (capabilityStatementContact p)
    ,  "description" .= toJSON (capabilityStatementDescription p)
    ,  "useContext" .= toJSON (capabilityStatementUseContext p)
    ,  "jurisdiction" .= toJSON (capabilityStatementJurisdiction p)
    ,  "purpose" .= toJSON (capabilityStatementPurpose p)
    ,  "copyright" .= toJSON (capabilityStatementCopyright p)
    ,  "kind" .= toJSON (capabilityStatementKind p)
    ,  "instantiates" .= toJSON (capabilityStatementInstantiates p)
    ,  "imports" .= toJSON (capabilityStatementImports p)
    ,  "software" .= toJSON (capabilityStatementSoftware p)
    ,  "implementation" .= toJSON (capabilityStatementImplementation p)
    ,  "fhirVersion" .= toJSON (capabilityStatementFhirVersion p)
    ,  "format" .= toJSON (capabilityStatementFormat p)
    ,  "patchFormat" .= toJSON (capabilityStatementPatchFormat p)
    ,  "implementationGuide" .= toJSON (capabilityStatementImplementationGuide p)
    ,  "rest" .= toJSON (capabilityStatementRest p)
    ,  "messaging" .= toJSON (capabilityStatementMessaging p)
    ,  "document" .= toJSON (capabilityStatementDocument p)
    ]
instance FromJSON CapabilityStatement where
  parseJSON = withObject "CapabilityStatement" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "CapabilityStatement" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        url <- o .:? "url"
        version <- o .:? "version"
        name <- o .:? "name"
        title <- o .:? "title"
        status <- o .:  "status"
        experimental <- o .:? "experimental"
        date <- o .:  "date"
        publisher <- o .:? "publisher"
        contact <- o .:? "contact" .!= []
        description <- o .:? "description"
        useContext <- o .:? "useContext" .!= []
        jurisdiction <- o .:? "jurisdiction" .!= []
        purpose <- o .:? "purpose"
        copyright <- o .:? "copyright"
        kind <- o .:  "kind"
        instantiates <- o .:? "instantiates" .!= []
        imports <- o .:? "imports" .!= []
        software <- o .:? "software"
        implementation <- o .:? "implementation"
        fhirVersion <- o .:  "fhirVersion"
        format <- o .:? "format" .!= []
        patchFormat <- o .:? "patchFormat" .!= []
        implementationGuide <- o .:? "implementationGuide" .!= []
        rest <- o .:? "rest" .!= []
        messaging <- o .:? "messaging" .!= []
        document <- o .:? "document" .!= []
        return CapabilityStatement{
            capabilityStatementId = id
          , capabilityStatementMeta = meta
          , capabilityStatementImplicitRules = implicitRules
          , capabilityStatementLanguage = language
          , capabilityStatementText = text
--          , capabilityStatementContained = contained
          , capabilityStatementExtension = extension
          , capabilityStatementModifierExtension = modifierExtension
          , capabilityStatementUrl = url
          , capabilityStatementVersion = version
          , capabilityStatementName = name
          , capabilityStatementTitle = title
          , capabilityStatementStatus = status
          , capabilityStatementExperimental = experimental
          , capabilityStatementDate = date
          , capabilityStatementPublisher = publisher
          , capabilityStatementContact = contact
          , capabilityStatementDescription = description
          , capabilityStatementUseContext = useContext
          , capabilityStatementJurisdiction = jurisdiction
          , capabilityStatementPurpose = purpose
          , capabilityStatementCopyright = copyright
          , capabilityStatementKind = kind
          , capabilityStatementInstantiates = instantiates
          , capabilityStatementImports = imports
          , capabilityStatementSoftware = software
          , capabilityStatementImplementation = implementation
          , capabilityStatementFhirVersion = fhirVersion
          , capabilityStatementFormat = format
          , capabilityStatementPatchFormat = patchFormat
          , capabilityStatementImplementationGuide = implementationGuide
          , capabilityStatementRest = rest
          , capabilityStatementMessaging = messaging
          , capabilityStatementDocument = document
          }
      _ -> fail "not a CapabilityStatement"
instance Xmlbf.ToXml CapabilityStatement where
  toXml p = Xmlbf.element "CapabilityStatement" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (capabilityStatementId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (capabilityStatementMeta p))
             , OptVal   "implicitRules" (fmap toUri (capabilityStatementImplicitRules p))
             , OptVal   "language" (fmap toLanguage (capabilityStatementLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (capabilityStatementText p))
--             , PropList "contained" (fmap Xmlbf.toXml (capabilityStatementContained p))
             , PropList "extension" (fmap Xmlbf.toXml (capabilityStatementExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (capabilityStatementModifierExtension p))
             , OptVal   "url" (fmap toUri (capabilityStatementUrl p))
             , OptVal   "version" (fmap toString (capabilityStatementVersion p))
             , OptVal   "name" (fmap toString (capabilityStatementName p))
             , OptVal   "title" (fmap toString (capabilityStatementTitle p))
             , Val      "status" (     toPublicationStatus (capabilityStatementStatus p))
             , OptVal   "experimental" (fmap toBoolean (capabilityStatementExperimental p))
             , Val      "date" (     toDateTime (capabilityStatementDate p))
             , OptVal   "publisher" (fmap toString (capabilityStatementPublisher p))
             , PropList "contact" (fmap Xmlbf.toXml (capabilityStatementContact p))
             , OptVal   "description" (fmap toMarkdown (capabilityStatementDescription p))
             , PropList "useContext" (fmap Xmlbf.toXml (capabilityStatementUseContext p))
             , PropList "jurisdiction" (fmap Xmlbf.toXml (capabilityStatementJurisdiction p))
             , OptVal   "purpose" (fmap toMarkdown (capabilityStatementPurpose p))
             , OptVal   "copyright" (fmap toMarkdown (capabilityStatementCopyright p))
             , Val      "kind" (     toCapabilityStatementKind (capabilityStatementKind p))
             , ValList  "instantiates" (fmap toCanonical (capabilityStatementInstantiates p))
             , ValList  "imports" (fmap toCanonical (capabilityStatementImports p))
             , OptProp  "software" (fmap Xmlbf.toXml (capabilityStatementSoftware p))
             , OptProp  "implementation" (fmap Xmlbf.toXml (capabilityStatementImplementation p))
             , Val      "fhirVersion" (     toFHIRVersion (capabilityStatementFhirVersion p))
             , ValList  "format" (fmap toCode (capabilityStatementFormat p))
             , ValList  "patchFormat" (fmap toCode (capabilityStatementPatchFormat p))
             , ValList  "implementationGuide" (fmap toCanonical (capabilityStatementImplementationGuide p))
             , PropList "rest" (fmap Xmlbf.toXml (capabilityStatementRest p))
             , PropList "messaging" (fmap Xmlbf.toXml (capabilityStatementMessaging p))
             , PropList "document" (fmap Xmlbf.toXml (capabilityStatementDocument p))
             ]
instance Xmlbf.FromXml CapabilityStatement where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" Xmlbf.fromXml
--    contained <- many     $ Xmlbf.pElement "contained" Xmlbf.fromXml
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    url <- optional $ Xmlbf.pElement "url" (Xmlbf.pAttr "value")
    version <- optional $ Xmlbf.pElement "version" (Xmlbf.pAttr "value")
    name <- optional $ Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    title <- optional $ Xmlbf.pElement "title" (Xmlbf.pAttr "value")
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    experimental <- optional $ Xmlbf.pElement "experimental" (Xmlbf.pAttr "value")
    date <-            Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    publisher <- optional $ Xmlbf.pElement "publisher" (Xmlbf.pAttr "value")
    contact <- many     $ Xmlbf.pElement "contact" Xmlbf.fromXml
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    useContext <- many     $ Xmlbf.pElement "useContext" Xmlbf.fromXml
    jurisdiction <- many     $ Xmlbf.pElement "jurisdiction" Xmlbf.fromXml
    purpose <- optional $ Xmlbf.pElement "purpose" (Xmlbf.pAttr "value")
    copyright <- optional $ Xmlbf.pElement "copyright" (Xmlbf.pAttr "value")
    kind <-            Xmlbf.pElement "kind" (Xmlbf.pAttr "value")
    instantiates <- many     $ Xmlbf.pElement "instantiates" (Xmlbf.pAttr "value")
    imports <- many     $ Xmlbf.pElement "imports" (Xmlbf.pAttr "value")
    software <- optional $ Xmlbf.pElement "software" Xmlbf.fromXml
    implementation <- optional $ Xmlbf.pElement "implementation" Xmlbf.fromXml
    fhirVersion <-            Xmlbf.pElement "fhirVersion" (Xmlbf.pAttr "value")
    format <- many     $ Xmlbf.pElement "format" (Xmlbf.pAttr "value")
    patchFormat <- many     $ Xmlbf.pElement "patchFormat" (Xmlbf.pAttr "value")
    implementationGuide <- many     $ Xmlbf.pElement "implementationGuide" (Xmlbf.pAttr "value")
    rest <- many     $ Xmlbf.pElement "rest" Xmlbf.fromXml
    messaging <- many     $ Xmlbf.pElement "messaging" Xmlbf.fromXml
    document <- many     $ Xmlbf.pElement "document" Xmlbf.fromXml
    return CapabilityStatement {
            capabilityStatementId = fmap fromId id
          , capabilityStatementMeta = meta
          , capabilityStatementImplicitRules = fmap fromUri implicitRules
          , capabilityStatementLanguage = fmap fromLanguage language
          , capabilityStatementText = text
--          , capabilityStatementContained = contained
          , capabilityStatementExtension = extension
          , capabilityStatementModifierExtension = modifierExtension
          , capabilityStatementUrl = fmap fromUri url
          , capabilityStatementVersion = fmap fromString version
          , capabilityStatementName = fmap fromString name
          , capabilityStatementTitle = fmap fromString title
          , capabilityStatementStatus =      fromPublicationStatus status
          , capabilityStatementExperimental = fmap fromBoolean experimental
          , capabilityStatementDate =      fromDateTime date
          , capabilityStatementPublisher = fmap fromString publisher
          , capabilityStatementContact = contact
          , capabilityStatementDescription = fmap fromMarkdown description
          , capabilityStatementUseContext = useContext
          , capabilityStatementJurisdiction = jurisdiction
          , capabilityStatementPurpose = fmap fromMarkdown purpose
          , capabilityStatementCopyright = fmap fromMarkdown copyright
          , capabilityStatementKind =      fromCapabilityStatementKind kind
          , capabilityStatementInstantiates = fmap fromCanonical instantiates
          , capabilityStatementImports = fmap fromCanonical imports
          , capabilityStatementSoftware = software
          , capabilityStatementImplementation = implementation
          , capabilityStatementFhirVersion =      fromFHIRVersion fhirVersion
          , capabilityStatementFormat = fmap fromCode format
          , capabilityStatementPatchFormat = fmap fromCode patchFormat
          , capabilityStatementImplementationGuide = fmap fromCanonical implementationGuide
          , capabilityStatementRest = rest
          , capabilityStatementMessaging = messaging
          , capabilityStatementDocument = document
          }



data CapabilityStatementImplementation = CapabilityStatementImplementation {
    capabilityStatementImplementationAttrId :: Maybe Text
  , capabilityStatementImplementationExtension :: [Extension]
  , capabilityStatementImplementationModifierExtension :: [Extension]
  , capabilityStatementImplementationDescription :: Text
  , capabilityStatementImplementationUrl :: Maybe Url
  , capabilityStatementImplementationCustodian :: Maybe Reference
  } deriving (Eq, Show)
--

instance ToJSON CapabilityStatementImplementation where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (capabilityStatementImplementationAttrId p)
    ,  "extension" .= toJSON (capabilityStatementImplementationExtension p)
    ,  "modifierExtension" .= toJSON (capabilityStatementImplementationModifierExtension p)
    ,  "description" .= toJSON (capabilityStatementImplementationDescription p)
    ,  "url" .= toJSON (capabilityStatementImplementationUrl p)
    ,  "custodian" .= toJSON (capabilityStatementImplementationCustodian p)
    ]
instance FromJSON CapabilityStatementImplementation where
  parseJSON = withObject "CapabilityStatementImplementation" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        description <- o .:  "description"
        url <- o .:? "url"
        custodian <- o .:? "custodian"
        return CapabilityStatementImplementation{
            capabilityStatementImplementationAttrId = id
          , capabilityStatementImplementationExtension = extension
          , capabilityStatementImplementationModifierExtension = modifierExtension
          , capabilityStatementImplementationDescription = description
          , capabilityStatementImplementationUrl = url
          , capabilityStatementImplementationCustodian = custodian
          }
instance Xmlbf.ToXml CapabilityStatementImplementation where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (capabilityStatementImplementationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (capabilityStatementImplementationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (capabilityStatementImplementationModifierExtension p))
             , Val      "description" (     toString (capabilityStatementImplementationDescription p))
             , OptVal   "url" (fmap toUrl (capabilityStatementImplementationUrl p))
             , OptProp  "custodian" (fmap Xmlbf.toXml (capabilityStatementImplementationCustodian p))
             ]
instance Xmlbf.FromXml CapabilityStatementImplementation where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    description <-            Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    url <- optional $ Xmlbf.pElement "url" (Xmlbf.pAttr "value")
    custodian <- optional $ Xmlbf.pElement "custodian" Xmlbf.fromXml
    return CapabilityStatementImplementation {
            capabilityStatementImplementationAttrId = id
          , capabilityStatementImplementationExtension = extension
          , capabilityStatementImplementationModifierExtension = modifierExtension
          , capabilityStatementImplementationDescription =      fromString description
          , capabilityStatementImplementationUrl = fmap fromUrl url
          , capabilityStatementImplementationCustodian = custodian
          }



data CapabilityStatementRestMode
    = CSRMClient
    | CSRMServer
  deriving (Eq, Show)

instance ToJSON CapabilityStatementRestMode where
    toJSON CSRMClient = String "client"
    toJSON CSRMServer = String "server"
instance FromJSON CapabilityStatementRestMode where
    parseJSON "client" = return CSRMClient
    parseJSON "server" = return CSRMServer

toCapabilityStatementRestMode CSRMClient = "client"
toCapabilityStatementRestMode CSRMServer = "server"
fromCapabilityStatementRestMode "client" = CSRMClient
fromCapabilityStatementRestMode "server" = CSRMServer


data CapabilityStatementRest = CapabilityStatementRest {
    capabilityStatementRestAttrId :: Maybe Text
  , capabilityStatementRestExtension :: [Extension]
  , capabilityStatementRestModifierExtension :: [Extension]
  , capabilityStatementRestMode :: CapabilityStatementRestMode
  , capabilityStatementRestDocumentation :: Maybe Markdown
  , capabilityStatementRestSecurity :: Maybe CapabilityStatementSecurity
  , capabilityStatementRestResource :: [CapabilityStatementResource]
  , capabilityStatementRestInteraction :: [CapabilityStatementInteraction1]
  , capabilityStatementRestSearchParam :: [CapabilityStatementSearchParam]
  , capabilityStatementRestOperation :: [CapabilityStatementOperation]
  , capabilityStatementRestCompartment :: [Canonical]
  } deriving (Eq, Show)
--

instance ToJSON CapabilityStatementRest where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (capabilityStatementRestAttrId p)
    ,  "extension" .= toJSON (capabilityStatementRestExtension p)
    ,  "modifierExtension" .= toJSON (capabilityStatementRestModifierExtension p)
    ,  "mode" .= toJSON (capabilityStatementRestMode p)
    ,  "documentation" .= toJSON (capabilityStatementRestDocumentation p)
    ,  "security" .= toJSON (capabilityStatementRestSecurity p)
    ,  "resource" .= toJSON (capabilityStatementRestResource p)
    ,  "interaction" .= toJSON (capabilityStatementRestInteraction p)
    ,  "searchParam" .= toJSON (capabilityStatementRestSearchParam p)
    ,  "operation" .= toJSON (capabilityStatementRestOperation p)
    ,  "compartment" .= toJSON (capabilityStatementRestCompartment p)
    ]
instance FromJSON CapabilityStatementRest where
  parseJSON = withObject "CapabilityStatementRest" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        mode <- o .:  "mode"
        documentation <- o .:? "documentation"
        security <- o .:? "security"
        resource <- o .:? "resource" .!= []
        interaction <- o .:? "interaction" .!= []
        searchParam <- o .:? "searchParam" .!= []
        operation <- o .:? "operation" .!= []
        compartment <- o .:? "compartment" .!= []
        return CapabilityStatementRest{
            capabilityStatementRestAttrId = id
          , capabilityStatementRestExtension = extension
          , capabilityStatementRestModifierExtension = modifierExtension
          , capabilityStatementRestMode = mode
          , capabilityStatementRestDocumentation = documentation
          , capabilityStatementRestSecurity = security
          , capabilityStatementRestResource = resource
          , capabilityStatementRestInteraction = interaction
          , capabilityStatementRestSearchParam = searchParam
          , capabilityStatementRestOperation = operation
          , capabilityStatementRestCompartment = compartment
          }
instance Xmlbf.ToXml CapabilityStatementRest where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (capabilityStatementRestAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (capabilityStatementRestExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (capabilityStatementRestModifierExtension p))
             , Val      "mode" (     toCapabilityStatementRestMode (capabilityStatementRestMode p))
             , OptVal   "documentation" (fmap toMarkdown (capabilityStatementRestDocumentation p))
             , OptProp  "security" (fmap Xmlbf.toXml (capabilityStatementRestSecurity p))
             , PropList "resource" (fmap Xmlbf.toXml (capabilityStatementRestResource p))
             , PropList "interaction" (fmap Xmlbf.toXml (capabilityStatementRestInteraction p))
             , PropList "searchParam" (fmap Xmlbf.toXml (capabilityStatementRestSearchParam p))
             , PropList "operation" (fmap Xmlbf.toXml (capabilityStatementRestOperation p))
             , ValList  "compartment" (fmap toCanonical (capabilityStatementRestCompartment p))
             ]
instance Xmlbf.FromXml CapabilityStatementRest where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    mode <-            Xmlbf.pElement "mode" (Xmlbf.pAttr "value")
    documentation <- optional $ Xmlbf.pElement "documentation" (Xmlbf.pAttr "value")
    security <- optional $ Xmlbf.pElement "security" Xmlbf.fromXml
    resource <- many     $ Xmlbf.pElement "resource" Xmlbf.fromXml
    interaction <- many     $ Xmlbf.pElement "interaction" Xmlbf.fromXml
    searchParam <- many     $ Xmlbf.pElement "searchParam" Xmlbf.fromXml
    operation <- many     $ Xmlbf.pElement "operation" Xmlbf.fromXml
    compartment <- many     $ Xmlbf.pElement "compartment" (Xmlbf.pAttr "value")
    return CapabilityStatementRest {
            capabilityStatementRestAttrId = id
          , capabilityStatementRestExtension = extension
          , capabilityStatementRestModifierExtension = modifierExtension
          , capabilityStatementRestMode =      fromCapabilityStatementRestMode mode
          , capabilityStatementRestDocumentation = fmap fromMarkdown documentation
          , capabilityStatementRestSecurity = security
          , capabilityStatementRestResource = resource
          , capabilityStatementRestInteraction = interaction
          , capabilityStatementRestSearchParam = searchParam
          , capabilityStatementRestOperation = operation
          , capabilityStatementRestCompartment = fmap fromCanonical compartment
          }



data CapabilityStatementDocumentMode
    = CSDMProducer
    | CSDMConsumer
  deriving (Eq, Show)

instance ToJSON CapabilityStatementDocumentMode where
    toJSON CSDMProducer = String "producer"
    toJSON CSDMConsumer = String "consumer"
instance FromJSON CapabilityStatementDocumentMode where
    parseJSON "producer" = return CSDMProducer
    parseJSON "consumer" = return CSDMConsumer

toCapabilityStatementDocumentMode CSDMProducer = "producer"
toCapabilityStatementDocumentMode CSDMConsumer = "consumer"
fromCapabilityStatementDocumentMode "producer" = CSDMProducer
fromCapabilityStatementDocumentMode "consumer" = CSDMConsumer


data CapabilityStatementDocument = CapabilityStatementDocument {
    capabilityStatementDocumentAttrId :: Maybe Text
  , capabilityStatementDocumentExtension :: [Extension]
  , capabilityStatementDocumentModifierExtension :: [Extension]
  , capabilityStatementDocumentMode :: CapabilityStatementDocumentMode
  , capabilityStatementDocumentDocumentation :: Maybe Markdown
  , capabilityStatementDocumentProfile :: Canonical
  } deriving (Eq, Show)
--

instance ToJSON CapabilityStatementDocument where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (capabilityStatementDocumentAttrId p)
    ,  "extension" .= toJSON (capabilityStatementDocumentExtension p)
    ,  "modifierExtension" .= toJSON (capabilityStatementDocumentModifierExtension p)
    ,  "mode" .= toJSON (capabilityStatementDocumentMode p)
    ,  "documentation" .= toJSON (capabilityStatementDocumentDocumentation p)
    ,  "profile" .= toJSON (capabilityStatementDocumentProfile p)
    ]
instance FromJSON CapabilityStatementDocument where
  parseJSON = withObject "CapabilityStatementDocument" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        mode <- o .:  "mode"
        documentation <- o .:? "documentation"
        profile <- o .:  "profile"
        return CapabilityStatementDocument{
            capabilityStatementDocumentAttrId = id
          , capabilityStatementDocumentExtension = extension
          , capabilityStatementDocumentModifierExtension = modifierExtension
          , capabilityStatementDocumentMode = mode
          , capabilityStatementDocumentDocumentation = documentation
          , capabilityStatementDocumentProfile = profile
          }
instance Xmlbf.ToXml CapabilityStatementDocument where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (capabilityStatementDocumentAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (capabilityStatementDocumentExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (capabilityStatementDocumentModifierExtension p))
             , Val      "mode" (     toCapabilityStatementDocumentMode (capabilityStatementDocumentMode p))
             , OptVal   "documentation" (fmap toMarkdown (capabilityStatementDocumentDocumentation p))
             , Val      "profile" (     toCanonical (capabilityStatementDocumentProfile p))
             ]
instance Xmlbf.FromXml CapabilityStatementDocument where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    mode <-            Xmlbf.pElement "mode" (Xmlbf.pAttr "value")
    documentation <- optional $ Xmlbf.pElement "documentation" (Xmlbf.pAttr "value")
    profile <-            Xmlbf.pElement "profile" (Xmlbf.pAttr "value")
    return CapabilityStatementDocument {
            capabilityStatementDocumentAttrId = id
          , capabilityStatementDocumentExtension = extension
          , capabilityStatementDocumentModifierExtension = modifierExtension
          , capabilityStatementDocumentMode =      fromCapabilityStatementDocumentMode mode
          , capabilityStatementDocumentDocumentation = fmap fromMarkdown documentation
          , capabilityStatementDocumentProfile =      fromCanonical profile
          }



data CapabilityStatementSoftware = CapabilityStatementSoftware {
    capabilityStatementSoftwareAttrId :: Maybe Text
  , capabilityStatementSoftwareExtension :: [Extension]
  , capabilityStatementSoftwareModifierExtension :: [Extension]
  , capabilityStatementSoftwareName :: Text
  , capabilityStatementSoftwareVersion :: Maybe Text
  , capabilityStatementSoftwareReleaseDate :: Maybe DateTime
  } deriving (Eq, Show)
--

instance ToJSON CapabilityStatementSoftware where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (capabilityStatementSoftwareAttrId p)
    ,  "extension" .= toJSON (capabilityStatementSoftwareExtension p)
    ,  "modifierExtension" .= toJSON (capabilityStatementSoftwareModifierExtension p)
    ,  "name" .= toJSON (capabilityStatementSoftwareName p)
    ,  "version" .= toJSON (capabilityStatementSoftwareVersion p)
    ,  "releaseDate" .= toJSON (capabilityStatementSoftwareReleaseDate p)
    ]
instance FromJSON CapabilityStatementSoftware where
  parseJSON = withObject "CapabilityStatementSoftware" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        name <- o .:  "name"
        version <- o .:? "version"
        releaseDate <- o .:? "releaseDate"
        return CapabilityStatementSoftware{
            capabilityStatementSoftwareAttrId = id
          , capabilityStatementSoftwareExtension = extension
          , capabilityStatementSoftwareModifierExtension = modifierExtension
          , capabilityStatementSoftwareName = name
          , capabilityStatementSoftwareVersion = version
          , capabilityStatementSoftwareReleaseDate = releaseDate
          }
instance Xmlbf.ToXml CapabilityStatementSoftware where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (capabilityStatementSoftwareAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (capabilityStatementSoftwareExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (capabilityStatementSoftwareModifierExtension p))
             , Val      "name" (     toString (capabilityStatementSoftwareName p))
             , OptVal   "version" (fmap toString (capabilityStatementSoftwareVersion p))
             , OptVal   "releaseDate" (fmap toDateTime (capabilityStatementSoftwareReleaseDate p))
             ]
instance Xmlbf.FromXml CapabilityStatementSoftware where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    name <-            Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    version <- optional $ Xmlbf.pElement "version" (Xmlbf.pAttr "value")
    releaseDate <- optional $ Xmlbf.pElement "releaseDate" (Xmlbf.pAttr "value")
    return CapabilityStatementSoftware {
            capabilityStatementSoftwareAttrId = id
          , capabilityStatementSoftwareExtension = extension
          , capabilityStatementSoftwareModifierExtension = modifierExtension
          , capabilityStatementSoftwareName =      fromString name
          , capabilityStatementSoftwareVersion = fmap fromString version
          , capabilityStatementSoftwareReleaseDate = fmap fromDateTime releaseDate
          }



data CapabilityStatementMessaging = CapabilityStatementMessaging {
    capabilityStatementMessagingAttrId :: Maybe Text
  , capabilityStatementMessagingExtension :: [Extension]
  , capabilityStatementMessagingModifierExtension :: [Extension]
  , capabilityStatementMessagingEndpoint :: [CapabilityStatementEndpoint]
  , capabilityStatementMessagingReliableCache :: Maybe UnsignedInt
  , capabilityStatementMessagingDocumentation :: Maybe Markdown
  , capabilityStatementMessagingSupportedMessage :: [CapabilityStatementSupportedMessage]
  } deriving (Eq, Show)
--

instance ToJSON CapabilityStatementMessaging where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (capabilityStatementMessagingAttrId p)
    ,  "extension" .= toJSON (capabilityStatementMessagingExtension p)
    ,  "modifierExtension" .= toJSON (capabilityStatementMessagingModifierExtension p)
    ,  "endpoint" .= toJSON (capabilityStatementMessagingEndpoint p)
    ,  "reliableCache" .= toJSON (capabilityStatementMessagingReliableCache p)
    ,  "documentation" .= toJSON (capabilityStatementMessagingDocumentation p)
    ,  "supportedMessage" .= toJSON (capabilityStatementMessagingSupportedMessage p)
    ]
instance FromJSON CapabilityStatementMessaging where
  parseJSON = withObject "CapabilityStatementMessaging" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        endpoint <- o .:? "endpoint" .!= []
        reliableCache <- o .:? "reliableCache"
        documentation <- o .:? "documentation"
        supportedMessage <- o .:? "supportedMessage" .!= []
        return CapabilityStatementMessaging{
            capabilityStatementMessagingAttrId = id
          , capabilityStatementMessagingExtension = extension
          , capabilityStatementMessagingModifierExtension = modifierExtension
          , capabilityStatementMessagingEndpoint = endpoint
          , capabilityStatementMessagingReliableCache = reliableCache
          , capabilityStatementMessagingDocumentation = documentation
          , capabilityStatementMessagingSupportedMessage = supportedMessage
          }
instance Xmlbf.ToXml CapabilityStatementMessaging where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (capabilityStatementMessagingAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (capabilityStatementMessagingExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (capabilityStatementMessagingModifierExtension p))
             , PropList "endpoint" (fmap Xmlbf.toXml (capabilityStatementMessagingEndpoint p))
             , OptVal   "reliableCache" (fmap toUnsignedInt (capabilityStatementMessagingReliableCache p))
             , OptVal   "documentation" (fmap toMarkdown (capabilityStatementMessagingDocumentation p))
             , PropList "supportedMessage" (fmap Xmlbf.toXml (capabilityStatementMessagingSupportedMessage p))
             ]
instance Xmlbf.FromXml CapabilityStatementMessaging where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    endpoint <- many     $ Xmlbf.pElement "endpoint" Xmlbf.fromXml
    reliableCache <- optional $ Xmlbf.pElement "reliableCache" (Xmlbf.pAttr "value")
    documentation <- optional $ Xmlbf.pElement "documentation" (Xmlbf.pAttr "value")
    supportedMessage <- many     $ Xmlbf.pElement "supportedMessage" Xmlbf.fromXml
    return CapabilityStatementMessaging {
            capabilityStatementMessagingAttrId = id
          , capabilityStatementMessagingExtension = extension
          , capabilityStatementMessagingModifierExtension = modifierExtension
          , capabilityStatementMessagingEndpoint = endpoint
          , capabilityStatementMessagingReliableCache = fmap fromUnsignedInt reliableCache
          , capabilityStatementMessagingDocumentation = fmap fromMarkdown documentation
          , capabilityStatementMessagingSupportedMessage = supportedMessage
          }



data CapabilityStatementSupportedMessageMode
    = CSSMMSender
    | CSSMMReceiver
  deriving (Eq, Show)

instance ToJSON CapabilityStatementSupportedMessageMode where
    toJSON CSSMMSender = String "sender"
    toJSON CSSMMReceiver = String "receiver"
instance FromJSON CapabilityStatementSupportedMessageMode where
    parseJSON "sender" = return CSSMMSender
    parseJSON "receiver" = return CSSMMReceiver

toCapabilityStatementSupportedMessageMode CSSMMSender = "sender"
toCapabilityStatementSupportedMessageMode CSSMMReceiver = "receiver"
fromCapabilityStatementSupportedMessageMode "sender" = CSSMMSender
fromCapabilityStatementSupportedMessageMode "receiver" = CSSMMReceiver


data CapabilityStatementSupportedMessage = CapabilityStatementSupportedMessage {
    capabilityStatementSupportedMessageAttrId :: Maybe Text
  , capabilityStatementSupportedMessageExtension :: [Extension]
  , capabilityStatementSupportedMessageModifierExtension :: [Extension]
  , capabilityStatementSupportedMessageMode :: CapabilityStatementSupportedMessageMode
  , capabilityStatementSupportedMessageDefinition :: Canonical
  } deriving (Eq, Show)
--

instance ToJSON CapabilityStatementSupportedMessage where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (capabilityStatementSupportedMessageAttrId p)
    ,  "extension" .= toJSON (capabilityStatementSupportedMessageExtension p)
    ,  "modifierExtension" .= toJSON (capabilityStatementSupportedMessageModifierExtension p)
    ,  "mode" .= toJSON (capabilityStatementSupportedMessageMode p)
    ,  "definition" .= toJSON (capabilityStatementSupportedMessageDefinition p)
    ]
instance FromJSON CapabilityStatementSupportedMessage where
  parseJSON = withObject "CapabilityStatementSupportedMessage" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        mode <- o .:  "mode"
        definition <- o .:  "definition"
        return CapabilityStatementSupportedMessage{
            capabilityStatementSupportedMessageAttrId = id
          , capabilityStatementSupportedMessageExtension = extension
          , capabilityStatementSupportedMessageModifierExtension = modifierExtension
          , capabilityStatementSupportedMessageMode = mode
          , capabilityStatementSupportedMessageDefinition = definition
          }
instance Xmlbf.ToXml CapabilityStatementSupportedMessage where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (capabilityStatementSupportedMessageAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (capabilityStatementSupportedMessageExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (capabilityStatementSupportedMessageModifierExtension p))
             , Val      "mode" (     toCapabilityStatementSupportedMessageMode (capabilityStatementSupportedMessageMode p))
             , Val      "definition" (     toCanonical (capabilityStatementSupportedMessageDefinition p))
             ]
instance Xmlbf.FromXml CapabilityStatementSupportedMessage where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    mode <-            Xmlbf.pElement "mode" (Xmlbf.pAttr "value")
    definition <-            Xmlbf.pElement "definition" (Xmlbf.pAttr "value")
    return CapabilityStatementSupportedMessage {
            capabilityStatementSupportedMessageAttrId = id
          , capabilityStatementSupportedMessageExtension = extension
          , capabilityStatementSupportedMessageModifierExtension = modifierExtension
          , capabilityStatementSupportedMessageMode =      fromCapabilityStatementSupportedMessageMode mode
          , capabilityStatementSupportedMessageDefinition =      fromCanonical definition
          }



data CapabilityStatementEndpoint = CapabilityStatementEndpoint {
    capabilityStatementEndpointAttrId :: Maybe Text
  , capabilityStatementEndpointExtension :: [Extension]
  , capabilityStatementEndpointModifierExtension :: [Extension]
  , capabilityStatementEndpointProtocol :: Coding
  , capabilityStatementEndpointAddress :: Url
  } deriving (Eq, Show)
--

instance ToJSON CapabilityStatementEndpoint where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (capabilityStatementEndpointAttrId p)
    ,  "extension" .= toJSON (capabilityStatementEndpointExtension p)
    ,  "modifierExtension" .= toJSON (capabilityStatementEndpointModifierExtension p)
    ,  "protocol" .= toJSON (capabilityStatementEndpointProtocol p)
    ,  "address" .= toJSON (capabilityStatementEndpointAddress p)
    ]
instance FromJSON CapabilityStatementEndpoint where
  parseJSON = withObject "CapabilityStatementEndpoint" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        protocol <- o .:  "protocol"
        address <- o .:  "address"
        return CapabilityStatementEndpoint{
            capabilityStatementEndpointAttrId = id
          , capabilityStatementEndpointExtension = extension
          , capabilityStatementEndpointModifierExtension = modifierExtension
          , capabilityStatementEndpointProtocol = protocol
          , capabilityStatementEndpointAddress = address
          }
instance Xmlbf.ToXml CapabilityStatementEndpoint where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (capabilityStatementEndpointAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (capabilityStatementEndpointExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (capabilityStatementEndpointModifierExtension p))
             , Prop     "protocol" (HM.empty, Xmlbf.toXml (capabilityStatementEndpointProtocol p))
             , Val      "address" (     toUrl (capabilityStatementEndpointAddress p))
             ]
instance Xmlbf.FromXml CapabilityStatementEndpoint where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    protocol <-            Xmlbf.pElement "protocol" Xmlbf.fromXml
    address <-            Xmlbf.pElement "address" (Xmlbf.pAttr "value")
    return CapabilityStatementEndpoint {
            capabilityStatementEndpointAttrId = id
          , capabilityStatementEndpointExtension = extension
          , capabilityStatementEndpointModifierExtension = modifierExtension
          , capabilityStatementEndpointProtocol = protocol
          , capabilityStatementEndpointAddress =      fromUrl address
          }



data CapabilityStatementInteraction1Code
    = CSI1CTransaction
    | CSI1CBatch
    | CSI1CSearchSystem
    | CSI1CHistorySystem
  deriving (Eq, Show)

instance ToJSON CapabilityStatementInteraction1Code where
    toJSON CSI1CTransaction = String "transaction"
    toJSON CSI1CBatch = String "batch"
    toJSON CSI1CSearchSystem = String "search-system"
    toJSON CSI1CHistorySystem = String "history-system"
instance FromJSON CapabilityStatementInteraction1Code where
    parseJSON "transaction" = return CSI1CTransaction
    parseJSON "batch" = return CSI1CBatch
    parseJSON "search-system" = return CSI1CSearchSystem
    parseJSON "history-system" = return CSI1CHistorySystem

toCapabilityStatementInteraction1Code CSI1CTransaction = "transaction"
toCapabilityStatementInteraction1Code CSI1CBatch = "batch"
toCapabilityStatementInteraction1Code CSI1CSearchSystem = "search-system"
toCapabilityStatementInteraction1Code CSI1CHistorySystem = "history-system"
fromCapabilityStatementInteraction1Code "transaction" = CSI1CTransaction
fromCapabilityStatementInteraction1Code "batch" = CSI1CBatch
fromCapabilityStatementInteraction1Code "search-system" = CSI1CSearchSystem
fromCapabilityStatementInteraction1Code "history-system" = CSI1CHistorySystem


data CapabilityStatementInteraction1 = CapabilityStatementInteraction1 {
    capabilityStatementInteraction1AttrId :: Maybe Text
  , capabilityStatementInteraction1Extension :: [Extension]
  , capabilityStatementInteraction1ModifierExtension :: [Extension]
  , capabilityStatementInteraction1Code :: CapabilityStatementInteraction1Code
  , capabilityStatementInteraction1Documentation :: Maybe Markdown
  } deriving (Eq, Show)
--

instance ToJSON CapabilityStatementInteraction1 where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (capabilityStatementInteraction1AttrId p)
    ,  "extension" .= toJSON (capabilityStatementInteraction1Extension p)
    ,  "modifierExtension" .= toJSON (capabilityStatementInteraction1ModifierExtension p)
    ,  "code" .= toJSON (capabilityStatementInteraction1Code p)
    ,  "documentation" .= toJSON (capabilityStatementInteraction1Documentation p)
    ]
instance FromJSON CapabilityStatementInteraction1 where
  parseJSON = withObject "CapabilityStatementInteraction1" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:  "code"
        documentation <- o .:? "documentation"
        return CapabilityStatementInteraction1{
            capabilityStatementInteraction1AttrId = id
          , capabilityStatementInteraction1Extension = extension
          , capabilityStatementInteraction1ModifierExtension = modifierExtension
          , capabilityStatementInteraction1Code = code
          , capabilityStatementInteraction1Documentation = documentation
          }
instance Xmlbf.ToXml CapabilityStatementInteraction1 where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (capabilityStatementInteraction1AttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (capabilityStatementInteraction1Extension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (capabilityStatementInteraction1ModifierExtension p))
             , Val      "code" (     toCapabilityStatementInteraction1Code (capabilityStatementInteraction1Code p))
             , OptVal   "documentation" (fmap toMarkdown (capabilityStatementInteraction1Documentation p))
             ]
instance Xmlbf.FromXml CapabilityStatementInteraction1 where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" (Xmlbf.pAttr "value")
    documentation <- optional $ Xmlbf.pElement "documentation" (Xmlbf.pAttr "value")
    return CapabilityStatementInteraction1 {
            capabilityStatementInteraction1AttrId = id
          , capabilityStatementInteraction1Extension = extension
          , capabilityStatementInteraction1ModifierExtension = modifierExtension
          , capabilityStatementInteraction1Code =      fromCapabilityStatementInteraction1Code code
          , capabilityStatementInteraction1Documentation = fmap fromMarkdown documentation
          }



data CapabilityStatementOperation = CapabilityStatementOperation {
    capabilityStatementOperationAttrId :: Maybe Text
  , capabilityStatementOperationExtension :: [Extension]
  , capabilityStatementOperationModifierExtension :: [Extension]
  , capabilityStatementOperationName :: Text
  , capabilityStatementOperationDefinition :: Canonical
  , capabilityStatementOperationDocumentation :: Maybe Markdown
  } deriving (Eq, Show)
--

instance ToJSON CapabilityStatementOperation where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (capabilityStatementOperationAttrId p)
    ,  "extension" .= toJSON (capabilityStatementOperationExtension p)
    ,  "modifierExtension" .= toJSON (capabilityStatementOperationModifierExtension p)
    ,  "name" .= toJSON (capabilityStatementOperationName p)
    ,  "definition" .= toJSON (capabilityStatementOperationDefinition p)
    ,  "documentation" .= toJSON (capabilityStatementOperationDocumentation p)
    ]
instance FromJSON CapabilityStatementOperation where
  parseJSON = withObject "CapabilityStatementOperation" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        name <- o .:  "name"
        definition <- o .:  "definition"
        documentation <- o .:? "documentation"
        return CapabilityStatementOperation{
            capabilityStatementOperationAttrId = id
          , capabilityStatementOperationExtension = extension
          , capabilityStatementOperationModifierExtension = modifierExtension
          , capabilityStatementOperationName = name
          , capabilityStatementOperationDefinition = definition
          , capabilityStatementOperationDocumentation = documentation
          }
instance Xmlbf.ToXml CapabilityStatementOperation where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (capabilityStatementOperationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (capabilityStatementOperationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (capabilityStatementOperationModifierExtension p))
             , Val      "name" (     toString (capabilityStatementOperationName p))
             , Val      "definition" (     toCanonical (capabilityStatementOperationDefinition p))
             , OptVal   "documentation" (fmap toMarkdown (capabilityStatementOperationDocumentation p))
             ]
instance Xmlbf.FromXml CapabilityStatementOperation where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    name <-            Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    definition <-            Xmlbf.pElement "definition" (Xmlbf.pAttr "value")
    documentation <- optional $ Xmlbf.pElement "documentation" (Xmlbf.pAttr "value")
    return CapabilityStatementOperation {
            capabilityStatementOperationAttrId = id
          , capabilityStatementOperationExtension = extension
          , capabilityStatementOperationModifierExtension = modifierExtension
          , capabilityStatementOperationName =      fromString name
          , capabilityStatementOperationDefinition =      fromCanonical definition
          , capabilityStatementOperationDocumentation = fmap fromMarkdown documentation
          }



data CapabilityStatementSecurity = CapabilityStatementSecurity {
    capabilityStatementSecurityAttrId :: Maybe Text
  , capabilityStatementSecurityExtension :: [Extension]
  , capabilityStatementSecurityModifierExtension :: [Extension]
  , capabilityStatementSecurityCors :: Maybe Boolean
  , capabilityStatementSecurityService :: [CodeableConcept]
  , capabilityStatementSecurityDescription :: Maybe Markdown
  } deriving (Eq, Show)
--

instance ToJSON CapabilityStatementSecurity where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (capabilityStatementSecurityAttrId p)
    ,  "extension" .= toJSON (capabilityStatementSecurityExtension p)
    ,  "modifierExtension" .= toJSON (capabilityStatementSecurityModifierExtension p)
    ,  "cors" .= toJSON (capabilityStatementSecurityCors p)
    ,  "service" .= toJSON (capabilityStatementSecurityService p)
    ,  "description" .= toJSON (capabilityStatementSecurityDescription p)
    ]
instance FromJSON CapabilityStatementSecurity where
  parseJSON = withObject "CapabilityStatementSecurity" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        cors <- o .:? "cors"
        service <- o .:? "service" .!= []
        description <- o .:? "description"
        return CapabilityStatementSecurity{
            capabilityStatementSecurityAttrId = id
          , capabilityStatementSecurityExtension = extension
          , capabilityStatementSecurityModifierExtension = modifierExtension
          , capabilityStatementSecurityCors = cors
          , capabilityStatementSecurityService = service
          , capabilityStatementSecurityDescription = description
          }
instance Xmlbf.ToXml CapabilityStatementSecurity where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (capabilityStatementSecurityAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (capabilityStatementSecurityExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (capabilityStatementSecurityModifierExtension p))
             , OptVal   "cors" (fmap toBoolean (capabilityStatementSecurityCors p))
             , PropList "service" (fmap Xmlbf.toXml (capabilityStatementSecurityService p))
             , OptVal   "description" (fmap toMarkdown (capabilityStatementSecurityDescription p))
             ]
instance Xmlbf.FromXml CapabilityStatementSecurity where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    cors <- optional $ Xmlbf.pElement "cors" (Xmlbf.pAttr "value")
    service <- many     $ Xmlbf.pElement "service" Xmlbf.fromXml
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    return CapabilityStatementSecurity {
            capabilityStatementSecurityAttrId = id
          , capabilityStatementSecurityExtension = extension
          , capabilityStatementSecurityModifierExtension = modifierExtension
          , capabilityStatementSecurityCors = fmap fromBoolean cors
          , capabilityStatementSecurityService = service
          , capabilityStatementSecurityDescription = fmap fromMarkdown description
          }



data CapabilityStatementInteractionCode
    = CSICRead
    | CSICVread
    | CSICUpdate
    | CSICPatch
    | CSICDelete
    | CSICHistoryInstance
    | CSICHistoryType
    | CSICCreate
    | CSICSearchType
  deriving (Eq, Show)

instance ToJSON CapabilityStatementInteractionCode where
    toJSON CSICRead = String "read"
    toJSON CSICVread = String "vread"
    toJSON CSICUpdate = String "update"
    toJSON CSICPatch = String "patch"
    toJSON CSICDelete = String "delete"
    toJSON CSICHistoryInstance = String "history-instance"
    toJSON CSICHistoryType = String "history-type"
    toJSON CSICCreate = String "create"
    toJSON CSICSearchType = String "search-type"
instance FromJSON CapabilityStatementInteractionCode where
    parseJSON "read" = return CSICRead
    parseJSON "vread" = return CSICVread
    parseJSON "update" = return CSICUpdate
    parseJSON "patch" = return CSICPatch
    parseJSON "delete" = return CSICDelete
    parseJSON "history-instance" = return CSICHistoryInstance
    parseJSON "history-type" = return CSICHistoryType
    parseJSON "create" = return CSICCreate
    parseJSON "search-type" = return CSICSearchType

toCapabilityStatementInteractionCode CSICRead = "read"
toCapabilityStatementInteractionCode CSICVread = "vread"
toCapabilityStatementInteractionCode CSICUpdate = "update"
toCapabilityStatementInteractionCode CSICPatch = "patch"
toCapabilityStatementInteractionCode CSICDelete = "delete"
toCapabilityStatementInteractionCode CSICHistoryInstance = "history-instance"
toCapabilityStatementInteractionCode CSICHistoryType = "history-type"
toCapabilityStatementInteractionCode CSICCreate = "create"
toCapabilityStatementInteractionCode CSICSearchType = "search-type"
fromCapabilityStatementInteractionCode "read" = CSICRead
fromCapabilityStatementInteractionCode "vread" = CSICVread
fromCapabilityStatementInteractionCode "update" = CSICUpdate
fromCapabilityStatementInteractionCode "patch" = CSICPatch
fromCapabilityStatementInteractionCode "delete" = CSICDelete
fromCapabilityStatementInteractionCode "history-instance" = CSICHistoryInstance
fromCapabilityStatementInteractionCode "history-type" = CSICHistoryType
fromCapabilityStatementInteractionCode "create" = CSICCreate
fromCapabilityStatementInteractionCode "search-type" = CSICSearchType


data CapabilityStatementInteraction = CapabilityStatementInteraction {
    capabilityStatementInteractionAttrId :: Maybe Text
  , capabilityStatementInteractionExtension :: [Extension]
  , capabilityStatementInteractionModifierExtension :: [Extension]
  , capabilityStatementInteractionCode :: CapabilityStatementInteractionCode
  , capabilityStatementInteractionDocumentation :: Maybe Markdown
  } deriving (Eq, Show)
--

instance ToJSON CapabilityStatementInteraction where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (capabilityStatementInteractionAttrId p)
    ,  "extension" .= toJSON (capabilityStatementInteractionExtension p)
    ,  "modifierExtension" .= toJSON (capabilityStatementInteractionModifierExtension p)
    ,  "code" .= toJSON (capabilityStatementInteractionCode p)
    ,  "documentation" .= toJSON (capabilityStatementInteractionDocumentation p)
    ]
instance FromJSON CapabilityStatementInteraction where
  parseJSON = withObject "CapabilityStatementInteraction" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:  "code"
        documentation <- o .:? "documentation"
        return CapabilityStatementInteraction{
            capabilityStatementInteractionAttrId = id
          , capabilityStatementInteractionExtension = extension
          , capabilityStatementInteractionModifierExtension = modifierExtension
          , capabilityStatementInteractionCode = code
          , capabilityStatementInteractionDocumentation = documentation
          }
instance Xmlbf.ToXml CapabilityStatementInteraction where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (capabilityStatementInteractionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (capabilityStatementInteractionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (capabilityStatementInteractionModifierExtension p))
             , Val      "code" (     toCapabilityStatementInteractionCode (capabilityStatementInteractionCode p))
             , OptVal   "documentation" (fmap toMarkdown (capabilityStatementInteractionDocumentation p))
             ]
instance Xmlbf.FromXml CapabilityStatementInteraction where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" (Xmlbf.pAttr "value")
    documentation <- optional $ Xmlbf.pElement "documentation" (Xmlbf.pAttr "value")
    return CapabilityStatementInteraction {
            capabilityStatementInteractionAttrId = id
          , capabilityStatementInteractionExtension = extension
          , capabilityStatementInteractionModifierExtension = modifierExtension
          , capabilityStatementInteractionCode =      fromCapabilityStatementInteractionCode code
          , capabilityStatementInteractionDocumentation = fmap fromMarkdown documentation
          }



data CapabilityStatementSearchParamType
    = CSSPTNumber
    | CSSPTDate
    | CSSPTString
    | CSSPTToken
    | CSSPTReference
    | CSSPTComposite
    | CSSPTQuantity
    | CSSPTUri
    | CSSPTSpecial
  deriving (Eq, Show)

instance ToJSON CapabilityStatementSearchParamType where
    toJSON CSSPTNumber = String "number"
    toJSON CSSPTDate = String "date"
    toJSON CSSPTString = String "string"
    toJSON CSSPTToken = String "token"
    toJSON CSSPTReference = String "reference"
    toJSON CSSPTComposite = String "composite"
    toJSON CSSPTQuantity = String "quantity"
    toJSON CSSPTUri = String "uri"
    toJSON CSSPTSpecial = String "special"
instance FromJSON CapabilityStatementSearchParamType where
    parseJSON "number" = return CSSPTNumber
    parseJSON "date" = return CSSPTDate
    parseJSON "string" = return CSSPTString
    parseJSON "token" = return CSSPTToken
    parseJSON "reference" = return CSSPTReference
    parseJSON "composite" = return CSSPTComposite
    parseJSON "quantity" = return CSSPTQuantity
    parseJSON "uri" = return CSSPTUri
    parseJSON "special" = return CSSPTSpecial

toCapabilityStatementSearchParamType CSSPTNumber = "number"
toCapabilityStatementSearchParamType CSSPTDate = "date"
toCapabilityStatementSearchParamType CSSPTString = "string"
toCapabilityStatementSearchParamType CSSPTToken = "token"
toCapabilityStatementSearchParamType CSSPTReference = "reference"
toCapabilityStatementSearchParamType CSSPTComposite = "composite"
toCapabilityStatementSearchParamType CSSPTQuantity = "quantity"
toCapabilityStatementSearchParamType CSSPTUri = "uri"
toCapabilityStatementSearchParamType CSSPTSpecial = "special"
fromCapabilityStatementSearchParamType "number" = CSSPTNumber
fromCapabilityStatementSearchParamType "date" = CSSPTDate
fromCapabilityStatementSearchParamType "string" = CSSPTString
fromCapabilityStatementSearchParamType "token" = CSSPTToken
fromCapabilityStatementSearchParamType "reference" = CSSPTReference
fromCapabilityStatementSearchParamType "composite" = CSSPTComposite
fromCapabilityStatementSearchParamType "quantity" = CSSPTQuantity
fromCapabilityStatementSearchParamType "uri" = CSSPTUri
fromCapabilityStatementSearchParamType "special" = CSSPTSpecial


data CapabilityStatementSearchParam = CapabilityStatementSearchParam {
    capabilityStatementSearchParamAttrId :: Maybe Text
  , capabilityStatementSearchParamExtension :: [Extension]
  , capabilityStatementSearchParamModifierExtension :: [Extension]
  , capabilityStatementSearchParamName :: Text
  , capabilityStatementSearchParamDefinition :: Maybe Canonical
  , capabilityStatementSearchParamType :: CapabilityStatementSearchParamType
  , capabilityStatementSearchParamDocumentation :: Maybe Markdown
  } deriving (Eq, Show)
--

instance ToJSON CapabilityStatementSearchParam where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (capabilityStatementSearchParamAttrId p)
    ,  "extension" .= toJSON (capabilityStatementSearchParamExtension p)
    ,  "modifierExtension" .= toJSON (capabilityStatementSearchParamModifierExtension p)
    ,  "name" .= toJSON (capabilityStatementSearchParamName p)
    ,  "definition" .= toJSON (capabilityStatementSearchParamDefinition p)
    ,  "type" .= toJSON (capabilityStatementSearchParamType p)
    ,  "documentation" .= toJSON (capabilityStatementSearchParamDocumentation p)
    ]
instance FromJSON CapabilityStatementSearchParam where
  parseJSON = withObject "CapabilityStatementSearchParam" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        name <- o .:  "name"
        definition <- o .:? "definition"
        ty <- o .:  "type"
        documentation <- o .:? "documentation"
        return CapabilityStatementSearchParam{
            capabilityStatementSearchParamAttrId = id
          , capabilityStatementSearchParamExtension = extension
          , capabilityStatementSearchParamModifierExtension = modifierExtension
          , capabilityStatementSearchParamName = name
          , capabilityStatementSearchParamDefinition = definition
          , capabilityStatementSearchParamType = ty
          , capabilityStatementSearchParamDocumentation = documentation
          }
instance Xmlbf.ToXml CapabilityStatementSearchParam where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (capabilityStatementSearchParamAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (capabilityStatementSearchParamExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (capabilityStatementSearchParamModifierExtension p))
             , Val      "name" (     toString (capabilityStatementSearchParamName p))
             , OptVal   "definition" (fmap toCanonical (capabilityStatementSearchParamDefinition p))
             , Val      "type" (     toCapabilityStatementSearchParamType (capabilityStatementSearchParamType p))
             , OptVal   "documentation" (fmap toMarkdown (capabilityStatementSearchParamDocumentation p))
             ]
instance Xmlbf.FromXml CapabilityStatementSearchParam where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    name <-            Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    definition <- optional $ Xmlbf.pElement "definition" (Xmlbf.pAttr "value")
    ty <-            Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    documentation <- optional $ Xmlbf.pElement "documentation" (Xmlbf.pAttr "value")
    return CapabilityStatementSearchParam {
            capabilityStatementSearchParamAttrId = id
          , capabilityStatementSearchParamExtension = extension
          , capabilityStatementSearchParamModifierExtension = modifierExtension
          , capabilityStatementSearchParamName =      fromString name
          , capabilityStatementSearchParamDefinition = fmap fromCanonical definition
          , capabilityStatementSearchParamType =      fromCapabilityStatementSearchParamType ty
          , capabilityStatementSearchParamDocumentation = fmap fromMarkdown documentation
          }



data CapabilityStatementResourceVersioning
    = CSRVNoVersion
    | CSRVVersioned
    | CSRVVersionedUpdate
  deriving (Eq, Show)

instance ToJSON CapabilityStatementResourceVersioning where
    toJSON CSRVNoVersion = String "no-version"
    toJSON CSRVVersioned = String "versioned"
    toJSON CSRVVersionedUpdate = String "versioned-update"
instance FromJSON CapabilityStatementResourceVersioning where
    parseJSON "no-version" = return CSRVNoVersion
    parseJSON "versioned" = return CSRVVersioned
    parseJSON "versioned-update" = return CSRVVersionedUpdate

toCapabilityStatementResourceVersioning CSRVNoVersion = "no-version"
toCapabilityStatementResourceVersioning CSRVVersioned = "versioned"
toCapabilityStatementResourceVersioning CSRVVersionedUpdate = "versioned-update"
fromCapabilityStatementResourceVersioning "no-version" = CSRVNoVersion
fromCapabilityStatementResourceVersioning "versioned" = CSRVVersioned
fromCapabilityStatementResourceVersioning "versioned-update" = CSRVVersionedUpdate


data CapabilityStatementResourceConditionalRead
    = CSRCRNotSupported
    | CSRCRModifiedSince
    | CSRCRNotMatch
    | CSRCRFullSupport
  deriving (Eq, Show)

instance ToJSON CapabilityStatementResourceConditionalRead where
    toJSON CSRCRNotSupported = String "not-supported"
    toJSON CSRCRModifiedSince = String "modified-since"
    toJSON CSRCRNotMatch = String "not-match"
    toJSON CSRCRFullSupport = String "full-support"
instance FromJSON CapabilityStatementResourceConditionalRead where
    parseJSON "not-supported" = return CSRCRNotSupported
    parseJSON "modified-since" = return CSRCRModifiedSince
    parseJSON "not-match" = return CSRCRNotMatch
    parseJSON "full-support" = return CSRCRFullSupport

toCapabilityStatementResourceConditionalRead CSRCRNotSupported = "not-supported"
toCapabilityStatementResourceConditionalRead CSRCRModifiedSince = "modified-since"
toCapabilityStatementResourceConditionalRead CSRCRNotMatch = "not-match"
toCapabilityStatementResourceConditionalRead CSRCRFullSupport = "full-support"
fromCapabilityStatementResourceConditionalRead "not-supported" = CSRCRNotSupported
fromCapabilityStatementResourceConditionalRead "modified-since" = CSRCRModifiedSince
fromCapabilityStatementResourceConditionalRead "not-match" = CSRCRNotMatch
fromCapabilityStatementResourceConditionalRead "full-support" = CSRCRFullSupport


data CapabilityStatementResourceConditionalDelete
    = CSRCDNotSupported
    | CSRCDSingle
    | CSRCDMultiple
  deriving (Eq, Show)

instance ToJSON CapabilityStatementResourceConditionalDelete where
    toJSON CSRCDNotSupported = String "not-supported"
    toJSON CSRCDSingle = String "single"
    toJSON CSRCDMultiple = String "multiple"
instance FromJSON CapabilityStatementResourceConditionalDelete where
    parseJSON "not-supported" = return CSRCDNotSupported
    parseJSON "single" = return CSRCDSingle
    parseJSON "multiple" = return CSRCDMultiple

toCapabilityStatementResourceConditionalDelete CSRCDNotSupported = "not-supported"
toCapabilityStatementResourceConditionalDelete CSRCDSingle = "single"
toCapabilityStatementResourceConditionalDelete CSRCDMultiple = "multiple"
fromCapabilityStatementResourceConditionalDelete "not-supported" = CSRCDNotSupported
fromCapabilityStatementResourceConditionalDelete "single" = CSRCDSingle
fromCapabilityStatementResourceConditionalDelete "multiple" = CSRCDMultiple


data CapabilityStatementResourceReferencePolicy
    = CSRRPLiteral
    | CSRRPLogical
    | CSRRPResolves
    | CSRRPEnforced
    | CSRRPLocal
  deriving (Eq, Show)

instance ToJSON CapabilityStatementResourceReferencePolicy where
    toJSON CSRRPLiteral = String "literal"
    toJSON CSRRPLogical = String "logical"
    toJSON CSRRPResolves = String "resolves"
    toJSON CSRRPEnforced = String "enforced"
    toJSON CSRRPLocal = String "local"
instance FromJSON CapabilityStatementResourceReferencePolicy where
    parseJSON "literal" = return CSRRPLiteral
    parseJSON "logical" = return CSRRPLogical
    parseJSON "resolves" = return CSRRPResolves
    parseJSON "enforced" = return CSRRPEnforced
    parseJSON "local" = return CSRRPLocal

toCapabilityStatementResourceReferencePolicy CSRRPLiteral = "literal"
toCapabilityStatementResourceReferencePolicy CSRRPLogical = "logical"
toCapabilityStatementResourceReferencePolicy CSRRPResolves = "resolves"
toCapabilityStatementResourceReferencePolicy CSRRPEnforced = "enforced"
toCapabilityStatementResourceReferencePolicy CSRRPLocal = "local"
fromCapabilityStatementResourceReferencePolicy "literal" = CSRRPLiteral
fromCapabilityStatementResourceReferencePolicy "logical" = CSRRPLogical
fromCapabilityStatementResourceReferencePolicy "resolves" = CSRRPResolves
fromCapabilityStatementResourceReferencePolicy "enforced" = CSRRPEnforced
fromCapabilityStatementResourceReferencePolicy "local" = CSRRPLocal


data CapabilityStatementResource = CapabilityStatementResource {
    capabilityStatementResourceAttrId :: Maybe Text
  , capabilityStatementResourceExtension :: [Extension]
  , capabilityStatementResourceModifierExtension :: [Extension]
  , capabilityStatementResourceType :: Code
  , capabilityStatementResourceProfile :: Maybe Canonical
  , capabilityStatementResourceSupportedProfile :: [Canonical]
  , capabilityStatementResourceDocumentation :: Maybe Markdown
  , capabilityStatementResourceInteraction :: [CapabilityStatementInteraction]
  , capabilityStatementResourceVersioning :: Maybe CapabilityStatementResourceVersioning
  , capabilityStatementResourceReadHistory :: Maybe Boolean
  , capabilityStatementResourceUpdateCreate :: Maybe Boolean
  , capabilityStatementResourceConditionalCreate :: Maybe Boolean
  , capabilityStatementResourceConditionalRead :: Maybe CapabilityStatementResourceConditionalRead
  , capabilityStatementResourceConditionalUpdate :: Maybe Boolean
  , capabilityStatementResourceConditionalDelete :: Maybe CapabilityStatementResourceConditionalDelete
  , capabilityStatementResourceReferencePolicy :: [CapabilityStatementResourceReferencePolicy]
  , capabilityStatementResourceSearchInclude :: [Text]
  , capabilityStatementResourceSearchRevInclude :: [Text]
  , capabilityStatementResourceSearchParam :: [CapabilityStatementSearchParam]
  , capabilityStatementResourceOperation :: [CapabilityStatementOperation]
  } deriving (Eq, Show)
--

instance ToJSON CapabilityStatementResource where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (capabilityStatementResourceAttrId p)
    ,  "extension" .= toJSON (capabilityStatementResourceExtension p)
    ,  "modifierExtension" .= toJSON (capabilityStatementResourceModifierExtension p)
    ,  "type" .= toJSON (capabilityStatementResourceType p)
    ,  "profile" .= toJSON (capabilityStatementResourceProfile p)
    ,  "supportedProfile" .= toJSON (capabilityStatementResourceSupportedProfile p)
    ,  "documentation" .= toJSON (capabilityStatementResourceDocumentation p)
    ,  "interaction" .= toJSON (capabilityStatementResourceInteraction p)
    ,  "versioning" .= toJSON (capabilityStatementResourceVersioning p)
    ,  "readHistory" .= toJSON (capabilityStatementResourceReadHistory p)
    ,  "updateCreate" .= toJSON (capabilityStatementResourceUpdateCreate p)
    ,  "conditionalCreate" .= toJSON (capabilityStatementResourceConditionalCreate p)
    ,  "conditionalRead" .= toJSON (capabilityStatementResourceConditionalRead p)
    ,  "conditionalUpdate" .= toJSON (capabilityStatementResourceConditionalUpdate p)
    ,  "conditionalDelete" .= toJSON (capabilityStatementResourceConditionalDelete p)
    ,  "referencePolicy" .= toJSON (capabilityStatementResourceReferencePolicy p)
    ,  "searchInclude" .= toJSON (capabilityStatementResourceSearchInclude p)
    ,  "searchRevInclude" .= toJSON (capabilityStatementResourceSearchRevInclude p)
    ,  "searchParam" .= toJSON (capabilityStatementResourceSearchParam p)
    ,  "operation" .= toJSON (capabilityStatementResourceOperation p)
    ]
instance FromJSON CapabilityStatementResource where
  parseJSON = withObject "CapabilityStatementResource" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:  "type"
        profile <- o .:? "profile"
        supportedProfile <- o .:? "supportedProfile" .!= []
        documentation <- o .:? "documentation"
        interaction <- o .:? "interaction" .!= []
        versioning <- o .:? "versioning"
        readHistory <- o .:? "readHistory"
        updateCreate <- o .:? "updateCreate"
        conditionalCreate <- o .:? "conditionalCreate"
        conditionalRead <- o .:? "conditionalRead"
        conditionalUpdate <- o .:? "conditionalUpdate"
        conditionalDelete <- o .:? "conditionalDelete"
        referencePolicy <- o .:? "referencePolicy" .!= []
        searchInclude <- o .:? "searchInclude" .!= []
        searchRevInclude <- o .:? "searchRevInclude" .!= []
        searchParam <- o .:? "searchParam" .!= []
        operation <- o .:? "operation" .!= []
        return CapabilityStatementResource{
            capabilityStatementResourceAttrId = id
          , capabilityStatementResourceExtension = extension
          , capabilityStatementResourceModifierExtension = modifierExtension
          , capabilityStatementResourceType = ty
          , capabilityStatementResourceProfile = profile
          , capabilityStatementResourceSupportedProfile = supportedProfile
          , capabilityStatementResourceDocumentation = documentation
          , capabilityStatementResourceInteraction = interaction
          , capabilityStatementResourceVersioning = versioning
          , capabilityStatementResourceReadHistory = readHistory
          , capabilityStatementResourceUpdateCreate = updateCreate
          , capabilityStatementResourceConditionalCreate = conditionalCreate
          , capabilityStatementResourceConditionalRead = conditionalRead
          , capabilityStatementResourceConditionalUpdate = conditionalUpdate
          , capabilityStatementResourceConditionalDelete = conditionalDelete
          , capabilityStatementResourceReferencePolicy = referencePolicy
          , capabilityStatementResourceSearchInclude = searchInclude
          , capabilityStatementResourceSearchRevInclude = searchRevInclude
          , capabilityStatementResourceSearchParam = searchParam
          , capabilityStatementResourceOperation = operation
          }
instance Xmlbf.ToXml CapabilityStatementResource where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (capabilityStatementResourceAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (capabilityStatementResourceExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (capabilityStatementResourceModifierExtension p))
             , Val      "type" (     toCode (capabilityStatementResourceType p))
             , OptVal   "profile" (fmap toCanonical (capabilityStatementResourceProfile p))
             , ValList  "supportedProfile" (fmap toCanonical (capabilityStatementResourceSupportedProfile p))
             , OptVal   "documentation" (fmap toMarkdown (capabilityStatementResourceDocumentation p))
             , PropList "interaction" (fmap Xmlbf.toXml (capabilityStatementResourceInteraction p))
             , OptVal   "versioning" (fmap toCapabilityStatementResourceVersioning (capabilityStatementResourceVersioning p))
             , OptVal   "readHistory" (fmap toBoolean (capabilityStatementResourceReadHistory p))
             , OptVal   "updateCreate" (fmap toBoolean (capabilityStatementResourceUpdateCreate p))
             , OptVal   "conditionalCreate" (fmap toBoolean (capabilityStatementResourceConditionalCreate p))
             , OptVal   "conditionalRead" (fmap toCapabilityStatementResourceConditionalRead (capabilityStatementResourceConditionalRead p))
             , OptVal   "conditionalUpdate" (fmap toBoolean (capabilityStatementResourceConditionalUpdate p))
             , OptVal   "conditionalDelete" (fmap toCapabilityStatementResourceConditionalDelete (capabilityStatementResourceConditionalDelete p))
             , ValList  "referencePolicy" (fmap toCapabilityStatementResourceReferencePolicy (capabilityStatementResourceReferencePolicy p))
             , ValList  "searchInclude" (fmap toString (capabilityStatementResourceSearchInclude p))
             , ValList  "searchRevInclude" (fmap toString (capabilityStatementResourceSearchRevInclude p))
             , PropList "searchParam" (fmap Xmlbf.toXml (capabilityStatementResourceSearchParam p))
             , PropList "operation" (fmap Xmlbf.toXml (capabilityStatementResourceOperation p))
             ]
instance Xmlbf.FromXml CapabilityStatementResource where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    profile <- optional $ Xmlbf.pElement "profile" (Xmlbf.pAttr "value")
    supportedProfile <- many     $ Xmlbf.pElement "supportedProfile" (Xmlbf.pAttr "value")
    documentation <- optional $ Xmlbf.pElement "documentation" (Xmlbf.pAttr "value")
    interaction <- many     $ Xmlbf.pElement "interaction" Xmlbf.fromXml
    versioning <- optional $ Xmlbf.pElement "versioning" (Xmlbf.pAttr "value")
    readHistory <- optional $ Xmlbf.pElement "readHistory" (Xmlbf.pAttr "value")
    updateCreate <- optional $ Xmlbf.pElement "updateCreate" (Xmlbf.pAttr "value")
    conditionalCreate <- optional $ Xmlbf.pElement "conditionalCreate" (Xmlbf.pAttr "value")
    conditionalRead <- optional $ Xmlbf.pElement "conditionalRead" (Xmlbf.pAttr "value")
    conditionalUpdate <- optional $ Xmlbf.pElement "conditionalUpdate" (Xmlbf.pAttr "value")
    conditionalDelete <- optional $ Xmlbf.pElement "conditionalDelete" (Xmlbf.pAttr "value")
    referencePolicy <- many     $ Xmlbf.pElement "referencePolicy" (Xmlbf.pAttr "value")
    searchInclude <- many     $ Xmlbf.pElement "searchInclude" (Xmlbf.pAttr "value")
    searchRevInclude <- many     $ Xmlbf.pElement "searchRevInclude" (Xmlbf.pAttr "value")
    searchParam <- many     $ Xmlbf.pElement "searchParam" Xmlbf.fromXml
    operation <- many     $ Xmlbf.pElement "operation" Xmlbf.fromXml
    return CapabilityStatementResource {
            capabilityStatementResourceAttrId = id
          , capabilityStatementResourceExtension = extension
          , capabilityStatementResourceModifierExtension = modifierExtension
          , capabilityStatementResourceType =      fromCode ty
          , capabilityStatementResourceProfile = fmap fromCanonical profile
          , capabilityStatementResourceSupportedProfile = fmap fromCanonical supportedProfile
          , capabilityStatementResourceDocumentation = fmap fromMarkdown documentation
          , capabilityStatementResourceInteraction = interaction
          , capabilityStatementResourceVersioning = fmap fromCapabilityStatementResourceVersioning versioning
          , capabilityStatementResourceReadHistory = fmap fromBoolean readHistory
          , capabilityStatementResourceUpdateCreate = fmap fromBoolean updateCreate
          , capabilityStatementResourceConditionalCreate = fmap fromBoolean conditionalCreate
          , capabilityStatementResourceConditionalRead = fmap fromCapabilityStatementResourceConditionalRead conditionalRead
          , capabilityStatementResourceConditionalUpdate = fmap fromBoolean conditionalUpdate
          , capabilityStatementResourceConditionalDelete = fmap fromCapabilityStatementResourceConditionalDelete conditionalDelete
          , capabilityStatementResourceReferencePolicy = fmap fromCapabilityStatementResourceReferencePolicy referencePolicy
          , capabilityStatementResourceSearchInclude = fmap fromString searchInclude
          , capabilityStatementResourceSearchRevInclude = fmap fromString searchRevInclude
          , capabilityStatementResourceSearchParam = searchParam
          , capabilityStatementResourceOperation = operation
          }




