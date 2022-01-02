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
-- FHIR 4.0.0 Consent
--

module Data.FHIR.Resources.Consent where

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

data ConsentStatus
    = CSDraft
    | CSProposed
    | CSActive
    | CSRejected
    | CSInactive
    | CSEnteredInError
  deriving (Eq, Show)

instance ToJSON ConsentStatus where
    toJSON CSDraft = String "draft"
    toJSON CSProposed = String "proposed"
    toJSON CSActive = String "active"
    toJSON CSRejected = String "rejected"
    toJSON CSInactive = String "inactive"
    toJSON CSEnteredInError = String "entered-in-error"
instance FromJSON ConsentStatus where
    parseJSON "draft" = return CSDraft
    parseJSON "proposed" = return CSProposed
    parseJSON "active" = return CSActive
    parseJSON "rejected" = return CSRejected
    parseJSON "inactive" = return CSInactive
    parseJSON "entered-in-error" = return CSEnteredInError

toConsentStatus CSDraft = "draft"
toConsentStatus CSProposed = "proposed"
toConsentStatus CSActive = "active"
toConsentStatus CSRejected = "rejected"
toConsentStatus CSInactive = "inactive"
toConsentStatus CSEnteredInError = "entered-in-error"
fromConsentStatus "draft" = CSDraft
fromConsentStatus "proposed" = CSProposed
fromConsentStatus "active" = CSActive
fromConsentStatus "rejected" = CSRejected
fromConsentStatus "inactive" = CSInactive
fromConsentStatus "entered-in-error" = CSEnteredInError


data ConsentSource
    = ConsentSourceAttachment Attachment
    | ConsentSourceReference Reference
    deriving (Eq, Show)

data Consent = Consent {
    consentId :: Maybe Id
  , consentMeta :: Maybe Meta
  , consentImplicitRules :: Maybe Uri
  , consentLanguage :: Maybe Language
  , consentText :: Maybe Narrative
--    consentContained :: [ResourceContainer]
  , consentExtension :: [Extension]
  , consentModifierExtension :: [Extension]
  , consentIdentifier :: [Identifier]
  , consentStatus :: ConsentStatus
  , consentScope :: CodeableConcept
  , consentCategory :: [CodeableConcept]
  , consentPatient :: Maybe Reference
  , consentDateTime :: Maybe DateTime
  , consentPerformer :: [Reference]
  , consentOrganization :: [Reference]
  , consentSource :: Maybe ConsentSource
  , consentPolicy :: [ConsentPolicy]
  , consentPolicyRule :: Maybe CodeableConcept
  , consentVerification :: [ConsentVerification]
  , consentProvision :: Maybe ConsentProvision
  }
--

instance ToJSON Consent where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Consent")
    ,  "id" .= toJSON (consentId p)
    ,  "meta" .= toJSON (consentMeta p)
    ,  "implicitRules" .= toJSON (consentImplicitRules p)
    ,  "language" .= toJSON (consentLanguage p)
    ,  "text" .= toJSON (consentText p)
--    , "contained" .= toJSON (consentContained p)
    ,  "extension" .= toJSON (consentExtension p)
    ,  "modifierExtension" .= toJSON (consentModifierExtension p)
    ,  "identifier" .= toJSON (consentIdentifier p)
    ,  "status" .= toJSON (consentStatus p)
    ,  "scope" .= toJSON (consentScope p)
    ,  "category" .= toJSON (consentCategory p)
    ,  "patient" .= toJSON (consentPatient p)
    ,  "dateTime" .= toJSON (consentDateTime p)
    ,  "performer" .= toJSON (consentPerformer p)
    ,  "organization" .= toJSON (consentOrganization p)
    , toSourceJSON (consentSource p)
    ,  "policy" .= toJSON (consentPolicy p)
    ,  "policyRule" .= toJSON (consentPolicyRule p)
    ,  "verification" .= toJSON (consentVerification p)
    ,  "provision" .= toJSON (consentProvision p)
    ]
    where 
      toSourceJSON (     Nothing   ) = ("source", Null)
      toSourceJSON (Just (ConsentSourceAttachment c)) = ("source", toJSON c)
      toSourceJSON (Just (ConsentSourceReference c)) = ("source", toJSON c)
instance FromJSON Consent where
  parseJSON = withObject "Consent" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Consent" -> do
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
        scope <- o .:  "scope"
        category <- o .:? "category" .!= []
        patient <- o .:? "patient"
        dateTime <- o .:? "dateTime"
        performer <- o .:? "performer" .!= []
        organization <- o .:? "organization" .!= []
        source <- parseSource o
        policy <- o .:? "policy" .!= []
        policyRule <- o .:? "policyRule"
        verification <- o .:? "verification" .!= []
        provision <- o .:? "provision"
        return Consent{
            consentId = id
          , consentMeta = meta
          , consentImplicitRules = implicitRules
          , consentLanguage = language
          , consentText = text
--          , consentContained = contained
          , consentExtension = extension
          , consentModifierExtension = modifierExtension
          , consentIdentifier = identifier
          , consentStatus = status
          , consentScope = scope
          , consentCategory = category
          , consentPatient = patient
          , consentDateTime = dateTime
          , consentPerformer = performer
          , consentOrganization = organization
          , consentSource = source
          , consentPolicy = policy
          , consentPolicyRule = policyRule
          , consentVerification = verification
          , consentProvision = provision
          }
      _ -> fail "not a Consent"
    where 
      parseSource o = parseSourceAttachment o <|> parseSourceReference o
      parseSourceAttachment o = do
                has <- o .: "sourceAttachment"
                return $ Just (ConsentSourceAttachment has)
      parseSourceReference o = do
                has <- o .: "sourceReference"
                return $ Just (ConsentSourceReference has)
instance Xmlbf.ToXml Consent where
  toXml p = Xmlbf.element "Consent" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (consentId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (consentMeta p))
             , OptVal   "implicitRules" (fmap toUri (consentImplicitRules p))
             , OptVal   "language" (fmap toLanguage (consentLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (consentText p))
--             , PropList "contained" (fmap Xmlbf.toXml (consentContained p))
             , PropList "extension" (fmap Xmlbf.toXml (consentExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (consentModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (consentIdentifier p))
             , Val      "status" (     toConsentStatus (consentStatus p))
             , Prop     "scope" (HM.empty, Xmlbf.toXml (consentScope p))
             , PropList "category" (fmap Xmlbf.toXml (consentCategory p))
             , OptProp  "patient" (fmap Xmlbf.toXml (consentPatient p))
             , OptVal   "dateTime" (fmap toDateTime (consentDateTime p))
             , PropList "performer" (fmap Xmlbf.toXml (consentPerformer p))
             , PropList "organization" (fmap Xmlbf.toXml (consentOrganization p))
             , toSourceXml (consentSource p)
             , PropList "policy" (fmap Xmlbf.toXml (consentPolicy p))
             , OptProp  "policyRule" (fmap Xmlbf.toXml (consentPolicyRule p))
             , PropList "verification" (fmap Xmlbf.toXml (consentVerification p))
             , OptProp  "provision" (fmap Xmlbf.toXml (consentProvision p))
             ]
          toSourceXml ( Nothing   ) = (OptVal "source" Nothing)
          toSourceXml (Just (ConsentSourceAttachment p)) = Prop  "sourceAttachment" (HM.empty, Xmlbf.toXml p)
          toSourceXml (Just (ConsentSourceReference p)) = Prop  "sourceReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml Consent where
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
    scope <-            Xmlbf.pElement "scope" Xmlbf.fromXml
    category <- many     $ Xmlbf.pElement "category" Xmlbf.fromXml
    patient <- optional $ Xmlbf.pElement "patient" Xmlbf.fromXml
    dateTime <- optional $ Xmlbf.pElement "dateTime" (Xmlbf.pAttr "value")
    performer <- many     $ Xmlbf.pElement "performer" Xmlbf.fromXml
    organization <- many     $ Xmlbf.pElement "organization" Xmlbf.fromXml
    source <- fromSourceXml
    policy <- many     $ Xmlbf.pElement "policy" Xmlbf.fromXml
    policyRule <- optional $ Xmlbf.pElement "policyRule" Xmlbf.fromXml
    verification <- many     $ Xmlbf.pElement "verification" Xmlbf.fromXml
    provision <- optional $ Xmlbf.pElement "provision" Xmlbf.fromXml
    return Consent {
            consentId = fmap fromId id
          , consentMeta = meta
          , consentImplicitRules = fmap fromUri implicitRules
          , consentLanguage = fmap fromLanguage language
          , consentText = text
--          , consentContained = contained
          , consentExtension = extension
          , consentModifierExtension = modifierExtension
          , consentIdentifier = identifier
          , consentStatus =      fromConsentStatus status
          , consentScope = scope
          , consentCategory = category
          , consentPatient = patient
          , consentDateTime = fmap fromDateTime dateTime
          , consentPerformer = performer
          , consentOrganization = organization
          , consentSource = source
          , consentPolicy = policy
          , consentPolicyRule = policyRule
          , consentVerification = verification
          , consentProvision = provision
          }

    where 
      fromSourceXml = parseSourceAttachment <|> parseSourceReference <|> pure Nothing
      parseSourceAttachment = do
                has <- Xmlbf.pElement "sourceAttachment" Xmlbf.fromXml
                return $ Just (ConsentSourceAttachment (                      has))
      parseSourceReference = do
                has <- Xmlbf.pElement "sourceReference" Xmlbf.fromXml
                return $ Just (ConsentSourceReference (                      has))


data ConsentProvisionType
    = CPTDeny
    | CPTPermit
  deriving (Eq, Show)

instance ToJSON ConsentProvisionType where
    toJSON CPTDeny = String "deny"
    toJSON CPTPermit = String "permit"
instance FromJSON ConsentProvisionType where
    parseJSON "deny" = return CPTDeny
    parseJSON "permit" = return CPTPermit

toConsentProvisionType CPTDeny = "deny"
toConsentProvisionType CPTPermit = "permit"
fromConsentProvisionType "deny" = CPTDeny
fromConsentProvisionType "permit" = CPTPermit


data ConsentProvision = ConsentProvision {
    consentProvisionAttrId :: Maybe Text
  , consentProvisionExtension :: [Extension]
  , consentProvisionModifierExtension :: [Extension]
  , consentProvisionType :: Maybe ConsentProvisionType
  , consentProvisionPeriod :: Maybe Period
  , consentProvisionActor :: [ConsentActor]
  , consentProvisionAction :: [CodeableConcept]
  , consentProvisionSecurityLabel :: [Coding]
  , consentProvisionPurpose :: [Coding]
  , consentProvisionClass :: [Coding]
  , consentProvisionCode :: [CodeableConcept]
  , consentProvisionDataPeriod :: Maybe Period
  , consentProvisionData :: [ConsentData]
  , consentProvisionProvision :: [ConsentProvision]
  }
--

instance ToJSON ConsentProvision where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (consentProvisionAttrId p)
    ,  "extension" .= toJSON (consentProvisionExtension p)
    ,  "modifierExtension" .= toJSON (consentProvisionModifierExtension p)
    ,  "type" .= toJSON (consentProvisionType p)
    ,  "period" .= toJSON (consentProvisionPeriod p)
    ,  "actor" .= toJSON (consentProvisionActor p)
    ,  "action" .= toJSON (consentProvisionAction p)
    ,  "securityLabel" .= toJSON (consentProvisionSecurityLabel p)
    ,  "purpose" .= toJSON (consentProvisionPurpose p)
    ,  "class" .= toJSON (consentProvisionClass p)
    ,  "code" .= toJSON (consentProvisionCode p)
    ,  "dataPeriod" .= toJSON (consentProvisionDataPeriod p)
    ,  "data" .= toJSON (consentProvisionData p)
    ,  "provision" .= toJSON (consentProvisionProvision p)
    ]
instance FromJSON ConsentProvision where
  parseJSON = withObject "ConsentProvision" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:? "type"
        period <- o .:? "period"
        actor <- o .:? "actor" .!= []
        action <- o .:? "action" .!= []
        securityLabel <- o .:? "securityLabel" .!= []
        purpose <- o .:? "purpose" .!= []
        cl <- o .:? "class" .!= []
        code <- o .:? "code" .!= []
        dataPeriod <- o .:? "dataPeriod"
        dt <- o .:? "data" .!= []
        provision <- o .:? "provision" .!= []
        return ConsentProvision{
            consentProvisionAttrId = id
          , consentProvisionExtension = extension
          , consentProvisionModifierExtension = modifierExtension
          , consentProvisionType = ty
          , consentProvisionPeriod = period
          , consentProvisionActor = actor
          , consentProvisionAction = action
          , consentProvisionSecurityLabel = securityLabel
          , consentProvisionPurpose = purpose
          , consentProvisionClass = cl
          , consentProvisionCode = code
          , consentProvisionDataPeriod = dataPeriod
          , consentProvisionData = dt
          , consentProvisionProvision = provision
          }
instance Xmlbf.ToXml ConsentProvision where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (consentProvisionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (consentProvisionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (consentProvisionModifierExtension p))
             , OptVal   "type" (fmap toConsentProvisionType (consentProvisionType p))
             , OptProp  "period" (fmap Xmlbf.toXml (consentProvisionPeriod p))
             , PropList "actor" (fmap Xmlbf.toXml (consentProvisionActor p))
             , PropList "action" (fmap Xmlbf.toXml (consentProvisionAction p))
             , PropList "securityLabel" (fmap Xmlbf.toXml (consentProvisionSecurityLabel p))
             , PropList "purpose" (fmap Xmlbf.toXml (consentProvisionPurpose p))
             , PropList "class" (fmap Xmlbf.toXml (consentProvisionClass p))
             , PropList "code" (fmap Xmlbf.toXml (consentProvisionCode p))
             , OptProp  "dataPeriod" (fmap Xmlbf.toXml (consentProvisionDataPeriod p))
             , PropList "data" (fmap Xmlbf.toXml (consentProvisionData p))
             , PropList "provision" (fmap Xmlbf.toXml (consentProvisionProvision p))
             ]
instance Xmlbf.FromXml ConsentProvision where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <- optional $ Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    actor <- many     $ Xmlbf.pElement "actor" Xmlbf.fromXml
    action <- many     $ Xmlbf.pElement "action" Xmlbf.fromXml
    securityLabel <- many     $ Xmlbf.pElement "securityLabel" Xmlbf.fromXml
    purpose <- many     $ Xmlbf.pElement "purpose" Xmlbf.fromXml
    cl <- many     $ Xmlbf.pElement "class" Xmlbf.fromXml
    code <- many     $ Xmlbf.pElement "code" Xmlbf.fromXml
    dataPeriod <- optional $ Xmlbf.pElement "dataPeriod" Xmlbf.fromXml
    dt <- many     $ Xmlbf.pElement "data" Xmlbf.fromXml
    provision <- many     $ Xmlbf.pElement "provision" Xmlbf.fromXml
    return ConsentProvision {
            consentProvisionAttrId = id
          , consentProvisionExtension = extension
          , consentProvisionModifierExtension = modifierExtension
          , consentProvisionType = fmap fromConsentProvisionType ty
          , consentProvisionPeriod = period
          , consentProvisionActor = actor
          , consentProvisionAction = action
          , consentProvisionSecurityLabel = securityLabel
          , consentProvisionPurpose = purpose
          , consentProvisionClass = cl
          , consentProvisionCode = code
          , consentProvisionDataPeriod = dataPeriod
          , consentProvisionData = dt
          , consentProvisionProvision = provision
          }



data ConsentPolicy = ConsentPolicy {
    consentPolicyAttrId :: Maybe Text
  , consentPolicyExtension :: [Extension]
  , consentPolicyModifierExtension :: [Extension]
  , consentPolicyAuthority :: Maybe Uri
  , consentPolicyUri :: Maybe Uri
  }
--

instance ToJSON ConsentPolicy where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (consentPolicyAttrId p)
    ,  "extension" .= toJSON (consentPolicyExtension p)
    ,  "modifierExtension" .= toJSON (consentPolicyModifierExtension p)
    ,  "authority" .= toJSON (consentPolicyAuthority p)
    ,  "uri" .= toJSON (consentPolicyUri p)
    ]
instance FromJSON ConsentPolicy where
  parseJSON = withObject "ConsentPolicy" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        authority <- o .:? "authority"
        uri <- o .:? "uri"
        return ConsentPolicy{
            consentPolicyAttrId = id
          , consentPolicyExtension = extension
          , consentPolicyModifierExtension = modifierExtension
          , consentPolicyAuthority = authority
          , consentPolicyUri = uri
          }
instance Xmlbf.ToXml ConsentPolicy where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (consentPolicyAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (consentPolicyExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (consentPolicyModifierExtension p))
             , OptVal   "authority" (fmap toUri (consentPolicyAuthority p))
             , OptVal   "uri" (fmap toUri (consentPolicyUri p))
             ]
instance Xmlbf.FromXml ConsentPolicy where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    authority <- optional $ Xmlbf.pElement "authority" (Xmlbf.pAttr "value")
    uri <- optional $ Xmlbf.pElement "uri" (Xmlbf.pAttr "value")
    return ConsentPolicy {
            consentPolicyAttrId = id
          , consentPolicyExtension = extension
          , consentPolicyModifierExtension = modifierExtension
          , consentPolicyAuthority = fmap fromUri authority
          , consentPolicyUri = fmap fromUri uri
          }



data ConsentActor = ConsentActor {
    consentActorAttrId :: Maybe Text
  , consentActorExtension :: [Extension]
  , consentActorModifierExtension :: [Extension]
  , consentActorRole :: CodeableConcept
  , consentActorReference :: Reference
  }
--

instance ToJSON ConsentActor where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (consentActorAttrId p)
    ,  "extension" .= toJSON (consentActorExtension p)
    ,  "modifierExtension" .= toJSON (consentActorModifierExtension p)
    ,  "role" .= toJSON (consentActorRole p)
    ,  "reference" .= toJSON (consentActorReference p)
    ]
instance FromJSON ConsentActor where
  parseJSON = withObject "ConsentActor" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        role <- o .:  "role"
        reference <- o .:  "reference"
        return ConsentActor{
            consentActorAttrId = id
          , consentActorExtension = extension
          , consentActorModifierExtension = modifierExtension
          , consentActorRole = role
          , consentActorReference = reference
          }
instance Xmlbf.ToXml ConsentActor where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (consentActorAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (consentActorExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (consentActorModifierExtension p))
             , Prop     "role" (HM.empty, Xmlbf.toXml (consentActorRole p))
             , Prop     "reference" (HM.empty, Xmlbf.toXml (consentActorReference p))
             ]
instance Xmlbf.FromXml ConsentActor where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    role <-            Xmlbf.pElement "role" Xmlbf.fromXml
    reference <-            Xmlbf.pElement "reference" Xmlbf.fromXml
    return ConsentActor {
            consentActorAttrId = id
          , consentActorExtension = extension
          , consentActorModifierExtension = modifierExtension
          , consentActorRole = role
          , consentActorReference = reference
          }



data ConsentDataMeaning
    = CDMInstance
    | CDMRelated
    | CDMDependents
    | CDMAuthoredby
  deriving (Eq, Show)

instance ToJSON ConsentDataMeaning where
    toJSON CDMInstance = String "instance"
    toJSON CDMRelated = String "related"
    toJSON CDMDependents = String "dependents"
    toJSON CDMAuthoredby = String "authoredby"
instance FromJSON ConsentDataMeaning where
    parseJSON "instance" = return CDMInstance
    parseJSON "related" = return CDMRelated
    parseJSON "dependents" = return CDMDependents
    parseJSON "authoredby" = return CDMAuthoredby

toConsentDataMeaning CDMInstance = "instance"
toConsentDataMeaning CDMRelated = "related"
toConsentDataMeaning CDMDependents = "dependents"
toConsentDataMeaning CDMAuthoredby = "authoredby"
fromConsentDataMeaning "instance" = CDMInstance
fromConsentDataMeaning "related" = CDMRelated
fromConsentDataMeaning "dependents" = CDMDependents
fromConsentDataMeaning "authoredby" = CDMAuthoredby


data ConsentData = ConsentData {
    consentDataAttrId :: Maybe Text
  , consentDataExtension :: [Extension]
  , consentDataModifierExtension :: [Extension]
  , consentDataMeaning :: ConsentDataMeaning
  , consentDataReference :: Reference
  }
--

instance ToJSON ConsentData where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (consentDataAttrId p)
    ,  "extension" .= toJSON (consentDataExtension p)
    ,  "modifierExtension" .= toJSON (consentDataModifierExtension p)
    ,  "meaning" .= toJSON (consentDataMeaning p)
    ,  "reference" .= toJSON (consentDataReference p)
    ]
instance FromJSON ConsentData where
  parseJSON = withObject "ConsentData" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        meaning <- o .:  "meaning"
        reference <- o .:  "reference"
        return ConsentData{
            consentDataAttrId = id
          , consentDataExtension = extension
          , consentDataModifierExtension = modifierExtension
          , consentDataMeaning = meaning
          , consentDataReference = reference
          }
instance Xmlbf.ToXml ConsentData where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (consentDataAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (consentDataExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (consentDataModifierExtension p))
             , Val      "meaning" (     toConsentDataMeaning (consentDataMeaning p))
             , Prop     "reference" (HM.empty, Xmlbf.toXml (consentDataReference p))
             ]
instance Xmlbf.FromXml ConsentData where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    meaning <-            Xmlbf.pElement "meaning" (Xmlbf.pAttr "value")
    reference <-            Xmlbf.pElement "reference" Xmlbf.fromXml
    return ConsentData {
            consentDataAttrId = id
          , consentDataExtension = extension
          , consentDataModifierExtension = modifierExtension
          , consentDataMeaning =      fromConsentDataMeaning meaning
          , consentDataReference = reference
          }



data ConsentVerification = ConsentVerification {
    consentVerificationAttrId :: Maybe Text
  , consentVerificationExtension :: [Extension]
  , consentVerificationModifierExtension :: [Extension]
  , consentVerificationVerified :: Boolean
  , consentVerificationVerifiedWith :: Maybe Reference
  , consentVerificationVerificationDate :: Maybe DateTime
  }
--

instance ToJSON ConsentVerification where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (consentVerificationAttrId p)
    ,  "extension" .= toJSON (consentVerificationExtension p)
    ,  "modifierExtension" .= toJSON (consentVerificationModifierExtension p)
    ,  "verified" .= toJSON (consentVerificationVerified p)
    ,  "verifiedWith" .= toJSON (consentVerificationVerifiedWith p)
    ,  "verificationDate" .= toJSON (consentVerificationVerificationDate p)
    ]
instance FromJSON ConsentVerification where
  parseJSON = withObject "ConsentVerification" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        verified <- o .:  "verified"
        verifiedWith <- o .:? "verifiedWith"
        verificationDate <- o .:? "verificationDate"
        return ConsentVerification{
            consentVerificationAttrId = id
          , consentVerificationExtension = extension
          , consentVerificationModifierExtension = modifierExtension
          , consentVerificationVerified = verified
          , consentVerificationVerifiedWith = verifiedWith
          , consentVerificationVerificationDate = verificationDate
          }
instance Xmlbf.ToXml ConsentVerification where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (consentVerificationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (consentVerificationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (consentVerificationModifierExtension p))
             , Val      "verified" (     toBoolean (consentVerificationVerified p))
             , OptProp  "verifiedWith" (fmap Xmlbf.toXml (consentVerificationVerifiedWith p))
             , OptVal   "verificationDate" (fmap toDateTime (consentVerificationVerificationDate p))
             ]
instance Xmlbf.FromXml ConsentVerification where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    verified <-            Xmlbf.pElement "verified" (Xmlbf.pAttr "value")
    verifiedWith <- optional $ Xmlbf.pElement "verifiedWith" Xmlbf.fromXml
    verificationDate <- optional $ Xmlbf.pElement "verificationDate" (Xmlbf.pAttr "value")
    return ConsentVerification {
            consentVerificationAttrId = id
          , consentVerificationExtension = extension
          , consentVerificationModifierExtension = modifierExtension
          , consentVerificationVerified =      fromBoolean verified
          , consentVerificationVerifiedWith = verifiedWith
          , consentVerificationVerificationDate = fmap fromDateTime verificationDate
          }




