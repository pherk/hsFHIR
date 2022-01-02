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
-- FHIR 4.0.0 Coverage
--

module Data.FHIR.Resources.Coverage where

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

data CoverageStatus
    = CSActive
    | CSCancelled
    | CSDraft
    | CSEnteredInError
  deriving (Eq, Show)

instance ToJSON CoverageStatus where
    toJSON CSActive = String "active"
    toJSON CSCancelled = String "cancelled"
    toJSON CSDraft = String "draft"
    toJSON CSEnteredInError = String "entered-in-error"
instance FromJSON CoverageStatus where
    parseJSON "active" = return CSActive
    parseJSON "cancelled" = return CSCancelled
    parseJSON "draft" = return CSDraft
    parseJSON "entered-in-error" = return CSEnteredInError

toCoverageStatus CSActive = "active"
toCoverageStatus CSCancelled = "cancelled"
toCoverageStatus CSDraft = "draft"
toCoverageStatus CSEnteredInError = "entered-in-error"
fromCoverageStatus "active" = CSActive
fromCoverageStatus "cancelled" = CSCancelled
fromCoverageStatus "draft" = CSDraft
fromCoverageStatus "entered-in-error" = CSEnteredInError


data Coverage = Coverage {
    coverageId :: Maybe Id
  , coverageMeta :: Maybe Meta
  , coverageImplicitRules :: Maybe Uri
  , coverageLanguage :: Maybe Language
  , coverageText :: Maybe Narrative
--    coverageContained :: [ResourceContainer]
  , coverageExtension :: [Extension]
  , coverageModifierExtension :: [Extension]
  , coverageIdentifier :: [Identifier]
  , coverageStatus :: CoverageStatus
  , coverageType :: Maybe CodeableConcept
  , coveragePolicyHolder :: Maybe Reference
  , coverageSubscriber :: Maybe Reference
  , coverageSubscriberId :: Maybe Text
  , coverageBeneficiary :: Reference
  , coverageDependent :: Maybe Text
  , coverageRelationship :: Maybe CodeableConcept
  , coveragePeriod :: Maybe Period
  , coveragePayor :: [Reference]
  , coverageClass :: [CoverageClass]
  , coverageOrder :: Maybe PositiveInt
  , coverageNetwork :: Maybe Text
  , coverageCostToBeneficiary :: [CoverageCostToBeneficiary]
  , coverageSubrogation :: Maybe Boolean
  , coverageContract :: [Reference]
  }
--

instance ToJSON Coverage where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Coverage")
    ,  "id" .= toJSON (coverageId p)
    ,  "meta" .= toJSON (coverageMeta p)
    ,  "implicitRules" .= toJSON (coverageImplicitRules p)
    ,  "language" .= toJSON (coverageLanguage p)
    ,  "text" .= toJSON (coverageText p)
--    , "contained" .= toJSON (coverageContained p)
    ,  "extension" .= toJSON (coverageExtension p)
    ,  "modifierExtension" .= toJSON (coverageModifierExtension p)
    ,  "identifier" .= toJSON (coverageIdentifier p)
    ,  "status" .= toJSON (coverageStatus p)
    ,  "type" .= toJSON (coverageType p)
    ,  "policyHolder" .= toJSON (coveragePolicyHolder p)
    ,  "subscriber" .= toJSON (coverageSubscriber p)
    ,  "subscriberId" .= toJSON (coverageSubscriberId p)
    ,  "beneficiary" .= toJSON (coverageBeneficiary p)
    ,  "dependent" .= toJSON (coverageDependent p)
    ,  "relationship" .= toJSON (coverageRelationship p)
    ,  "period" .= toJSON (coveragePeriod p)
    ,  "payor" .= toJSON (coveragePayor p)
    ,  "class" .= toJSON (coverageClass p)
    ,  "order" .= toJSON (coverageOrder p)
    ,  "network" .= toJSON (coverageNetwork p)
    ,  "costToBeneficiary" .= toJSON (coverageCostToBeneficiary p)
    ,  "subrogation" .= toJSON (coverageSubrogation p)
    ,  "contract" .= toJSON (coverageContract p)
    ]
instance FromJSON Coverage where
  parseJSON = withObject "Coverage" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Coverage" -> do
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
        ty <- o .:? "type"
        policyHolder <- o .:? "policyHolder"
        subscriber <- o .:? "subscriber"
        subscriberId <- o .:? "subscriberId"
        beneficiary <- o .:  "beneficiary"
        dependent <- o .:? "dependent"
        relationship <- o .:? "relationship"
        period <- o .:? "period"
        payor <- o .:? "payor" .!= []
        cl <- o .:? "class" .!= []
        order <- o .:? "order"
        network <- o .:? "network"
        costToBeneficiary <- o .:? "costToBeneficiary" .!= []
        subrogation <- o .:? "subrogation"
        contract <- o .:? "contract" .!= []
        return Coverage{
            coverageId = id
          , coverageMeta = meta
          , coverageImplicitRules = implicitRules
          , coverageLanguage = language
          , coverageText = text
--          , coverageContained = contained
          , coverageExtension = extension
          , coverageModifierExtension = modifierExtension
          , coverageIdentifier = identifier
          , coverageStatus = status
          , coverageType = ty
          , coveragePolicyHolder = policyHolder
          , coverageSubscriber = subscriber
          , coverageSubscriberId = subscriberId
          , coverageBeneficiary = beneficiary
          , coverageDependent = dependent
          , coverageRelationship = relationship
          , coveragePeriod = period
          , coveragePayor = payor
          , coverageClass = cl
          , coverageOrder = order
          , coverageNetwork = network
          , coverageCostToBeneficiary = costToBeneficiary
          , coverageSubrogation = subrogation
          , coverageContract = contract
          }
      _ -> fail "not a Coverage"
instance Xmlbf.ToXml Coverage where
  toXml p = Xmlbf.element "Coverage" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (coverageId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (coverageMeta p))
             , OptVal   "implicitRules" (fmap toUri (coverageImplicitRules p))
             , OptVal   "language" (fmap toLanguage (coverageLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (coverageText p))
--             , PropList "contained" (fmap Xmlbf.toXml (coverageContained p))
             , PropList "extension" (fmap Xmlbf.toXml (coverageExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (coverageModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (coverageIdentifier p))
             , Val      "status" (     toCoverageStatus (coverageStatus p))
             , OptProp  "type" (fmap Xmlbf.toXml (coverageType p))
             , OptProp  "policyHolder" (fmap Xmlbf.toXml (coveragePolicyHolder p))
             , OptProp  "subscriber" (fmap Xmlbf.toXml (coverageSubscriber p))
             , OptVal   "subscriberId" (fmap toString (coverageSubscriberId p))
             , Prop     "beneficiary" (HM.empty, Xmlbf.toXml (coverageBeneficiary p))
             , OptVal   "dependent" (fmap toString (coverageDependent p))
             , OptProp  "relationship" (fmap Xmlbf.toXml (coverageRelationship p))
             , OptProp  "period" (fmap Xmlbf.toXml (coveragePeriod p))
             , PropList "payor" (fmap Xmlbf.toXml (coveragePayor p))
             , PropList "class" (fmap Xmlbf.toXml (coverageClass p))
             , OptVal   "order" (fmap toPositiveInt (coverageOrder p))
             , OptVal   "network" (fmap toString (coverageNetwork p))
             , PropList "costToBeneficiary" (fmap Xmlbf.toXml (coverageCostToBeneficiary p))
             , OptVal   "subrogation" (fmap toBoolean (coverageSubrogation p))
             , PropList "contract" (fmap Xmlbf.toXml (coverageContract p))
             ]
instance Xmlbf.FromXml Coverage where
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
    ty <- optional $ Xmlbf.pElement "type" Xmlbf.fromXml
    policyHolder <- optional $ Xmlbf.pElement "policyHolder" Xmlbf.fromXml
    subscriber <- optional $ Xmlbf.pElement "subscriber" Xmlbf.fromXml
    subscriberId <- optional $ Xmlbf.pElement "subscriberId" (Xmlbf.pAttr "value")
    beneficiary <-            Xmlbf.pElement "beneficiary" Xmlbf.fromXml
    dependent <- optional $ Xmlbf.pElement "dependent" (Xmlbf.pAttr "value")
    relationship <- optional $ Xmlbf.pElement "relationship" Xmlbf.fromXml
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    payor <- many     $ Xmlbf.pElement "payor" Xmlbf.fromXml
    cl <- many     $ Xmlbf.pElement "class" Xmlbf.fromXml
    order <- optional $ Xmlbf.pElement "order" (Xmlbf.pAttr "value")
    network <- optional $ Xmlbf.pElement "network" (Xmlbf.pAttr "value")
    costToBeneficiary <- many     $ Xmlbf.pElement "costToBeneficiary" Xmlbf.fromXml
    subrogation <- optional $ Xmlbf.pElement "subrogation" (Xmlbf.pAttr "value")
    contract <- many     $ Xmlbf.pElement "contract" Xmlbf.fromXml
    return Coverage {
            coverageId = fmap fromId id
          , coverageMeta = meta
          , coverageImplicitRules = fmap fromUri implicitRules
          , coverageLanguage = fmap fromLanguage language
          , coverageText = text
--          , coverageContained = contained
          , coverageExtension = extension
          , coverageModifierExtension = modifierExtension
          , coverageIdentifier = identifier
          , coverageStatus =      fromCoverageStatus status
          , coverageType = ty
          , coveragePolicyHolder = policyHolder
          , coverageSubscriber = subscriber
          , coverageSubscriberId = fmap fromString subscriberId
          , coverageBeneficiary = beneficiary
          , coverageDependent = fmap fromString dependent
          , coverageRelationship = relationship
          , coveragePeriod = period
          , coveragePayor = payor
          , coverageClass = cl
          , coverageOrder = fmap fromPositiveInt order
          , coverageNetwork = fmap fromString network
          , coverageCostToBeneficiary = costToBeneficiary
          , coverageSubrogation = fmap fromBoolean subrogation
          , coverageContract = contract
          }



data CoverageEligibilityResponseBenefitAllowed
    = CoverageEligibilityResponseBenefitAllowedUnsignedInt UnsignedInt
    | CoverageEligibilityResponseBenefitAllowedString Text
    | CoverageEligibilityResponseBenefitAllowedMoney Money
    deriving (Eq, Show)

data CoverageEligibilityResponseBenefitUsed
    = CoverageEligibilityResponseBenefitUsedUnsignedInt UnsignedInt
    | CoverageEligibilityResponseBenefitUsedString Text
    | CoverageEligibilityResponseBenefitUsedMoney Money
    deriving (Eq, Show)

data CoverageEligibilityResponseBenefit = CoverageEligibilityResponseBenefit {
    coverageEligibilityResponseBenefitAttrId :: Maybe Text
  , coverageEligibilityResponseBenefitExtension :: [Extension]
  , coverageEligibilityResponseBenefitModifierExtension :: [Extension]
  , coverageEligibilityResponseBenefitType :: CodeableConcept
  , coverageEligibilityResponseBenefitAllowedUnsignedInt :: Maybe UnsignedInt
  , coverageEligibilityResponseBenefitAllowedString :: Maybe Text
  , coverageEligibilityResponseBenefitAllowedMoney :: Maybe Money
  , coverageEligibilityResponseBenefitUsedUnsignedInt :: Maybe UnsignedInt
  , coverageEligibilityResponseBenefitUsedString :: Maybe Text
  , coverageEligibilityResponseBenefitUsedMoney :: Maybe Money
  }
--

instance ToJSON CoverageEligibilityResponseBenefit where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (coverageEligibilityResponseBenefitAttrId p)
    ,  "extension" .= toJSON (coverageEligibilityResponseBenefitExtension p)
    ,  "modifierExtension" .= toJSON (coverageEligibilityResponseBenefitModifierExtension p)
    ,  "type" .= toJSON (coverageEligibilityResponseBenefitType p)
    ,  "allowedUnsignedInt" .= toJSON (coverageEligibilityResponseBenefitAllowedUnsignedInt p)
    ,  "allowedString" .= toJSON (coverageEligibilityResponseBenefitAllowedString p)
    ,  "allowedMoney" .= toJSON (coverageEligibilityResponseBenefitAllowedMoney p)
    ,  "usedUnsignedInt" .= toJSON (coverageEligibilityResponseBenefitUsedUnsignedInt p)
    ,  "usedString" .= toJSON (coverageEligibilityResponseBenefitUsedString p)
    ,  "usedMoney" .= toJSON (coverageEligibilityResponseBenefitUsedMoney p)
    ]
instance FromJSON CoverageEligibilityResponseBenefit where
  parseJSON = withObject "CoverageEligibilityResponseBenefit" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:  "type"
        allowedUnsignedInt <- o .:? "allowedUnsignedInt"
        allowedString <- o .:? "allowedString"
        allowedMoney <- o .:? "allowedMoney"
        usedUnsignedInt <- o .:? "usedUnsignedInt"
        usedString <- o .:? "usedString"
        usedMoney <- o .:? "usedMoney"
        return CoverageEligibilityResponseBenefit{
            coverageEligibilityResponseBenefitAttrId = id
          , coverageEligibilityResponseBenefitExtension = extension
          , coverageEligibilityResponseBenefitModifierExtension = modifierExtension
          , coverageEligibilityResponseBenefitType = ty
          , coverageEligibilityResponseBenefitAllowedUnsignedInt = allowedUnsignedInt
          , coverageEligibilityResponseBenefitAllowedString = allowedString
          , coverageEligibilityResponseBenefitAllowedMoney = allowedMoney
          , coverageEligibilityResponseBenefitUsedUnsignedInt = usedUnsignedInt
          , coverageEligibilityResponseBenefitUsedString = usedString
          , coverageEligibilityResponseBenefitUsedMoney = usedMoney
          }
instance Xmlbf.ToXml CoverageEligibilityResponseBenefit where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (coverageEligibilityResponseBenefitAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (coverageEligibilityResponseBenefitExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (coverageEligibilityResponseBenefitModifierExtension p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (coverageEligibilityResponseBenefitType p))
             , OptVal   "allowedUnsignedInt" (fmap toUnsignedInt (coverageEligibilityResponseBenefitAllowedUnsignedInt p))
             , OptVal   "allowedString" (fmap toString (coverageEligibilityResponseBenefitAllowedString p))
             , OptProp  "allowedMoney" (fmap Xmlbf.toXml (coverageEligibilityResponseBenefitAllowedMoney p))
             , OptVal   "usedUnsignedInt" (fmap toUnsignedInt (coverageEligibilityResponseBenefitUsedUnsignedInt p))
             , OptVal   "usedString" (fmap toString (coverageEligibilityResponseBenefitUsedString p))
             , OptProp  "usedMoney" (fmap Xmlbf.toXml (coverageEligibilityResponseBenefitUsedMoney p))
             ]
instance Xmlbf.FromXml CoverageEligibilityResponseBenefit where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" Xmlbf.fromXml
    allowedUnsignedInt <- optional $ Xmlbf.pElement "allowedUnsignedInt" (Xmlbf.pAttr "value")
    allowedString <- optional $ Xmlbf.pElement "allowedString" (Xmlbf.pAttr "value")
    allowedMoney <- optional $ Xmlbf.pElement "allowedMoney" Xmlbf.fromXml
    usedUnsignedInt <- optional $ Xmlbf.pElement "usedUnsignedInt" (Xmlbf.pAttr "value")
    usedString <- optional $ Xmlbf.pElement "usedString" (Xmlbf.pAttr "value")
    usedMoney <- optional $ Xmlbf.pElement "usedMoney" Xmlbf.fromXml
    return CoverageEligibilityResponseBenefit {
            coverageEligibilityResponseBenefitAttrId = id
          , coverageEligibilityResponseBenefitExtension = extension
          , coverageEligibilityResponseBenefitModifierExtension = modifierExtension
          , coverageEligibilityResponseBenefitType = ty
          , coverageEligibilityResponseBenefitAllowedUnsignedInt = fmap fromUnsignedInt allowedUnsignedInt
          , coverageEligibilityResponseBenefitAllowedString = fmap fromString allowedString
          , coverageEligibilityResponseBenefitAllowedMoney = allowedMoney
          , coverageEligibilityResponseBenefitUsedUnsignedInt = fmap fromUnsignedInt usedUnsignedInt
          , coverageEligibilityResponseBenefitUsedString = fmap fromString usedString
          , coverageEligibilityResponseBenefitUsedMoney = usedMoney
          }



data CoverageEligibilityRequestDiagnosisDiagnosis
    = CoverageEligibilityRequestDiagnosisDiagnosisCodeableConcept CodeableConcept
    | CoverageEligibilityRequestDiagnosisDiagnosisReference Reference
    deriving (Eq, Show)

data CoverageEligibilityRequestDiagnosis = CoverageEligibilityRequestDiagnosis {
    coverageEligibilityRequestDiagnosisAttrId :: Maybe Text
  , coverageEligibilityRequestDiagnosisExtension :: [Extension]
  , coverageEligibilityRequestDiagnosisModifierExtension :: [Extension]
  , coverageEligibilityRequestDiagnosisDiagnosisCodeableConcept :: Maybe CodeableConcept
  , coverageEligibilityRequestDiagnosisDiagnosisReference :: Maybe Reference
  }
--

instance ToJSON CoverageEligibilityRequestDiagnosis where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (coverageEligibilityRequestDiagnosisAttrId p)
    ,  "extension" .= toJSON (coverageEligibilityRequestDiagnosisExtension p)
    ,  "modifierExtension" .= toJSON (coverageEligibilityRequestDiagnosisModifierExtension p)
    ,  "diagnosisCodeableConcept" .= toJSON (coverageEligibilityRequestDiagnosisDiagnosisCodeableConcept p)
    ,  "diagnosisReference" .= toJSON (coverageEligibilityRequestDiagnosisDiagnosisReference p)
    ]
instance FromJSON CoverageEligibilityRequestDiagnosis where
  parseJSON = withObject "CoverageEligibilityRequestDiagnosis" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        diagnosisCodeableConcept <- o .:? "diagnosisCodeableConcept"
        diagnosisReference <- o .:? "diagnosisReference"
        return CoverageEligibilityRequestDiagnosis{
            coverageEligibilityRequestDiagnosisAttrId = id
          , coverageEligibilityRequestDiagnosisExtension = extension
          , coverageEligibilityRequestDiagnosisModifierExtension = modifierExtension
          , coverageEligibilityRequestDiagnosisDiagnosisCodeableConcept = diagnosisCodeableConcept
          , coverageEligibilityRequestDiagnosisDiagnosisReference = diagnosisReference
          }
instance Xmlbf.ToXml CoverageEligibilityRequestDiagnosis where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (coverageEligibilityRequestDiagnosisAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (coverageEligibilityRequestDiagnosisExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (coverageEligibilityRequestDiagnosisModifierExtension p))
             , OptProp  "diagnosisCodeableConcept" (fmap Xmlbf.toXml (coverageEligibilityRequestDiagnosisDiagnosisCodeableConcept p))
             , OptProp  "diagnosisReference" (fmap Xmlbf.toXml (coverageEligibilityRequestDiagnosisDiagnosisReference p))
             ]
instance Xmlbf.FromXml CoverageEligibilityRequestDiagnosis where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    diagnosisCodeableConcept <- optional $ Xmlbf.pElement "diagnosisCodeableConcept" Xmlbf.fromXml
    diagnosisReference <- optional $ Xmlbf.pElement "diagnosisReference" Xmlbf.fromXml
    return CoverageEligibilityRequestDiagnosis {
            coverageEligibilityRequestDiagnosisAttrId = id
          , coverageEligibilityRequestDiagnosisExtension = extension
          , coverageEligibilityRequestDiagnosisModifierExtension = modifierExtension
          , coverageEligibilityRequestDiagnosisDiagnosisCodeableConcept = diagnosisCodeableConcept
          , coverageEligibilityRequestDiagnosisDiagnosisReference = diagnosisReference
          }



data CoverageEligibilityRequestStatus
    = CERSActive
    | CERSCancelled
    | CERSDraft
    | CERSEnteredInError
  deriving (Eq, Show)

instance ToJSON CoverageEligibilityRequestStatus where
    toJSON CERSActive = String "active"
    toJSON CERSCancelled = String "cancelled"
    toJSON CERSDraft = String "draft"
    toJSON CERSEnteredInError = String "entered-in-error"
instance FromJSON CoverageEligibilityRequestStatus where
    parseJSON "active" = return CERSActive
    parseJSON "cancelled" = return CERSCancelled
    parseJSON "draft" = return CERSDraft
    parseJSON "entered-in-error" = return CERSEnteredInError

toCoverageEligibilityRequestStatus CERSActive = "active"
toCoverageEligibilityRequestStatus CERSCancelled = "cancelled"
toCoverageEligibilityRequestStatus CERSDraft = "draft"
toCoverageEligibilityRequestStatus CERSEnteredInError = "entered-in-error"
fromCoverageEligibilityRequestStatus "active" = CERSActive
fromCoverageEligibilityRequestStatus "cancelled" = CERSCancelled
fromCoverageEligibilityRequestStatus "draft" = CERSDraft
fromCoverageEligibilityRequestStatus "entered-in-error" = CERSEnteredInError


data CoverageEligibilityRequestPurpose
    = CERPAuthRequirements
    | CERPBenefits
    | CERPDiscovery
    | CERPValidation
  deriving (Eq, Show)

instance ToJSON CoverageEligibilityRequestPurpose where
    toJSON CERPAuthRequirements = String "auth-requirements"
    toJSON CERPBenefits = String "benefits"
    toJSON CERPDiscovery = String "discovery"
    toJSON CERPValidation = String "validation"
instance FromJSON CoverageEligibilityRequestPurpose where
    parseJSON "auth-requirements" = return CERPAuthRequirements
    parseJSON "benefits" = return CERPBenefits
    parseJSON "discovery" = return CERPDiscovery
    parseJSON "validation" = return CERPValidation

toCoverageEligibilityRequestPurpose CERPAuthRequirements = "auth-requirements"
toCoverageEligibilityRequestPurpose CERPBenefits = "benefits"
toCoverageEligibilityRequestPurpose CERPDiscovery = "discovery"
toCoverageEligibilityRequestPurpose CERPValidation = "validation"
fromCoverageEligibilityRequestPurpose "auth-requirements" = CERPAuthRequirements
fromCoverageEligibilityRequestPurpose "benefits" = CERPBenefits
fromCoverageEligibilityRequestPurpose "discovery" = CERPDiscovery
fromCoverageEligibilityRequestPurpose "validation" = CERPValidation


data CoverageEligibilityRequestServiced
    = CoverageEligibilityRequestServicedDate Date
    | CoverageEligibilityRequestServicedPeriod Period
    deriving (Eq, Show)

data CoverageEligibilityRequest = CoverageEligibilityRequest {
    coverageEligibilityRequestId :: Maybe Id
  , coverageEligibilityRequestMeta :: Maybe Meta
  , coverageEligibilityRequestImplicitRules :: Maybe Uri
  , coverageEligibilityRequestLanguage :: Maybe Language
  , coverageEligibilityRequestText :: Maybe Narrative
--    coverageEligibilityRequestContained :: [ResourceContainer]
  , coverageEligibilityRequestExtension :: [Extension]
  , coverageEligibilityRequestModifierExtension :: [Extension]
  , coverageEligibilityRequestIdentifier :: [Identifier]
  , coverageEligibilityRequestStatus :: CoverageEligibilityRequestStatus
  , coverageEligibilityRequestPriority :: Maybe CodeableConcept
  , coverageEligibilityRequestPurpose :: [CoverageEligibilityRequestPurpose]
  , coverageEligibilityRequestPatient :: Reference
  , coverageEligibilityRequestServicedDate :: Maybe Date
  , coverageEligibilityRequestServicedPeriod :: Maybe Period
  , coverageEligibilityRequestCreated :: DateTime
  , coverageEligibilityRequestEnterer :: Maybe Reference
  , coverageEligibilityRequestProvider :: Maybe Reference
  , coverageEligibilityRequestInsurer :: Reference
  , coverageEligibilityRequestFacility :: Maybe Reference
  , coverageEligibilityRequestSupportingInfo :: [CoverageEligibilityRequestSupportingInfo]
  , coverageEligibilityRequestInsurance :: [CoverageEligibilityRequestInsurance]
  , coverageEligibilityRequestItem :: [CoverageEligibilityRequestItem]
  }
--

instance ToJSON CoverageEligibilityRequest where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "CoverageEligibilityRequest")
    ,  "id" .= toJSON (coverageEligibilityRequestId p)
    ,  "meta" .= toJSON (coverageEligibilityRequestMeta p)
    ,  "implicitRules" .= toJSON (coverageEligibilityRequestImplicitRules p)
    ,  "language" .= toJSON (coverageEligibilityRequestLanguage p)
    ,  "text" .= toJSON (coverageEligibilityRequestText p)
--    , "contained" .= toJSON (coverageEligibilityRequestContained p)
    ,  "extension" .= toJSON (coverageEligibilityRequestExtension p)
    ,  "modifierExtension" .= toJSON (coverageEligibilityRequestModifierExtension p)
    ,  "identifier" .= toJSON (coverageEligibilityRequestIdentifier p)
    ,  "status" .= toJSON (coverageEligibilityRequestStatus p)
    ,  "priority" .= toJSON (coverageEligibilityRequestPriority p)
    ,  "purpose" .= toJSON (coverageEligibilityRequestPurpose p)
    ,  "patient" .= toJSON (coverageEligibilityRequestPatient p)
    ,  "servicedDate" .= toJSON (coverageEligibilityRequestServicedDate p)
    ,  "servicedPeriod" .= toJSON (coverageEligibilityRequestServicedPeriod p)
    ,  "created" .= toJSON (coverageEligibilityRequestCreated p)
    ,  "enterer" .= toJSON (coverageEligibilityRequestEnterer p)
    ,  "provider" .= toJSON (coverageEligibilityRequestProvider p)
    ,  "insurer" .= toJSON (coverageEligibilityRequestInsurer p)
    ,  "facility" .= toJSON (coverageEligibilityRequestFacility p)
    ,  "supportingInfo" .= toJSON (coverageEligibilityRequestSupportingInfo p)
    ,  "insurance" .= toJSON (coverageEligibilityRequestInsurance p)
    ,  "item" .= toJSON (coverageEligibilityRequestItem p)
    ]
instance FromJSON CoverageEligibilityRequest where
  parseJSON = withObject "CoverageEligibilityRequest" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "CoverageEligibilityRequest" -> do
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
        priority <- o .:? "priority"
        purpose <- o .:? "purpose" .!= []
        patient <- o .:  "patient"
        servicedDate <- o .:? "servicedDate"
        servicedPeriod <- o .:? "servicedPeriod"
        created <- o .:  "created"
        enterer <- o .:? "enterer"
        provider <- o .:? "provider"
        insurer <- o .:  "insurer"
        facility <- o .:? "facility"
        supportingInfo <- o .:? "supportingInfo" .!= []
        insurance <- o .:? "insurance" .!= []
        item <- o .:? "item" .!= []
        return CoverageEligibilityRequest{
            coverageEligibilityRequestId = id
          , coverageEligibilityRequestMeta = meta
          , coverageEligibilityRequestImplicitRules = implicitRules
          , coverageEligibilityRequestLanguage = language
          , coverageEligibilityRequestText = text
--          , coverageEligibilityRequestContained = contained
          , coverageEligibilityRequestExtension = extension
          , coverageEligibilityRequestModifierExtension = modifierExtension
          , coverageEligibilityRequestIdentifier = identifier
          , coverageEligibilityRequestStatus = status
          , coverageEligibilityRequestPriority = priority
          , coverageEligibilityRequestPurpose = purpose
          , coverageEligibilityRequestPatient = patient
          , coverageEligibilityRequestServicedDate = servicedDate
          , coverageEligibilityRequestServicedPeriod = servicedPeriod
          , coverageEligibilityRequestCreated = created
          , coverageEligibilityRequestEnterer = enterer
          , coverageEligibilityRequestProvider = provider
          , coverageEligibilityRequestInsurer = insurer
          , coverageEligibilityRequestFacility = facility
          , coverageEligibilityRequestSupportingInfo = supportingInfo
          , coverageEligibilityRequestInsurance = insurance
          , coverageEligibilityRequestItem = item
          }
      _ -> fail "not a CoverageEligibilityRequest"
instance Xmlbf.ToXml CoverageEligibilityRequest where
  toXml p = Xmlbf.element "CoverageEligibilityRequest" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (coverageEligibilityRequestId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (coverageEligibilityRequestMeta p))
             , OptVal   "implicitRules" (fmap toUri (coverageEligibilityRequestImplicitRules p))
             , OptVal   "language" (fmap toLanguage (coverageEligibilityRequestLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (coverageEligibilityRequestText p))
--             , PropList "contained" (fmap Xmlbf.toXml (coverageEligibilityRequestContained p))
             , PropList "extension" (fmap Xmlbf.toXml (coverageEligibilityRequestExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (coverageEligibilityRequestModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (coverageEligibilityRequestIdentifier p))
             , Val      "status" (     toCoverageEligibilityRequestStatus (coverageEligibilityRequestStatus p))
             , OptProp  "priority" (fmap Xmlbf.toXml (coverageEligibilityRequestPriority p))
             , ValList  "purpose" (fmap toCoverageEligibilityRequestPurpose (coverageEligibilityRequestPurpose p))
             , Prop     "patient" (HM.empty, Xmlbf.toXml (coverageEligibilityRequestPatient p))
             , OptVal   "servicedDate" (fmap toDate (coverageEligibilityRequestServicedDate p))
             , OptProp  "servicedPeriod" (fmap Xmlbf.toXml (coverageEligibilityRequestServicedPeriod p))
             , Val      "created" (     toDateTime (coverageEligibilityRequestCreated p))
             , OptProp  "enterer" (fmap Xmlbf.toXml (coverageEligibilityRequestEnterer p))
             , OptProp  "provider" (fmap Xmlbf.toXml (coverageEligibilityRequestProvider p))
             , Prop     "insurer" (HM.empty, Xmlbf.toXml (coverageEligibilityRequestInsurer p))
             , OptProp  "facility" (fmap Xmlbf.toXml (coverageEligibilityRequestFacility p))
             , PropList "supportingInfo" (fmap Xmlbf.toXml (coverageEligibilityRequestSupportingInfo p))
             , PropList "insurance" (fmap Xmlbf.toXml (coverageEligibilityRequestInsurance p))
             , PropList "item" (fmap Xmlbf.toXml (coverageEligibilityRequestItem p))
             ]
instance Xmlbf.FromXml CoverageEligibilityRequest where
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
    priority <- optional $ Xmlbf.pElement "priority" Xmlbf.fromXml
    purpose <- many     $ Xmlbf.pElement "purpose" (Xmlbf.pAttr "value")
    patient <-            Xmlbf.pElement "patient" Xmlbf.fromXml
    servicedDate <- optional $ Xmlbf.pElement "servicedDate" (Xmlbf.pAttr "value")
    servicedPeriod <- optional $ Xmlbf.pElement "servicedPeriod" Xmlbf.fromXml
    created <-            Xmlbf.pElement "created" (Xmlbf.pAttr "value")
    enterer <- optional $ Xmlbf.pElement "enterer" Xmlbf.fromXml
    provider <- optional $ Xmlbf.pElement "provider" Xmlbf.fromXml
    insurer <-            Xmlbf.pElement "insurer" Xmlbf.fromXml
    facility <- optional $ Xmlbf.pElement "facility" Xmlbf.fromXml
    supportingInfo <- many     $ Xmlbf.pElement "supportingInfo" Xmlbf.fromXml
    insurance <- many     $ Xmlbf.pElement "insurance" Xmlbf.fromXml
    item <- many     $ Xmlbf.pElement "item" Xmlbf.fromXml
    return CoverageEligibilityRequest {
            coverageEligibilityRequestId = fmap fromId id
          , coverageEligibilityRequestMeta = meta
          , coverageEligibilityRequestImplicitRules = fmap fromUri implicitRules
          , coverageEligibilityRequestLanguage = fmap fromLanguage language
          , coverageEligibilityRequestText = text
--          , coverageEligibilityRequestContained = contained
          , coverageEligibilityRequestExtension = extension
          , coverageEligibilityRequestModifierExtension = modifierExtension
          , coverageEligibilityRequestIdentifier = identifier
          , coverageEligibilityRequestStatus =      fromCoverageEligibilityRequestStatus status
          , coverageEligibilityRequestPriority = priority
          , coverageEligibilityRequestPurpose = fmap fromCoverageEligibilityRequestPurpose purpose
          , coverageEligibilityRequestPatient = patient
          , coverageEligibilityRequestServicedDate = fmap fromDate servicedDate
          , coverageEligibilityRequestServicedPeriod = servicedPeriod
          , coverageEligibilityRequestCreated =      fromDateTime created
          , coverageEligibilityRequestEnterer = enterer
          , coverageEligibilityRequestProvider = provider
          , coverageEligibilityRequestInsurer = insurer
          , coverageEligibilityRequestFacility = facility
          , coverageEligibilityRequestSupportingInfo = supportingInfo
          , coverageEligibilityRequestInsurance = insurance
          , coverageEligibilityRequestItem = item
          }



data CoverageClass = CoverageClass {
    coverageClassAttrId :: Maybe Text
  , coverageClassExtension :: [Extension]
  , coverageClassModifierExtension :: [Extension]
  , coverageClassType :: CodeableConcept
  , coverageClassValue :: Text
  , coverageClassName :: Maybe Text
  }
--

instance ToJSON CoverageClass where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (coverageClassAttrId p)
    ,  "extension" .= toJSON (coverageClassExtension p)
    ,  "modifierExtension" .= toJSON (coverageClassModifierExtension p)
    ,  "type" .= toJSON (coverageClassType p)
    ,  "value" .= toJSON (coverageClassValue p)
    ,  "name" .= toJSON (coverageClassName p)
    ]
instance FromJSON CoverageClass where
  parseJSON = withObject "CoverageClass" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:  "type"
        value <- o .:  "value"
        name <- o .:? "name"
        return CoverageClass{
            coverageClassAttrId = id
          , coverageClassExtension = extension
          , coverageClassModifierExtension = modifierExtension
          , coverageClassType = ty
          , coverageClassValue = value
          , coverageClassName = name
          }
instance Xmlbf.ToXml CoverageClass where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (coverageClassAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (coverageClassExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (coverageClassModifierExtension p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (coverageClassType p))
             , Val      "value" (     toString (coverageClassValue p))
             , OptVal   "name" (fmap toString (coverageClassName p))
             ]
instance Xmlbf.FromXml CoverageClass where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" Xmlbf.fromXml
    value <-            Xmlbf.pElement "value" (Xmlbf.pAttr "value")
    name <- optional $ Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    return CoverageClass {
            coverageClassAttrId = id
          , coverageClassExtension = extension
          , coverageClassModifierExtension = modifierExtension
          , coverageClassType = ty
          , coverageClassValue =      fromString value
          , coverageClassName = fmap fromString name
          }



data CoverageException = CoverageException {
    coverageExceptionAttrId :: Maybe Text
  , coverageExceptionExtension :: [Extension]
  , coverageExceptionModifierExtension :: [Extension]
  , coverageExceptionType :: CodeableConcept
  , coverageExceptionPeriod :: Maybe Period
  }
--

instance ToJSON CoverageException where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (coverageExceptionAttrId p)
    ,  "extension" .= toJSON (coverageExceptionExtension p)
    ,  "modifierExtension" .= toJSON (coverageExceptionModifierExtension p)
    ,  "type" .= toJSON (coverageExceptionType p)
    ,  "period" .= toJSON (coverageExceptionPeriod p)
    ]
instance FromJSON CoverageException where
  parseJSON = withObject "CoverageException" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:  "type"
        period <- o .:? "period"
        return CoverageException{
            coverageExceptionAttrId = id
          , coverageExceptionExtension = extension
          , coverageExceptionModifierExtension = modifierExtension
          , coverageExceptionType = ty
          , coverageExceptionPeriod = period
          }
instance Xmlbf.ToXml CoverageException where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (coverageExceptionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (coverageExceptionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (coverageExceptionModifierExtension p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (coverageExceptionType p))
             , OptProp  "period" (fmap Xmlbf.toXml (coverageExceptionPeriod p))
             ]
instance Xmlbf.FromXml CoverageException where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" Xmlbf.fromXml
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    return CoverageException {
            coverageExceptionAttrId = id
          , coverageExceptionExtension = extension
          , coverageExceptionModifierExtension = modifierExtension
          , coverageExceptionType = ty
          , coverageExceptionPeriod = period
          }



data CoverageEligibilityResponseError = CoverageEligibilityResponseError {
    coverageEligibilityResponseErrorAttrId :: Maybe Text
  , coverageEligibilityResponseErrorExtension :: [Extension]
  , coverageEligibilityResponseErrorModifierExtension :: [Extension]
  , coverageEligibilityResponseErrorCode :: CodeableConcept
  }
--

instance ToJSON CoverageEligibilityResponseError where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (coverageEligibilityResponseErrorAttrId p)
    ,  "extension" .= toJSON (coverageEligibilityResponseErrorExtension p)
    ,  "modifierExtension" .= toJSON (coverageEligibilityResponseErrorModifierExtension p)
    ,  "code" .= toJSON (coverageEligibilityResponseErrorCode p)
    ]
instance FromJSON CoverageEligibilityResponseError where
  parseJSON = withObject "CoverageEligibilityResponseError" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:  "code"
        return CoverageEligibilityResponseError{
            coverageEligibilityResponseErrorAttrId = id
          , coverageEligibilityResponseErrorExtension = extension
          , coverageEligibilityResponseErrorModifierExtension = modifierExtension
          , coverageEligibilityResponseErrorCode = code
          }
instance Xmlbf.ToXml CoverageEligibilityResponseError where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (coverageEligibilityResponseErrorAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (coverageEligibilityResponseErrorExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (coverageEligibilityResponseErrorModifierExtension p))
             , Prop     "code" (HM.empty, Xmlbf.toXml (coverageEligibilityResponseErrorCode p))
             ]
instance Xmlbf.FromXml CoverageEligibilityResponseError where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" Xmlbf.fromXml
    return CoverageEligibilityResponseError {
            coverageEligibilityResponseErrorAttrId = id
          , coverageEligibilityResponseErrorExtension = extension
          , coverageEligibilityResponseErrorModifierExtension = modifierExtension
          , coverageEligibilityResponseErrorCode = code
          }



data CoverageEligibilityRequestSupportingInfo = CoverageEligibilityRequestSupportingInfo {
    coverageEligibilityRequestSupportingInfoAttrId :: Maybe Text
  , coverageEligibilityRequestSupportingInfoExtension :: [Extension]
  , coverageEligibilityRequestSupportingInfoModifierExtension :: [Extension]
  , coverageEligibilityRequestSupportingInfoSequence :: PositiveInt
  , coverageEligibilityRequestSupportingInfoInformation :: Reference
  , coverageEligibilityRequestSupportingInfoAppliesToAll :: Maybe Boolean
  }
--

instance ToJSON CoverageEligibilityRequestSupportingInfo where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (coverageEligibilityRequestSupportingInfoAttrId p)
    ,  "extension" .= toJSON (coverageEligibilityRequestSupportingInfoExtension p)
    ,  "modifierExtension" .= toJSON (coverageEligibilityRequestSupportingInfoModifierExtension p)
    ,  "sequence" .= toJSON (coverageEligibilityRequestSupportingInfoSequence p)
    ,  "information" .= toJSON (coverageEligibilityRequestSupportingInfoInformation p)
    ,  "appliesToAll" .= toJSON (coverageEligibilityRequestSupportingInfoAppliesToAll p)
    ]
instance FromJSON CoverageEligibilityRequestSupportingInfo where
  parseJSON = withObject "CoverageEligibilityRequestSupportingInfo" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        sequence <- o .:  "sequence"
        information <- o .:  "information"
        appliesToAll <- o .:? "appliesToAll"
        return CoverageEligibilityRequestSupportingInfo{
            coverageEligibilityRequestSupportingInfoAttrId = id
          , coverageEligibilityRequestSupportingInfoExtension = extension
          , coverageEligibilityRequestSupportingInfoModifierExtension = modifierExtension
          , coverageEligibilityRequestSupportingInfoSequence = sequence
          , coverageEligibilityRequestSupportingInfoInformation = information
          , coverageEligibilityRequestSupportingInfoAppliesToAll = appliesToAll
          }
instance Xmlbf.ToXml CoverageEligibilityRequestSupportingInfo where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (coverageEligibilityRequestSupportingInfoAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (coverageEligibilityRequestSupportingInfoExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (coverageEligibilityRequestSupportingInfoModifierExtension p))
             , Val      "sequence" (     toPositiveInt (coverageEligibilityRequestSupportingInfoSequence p))
             , Prop     "information" (HM.empty, Xmlbf.toXml (coverageEligibilityRequestSupportingInfoInformation p))
             , OptVal   "appliesToAll" (fmap toBoolean (coverageEligibilityRequestSupportingInfoAppliesToAll p))
             ]
instance Xmlbf.FromXml CoverageEligibilityRequestSupportingInfo where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    sequence <-            Xmlbf.pElement "sequence" (Xmlbf.pAttr "value")
    information <-            Xmlbf.pElement "information" Xmlbf.fromXml
    appliesToAll <- optional $ Xmlbf.pElement "appliesToAll" (Xmlbf.pAttr "value")
    return CoverageEligibilityRequestSupportingInfo {
            coverageEligibilityRequestSupportingInfoAttrId = id
          , coverageEligibilityRequestSupportingInfoExtension = extension
          , coverageEligibilityRequestSupportingInfoModifierExtension = modifierExtension
          , coverageEligibilityRequestSupportingInfoSequence =      fromPositiveInt sequence
          , coverageEligibilityRequestSupportingInfoInformation = information
          , coverageEligibilityRequestSupportingInfoAppliesToAll = fmap fromBoolean appliesToAll
          }



data CoverageEligibilityRequestInsurance = CoverageEligibilityRequestInsurance {
    coverageEligibilityRequestInsuranceAttrId :: Maybe Text
  , coverageEligibilityRequestInsuranceExtension :: [Extension]
  , coverageEligibilityRequestInsuranceModifierExtension :: [Extension]
  , coverageEligibilityRequestInsuranceFocal :: Maybe Boolean
  , coverageEligibilityRequestInsuranceCoverage :: Reference
  , coverageEligibilityRequestInsuranceBusinessArrangement :: Maybe Text
  }
--

instance ToJSON CoverageEligibilityRequestInsurance where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (coverageEligibilityRequestInsuranceAttrId p)
    ,  "extension" .= toJSON (coverageEligibilityRequestInsuranceExtension p)
    ,  "modifierExtension" .= toJSON (coverageEligibilityRequestInsuranceModifierExtension p)
    ,  "focal" .= toJSON (coverageEligibilityRequestInsuranceFocal p)
    ,  "coverage" .= toJSON (coverageEligibilityRequestInsuranceCoverage p)
    ,  "businessArrangement" .= toJSON (coverageEligibilityRequestInsuranceBusinessArrangement p)
    ]
instance FromJSON CoverageEligibilityRequestInsurance where
  parseJSON = withObject "CoverageEligibilityRequestInsurance" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        focal <- o .:? "focal"
        coverage <- o .:  "coverage"
        businessArrangement <- o .:? "businessArrangement"
        return CoverageEligibilityRequestInsurance{
            coverageEligibilityRequestInsuranceAttrId = id
          , coverageEligibilityRequestInsuranceExtension = extension
          , coverageEligibilityRequestInsuranceModifierExtension = modifierExtension
          , coverageEligibilityRequestInsuranceFocal = focal
          , coverageEligibilityRequestInsuranceCoverage = coverage
          , coverageEligibilityRequestInsuranceBusinessArrangement = businessArrangement
          }
instance Xmlbf.ToXml CoverageEligibilityRequestInsurance where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (coverageEligibilityRequestInsuranceAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (coverageEligibilityRequestInsuranceExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (coverageEligibilityRequestInsuranceModifierExtension p))
             , OptVal   "focal" (fmap toBoolean (coverageEligibilityRequestInsuranceFocal p))
             , Prop     "coverage" (HM.empty, Xmlbf.toXml (coverageEligibilityRequestInsuranceCoverage p))
             , OptVal   "businessArrangement" (fmap toString (coverageEligibilityRequestInsuranceBusinessArrangement p))
             ]
instance Xmlbf.FromXml CoverageEligibilityRequestInsurance where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    focal <- optional $ Xmlbf.pElement "focal" (Xmlbf.pAttr "value")
    coverage <-            Xmlbf.pElement "coverage" Xmlbf.fromXml
    businessArrangement <- optional $ Xmlbf.pElement "businessArrangement" (Xmlbf.pAttr "value")
    return CoverageEligibilityRequestInsurance {
            coverageEligibilityRequestInsuranceAttrId = id
          , coverageEligibilityRequestInsuranceExtension = extension
          , coverageEligibilityRequestInsuranceModifierExtension = modifierExtension
          , coverageEligibilityRequestInsuranceFocal = fmap fromBoolean focal
          , coverageEligibilityRequestInsuranceCoverage = coverage
          , coverageEligibilityRequestInsuranceBusinessArrangement = fmap fromString businessArrangement
          }



data CoverageEligibilityRequestItem = CoverageEligibilityRequestItem {
    coverageEligibilityRequestItemAttrId :: Maybe Text
  , coverageEligibilityRequestItemExtension :: [Extension]
  , coverageEligibilityRequestItemModifierExtension :: [Extension]
  , coverageEligibilityRequestItemSupportingInfoSequence :: [PositiveInt]
  , coverageEligibilityRequestItemCategory :: Maybe CodeableConcept
  , coverageEligibilityRequestItemProductOrService :: Maybe CodeableConcept
  , coverageEligibilityRequestItemModifier :: [CodeableConcept]
  , coverageEligibilityRequestItemProvider :: Maybe Reference
  , coverageEligibilityRequestItemQuantity :: Maybe Quantity
  , coverageEligibilityRequestItemUnitPrice :: Maybe Money
  , coverageEligibilityRequestItemFacility :: Maybe Reference
  , coverageEligibilityRequestItemDiagnosis :: [CoverageEligibilityRequestDiagnosis]
  , coverageEligibilityRequestItemDetail :: [Reference]
  }
--

instance ToJSON CoverageEligibilityRequestItem where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (coverageEligibilityRequestItemAttrId p)
    ,  "extension" .= toJSON (coverageEligibilityRequestItemExtension p)
    ,  "modifierExtension" .= toJSON (coverageEligibilityRequestItemModifierExtension p)
    ,  "supportingInfoSequence" .= toJSON (coverageEligibilityRequestItemSupportingInfoSequence p)
    ,  "category" .= toJSON (coverageEligibilityRequestItemCategory p)
    ,  "productOrService" .= toJSON (coverageEligibilityRequestItemProductOrService p)
    ,  "modifier" .= toJSON (coverageEligibilityRequestItemModifier p)
    ,  "provider" .= toJSON (coverageEligibilityRequestItemProvider p)
    ,  "quantity" .= toJSON (coverageEligibilityRequestItemQuantity p)
    ,  "unitPrice" .= toJSON (coverageEligibilityRequestItemUnitPrice p)
    ,  "facility" .= toJSON (coverageEligibilityRequestItemFacility p)
    ,  "diagnosis" .= toJSON (coverageEligibilityRequestItemDiagnosis p)
    ,  "detail" .= toJSON (coverageEligibilityRequestItemDetail p)
    ]
instance FromJSON CoverageEligibilityRequestItem where
  parseJSON = withObject "CoverageEligibilityRequestItem" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        supportingInfoSequence <- o .:? "supportingInfoSequence" .!= []
        category <- o .:? "category"
        productOrService <- o .:? "productOrService"
        modifier <- o .:? "modifier" .!= []
        provider <- o .:? "provider"
        quantity <- o .:? "quantity"
        unitPrice <- o .:? "unitPrice"
        facility <- o .:? "facility"
        diagnosis <- o .:? "diagnosis" .!= []
        detail <- o .:? "detail" .!= []
        return CoverageEligibilityRequestItem{
            coverageEligibilityRequestItemAttrId = id
          , coverageEligibilityRequestItemExtension = extension
          , coverageEligibilityRequestItemModifierExtension = modifierExtension
          , coverageEligibilityRequestItemSupportingInfoSequence = supportingInfoSequence
          , coverageEligibilityRequestItemCategory = category
          , coverageEligibilityRequestItemProductOrService = productOrService
          , coverageEligibilityRequestItemModifier = modifier
          , coverageEligibilityRequestItemProvider = provider
          , coverageEligibilityRequestItemQuantity = quantity
          , coverageEligibilityRequestItemUnitPrice = unitPrice
          , coverageEligibilityRequestItemFacility = facility
          , coverageEligibilityRequestItemDiagnosis = diagnosis
          , coverageEligibilityRequestItemDetail = detail
          }
instance Xmlbf.ToXml CoverageEligibilityRequestItem where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (coverageEligibilityRequestItemAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (coverageEligibilityRequestItemExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (coverageEligibilityRequestItemModifierExtension p))
             , ValList  "supportingInfoSequence" (fmap toPositiveInt (coverageEligibilityRequestItemSupportingInfoSequence p))
             , OptProp  "category" (fmap Xmlbf.toXml (coverageEligibilityRequestItemCategory p))
             , OptProp  "productOrService" (fmap Xmlbf.toXml (coverageEligibilityRequestItemProductOrService p))
             , PropList "modifier" (fmap Xmlbf.toXml (coverageEligibilityRequestItemModifier p))
             , OptProp  "provider" (fmap Xmlbf.toXml (coverageEligibilityRequestItemProvider p))
             , OptProp  "quantity" (fmap Xmlbf.toXml (coverageEligibilityRequestItemQuantity p))
             , OptProp  "unitPrice" (fmap Xmlbf.toXml (coverageEligibilityRequestItemUnitPrice p))
             , OptProp  "facility" (fmap Xmlbf.toXml (coverageEligibilityRequestItemFacility p))
             , PropList "diagnosis" (fmap Xmlbf.toXml (coverageEligibilityRequestItemDiagnosis p))
             , PropList "detail" (fmap Xmlbf.toXml (coverageEligibilityRequestItemDetail p))
             ]
instance Xmlbf.FromXml CoverageEligibilityRequestItem where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    supportingInfoSequence <- many     $ Xmlbf.pElement "supportingInfoSequence" (Xmlbf.pAttr "value")
    category <- optional $ Xmlbf.pElement "category" Xmlbf.fromXml
    productOrService <- optional $ Xmlbf.pElement "productOrService" Xmlbf.fromXml
    modifier <- many     $ Xmlbf.pElement "modifier" Xmlbf.fromXml
    provider <- optional $ Xmlbf.pElement "provider" Xmlbf.fromXml
    quantity <- optional $ Xmlbf.pElement "quantity" Xmlbf.fromXml
    unitPrice <- optional $ Xmlbf.pElement "unitPrice" Xmlbf.fromXml
    facility <- optional $ Xmlbf.pElement "facility" Xmlbf.fromXml
    diagnosis <- many     $ Xmlbf.pElement "diagnosis" Xmlbf.fromXml
    detail <- many     $ Xmlbf.pElement "detail" Xmlbf.fromXml
    return CoverageEligibilityRequestItem {
            coverageEligibilityRequestItemAttrId = id
          , coverageEligibilityRequestItemExtension = extension
          , coverageEligibilityRequestItemModifierExtension = modifierExtension
          , coverageEligibilityRequestItemSupportingInfoSequence = fmap fromPositiveInt supportingInfoSequence
          , coverageEligibilityRequestItemCategory = category
          , coverageEligibilityRequestItemProductOrService = productOrService
          , coverageEligibilityRequestItemModifier = modifier
          , coverageEligibilityRequestItemProvider = provider
          , coverageEligibilityRequestItemQuantity = quantity
          , coverageEligibilityRequestItemUnitPrice = unitPrice
          , coverageEligibilityRequestItemFacility = facility
          , coverageEligibilityRequestItemDiagnosis = diagnosis
          , coverageEligibilityRequestItemDetail = detail
          }



data CoverageEligibilityResponseInsurance = CoverageEligibilityResponseInsurance {
    coverageEligibilityResponseInsuranceAttrId :: Maybe Text
  , coverageEligibilityResponseInsuranceExtension :: [Extension]
  , coverageEligibilityResponseInsuranceModifierExtension :: [Extension]
  , coverageEligibilityResponseInsuranceCoverage :: Reference
  , coverageEligibilityResponseInsuranceInforce :: Maybe Boolean
  , coverageEligibilityResponseInsuranceBenefitPeriod :: Maybe Period
  , coverageEligibilityResponseInsuranceItem :: [CoverageEligibilityResponseItem]
  }
--

instance ToJSON CoverageEligibilityResponseInsurance where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (coverageEligibilityResponseInsuranceAttrId p)
    ,  "extension" .= toJSON (coverageEligibilityResponseInsuranceExtension p)
    ,  "modifierExtension" .= toJSON (coverageEligibilityResponseInsuranceModifierExtension p)
    ,  "coverage" .= toJSON (coverageEligibilityResponseInsuranceCoverage p)
    ,  "inforce" .= toJSON (coverageEligibilityResponseInsuranceInforce p)
    ,  "benefitPeriod" .= toJSON (coverageEligibilityResponseInsuranceBenefitPeriod p)
    ,  "item" .= toJSON (coverageEligibilityResponseInsuranceItem p)
    ]
instance FromJSON CoverageEligibilityResponseInsurance where
  parseJSON = withObject "CoverageEligibilityResponseInsurance" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        coverage <- o .:  "coverage"
        inforce <- o .:? "inforce"
        benefitPeriod <- o .:? "benefitPeriod"
        item <- o .:? "item" .!= []
        return CoverageEligibilityResponseInsurance{
            coverageEligibilityResponseInsuranceAttrId = id
          , coverageEligibilityResponseInsuranceExtension = extension
          , coverageEligibilityResponseInsuranceModifierExtension = modifierExtension
          , coverageEligibilityResponseInsuranceCoverage = coverage
          , coverageEligibilityResponseInsuranceInforce = inforce
          , coverageEligibilityResponseInsuranceBenefitPeriod = benefitPeriod
          , coverageEligibilityResponseInsuranceItem = item
          }
instance Xmlbf.ToXml CoverageEligibilityResponseInsurance where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (coverageEligibilityResponseInsuranceAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (coverageEligibilityResponseInsuranceExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (coverageEligibilityResponseInsuranceModifierExtension p))
             , Prop     "coverage" (HM.empty, Xmlbf.toXml (coverageEligibilityResponseInsuranceCoverage p))
             , OptVal   "inforce" (fmap toBoolean (coverageEligibilityResponseInsuranceInforce p))
             , OptProp  "benefitPeriod" (fmap Xmlbf.toXml (coverageEligibilityResponseInsuranceBenefitPeriod p))
             , PropList "item" (fmap Xmlbf.toXml (coverageEligibilityResponseInsuranceItem p))
             ]
instance Xmlbf.FromXml CoverageEligibilityResponseInsurance where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    coverage <-            Xmlbf.pElement "coverage" Xmlbf.fromXml
    inforce <- optional $ Xmlbf.pElement "inforce" (Xmlbf.pAttr "value")
    benefitPeriod <- optional $ Xmlbf.pElement "benefitPeriod" Xmlbf.fromXml
    item <- many     $ Xmlbf.pElement "item" Xmlbf.fromXml
    return CoverageEligibilityResponseInsurance {
            coverageEligibilityResponseInsuranceAttrId = id
          , coverageEligibilityResponseInsuranceExtension = extension
          , coverageEligibilityResponseInsuranceModifierExtension = modifierExtension
          , coverageEligibilityResponseInsuranceCoverage = coverage
          , coverageEligibilityResponseInsuranceInforce = fmap fromBoolean inforce
          , coverageEligibilityResponseInsuranceBenefitPeriod = benefitPeriod
          , coverageEligibilityResponseInsuranceItem = item
          }



data CoverageCostToBeneficiaryValue
    = CoverageCostToBeneficiaryValueQuantity Quantity
    | CoverageCostToBeneficiaryValueMoney Money
    deriving (Eq, Show)

data CoverageCostToBeneficiary = CoverageCostToBeneficiary {
    coverageCostToBeneficiaryAttrId :: Maybe Text
  , coverageCostToBeneficiaryExtension :: [Extension]
  , coverageCostToBeneficiaryModifierExtension :: [Extension]
  , coverageCostToBeneficiaryType :: Maybe CodeableConcept
  , coverageCostToBeneficiaryValue :: CoverageCostToBeneficiaryValue
  , coverageCostToBeneficiaryException :: [CoverageException]
  }
--

instance ToJSON CoverageCostToBeneficiary where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (coverageCostToBeneficiaryAttrId p)
    ,  "extension" .= toJSON (coverageCostToBeneficiaryExtension p)
    ,  "modifierExtension" .= toJSON (coverageCostToBeneficiaryModifierExtension p)
    ,  "type" .= toJSON (coverageCostToBeneficiaryType p)
    , toValueJSON (coverageCostToBeneficiaryValue p)
    ,  "exception" .= toJSON (coverageCostToBeneficiaryException p)
    ]
    where 
      toValueJSON (     (CoverageCostToBeneficiaryValueQuantity c)) = ("value", toJSON c)
      toValueJSON (     (CoverageCostToBeneficiaryValueMoney c)) = ("value", toJSON c)
instance FromJSON CoverageCostToBeneficiary where
  parseJSON = withObject "CoverageCostToBeneficiary" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:? "type"
        value <- parseValue o
        exception <- o .:? "exception" .!= []
        return CoverageCostToBeneficiary{
            coverageCostToBeneficiaryAttrId = id
          , coverageCostToBeneficiaryExtension = extension
          , coverageCostToBeneficiaryModifierExtension = modifierExtension
          , coverageCostToBeneficiaryType = ty
          , coverageCostToBeneficiaryValue = value
          , coverageCostToBeneficiaryException = exception
          }
    where 
      parseValue o = parseValueQuantity o <|> parseValueMoney o
      parseValueQuantity o = do
                has <- o .: "valueQuantity"
                return $ CoverageCostToBeneficiaryValueQuantity has
      parseValueMoney o = do
                has <- o .: "valueMoney"
                return $ CoverageCostToBeneficiaryValueMoney has
instance Xmlbf.ToXml CoverageCostToBeneficiary where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (coverageCostToBeneficiaryAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (coverageCostToBeneficiaryExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (coverageCostToBeneficiaryModifierExtension p))
             , OptProp  "type" (fmap Xmlbf.toXml (coverageCostToBeneficiaryType p))
             , toValueXml (coverageCostToBeneficiaryValue p)
             , PropList "exception" (fmap Xmlbf.toXml (coverageCostToBeneficiaryException p))
             ]
       where 
          toValueXml (     (CoverageCostToBeneficiaryValueQuantity p)) = Prop     "valueQuantity" (HM.empty, Xmlbf.toXml p)
          toValueXml (     (CoverageCostToBeneficiaryValueMoney p)) = Prop     "valueMoney" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml CoverageCostToBeneficiary where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <- optional $ Xmlbf.pElement "type" Xmlbf.fromXml
    value <- fromValueXml
    exception <- many     $ Xmlbf.pElement "exception" Xmlbf.fromXml
    return CoverageCostToBeneficiary {
            coverageCostToBeneficiaryAttrId = id
          , coverageCostToBeneficiaryExtension = extension
          , coverageCostToBeneficiaryModifierExtension = modifierExtension
          , coverageCostToBeneficiaryType = ty
          , coverageCostToBeneficiaryValue = value
          , coverageCostToBeneficiaryException = exception
          }

    where 
      fromValueXml = parseValueQuantity <|> parseValueMoney
      parseValueQuantity = do
                has <- Xmlbf.pElement "valueQuantity" Xmlbf.fromXml
                return $ CoverageCostToBeneficiaryValueQuantity (                      has)
      parseValueMoney = do
                has <- Xmlbf.pElement "valueMoney" Xmlbf.fromXml
                return $ CoverageCostToBeneficiaryValueMoney (                      has)


data CoverageEligibilityResponseItem = CoverageEligibilityResponseItem {
    coverageEligibilityResponseItemAttrId :: Maybe Text
  , coverageEligibilityResponseItemExtension :: [Extension]
  , coverageEligibilityResponseItemModifierExtension :: [Extension]
  , coverageEligibilityResponseItemCategory :: Maybe CodeableConcept
  , coverageEligibilityResponseItemProductOrService :: Maybe CodeableConcept
  , coverageEligibilityResponseItemModifier :: [CodeableConcept]
  , coverageEligibilityResponseItemProvider :: Maybe Reference
  , coverageEligibilityResponseItemExcluded :: Maybe Boolean
  , coverageEligibilityResponseItemName :: Maybe Text
  , coverageEligibilityResponseItemDescription :: Maybe Text
  , coverageEligibilityResponseItemNetwork :: Maybe CodeableConcept
  , coverageEligibilityResponseItemUnit :: Maybe CodeableConcept
  , coverageEligibilityResponseItemTerm :: Maybe CodeableConcept
  , coverageEligibilityResponseItemBenefit :: [CoverageEligibilityResponseBenefit]
  , coverageEligibilityResponseItemAuthorizationRequired :: Maybe Boolean
  , coverageEligibilityResponseItemAuthorizationSupporting :: [CodeableConcept]
  , coverageEligibilityResponseItemAuthorizationUrl :: Maybe Uri
  }
--

instance ToJSON CoverageEligibilityResponseItem where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (coverageEligibilityResponseItemAttrId p)
    ,  "extension" .= toJSON (coverageEligibilityResponseItemExtension p)
    ,  "modifierExtension" .= toJSON (coverageEligibilityResponseItemModifierExtension p)
    ,  "category" .= toJSON (coverageEligibilityResponseItemCategory p)
    ,  "productOrService" .= toJSON (coverageEligibilityResponseItemProductOrService p)
    ,  "modifier" .= toJSON (coverageEligibilityResponseItemModifier p)
    ,  "provider" .= toJSON (coverageEligibilityResponseItemProvider p)
    ,  "excluded" .= toJSON (coverageEligibilityResponseItemExcluded p)
    ,  "name" .= toJSON (coverageEligibilityResponseItemName p)
    ,  "description" .= toJSON (coverageEligibilityResponseItemDescription p)
    ,  "network" .= toJSON (coverageEligibilityResponseItemNetwork p)
    ,  "unit" .= toJSON (coverageEligibilityResponseItemUnit p)
    ,  "term" .= toJSON (coverageEligibilityResponseItemTerm p)
    ,  "benefit" .= toJSON (coverageEligibilityResponseItemBenefit p)
    ,  "authorizationRequired" .= toJSON (coverageEligibilityResponseItemAuthorizationRequired p)
    ,  "authorizationSupporting" .= toJSON (coverageEligibilityResponseItemAuthorizationSupporting p)
    ,  "authorizationUrl" .= toJSON (coverageEligibilityResponseItemAuthorizationUrl p)
    ]
instance FromJSON CoverageEligibilityResponseItem where
  parseJSON = withObject "CoverageEligibilityResponseItem" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        category <- o .:? "category"
        productOrService <- o .:? "productOrService"
        modifier <- o .:? "modifier" .!= []
        provider <- o .:? "provider"
        excluded <- o .:? "excluded"
        name <- o .:? "name"
        description <- o .:? "description"
        network <- o .:? "network"
        unit <- o .:? "unit"
        term <- o .:? "term"
        benefit <- o .:? "benefit" .!= []
        authorizationRequired <- o .:? "authorizationRequired"
        authorizationSupporting <- o .:? "authorizationSupporting" .!= []
        authorizationUrl <- o .:? "authorizationUrl"
        return CoverageEligibilityResponseItem{
            coverageEligibilityResponseItemAttrId = id
          , coverageEligibilityResponseItemExtension = extension
          , coverageEligibilityResponseItemModifierExtension = modifierExtension
          , coverageEligibilityResponseItemCategory = category
          , coverageEligibilityResponseItemProductOrService = productOrService
          , coverageEligibilityResponseItemModifier = modifier
          , coverageEligibilityResponseItemProvider = provider
          , coverageEligibilityResponseItemExcluded = excluded
          , coverageEligibilityResponseItemName = name
          , coverageEligibilityResponseItemDescription = description
          , coverageEligibilityResponseItemNetwork = network
          , coverageEligibilityResponseItemUnit = unit
          , coverageEligibilityResponseItemTerm = term
          , coverageEligibilityResponseItemBenefit = benefit
          , coverageEligibilityResponseItemAuthorizationRequired = authorizationRequired
          , coverageEligibilityResponseItemAuthorizationSupporting = authorizationSupporting
          , coverageEligibilityResponseItemAuthorizationUrl = authorizationUrl
          }
instance Xmlbf.ToXml CoverageEligibilityResponseItem where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (coverageEligibilityResponseItemAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (coverageEligibilityResponseItemExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (coverageEligibilityResponseItemModifierExtension p))
             , OptProp  "category" (fmap Xmlbf.toXml (coverageEligibilityResponseItemCategory p))
             , OptProp  "productOrService" (fmap Xmlbf.toXml (coverageEligibilityResponseItemProductOrService p))
             , PropList "modifier" (fmap Xmlbf.toXml (coverageEligibilityResponseItemModifier p))
             , OptProp  "provider" (fmap Xmlbf.toXml (coverageEligibilityResponseItemProvider p))
             , OptVal   "excluded" (fmap toBoolean (coverageEligibilityResponseItemExcluded p))
             , OptVal   "name" (fmap toString (coverageEligibilityResponseItemName p))
             , OptVal   "description" (fmap toString (coverageEligibilityResponseItemDescription p))
             , OptProp  "network" (fmap Xmlbf.toXml (coverageEligibilityResponseItemNetwork p))
             , OptProp  "unit" (fmap Xmlbf.toXml (coverageEligibilityResponseItemUnit p))
             , OptProp  "term" (fmap Xmlbf.toXml (coverageEligibilityResponseItemTerm p))
             , PropList "benefit" (fmap Xmlbf.toXml (coverageEligibilityResponseItemBenefit p))
             , OptVal   "authorizationRequired" (fmap toBoolean (coverageEligibilityResponseItemAuthorizationRequired p))
             , PropList "authorizationSupporting" (fmap Xmlbf.toXml (coverageEligibilityResponseItemAuthorizationSupporting p))
             , OptVal   "authorizationUrl" (fmap toUri (coverageEligibilityResponseItemAuthorizationUrl p))
             ]
instance Xmlbf.FromXml CoverageEligibilityResponseItem where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    category <- optional $ Xmlbf.pElement "category" Xmlbf.fromXml
    productOrService <- optional $ Xmlbf.pElement "productOrService" Xmlbf.fromXml
    modifier <- many     $ Xmlbf.pElement "modifier" Xmlbf.fromXml
    provider <- optional $ Xmlbf.pElement "provider" Xmlbf.fromXml
    excluded <- optional $ Xmlbf.pElement "excluded" (Xmlbf.pAttr "value")
    name <- optional $ Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    network <- optional $ Xmlbf.pElement "network" Xmlbf.fromXml
    unit <- optional $ Xmlbf.pElement "unit" Xmlbf.fromXml
    term <- optional $ Xmlbf.pElement "term" Xmlbf.fromXml
    benefit <- many     $ Xmlbf.pElement "benefit" Xmlbf.fromXml
    authorizationRequired <- optional $ Xmlbf.pElement "authorizationRequired" (Xmlbf.pAttr "value")
    authorizationSupporting <- many     $ Xmlbf.pElement "authorizationSupporting" Xmlbf.fromXml
    authorizationUrl <- optional $ Xmlbf.pElement "authorizationUrl" (Xmlbf.pAttr "value")
    return CoverageEligibilityResponseItem {
            coverageEligibilityResponseItemAttrId = id
          , coverageEligibilityResponseItemExtension = extension
          , coverageEligibilityResponseItemModifierExtension = modifierExtension
          , coverageEligibilityResponseItemCategory = category
          , coverageEligibilityResponseItemProductOrService = productOrService
          , coverageEligibilityResponseItemModifier = modifier
          , coverageEligibilityResponseItemProvider = provider
          , coverageEligibilityResponseItemExcluded = fmap fromBoolean excluded
          , coverageEligibilityResponseItemName = fmap fromString name
          , coverageEligibilityResponseItemDescription = fmap fromString description
          , coverageEligibilityResponseItemNetwork = network
          , coverageEligibilityResponseItemUnit = unit
          , coverageEligibilityResponseItemTerm = term
          , coverageEligibilityResponseItemBenefit = benefit
          , coverageEligibilityResponseItemAuthorizationRequired = fmap fromBoolean authorizationRequired
          , coverageEligibilityResponseItemAuthorizationSupporting = authorizationSupporting
          , coverageEligibilityResponseItemAuthorizationUrl = fmap fromUri authorizationUrl
          }



data CoverageEligibilityResponseStatus
    = CERS2Active
    | CERS2Cancelled
    | CERS2Draft
    | CERS2EnteredInError
  deriving (Eq, Show)

instance ToJSON CoverageEligibilityResponseStatus where
    toJSON CERS2Active = String "active"
    toJSON CERS2Cancelled = String "cancelled"
    toJSON CERS2Draft = String "draft"
    toJSON CERS2EnteredInError = String "entered-in-error"
instance FromJSON CoverageEligibilityResponseStatus where
    parseJSON "active" = return CERS2Active
    parseJSON "cancelled" = return CERS2Cancelled
    parseJSON "draft" = return CERS2Draft
    parseJSON "entered-in-error" = return CERS2EnteredInError

toCoverageEligibilityResponseStatus CERS2Active = "active"
toCoverageEligibilityResponseStatus CERS2Cancelled = "cancelled"
toCoverageEligibilityResponseStatus CERS2Draft = "draft"
toCoverageEligibilityResponseStatus CERS2EnteredInError = "entered-in-error"
fromCoverageEligibilityResponseStatus "active" = CERS2Active
fromCoverageEligibilityResponseStatus "cancelled" = CERS2Cancelled
fromCoverageEligibilityResponseStatus "draft" = CERS2Draft
fromCoverageEligibilityResponseStatus "entered-in-error" = CERS2EnteredInError


data CoverageEligibilityResponsePurpose
    = CERP2AuthRequirements
    | CERP2Benefits
    | CERP2Discovery
    | CERP2Validation
  deriving (Eq, Show)

instance ToJSON CoverageEligibilityResponsePurpose where
    toJSON CERP2AuthRequirements = String "auth-requirements"
    toJSON CERP2Benefits = String "benefits"
    toJSON CERP2Discovery = String "discovery"
    toJSON CERP2Validation = String "validation"
instance FromJSON CoverageEligibilityResponsePurpose where
    parseJSON "auth-requirements" = return CERP2AuthRequirements
    parseJSON "benefits" = return CERP2Benefits
    parseJSON "discovery" = return CERP2Discovery
    parseJSON "validation" = return CERP2Validation

toCoverageEligibilityResponsePurpose CERP2AuthRequirements = "auth-requirements"
toCoverageEligibilityResponsePurpose CERP2Benefits = "benefits"
toCoverageEligibilityResponsePurpose CERP2Discovery = "discovery"
toCoverageEligibilityResponsePurpose CERP2Validation = "validation"
fromCoverageEligibilityResponsePurpose "auth-requirements" = CERP2AuthRequirements
fromCoverageEligibilityResponsePurpose "benefits" = CERP2Benefits
fromCoverageEligibilityResponsePurpose "discovery" = CERP2Discovery
fromCoverageEligibilityResponsePurpose "validation" = CERP2Validation


data CoverageEligibilityResponseOutcome
    = CEROQueued
    | CEROComplete
    | CEROError
    | CEROPartial
  deriving (Eq, Show)

instance ToJSON CoverageEligibilityResponseOutcome where
    toJSON CEROQueued = String "queued"
    toJSON CEROComplete = String "complete"
    toJSON CEROError = String "error"
    toJSON CEROPartial = String "partial"
instance FromJSON CoverageEligibilityResponseOutcome where
    parseJSON "queued" = return CEROQueued
    parseJSON "complete" = return CEROComplete
    parseJSON "error" = return CEROError
    parseJSON "partial" = return CEROPartial

toCoverageEligibilityResponseOutcome CEROQueued = "queued"
toCoverageEligibilityResponseOutcome CEROComplete = "complete"
toCoverageEligibilityResponseOutcome CEROError = "error"
toCoverageEligibilityResponseOutcome CEROPartial = "partial"
fromCoverageEligibilityResponseOutcome "queued" = CEROQueued
fromCoverageEligibilityResponseOutcome "complete" = CEROComplete
fromCoverageEligibilityResponseOutcome "error" = CEROError
fromCoverageEligibilityResponseOutcome "partial" = CEROPartial


data CoverageEligibilityResponseServiced
    = CoverageEligibilityResponseServicedDate Date
    | CoverageEligibilityResponseServicedPeriod Period
    deriving (Eq, Show)

data CoverageEligibilityResponse = CoverageEligibilityResponse {
    coverageEligibilityResponseId :: Maybe Id
  , coverageEligibilityResponseMeta :: Maybe Meta
  , coverageEligibilityResponseImplicitRules :: Maybe Uri
  , coverageEligibilityResponseLanguage :: Maybe Language
  , coverageEligibilityResponseText :: Maybe Narrative
--    coverageEligibilityResponseContained :: [ResourceContainer]
  , coverageEligibilityResponseExtension :: [Extension]
  , coverageEligibilityResponseModifierExtension :: [Extension]
  , coverageEligibilityResponseIdentifier :: [Identifier]
  , coverageEligibilityResponseStatus :: CoverageEligibilityResponseStatus
  , coverageEligibilityResponsePurpose :: [CoverageEligibilityResponsePurpose]
  , coverageEligibilityResponsePatient :: Reference
  , coverageEligibilityResponseServicedDate :: Maybe Date
  , coverageEligibilityResponseServicedPeriod :: Maybe Period
  , coverageEligibilityResponseCreated :: DateTime
  , coverageEligibilityResponseRequestor :: Maybe Reference
  , coverageEligibilityResponseRequest :: Reference
  , coverageEligibilityResponseOutcome :: CoverageEligibilityResponseOutcome
  , coverageEligibilityResponseDisposition :: Maybe Text
  , coverageEligibilityResponseInsurer :: Reference
  , coverageEligibilityResponseInsurance :: [CoverageEligibilityResponseInsurance]
  , coverageEligibilityResponsePreAuthRef :: Maybe Text
  , coverageEligibilityResponseForm :: Maybe CodeableConcept
  , coverageEligibilityResponseError :: [CoverageEligibilityResponseError]
  }
--

instance ToJSON CoverageEligibilityResponse where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "CoverageEligibilityResponse")
    ,  "id" .= toJSON (coverageEligibilityResponseId p)
    ,  "meta" .= toJSON (coverageEligibilityResponseMeta p)
    ,  "implicitRules" .= toJSON (coverageEligibilityResponseImplicitRules p)
    ,  "language" .= toJSON (coverageEligibilityResponseLanguage p)
    ,  "text" .= toJSON (coverageEligibilityResponseText p)
--    , "contained" .= toJSON (coverageEligibilityResponseContained p)
    ,  "extension" .= toJSON (coverageEligibilityResponseExtension p)
    ,  "modifierExtension" .= toJSON (coverageEligibilityResponseModifierExtension p)
    ,  "identifier" .= toJSON (coverageEligibilityResponseIdentifier p)
    ,  "status" .= toJSON (coverageEligibilityResponseStatus p)
    ,  "purpose" .= toJSON (coverageEligibilityResponsePurpose p)
    ,  "patient" .= toJSON (coverageEligibilityResponsePatient p)
    ,  "servicedDate" .= toJSON (coverageEligibilityResponseServicedDate p)
    ,  "servicedPeriod" .= toJSON (coverageEligibilityResponseServicedPeriod p)
    ,  "created" .= toJSON (coverageEligibilityResponseCreated p)
    ,  "requestor" .= toJSON (coverageEligibilityResponseRequestor p)
    ,  "request" .= toJSON (coverageEligibilityResponseRequest p)
    ,  "outcome" .= toJSON (coverageEligibilityResponseOutcome p)
    ,  "disposition" .= toJSON (coverageEligibilityResponseDisposition p)
    ,  "insurer" .= toJSON (coverageEligibilityResponseInsurer p)
    ,  "insurance" .= toJSON (coverageEligibilityResponseInsurance p)
    ,  "preAuthRef" .= toJSON (coverageEligibilityResponsePreAuthRef p)
    ,  "form" .= toJSON (coverageEligibilityResponseForm p)
    ,  "error" .= toJSON (coverageEligibilityResponseError p)
    ]
instance FromJSON CoverageEligibilityResponse where
  parseJSON = withObject "CoverageEligibilityResponse" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "CoverageEligibilityResponse" -> do
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
        purpose <- o .:? "purpose" .!= []
        patient <- o .:  "patient"
        servicedDate <- o .:? "servicedDate"
        servicedPeriod <- o .:? "servicedPeriod"
        created <- o .:  "created"
        requestor <- o .:? "requestor"
        request <- o .:  "request"
        outcome <- o .:  "outcome"
        disposition <- o .:? "disposition"
        insurer <- o .:  "insurer"
        insurance <- o .:? "insurance" .!= []
        preAuthRef <- o .:? "preAuthRef"
        form <- o .:? "form"
        error <- o .:? "error" .!= []
        return CoverageEligibilityResponse{
            coverageEligibilityResponseId = id
          , coverageEligibilityResponseMeta = meta
          , coverageEligibilityResponseImplicitRules = implicitRules
          , coverageEligibilityResponseLanguage = language
          , coverageEligibilityResponseText = text
--          , coverageEligibilityResponseContained = contained
          , coverageEligibilityResponseExtension = extension
          , coverageEligibilityResponseModifierExtension = modifierExtension
          , coverageEligibilityResponseIdentifier = identifier
          , coverageEligibilityResponseStatus = status
          , coverageEligibilityResponsePurpose = purpose
          , coverageEligibilityResponsePatient = patient
          , coverageEligibilityResponseServicedDate = servicedDate
          , coverageEligibilityResponseServicedPeriod = servicedPeriod
          , coverageEligibilityResponseCreated = created
          , coverageEligibilityResponseRequestor = requestor
          , coverageEligibilityResponseRequest = request
          , coverageEligibilityResponseOutcome = outcome
          , coverageEligibilityResponseDisposition = disposition
          , coverageEligibilityResponseInsurer = insurer
          , coverageEligibilityResponseInsurance = insurance
          , coverageEligibilityResponsePreAuthRef = preAuthRef
          , coverageEligibilityResponseForm = form
          , coverageEligibilityResponseError = error
          }
      _ -> fail "not a CoverageEligibilityResponse"
instance Xmlbf.ToXml CoverageEligibilityResponse where
  toXml p = Xmlbf.element "CoverageEligibilityResponse" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (coverageEligibilityResponseId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (coverageEligibilityResponseMeta p))
             , OptVal   "implicitRules" (fmap toUri (coverageEligibilityResponseImplicitRules p))
             , OptVal   "language" (fmap toLanguage (coverageEligibilityResponseLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (coverageEligibilityResponseText p))
--             , PropList "contained" (fmap Xmlbf.toXml (coverageEligibilityResponseContained p))
             , PropList "extension" (fmap Xmlbf.toXml (coverageEligibilityResponseExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (coverageEligibilityResponseModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (coverageEligibilityResponseIdentifier p))
             , Val      "status" (     toCoverageEligibilityResponseStatus (coverageEligibilityResponseStatus p))
             , ValList  "purpose" (fmap toCoverageEligibilityResponsePurpose (coverageEligibilityResponsePurpose p))
             , Prop     "patient" (HM.empty, Xmlbf.toXml (coverageEligibilityResponsePatient p))
             , OptVal   "servicedDate" (fmap toDate (coverageEligibilityResponseServicedDate p))
             , OptProp  "servicedPeriod" (fmap Xmlbf.toXml (coverageEligibilityResponseServicedPeriod p))
             , Val      "created" (     toDateTime (coverageEligibilityResponseCreated p))
             , OptProp  "requestor" (fmap Xmlbf.toXml (coverageEligibilityResponseRequestor p))
             , Prop     "request" (HM.empty, Xmlbf.toXml (coverageEligibilityResponseRequest p))
             , Val      "outcome" (     toCoverageEligibilityResponseOutcome (coverageEligibilityResponseOutcome p))
             , OptVal   "disposition" (fmap toString (coverageEligibilityResponseDisposition p))
             , Prop     "insurer" (HM.empty, Xmlbf.toXml (coverageEligibilityResponseInsurer p))
             , PropList "insurance" (fmap Xmlbf.toXml (coverageEligibilityResponseInsurance p))
             , OptVal   "preAuthRef" (fmap toString (coverageEligibilityResponsePreAuthRef p))
             , OptProp  "form" (fmap Xmlbf.toXml (coverageEligibilityResponseForm p))
             , PropList "error" (fmap Xmlbf.toXml (coverageEligibilityResponseError p))
             ]
instance Xmlbf.FromXml CoverageEligibilityResponse where
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
    purpose <- many     $ Xmlbf.pElement "purpose" (Xmlbf.pAttr "value")
    patient <-            Xmlbf.pElement "patient" Xmlbf.fromXml
    servicedDate <- optional $ Xmlbf.pElement "servicedDate" (Xmlbf.pAttr "value")
    servicedPeriod <- optional $ Xmlbf.pElement "servicedPeriod" Xmlbf.fromXml
    created <-            Xmlbf.pElement "created" (Xmlbf.pAttr "value")
    requestor <- optional $ Xmlbf.pElement "requestor" Xmlbf.fromXml
    request <-            Xmlbf.pElement "request" Xmlbf.fromXml
    outcome <-            Xmlbf.pElement "outcome" (Xmlbf.pAttr "value")
    disposition <- optional $ Xmlbf.pElement "disposition" (Xmlbf.pAttr "value")
    insurer <-            Xmlbf.pElement "insurer" Xmlbf.fromXml
    insurance <- many     $ Xmlbf.pElement "insurance" Xmlbf.fromXml
    preAuthRef <- optional $ Xmlbf.pElement "preAuthRef" (Xmlbf.pAttr "value")
    form <- optional $ Xmlbf.pElement "form" Xmlbf.fromXml
    error <- many     $ Xmlbf.pElement "error" Xmlbf.fromXml
    return CoverageEligibilityResponse {
            coverageEligibilityResponseId = fmap fromId id
          , coverageEligibilityResponseMeta = meta
          , coverageEligibilityResponseImplicitRules = fmap fromUri implicitRules
          , coverageEligibilityResponseLanguage = fmap fromLanguage language
          , coverageEligibilityResponseText = text
--          , coverageEligibilityResponseContained = contained
          , coverageEligibilityResponseExtension = extension
          , coverageEligibilityResponseModifierExtension = modifierExtension
          , coverageEligibilityResponseIdentifier = identifier
          , coverageEligibilityResponseStatus =      fromCoverageEligibilityResponseStatus status
          , coverageEligibilityResponsePurpose = fmap fromCoverageEligibilityResponsePurpose purpose
          , coverageEligibilityResponsePatient = patient
          , coverageEligibilityResponseServicedDate = fmap fromDate servicedDate
          , coverageEligibilityResponseServicedPeriod = servicedPeriod
          , coverageEligibilityResponseCreated =      fromDateTime created
          , coverageEligibilityResponseRequestor = requestor
          , coverageEligibilityResponseRequest = request
          , coverageEligibilityResponseOutcome =      fromCoverageEligibilityResponseOutcome outcome
          , coverageEligibilityResponseDisposition = fmap fromString disposition
          , coverageEligibilityResponseInsurer = insurer
          , coverageEligibilityResponseInsurance = insurance
          , coverageEligibilityResponsePreAuthRef = fmap fromString preAuthRef
          , coverageEligibilityResponseForm = form
          , coverageEligibilityResponseError = error
          }




