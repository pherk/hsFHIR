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
-- FHIR 4.0.0 ClinicalImpression
--

module Data.FHIR.Resources.ClinicalImpression where

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

data ClinicalImpressionStatus
    = CISInProgress
    | CISCompleted
    | CISEnteredInError
  deriving (Eq, Show)

instance ToJSON ClinicalImpressionStatus where
    toJSON CISInProgress = String "in-progress"
    toJSON CISCompleted = String "completed"
    toJSON CISEnteredInError = String "entered-in-error"
instance FromJSON ClinicalImpressionStatus where
    parseJSON "in-progress" = return CISInProgress
    parseJSON "completed" = return CISCompleted
    parseJSON "entered-in-error" = return CISEnteredInError

toClinicalImpressionStatus CISInProgress = "in-progress"
toClinicalImpressionStatus CISCompleted = "completed"
toClinicalImpressionStatus CISEnteredInError = "entered-in-error"
fromClinicalImpressionStatus "in-progress" = CISInProgress
fromClinicalImpressionStatus "completed" = CISCompleted
fromClinicalImpressionStatus "entered-in-error" = CISEnteredInError


data ClinicalImpressionEffective
    = ClinicalImpressionEffectiveDateTime DateTime
    | ClinicalImpressionEffectivePeriod Period
    deriving (Eq, Show)

data ClinicalImpression = ClinicalImpression {
    clinicalImpressionId :: Maybe Id
  , clinicalImpressionMeta :: Maybe Meta
  , clinicalImpressionImplicitRules :: Maybe Uri
  , clinicalImpressionLanguage :: Maybe Language
  , clinicalImpressionText :: Maybe Narrative
--    clinicalImpressionContained :: [ResourceContainer]
  , clinicalImpressionExtension :: [Extension]
  , clinicalImpressionModifierExtension :: [Extension]
  , clinicalImpressionIdentifier :: [Identifier]
  , clinicalImpressionStatus :: ClinicalImpressionStatus
  , clinicalImpressionStatusReason :: Maybe CodeableConcept
  , clinicalImpressionCode :: Maybe CodeableConcept
  , clinicalImpressionDescription :: Maybe Text
  , clinicalImpressionSubject :: Reference
  , clinicalImpressionEncounter :: Maybe Reference
  , clinicalImpressionEffectiveDateTime :: Maybe DateTime
  , clinicalImpressionEffectivePeriod :: Maybe Period
  , clinicalImpressionDate :: Maybe DateTime
  , clinicalImpressionAssessor :: Maybe Reference
  , clinicalImpressionPrevious :: Maybe Reference
  , clinicalImpressionProblem :: [Reference]
  , clinicalImpressionInvestigation :: [ClinicalImpressionInvestigation]
  , clinicalImpressionProtocol :: [Uri]
  , clinicalImpressionSummary :: Maybe Text
  , clinicalImpressionFinding :: [ClinicalImpressionFinding]
  , clinicalImpressionPrognosisCodeableConcept :: [CodeableConcept]
  , clinicalImpressionPrognosisReference :: [Reference]
  , clinicalImpressionSupportingInfo :: [Reference]
  , clinicalImpressionNote :: [Annotation]
  }
--

instance ToJSON ClinicalImpression where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "ClinicalImpression")
    ,  "id" .= toJSON (clinicalImpressionId p)
    ,  "meta" .= toJSON (clinicalImpressionMeta p)
    ,  "implicitRules" .= toJSON (clinicalImpressionImplicitRules p)
    ,  "language" .= toJSON (clinicalImpressionLanguage p)
    ,  "text" .= toJSON (clinicalImpressionText p)
--    , "contained" .= toJSON (clinicalImpressionContained p)
    ,  "extension" .= toJSON (clinicalImpressionExtension p)
    ,  "modifierExtension" .= toJSON (clinicalImpressionModifierExtension p)
    ,  "identifier" .= toJSON (clinicalImpressionIdentifier p)
    ,  "status" .= toJSON (clinicalImpressionStatus p)
    ,  "statusReason" .= toJSON (clinicalImpressionStatusReason p)
    ,  "code" .= toJSON (clinicalImpressionCode p)
    ,  "description" .= toJSON (clinicalImpressionDescription p)
    ,  "subject" .= toJSON (clinicalImpressionSubject p)
    ,  "encounter" .= toJSON (clinicalImpressionEncounter p)
    ,  "effectiveDateTime" .= toJSON (clinicalImpressionEffectiveDateTime p)
    ,  "effectivePeriod" .= toJSON (clinicalImpressionEffectivePeriod p)
    ,  "date" .= toJSON (clinicalImpressionDate p)
    ,  "assessor" .= toJSON (clinicalImpressionAssessor p)
    ,  "previous" .= toJSON (clinicalImpressionPrevious p)
    ,  "problem" .= toJSON (clinicalImpressionProblem p)
    ,  "investigation" .= toJSON (clinicalImpressionInvestigation p)
    ,  "protocol" .= toJSON (clinicalImpressionProtocol p)
    ,  "summary" .= toJSON (clinicalImpressionSummary p)
    ,  "finding" .= toJSON (clinicalImpressionFinding p)
    ,  "prognosisCodeableConcept" .= toJSON (clinicalImpressionPrognosisCodeableConcept p)
    ,  "prognosisReference" .= toJSON (clinicalImpressionPrognosisReference p)
    ,  "supportingInfo" .= toJSON (clinicalImpressionSupportingInfo p)
    ,  "note" .= toJSON (clinicalImpressionNote p)
    ]
instance FromJSON ClinicalImpression where
  parseJSON = withObject "ClinicalImpression" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "ClinicalImpression" -> do
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
        statusReason <- o .:? "statusReason"
        code <- o .:? "code"
        description <- o .:? "description"
        subject <- o .:  "subject"
        encounter <- o .:? "encounter"
        effectiveDateTime <- o .:? "effectiveDateTime"
        effectivePeriod <- o .:? "effectivePeriod"
        date <- o .:? "date"
        assessor <- o .:? "assessor"
        previous <- o .:? "previous"
        problem <- o .:? "problem" .!= []
        investigation <- o .:? "investigation" .!= []
        protocol <- o .:? "protocol" .!= []
        summary <- o .:? "summary"
        finding <- o .:? "finding" .!= []
        prognosisCodeableConcept <- o .:? "prognosisCodeableConcept" .!= []
        prognosisReference <- o .:? "prognosisReference" .!= []
        supportingInfo <- o .:? "supportingInfo" .!= []
        note <- o .:? "note" .!= []
        return ClinicalImpression{
            clinicalImpressionId = id
          , clinicalImpressionMeta = meta
          , clinicalImpressionImplicitRules = implicitRules
          , clinicalImpressionLanguage = language
          , clinicalImpressionText = text
--          , clinicalImpressionContained = contained
          , clinicalImpressionExtension = extension
          , clinicalImpressionModifierExtension = modifierExtension
          , clinicalImpressionIdentifier = identifier
          , clinicalImpressionStatus = status
          , clinicalImpressionStatusReason = statusReason
          , clinicalImpressionCode = code
          , clinicalImpressionDescription = description
          , clinicalImpressionSubject = subject
          , clinicalImpressionEncounter = encounter
          , clinicalImpressionEffectiveDateTime = effectiveDateTime
          , clinicalImpressionEffectivePeriod = effectivePeriod
          , clinicalImpressionDate = date
          , clinicalImpressionAssessor = assessor
          , clinicalImpressionPrevious = previous
          , clinicalImpressionProblem = problem
          , clinicalImpressionInvestigation = investigation
          , clinicalImpressionProtocol = protocol
          , clinicalImpressionSummary = summary
          , clinicalImpressionFinding = finding
          , clinicalImpressionPrognosisCodeableConcept = prognosisCodeableConcept
          , clinicalImpressionPrognosisReference = prognosisReference
          , clinicalImpressionSupportingInfo = supportingInfo
          , clinicalImpressionNote = note
          }
      _ -> fail "not a ClinicalImpression"
instance Xmlbf.ToXml ClinicalImpression where
  toXml p = Xmlbf.element "ClinicalImpression" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (clinicalImpressionId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (clinicalImpressionMeta p))
             , OptVal   "implicitRules" (fmap toUri (clinicalImpressionImplicitRules p))
             , OptVal   "language" (fmap toLanguage (clinicalImpressionLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (clinicalImpressionText p))
--             , PropList "contained" (fmap Xmlbf.toXml (clinicalImpressionContained p))
             , PropList "extension" (fmap Xmlbf.toXml (clinicalImpressionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (clinicalImpressionModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (clinicalImpressionIdentifier p))
             , Val      "status" (     toClinicalImpressionStatus (clinicalImpressionStatus p))
             , OptProp  "statusReason" (fmap Xmlbf.toXml (clinicalImpressionStatusReason p))
             , OptProp  "code" (fmap Xmlbf.toXml (clinicalImpressionCode p))
             , OptVal   "description" (fmap toString (clinicalImpressionDescription p))
             , Prop     "subject" (HM.empty, Xmlbf.toXml (clinicalImpressionSubject p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (clinicalImpressionEncounter p))
             , OptVal   "effectiveDateTime" (fmap toDateTime (clinicalImpressionEffectiveDateTime p))
             , OptProp  "effectivePeriod" (fmap Xmlbf.toXml (clinicalImpressionEffectivePeriod p))
             , OptVal   "date" (fmap toDateTime (clinicalImpressionDate p))
             , OptProp  "assessor" (fmap Xmlbf.toXml (clinicalImpressionAssessor p))
             , OptProp  "previous" (fmap Xmlbf.toXml (clinicalImpressionPrevious p))
             , PropList "problem" (fmap Xmlbf.toXml (clinicalImpressionProblem p))
             , PropList "investigation" (fmap Xmlbf.toXml (clinicalImpressionInvestigation p))
             , ValList  "protocol" (fmap toUri (clinicalImpressionProtocol p))
             , OptVal   "summary" (fmap toString (clinicalImpressionSummary p))
             , PropList "finding" (fmap Xmlbf.toXml (clinicalImpressionFinding p))
             , PropList "prognosisCodeableConcept" (fmap Xmlbf.toXml (clinicalImpressionPrognosisCodeableConcept p))
             , PropList "prognosisReference" (fmap Xmlbf.toXml (clinicalImpressionPrognosisReference p))
             , PropList "supportingInfo" (fmap Xmlbf.toXml (clinicalImpressionSupportingInfo p))
             , PropList "note" (fmap Xmlbf.toXml (clinicalImpressionNote p))
             ]
instance Xmlbf.FromXml ClinicalImpression where
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
    statusReason <- optional $ Xmlbf.pElement "statusReason" Xmlbf.fromXml
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    subject <-            Xmlbf.pElement "subject" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    effectiveDateTime <- optional $ Xmlbf.pElement "effectiveDateTime" (Xmlbf.pAttr "value")
    effectivePeriod <- optional $ Xmlbf.pElement "effectivePeriod" Xmlbf.fromXml
    date <- optional $ Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    assessor <- optional $ Xmlbf.pElement "assessor" Xmlbf.fromXml
    previous <- optional $ Xmlbf.pElement "previous" Xmlbf.fromXml
    problem <- many     $ Xmlbf.pElement "problem" Xmlbf.fromXml
    investigation <- many     $ Xmlbf.pElement "investigation" Xmlbf.fromXml
    protocol <- many     $ Xmlbf.pElement "protocol" (Xmlbf.pAttr "value")
    summary <- optional $ Xmlbf.pElement "summary" (Xmlbf.pAttr "value")
    finding <- many     $ Xmlbf.pElement "finding" Xmlbf.fromXml
    prognosisCodeableConcept <- many     $ Xmlbf.pElement "prognosisCodeableConcept" Xmlbf.fromXml
    prognosisReference <- many     $ Xmlbf.pElement "prognosisReference" Xmlbf.fromXml
    supportingInfo <- many     $ Xmlbf.pElement "supportingInfo" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    return ClinicalImpression {
            clinicalImpressionId = fmap fromId id
          , clinicalImpressionMeta = meta
          , clinicalImpressionImplicitRules = fmap fromUri implicitRules
          , clinicalImpressionLanguage = fmap fromLanguage language
          , clinicalImpressionText = text
--          , clinicalImpressionContained = contained
          , clinicalImpressionExtension = extension
          , clinicalImpressionModifierExtension = modifierExtension
          , clinicalImpressionIdentifier = identifier
          , clinicalImpressionStatus =      fromClinicalImpressionStatus status
          , clinicalImpressionStatusReason = statusReason
          , clinicalImpressionCode = code
          , clinicalImpressionDescription = fmap fromString description
          , clinicalImpressionSubject = subject
          , clinicalImpressionEncounter = encounter
          , clinicalImpressionEffectiveDateTime = fmap fromDateTime effectiveDateTime
          , clinicalImpressionEffectivePeriod = effectivePeriod
          , clinicalImpressionDate = fmap fromDateTime date
          , clinicalImpressionAssessor = assessor
          , clinicalImpressionPrevious = previous
          , clinicalImpressionProblem = problem
          , clinicalImpressionInvestigation = investigation
          , clinicalImpressionProtocol = fmap fromUri protocol
          , clinicalImpressionSummary = fmap fromString summary
          , clinicalImpressionFinding = finding
          , clinicalImpressionPrognosisCodeableConcept = prognosisCodeableConcept
          , clinicalImpressionPrognosisReference = prognosisReference
          , clinicalImpressionSupportingInfo = supportingInfo
          , clinicalImpressionNote = note
          }



data ClinicalImpressionInvestigation = ClinicalImpressionInvestigation {
    clinicalImpressionInvestigationAttrId :: Maybe Text
  , clinicalImpressionInvestigationExtension :: [Extension]
  , clinicalImpressionInvestigationModifierExtension :: [Extension]
  , clinicalImpressionInvestigationCode :: CodeableConcept
  , clinicalImpressionInvestigationItem :: [Reference]
  }
--

instance ToJSON ClinicalImpressionInvestigation where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (clinicalImpressionInvestigationAttrId p)
    ,  "extension" .= toJSON (clinicalImpressionInvestigationExtension p)
    ,  "modifierExtension" .= toJSON (clinicalImpressionInvestigationModifierExtension p)
    ,  "code" .= toJSON (clinicalImpressionInvestigationCode p)
    ,  "item" .= toJSON (clinicalImpressionInvestigationItem p)
    ]
instance FromJSON ClinicalImpressionInvestigation where
  parseJSON = withObject "ClinicalImpressionInvestigation" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:  "code"
        item <- o .:? "item" .!= []
        return ClinicalImpressionInvestigation{
            clinicalImpressionInvestigationAttrId = id
          , clinicalImpressionInvestigationExtension = extension
          , clinicalImpressionInvestigationModifierExtension = modifierExtension
          , clinicalImpressionInvestigationCode = code
          , clinicalImpressionInvestigationItem = item
          }
instance Xmlbf.ToXml ClinicalImpressionInvestigation where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (clinicalImpressionInvestigationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (clinicalImpressionInvestigationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (clinicalImpressionInvestigationModifierExtension p))
             , Prop     "code" (HM.empty, Xmlbf.toXml (clinicalImpressionInvestigationCode p))
             , PropList "item" (fmap Xmlbf.toXml (clinicalImpressionInvestigationItem p))
             ]
instance Xmlbf.FromXml ClinicalImpressionInvestigation where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" Xmlbf.fromXml
    item <- many     $ Xmlbf.pElement "item" Xmlbf.fromXml
    return ClinicalImpressionInvestigation {
            clinicalImpressionInvestigationAttrId = id
          , clinicalImpressionInvestigationExtension = extension
          , clinicalImpressionInvestigationModifierExtension = modifierExtension
          , clinicalImpressionInvestigationCode = code
          , clinicalImpressionInvestigationItem = item
          }



data ClinicalImpressionFinding = ClinicalImpressionFinding {
    clinicalImpressionFindingAttrId :: Maybe Text
  , clinicalImpressionFindingExtension :: [Extension]
  , clinicalImpressionFindingModifierExtension :: [Extension]
  , clinicalImpressionFindingItemCodeableConcept :: Maybe CodeableConcept
  , clinicalImpressionFindingItemReference :: Maybe Reference
  , clinicalImpressionFindingBasis :: Maybe Text
  }
--

instance ToJSON ClinicalImpressionFinding where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (clinicalImpressionFindingAttrId p)
    ,  "extension" .= toJSON (clinicalImpressionFindingExtension p)
    ,  "modifierExtension" .= toJSON (clinicalImpressionFindingModifierExtension p)
    ,  "itemCodeableConcept" .= toJSON (clinicalImpressionFindingItemCodeableConcept p)
    ,  "itemReference" .= toJSON (clinicalImpressionFindingItemReference p)
    ,  "basis" .= toJSON (clinicalImpressionFindingBasis p)
    ]
instance FromJSON ClinicalImpressionFinding where
  parseJSON = withObject "ClinicalImpressionFinding" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        itemCodeableConcept <- o .:? "itemCodeableConcept"
        itemReference <- o .:? "itemReference"
        basis <- o .:? "basis"
        return ClinicalImpressionFinding{
            clinicalImpressionFindingAttrId = id
          , clinicalImpressionFindingExtension = extension
          , clinicalImpressionFindingModifierExtension = modifierExtension
          , clinicalImpressionFindingItemCodeableConcept = itemCodeableConcept
          , clinicalImpressionFindingItemReference = itemReference
          , clinicalImpressionFindingBasis = basis
          }
instance Xmlbf.ToXml ClinicalImpressionFinding where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (clinicalImpressionFindingAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (clinicalImpressionFindingExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (clinicalImpressionFindingModifierExtension p))
             , OptProp  "itemCodeableConcept" (fmap Xmlbf.toXml (clinicalImpressionFindingItemCodeableConcept p))
             , OptProp  "itemReference" (fmap Xmlbf.toXml (clinicalImpressionFindingItemReference p))
             , OptVal   "basis" (fmap toString (clinicalImpressionFindingBasis p))
             ]
instance Xmlbf.FromXml ClinicalImpressionFinding where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    itemCodeableConcept <- optional $ Xmlbf.pElement "itemCodeableConcept" Xmlbf.fromXml
    itemReference <- optional $ Xmlbf.pElement "itemReference" Xmlbf.fromXml
    basis <- optional $ Xmlbf.pElement "basis" (Xmlbf.pAttr "value")
    return ClinicalImpressionFinding {
            clinicalImpressionFindingAttrId = id
          , clinicalImpressionFindingExtension = extension
          , clinicalImpressionFindingModifierExtension = modifierExtension
          , clinicalImpressionFindingItemCodeableConcept = itemCodeableConcept
          , clinicalImpressionFindingItemReference = itemReference
          , clinicalImpressionFindingBasis = fmap fromString basis
          }




