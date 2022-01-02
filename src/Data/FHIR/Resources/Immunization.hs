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
-- FHIR 4.0.0 Immunization
--

module Data.FHIR.Resources.Immunization where

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

data ImmunizationStatus
    = ISCompleted
    | ISEnteredInError
    | ISNotDone
  deriving (Eq, Show)

instance ToJSON ImmunizationStatus where
    toJSON ISCompleted = String "completed"
    toJSON ISEnteredInError = String "entered-in-error"
    toJSON ISNotDone = String "not-done"
instance FromJSON ImmunizationStatus where
    parseJSON "completed" = return ISCompleted
    parseJSON "entered-in-error" = return ISEnteredInError
    parseJSON "not-done" = return ISNotDone

toImmunizationStatus ISCompleted = "completed"
toImmunizationStatus ISEnteredInError = "entered-in-error"
toImmunizationStatus ISNotDone = "not-done"
fromImmunizationStatus "completed" = ISCompleted
fromImmunizationStatus "entered-in-error" = ISEnteredInError
fromImmunizationStatus "not-done" = ISNotDone


data ImmunizationOccurrence
    = ImmunizationOccurrenceDateTime DateTime
    | ImmunizationOccurrenceString Text
    deriving (Eq, Show)

data Immunization = Immunization {
    immunizationId :: Maybe Id
  , immunizationMeta :: Maybe Meta
  , immunizationImplicitRules :: Maybe Uri
  , immunizationLanguage :: Maybe Language
  , immunizationText :: Maybe Narrative
--    immunizationContained :: [ResourceContainer]
  , immunizationExtension :: [Extension]
  , immunizationModifierExtension :: [Extension]
  , immunizationIdentifier :: [Identifier]
  , immunizationStatus :: ImmunizationStatus
  , immunizationStatusReason :: Maybe CodeableConcept
  , immunizationVaccineCode :: CodeableConcept
  , immunizationPatient :: Reference
  , immunizationEncounter :: Maybe Reference
  , immunizationOccurrence :: ImmunizationOccurrence
  , immunizationRecorded :: Maybe DateTime
  , immunizationPrimarySource :: Maybe Boolean
  , immunizationReportOrigin :: Maybe CodeableConcept
  , immunizationLocation :: Maybe Reference
  , immunizationManufacturer :: Maybe Reference
  , immunizationLotNumber :: Maybe Text
  , immunizationExpirationDate :: Maybe Date
  , immunizationSite :: Maybe CodeableConcept
  , immunizationRoute :: Maybe CodeableConcept
  , immunizationDoseQuantity :: Maybe Quantity
  , immunizationPerformer :: [ImmunizationPerformer]
  , immunizationNote :: [Annotation]
  , immunizationReasonCode :: [CodeableConcept]
  , immunizationReasonReference :: [Reference]
  , immunizationIsSubpotent :: Maybe Boolean
  , immunizationSubpotentReason :: [CodeableConcept]
  , immunizationEducation :: [ImmunizationEducation]
  , immunizationProgramEligibility :: [CodeableConcept]
  , immunizationFundingSource :: Maybe CodeableConcept
  , immunizationReaction :: [ImmunizationReaction]
  , immunizationProtocolApplied :: [ImmunizationProtocolApplied]
  }
--

instance ToJSON Immunization where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Immunization")
    ,  "id" .= toJSON (immunizationId p)
    ,  "meta" .= toJSON (immunizationMeta p)
    ,  "implicitRules" .= toJSON (immunizationImplicitRules p)
    ,  "language" .= toJSON (immunizationLanguage p)
    ,  "text" .= toJSON (immunizationText p)
--    , "contained" .= toJSON (immunizationContained p)
    ,  "extension" .= toJSON (immunizationExtension p)
    ,  "modifierExtension" .= toJSON (immunizationModifierExtension p)
    ,  "identifier" .= toJSON (immunizationIdentifier p)
    ,  "status" .= toJSON (immunizationStatus p)
    ,  "statusReason" .= toJSON (immunizationStatusReason p)
    ,  "vaccineCode" .= toJSON (immunizationVaccineCode p)
    ,  "patient" .= toJSON (immunizationPatient p)
    ,  "encounter" .= toJSON (immunizationEncounter p)
    , toOccurrenceJSON (immunizationOccurrence p)
    ,  "recorded" .= toJSON (immunizationRecorded p)
    ,  "primarySource" .= toJSON (immunizationPrimarySource p)
    ,  "reportOrigin" .= toJSON (immunizationReportOrigin p)
    ,  "location" .= toJSON (immunizationLocation p)
    ,  "manufacturer" .= toJSON (immunizationManufacturer p)
    ,  "lotNumber" .= toJSON (immunizationLotNumber p)
    ,  "expirationDate" .= toJSON (immunizationExpirationDate p)
    ,  "site" .= toJSON (immunizationSite p)
    ,  "route" .= toJSON (immunizationRoute p)
    ,  "doseQuantity" .= toJSON (immunizationDoseQuantity p)
    ,  "performer" .= toJSON (immunizationPerformer p)
    ,  "note" .= toJSON (immunizationNote p)
    ,  "reasonCode" .= toJSON (immunizationReasonCode p)
    ,  "reasonReference" .= toJSON (immunizationReasonReference p)
    ,  "isSubpotent" .= toJSON (immunizationIsSubpotent p)
    ,  "subpotentReason" .= toJSON (immunizationSubpotentReason p)
    ,  "education" .= toJSON (immunizationEducation p)
    ,  "programEligibility" .= toJSON (immunizationProgramEligibility p)
    ,  "fundingSource" .= toJSON (immunizationFundingSource p)
    ,  "reaction" .= toJSON (immunizationReaction p)
    ,  "protocolApplied" .= toJSON (immunizationProtocolApplied p)
    ]
    where 
      toOccurrenceJSON (     (ImmunizationOccurrenceDateTime c)) = ("occurrence", toJSON c)
      toOccurrenceJSON (     (ImmunizationOccurrenceString c)) = ("occurrence", toJSON c)
instance FromJSON Immunization where
  parseJSON = withObject "Immunization" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Immunization" -> do
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
        vaccineCode <- o .:  "vaccineCode"
        patient <- o .:  "patient"
        encounter <- o .:? "encounter"
        occurrence <- parseOccurrence o
        recorded <- o .:? "recorded"
        primarySource <- o .:? "primarySource"
        reportOrigin <- o .:? "reportOrigin"
        location <- o .:? "location"
        manufacturer <- o .:? "manufacturer"
        lotNumber <- o .:? "lotNumber"
        expirationDate <- o .:? "expirationDate"
        site <- o .:? "site"
        route <- o .:? "route"
        doseQuantity <- o .:? "doseQuantity"
        performer <- o .:? "performer" .!= []
        note <- o .:? "note" .!= []
        reasonCode <- o .:? "reasonCode" .!= []
        reasonReference <- o .:? "reasonReference" .!= []
        isSubpotent <- o .:? "isSubpotent"
        subpotentReason <- o .:? "subpotentReason" .!= []
        education <- o .:? "education" .!= []
        programEligibility <- o .:? "programEligibility" .!= []
        fundingSource <- o .:? "fundingSource"
        reaction <- o .:? "reaction" .!= []
        protocolApplied <- o .:? "protocolApplied" .!= []
        return Immunization{
            immunizationId = id
          , immunizationMeta = meta
          , immunizationImplicitRules = implicitRules
          , immunizationLanguage = language
          , immunizationText = text
--          , immunizationContained = contained
          , immunizationExtension = extension
          , immunizationModifierExtension = modifierExtension
          , immunizationIdentifier = identifier
          , immunizationStatus = status
          , immunizationStatusReason = statusReason
          , immunizationVaccineCode = vaccineCode
          , immunizationPatient = patient
          , immunizationEncounter = encounter
          , immunizationOccurrence = occurrence
          , immunizationRecorded = recorded
          , immunizationPrimarySource = primarySource
          , immunizationReportOrigin = reportOrigin
          , immunizationLocation = location
          , immunizationManufacturer = manufacturer
          , immunizationLotNumber = lotNumber
          , immunizationExpirationDate = expirationDate
          , immunizationSite = site
          , immunizationRoute = route
          , immunizationDoseQuantity = doseQuantity
          , immunizationPerformer = performer
          , immunizationNote = note
          , immunizationReasonCode = reasonCode
          , immunizationReasonReference = reasonReference
          , immunizationIsSubpotent = isSubpotent
          , immunizationSubpotentReason = subpotentReason
          , immunizationEducation = education
          , immunizationProgramEligibility = programEligibility
          , immunizationFundingSource = fundingSource
          , immunizationReaction = reaction
          , immunizationProtocolApplied = protocolApplied
          }
      _ -> fail "not a Immunization"
    where 
      parseOccurrence o = parseOccurrenceDateTime o <|> parseOccurrenceString o
      parseOccurrenceDateTime o = do
                has <- o .: "occurrenceDateTime"
                return $ ImmunizationOccurrenceDateTime has
      parseOccurrenceString o = do
                has <- o .: "occurrenceString"
                return $ ImmunizationOccurrenceString has
instance Xmlbf.ToXml Immunization where
  toXml p = Xmlbf.element "Immunization" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (immunizationId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (immunizationMeta p))
             , OptVal   "implicitRules" (fmap toUri (immunizationImplicitRules p))
             , OptVal   "language" (fmap toLanguage (immunizationLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (immunizationText p))
--             , PropList "contained" (fmap Xmlbf.toXml (immunizationContained p))
             , PropList "extension" (fmap Xmlbf.toXml (immunizationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (immunizationModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (immunizationIdentifier p))
             , Val      "status" (     toImmunizationStatus (immunizationStatus p))
             , OptProp  "statusReason" (fmap Xmlbf.toXml (immunizationStatusReason p))
             , Prop     "vaccineCode" (HM.empty, Xmlbf.toXml (immunizationVaccineCode p))
             , Prop     "patient" (HM.empty, Xmlbf.toXml (immunizationPatient p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (immunizationEncounter p))
             , toOccurrenceXml (immunizationOccurrence p)
             , OptVal   "recorded" (fmap toDateTime (immunizationRecorded p))
             , OptVal   "primarySource" (fmap toBoolean (immunizationPrimarySource p))
             , OptProp  "reportOrigin" (fmap Xmlbf.toXml (immunizationReportOrigin p))
             , OptProp  "location" (fmap Xmlbf.toXml (immunizationLocation p))
             , OptProp  "manufacturer" (fmap Xmlbf.toXml (immunizationManufacturer p))
             , OptVal   "lotNumber" (fmap toString (immunizationLotNumber p))
             , OptVal   "expirationDate" (fmap toDate (immunizationExpirationDate p))
             , OptProp  "site" (fmap Xmlbf.toXml (immunizationSite p))
             , OptProp  "route" (fmap Xmlbf.toXml (immunizationRoute p))
             , OptProp  "doseQuantity" (fmap Xmlbf.toXml (immunizationDoseQuantity p))
             , PropList "performer" (fmap Xmlbf.toXml (immunizationPerformer p))
             , PropList "note" (fmap Xmlbf.toXml (immunizationNote p))
             , PropList "reasonCode" (fmap Xmlbf.toXml (immunizationReasonCode p))
             , PropList "reasonReference" (fmap Xmlbf.toXml (immunizationReasonReference p))
             , OptVal   "isSubpotent" (fmap toBoolean (immunizationIsSubpotent p))
             , PropList "subpotentReason" (fmap Xmlbf.toXml (immunizationSubpotentReason p))
             , PropList "education" (fmap Xmlbf.toXml (immunizationEducation p))
             , PropList "programEligibility" (fmap Xmlbf.toXml (immunizationProgramEligibility p))
             , OptProp  "fundingSource" (fmap Xmlbf.toXml (immunizationFundingSource p))
             , PropList "reaction" (fmap Xmlbf.toXml (immunizationReaction p))
             , PropList "protocolApplied" (fmap Xmlbf.toXml (immunizationProtocolApplied p))
             ]
          toOccurrenceXml (     (ImmunizationOccurrenceDateTime p)) = Val      "occurrenceDateTime" (     toDateTime p)
          toOccurrenceXml (     (ImmunizationOccurrenceString p)) = Val      "occurrenceString" (     toString p)
instance Xmlbf.FromXml Immunization where
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
    vaccineCode <-            Xmlbf.pElement "vaccineCode" Xmlbf.fromXml
    patient <-            Xmlbf.pElement "patient" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    occurrence <- fromOccurrenceXml
    recorded <- optional $ Xmlbf.pElement "recorded" (Xmlbf.pAttr "value")
    primarySource <- optional $ Xmlbf.pElement "primarySource" (Xmlbf.pAttr "value")
    reportOrigin <- optional $ Xmlbf.pElement "reportOrigin" Xmlbf.fromXml
    location <- optional $ Xmlbf.pElement "location" Xmlbf.fromXml
    manufacturer <- optional $ Xmlbf.pElement "manufacturer" Xmlbf.fromXml
    lotNumber <- optional $ Xmlbf.pElement "lotNumber" (Xmlbf.pAttr "value")
    expirationDate <- optional $ Xmlbf.pElement "expirationDate" (Xmlbf.pAttr "value")
    site <- optional $ Xmlbf.pElement "site" Xmlbf.fromXml
    route <- optional $ Xmlbf.pElement "route" Xmlbf.fromXml
    doseQuantity <- optional $ Xmlbf.pElement "doseQuantity" Xmlbf.fromXml
    performer <- many     $ Xmlbf.pElement "performer" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    reasonCode <- many     $ Xmlbf.pElement "reasonCode" Xmlbf.fromXml
    reasonReference <- many     $ Xmlbf.pElement "reasonReference" Xmlbf.fromXml
    isSubpotent <- optional $ Xmlbf.pElement "isSubpotent" (Xmlbf.pAttr "value")
    subpotentReason <- many     $ Xmlbf.pElement "subpotentReason" Xmlbf.fromXml
    education <- many     $ Xmlbf.pElement "education" Xmlbf.fromXml
    programEligibility <- many     $ Xmlbf.pElement "programEligibility" Xmlbf.fromXml
    fundingSource <- optional $ Xmlbf.pElement "fundingSource" Xmlbf.fromXml
    reaction <- many     $ Xmlbf.pElement "reaction" Xmlbf.fromXml
    protocolApplied <- many     $ Xmlbf.pElement "protocolApplied" Xmlbf.fromXml
    return Immunization {
            immunizationId = fmap fromId id
          , immunizationMeta = meta
          , immunizationImplicitRules = fmap fromUri implicitRules
          , immunizationLanguage = fmap fromLanguage language
          , immunizationText = text
--          , immunizationContained = contained
          , immunizationExtension = extension
          , immunizationModifierExtension = modifierExtension
          , immunizationIdentifier = identifier
          , immunizationStatus =      fromImmunizationStatus status
          , immunizationStatusReason = statusReason
          , immunizationVaccineCode = vaccineCode
          , immunizationPatient = patient
          , immunizationEncounter = encounter
          , immunizationOccurrence = occurrence
          , immunizationRecorded = fmap fromDateTime recorded
          , immunizationPrimarySource = fmap fromBoolean primarySource
          , immunizationReportOrigin = reportOrigin
          , immunizationLocation = location
          , immunizationManufacturer = manufacturer
          , immunizationLotNumber = fmap fromString lotNumber
          , immunizationExpirationDate = fmap fromDate expirationDate
          , immunizationSite = site
          , immunizationRoute = route
          , immunizationDoseQuantity = doseQuantity
          , immunizationPerformer = performer
          , immunizationNote = note
          , immunizationReasonCode = reasonCode
          , immunizationReasonReference = reasonReference
          , immunizationIsSubpotent = fmap fromBoolean isSubpotent
          , immunizationSubpotentReason = subpotentReason
          , immunizationEducation = education
          , immunizationProgramEligibility = programEligibility
          , immunizationFundingSource = fundingSource
          , immunizationReaction = reaction
          , immunizationProtocolApplied = protocolApplied
          }

    where 
      fromOccurrenceXml = parseOccurrenceDateTime <|> parseOccurrenceString
      parseOccurrenceDateTime = do
                has <- Xmlbf.pElement "occurrenceDateTime" (Xmlbf.pAttr "value")
                return $ ImmunizationOccurrenceDateTime (     toDateTime has)
      parseOccurrenceString = do
                has <- Xmlbf.pElement "occurrenceString" (Xmlbf.pAttr "value")
                return $ ImmunizationOccurrenceString (     toString has)


data ImmunizationRecommendation = ImmunizationRecommendation {
    immunizationRecommendationId :: Maybe Id
  , immunizationRecommendationMeta :: Maybe Meta
  , immunizationRecommendationImplicitRules :: Maybe Uri
  , immunizationRecommendationLanguage :: Maybe Language
  , immunizationRecommendationText :: Maybe Narrative
--    immunizationRecommendationContained :: [ResourceContainer]
  , immunizationRecommendationExtension :: [Extension]
  , immunizationRecommendationModifierExtension :: [Extension]
  , immunizationRecommendationIdentifier :: [Identifier]
  , immunizationRecommendationPatient :: Reference
  , immunizationRecommendationDate :: DateTime
  , immunizationRecommendationAuthority :: Maybe Reference
  , immunizationRecommendationRecommendation :: [ImmunizationRecommendationRecommendation]
  }
--

instance ToJSON ImmunizationRecommendation where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "ImmunizationRecommendation")
    ,  "id" .= toJSON (immunizationRecommendationId p)
    ,  "meta" .= toJSON (immunizationRecommendationMeta p)
    ,  "implicitRules" .= toJSON (immunizationRecommendationImplicitRules p)
    ,  "language" .= toJSON (immunizationRecommendationLanguage p)
    ,  "text" .= toJSON (immunizationRecommendationText p)
--    , "contained" .= toJSON (immunizationRecommendationContained p)
    ,  "extension" .= toJSON (immunizationRecommendationExtension p)
    ,  "modifierExtension" .= toJSON (immunizationRecommendationModifierExtension p)
    ,  "identifier" .= toJSON (immunizationRecommendationIdentifier p)
    ,  "patient" .= toJSON (immunizationRecommendationPatient p)
    ,  "date" .= toJSON (immunizationRecommendationDate p)
    ,  "authority" .= toJSON (immunizationRecommendationAuthority p)
    ,  "recommendation" .= toJSON (immunizationRecommendationRecommendation p)
    ]
instance FromJSON ImmunizationRecommendation where
  parseJSON = withObject "ImmunizationRecommendation" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "ImmunizationRecommendation" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        patient <- o .:  "patient"
        date <- o .:  "date"
        authority <- o .:? "authority"
        recommendation <- o .:? "recommendation" .!= []
        return ImmunizationRecommendation{
            immunizationRecommendationId = id
          , immunizationRecommendationMeta = meta
          , immunizationRecommendationImplicitRules = implicitRules
          , immunizationRecommendationLanguage = language
          , immunizationRecommendationText = text
--          , immunizationRecommendationContained = contained
          , immunizationRecommendationExtension = extension
          , immunizationRecommendationModifierExtension = modifierExtension
          , immunizationRecommendationIdentifier = identifier
          , immunizationRecommendationPatient = patient
          , immunizationRecommendationDate = date
          , immunizationRecommendationAuthority = authority
          , immunizationRecommendationRecommendation = recommendation
          }
      _ -> fail "not a ImmunizationRecommendation"
instance Xmlbf.ToXml ImmunizationRecommendation where
  toXml p = Xmlbf.element "ImmunizationRecommendation" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (immunizationRecommendationId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (immunizationRecommendationMeta p))
             , OptVal   "implicitRules" (fmap toUri (immunizationRecommendationImplicitRules p))
             , OptVal   "language" (fmap toLanguage (immunizationRecommendationLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (immunizationRecommendationText p))
--             , PropList "contained" (fmap Xmlbf.toXml (immunizationRecommendationContained p))
             , PropList "extension" (fmap Xmlbf.toXml (immunizationRecommendationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (immunizationRecommendationModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (immunizationRecommendationIdentifier p))
             , Prop     "patient" (HM.empty, Xmlbf.toXml (immunizationRecommendationPatient p))
             , Val      "date" (     toDateTime (immunizationRecommendationDate p))
             , OptProp  "authority" (fmap Xmlbf.toXml (immunizationRecommendationAuthority p))
             , PropList "recommendation" (fmap Xmlbf.toXml (immunizationRecommendationRecommendation p))
             ]
instance Xmlbf.FromXml ImmunizationRecommendation where
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
    patient <-            Xmlbf.pElement "patient" Xmlbf.fromXml
    date <-            Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    authority <- optional $ Xmlbf.pElement "authority" Xmlbf.fromXml
    recommendation <- many     $ Xmlbf.pElement "recommendation" Xmlbf.fromXml
    return ImmunizationRecommendation {
            immunizationRecommendationId = fmap fromId id
          , immunizationRecommendationMeta = meta
          , immunizationRecommendationImplicitRules = fmap fromUri implicitRules
          , immunizationRecommendationLanguage = fmap fromLanguage language
          , immunizationRecommendationText = text
--          , immunizationRecommendationContained = contained
          , immunizationRecommendationExtension = extension
          , immunizationRecommendationModifierExtension = modifierExtension
          , immunizationRecommendationIdentifier = identifier
          , immunizationRecommendationPatient = patient
          , immunizationRecommendationDate =      fromDateTime date
          , immunizationRecommendationAuthority = authority
          , immunizationRecommendationRecommendation = recommendation
          }



data ImmunizationEducation = ImmunizationEducation {
    immunizationEducationAttrId :: Maybe Text
  , immunizationEducationExtension :: [Extension]
  , immunizationEducationModifierExtension :: [Extension]
  , immunizationEducationDocumentType :: Maybe Text
  , immunizationEducationReference :: Maybe Uri
  , immunizationEducationPublicationDate :: Maybe DateTime
  , immunizationEducationPresentationDate :: Maybe DateTime
  }
--

instance ToJSON ImmunizationEducation where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (immunizationEducationAttrId p)
    ,  "extension" .= toJSON (immunizationEducationExtension p)
    ,  "modifierExtension" .= toJSON (immunizationEducationModifierExtension p)
    ,  "documentType" .= toJSON (immunizationEducationDocumentType p)
    ,  "reference" .= toJSON (immunizationEducationReference p)
    ,  "publicationDate" .= toJSON (immunizationEducationPublicationDate p)
    ,  "presentationDate" .= toJSON (immunizationEducationPresentationDate p)
    ]
instance FromJSON ImmunizationEducation where
  parseJSON = withObject "ImmunizationEducation" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        documentType <- o .:? "documentType"
        reference <- o .:? "reference"
        publicationDate <- o .:? "publicationDate"
        presentationDate <- o .:? "presentationDate"
        return ImmunizationEducation{
            immunizationEducationAttrId = id
          , immunizationEducationExtension = extension
          , immunizationEducationModifierExtension = modifierExtension
          , immunizationEducationDocumentType = documentType
          , immunizationEducationReference = reference
          , immunizationEducationPublicationDate = publicationDate
          , immunizationEducationPresentationDate = presentationDate
          }
instance Xmlbf.ToXml ImmunizationEducation where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (immunizationEducationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (immunizationEducationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (immunizationEducationModifierExtension p))
             , OptVal   "documentType" (fmap toString (immunizationEducationDocumentType p))
             , OptVal   "reference" (fmap toUri (immunizationEducationReference p))
             , OptVal   "publicationDate" (fmap toDateTime (immunizationEducationPublicationDate p))
             , OptVal   "presentationDate" (fmap toDateTime (immunizationEducationPresentationDate p))
             ]
instance Xmlbf.FromXml ImmunizationEducation where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    documentType <- optional $ Xmlbf.pElement "documentType" (Xmlbf.pAttr "value")
    reference <- optional $ Xmlbf.pElement "reference" (Xmlbf.pAttr "value")
    publicationDate <- optional $ Xmlbf.pElement "publicationDate" (Xmlbf.pAttr "value")
    presentationDate <- optional $ Xmlbf.pElement "presentationDate" (Xmlbf.pAttr "value")
    return ImmunizationEducation {
            immunizationEducationAttrId = id
          , immunizationEducationExtension = extension
          , immunizationEducationModifierExtension = modifierExtension
          , immunizationEducationDocumentType = fmap fromString documentType
          , immunizationEducationReference = fmap fromUri reference
          , immunizationEducationPublicationDate = fmap fromDateTime publicationDate
          , immunizationEducationPresentationDate = fmap fromDateTime presentationDate
          }



data ImmunizationEvaluationStatus
    = IESCompleted
    | IESEnteredInError
  deriving (Eq, Show)

instance ToJSON ImmunizationEvaluationStatus where
    toJSON IESCompleted = String "completed"
    toJSON IESEnteredInError = String "entered-in-error"
instance FromJSON ImmunizationEvaluationStatus where
    parseJSON "completed" = return IESCompleted
    parseJSON "entered-in-error" = return IESEnteredInError

toImmunizationEvaluationStatus IESCompleted = "completed"
toImmunizationEvaluationStatus IESEnteredInError = "entered-in-error"
fromImmunizationEvaluationStatus "completed" = IESCompleted
fromImmunizationEvaluationStatus "entered-in-error" = IESEnteredInError


data ImmunizationEvaluationDoseNumber
    = ImmunizationEvaluationDoseNumberPositiveInt PositiveInt
    | ImmunizationEvaluationDoseNumberString Text
    deriving (Eq, Show)

data ImmunizationEvaluationSeriesDoses
    = ImmunizationEvaluationSeriesDosesPositiveInt PositiveInt
    | ImmunizationEvaluationSeriesDosesString Text
    deriving (Eq, Show)

data ImmunizationEvaluation = ImmunizationEvaluation {
    immunizationEvaluationId :: Maybe Id
  , immunizationEvaluationMeta :: Maybe Meta
  , immunizationEvaluationImplicitRules :: Maybe Uri
  , immunizationEvaluationLanguage :: Maybe Language
  , immunizationEvaluationText :: Maybe Narrative
--    immunizationEvaluationContained :: [ResourceContainer]
  , immunizationEvaluationExtension :: [Extension]
  , immunizationEvaluationModifierExtension :: [Extension]
  , immunizationEvaluationIdentifier :: [Identifier]
  , immunizationEvaluationStatus :: ImmunizationEvaluationStatus
  , immunizationEvaluationPatient :: Reference
  , immunizationEvaluationDate :: Maybe DateTime
  , immunizationEvaluationAuthority :: Maybe Reference
  , immunizationEvaluationTargetDisease :: CodeableConcept
  , immunizationEvaluationImmunizationEvent :: Reference
  , immunizationEvaluationDoseStatus :: CodeableConcept
  , immunizationEvaluationDoseStatusReason :: [CodeableConcept]
  , immunizationEvaluationDescription :: Maybe Text
  , immunizationEvaluationSeries :: Maybe Text
  , immunizationEvaluationDoseNumberPositiveInt :: Maybe PositiveInt
  , immunizationEvaluationDoseNumberString :: Maybe Text
  , immunizationEvaluationSeriesDosesPositiveInt :: Maybe PositiveInt
  , immunizationEvaluationSeriesDosesString :: Maybe Text
  }
--

instance ToJSON ImmunizationEvaluation where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "ImmunizationEvaluation")
    ,  "id" .= toJSON (immunizationEvaluationId p)
    ,  "meta" .= toJSON (immunizationEvaluationMeta p)
    ,  "implicitRules" .= toJSON (immunizationEvaluationImplicitRules p)
    ,  "language" .= toJSON (immunizationEvaluationLanguage p)
    ,  "text" .= toJSON (immunizationEvaluationText p)
--    , "contained" .= toJSON (immunizationEvaluationContained p)
    ,  "extension" .= toJSON (immunizationEvaluationExtension p)
    ,  "modifierExtension" .= toJSON (immunizationEvaluationModifierExtension p)
    ,  "identifier" .= toJSON (immunizationEvaluationIdentifier p)
    ,  "status" .= toJSON (immunizationEvaluationStatus p)
    ,  "patient" .= toJSON (immunizationEvaluationPatient p)
    ,  "date" .= toJSON (immunizationEvaluationDate p)
    ,  "authority" .= toJSON (immunizationEvaluationAuthority p)
    ,  "targetDisease" .= toJSON (immunizationEvaluationTargetDisease p)
    ,  "immunizationEvent" .= toJSON (immunizationEvaluationImmunizationEvent p)
    ,  "doseStatus" .= toJSON (immunizationEvaluationDoseStatus p)
    ,  "doseStatusReason" .= toJSON (immunizationEvaluationDoseStatusReason p)
    ,  "description" .= toJSON (immunizationEvaluationDescription p)
    ,  "series" .= toJSON (immunizationEvaluationSeries p)
    ,  "doseNumberPositiveInt" .= toJSON (immunizationEvaluationDoseNumberPositiveInt p)
    ,  "doseNumberString" .= toJSON (immunizationEvaluationDoseNumberString p)
    ,  "seriesDosesPositiveInt" .= toJSON (immunizationEvaluationSeriesDosesPositiveInt p)
    ,  "seriesDosesString" .= toJSON (immunizationEvaluationSeriesDosesString p)
    ]
instance FromJSON ImmunizationEvaluation where
  parseJSON = withObject "ImmunizationEvaluation" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "ImmunizationEvaluation" -> do
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
        patient <- o .:  "patient"
        date <- o .:? "date"
        authority <- o .:? "authority"
        targetDisease <- o .:  "targetDisease"
        immunizationEvent <- o .:  "immunizationEvent"
        doseStatus <- o .:  "doseStatus"
        doseStatusReason <- o .:? "doseStatusReason" .!= []
        description <- o .:? "description"
        series <- o .:? "series"
        doseNumberPositiveInt <- o .:? "doseNumberPositiveInt"
        doseNumberString <- o .:? "doseNumberString"
        seriesDosesPositiveInt <- o .:? "seriesDosesPositiveInt"
        seriesDosesString <- o .:? "seriesDosesString"
        return ImmunizationEvaluation{
            immunizationEvaluationId = id
          , immunizationEvaluationMeta = meta
          , immunizationEvaluationImplicitRules = implicitRules
          , immunizationEvaluationLanguage = language
          , immunizationEvaluationText = text
--          , immunizationEvaluationContained = contained
          , immunizationEvaluationExtension = extension
          , immunizationEvaluationModifierExtension = modifierExtension
          , immunizationEvaluationIdentifier = identifier
          , immunizationEvaluationStatus = status
          , immunizationEvaluationPatient = patient
          , immunizationEvaluationDate = date
          , immunizationEvaluationAuthority = authority
          , immunizationEvaluationTargetDisease = targetDisease
          , immunizationEvaluationImmunizationEvent = immunizationEvent
          , immunizationEvaluationDoseStatus = doseStatus
          , immunizationEvaluationDoseStatusReason = doseStatusReason
          , immunizationEvaluationDescription = description
          , immunizationEvaluationSeries = series
          , immunizationEvaluationDoseNumberPositiveInt = doseNumberPositiveInt
          , immunizationEvaluationDoseNumberString = doseNumberString
          , immunizationEvaluationSeriesDosesPositiveInt = seriesDosesPositiveInt
          , immunizationEvaluationSeriesDosesString = seriesDosesString
          }
      _ -> fail "not a ImmunizationEvaluation"
instance Xmlbf.ToXml ImmunizationEvaluation where
  toXml p = Xmlbf.element "ImmunizationEvaluation" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (immunizationEvaluationId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (immunizationEvaluationMeta p))
             , OptVal   "implicitRules" (fmap toUri (immunizationEvaluationImplicitRules p))
             , OptVal   "language" (fmap toLanguage (immunizationEvaluationLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (immunizationEvaluationText p))
--             , PropList "contained" (fmap Xmlbf.toXml (immunizationEvaluationContained p))
             , PropList "extension" (fmap Xmlbf.toXml (immunizationEvaluationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (immunizationEvaluationModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (immunizationEvaluationIdentifier p))
             , Val      "status" (     toImmunizationEvaluationStatus (immunizationEvaluationStatus p))
             , Prop     "patient" (HM.empty, Xmlbf.toXml (immunizationEvaluationPatient p))
             , OptVal   "date" (fmap toDateTime (immunizationEvaluationDate p))
             , OptProp  "authority" (fmap Xmlbf.toXml (immunizationEvaluationAuthority p))
             , Prop     "targetDisease" (HM.empty, Xmlbf.toXml (immunizationEvaluationTargetDisease p))
             , Prop     "immunizationEvent" (HM.empty, Xmlbf.toXml (immunizationEvaluationImmunizationEvent p))
             , Prop     "doseStatus" (HM.empty, Xmlbf.toXml (immunizationEvaluationDoseStatus p))
             , PropList "doseStatusReason" (fmap Xmlbf.toXml (immunizationEvaluationDoseStatusReason p))
             , OptVal   "description" (fmap toString (immunizationEvaluationDescription p))
             , OptVal   "series" (fmap toString (immunizationEvaluationSeries p))
             , OptVal   "doseNumberPositiveInt" (fmap toPositiveInt (immunizationEvaluationDoseNumberPositiveInt p))
             , OptVal   "doseNumberString" (fmap toString (immunizationEvaluationDoseNumberString p))
             , OptVal   "seriesDosesPositiveInt" (fmap toPositiveInt (immunizationEvaluationSeriesDosesPositiveInt p))
             , OptVal   "seriesDosesString" (fmap toString (immunizationEvaluationSeriesDosesString p))
             ]
instance Xmlbf.FromXml ImmunizationEvaluation where
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
    patient <-            Xmlbf.pElement "patient" Xmlbf.fromXml
    date <- optional $ Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    authority <- optional $ Xmlbf.pElement "authority" Xmlbf.fromXml
    targetDisease <-            Xmlbf.pElement "targetDisease" Xmlbf.fromXml
    immunizationEvent <-            Xmlbf.pElement "immunizationEvent" Xmlbf.fromXml
    doseStatus <-            Xmlbf.pElement "doseStatus" Xmlbf.fromXml
    doseStatusReason <- many     $ Xmlbf.pElement "doseStatusReason" Xmlbf.fromXml
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    series <- optional $ Xmlbf.pElement "series" (Xmlbf.pAttr "value")
    doseNumberPositiveInt <- optional $ Xmlbf.pElement "doseNumberPositiveInt" (Xmlbf.pAttr "value")
    doseNumberString <- optional $ Xmlbf.pElement "doseNumberString" (Xmlbf.pAttr "value")
    seriesDosesPositiveInt <- optional $ Xmlbf.pElement "seriesDosesPositiveInt" (Xmlbf.pAttr "value")
    seriesDosesString <- optional $ Xmlbf.pElement "seriesDosesString" (Xmlbf.pAttr "value")
    return ImmunizationEvaluation {
            immunizationEvaluationId = fmap fromId id
          , immunizationEvaluationMeta = meta
          , immunizationEvaluationImplicitRules = fmap fromUri implicitRules
          , immunizationEvaluationLanguage = fmap fromLanguage language
          , immunizationEvaluationText = text
--          , immunizationEvaluationContained = contained
          , immunizationEvaluationExtension = extension
          , immunizationEvaluationModifierExtension = modifierExtension
          , immunizationEvaluationIdentifier = identifier
          , immunizationEvaluationStatus =      fromImmunizationEvaluationStatus status
          , immunizationEvaluationPatient = patient
          , immunizationEvaluationDate = fmap fromDateTime date
          , immunizationEvaluationAuthority = authority
          , immunizationEvaluationTargetDisease = targetDisease
          , immunizationEvaluationImmunizationEvent = immunizationEvent
          , immunizationEvaluationDoseStatus = doseStatus
          , immunizationEvaluationDoseStatusReason = doseStatusReason
          , immunizationEvaluationDescription = fmap fromString description
          , immunizationEvaluationSeries = fmap fromString series
          , immunizationEvaluationDoseNumberPositiveInt = fmap fromPositiveInt doseNumberPositiveInt
          , immunizationEvaluationDoseNumberString = fmap fromString doseNumberString
          , immunizationEvaluationSeriesDosesPositiveInt = fmap fromPositiveInt seriesDosesPositiveInt
          , immunizationEvaluationSeriesDosesString = fmap fromString seriesDosesString
          }



data ImmunizationRecommendationDateCriterion = ImmunizationRecommendationDateCriterion {
    immunizationRecommendationDateCriterionAttrId :: Maybe Text
  , immunizationRecommendationDateCriterionExtension :: [Extension]
  , immunizationRecommendationDateCriterionModifierExtension :: [Extension]
  , immunizationRecommendationDateCriterionCode :: CodeableConcept
  , immunizationRecommendationDateCriterionValue :: DateTime
  }
--

instance ToJSON ImmunizationRecommendationDateCriterion where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (immunizationRecommendationDateCriterionAttrId p)
    ,  "extension" .= toJSON (immunizationRecommendationDateCriterionExtension p)
    ,  "modifierExtension" .= toJSON (immunizationRecommendationDateCriterionModifierExtension p)
    ,  "code" .= toJSON (immunizationRecommendationDateCriterionCode p)
    ,  "value" .= toJSON (immunizationRecommendationDateCriterionValue p)
    ]
instance FromJSON ImmunizationRecommendationDateCriterion where
  parseJSON = withObject "ImmunizationRecommendationDateCriterion" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:  "code"
        value <- o .:  "value"
        return ImmunizationRecommendationDateCriterion{
            immunizationRecommendationDateCriterionAttrId = id
          , immunizationRecommendationDateCriterionExtension = extension
          , immunizationRecommendationDateCriterionModifierExtension = modifierExtension
          , immunizationRecommendationDateCriterionCode = code
          , immunizationRecommendationDateCriterionValue = value
          }
instance Xmlbf.ToXml ImmunizationRecommendationDateCriterion where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (immunizationRecommendationDateCriterionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (immunizationRecommendationDateCriterionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (immunizationRecommendationDateCriterionModifierExtension p))
             , Prop     "code" (HM.empty, Xmlbf.toXml (immunizationRecommendationDateCriterionCode p))
             , Val      "value" (     toDateTime (immunizationRecommendationDateCriterionValue p))
             ]
instance Xmlbf.FromXml ImmunizationRecommendationDateCriterion where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" Xmlbf.fromXml
    value <-            Xmlbf.pElement "value" (Xmlbf.pAttr "value")
    return ImmunizationRecommendationDateCriterion {
            immunizationRecommendationDateCriterionAttrId = id
          , immunizationRecommendationDateCriterionExtension = extension
          , immunizationRecommendationDateCriterionModifierExtension = modifierExtension
          , immunizationRecommendationDateCriterionCode = code
          , immunizationRecommendationDateCriterionValue =      fromDateTime value
          }



data ImmunizationProtocolAppliedDoseNumber
    = ImmunizationProtocolAppliedDoseNumberPositiveInt PositiveInt
    | ImmunizationProtocolAppliedDoseNumberString Text
    deriving (Eq, Show)

data ImmunizationProtocolAppliedSeriesDoses
    = ImmunizationProtocolAppliedSeriesDosesPositiveInt PositiveInt
    | ImmunizationProtocolAppliedSeriesDosesString Text
    deriving (Eq, Show)

data ImmunizationProtocolApplied = ImmunizationProtocolApplied {
    immunizationProtocolAppliedAttrId :: Maybe Text
  , immunizationProtocolAppliedExtension :: [Extension]
  , immunizationProtocolAppliedModifierExtension :: [Extension]
  , immunizationProtocolAppliedSeries :: Maybe Text
  , immunizationProtocolAppliedAuthority :: Maybe Reference
  , immunizationProtocolAppliedTargetDisease :: [CodeableConcept]
  , immunizationProtocolAppliedDoseNumber :: ImmunizationProtocolAppliedDoseNumber
  , immunizationProtocolAppliedSeriesDoses :: Maybe ImmunizationProtocolAppliedSeriesDoses
  }
--

instance ToJSON ImmunizationProtocolApplied where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (immunizationProtocolAppliedAttrId p)
    ,  "extension" .= toJSON (immunizationProtocolAppliedExtension p)
    ,  "modifierExtension" .= toJSON (immunizationProtocolAppliedModifierExtension p)
    ,  "series" .= toJSON (immunizationProtocolAppliedSeries p)
    ,  "authority" .= toJSON (immunizationProtocolAppliedAuthority p)
    ,  "targetDisease" .= toJSON (immunizationProtocolAppliedTargetDisease p)
    , toDoseNumberJSON (immunizationProtocolAppliedDoseNumber p)
    , toSeriesDosesJSON (immunizationProtocolAppliedSeriesDoses p)
    ]
    where 
      toDoseNumberJSON (     (ImmunizationProtocolAppliedDoseNumberPositiveInt c)) = ("doseNumber", toJSON c)
      toDoseNumberJSON (     (ImmunizationProtocolAppliedDoseNumberString c)) = ("doseNumber", toJSON c)
      toSeriesDosesJSON (     Nothing   ) = ("seriesDoses", Null)
      toSeriesDosesJSON (Just (ImmunizationProtocolAppliedSeriesDosesPositiveInt c)) = ("seriesDoses", toJSON c)
      toSeriesDosesJSON (Just (ImmunizationProtocolAppliedSeriesDosesString c)) = ("seriesDoses", toJSON c)
instance FromJSON ImmunizationProtocolApplied where
  parseJSON = withObject "ImmunizationProtocolApplied" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        series <- o .:? "series"
        authority <- o .:? "authority"
        targetDisease <- o .:? "targetDisease" .!= []
        doseNumber <- parseDoseNumber o
        seriesDoses <- parseSeriesDoses o
        return ImmunizationProtocolApplied{
            immunizationProtocolAppliedAttrId = id
          , immunizationProtocolAppliedExtension = extension
          , immunizationProtocolAppliedModifierExtension = modifierExtension
          , immunizationProtocolAppliedSeries = series
          , immunizationProtocolAppliedAuthority = authority
          , immunizationProtocolAppliedTargetDisease = targetDisease
          , immunizationProtocolAppliedDoseNumber = doseNumber
          , immunizationProtocolAppliedSeriesDoses = seriesDoses
          }
    where 
      parseDoseNumber o = parseDoseNumberPositiveInt o <|> parseDoseNumberString o
      parseDoseNumberPositiveInt o = do
                has <- o .: "doseNumberPositiveInt"
                return $ ImmunizationProtocolAppliedDoseNumberPositiveInt has
      parseDoseNumberString o = do
                has <- o .: "doseNumberString"
                return $ ImmunizationProtocolAppliedDoseNumberString has
      parseSeriesDoses o = parseSeriesDosesPositiveInt o <|> parseSeriesDosesString o
      parseSeriesDosesPositiveInt o = do
                has <- o .: "seriesDosesPositiveInt"
                return $ Just (ImmunizationProtocolAppliedSeriesDosesPositiveInt has)
      parseSeriesDosesString o = do
                has <- o .: "seriesDosesString"
                return $ Just (ImmunizationProtocolAppliedSeriesDosesString has)
instance Xmlbf.ToXml ImmunizationProtocolApplied where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (immunizationProtocolAppliedAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (immunizationProtocolAppliedExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (immunizationProtocolAppliedModifierExtension p))
             , OptVal   "series" (fmap toString (immunizationProtocolAppliedSeries p))
             , OptProp  "authority" (fmap Xmlbf.toXml (immunizationProtocolAppliedAuthority p))
             , PropList "targetDisease" (fmap Xmlbf.toXml (immunizationProtocolAppliedTargetDisease p))
             , toDoseNumberXml (immunizationProtocolAppliedDoseNumber p)
             , toSeriesDosesXml (immunizationProtocolAppliedSeriesDoses p)
             ]
       where 
          toDoseNumberXml (     (ImmunizationProtocolAppliedDoseNumberPositiveInt p)) = Val      "doseNumberPositiveInt" (     toPositiveInt p)
          toDoseNumberXml (     (ImmunizationProtocolAppliedDoseNumberString p)) = Val      "doseNumberString" (     toString p)
          toSeriesDosesXml ( Nothing   ) = (OptVal "seriesDoses" Nothing)
          toSeriesDosesXml (Just (ImmunizationProtocolAppliedSeriesDosesPositiveInt p)) = Val   "seriesDosesPositiveInt" (toPositiveInt p)
          toSeriesDosesXml (Just (ImmunizationProtocolAppliedSeriesDosesString p)) = Val   "seriesDosesString" (toString p)
instance Xmlbf.FromXml ImmunizationProtocolApplied where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    series <- optional $ Xmlbf.pElement "series" (Xmlbf.pAttr "value")
    authority <- optional $ Xmlbf.pElement "authority" Xmlbf.fromXml
    targetDisease <- many     $ Xmlbf.pElement "targetDisease" Xmlbf.fromXml
    doseNumber <- fromDoseNumberXml
    seriesDoses <- fromSeriesDosesXml
    return ImmunizationProtocolApplied {
            immunizationProtocolAppliedAttrId = id
          , immunizationProtocolAppliedExtension = extension
          , immunizationProtocolAppliedModifierExtension = modifierExtension
          , immunizationProtocolAppliedSeries = fmap fromString series
          , immunizationProtocolAppliedAuthority = authority
          , immunizationProtocolAppliedTargetDisease = targetDisease
          , immunizationProtocolAppliedDoseNumber = doseNumber
          , immunizationProtocolAppliedSeriesDoses = seriesDoses
          }

    where 
      fromDoseNumberXml = parseDoseNumberPositiveInt <|> parseDoseNumberString
      parseDoseNumberPositiveInt = do
                has <- Xmlbf.pElement "doseNumberPositiveInt" (Xmlbf.pAttr "value")
                return $ ImmunizationProtocolAppliedDoseNumberPositiveInt (     fromPositiveInt has)
      parseDoseNumberString = do
                has <- Xmlbf.pElement "doseNumberString" (Xmlbf.pAttr "value")
                return $ ImmunizationProtocolAppliedDoseNumberString (     fromString has)
      fromSeriesDosesXml = parseSeriesDosesPositiveInt <|> parseSeriesDosesString <|> pure Nothing
      parseSeriesDosesPositiveInt = do
                has <- Xmlbf.pElement "seriesDosesPositiveInt" (Xmlbf.pAttr "value")
                return $ Just (ImmunizationProtocolAppliedSeriesDosesPositiveInt (     fromPositiveInt has))
      parseSeriesDosesString = do
                has <- Xmlbf.pElement "seriesDosesString" (Xmlbf.pAttr "value")
                return $ Just (ImmunizationProtocolAppliedSeriesDosesString (     fromString has))


data ImmunizationRecommendationRecommendationDoseNumber
    = ImmunizationRecommendationRecommendationDoseNumberPositiveInt PositiveInt
    | ImmunizationRecommendationRecommendationDoseNumberString Text
    deriving (Eq, Show)

data ImmunizationRecommendationRecommendationSeriesDoses
    = ImmunizationRecommendationRecommendationSeriesDosesPositiveInt PositiveInt
    | ImmunizationRecommendationRecommendationSeriesDosesString Text
    deriving (Eq, Show)

data ImmunizationRecommendationRecommendation = ImmunizationRecommendationRecommendation {
    immunizationRecommendationRecommendationAttrId :: Maybe Text
  , immunizationRecommendationRecommendationExtension :: [Extension]
  , immunizationRecommendationRecommendationModifierExtension :: [Extension]
  , immunizationRecommendationRecommendationVaccineCode :: [CodeableConcept]
  , immunizationRecommendationRecommendationTargetDisease :: Maybe CodeableConcept
  , immunizationRecommendationRecommendationContraindicatedVaccineCode :: [CodeableConcept]
  , immunizationRecommendationRecommendationForecastStatus :: CodeableConcept
  , immunizationRecommendationRecommendationForecastReason :: [CodeableConcept]
  , immunizationRecommendationRecommendationDateCriterion :: [ImmunizationRecommendationDateCriterion]
  , immunizationRecommendationRecommendationDescription :: Maybe Text
  , immunizationRecommendationRecommendationSeries :: Maybe Text
  , immunizationRecommendationRecommendationDoseNumberPositiveInt :: Maybe PositiveInt
  , immunizationRecommendationRecommendationDoseNumberString :: Maybe Text
  , immunizationRecommendationRecommendationSeriesDosesPositiveInt :: Maybe PositiveInt
  , immunizationRecommendationRecommendationSeriesDosesString :: Maybe Text
  , immunizationRecommendationRecommendationSupportingImmunization :: [Reference]
  , immunizationRecommendationRecommendationSupportingPatientInformation :: [Reference]
  }
--

instance ToJSON ImmunizationRecommendationRecommendation where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (immunizationRecommendationRecommendationAttrId p)
    ,  "extension" .= toJSON (immunizationRecommendationRecommendationExtension p)
    ,  "modifierExtension" .= toJSON (immunizationRecommendationRecommendationModifierExtension p)
    ,  "vaccineCode" .= toJSON (immunizationRecommendationRecommendationVaccineCode p)
    ,  "targetDisease" .= toJSON (immunizationRecommendationRecommendationTargetDisease p)
    ,  "contraindicatedVaccineCode" .= toJSON (immunizationRecommendationRecommendationContraindicatedVaccineCode p)
    ,  "forecastStatus" .= toJSON (immunizationRecommendationRecommendationForecastStatus p)
    ,  "forecastReason" .= toJSON (immunizationRecommendationRecommendationForecastReason p)
    ,  "dateCriterion" .= toJSON (immunizationRecommendationRecommendationDateCriterion p)
    ,  "description" .= toJSON (immunizationRecommendationRecommendationDescription p)
    ,  "series" .= toJSON (immunizationRecommendationRecommendationSeries p)
    ,  "doseNumberPositiveInt" .= toJSON (immunizationRecommendationRecommendationDoseNumberPositiveInt p)
    ,  "doseNumberString" .= toJSON (immunizationRecommendationRecommendationDoseNumberString p)
    ,  "seriesDosesPositiveInt" .= toJSON (immunizationRecommendationRecommendationSeriesDosesPositiveInt p)
    ,  "seriesDosesString" .= toJSON (immunizationRecommendationRecommendationSeriesDosesString p)
    ,  "supportingImmunization" .= toJSON (immunizationRecommendationRecommendationSupportingImmunization p)
    ,  "supportingPatientInformation" .= toJSON (immunizationRecommendationRecommendationSupportingPatientInformation p)
    ]
instance FromJSON ImmunizationRecommendationRecommendation where
  parseJSON = withObject "ImmunizationRecommendationRecommendation" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        vaccineCode <- o .:? "vaccineCode" .!= []
        targetDisease <- o .:? "targetDisease"
        contraindicatedVaccineCode <- o .:? "contraindicatedVaccineCode" .!= []
        forecastStatus <- o .:  "forecastStatus"
        forecastReason <- o .:? "forecastReason" .!= []
        dateCriterion <- o .:? "dateCriterion" .!= []
        description <- o .:? "description"
        series <- o .:? "series"
        doseNumberPositiveInt <- o .:? "doseNumberPositiveInt"
        doseNumberString <- o .:? "doseNumberString"
        seriesDosesPositiveInt <- o .:? "seriesDosesPositiveInt"
        seriesDosesString <- o .:? "seriesDosesString"
        supportingImmunization <- o .:? "supportingImmunization" .!= []
        supportingPatientInformation <- o .:? "supportingPatientInformation" .!= []
        return ImmunizationRecommendationRecommendation{
            immunizationRecommendationRecommendationAttrId = id
          , immunizationRecommendationRecommendationExtension = extension
          , immunizationRecommendationRecommendationModifierExtension = modifierExtension
          , immunizationRecommendationRecommendationVaccineCode = vaccineCode
          , immunizationRecommendationRecommendationTargetDisease = targetDisease
          , immunizationRecommendationRecommendationContraindicatedVaccineCode = contraindicatedVaccineCode
          , immunizationRecommendationRecommendationForecastStatus = forecastStatus
          , immunizationRecommendationRecommendationForecastReason = forecastReason
          , immunizationRecommendationRecommendationDateCriterion = dateCriterion
          , immunizationRecommendationRecommendationDescription = description
          , immunizationRecommendationRecommendationSeries = series
          , immunizationRecommendationRecommendationDoseNumberPositiveInt = doseNumberPositiveInt
          , immunizationRecommendationRecommendationDoseNumberString = doseNumberString
          , immunizationRecommendationRecommendationSeriesDosesPositiveInt = seriesDosesPositiveInt
          , immunizationRecommendationRecommendationSeriesDosesString = seriesDosesString
          , immunizationRecommendationRecommendationSupportingImmunization = supportingImmunization
          , immunizationRecommendationRecommendationSupportingPatientInformation = supportingPatientInformation
          }
instance Xmlbf.ToXml ImmunizationRecommendationRecommendation where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (immunizationRecommendationRecommendationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (immunizationRecommendationRecommendationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (immunizationRecommendationRecommendationModifierExtension p))
             , PropList "vaccineCode" (fmap Xmlbf.toXml (immunizationRecommendationRecommendationVaccineCode p))
             , OptProp  "targetDisease" (fmap Xmlbf.toXml (immunizationRecommendationRecommendationTargetDisease p))
             , PropList "contraindicatedVaccineCode" (fmap Xmlbf.toXml (immunizationRecommendationRecommendationContraindicatedVaccineCode p))
             , Prop     "forecastStatus" (HM.empty, Xmlbf.toXml (immunizationRecommendationRecommendationForecastStatus p))
             , PropList "forecastReason" (fmap Xmlbf.toXml (immunizationRecommendationRecommendationForecastReason p))
             , PropList "dateCriterion" (fmap Xmlbf.toXml (immunizationRecommendationRecommendationDateCriterion p))
             , OptVal   "description" (fmap toString (immunizationRecommendationRecommendationDescription p))
             , OptVal   "series" (fmap toString (immunizationRecommendationRecommendationSeries p))
             , OptVal   "doseNumberPositiveInt" (fmap toPositiveInt (immunizationRecommendationRecommendationDoseNumberPositiveInt p))
             , OptVal   "doseNumberString" (fmap toString (immunizationRecommendationRecommendationDoseNumberString p))
             , OptVal   "seriesDosesPositiveInt" (fmap toPositiveInt (immunizationRecommendationRecommendationSeriesDosesPositiveInt p))
             , OptVal   "seriesDosesString" (fmap toString (immunizationRecommendationRecommendationSeriesDosesString p))
             , PropList "supportingImmunization" (fmap Xmlbf.toXml (immunizationRecommendationRecommendationSupportingImmunization p))
             , PropList "supportingPatientInformation" (fmap Xmlbf.toXml (immunizationRecommendationRecommendationSupportingPatientInformation p))
             ]
instance Xmlbf.FromXml ImmunizationRecommendationRecommendation where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    vaccineCode <- many     $ Xmlbf.pElement "vaccineCode" Xmlbf.fromXml
    targetDisease <- optional $ Xmlbf.pElement "targetDisease" Xmlbf.fromXml
    contraindicatedVaccineCode <- many     $ Xmlbf.pElement "contraindicatedVaccineCode" Xmlbf.fromXml
    forecastStatus <-            Xmlbf.pElement "forecastStatus" Xmlbf.fromXml
    forecastReason <- many     $ Xmlbf.pElement "forecastReason" Xmlbf.fromXml
    dateCriterion <- many     $ Xmlbf.pElement "dateCriterion" Xmlbf.fromXml
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    series <- optional $ Xmlbf.pElement "series" (Xmlbf.pAttr "value")
    doseNumberPositiveInt <- optional $ Xmlbf.pElement "doseNumberPositiveInt" (Xmlbf.pAttr "value")
    doseNumberString <- optional $ Xmlbf.pElement "doseNumberString" (Xmlbf.pAttr "value")
    seriesDosesPositiveInt <- optional $ Xmlbf.pElement "seriesDosesPositiveInt" (Xmlbf.pAttr "value")
    seriesDosesString <- optional $ Xmlbf.pElement "seriesDosesString" (Xmlbf.pAttr "value")
    supportingImmunization <- many     $ Xmlbf.pElement "supportingImmunization" Xmlbf.fromXml
    supportingPatientInformation <- many     $ Xmlbf.pElement "supportingPatientInformation" Xmlbf.fromXml
    return ImmunizationRecommendationRecommendation {
            immunizationRecommendationRecommendationAttrId = id
          , immunizationRecommendationRecommendationExtension = extension
          , immunizationRecommendationRecommendationModifierExtension = modifierExtension
          , immunizationRecommendationRecommendationVaccineCode = vaccineCode
          , immunizationRecommendationRecommendationTargetDisease = targetDisease
          , immunizationRecommendationRecommendationContraindicatedVaccineCode = contraindicatedVaccineCode
          , immunizationRecommendationRecommendationForecastStatus = forecastStatus
          , immunizationRecommendationRecommendationForecastReason = forecastReason
          , immunizationRecommendationRecommendationDateCriterion = dateCriterion
          , immunizationRecommendationRecommendationDescription = fmap fromString description
          , immunizationRecommendationRecommendationSeries = fmap fromString series
          , immunizationRecommendationRecommendationDoseNumberPositiveInt = fmap fromPositiveInt doseNumberPositiveInt
          , immunizationRecommendationRecommendationDoseNumberString = fmap fromString doseNumberString
          , immunizationRecommendationRecommendationSeriesDosesPositiveInt = fmap fromPositiveInt seriesDosesPositiveInt
          , immunizationRecommendationRecommendationSeriesDosesString = fmap fromString seriesDosesString
          , immunizationRecommendationRecommendationSupportingImmunization = supportingImmunization
          , immunizationRecommendationRecommendationSupportingPatientInformation = supportingPatientInformation
          }



data ImmunizationReaction = ImmunizationReaction {
    immunizationReactionAttrId :: Maybe Text
  , immunizationReactionExtension :: [Extension]
  , immunizationReactionModifierExtension :: [Extension]
  , immunizationReactionDate :: Maybe DateTime
  , immunizationReactionDetail :: Maybe Reference
  , immunizationReactionReported :: Maybe Boolean
  }
--

instance ToJSON ImmunizationReaction where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (immunizationReactionAttrId p)
    ,  "extension" .= toJSON (immunizationReactionExtension p)
    ,  "modifierExtension" .= toJSON (immunizationReactionModifierExtension p)
    ,  "date" .= toJSON (immunizationReactionDate p)
    ,  "detail" .= toJSON (immunizationReactionDetail p)
    ,  "reported" .= toJSON (immunizationReactionReported p)
    ]
instance FromJSON ImmunizationReaction where
  parseJSON = withObject "ImmunizationReaction" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        date <- o .:? "date"
        detail <- o .:? "detail"
        reported <- o .:? "reported"
        return ImmunizationReaction{
            immunizationReactionAttrId = id
          , immunizationReactionExtension = extension
          , immunizationReactionModifierExtension = modifierExtension
          , immunizationReactionDate = date
          , immunizationReactionDetail = detail
          , immunizationReactionReported = reported
          }
instance Xmlbf.ToXml ImmunizationReaction where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (immunizationReactionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (immunizationReactionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (immunizationReactionModifierExtension p))
             , OptVal   "date" (fmap toDateTime (immunizationReactionDate p))
             , OptProp  "detail" (fmap Xmlbf.toXml (immunizationReactionDetail p))
             , OptVal   "reported" (fmap toBoolean (immunizationReactionReported p))
             ]
instance Xmlbf.FromXml ImmunizationReaction where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    date <- optional $ Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    detail <- optional $ Xmlbf.pElement "detail" Xmlbf.fromXml
    reported <- optional $ Xmlbf.pElement "reported" (Xmlbf.pAttr "value")
    return ImmunizationReaction {
            immunizationReactionAttrId = id
          , immunizationReactionExtension = extension
          , immunizationReactionModifierExtension = modifierExtension
          , immunizationReactionDate = fmap fromDateTime date
          , immunizationReactionDetail = detail
          , immunizationReactionReported = fmap fromBoolean reported
          }



data ImmunizationPerformer = ImmunizationPerformer {
    immunizationPerformerAttrId :: Maybe Text
  , immunizationPerformerExtension :: [Extension]
  , immunizationPerformerModifierExtension :: [Extension]
  , immunizationPerformerFunction :: Maybe CodeableConcept
  , immunizationPerformerActor :: Reference
  }
--

instance ToJSON ImmunizationPerformer where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (immunizationPerformerAttrId p)
    ,  "extension" .= toJSON (immunizationPerformerExtension p)
    ,  "modifierExtension" .= toJSON (immunizationPerformerModifierExtension p)
    ,  "function" .= toJSON (immunizationPerformerFunction p)
    ,  "actor" .= toJSON (immunizationPerformerActor p)
    ]
instance FromJSON ImmunizationPerformer where
  parseJSON = withObject "ImmunizationPerformer" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        function <- o .:? "function"
        actor <- o .:  "actor"
        return ImmunizationPerformer{
            immunizationPerformerAttrId = id
          , immunizationPerformerExtension = extension
          , immunizationPerformerModifierExtension = modifierExtension
          , immunizationPerformerFunction = function
          , immunizationPerformerActor = actor
          }
instance Xmlbf.ToXml ImmunizationPerformer where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (immunizationPerformerAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (immunizationPerformerExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (immunizationPerformerModifierExtension p))
             , OptProp  "function" (fmap Xmlbf.toXml (immunizationPerformerFunction p))
             , Prop     "actor" (HM.empty, Xmlbf.toXml (immunizationPerformerActor p))
             ]
instance Xmlbf.FromXml ImmunizationPerformer where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    function <- optional $ Xmlbf.pElement "function" Xmlbf.fromXml
    actor <-            Xmlbf.pElement "actor" Xmlbf.fromXml
    return ImmunizationPerformer {
            immunizationPerformerAttrId = id
          , immunizationPerformerExtension = extension
          , immunizationPerformerModifierExtension = modifierExtension
          , immunizationPerformerFunction = function
          , immunizationPerformerActor = actor
          }




