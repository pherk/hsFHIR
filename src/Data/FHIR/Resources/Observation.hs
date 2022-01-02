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
-- FHIR 4.0.0 Observation
--

module Data.FHIR.Resources.Observation where

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

data ObservationStatus
    = OSRegistered
    | OSPreliminary
    | OSFinal
    | OSAmended
    | OSCorrected
    | OSCancelled
    | OSEnteredInError
    | OSUnknown
  deriving (Eq, Show)

instance ToJSON ObservationStatus where
    toJSON OSRegistered = String "registered"
    toJSON OSPreliminary = String "preliminary"
    toJSON OSFinal = String "final"
    toJSON OSAmended = String "amended"
    toJSON OSCorrected = String "corrected"
    toJSON OSCancelled = String "cancelled"
    toJSON OSEnteredInError = String "entered-in-error"
    toJSON OSUnknown = String "unknown"
instance FromJSON ObservationStatus where
    parseJSON "registered" = return OSRegistered
    parseJSON "preliminary" = return OSPreliminary
    parseJSON "final" = return OSFinal
    parseJSON "amended" = return OSAmended
    parseJSON "corrected" = return OSCorrected
    parseJSON "cancelled" = return OSCancelled
    parseJSON "entered-in-error" = return OSEnteredInError
    parseJSON "unknown" = return OSUnknown

toObservationStatus OSRegistered = "registered"
toObservationStatus OSPreliminary = "preliminary"
toObservationStatus OSFinal = "final"
toObservationStatus OSAmended = "amended"
toObservationStatus OSCorrected = "corrected"
toObservationStatus OSCancelled = "cancelled"
toObservationStatus OSEnteredInError = "entered-in-error"
toObservationStatus OSUnknown = "unknown"
fromObservationStatus "registered" = OSRegistered
fromObservationStatus "preliminary" = OSPreliminary
fromObservationStatus "final" = OSFinal
fromObservationStatus "amended" = OSAmended
fromObservationStatus "corrected" = OSCorrected
fromObservationStatus "cancelled" = OSCancelled
fromObservationStatus "entered-in-error" = OSEnteredInError
fromObservationStatus "unknown" = OSUnknown


data ObservationEffective
    = ObservationEffectiveDateTime DateTime
    | ObservationEffectivePeriod Period
    | ObservationEffectiveTiming Timing
    | ObservationEffectiveInstant Instant
    deriving (Eq, Show)

data ObservationValue
    = ObservationValueQuantity Quantity
    | ObservationValueCodeableConcept CodeableConcept
    | ObservationValueString Text
    | ObservationValueBoolean Boolean
    | ObservationValueInteger Integer
    | ObservationValueRange Range
    | ObservationValueRatio Ratio
    | ObservationValueSampledData SampledData
    | ObservationValueTime Time
    | ObservationValueDateTime DateTime
    | ObservationValuePeriod Period
    deriving (Eq, Show)

data Observation = Observation {
    observationId :: Maybe Id
  , observationMeta :: Maybe Meta
  , observationImplicitRules :: Maybe Uri
  , observationLanguage :: Maybe Language
  , observationText :: Maybe Narrative
--    observationContained :: [ResourceContainer]
  , observationExtension :: [Extension]
  , observationModifierExtension :: [Extension]
  , observationIdentifier :: [Identifier]
  , observationBasedOn :: [Reference]
  , observationPartOf :: [Reference]
  , observationStatus :: ObservationStatus
  , observationCategory :: [CodeableConcept]
  , observationCode :: CodeableConcept
  , observationSubject :: Maybe Reference
  , observationFocus :: [Reference]
  , observationEncounter :: Maybe Reference
  , observationEffective :: Maybe ObservationEffective
  , observationIssued :: Maybe Instant
  , observationPerformer :: [Reference]
  , observationValue :: Maybe ObservationValue
  , observationDataAbsentReason :: Maybe CodeableConcept
  , observationInterpretation :: [CodeableConcept]
  , observationNote :: [Annotation]
  , observationBodySite :: Maybe CodeableConcept
  , observationMethod :: Maybe CodeableConcept
  , observationSpecimen :: Maybe Reference
  , observationDevice :: Maybe Reference
  , observationReferenceRange :: [ObservationReferenceRange]
  , observationHasMember :: [Reference]
  , observationDerivedFrom :: [Reference]
  , observationComponent :: [ObservationComponent]
  }
--

instance ToJSON Observation where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Observation")
    ,  "id" .= toJSON (observationId p)
    ,  "meta" .= toJSON (observationMeta p)
    ,  "implicitRules" .= toJSON (observationImplicitRules p)
    ,  "language" .= toJSON (observationLanguage p)
    ,  "text" .= toJSON (observationText p)
--    , "contained" .= toJSON (observationContained p)
    ,  "extension" .= toJSON (observationExtension p)
    ,  "modifierExtension" .= toJSON (observationModifierExtension p)
    ,  "identifier" .= toJSON (observationIdentifier p)
    ,  "basedOn" .= toJSON (observationBasedOn p)
    ,  "partOf" .= toJSON (observationPartOf p)
    ,  "status" .= toJSON (observationStatus p)
    ,  "category" .= toJSON (observationCategory p)
    ,  "code" .= toJSON (observationCode p)
    ,  "subject" .= toJSON (observationSubject p)
    ,  "focus" .= toJSON (observationFocus p)
    ,  "encounter" .= toJSON (observationEncounter p)
    , toEffectiveJSON (observationEffective p)
    ,  "issued" .= toJSON (observationIssued p)
    ,  "performer" .= toJSON (observationPerformer p)
    , toValueJSON (observationValue p)
    ,  "dataAbsentReason" .= toJSON (observationDataAbsentReason p)
    ,  "interpretation" .= toJSON (observationInterpretation p)
    ,  "note" .= toJSON (observationNote p)
    ,  "bodySite" .= toJSON (observationBodySite p)
    ,  "method" .= toJSON (observationMethod p)
    ,  "specimen" .= toJSON (observationSpecimen p)
    ,  "device" .= toJSON (observationDevice p)
    ,  "referenceRange" .= toJSON (observationReferenceRange p)
    ,  "hasMember" .= toJSON (observationHasMember p)
    ,  "derivedFrom" .= toJSON (observationDerivedFrom p)
    ,  "component" .= toJSON (observationComponent p)
    ]
    where 
      toEffectiveJSON (     Nothing   ) = ("effective", Null)
      toEffectiveJSON (Just (ObservationEffectiveDateTime c)) = ("effective", toJSON c)
      toEffectiveJSON (Just (ObservationEffectivePeriod c)) = ("effective", toJSON c)
      toEffectiveJSON (Just (ObservationEffectiveTiming c)) = ("effective", toJSON c)
      toEffectiveJSON (Just (ObservationEffectiveInstant c)) = ("effective", toJSON c)
      toValueJSON (     Nothing   ) = ("value", Null)
      toValueJSON (Just (ObservationValueQuantity c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationValueCodeableConcept c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationValueString c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationValueBoolean c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationValueInteger c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationValueRange c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationValueRatio c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationValueSampledData c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationValueTime c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationValueDateTime c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationValuePeriod c)) = ("value", toJSON c)
instance FromJSON Observation where
  parseJSON = withObject "Observation" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Observation" -> do
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
        partOf <- o .:? "partOf" .!= []
        status <- o .:  "status"
        category <- o .:? "category" .!= []
        code <- o .:  "code"
        subject <- o .:? "subject"
        focus <- o .:? "focus" .!= []
        encounter <- o .:? "encounter"
        effective <- parseEffective o
        issued <- o .:? "issued"
        performer <- o .:? "performer" .!= []
        value <- parseValue o
        dataAbsentReason <- o .:? "dataAbsentReason"
        interpretation <- o .:? "interpretation" .!= []
        note <- o .:? "note" .!= []
        bodySite <- o .:? "bodySite"
        method <- o .:? "method"
        specimen <- o .:? "specimen"
        device <- o .:? "device"
        referenceRange <- o .:? "referenceRange" .!= []
        hasMember <- o .:? "hasMember" .!= []
        derivedFrom <- o .:? "derivedFrom" .!= []
        component <- o .:? "component" .!= []
        return Observation{
            observationId = id
          , observationMeta = meta
          , observationImplicitRules = implicitRules
          , observationLanguage = language
          , observationText = text
--          , observationContained = contained
          , observationExtension = extension
          , observationModifierExtension = modifierExtension
          , observationIdentifier = identifier
          , observationBasedOn = basedOn
          , observationPartOf = partOf
          , observationStatus = status
          , observationCategory = category
          , observationCode = code
          , observationSubject = subject
          , observationFocus = focus
          , observationEncounter = encounter
          , observationEffective = effective
          , observationIssued = issued
          , observationPerformer = performer
          , observationValue = value
          , observationDataAbsentReason = dataAbsentReason
          , observationInterpretation = interpretation
          , observationNote = note
          , observationBodySite = bodySite
          , observationMethod = method
          , observationSpecimen = specimen
          , observationDevice = device
          , observationReferenceRange = referenceRange
          , observationHasMember = hasMember
          , observationDerivedFrom = derivedFrom
          , observationComponent = component
          }
      _ -> fail "not a Observation"
    where 
      parseEffective o = parseEffectiveDateTime o <|> parseEffectivePeriod o <|> parseEffectiveTiming o <|> parseEffectiveInstant o
      parseEffectiveDateTime o = do
                has <- o .: "effectiveDateTime"
                return $ Just (ObservationEffectiveDateTime has)
      parseEffectivePeriod o = do
                has <- o .: "effectivePeriod"
                return $ Just (ObservationEffectivePeriod has)
      parseEffectiveTiming o = do
                has <- o .: "effectiveTiming"
                return $ Just (ObservationEffectiveTiming has)
      parseEffectiveInstant o = do
                has <- o .: "effectiveInstant"
                return $ Just (ObservationEffectiveInstant has)
      parseValue o = parseValueQuantity o <|> parseValueCodeableConcept o <|> parseValueString o <|> parseValueBoolean o <|> parseValueInteger o <|> parseValueRange o <|> parseValueRatio o <|> parseValueSampledData o <|> parseValueTime o <|> parseValueDateTime o <|> parseValuePeriod o
      parseValueQuantity o = do
                has <- o .: "valueQuantity"
                return $ Just (ObservationValueQuantity has)
      parseValueCodeableConcept o = do
                has <- o .: "valueCodeableConcept"
                return $ Just (ObservationValueCodeableConcept has)
      parseValueString o = do
                has <- o .: "valueString"
                return $ Just (ObservationValueString has)
      parseValueBoolean o = do
                has <- o .: "valueBoolean"
                return $ Just (ObservationValueBoolean has)
      parseValueInteger o = do
                has <- o .: "valueInteger"
                return $ Just (ObservationValueInteger has)
      parseValueRange o = do
                has <- o .: "valueRange"
                return $ Just (ObservationValueRange has)
      parseValueRatio o = do
                has <- o .: "valueRatio"
                return $ Just (ObservationValueRatio has)
      parseValueSampledData o = do
                has <- o .: "valueSampledData"
                return $ Just (ObservationValueSampledData has)
      parseValueTime o = do
                has <- o .: "valueTime"
                return $ Just (ObservationValueTime has)
      parseValueDateTime o = do
                has <- o .: "valueDateTime"
                return $ Just (ObservationValueDateTime has)
      parseValuePeriod o = do
                has <- o .: "valuePeriod"
                return $ Just (ObservationValuePeriod has)
instance Xmlbf.ToXml Observation where
  toXml p = Xmlbf.element "Observation" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (observationId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (observationMeta p))
             , OptVal   "implicitRules" (fmap toUri (observationImplicitRules p))
             , OptVal   "language" (fmap toLanguage (observationLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (observationText p))
--             , PropList "contained" (fmap Xmlbf.toXml (observationContained p))
             , PropList "extension" (fmap Xmlbf.toXml (observationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (observationModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (observationIdentifier p))
             , PropList "basedOn" (fmap Xmlbf.toXml (observationBasedOn p))
             , PropList "partOf" (fmap Xmlbf.toXml (observationPartOf p))
             , Val      "status" (     toObservationStatus (observationStatus p))
             , PropList "category" (fmap Xmlbf.toXml (observationCategory p))
             , Prop     "code" (HM.empty, Xmlbf.toXml (observationCode p))
             , OptProp  "subject" (fmap Xmlbf.toXml (observationSubject p))
             , PropList "focus" (fmap Xmlbf.toXml (observationFocus p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (observationEncounter p))
             , toEffectiveXml (observationEffective p)
             , OptVal   "issued" (fmap toInstant (observationIssued p))
             , PropList "performer" (fmap Xmlbf.toXml (observationPerformer p))
             , toValueXml (observationValue p)
             , OptProp  "dataAbsentReason" (fmap Xmlbf.toXml (observationDataAbsentReason p))
             , PropList "interpretation" (fmap Xmlbf.toXml (observationInterpretation p))
             , PropList "note" (fmap Xmlbf.toXml (observationNote p))
             , OptProp  "bodySite" (fmap Xmlbf.toXml (observationBodySite p))
             , OptProp  "method" (fmap Xmlbf.toXml (observationMethod p))
             , OptProp  "specimen" (fmap Xmlbf.toXml (observationSpecimen p))
             , OptProp  "device" (fmap Xmlbf.toXml (observationDevice p))
             , PropList "referenceRange" (fmap Xmlbf.toXml (observationReferenceRange p))
             , PropList "hasMember" (fmap Xmlbf.toXml (observationHasMember p))
             , PropList "derivedFrom" (fmap Xmlbf.toXml (observationDerivedFrom p))
             , PropList "component" (fmap Xmlbf.toXml (observationComponent p))
             ]
          toEffectiveXml ( Nothing   ) = (OptVal "effective" Nothing)
          toEffectiveXml (Just (ObservationEffectiveDateTime p)) = Val   "effectiveDateTime" (toDateTime p)
          toEffectiveXml (Just (ObservationEffectivePeriod p)) = Prop  "effectivePeriod" (HM.empty, Xmlbf.toXml p)
          toEffectiveXml (Just (ObservationEffectiveTiming p)) = Prop  "effectiveTiming" (HM.empty, Xmlbf.toXml p)
          toEffectiveXml (Just (ObservationEffectiveInstant p)) = Val   "effectiveInstant" (toInstant p)
          toValueXml ( Nothing   ) = (OptVal "value" Nothing)
          toValueXml (Just (ObservationValueQuantity p)) = Prop  "valueQuantity" (HM.empty, Xmlbf.toXml p)
          toValueXml (Just (ObservationValueCodeableConcept p)) = Prop  "valueCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toValueXml (Just (ObservationValueString p)) = Val   "valueString" (toString p)
          toValueXml (Just (ObservationValueBoolean p)) = Val   "valueBoolean" (toBoolean p)
          toValueXml (Just (ObservationValueInteger p)) = Val   "valueInteger" (toInt p)
          toValueXml (Just (ObservationValueRange p)) = Prop  "valueRange" (HM.empty, Xmlbf.toXml p)
          toValueXml (Just (ObservationValueRatio p)) = Prop  "valueRatio" (HM.empty, Xmlbf.toXml p)
          toValueXml (Just (ObservationValueSampledData p)) = Prop  "valueSampledData" (HM.empty, Xmlbf.toXml p)
          toValueXml (Just (ObservationValueTime p)) = Val   "valueTime" (toTime p)
          toValueXml (Just (ObservationValueDateTime p)) = Val   "valueDateTime" (toDateTime p)
          toValueXml (Just (ObservationValuePeriod p)) = Prop  "valuePeriod" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml Observation where
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
    partOf <- many     $ Xmlbf.pElement "partOf" Xmlbf.fromXml
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    category <- many     $ Xmlbf.pElement "category" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" Xmlbf.fromXml
    subject <- optional $ Xmlbf.pElement "subject" Xmlbf.fromXml
    focus <- many     $ Xmlbf.pElement "focus" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    effective <- fromEffectiveXml
    issued <- optional $ Xmlbf.pElement "issued" (Xmlbf.pAttr "value")
    performer <- many     $ Xmlbf.pElement "performer" Xmlbf.fromXml
    value <- fromValueXml
    dataAbsentReason <- optional $ Xmlbf.pElement "dataAbsentReason" Xmlbf.fromXml
    interpretation <- many     $ Xmlbf.pElement "interpretation" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    bodySite <- optional $ Xmlbf.pElement "bodySite" Xmlbf.fromXml
    method <- optional $ Xmlbf.pElement "method" Xmlbf.fromXml
    specimen <- optional $ Xmlbf.pElement "specimen" Xmlbf.fromXml
    device <- optional $ Xmlbf.pElement "device" Xmlbf.fromXml
    referenceRange <- many     $ Xmlbf.pElement "referenceRange" Xmlbf.fromXml
    hasMember <- many     $ Xmlbf.pElement "hasMember" Xmlbf.fromXml
    derivedFrom <- many     $ Xmlbf.pElement "derivedFrom" Xmlbf.fromXml
    component <- many     $ Xmlbf.pElement "component" Xmlbf.fromXml
    return Observation {
            observationId = fmap fromId id
          , observationMeta = meta
          , observationImplicitRules = fmap fromUri implicitRules
          , observationLanguage = fmap fromLanguage language
          , observationText = text
--          , observationContained = contained
          , observationExtension = extension
          , observationModifierExtension = modifierExtension
          , observationIdentifier = identifier
          , observationBasedOn = basedOn
          , observationPartOf = partOf
          , observationStatus =      fromObservationStatus status
          , observationCategory = category
          , observationCode = code
          , observationSubject = subject
          , observationFocus = focus
          , observationEncounter = encounter
          , observationEffective = effective
          , observationIssued = fmap fromInstant issued
          , observationPerformer = performer
          , observationValue = value
          , observationDataAbsentReason = dataAbsentReason
          , observationInterpretation = interpretation
          , observationNote = note
          , observationBodySite = bodySite
          , observationMethod = method
          , observationSpecimen = specimen
          , observationDevice = device
          , observationReferenceRange = referenceRange
          , observationHasMember = hasMember
          , observationDerivedFrom = derivedFrom
          , observationComponent = component
          }

    where 
      fromEffectiveXml = parseEffectiveDateTime <|> parseEffectivePeriod <|> parseEffectiveTiming <|> parseEffectiveInstant <|> pure Nothing
      parseEffectiveDateTime = do
                has <- Xmlbf.pElement "effectiveDateTime" (Xmlbf.pAttr "value")
                return $ Just (ObservationEffectiveDateTime (     fromDateTime has))
      parseEffectivePeriod = do
                has <- Xmlbf.pElement "effectivePeriod" Xmlbf.fromXml
                return $ Just (ObservationEffectivePeriod (                      has))
      parseEffectiveTiming = do
                has <- Xmlbf.pElement "effectiveTiming" Xmlbf.fromXml
                return $ Just (ObservationEffectiveTiming (                      has))
      parseEffectiveInstant = do
                has <- Xmlbf.pElement "effectiveInstant" (Xmlbf.pAttr "value")
                return $ Just (ObservationEffectiveInstant (     fromInstant has))
      fromValueXml = parseValueQuantity <|> parseValueCodeableConcept <|> parseValueString <|> parseValueBoolean <|> parseValueInteger <|> parseValueRange <|> parseValueRatio <|> parseValueSampledData <|> parseValueTime <|> parseValueDateTime <|> parseValuePeriod <|> pure Nothing
      parseValueQuantity = do
                has <- Xmlbf.pElement "valueQuantity" Xmlbf.fromXml
                return $ Just (ObservationValueQuantity (                      has))
      parseValueCodeableConcept = do
                has <- Xmlbf.pElement "valueCodeableConcept" Xmlbf.fromXml
                return $ Just (ObservationValueCodeableConcept (                      has))
      parseValueString = do
                has <- Xmlbf.pElement "valueString" (Xmlbf.pAttr "value")
                return $ Just (ObservationValueString (     fromString has))
      parseValueBoolean = do
                has <- Xmlbf.pElement "valueBoolean" (Xmlbf.pAttr "value")
                return $ Just (ObservationValueBoolean (     fromBoolean has))
      parseValueInteger = do
                has <- Xmlbf.pElement "valueInteger" (Xmlbf.pAttr "value")
                return $ Just (ObservationValueInteger (     fromInt has))
      parseValueRange = do
                has <- Xmlbf.pElement "valueRange" Xmlbf.fromXml
                return $ Just (ObservationValueRange (                      has))
      parseValueRatio = do
                has <- Xmlbf.pElement "valueRatio" Xmlbf.fromXml
                return $ Just (ObservationValueRatio (                      has))
      parseValueSampledData = do
                has <- Xmlbf.pElement "valueSampledData" Xmlbf.fromXml
                return $ Just (ObservationValueSampledData (                      has))
      parseValueTime = do
                has <- Xmlbf.pElement "valueTime" (Xmlbf.pAttr "value")
                return $ Just (ObservationValueTime (     fromTime has))
      parseValueDateTime = do
                has <- Xmlbf.pElement "valueDateTime" (Xmlbf.pAttr "value")
                return $ Just (ObservationValueDateTime (     fromDateTime has))
      parseValuePeriod = do
                has <- Xmlbf.pElement "valuePeriod" Xmlbf.fromXml
                return $ Just (ObservationValuePeriod (                      has))


data ObservationDefinitionQualifiedIntervalCategory
    = ODQICReference
    | ODQICCritical
    | ODQICAbsolute
  deriving (Eq, Show)

instance ToJSON ObservationDefinitionQualifiedIntervalCategory where
    toJSON ODQICReference = String "reference"
    toJSON ODQICCritical = String "critical"
    toJSON ODQICAbsolute = String "absolute"
instance FromJSON ObservationDefinitionQualifiedIntervalCategory where
    parseJSON "reference" = return ODQICReference
    parseJSON "critical" = return ODQICCritical
    parseJSON "absolute" = return ODQICAbsolute

toObservationDefinitionQualifiedIntervalCategory ODQICReference = "reference"
toObservationDefinitionQualifiedIntervalCategory ODQICCritical = "critical"
toObservationDefinitionQualifiedIntervalCategory ODQICAbsolute = "absolute"
fromObservationDefinitionQualifiedIntervalCategory "reference" = ODQICReference
fromObservationDefinitionQualifiedIntervalCategory "critical" = ODQICCritical
fromObservationDefinitionQualifiedIntervalCategory "absolute" = ODQICAbsolute


data ObservationDefinitionQualifiedInterval = ObservationDefinitionQualifiedInterval {
    observationDefinitionQualifiedIntervalAttrId :: Maybe Text
  , observationDefinitionQualifiedIntervalExtension :: [Extension]
  , observationDefinitionQualifiedIntervalModifierExtension :: [Extension]
  , observationDefinitionQualifiedIntervalCategory :: Maybe ObservationDefinitionQualifiedIntervalCategory
  , observationDefinitionQualifiedIntervalRange :: Maybe Range
  , observationDefinitionQualifiedIntervalContext :: Maybe CodeableConcept
  , observationDefinitionQualifiedIntervalAppliesTo :: [CodeableConcept]
  , observationDefinitionQualifiedIntervalGender :: Maybe AdministrativeGender
  , observationDefinitionQualifiedIntervalAge :: Maybe Range
  , observationDefinitionQualifiedIntervalGestationalAge :: Maybe Range
  , observationDefinitionQualifiedIntervalCondition :: Maybe Text
  }
--

instance ToJSON ObservationDefinitionQualifiedInterval where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (observationDefinitionQualifiedIntervalAttrId p)
    ,  "extension" .= toJSON (observationDefinitionQualifiedIntervalExtension p)
    ,  "modifierExtension" .= toJSON (observationDefinitionQualifiedIntervalModifierExtension p)
    ,  "category" .= toJSON (observationDefinitionQualifiedIntervalCategory p)
    ,  "range" .= toJSON (observationDefinitionQualifiedIntervalRange p)
    ,  "context" .= toJSON (observationDefinitionQualifiedIntervalContext p)
    ,  "appliesTo" .= toJSON (observationDefinitionQualifiedIntervalAppliesTo p)
    ,  "gender" .= toJSON (observationDefinitionQualifiedIntervalGender p)
    ,  "age" .= toJSON (observationDefinitionQualifiedIntervalAge p)
    ,  "gestationalAge" .= toJSON (observationDefinitionQualifiedIntervalGestationalAge p)
    ,  "condition" .= toJSON (observationDefinitionQualifiedIntervalCondition p)
    ]
instance FromJSON ObservationDefinitionQualifiedInterval where
  parseJSON = withObject "ObservationDefinitionQualifiedInterval" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        category <- o .:? "category"
        range <- o .:? "range"
        context <- o .:? "context"
        appliesTo <- o .:? "appliesTo" .!= []
        gender <- o .:? "gender"
        age <- o .:? "age"
        gestationalAge <- o .:? "gestationalAge"
        condition <- o .:? "condition"
        return ObservationDefinitionQualifiedInterval{
            observationDefinitionQualifiedIntervalAttrId = id
          , observationDefinitionQualifiedIntervalExtension = extension
          , observationDefinitionQualifiedIntervalModifierExtension = modifierExtension
          , observationDefinitionQualifiedIntervalCategory = category
          , observationDefinitionQualifiedIntervalRange = range
          , observationDefinitionQualifiedIntervalContext = context
          , observationDefinitionQualifiedIntervalAppliesTo = appliesTo
          , observationDefinitionQualifiedIntervalGender = gender
          , observationDefinitionQualifiedIntervalAge = age
          , observationDefinitionQualifiedIntervalGestationalAge = gestationalAge
          , observationDefinitionQualifiedIntervalCondition = condition
          }
instance Xmlbf.ToXml ObservationDefinitionQualifiedInterval where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (observationDefinitionQualifiedIntervalAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (observationDefinitionQualifiedIntervalExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (observationDefinitionQualifiedIntervalModifierExtension p))
             , OptVal   "category" (fmap toObservationDefinitionQualifiedIntervalCategory (observationDefinitionQualifiedIntervalCategory p))
             , OptProp  "range" (fmap Xmlbf.toXml (observationDefinitionQualifiedIntervalRange p))
             , OptProp  "context" (fmap Xmlbf.toXml (observationDefinitionQualifiedIntervalContext p))
             , PropList "appliesTo" (fmap Xmlbf.toXml (observationDefinitionQualifiedIntervalAppliesTo p))
             , OptVal   "gender" (fmap toAdministrativeGender (observationDefinitionQualifiedIntervalGender p))
             , OptProp  "age" (fmap Xmlbf.toXml (observationDefinitionQualifiedIntervalAge p))
             , OptProp  "gestationalAge" (fmap Xmlbf.toXml (observationDefinitionQualifiedIntervalGestationalAge p))
             , OptVal   "condition" (fmap toString (observationDefinitionQualifiedIntervalCondition p))
             ]
instance Xmlbf.FromXml ObservationDefinitionQualifiedInterval where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    category <- optional $ Xmlbf.pElement "category" (Xmlbf.pAttr "value")
    range <- optional $ Xmlbf.pElement "range" Xmlbf.fromXml
    context <- optional $ Xmlbf.pElement "context" Xmlbf.fromXml
    appliesTo <- many     $ Xmlbf.pElement "appliesTo" Xmlbf.fromXml
    gender <- optional $ Xmlbf.pElement "gender" (Xmlbf.pAttr "value")
    age <- optional $ Xmlbf.pElement "age" Xmlbf.fromXml
    gestationalAge <- optional $ Xmlbf.pElement "gestationalAge" Xmlbf.fromXml
    condition <- optional $ Xmlbf.pElement "condition" (Xmlbf.pAttr "value")
    return ObservationDefinitionQualifiedInterval {
            observationDefinitionQualifiedIntervalAttrId = id
          , observationDefinitionQualifiedIntervalExtension = extension
          , observationDefinitionQualifiedIntervalModifierExtension = modifierExtension
          , observationDefinitionQualifiedIntervalCategory = fmap fromObservationDefinitionQualifiedIntervalCategory category
          , observationDefinitionQualifiedIntervalRange = range
          , observationDefinitionQualifiedIntervalContext = context
          , observationDefinitionQualifiedIntervalAppliesTo = appliesTo
          , observationDefinitionQualifiedIntervalGender = fmap fromAdministrativeGender gender
          , observationDefinitionQualifiedIntervalAge = age
          , observationDefinitionQualifiedIntervalGestationalAge = gestationalAge
          , observationDefinitionQualifiedIntervalCondition = fmap fromString condition
          }



data ObservationComponentValue
    = ObservationComponentValueQuantity Quantity
    | ObservationComponentValueCodeableConcept CodeableConcept
    | ObservationComponentValueString Text
    | ObservationComponentValueBoolean Boolean
    | ObservationComponentValueInteger Integer
    | ObservationComponentValueRange Range
    | ObservationComponentValueRatio Ratio
    | ObservationComponentValueSampledData SampledData
    | ObservationComponentValueTime Time
    | ObservationComponentValueDateTime DateTime
    | ObservationComponentValuePeriod Period
    deriving (Eq, Show)

data ObservationComponent = ObservationComponent {
    observationComponentAttrId :: Maybe Text
  , observationComponentExtension :: [Extension]
  , observationComponentModifierExtension :: [Extension]
  , observationComponentCode :: CodeableConcept
  , observationComponentValue :: Maybe ObservationComponentValue
  , observationComponentDataAbsentReason :: Maybe CodeableConcept
  , observationComponentInterpretation :: [CodeableConcept]
  , observationComponentReferenceRange :: [ObservationReferenceRange]
  }
--

instance ToJSON ObservationComponent where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (observationComponentAttrId p)
    ,  "extension" .= toJSON (observationComponentExtension p)
    ,  "modifierExtension" .= toJSON (observationComponentModifierExtension p)
    ,  "code" .= toJSON (observationComponentCode p)
    , toValueJSON (observationComponentValue p)
    ,  "dataAbsentReason" .= toJSON (observationComponentDataAbsentReason p)
    ,  "interpretation" .= toJSON (observationComponentInterpretation p)
    ,  "referenceRange" .= toJSON (observationComponentReferenceRange p)
    ]
    where 
      toValueJSON (     Nothing   ) = ("value", Null)
      toValueJSON (Just (ObservationComponentValueQuantity c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationComponentValueCodeableConcept c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationComponentValueString c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationComponentValueBoolean c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationComponentValueInteger c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationComponentValueRange c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationComponentValueRatio c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationComponentValueSampledData c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationComponentValueTime c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationComponentValueDateTime c)) = ("value", toJSON c)
      toValueJSON (Just (ObservationComponentValuePeriod c)) = ("value", toJSON c)
instance FromJSON ObservationComponent where
  parseJSON = withObject "ObservationComponent" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:  "code"
        value <- parseValue o
        dataAbsentReason <- o .:? "dataAbsentReason"
        interpretation <- o .:? "interpretation" .!= []
        referenceRange <- o .:? "referenceRange" .!= []
        return ObservationComponent{
            observationComponentAttrId = id
          , observationComponentExtension = extension
          , observationComponentModifierExtension = modifierExtension
          , observationComponentCode = code
          , observationComponentValue = value
          , observationComponentDataAbsentReason = dataAbsentReason
          , observationComponentInterpretation = interpretation
          , observationComponentReferenceRange = referenceRange
          }
    where 
      parseValue o = parseValueQuantity o <|> parseValueCodeableConcept o <|> parseValueString o <|> parseValueBoolean o <|> parseValueInteger o <|> parseValueRange o <|> parseValueRatio o <|> parseValueSampledData o <|> parseValueTime o <|> parseValueDateTime o <|> parseValuePeriod o
      parseValueQuantity o = do
                has <- o .: "valueQuantity"
                return $ Just (ObservationComponentValueQuantity has)
      parseValueCodeableConcept o = do
                has <- o .: "valueCodeableConcept"
                return $ Just (ObservationComponentValueCodeableConcept has)
      parseValueString o = do
                has <- o .: "valueString"
                return $ Just (ObservationComponentValueString has)
      parseValueBoolean o = do
                has <- o .: "valueBoolean"
                return $ Just (ObservationComponentValueBoolean has)
      parseValueInteger o = do
                has <- o .: "valueInteger"
                return $ Just (ObservationComponentValueInteger has)
      parseValueRange o = do
                has <- o .: "valueRange"
                return $ Just (ObservationComponentValueRange has)
      parseValueRatio o = do
                has <- o .: "valueRatio"
                return $ Just (ObservationComponentValueRatio has)
      parseValueSampledData o = do
                has <- o .: "valueSampledData"
                return $ Just (ObservationComponentValueSampledData has)
      parseValueTime o = do
                has <- o .: "valueTime"
                return $ Just (ObservationComponentValueTime has)
      parseValueDateTime o = do
                has <- o .: "valueDateTime"
                return $ Just (ObservationComponentValueDateTime has)
      parseValuePeriod o = do
                has <- o .: "valuePeriod"
                return $ Just (ObservationComponentValuePeriod has)
instance Xmlbf.ToXml ObservationComponent where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (observationComponentAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (observationComponentExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (observationComponentModifierExtension p))
             , Prop     "code" (HM.empty, Xmlbf.toXml (observationComponentCode p))
             , toValueXml (observationComponentValue p)
             , OptProp  "dataAbsentReason" (fmap Xmlbf.toXml (observationComponentDataAbsentReason p))
             , PropList "interpretation" (fmap Xmlbf.toXml (observationComponentInterpretation p))
             , PropList "referenceRange" (fmap Xmlbf.toXml (observationComponentReferenceRange p))
             ]
       where 
          toValueXml ( Nothing   ) = (OptVal "value" Nothing)
          toValueXml (Just (ObservationComponentValueQuantity p)) = Prop  "valueQuantity" (HM.empty, Xmlbf.toXml p)
          toValueXml (Just (ObservationComponentValueCodeableConcept p)) = Prop  "valueCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toValueXml (Just (ObservationComponentValueString p)) = Val   "valueString" (toString p)
          toValueXml (Just (ObservationComponentValueBoolean p)) = Val   "valueBoolean" (toBoolean p)
          toValueXml (Just (ObservationComponentValueInteger p)) = Val   "valueInteger" (toInt p)
          toValueXml (Just (ObservationComponentValueRange p)) = Prop  "valueRange" (HM.empty, Xmlbf.toXml p)
          toValueXml (Just (ObservationComponentValueRatio p)) = Prop  "valueRatio" (HM.empty, Xmlbf.toXml p)
          toValueXml (Just (ObservationComponentValueSampledData p)) = Prop  "valueSampledData" (HM.empty, Xmlbf.toXml p)
          toValueXml (Just (ObservationComponentValueTime p)) = Val   "valueTime" (toTime p)
          toValueXml (Just (ObservationComponentValueDateTime p)) = Val   "valueDateTime" (toDateTime p)
          toValueXml (Just (ObservationComponentValuePeriod p)) = Prop  "valuePeriod" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml ObservationComponent where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" Xmlbf.fromXml
    value <- fromValueXml
    dataAbsentReason <- optional $ Xmlbf.pElement "dataAbsentReason" Xmlbf.fromXml
    interpretation <- many     $ Xmlbf.pElement "interpretation" Xmlbf.fromXml
    referenceRange <- many     $ Xmlbf.pElement "referenceRange" Xmlbf.fromXml
    return ObservationComponent {
            observationComponentAttrId = id
          , observationComponentExtension = extension
          , observationComponentModifierExtension = modifierExtension
          , observationComponentCode = code
          , observationComponentValue = value
          , observationComponentDataAbsentReason = dataAbsentReason
          , observationComponentInterpretation = interpretation
          , observationComponentReferenceRange = referenceRange
          }

    where 
      fromValueXml = parseValueQuantity <|> parseValueCodeableConcept <|> parseValueString <|> parseValueBoolean <|> parseValueInteger <|> parseValueRange <|> parseValueRatio <|> parseValueSampledData <|> parseValueTime <|> parseValueDateTime <|> parseValuePeriod <|> pure Nothing
      parseValueQuantity = do
                has <- Xmlbf.pElement "valueQuantity" Xmlbf.fromXml
                return $ Just (ObservationComponentValueQuantity (                      has))
      parseValueCodeableConcept = do
                has <- Xmlbf.pElement "valueCodeableConcept" Xmlbf.fromXml
                return $ Just (ObservationComponentValueCodeableConcept (                      has))
      parseValueString = do
                has <- Xmlbf.pElement "valueString" (Xmlbf.pAttr "value")
                return $ Just (ObservationComponentValueString (     fromString has))
      parseValueBoolean = do
                has <- Xmlbf.pElement "valueBoolean" (Xmlbf.pAttr "value")
                return $ Just (ObservationComponentValueBoolean (     fromBoolean has))
      parseValueInteger = do
                has <- Xmlbf.pElement "valueInteger" (Xmlbf.pAttr "value")
                return $ Just (ObservationComponentValueInteger (     fromInt has))
      parseValueRange = do
                has <- Xmlbf.pElement "valueRange" Xmlbf.fromXml
                return $ Just (ObservationComponentValueRange (                      has))
      parseValueRatio = do
                has <- Xmlbf.pElement "valueRatio" Xmlbf.fromXml
                return $ Just (ObservationComponentValueRatio (                      has))
      parseValueSampledData = do
                has <- Xmlbf.pElement "valueSampledData" Xmlbf.fromXml
                return $ Just (ObservationComponentValueSampledData (                      has))
      parseValueTime = do
                has <- Xmlbf.pElement "valueTime" (Xmlbf.pAttr "value")
                return $ Just (ObservationComponentValueTime (     fromTime has))
      parseValueDateTime = do
                has <- Xmlbf.pElement "valueDateTime" (Xmlbf.pAttr "value")
                return $ Just (ObservationComponentValueDateTime (     fromDateTime has))
      parseValuePeriod = do
                has <- Xmlbf.pElement "valuePeriod" Xmlbf.fromXml
                return $ Just (ObservationComponentValuePeriod (                      has))


data ObservationDefinitionPermittedDataType
    = ODPDTQuantity
    | ODPDTCodeableConcept
    | ODPDTString
    | ODPDTBoolean
    | ODPDTInteger
    | ODPDTRange
    | ODPDTRatio
    | ODPDTSampledData
    | ODPDTTime
    | ODPDTDateTime
    | ODPDTPeriod
  deriving (Eq, Show)

instance ToJSON ObservationDefinitionPermittedDataType where
    toJSON ODPDTQuantity = String "Quantity"
    toJSON ODPDTCodeableConcept = String "CodeableConcept"
    toJSON ODPDTString = String "string"
    toJSON ODPDTBoolean = String "boolean"
    toJSON ODPDTInteger = String "integer"
    toJSON ODPDTRange = String "Range"
    toJSON ODPDTRatio = String "Ratio"
    toJSON ODPDTSampledData = String "SampledData"
    toJSON ODPDTTime = String "time"
    toJSON ODPDTDateTime = String "dateTime"
    toJSON ODPDTPeriod = String "Period"
instance FromJSON ObservationDefinitionPermittedDataType where
    parseJSON "Quantity" = return ODPDTQuantity
    parseJSON "CodeableConcept" = return ODPDTCodeableConcept
    parseJSON "string" = return ODPDTString
    parseJSON "boolean" = return ODPDTBoolean
    parseJSON "integer" = return ODPDTInteger
    parseJSON "Range" = return ODPDTRange
    parseJSON "Ratio" = return ODPDTRatio
    parseJSON "SampledData" = return ODPDTSampledData
    parseJSON "time" = return ODPDTTime
    parseJSON "dateTime" = return ODPDTDateTime
    parseJSON "Period" = return ODPDTPeriod

toObservationDefinitionPermittedDataType ODPDTQuantity = "Quantity"
toObservationDefinitionPermittedDataType ODPDTCodeableConcept = "CodeableConcept"
toObservationDefinitionPermittedDataType ODPDTString = "string"
toObservationDefinitionPermittedDataType ODPDTBoolean = "boolean"
toObservationDefinitionPermittedDataType ODPDTInteger = "integer"
toObservationDefinitionPermittedDataType ODPDTRange = "Range"
toObservationDefinitionPermittedDataType ODPDTRatio = "Ratio"
toObservationDefinitionPermittedDataType ODPDTSampledData = "SampledData"
toObservationDefinitionPermittedDataType ODPDTTime = "time"
toObservationDefinitionPermittedDataType ODPDTDateTime = "dateTime"
toObservationDefinitionPermittedDataType ODPDTPeriod = "Period"
fromObservationDefinitionPermittedDataType "Quantity" = ODPDTQuantity
fromObservationDefinitionPermittedDataType "CodeableConcept" = ODPDTCodeableConcept
fromObservationDefinitionPermittedDataType "string" = ODPDTString
fromObservationDefinitionPermittedDataType "boolean" = ODPDTBoolean
fromObservationDefinitionPermittedDataType "integer" = ODPDTInteger
fromObservationDefinitionPermittedDataType "Range" = ODPDTRange
fromObservationDefinitionPermittedDataType "Ratio" = ODPDTRatio
fromObservationDefinitionPermittedDataType "SampledData" = ODPDTSampledData
fromObservationDefinitionPermittedDataType "time" = ODPDTTime
fromObservationDefinitionPermittedDataType "dateTime" = ODPDTDateTime
fromObservationDefinitionPermittedDataType "Period" = ODPDTPeriod


data ObservationDefinition = ObservationDefinition {
    observationDefinitionId :: Maybe Id
  , observationDefinitionMeta :: Maybe Meta
  , observationDefinitionImplicitRules :: Maybe Uri
  , observationDefinitionLanguage :: Maybe Language
  , observationDefinitionText :: Maybe Narrative
--    observationDefinitionContained :: [ResourceContainer]
  , observationDefinitionExtension :: [Extension]
  , observationDefinitionModifierExtension :: [Extension]
  , observationDefinitionCategory :: [CodeableConcept]
  , observationDefinitionCode :: CodeableConcept
  , observationDefinitionIdentifier :: [Identifier]
  , observationDefinitionPermittedDataType :: [ObservationDefinitionPermittedDataType]
  , observationDefinitionMultipleResultsAllowed :: Maybe Boolean
  , observationDefinitionMethod :: Maybe CodeableConcept
  , observationDefinitionPreferredReportName :: Maybe Text
  , observationDefinitionQuantitativeDetails :: Maybe ObservationDefinitionQuantitativeDetails
  , observationDefinitionQualifiedInterval :: [ObservationDefinitionQualifiedInterval]
  , observationDefinitionValidCodedValueSet :: Maybe Reference
  , observationDefinitionNormalCodedValueSet :: Maybe Reference
  , observationDefinitionAbnormalCodedValueSet :: Maybe Reference
  , observationDefinitionCriticalCodedValueSet :: Maybe Reference
  }
--

instance ToJSON ObservationDefinition where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "ObservationDefinition")
    ,  "id" .= toJSON (observationDefinitionId p)
    ,  "meta" .= toJSON (observationDefinitionMeta p)
    ,  "implicitRules" .= toJSON (observationDefinitionImplicitRules p)
    ,  "language" .= toJSON (observationDefinitionLanguage p)
    ,  "text" .= toJSON (observationDefinitionText p)
--    , "contained" .= toJSON (observationDefinitionContained p)
    ,  "extension" .= toJSON (observationDefinitionExtension p)
    ,  "modifierExtension" .= toJSON (observationDefinitionModifierExtension p)
    ,  "category" .= toJSON (observationDefinitionCategory p)
    ,  "code" .= toJSON (observationDefinitionCode p)
    ,  "identifier" .= toJSON (observationDefinitionIdentifier p)
    ,  "permittedDataType" .= toJSON (observationDefinitionPermittedDataType p)
    ,  "multipleResultsAllowed" .= toJSON (observationDefinitionMultipleResultsAllowed p)
    ,  "method" .= toJSON (observationDefinitionMethod p)
    ,  "preferredReportName" .= toJSON (observationDefinitionPreferredReportName p)
    ,  "quantitativeDetails" .= toJSON (observationDefinitionQuantitativeDetails p)
    ,  "qualifiedInterval" .= toJSON (observationDefinitionQualifiedInterval p)
    ,  "validCodedValueSet" .= toJSON (observationDefinitionValidCodedValueSet p)
    ,  "normalCodedValueSet" .= toJSON (observationDefinitionNormalCodedValueSet p)
    ,  "abnormalCodedValueSet" .= toJSON (observationDefinitionAbnormalCodedValueSet p)
    ,  "criticalCodedValueSet" .= toJSON (observationDefinitionCriticalCodedValueSet p)
    ]
instance FromJSON ObservationDefinition where
  parseJSON = withObject "ObservationDefinition" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "ObservationDefinition" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        category <- o .:? "category" .!= []
        code <- o .:  "code"
        identifier <- o .:? "identifier" .!= []
        permittedDataType <- o .:? "permittedDataType" .!= []
        multipleResultsAllowed <- o .:? "multipleResultsAllowed"
        method <- o .:? "method"
        preferredReportName <- o .:? "preferredReportName"
        quantitativeDetails <- o .:? "quantitativeDetails"
        qualifiedInterval <- o .:? "qualifiedInterval" .!= []
        validCodedValueSet <- o .:? "validCodedValueSet"
        normalCodedValueSet <- o .:? "normalCodedValueSet"
        abnormalCodedValueSet <- o .:? "abnormalCodedValueSet"
        criticalCodedValueSet <- o .:? "criticalCodedValueSet"
        return ObservationDefinition{
            observationDefinitionId = id
          , observationDefinitionMeta = meta
          , observationDefinitionImplicitRules = implicitRules
          , observationDefinitionLanguage = language
          , observationDefinitionText = text
--          , observationDefinitionContained = contained
          , observationDefinitionExtension = extension
          , observationDefinitionModifierExtension = modifierExtension
          , observationDefinitionCategory = category
          , observationDefinitionCode = code
          , observationDefinitionIdentifier = identifier
          , observationDefinitionPermittedDataType = permittedDataType
          , observationDefinitionMultipleResultsAllowed = multipleResultsAllowed
          , observationDefinitionMethod = method
          , observationDefinitionPreferredReportName = preferredReportName
          , observationDefinitionQuantitativeDetails = quantitativeDetails
          , observationDefinitionQualifiedInterval = qualifiedInterval
          , observationDefinitionValidCodedValueSet = validCodedValueSet
          , observationDefinitionNormalCodedValueSet = normalCodedValueSet
          , observationDefinitionAbnormalCodedValueSet = abnormalCodedValueSet
          , observationDefinitionCriticalCodedValueSet = criticalCodedValueSet
          }
      _ -> fail "not a ObservationDefinition"
instance Xmlbf.ToXml ObservationDefinition where
  toXml p = Xmlbf.element "ObservationDefinition" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (observationDefinitionId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (observationDefinitionMeta p))
             , OptVal   "implicitRules" (fmap toUri (observationDefinitionImplicitRules p))
             , OptVal   "language" (fmap toLanguage (observationDefinitionLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (observationDefinitionText p))
--             , PropList "contained" (fmap Xmlbf.toXml (observationDefinitionContained p))
             , PropList "extension" (fmap Xmlbf.toXml (observationDefinitionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (observationDefinitionModifierExtension p))
             , PropList "category" (fmap Xmlbf.toXml (observationDefinitionCategory p))
             , Prop     "code" (HM.empty, Xmlbf.toXml (observationDefinitionCode p))
             , PropList "identifier" (fmap Xmlbf.toXml (observationDefinitionIdentifier p))
             , ValList  "permittedDataType" (fmap toObservationDefinitionPermittedDataType (observationDefinitionPermittedDataType p))
             , OptVal   "multipleResultsAllowed" (fmap toBoolean (observationDefinitionMultipleResultsAllowed p))
             , OptProp  "method" (fmap Xmlbf.toXml (observationDefinitionMethod p))
             , OptVal   "preferredReportName" (fmap toString (observationDefinitionPreferredReportName p))
             , OptProp  "quantitativeDetails" (fmap Xmlbf.toXml (observationDefinitionQuantitativeDetails p))
             , PropList "qualifiedInterval" (fmap Xmlbf.toXml (observationDefinitionQualifiedInterval p))
             , OptProp  "validCodedValueSet" (fmap Xmlbf.toXml (observationDefinitionValidCodedValueSet p))
             , OptProp  "normalCodedValueSet" (fmap Xmlbf.toXml (observationDefinitionNormalCodedValueSet p))
             , OptProp  "abnormalCodedValueSet" (fmap Xmlbf.toXml (observationDefinitionAbnormalCodedValueSet p))
             , OptProp  "criticalCodedValueSet" (fmap Xmlbf.toXml (observationDefinitionCriticalCodedValueSet p))
             ]
instance Xmlbf.FromXml ObservationDefinition where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" Xmlbf.fromXml
--    contained <- many     $ Xmlbf.pElement "contained" Xmlbf.fromXml
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    category <- many     $ Xmlbf.pElement "category" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" Xmlbf.fromXml
    identifier <- many     $ Xmlbf.pElement "identifier" Xmlbf.fromXml
    permittedDataType <- many     $ Xmlbf.pElement "permittedDataType" (Xmlbf.pAttr "value")
    multipleResultsAllowed <- optional $ Xmlbf.pElement "multipleResultsAllowed" (Xmlbf.pAttr "value")
    method <- optional $ Xmlbf.pElement "method" Xmlbf.fromXml
    preferredReportName <- optional $ Xmlbf.pElement "preferredReportName" (Xmlbf.pAttr "value")
    quantitativeDetails <- optional $ Xmlbf.pElement "quantitativeDetails" Xmlbf.fromXml
    qualifiedInterval <- many     $ Xmlbf.pElement "qualifiedInterval" Xmlbf.fromXml
    validCodedValueSet <- optional $ Xmlbf.pElement "validCodedValueSet" Xmlbf.fromXml
    normalCodedValueSet <- optional $ Xmlbf.pElement "normalCodedValueSet" Xmlbf.fromXml
    abnormalCodedValueSet <- optional $ Xmlbf.pElement "abnormalCodedValueSet" Xmlbf.fromXml
    criticalCodedValueSet <- optional $ Xmlbf.pElement "criticalCodedValueSet" Xmlbf.fromXml
    return ObservationDefinition {
            observationDefinitionId = fmap fromId id
          , observationDefinitionMeta = meta
          , observationDefinitionImplicitRules = fmap fromUri implicitRules
          , observationDefinitionLanguage = fmap fromLanguage language
          , observationDefinitionText = text
--          , observationDefinitionContained = contained
          , observationDefinitionExtension = extension
          , observationDefinitionModifierExtension = modifierExtension
          , observationDefinitionCategory = category
          , observationDefinitionCode = code
          , observationDefinitionIdentifier = identifier
          , observationDefinitionPermittedDataType = fmap fromObservationDefinitionPermittedDataType permittedDataType
          , observationDefinitionMultipleResultsAllowed = fmap fromBoolean multipleResultsAllowed
          , observationDefinitionMethod = method
          , observationDefinitionPreferredReportName = fmap fromString preferredReportName
          , observationDefinitionQuantitativeDetails = quantitativeDetails
          , observationDefinitionQualifiedInterval = qualifiedInterval
          , observationDefinitionValidCodedValueSet = validCodedValueSet
          , observationDefinitionNormalCodedValueSet = normalCodedValueSet
          , observationDefinitionAbnormalCodedValueSet = abnormalCodedValueSet
          , observationDefinitionCriticalCodedValueSet = criticalCodedValueSet
          }



data ObservationDefinitionQuantitativeDetails = ObservationDefinitionQuantitativeDetails {
    observationDefinitionQuantitativeDetailsAttrId :: Maybe Text
  , observationDefinitionQuantitativeDetailsExtension :: [Extension]
  , observationDefinitionQuantitativeDetailsModifierExtension :: [Extension]
  , observationDefinitionQuantitativeDetailsCustomaryUnit :: Maybe CodeableConcept
  , observationDefinitionQuantitativeDetailsUnit :: Maybe CodeableConcept
  , observationDefinitionQuantitativeDetailsConversionFactor :: Maybe Decimal
  , observationDefinitionQuantitativeDetailsDecimalPrecision :: Maybe Integer
  }
--

instance ToJSON ObservationDefinitionQuantitativeDetails where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (observationDefinitionQuantitativeDetailsAttrId p)
    ,  "extension" .= toJSON (observationDefinitionQuantitativeDetailsExtension p)
    ,  "modifierExtension" .= toJSON (observationDefinitionQuantitativeDetailsModifierExtension p)
    ,  "customaryUnit" .= toJSON (observationDefinitionQuantitativeDetailsCustomaryUnit p)
    ,  "unit" .= toJSON (observationDefinitionQuantitativeDetailsUnit p)
    ,  "conversionFactor" .= toJSON (observationDefinitionQuantitativeDetailsConversionFactor p)
    ,  "decimalPrecision" .= toJSON (observationDefinitionQuantitativeDetailsDecimalPrecision p)
    ]
instance FromJSON ObservationDefinitionQuantitativeDetails where
  parseJSON = withObject "ObservationDefinitionQuantitativeDetails" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        customaryUnit <- o .:? "customaryUnit"
        unit <- o .:? "unit"
        conversionFactor <- o .:? "conversionFactor"
        decimalPrecision <- o .:? "decimalPrecision"
        return ObservationDefinitionQuantitativeDetails{
            observationDefinitionQuantitativeDetailsAttrId = id
          , observationDefinitionQuantitativeDetailsExtension = extension
          , observationDefinitionQuantitativeDetailsModifierExtension = modifierExtension
          , observationDefinitionQuantitativeDetailsCustomaryUnit = customaryUnit
          , observationDefinitionQuantitativeDetailsUnit = unit
          , observationDefinitionQuantitativeDetailsConversionFactor = conversionFactor
          , observationDefinitionQuantitativeDetailsDecimalPrecision = decimalPrecision
          }
instance Xmlbf.ToXml ObservationDefinitionQuantitativeDetails where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (observationDefinitionQuantitativeDetailsAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (observationDefinitionQuantitativeDetailsExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (observationDefinitionQuantitativeDetailsModifierExtension p))
             , OptProp  "customaryUnit" (fmap Xmlbf.toXml (observationDefinitionQuantitativeDetailsCustomaryUnit p))
             , OptProp  "unit" (fmap Xmlbf.toXml (observationDefinitionQuantitativeDetailsUnit p))
             , OptVal   "conversionFactor" (fmap toDecimal (observationDefinitionQuantitativeDetailsConversionFactor p))
             , OptVal   "decimalPrecision" (fmap toInt (observationDefinitionQuantitativeDetailsDecimalPrecision p))
             ]
instance Xmlbf.FromXml ObservationDefinitionQuantitativeDetails where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    customaryUnit <- optional $ Xmlbf.pElement "customaryUnit" Xmlbf.fromXml
    unit <- optional $ Xmlbf.pElement "unit" Xmlbf.fromXml
    conversionFactor <- optional $ Xmlbf.pElement "conversionFactor" (Xmlbf.pAttr "value")
    decimalPrecision <- optional $ Xmlbf.pElement "decimalPrecision" (Xmlbf.pAttr "value")
    return ObservationDefinitionQuantitativeDetails {
            observationDefinitionQuantitativeDetailsAttrId = id
          , observationDefinitionQuantitativeDetailsExtension = extension
          , observationDefinitionQuantitativeDetailsModifierExtension = modifierExtension
          , observationDefinitionQuantitativeDetailsCustomaryUnit = customaryUnit
          , observationDefinitionQuantitativeDetailsUnit = unit
          , observationDefinitionQuantitativeDetailsConversionFactor = fmap fromDecimal conversionFactor
          , observationDefinitionQuantitativeDetailsDecimalPrecision = fmap fromInt decimalPrecision
          }



data ObservationReferenceRange = ObservationReferenceRange {
    observationReferenceRangeAttrId :: Maybe Text
  , observationReferenceRangeExtension :: [Extension]
  , observationReferenceRangeModifierExtension :: [Extension]
  , observationReferenceRangeLow :: Maybe Quantity
  , observationReferenceRangeHigh :: Maybe Quantity
  , observationReferenceRangeType :: Maybe CodeableConcept
  , observationReferenceRangeAppliesTo :: [CodeableConcept]
  , observationReferenceRangeAge :: Maybe Range
  , observationReferenceRangeText :: Maybe Text
  }
--

instance ToJSON ObservationReferenceRange where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (observationReferenceRangeAttrId p)
    ,  "extension" .= toJSON (observationReferenceRangeExtension p)
    ,  "modifierExtension" .= toJSON (observationReferenceRangeModifierExtension p)
    ,  "low" .= toJSON (observationReferenceRangeLow p)
    ,  "high" .= toJSON (observationReferenceRangeHigh p)
    ,  "type" .= toJSON (observationReferenceRangeType p)
    ,  "appliesTo" .= toJSON (observationReferenceRangeAppliesTo p)
    ,  "age" .= toJSON (observationReferenceRangeAge p)
    ,  "text" .= toJSON (observationReferenceRangeText p)
    ]
instance FromJSON ObservationReferenceRange where
  parseJSON = withObject "ObservationReferenceRange" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        low <- o .:? "low"
        high <- o .:? "high"
        ty <- o .:? "type"
        appliesTo <- o .:? "appliesTo" .!= []
        age <- o .:? "age"
        text <- o .:? "text"
        return ObservationReferenceRange{
            observationReferenceRangeAttrId = id
          , observationReferenceRangeExtension = extension
          , observationReferenceRangeModifierExtension = modifierExtension
          , observationReferenceRangeLow = low
          , observationReferenceRangeHigh = high
          , observationReferenceRangeType = ty
          , observationReferenceRangeAppliesTo = appliesTo
          , observationReferenceRangeAge = age
          , observationReferenceRangeText = text
          }
instance Xmlbf.ToXml ObservationReferenceRange where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (observationReferenceRangeAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (observationReferenceRangeExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (observationReferenceRangeModifierExtension p))
             , OptProp  "low" (fmap Xmlbf.toXml (observationReferenceRangeLow p))
             , OptProp  "high" (fmap Xmlbf.toXml (observationReferenceRangeHigh p))
             , OptProp  "type" (fmap Xmlbf.toXml (observationReferenceRangeType p))
             , PropList "appliesTo" (fmap Xmlbf.toXml (observationReferenceRangeAppliesTo p))
             , OptProp  "age" (fmap Xmlbf.toXml (observationReferenceRangeAge p))
             , OptVal   "text" (fmap toString (observationReferenceRangeText p))
             ]
instance Xmlbf.FromXml ObservationReferenceRange where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    low <- optional $ Xmlbf.pElement "low" Xmlbf.fromXml
    high <- optional $ Xmlbf.pElement "high" Xmlbf.fromXml
    ty <- optional $ Xmlbf.pElement "type" Xmlbf.fromXml
    appliesTo <- many     $ Xmlbf.pElement "appliesTo" Xmlbf.fromXml
    age <- optional $ Xmlbf.pElement "age" Xmlbf.fromXml
    text <- optional $ Xmlbf.pElement "text" (Xmlbf.pAttr "value")
    return ObservationReferenceRange {
            observationReferenceRangeAttrId = id
          , observationReferenceRangeExtension = extension
          , observationReferenceRangeModifierExtension = modifierExtension
          , observationReferenceRangeLow = low
          , observationReferenceRangeHigh = high
          , observationReferenceRangeType = ty
          , observationReferenceRangeAppliesTo = appliesTo
          , observationReferenceRangeAge = age
          , observationReferenceRangeText = fmap fromString text
          }




