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
-- FHIR 4.0.0 Procedure
--

module Data.FHIR.Resources.Procedure where

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

data ProcedureStatus
    = PSPreparation
    | PSInProgress
    | PSNotDone
    | PSOnHold
    | PSStopped
    | PSCompleted
    | PSEnteredInError
    | PSUnknown
  deriving (Eq, Show)

instance ToJSON ProcedureStatus where
    toJSON PSPreparation = String "preparation"
    toJSON PSInProgress = String "in-progress"
    toJSON PSNotDone = String "not-done"
    toJSON PSOnHold = String "on-hold"
    toJSON PSStopped = String "stopped"
    toJSON PSCompleted = String "completed"
    toJSON PSEnteredInError = String "entered-in-error"
    toJSON PSUnknown = String "unknown"
instance FromJSON ProcedureStatus where
    parseJSON "preparation" = return PSPreparation
    parseJSON "in-progress" = return PSInProgress
    parseJSON "not-done" = return PSNotDone
    parseJSON "on-hold" = return PSOnHold
    parseJSON "stopped" = return PSStopped
    parseJSON "completed" = return PSCompleted
    parseJSON "entered-in-error" = return PSEnteredInError
    parseJSON "unknown" = return PSUnknown

toProcedureStatus PSPreparation = "preparation"
toProcedureStatus PSInProgress = "in-progress"
toProcedureStatus PSNotDone = "not-done"
toProcedureStatus PSOnHold = "on-hold"
toProcedureStatus PSStopped = "stopped"
toProcedureStatus PSCompleted = "completed"
toProcedureStatus PSEnteredInError = "entered-in-error"
toProcedureStatus PSUnknown = "unknown"
fromProcedureStatus "preparation" = PSPreparation
fromProcedureStatus "in-progress" = PSInProgress
fromProcedureStatus "not-done" = PSNotDone
fromProcedureStatus "on-hold" = PSOnHold
fromProcedureStatus "stopped" = PSStopped
fromProcedureStatus "completed" = PSCompleted
fromProcedureStatus "entered-in-error" = PSEnteredInError
fromProcedureStatus "unknown" = PSUnknown


data ProcedurePerformed
    = ProcedurePerformedDateTime DateTime
    | ProcedurePerformedPeriod Period
    | ProcedurePerformedString Text
    | ProcedurePerformedAge Age
    | ProcedurePerformedRange Range
    deriving (Eq, Show)

data Procedure = Procedure {
    procedureId :: Maybe Id
  , procedureMeta :: Maybe Meta
  , procedureImplicitRules :: Maybe Uri
  , procedureLanguage :: Maybe Language
  , procedureText :: Maybe Narrative
--    procedureContained :: [ResourceContainer]
  , procedureExtension :: [Extension]
  , procedureModifierExtension :: [Extension]
  , procedureIdentifier :: [Identifier]
  , procedureInstantiatesCanonical :: [Canonical]
  , procedureInstantiatesUri :: [Uri]
  , procedureBasedOn :: [Reference]
  , procedurePartOf :: [Reference]
  , procedureStatus :: ProcedureStatus
  , procedureStatusReason :: Maybe CodeableConcept
  , procedureCategory :: Maybe CodeableConcept
  , procedureCode :: Maybe CodeableConcept
  , procedureSubject :: Reference
  , procedureEncounter :: Maybe Reference
  , procedurePerformed :: Maybe ProcedurePerformed
  , procedureRecorder :: Maybe Reference
  , procedureAsserter :: Maybe Reference
  , procedurePerformer :: [ProcedurePerformer]
  , procedureLocation :: Maybe Reference
  , procedureReasonCode :: [CodeableConcept]
  , procedureReasonReference :: [Reference]
  , procedureBodySite :: [CodeableConcept]
  , procedureOutcome :: Maybe CodeableConcept
  , procedureReport :: [Reference]
  , procedureComplication :: [CodeableConcept]
  , procedureComplicationDetail :: [Reference]
  , procedureFollowUp :: [CodeableConcept]
  , procedureNote :: [Annotation]
  , procedureFocalDevice :: [ProcedureFocalDevice]
  , procedureUsedReference :: [Reference]
  , procedureUsedCode :: [CodeableConcept]
  }
--

instance ToJSON Procedure where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Procedure")
    ,  "id" .= toJSON (procedureId p)
    ,  "meta" .= toJSON (procedureMeta p)
    ,  "implicitRules" .= toJSON (procedureImplicitRules p)
    ,  "language" .= toJSON (procedureLanguage p)
    ,  "text" .= toJSON (procedureText p)
--    , "contained" .= toJSON (procedureContained p)
    ,  "extension" .= toJSON (procedureExtension p)
    ,  "modifierExtension" .= toJSON (procedureModifierExtension p)
    ,  "identifier" .= toJSON (procedureIdentifier p)
    ,  "instantiatesCanonical" .= toJSON (procedureInstantiatesCanonical p)
    ,  "instantiatesUri" .= toJSON (procedureInstantiatesUri p)
    ,  "basedOn" .= toJSON (procedureBasedOn p)
    ,  "partOf" .= toJSON (procedurePartOf p)
    ,  "status" .= toJSON (procedureStatus p)
    ,  "statusReason" .= toJSON (procedureStatusReason p)
    ,  "category" .= toJSON (procedureCategory p)
    ,  "code" .= toJSON (procedureCode p)
    ,  "subject" .= toJSON (procedureSubject p)
    ,  "encounter" .= toJSON (procedureEncounter p)
    , toPerformedJSON (procedurePerformed p)
    ,  "recorder" .= toJSON (procedureRecorder p)
    ,  "asserter" .= toJSON (procedureAsserter p)
    ,  "performer" .= toJSON (procedurePerformer p)
    ,  "location" .= toJSON (procedureLocation p)
    ,  "reasonCode" .= toJSON (procedureReasonCode p)
    ,  "reasonReference" .= toJSON (procedureReasonReference p)
    ,  "bodySite" .= toJSON (procedureBodySite p)
    ,  "outcome" .= toJSON (procedureOutcome p)
    ,  "report" .= toJSON (procedureReport p)
    ,  "complication" .= toJSON (procedureComplication p)
    ,  "complicationDetail" .= toJSON (procedureComplicationDetail p)
    ,  "followUp" .= toJSON (procedureFollowUp p)
    ,  "note" .= toJSON (procedureNote p)
    ,  "focalDevice" .= toJSON (procedureFocalDevice p)
    ,  "usedReference" .= toJSON (procedureUsedReference p)
    ,  "usedCode" .= toJSON (procedureUsedCode p)
    ]
    where 
      toPerformedJSON (     Nothing   ) = ("performed", Null)
      toPerformedJSON (Just (ProcedurePerformedDateTime c)) = ("performed", toJSON c)
      toPerformedJSON (Just (ProcedurePerformedPeriod c)) = ("performed", toJSON c)
      toPerformedJSON (Just (ProcedurePerformedString c)) = ("performed", toJSON c)
      toPerformedJSON (Just (ProcedurePerformedAge c)) = ("performed", toJSON c)
      toPerformedJSON (Just (ProcedurePerformedRange c)) = ("performed", toJSON c)
instance FromJSON Procedure where
  parseJSON = withObject "Procedure" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Procedure" -> do
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
        partOf <- o .:? "partOf" .!= []
        status <- o .:  "status"
        statusReason <- o .:? "statusReason"
        category <- o .:? "category"
        code <- o .:? "code"
        subject <- o .:  "subject"
        encounter <- o .:? "encounter"
        performed <- parsePerformed o
        recorder <- o .:? "recorder"
        asserter <- o .:? "asserter"
        performer <- o .:? "performer" .!= []
        location <- o .:? "location"
        reasonCode <- o .:? "reasonCode" .!= []
        reasonReference <- o .:? "reasonReference" .!= []
        bodySite <- o .:? "bodySite" .!= []
        outcome <- o .:? "outcome"
        report <- o .:? "report" .!= []
        complication <- o .:? "complication" .!= []
        complicationDetail <- o .:? "complicationDetail" .!= []
        followUp <- o .:? "followUp" .!= []
        note <- o .:? "note" .!= []
        focalDevice <- o .:? "focalDevice" .!= []
        usedReference <- o .:? "usedReference" .!= []
        usedCode <- o .:? "usedCode" .!= []
        return Procedure{
            procedureId = id
          , procedureMeta = meta
          , procedureImplicitRules = implicitRules
          , procedureLanguage = language
          , procedureText = text
--          , procedureContained = contained
          , procedureExtension = extension
          , procedureModifierExtension = modifierExtension
          , procedureIdentifier = identifier
          , procedureInstantiatesCanonical = instantiatesCanonical
          , procedureInstantiatesUri = instantiatesUri
          , procedureBasedOn = basedOn
          , procedurePartOf = partOf
          , procedureStatus = status
          , procedureStatusReason = statusReason
          , procedureCategory = category
          , procedureCode = code
          , procedureSubject = subject
          , procedureEncounter = encounter
          , procedurePerformed = performed
          , procedureRecorder = recorder
          , procedureAsserter = asserter
          , procedurePerformer = performer
          , procedureLocation = location
          , procedureReasonCode = reasonCode
          , procedureReasonReference = reasonReference
          , procedureBodySite = bodySite
          , procedureOutcome = outcome
          , procedureReport = report
          , procedureComplication = complication
          , procedureComplicationDetail = complicationDetail
          , procedureFollowUp = followUp
          , procedureNote = note
          , procedureFocalDevice = focalDevice
          , procedureUsedReference = usedReference
          , procedureUsedCode = usedCode
          }
      _ -> fail "not a Procedure"
    where 
      parsePerformed o = parsePerformedDateTime o <|> parsePerformedPeriod o <|> parsePerformedString o <|> parsePerformedAge o <|> parsePerformedRange o
      parsePerformedDateTime o = do
                has <- o .: "performedDateTime"
                return $ Just (ProcedurePerformedDateTime has)
      parsePerformedPeriod o = do
                has <- o .: "performedPeriod"
                return $ Just (ProcedurePerformedPeriod has)
      parsePerformedString o = do
                has <- o .: "performedString"
                return $ Just (ProcedurePerformedString has)
      parsePerformedAge o = do
                has <- o .: "performedAge"
                return $ Just (ProcedurePerformedAge has)
      parsePerformedRange o = do
                has <- o .: "performedRange"
                return $ Just (ProcedurePerformedRange has)
instance Xmlbf.ToXml Procedure where
  toXml p = Xmlbf.element "Procedure" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (procedureId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (procedureMeta p))
             , OptVal   "implicitRules" (fmap toUri (procedureImplicitRules p))
             , OptVal   "language" (fmap toLanguage (procedureLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (procedureText p))
--             , PropList "contained" (fmap Xmlbf.toXml (procedureContained p))
             , PropList "extension" (fmap Xmlbf.toXml (procedureExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (procedureModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (procedureIdentifier p))
             , ValList  "instantiatesCanonical" (fmap toCanonical (procedureInstantiatesCanonical p))
             , ValList  "instantiatesUri" (fmap toUri (procedureInstantiatesUri p))
             , PropList "basedOn" (fmap Xmlbf.toXml (procedureBasedOn p))
             , PropList "partOf" (fmap Xmlbf.toXml (procedurePartOf p))
             , Val      "status" (     toProcedureStatus (procedureStatus p))
             , OptProp  "statusReason" (fmap Xmlbf.toXml (procedureStatusReason p))
             , OptProp  "category" (fmap Xmlbf.toXml (procedureCategory p))
             , OptProp  "code" (fmap Xmlbf.toXml (procedureCode p))
             , Prop     "subject" (HM.empty, Xmlbf.toXml (procedureSubject p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (procedureEncounter p))
             , toPerformedXml (procedurePerformed p)
             , OptProp  "recorder" (fmap Xmlbf.toXml (procedureRecorder p))
             , OptProp  "asserter" (fmap Xmlbf.toXml (procedureAsserter p))
             , PropList "performer" (fmap Xmlbf.toXml (procedurePerformer p))
             , OptProp  "location" (fmap Xmlbf.toXml (procedureLocation p))
             , PropList "reasonCode" (fmap Xmlbf.toXml (procedureReasonCode p))
             , PropList "reasonReference" (fmap Xmlbf.toXml (procedureReasonReference p))
             , PropList "bodySite" (fmap Xmlbf.toXml (procedureBodySite p))
             , OptProp  "outcome" (fmap Xmlbf.toXml (procedureOutcome p))
             , PropList "report" (fmap Xmlbf.toXml (procedureReport p))
             , PropList "complication" (fmap Xmlbf.toXml (procedureComplication p))
             , PropList "complicationDetail" (fmap Xmlbf.toXml (procedureComplicationDetail p))
             , PropList "followUp" (fmap Xmlbf.toXml (procedureFollowUp p))
             , PropList "note" (fmap Xmlbf.toXml (procedureNote p))
             , PropList "focalDevice" (fmap Xmlbf.toXml (procedureFocalDevice p))
             , PropList "usedReference" (fmap Xmlbf.toXml (procedureUsedReference p))
             , PropList "usedCode" (fmap Xmlbf.toXml (procedureUsedCode p))
             ]
          toPerformedXml ( Nothing   ) = (OptVal "performed" Nothing)
          toPerformedXml (Just (ProcedurePerformedDateTime p)) = Val   "performedDateTime" (toDateTime p)
          toPerformedXml (Just (ProcedurePerformedPeriod p)) = Prop  "performedPeriod" (HM.empty, Xmlbf.toXml p)
          toPerformedXml (Just (ProcedurePerformedString p)) = Val   "performedString" (toString p)
          toPerformedXml (Just (ProcedurePerformedAge p)) = Prop  "performedAge" (HM.empty, Xmlbf.toXml p)
          toPerformedXml (Just (ProcedurePerformedRange p)) = Prop  "performedRange" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml Procedure where
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
    partOf <- many     $ Xmlbf.pElement "partOf" Xmlbf.fromXml
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    statusReason <- optional $ Xmlbf.pElement "statusReason" Xmlbf.fromXml
    category <- optional $ Xmlbf.pElement "category" Xmlbf.fromXml
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    subject <-            Xmlbf.pElement "subject" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    performed <- fromPerformedXml
    recorder <- optional $ Xmlbf.pElement "recorder" Xmlbf.fromXml
    asserter <- optional $ Xmlbf.pElement "asserter" Xmlbf.fromXml
    performer <- many     $ Xmlbf.pElement "performer" Xmlbf.fromXml
    location <- optional $ Xmlbf.pElement "location" Xmlbf.fromXml
    reasonCode <- many     $ Xmlbf.pElement "reasonCode" Xmlbf.fromXml
    reasonReference <- many     $ Xmlbf.pElement "reasonReference" Xmlbf.fromXml
    bodySite <- many     $ Xmlbf.pElement "bodySite" Xmlbf.fromXml
    outcome <- optional $ Xmlbf.pElement "outcome" Xmlbf.fromXml
    report <- many     $ Xmlbf.pElement "report" Xmlbf.fromXml
    complication <- many     $ Xmlbf.pElement "complication" Xmlbf.fromXml
    complicationDetail <- many     $ Xmlbf.pElement "complicationDetail" Xmlbf.fromXml
    followUp <- many     $ Xmlbf.pElement "followUp" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    focalDevice <- many     $ Xmlbf.pElement "focalDevice" Xmlbf.fromXml
    usedReference <- many     $ Xmlbf.pElement "usedReference" Xmlbf.fromXml
    usedCode <- many     $ Xmlbf.pElement "usedCode" Xmlbf.fromXml
    return Procedure {
            procedureId = fmap fromId id
          , procedureMeta = meta
          , procedureImplicitRules = fmap fromUri implicitRules
          , procedureLanguage = fmap fromLanguage language
          , procedureText = text
--          , procedureContained = contained
          , procedureExtension = extension
          , procedureModifierExtension = modifierExtension
          , procedureIdentifier = identifier
          , procedureInstantiatesCanonical = fmap fromCanonical instantiatesCanonical
          , procedureInstantiatesUri = fmap fromUri instantiatesUri
          , procedureBasedOn = basedOn
          , procedurePartOf = partOf
          , procedureStatus =      fromProcedureStatus status
          , procedureStatusReason = statusReason
          , procedureCategory = category
          , procedureCode = code
          , procedureSubject = subject
          , procedureEncounter = encounter
          , procedurePerformed = performed
          , procedureRecorder = recorder
          , procedureAsserter = asserter
          , procedurePerformer = performer
          , procedureLocation = location
          , procedureReasonCode = reasonCode
          , procedureReasonReference = reasonReference
          , procedureBodySite = bodySite
          , procedureOutcome = outcome
          , procedureReport = report
          , procedureComplication = complication
          , procedureComplicationDetail = complicationDetail
          , procedureFollowUp = followUp
          , procedureNote = note
          , procedureFocalDevice = focalDevice
          , procedureUsedReference = usedReference
          , procedureUsedCode = usedCode
          }

    where 
      fromPerformedXml = parsePerformedDateTime <|> parsePerformedPeriod <|> parsePerformedString <|> parsePerformedAge <|> parsePerformedRange <|> pure Nothing
      parsePerformedDateTime = do
                has <- Xmlbf.pElement "performedDateTime" (Xmlbf.pAttr "value")
                return $ Just (ProcedurePerformedDateTime (     toDateTime has))
      parsePerformedPeriod = do
                has <- Xmlbf.pElement "performedPeriod" Xmlbf.fromXml
                return $ Just (ProcedurePerformedPeriod (                      has))
      parsePerformedString = do
                has <- Xmlbf.pElement "performedString" (Xmlbf.pAttr "value")
                return $ Just (ProcedurePerformedString (     toString has))
      parsePerformedAge = do
                has <- Xmlbf.pElement "performedAge" Xmlbf.fromXml
                return $ Just (ProcedurePerformedAge (                      has))
      parsePerformedRange = do
                has <- Xmlbf.pElement "performedRange" Xmlbf.fromXml
                return $ Just (ProcedurePerformedRange (                      has))


data ProcedurePerformer = ProcedurePerformer {
    procedurePerformerAttrId :: Maybe Text
  , procedurePerformerExtension :: [Extension]
  , procedurePerformerModifierExtension :: [Extension]
  , procedurePerformerFunction :: Maybe CodeableConcept
  , procedurePerformerActor :: Reference
  , procedurePerformerOnBehalfOf :: Maybe Reference
  }
--

instance ToJSON ProcedurePerformer where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (procedurePerformerAttrId p)
    ,  "extension" .= toJSON (procedurePerformerExtension p)
    ,  "modifierExtension" .= toJSON (procedurePerformerModifierExtension p)
    ,  "function" .= toJSON (procedurePerformerFunction p)
    ,  "actor" .= toJSON (procedurePerformerActor p)
    ,  "onBehalfOf" .= toJSON (procedurePerformerOnBehalfOf p)
    ]
instance FromJSON ProcedurePerformer where
  parseJSON = withObject "ProcedurePerformer" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        function <- o .:? "function"
        actor <- o .:  "actor"
        onBehalfOf <- o .:? "onBehalfOf"
        return ProcedurePerformer{
            procedurePerformerAttrId = id
          , procedurePerformerExtension = extension
          , procedurePerformerModifierExtension = modifierExtension
          , procedurePerformerFunction = function
          , procedurePerformerActor = actor
          , procedurePerformerOnBehalfOf = onBehalfOf
          }
instance Xmlbf.ToXml ProcedurePerformer where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (procedurePerformerAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (procedurePerformerExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (procedurePerformerModifierExtension p))
             , OptProp  "function" (fmap Xmlbf.toXml (procedurePerformerFunction p))
             , Prop     "actor" (HM.empty, Xmlbf.toXml (procedurePerformerActor p))
             , OptProp  "onBehalfOf" (fmap Xmlbf.toXml (procedurePerformerOnBehalfOf p))
             ]
instance Xmlbf.FromXml ProcedurePerformer where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    function <- optional $ Xmlbf.pElement "function" Xmlbf.fromXml
    actor <-            Xmlbf.pElement "actor" Xmlbf.fromXml
    onBehalfOf <- optional $ Xmlbf.pElement "onBehalfOf" Xmlbf.fromXml
    return ProcedurePerformer {
            procedurePerformerAttrId = id
          , procedurePerformerExtension = extension
          , procedurePerformerModifierExtension = modifierExtension
          , procedurePerformerFunction = function
          , procedurePerformerActor = actor
          , procedurePerformerOnBehalfOf = onBehalfOf
          }



data ProcedureFocalDevice = ProcedureFocalDevice {
    procedureFocalDeviceAttrId :: Maybe Text
  , procedureFocalDeviceExtension :: [Extension]
  , procedureFocalDeviceModifierExtension :: [Extension]
  , procedureFocalDeviceAction :: Maybe CodeableConcept
  , procedureFocalDeviceManipulated :: Reference
  }
--

instance ToJSON ProcedureFocalDevice where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (procedureFocalDeviceAttrId p)
    ,  "extension" .= toJSON (procedureFocalDeviceExtension p)
    ,  "modifierExtension" .= toJSON (procedureFocalDeviceModifierExtension p)
    ,  "action" .= toJSON (procedureFocalDeviceAction p)
    ,  "manipulated" .= toJSON (procedureFocalDeviceManipulated p)
    ]
instance FromJSON ProcedureFocalDevice where
  parseJSON = withObject "ProcedureFocalDevice" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        action <- o .:? "action"
        manipulated <- o .:  "manipulated"
        return ProcedureFocalDevice{
            procedureFocalDeviceAttrId = id
          , procedureFocalDeviceExtension = extension
          , procedureFocalDeviceModifierExtension = modifierExtension
          , procedureFocalDeviceAction = action
          , procedureFocalDeviceManipulated = manipulated
          }
instance Xmlbf.ToXml ProcedureFocalDevice where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (procedureFocalDeviceAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (procedureFocalDeviceExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (procedureFocalDeviceModifierExtension p))
             , OptProp  "action" (fmap Xmlbf.toXml (procedureFocalDeviceAction p))
             , Prop     "manipulated" (HM.empty, Xmlbf.toXml (procedureFocalDeviceManipulated p))
             ]
instance Xmlbf.FromXml ProcedureFocalDevice where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    action <- optional $ Xmlbf.pElement "action" Xmlbf.fromXml
    manipulated <-            Xmlbf.pElement "manipulated" Xmlbf.fromXml
    return ProcedureFocalDevice {
            procedureFocalDeviceAttrId = id
          , procedureFocalDeviceExtension = extension
          , procedureFocalDeviceModifierExtension = modifierExtension
          , procedureFocalDeviceAction = action
          , procedureFocalDeviceManipulated = manipulated
          }




