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
-- FHIR 4.0.0 ImagingStudy
--

module Data.FHIR.Resources.ImagingStudy where

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

data ImagingStudyStatus
    = ISSRegistered
    | ISSAvailable
    | ISSCancelled
    | ISSEnteredInError
    | ISSUnknown
  deriving (Eq, Show)

instance ToJSON ImagingStudyStatus where
    toJSON ISSRegistered = String "registered"
    toJSON ISSAvailable = String "available"
    toJSON ISSCancelled = String "cancelled"
    toJSON ISSEnteredInError = String "entered-in-error"
    toJSON ISSUnknown = String "unknown"
instance FromJSON ImagingStudyStatus where
    parseJSON "registered" = return ISSRegistered
    parseJSON "available" = return ISSAvailable
    parseJSON "cancelled" = return ISSCancelled
    parseJSON "entered-in-error" = return ISSEnteredInError
    parseJSON "unknown" = return ISSUnknown

toImagingStudyStatus ISSRegistered = "registered"
toImagingStudyStatus ISSAvailable = "available"
toImagingStudyStatus ISSCancelled = "cancelled"
toImagingStudyStatus ISSEnteredInError = "entered-in-error"
toImagingStudyStatus ISSUnknown = "unknown"
fromImagingStudyStatus "registered" = ISSRegistered
fromImagingStudyStatus "available" = ISSAvailable
fromImagingStudyStatus "cancelled" = ISSCancelled
fromImagingStudyStatus "entered-in-error" = ISSEnteredInError
fromImagingStudyStatus "unknown" = ISSUnknown


data ImagingStudy = ImagingStudy {
    imagingStudyId :: Maybe Id
  , imagingStudyMeta :: Maybe Meta
  , imagingStudyImplicitRules :: Maybe Uri
  , imagingStudyLanguage :: Maybe Language
  , imagingStudyText :: Maybe Narrative
--    imagingStudyContained :: [ResourceContainer]
  , imagingStudyExtension :: [Extension]
  , imagingStudyModifierExtension :: [Extension]
  , imagingStudyIdentifier :: [Identifier]
  , imagingStudyStatus :: ImagingStudyStatus
  , imagingStudyModality :: [Coding]
  , imagingStudySubject :: Reference
  , imagingStudyEncounter :: Maybe Reference
  , imagingStudyStarted :: Maybe DateTime
  , imagingStudyBasedOn :: [Reference]
  , imagingStudyReferrer :: Maybe Reference
  , imagingStudyInterpreter :: [Reference]
  , imagingStudyEndpoint :: [Reference]
  , imagingStudyNumberOfSeries :: Maybe UnsignedInt
  , imagingStudyNumberOfInstances :: Maybe UnsignedInt
  , imagingStudyProcedureReference :: Maybe Reference
  , imagingStudyProcedureCode :: [CodeableConcept]
  , imagingStudyLocation :: Maybe Reference
  , imagingStudyReasonCode :: [CodeableConcept]
  , imagingStudyReasonReference :: [Reference]
  , imagingStudyNote :: [Annotation]
  , imagingStudyDescription :: Maybe Text
  , imagingStudySeries :: [ImagingStudySeries]
  }
--

instance ToJSON ImagingStudy where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "ImagingStudy")
    ,  "id" .= toJSON (imagingStudyId p)
    ,  "meta" .= toJSON (imagingStudyMeta p)
    ,  "implicitRules" .= toJSON (imagingStudyImplicitRules p)
    ,  "language" .= toJSON (imagingStudyLanguage p)
    ,  "text" .= toJSON (imagingStudyText p)
--    , "contained" .= toJSON (imagingStudyContained p)
    ,  "extension" .= toJSON (imagingStudyExtension p)
    ,  "modifierExtension" .= toJSON (imagingStudyModifierExtension p)
    ,  "identifier" .= toJSON (imagingStudyIdentifier p)
    ,  "status" .= toJSON (imagingStudyStatus p)
    ,  "modality" .= toJSON (imagingStudyModality p)
    ,  "subject" .= toJSON (imagingStudySubject p)
    ,  "encounter" .= toJSON (imagingStudyEncounter p)
    ,  "started" .= toJSON (imagingStudyStarted p)
    ,  "basedOn" .= toJSON (imagingStudyBasedOn p)
    ,  "referrer" .= toJSON (imagingStudyReferrer p)
    ,  "interpreter" .= toJSON (imagingStudyInterpreter p)
    ,  "endpoint" .= toJSON (imagingStudyEndpoint p)
    ,  "numberOfSeries" .= toJSON (imagingStudyNumberOfSeries p)
    ,  "numberOfInstances" .= toJSON (imagingStudyNumberOfInstances p)
    ,  "procedureReference" .= toJSON (imagingStudyProcedureReference p)
    ,  "procedureCode" .= toJSON (imagingStudyProcedureCode p)
    ,  "location" .= toJSON (imagingStudyLocation p)
    ,  "reasonCode" .= toJSON (imagingStudyReasonCode p)
    ,  "reasonReference" .= toJSON (imagingStudyReasonReference p)
    ,  "note" .= toJSON (imagingStudyNote p)
    ,  "description" .= toJSON (imagingStudyDescription p)
    ,  "series" .= toJSON (imagingStudySeries p)
    ]
instance FromJSON ImagingStudy where
  parseJSON = withObject "ImagingStudy" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "ImagingStudy" -> do
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
        modality <- o .:? "modality" .!= []
        subject <- o .:  "subject"
        encounter <- o .:? "encounter"
        started <- o .:? "started"
        basedOn <- o .:? "basedOn" .!= []
        referrer <- o .:? "referrer"
        interpreter <- o .:? "interpreter" .!= []
        endpoint <- o .:? "endpoint" .!= []
        numberOfSeries <- o .:? "numberOfSeries"
        numberOfInstances <- o .:? "numberOfInstances"
        procedureReference <- o .:? "procedureReference"
        procedureCode <- o .:? "procedureCode" .!= []
        location <- o .:? "location"
        reasonCode <- o .:? "reasonCode" .!= []
        reasonReference <- o .:? "reasonReference" .!= []
        note <- o .:? "note" .!= []
        description <- o .:? "description"
        series <- o .:? "series" .!= []
        return ImagingStudy{
            imagingStudyId = id
          , imagingStudyMeta = meta
          , imagingStudyImplicitRules = implicitRules
          , imagingStudyLanguage = language
          , imagingStudyText = text
--          , imagingStudyContained = contained
          , imagingStudyExtension = extension
          , imagingStudyModifierExtension = modifierExtension
          , imagingStudyIdentifier = identifier
          , imagingStudyStatus = status
          , imagingStudyModality = modality
          , imagingStudySubject = subject
          , imagingStudyEncounter = encounter
          , imagingStudyStarted = started
          , imagingStudyBasedOn = basedOn
          , imagingStudyReferrer = referrer
          , imagingStudyInterpreter = interpreter
          , imagingStudyEndpoint = endpoint
          , imagingStudyNumberOfSeries = numberOfSeries
          , imagingStudyNumberOfInstances = numberOfInstances
          , imagingStudyProcedureReference = procedureReference
          , imagingStudyProcedureCode = procedureCode
          , imagingStudyLocation = location
          , imagingStudyReasonCode = reasonCode
          , imagingStudyReasonReference = reasonReference
          , imagingStudyNote = note
          , imagingStudyDescription = description
          , imagingStudySeries = series
          }
      _ -> fail "not a ImagingStudy"
instance Xmlbf.ToXml ImagingStudy where
  toXml p = Xmlbf.element "ImagingStudy" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (imagingStudyId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (imagingStudyMeta p))
             , OptVal   "implicitRules" (fmap toUri (imagingStudyImplicitRules p))
             , OptVal   "language" (fmap toLanguage (imagingStudyLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (imagingStudyText p))
--             , PropList "contained" (fmap Xmlbf.toXml (imagingStudyContained p))
             , PropList "extension" (fmap Xmlbf.toXml (imagingStudyExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (imagingStudyModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (imagingStudyIdentifier p))
             , Val      "status" (     toImagingStudyStatus (imagingStudyStatus p))
             , PropList "modality" (fmap Xmlbf.toXml (imagingStudyModality p))
             , Prop     "subject" (HM.empty, Xmlbf.toXml (imagingStudySubject p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (imagingStudyEncounter p))
             , OptVal   "started" (fmap toDateTime (imagingStudyStarted p))
             , PropList "basedOn" (fmap Xmlbf.toXml (imagingStudyBasedOn p))
             , OptProp  "referrer" (fmap Xmlbf.toXml (imagingStudyReferrer p))
             , PropList "interpreter" (fmap Xmlbf.toXml (imagingStudyInterpreter p))
             , PropList "endpoint" (fmap Xmlbf.toXml (imagingStudyEndpoint p))
             , OptVal   "numberOfSeries" (fmap toUnsignedInt (imagingStudyNumberOfSeries p))
             , OptVal   "numberOfInstances" (fmap toUnsignedInt (imagingStudyNumberOfInstances p))
             , OptProp  "procedureReference" (fmap Xmlbf.toXml (imagingStudyProcedureReference p))
             , PropList "procedureCode" (fmap Xmlbf.toXml (imagingStudyProcedureCode p))
             , OptProp  "location" (fmap Xmlbf.toXml (imagingStudyLocation p))
             , PropList "reasonCode" (fmap Xmlbf.toXml (imagingStudyReasonCode p))
             , PropList "reasonReference" (fmap Xmlbf.toXml (imagingStudyReasonReference p))
             , PropList "note" (fmap Xmlbf.toXml (imagingStudyNote p))
             , OptVal   "description" (fmap toString (imagingStudyDescription p))
             , PropList "series" (fmap Xmlbf.toXml (imagingStudySeries p))
             ]
instance Xmlbf.FromXml ImagingStudy where
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
    modality <- many     $ Xmlbf.pElement "modality" Xmlbf.fromXml
    subject <-            Xmlbf.pElement "subject" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    started <- optional $ Xmlbf.pElement "started" (Xmlbf.pAttr "value")
    basedOn <- many     $ Xmlbf.pElement "basedOn" Xmlbf.fromXml
    referrer <- optional $ Xmlbf.pElement "referrer" Xmlbf.fromXml
    interpreter <- many     $ Xmlbf.pElement "interpreter" Xmlbf.fromXml
    endpoint <- many     $ Xmlbf.pElement "endpoint" Xmlbf.fromXml
    numberOfSeries <- optional $ Xmlbf.pElement "numberOfSeries" (Xmlbf.pAttr "value")
    numberOfInstances <- optional $ Xmlbf.pElement "numberOfInstances" (Xmlbf.pAttr "value")
    procedureReference <- optional $ Xmlbf.pElement "procedureReference" Xmlbf.fromXml
    procedureCode <- many     $ Xmlbf.pElement "procedureCode" Xmlbf.fromXml
    location <- optional $ Xmlbf.pElement "location" Xmlbf.fromXml
    reasonCode <- many     $ Xmlbf.pElement "reasonCode" Xmlbf.fromXml
    reasonReference <- many     $ Xmlbf.pElement "reasonReference" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    series <- many     $ Xmlbf.pElement "series" Xmlbf.fromXml
    return ImagingStudy {
            imagingStudyId = fmap fromId id
          , imagingStudyMeta = meta
          , imagingStudyImplicitRules = fmap fromUri implicitRules
          , imagingStudyLanguage = fmap fromLanguage language
          , imagingStudyText = text
--          , imagingStudyContained = contained
          , imagingStudyExtension = extension
          , imagingStudyModifierExtension = modifierExtension
          , imagingStudyIdentifier = identifier
          , imagingStudyStatus =      fromImagingStudyStatus status
          , imagingStudyModality = modality
          , imagingStudySubject = subject
          , imagingStudyEncounter = encounter
          , imagingStudyStarted = fmap fromDateTime started
          , imagingStudyBasedOn = basedOn
          , imagingStudyReferrer = referrer
          , imagingStudyInterpreter = interpreter
          , imagingStudyEndpoint = endpoint
          , imagingStudyNumberOfSeries = fmap fromUnsignedInt numberOfSeries
          , imagingStudyNumberOfInstances = fmap fromUnsignedInt numberOfInstances
          , imagingStudyProcedureReference = procedureReference
          , imagingStudyProcedureCode = procedureCode
          , imagingStudyLocation = location
          , imagingStudyReasonCode = reasonCode
          , imagingStudyReasonReference = reasonReference
          , imagingStudyNote = note
          , imagingStudyDescription = fmap fromString description
          , imagingStudySeries = series
          }



data ImagingStudyInstance = ImagingStudyInstance {
    imagingStudyInstanceAttrId :: Maybe Text
  , imagingStudyInstanceExtension :: [Extension]
  , imagingStudyInstanceModifierExtension :: [Extension]
  , imagingStudyInstanceUid :: Id
  , imagingStudyInstanceSopClass :: Coding
  , imagingStudyInstanceNumber :: Maybe UnsignedInt
  , imagingStudyInstanceTitle :: Maybe Text
  }
--

instance ToJSON ImagingStudyInstance where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (imagingStudyInstanceAttrId p)
    ,  "extension" .= toJSON (imagingStudyInstanceExtension p)
    ,  "modifierExtension" .= toJSON (imagingStudyInstanceModifierExtension p)
    ,  "uid" .= toJSON (imagingStudyInstanceUid p)
    ,  "sopClass" .= toJSON (imagingStudyInstanceSopClass p)
    ,  "number" .= toJSON (imagingStudyInstanceNumber p)
    ,  "title" .= toJSON (imagingStudyInstanceTitle p)
    ]
instance FromJSON ImagingStudyInstance where
  parseJSON = withObject "ImagingStudyInstance" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        uid <- o .:  "uid"
        sopClass <- o .:  "sopClass"
        number <- o .:? "number"
        title <- o .:? "title"
        return ImagingStudyInstance{
            imagingStudyInstanceAttrId = id
          , imagingStudyInstanceExtension = extension
          , imagingStudyInstanceModifierExtension = modifierExtension
          , imagingStudyInstanceUid = uid
          , imagingStudyInstanceSopClass = sopClass
          , imagingStudyInstanceNumber = number
          , imagingStudyInstanceTitle = title
          }
instance Xmlbf.ToXml ImagingStudyInstance where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (imagingStudyInstanceAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (imagingStudyInstanceExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (imagingStudyInstanceModifierExtension p))
             , Val      "uid" (     toId (imagingStudyInstanceUid p))
             , Prop     "sopClass" (HM.empty, Xmlbf.toXml (imagingStudyInstanceSopClass p))
             , OptVal   "number" (fmap toUnsignedInt (imagingStudyInstanceNumber p))
             , OptVal   "title" (fmap toString (imagingStudyInstanceTitle p))
             ]
instance Xmlbf.FromXml ImagingStudyInstance where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    uid <-            Xmlbf.pElement "uid" (Xmlbf.pAttr "value")
    sopClass <-            Xmlbf.pElement "sopClass" Xmlbf.fromXml
    number <- optional $ Xmlbf.pElement "number" (Xmlbf.pAttr "value")
    title <- optional $ Xmlbf.pElement "title" (Xmlbf.pAttr "value")
    return ImagingStudyInstance {
            imagingStudyInstanceAttrId = id
          , imagingStudyInstanceExtension = extension
          , imagingStudyInstanceModifierExtension = modifierExtension
          , imagingStudyInstanceUid =      fromId uid
          , imagingStudyInstanceSopClass = sopClass
          , imagingStudyInstanceNumber = fmap fromUnsignedInt number
          , imagingStudyInstanceTitle = fmap fromString title
          }



data ImagingStudyPerformer = ImagingStudyPerformer {
    imagingStudyPerformerAttrId :: Maybe Text
  , imagingStudyPerformerExtension :: [Extension]
  , imagingStudyPerformerModifierExtension :: [Extension]
  , imagingStudyPerformerFunction :: Maybe CodeableConcept
  , imagingStudyPerformerActor :: Reference
  }
--

instance ToJSON ImagingStudyPerformer where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (imagingStudyPerformerAttrId p)
    ,  "extension" .= toJSON (imagingStudyPerformerExtension p)
    ,  "modifierExtension" .= toJSON (imagingStudyPerformerModifierExtension p)
    ,  "function" .= toJSON (imagingStudyPerformerFunction p)
    ,  "actor" .= toJSON (imagingStudyPerformerActor p)
    ]
instance FromJSON ImagingStudyPerformer where
  parseJSON = withObject "ImagingStudyPerformer" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        function <- o .:? "function"
        actor <- o .:  "actor"
        return ImagingStudyPerformer{
            imagingStudyPerformerAttrId = id
          , imagingStudyPerformerExtension = extension
          , imagingStudyPerformerModifierExtension = modifierExtension
          , imagingStudyPerformerFunction = function
          , imagingStudyPerformerActor = actor
          }
instance Xmlbf.ToXml ImagingStudyPerformer where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (imagingStudyPerformerAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (imagingStudyPerformerExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (imagingStudyPerformerModifierExtension p))
             , OptProp  "function" (fmap Xmlbf.toXml (imagingStudyPerformerFunction p))
             , Prop     "actor" (HM.empty, Xmlbf.toXml (imagingStudyPerformerActor p))
             ]
instance Xmlbf.FromXml ImagingStudyPerformer where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    function <- optional $ Xmlbf.pElement "function" Xmlbf.fromXml
    actor <-            Xmlbf.pElement "actor" Xmlbf.fromXml
    return ImagingStudyPerformer {
            imagingStudyPerformerAttrId = id
          , imagingStudyPerformerExtension = extension
          , imagingStudyPerformerModifierExtension = modifierExtension
          , imagingStudyPerformerFunction = function
          , imagingStudyPerformerActor = actor
          }



data ImagingStudySeries = ImagingStudySeries {
    imagingStudySeriesAttrId :: Maybe Text
  , imagingStudySeriesExtension :: [Extension]
  , imagingStudySeriesModifierExtension :: [Extension]
  , imagingStudySeriesUid :: Id
  , imagingStudySeriesNumber :: Maybe UnsignedInt
  , imagingStudySeriesModality :: Coding
  , imagingStudySeriesDescription :: Maybe Text
  , imagingStudySeriesNumberOfInstances :: Maybe UnsignedInt
  , imagingStudySeriesEndpoint :: [Reference]
  , imagingStudySeriesBodySite :: Maybe Coding
  , imagingStudySeriesLaterality :: Maybe Coding
  , imagingStudySeriesSpecimen :: [Reference]
  , imagingStudySeriesStarted :: Maybe DateTime
  , imagingStudySeriesPerformer :: [ImagingStudyPerformer]
  , imagingStudySeriesInstance :: [ImagingStudyInstance]
  }
--

instance ToJSON ImagingStudySeries where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (imagingStudySeriesAttrId p)
    ,  "extension" .= toJSON (imagingStudySeriesExtension p)
    ,  "modifierExtension" .= toJSON (imagingStudySeriesModifierExtension p)
    ,  "uid" .= toJSON (imagingStudySeriesUid p)
    ,  "number" .= toJSON (imagingStudySeriesNumber p)
    ,  "modality" .= toJSON (imagingStudySeriesModality p)
    ,  "description" .= toJSON (imagingStudySeriesDescription p)
    ,  "numberOfInstances" .= toJSON (imagingStudySeriesNumberOfInstances p)
    ,  "endpoint" .= toJSON (imagingStudySeriesEndpoint p)
    ,  "bodySite" .= toJSON (imagingStudySeriesBodySite p)
    ,  "laterality" .= toJSON (imagingStudySeriesLaterality p)
    ,  "specimen" .= toJSON (imagingStudySeriesSpecimen p)
    ,  "started" .= toJSON (imagingStudySeriesStarted p)
    ,  "performer" .= toJSON (imagingStudySeriesPerformer p)
    ,  "instance" .= toJSON (imagingStudySeriesInstance p)
    ]
instance FromJSON ImagingStudySeries where
  parseJSON = withObject "ImagingStudySeries" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        uid <- o .:  "uid"
        number <- o .:? "number"
        modality <- o .:  "modality"
        description <- o .:? "description"
        numberOfInstances <- o .:? "numberOfInstances"
        endpoint <- o .:? "endpoint" .!= []
        bodySite <- o .:? "bodySite"
        laterality <- o .:? "laterality"
        specimen <- o .:? "specimen" .!= []
        started <- o .:? "started"
        performer <- o .:? "performer" .!= []
        ins <- o .:? "instance" .!= []
        return ImagingStudySeries{
            imagingStudySeriesAttrId = id
          , imagingStudySeriesExtension = extension
          , imagingStudySeriesModifierExtension = modifierExtension
          , imagingStudySeriesUid = uid
          , imagingStudySeriesNumber = number
          , imagingStudySeriesModality = modality
          , imagingStudySeriesDescription = description
          , imagingStudySeriesNumberOfInstances = numberOfInstances
          , imagingStudySeriesEndpoint = endpoint
          , imagingStudySeriesBodySite = bodySite
          , imagingStudySeriesLaterality = laterality
          , imagingStudySeriesSpecimen = specimen
          , imagingStudySeriesStarted = started
          , imagingStudySeriesPerformer = performer
          , imagingStudySeriesInstance = ins
          }
instance Xmlbf.ToXml ImagingStudySeries where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (imagingStudySeriesAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (imagingStudySeriesExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (imagingStudySeriesModifierExtension p))
             , Val      "uid" (     toId (imagingStudySeriesUid p))
             , OptVal   "number" (fmap toUnsignedInt (imagingStudySeriesNumber p))
             , Prop     "modality" (HM.empty, Xmlbf.toXml (imagingStudySeriesModality p))
             , OptVal   "description" (fmap toString (imagingStudySeriesDescription p))
             , OptVal   "numberOfInstances" (fmap toUnsignedInt (imagingStudySeriesNumberOfInstances p))
             , PropList "endpoint" (fmap Xmlbf.toXml (imagingStudySeriesEndpoint p))
             , OptProp  "bodySite" (fmap Xmlbf.toXml (imagingStudySeriesBodySite p))
             , OptProp  "laterality" (fmap Xmlbf.toXml (imagingStudySeriesLaterality p))
             , PropList "specimen" (fmap Xmlbf.toXml (imagingStudySeriesSpecimen p))
             , OptVal   "started" (fmap toDateTime (imagingStudySeriesStarted p))
             , PropList "performer" (fmap Xmlbf.toXml (imagingStudySeriesPerformer p))
             , PropList "instance" (fmap Xmlbf.toXml (imagingStudySeriesInstance p))
             ]
instance Xmlbf.FromXml ImagingStudySeries where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    uid <-            Xmlbf.pElement "uid" (Xmlbf.pAttr "value")
    number <- optional $ Xmlbf.pElement "number" (Xmlbf.pAttr "value")
    modality <-            Xmlbf.pElement "modality" Xmlbf.fromXml
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    numberOfInstances <- optional $ Xmlbf.pElement "numberOfInstances" (Xmlbf.pAttr "value")
    endpoint <- many     $ Xmlbf.pElement "endpoint" Xmlbf.fromXml
    bodySite <- optional $ Xmlbf.pElement "bodySite" Xmlbf.fromXml
    laterality <- optional $ Xmlbf.pElement "laterality" Xmlbf.fromXml
    specimen <- many     $ Xmlbf.pElement "specimen" Xmlbf.fromXml
    started <- optional $ Xmlbf.pElement "started" (Xmlbf.pAttr "value")
    performer <- many     $ Xmlbf.pElement "performer" Xmlbf.fromXml
    ins <- many     $ Xmlbf.pElement "instance" Xmlbf.fromXml
    return ImagingStudySeries {
            imagingStudySeriesAttrId = id
          , imagingStudySeriesExtension = extension
          , imagingStudySeriesModifierExtension = modifierExtension
          , imagingStudySeriesUid =      fromId uid
          , imagingStudySeriesNumber = fmap fromUnsignedInt number
          , imagingStudySeriesModality = modality
          , imagingStudySeriesDescription = fmap fromString description
          , imagingStudySeriesNumberOfInstances = fmap fromUnsignedInt numberOfInstances
          , imagingStudySeriesEndpoint = endpoint
          , imagingStudySeriesBodySite = bodySite
          , imagingStudySeriesLaterality = laterality
          , imagingStudySeriesSpecimen = specimen
          , imagingStudySeriesStarted = fmap fromDateTime started
          , imagingStudySeriesPerformer = performer
          , imagingStudySeriesInstance = ins
          }




