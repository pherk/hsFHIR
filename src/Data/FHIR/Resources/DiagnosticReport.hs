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
-- FHIR 4.0.0 DiagnosticReport
--

module Data.FHIR.Resources.DiagnosticReport where

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

data DiagnosticReportStatus
    = DRSRegistered
    | DRSPartial
    | DRSPreliminary
    | DRSFinal
    | DRSAmended
    | DRSCorrected
    | DRSAppended
    | DRSCancelled
    | DRSEnteredInError
    | DRSUnknown
  deriving (Eq, Show)

instance ToJSON DiagnosticReportStatus where
    toJSON DRSRegistered = String "registered"
    toJSON DRSPartial = String "partial"
    toJSON DRSPreliminary = String "preliminary"
    toJSON DRSFinal = String "final"
    toJSON DRSAmended = String "amended"
    toJSON DRSCorrected = String "corrected"
    toJSON DRSAppended = String "appended"
    toJSON DRSCancelled = String "cancelled"
    toJSON DRSEnteredInError = String "entered-in-error"
    toJSON DRSUnknown = String "unknown"
instance FromJSON DiagnosticReportStatus where
    parseJSON "registered" = return DRSRegistered
    parseJSON "partial" = return DRSPartial
    parseJSON "preliminary" = return DRSPreliminary
    parseJSON "final" = return DRSFinal
    parseJSON "amended" = return DRSAmended
    parseJSON "corrected" = return DRSCorrected
    parseJSON "appended" = return DRSAppended
    parseJSON "cancelled" = return DRSCancelled
    parseJSON "entered-in-error" = return DRSEnteredInError
    parseJSON "unknown" = return DRSUnknown

toDiagnosticReportStatus DRSRegistered = "registered"
toDiagnosticReportStatus DRSPartial = "partial"
toDiagnosticReportStatus DRSPreliminary = "preliminary"
toDiagnosticReportStatus DRSFinal = "final"
toDiagnosticReportStatus DRSAmended = "amended"
toDiagnosticReportStatus DRSCorrected = "corrected"
toDiagnosticReportStatus DRSAppended = "appended"
toDiagnosticReportStatus DRSCancelled = "cancelled"
toDiagnosticReportStatus DRSEnteredInError = "entered-in-error"
toDiagnosticReportStatus DRSUnknown = "unknown"
fromDiagnosticReportStatus "registered" = DRSRegistered
fromDiagnosticReportStatus "partial" = DRSPartial
fromDiagnosticReportStatus "preliminary" = DRSPreliminary
fromDiagnosticReportStatus "final" = DRSFinal
fromDiagnosticReportStatus "amended" = DRSAmended
fromDiagnosticReportStatus "corrected" = DRSCorrected
fromDiagnosticReportStatus "appended" = DRSAppended
fromDiagnosticReportStatus "cancelled" = DRSCancelled
fromDiagnosticReportStatus "entered-in-error" = DRSEnteredInError
fromDiagnosticReportStatus "unknown" = DRSUnknown


data DiagnosticReportEffective
    = DiagnosticReportEffectiveDateTime DateTime
    | DiagnosticReportEffectivePeriod Period
    deriving (Eq, Show)

data DiagnosticReport = DiagnosticReport {
    diagnosticReportId :: Maybe Id
  , diagnosticReportMeta :: Maybe Meta
  , diagnosticReportImplicitRules :: Maybe Uri
  , diagnosticReportLanguage :: Maybe Language
  , diagnosticReportText :: Maybe Narrative
--    diagnosticReportContained :: [ResourceContainer]
  , diagnosticReportExtension :: [Extension]
  , diagnosticReportModifierExtension :: [Extension]
  , diagnosticReportIdentifier :: [Identifier]
  , diagnosticReportBasedOn :: [Reference]
  , diagnosticReportStatus :: DiagnosticReportStatus
  , diagnosticReportCategory :: [CodeableConcept]
  , diagnosticReportCode :: CodeableConcept
  , diagnosticReportSubject :: Maybe Reference
  , diagnosticReportEncounter :: Maybe Reference
  , diagnosticReportEffective :: Maybe DiagnosticReportEffective
  , diagnosticReportIssued :: Maybe Instant
  , diagnosticReportPerformer :: [Reference]
  , diagnosticReportResultsInterpreter :: [Reference]
  , diagnosticReportSpecimen :: [Reference]
  , diagnosticReportResult :: [Reference]
  , diagnosticReportImagingStudy :: [Reference]
  , diagnosticReportMedia :: [DiagnosticReportMedia]
  , diagnosticReportConclusion :: Maybe Text
  , diagnosticReportConclusionCode :: [CodeableConcept]
  , diagnosticReportPresentedForm :: [Attachment]
  }
--

instance ToJSON DiagnosticReport where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "DiagnosticReport")
    ,  "id" .= toJSON (diagnosticReportId p)
    ,  "meta" .= toJSON (diagnosticReportMeta p)
    ,  "implicitRules" .= toJSON (diagnosticReportImplicitRules p)
    ,  "language" .= toJSON (diagnosticReportLanguage p)
    ,  "text" .= toJSON (diagnosticReportText p)
--    , "contained" .= toJSON (diagnosticReportContained p)
    ,  "extension" .= toJSON (diagnosticReportExtension p)
    ,  "modifierExtension" .= toJSON (diagnosticReportModifierExtension p)
    ,  "identifier" .= toJSON (diagnosticReportIdentifier p)
    ,  "basedOn" .= toJSON (diagnosticReportBasedOn p)
    ,  "status" .= toJSON (diagnosticReportStatus p)
    ,  "category" .= toJSON (diagnosticReportCategory p)
    ,  "code" .= toJSON (diagnosticReportCode p)
    ,  "subject" .= toJSON (diagnosticReportSubject p)
    ,  "encounter" .= toJSON (diagnosticReportEncounter p)
    , toEffectiveJSON (diagnosticReportEffective p)
    ,  "issued" .= toJSON (diagnosticReportIssued p)
    ,  "performer" .= toJSON (diagnosticReportPerformer p)
    ,  "resultsInterpreter" .= toJSON (diagnosticReportResultsInterpreter p)
    ,  "specimen" .= toJSON (diagnosticReportSpecimen p)
    ,  "result" .= toJSON (diagnosticReportResult p)
    ,  "imagingStudy" .= toJSON (diagnosticReportImagingStudy p)
    ,  "media" .= toJSON (diagnosticReportMedia p)
    ,  "conclusion" .= toJSON (diagnosticReportConclusion p)
    ,  "conclusionCode" .= toJSON (diagnosticReportConclusionCode p)
    ,  "presentedForm" .= toJSON (diagnosticReportPresentedForm p)
    ]
    where 
      toEffectiveJSON (     Nothing   ) = ("effective", Null)
      toEffectiveJSON (Just (DiagnosticReportEffectiveDateTime c)) = ("effective", toJSON c)
      toEffectiveJSON (Just (DiagnosticReportEffectivePeriod c)) = ("effective", toJSON c)
instance FromJSON DiagnosticReport where
  parseJSON = withObject "DiagnosticReport" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "DiagnosticReport" -> do
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
        status <- o .:  "status"
        category <- o .:? "category" .!= []
        code <- o .:  "code"
        subject <- o .:? "subject"
        encounter <- o .:? "encounter"
        effective <- parseEffective o
        issued <- o .:? "issued"
        performer <- o .:? "performer" .!= []
        resultsInterpreter <- o .:? "resultsInterpreter" .!= []
        specimen <- o .:? "specimen" .!= []
        result <- o .:? "result" .!= []
        imagingStudy <- o .:? "imagingStudy" .!= []
        media <- o .:? "media" .!= []
        conclusion <- o .:? "conclusion"
        conclusionCode <- o .:? "conclusionCode" .!= []
        presentedForm <- o .:? "presentedForm" .!= []
        return DiagnosticReport{
            diagnosticReportId = id
          , diagnosticReportMeta = meta
          , diagnosticReportImplicitRules = implicitRules
          , diagnosticReportLanguage = language
          , diagnosticReportText = text
--          , diagnosticReportContained = contained
          , diagnosticReportExtension = extension
          , diagnosticReportModifierExtension = modifierExtension
          , diagnosticReportIdentifier = identifier
          , diagnosticReportBasedOn = basedOn
          , diagnosticReportStatus = status
          , diagnosticReportCategory = category
          , diagnosticReportCode = code
          , diagnosticReportSubject = subject
          , diagnosticReportEncounter = encounter
          , diagnosticReportEffective = effective
          , diagnosticReportIssued = issued
          , diagnosticReportPerformer = performer
          , diagnosticReportResultsInterpreter = resultsInterpreter
          , diagnosticReportSpecimen = specimen
          , diagnosticReportResult = result
          , diagnosticReportImagingStudy = imagingStudy
          , diagnosticReportMedia = media
          , diagnosticReportConclusion = conclusion
          , diagnosticReportConclusionCode = conclusionCode
          , diagnosticReportPresentedForm = presentedForm
          }
      _ -> fail "not a DiagnosticReport"
    where 
      parseEffective o = parseEffectiveDateTime o <|> parseEffectivePeriod o
      parseEffectiveDateTime o = do
                has <- o .: "effectiveDateTime"
                return $ Just (DiagnosticReportEffectiveDateTime has)
      parseEffectivePeriod o = do
                has <- o .: "effectivePeriod"
                return $ Just (DiagnosticReportEffectivePeriod has)
instance Xmlbf.ToXml DiagnosticReport where
  toXml p = Xmlbf.element "DiagnosticReport" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (diagnosticReportId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (diagnosticReportMeta p))
             , OptVal   "implicitRules" (fmap toUri (diagnosticReportImplicitRules p))
             , OptVal   "language" (fmap toLanguage (diagnosticReportLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (diagnosticReportText p))
--             , PropList "contained" (fmap Xmlbf.toXml (diagnosticReportContained p))
             , PropList "extension" (fmap Xmlbf.toXml (diagnosticReportExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (diagnosticReportModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (diagnosticReportIdentifier p))
             , PropList "basedOn" (fmap Xmlbf.toXml (diagnosticReportBasedOn p))
             , Val      "status" (     toDiagnosticReportStatus (diagnosticReportStatus p))
             , PropList "category" (fmap Xmlbf.toXml (diagnosticReportCategory p))
             , Prop     "code" (HM.empty, Xmlbf.toXml (diagnosticReportCode p))
             , OptProp  "subject" (fmap Xmlbf.toXml (diagnosticReportSubject p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (diagnosticReportEncounter p))
             , toEffectiveXml (diagnosticReportEffective p)
             , OptVal   "issued" (fmap toInstant (diagnosticReportIssued p))
             , PropList "performer" (fmap Xmlbf.toXml (diagnosticReportPerformer p))
             , PropList "resultsInterpreter" (fmap Xmlbf.toXml (diagnosticReportResultsInterpreter p))
             , PropList "specimen" (fmap Xmlbf.toXml (diagnosticReportSpecimen p))
             , PropList "result" (fmap Xmlbf.toXml (diagnosticReportResult p))
             , PropList "imagingStudy" (fmap Xmlbf.toXml (diagnosticReportImagingStudy p))
             , PropList "media" (fmap Xmlbf.toXml (diagnosticReportMedia p))
             , OptVal   "conclusion" (fmap toString (diagnosticReportConclusion p))
             , PropList "conclusionCode" (fmap Xmlbf.toXml (diagnosticReportConclusionCode p))
             , PropList "presentedForm" (fmap Xmlbf.toXml (diagnosticReportPresentedForm p))
             ]
          toEffectiveXml ( Nothing   ) = (OptVal "effective" Nothing)
          toEffectiveXml (Just (DiagnosticReportEffectiveDateTime p)) = Val   "effectiveDateTime" (toDateTime p)
          toEffectiveXml (Just (DiagnosticReportEffectivePeriod p)) = Prop  "effectivePeriod" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml DiagnosticReport where
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
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    category <- many     $ Xmlbf.pElement "category" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" Xmlbf.fromXml
    subject <- optional $ Xmlbf.pElement "subject" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    effective <- fromEffectiveXml
    issued <- optional $ Xmlbf.pElement "issued" (Xmlbf.pAttr "value")
    performer <- many     $ Xmlbf.pElement "performer" Xmlbf.fromXml
    resultsInterpreter <- many     $ Xmlbf.pElement "resultsInterpreter" Xmlbf.fromXml
    specimen <- many     $ Xmlbf.pElement "specimen" Xmlbf.fromXml
    result <- many     $ Xmlbf.pElement "result" Xmlbf.fromXml
    imagingStudy <- many     $ Xmlbf.pElement "imagingStudy" Xmlbf.fromXml
    media <- many     $ Xmlbf.pElement "media" Xmlbf.fromXml
    conclusion <- optional $ Xmlbf.pElement "conclusion" (Xmlbf.pAttr "value")
    conclusionCode <- many     $ Xmlbf.pElement "conclusionCode" Xmlbf.fromXml
    presentedForm <- many     $ Xmlbf.pElement "presentedForm" Xmlbf.fromXml
    return DiagnosticReport {
            diagnosticReportId = fmap fromId id
          , diagnosticReportMeta = meta
          , diagnosticReportImplicitRules = fmap fromUri implicitRules
          , diagnosticReportLanguage = fmap fromLanguage language
          , diagnosticReportText = text
--          , diagnosticReportContained = contained
          , diagnosticReportExtension = extension
          , diagnosticReportModifierExtension = modifierExtension
          , diagnosticReportIdentifier = identifier
          , diagnosticReportBasedOn = basedOn
          , diagnosticReportStatus =      fromDiagnosticReportStatus status
          , diagnosticReportCategory = category
          , diagnosticReportCode = code
          , diagnosticReportSubject = subject
          , diagnosticReportEncounter = encounter
          , diagnosticReportEffective = effective
          , diagnosticReportIssued = fmap fromInstant issued
          , diagnosticReportPerformer = performer
          , diagnosticReportResultsInterpreter = resultsInterpreter
          , diagnosticReportSpecimen = specimen
          , diagnosticReportResult = result
          , diagnosticReportImagingStudy = imagingStudy
          , diagnosticReportMedia = media
          , diagnosticReportConclusion = fmap fromString conclusion
          , diagnosticReportConclusionCode = conclusionCode
          , diagnosticReportPresentedForm = presentedForm
          }

    where 
      fromEffectiveXml = parseEffectiveDateTime <|> parseEffectivePeriod <|> pure Nothing
      parseEffectiveDateTime = do
                has <- Xmlbf.pElement "effectiveDateTime" (Xmlbf.pAttr "value")
                return $ Just (DiagnosticReportEffectiveDateTime (     toDateTime has))
      parseEffectivePeriod = do
                has <- Xmlbf.pElement "effectivePeriod" Xmlbf.fromXml
                return $ Just (DiagnosticReportEffectivePeriod (                      has))


data DiagnosticReportMedia = DiagnosticReportMedia {
    diagnosticReportMediaAttrId :: Maybe Text
  , diagnosticReportMediaExtension :: [Extension]
  , diagnosticReportMediaModifierExtension :: [Extension]
  , diagnosticReportMediaComment :: Maybe Text
  , diagnosticReportMediaLink :: Reference
  }
--

instance ToJSON DiagnosticReportMedia where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (diagnosticReportMediaAttrId p)
    ,  "extension" .= toJSON (diagnosticReportMediaExtension p)
    ,  "modifierExtension" .= toJSON (diagnosticReportMediaModifierExtension p)
    ,  "comment" .= toJSON (diagnosticReportMediaComment p)
    ,  "link" .= toJSON (diagnosticReportMediaLink p)
    ]
instance FromJSON DiagnosticReportMedia where
  parseJSON = withObject "DiagnosticReportMedia" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        comment <- o .:? "comment"
        link <- o .:  "link"
        return DiagnosticReportMedia{
            diagnosticReportMediaAttrId = id
          , diagnosticReportMediaExtension = extension
          , diagnosticReportMediaModifierExtension = modifierExtension
          , diagnosticReportMediaComment = comment
          , diagnosticReportMediaLink = link
          }
instance Xmlbf.ToXml DiagnosticReportMedia where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (diagnosticReportMediaAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (diagnosticReportMediaExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (diagnosticReportMediaModifierExtension p))
             , OptVal   "comment" (fmap toString (diagnosticReportMediaComment p))
             , Prop     "link" (HM.empty, Xmlbf.toXml (diagnosticReportMediaLink p))
             ]
instance Xmlbf.FromXml DiagnosticReportMedia where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    comment <- optional $ Xmlbf.pElement "comment" (Xmlbf.pAttr "value")
    link <-            Xmlbf.pElement "link" Xmlbf.fromXml
    return DiagnosticReportMedia {
            diagnosticReportMediaAttrId = id
          , diagnosticReportMediaExtension = extension
          , diagnosticReportMediaModifierExtension = modifierExtension
          , diagnosticReportMediaComment = fmap fromString comment
          , diagnosticReportMediaLink = link
          }




