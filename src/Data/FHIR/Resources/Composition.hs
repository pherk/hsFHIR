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
-- FHIR 4.0.0 Composition
--

module Data.FHIR.Resources.Composition where

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

data CompositionStatus
    = CSPreliminary
    | CSFinal
    | CSAmended
    | CompSEnteredInError
  deriving (Eq, Show)

instance ToJSON CompositionStatus where
    toJSON CSPreliminary = String "preliminary"
    toJSON CSFinal = String "final"
    toJSON CSAmended = String "amended"
    toJSON CompSEnteredInError = String "entered-in-error"
instance FromJSON CompositionStatus where
    parseJSON "preliminary" = return CSPreliminary
    parseJSON "final" = return CSFinal
    parseJSON "amended" = return CSAmended
    parseJSON "entered-in-error" = return CompSEnteredInError

toCompositionStatus CSPreliminary = "preliminary"
toCompositionStatus CSFinal = "final"
toCompositionStatus CSAmended = "amended"
toCompositionStatus CompSEnteredInError = "entered-in-error"
fromCompositionStatus "preliminary" = CSPreliminary
fromCompositionStatus "final" = CSFinal
fromCompositionStatus "amended" = CSAmended
fromCompositionStatus "entered-in-error" = CompSEnteredInError


data CompositionConfidentiality
    = CCU
    | CCL
    | CCM
    | CCN
    | CCR
    | CCV
  deriving (Eq, Show)

instance ToJSON CompositionConfidentiality where
    toJSON CCU = String "U"
    toJSON CCL = String "L"
    toJSON CCM = String "M"
    toJSON CCN = String "N"
    toJSON CCR = String "R"
    toJSON CCV = String "V"
instance FromJSON CompositionConfidentiality where
    parseJSON "U" = return CCU
    parseJSON "L" = return CCL
    parseJSON "M" = return CCM
    parseJSON "N" = return CCN
    parseJSON "R" = return CCR
    parseJSON "V" = return CCV

toCompositionConfidentiality CCU = "U"
toCompositionConfidentiality CCL = "L"
toCompositionConfidentiality CCM = "M"
toCompositionConfidentiality CCN = "N"
toCompositionConfidentiality CCR = "R"
toCompositionConfidentiality CCV = "V"
fromCompositionConfidentiality "U" = CCU
fromCompositionConfidentiality "L" = CCL
fromCompositionConfidentiality "M" = CCM
fromCompositionConfidentiality "N" = CCN
fromCompositionConfidentiality "R" = CCR
fromCompositionConfidentiality "V" = CCV


data Composition = Composition {
    compositionId :: Maybe Id
  , compositionMeta :: Maybe Meta
  , compositionImplicitRules :: Maybe Uri
  , compositionLanguage :: Maybe Language
  , compositionText :: Maybe Narrative
--    compositionContained :: [ResourceContainer]
  , compositionExtension :: [Extension]
  , compositionModifierExtension :: [Extension]
  , compositionIdentifier :: Maybe Identifier
  , compositionStatus :: CompositionStatus
  , compositionType :: CodeableConcept
  , compositionCategory :: [CodeableConcept]
  , compositionSubject :: Maybe Reference
  , compositionEncounter :: Maybe Reference
  , compositionDate :: DateTime
  , compositionAuthor :: [Reference]
  , compositionTitle :: Text
  , compositionConfidentiality :: Maybe CompositionConfidentiality
  , compositionAttester :: [CompositionAttester]
  , compositionCustodian :: Maybe Reference
  , compositionRelatesTo :: [CompositionRelatesTo]
  , compositionEvent :: [CompositionEvent]
  , compositionSection :: [CompositionSection]
  }
  deriving (Eq, Show)
--

instance ToJSON Composition where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Composition")
    ,  "id" .= toJSON (compositionId p)
    ,  "meta" .= toJSON (compositionMeta p)
    ,  "implicitRules" .= toJSON (compositionImplicitRules p)
    ,  "language" .= toJSON (compositionLanguage p)
    ,  "text" .= toJSON (compositionText p)
--    , "contained" .= toJSON (compositionContained p)
    ,  "extension" .= toJSON (compositionExtension p)
    ,  "modifierExtension" .= toJSON (compositionModifierExtension p)
    ,  "identifier" .= toJSON (compositionIdentifier p)
    ,  "status" .= toJSON (compositionStatus p)
    ,  "type" .= toJSON (compositionType p)
    ,  "category" .= toJSON (compositionCategory p)
    ,  "subject" .= toJSON (compositionSubject p)
    ,  "encounter" .= toJSON (compositionEncounter p)
    ,  "date" .= toJSON (compositionDate p)
    ,  "author" .= toJSON (compositionAuthor p)
    ,  "title" .= toJSON (compositionTitle p)
    ,  "confidentiality" .= toJSON (compositionConfidentiality p)
    ,  "attester" .= toJSON (compositionAttester p)
    ,  "custodian" .= toJSON (compositionCustodian p)
    ,  "relatesTo" .= toJSON (compositionRelatesTo p)
    ,  "event" .= toJSON (compositionEvent p)
    ,  "section" .= toJSON (compositionSection p)
    ]
instance FromJSON Composition where
  parseJSON = withObject "Composition" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Composition" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier"
        status <- o .:  "status"
        ty <- o .:  "type"
        category <- o .:? "category" .!= []
        subject <- o .:? "subject"
        encounter <- o .:? "encounter"
        date <- o .:  "date"
        author <- o .:? "author" .!= []
        title <- o .:  "title"
        confidentiality <- o .:? "confidentiality"
        attester <- o .:? "attester" .!= []
        custodian <- o .:? "custodian"
        relatesTo <- o .:? "relatesTo" .!= []
        event <- o .:? "event" .!= []
        section <- o .:? "section" .!= []
        return Composition{
            compositionId = id
          , compositionMeta = meta
          , compositionImplicitRules = implicitRules
          , compositionLanguage = language
          , compositionText = text
--          , compositionContained = contained
          , compositionExtension = extension
          , compositionModifierExtension = modifierExtension
          , compositionIdentifier = identifier
          , compositionStatus = status
          , compositionType = ty
          , compositionCategory = category
          , compositionSubject = subject
          , compositionEncounter = encounter
          , compositionDate = date
          , compositionAuthor = author
          , compositionTitle = title
          , compositionConfidentiality = confidentiality
          , compositionAttester = attester
          , compositionCustodian = custodian
          , compositionRelatesTo = relatesTo
          , compositionEvent = event
          , compositionSection = section
          }
      _ -> fail "not a Composition"
instance Xmlbf.ToXml Composition where
  toXml p = Xmlbf.element "Composition" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (compositionId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (compositionMeta p))
             , OptVal   "implicitRules" (fmap toUri (compositionImplicitRules p))
             , OptVal   "language" (fmap toLanguage (compositionLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (compositionText p))
--             , PropList "contained" (fmap Xmlbf.toXml (compositionContained p))
             , PropList "extension" (fmap Xmlbf.toXml (compositionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (compositionModifierExtension p))
             , OptProp  "identifier" (fmap Xmlbf.toXml (compositionIdentifier p))
             , Val      "status" (     toCompositionStatus (compositionStatus p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (compositionType p))
             , PropList "category" (fmap Xmlbf.toXml (compositionCategory p))
             , OptProp  "subject" (fmap Xmlbf.toXml (compositionSubject p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (compositionEncounter p))
             , Val      "date" (     toDateTime (compositionDate p))
             , PropList "author" (fmap Xmlbf.toXml (compositionAuthor p))
             , Val      "title" (     toString (compositionTitle p))
             , OptVal   "confidentiality" (fmap toCompositionConfidentiality (compositionConfidentiality p))
             , PropList "attester" (fmap Xmlbf.toXml (compositionAttester p))
             , OptProp  "custodian" (fmap Xmlbf.toXml (compositionCustodian p))
             , PropList "relatesTo" (fmap Xmlbf.toXml (compositionRelatesTo p))
             , PropList "event" (fmap Xmlbf.toXml (compositionEvent p))
             , PropList "section" (fmap Xmlbf.toXml (compositionSection p))
             ]
instance Xmlbf.FromXml Composition where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" Xmlbf.fromXml
--    contained <- many     $ Xmlbf.pElement "contained" Xmlbf.fromXml
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    identifier <- optional $ Xmlbf.pElement "identifier" Xmlbf.fromXml
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    ty <-            Xmlbf.pElement "type" Xmlbf.fromXml
    category <- many     $ Xmlbf.pElement "category" Xmlbf.fromXml
    subject <- optional $ Xmlbf.pElement "subject" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    date <-            Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    author <- many     $ Xmlbf.pElement "author" Xmlbf.fromXml
    title <-            Xmlbf.pElement "title" (Xmlbf.pAttr "value")
    confidentiality <- optional $ Xmlbf.pElement "confidentiality" (Xmlbf.pAttr "value")
    attester <- many     $ Xmlbf.pElement "attester" Xmlbf.fromXml
    custodian <- optional $ Xmlbf.pElement "custodian" Xmlbf.fromXml
    relatesTo <- many     $ Xmlbf.pElement "relatesTo" Xmlbf.fromXml
    event <- many     $ Xmlbf.pElement "event" Xmlbf.fromXml
    section <- many     $ Xmlbf.pElement "section" Xmlbf.fromXml
    return Composition {
            compositionId = fmap fromId id
          , compositionMeta = meta
          , compositionImplicitRules = fmap fromUri implicitRules
          , compositionLanguage = fmap fromLanguage language
          , compositionText = text
--          , compositionContained = contained
          , compositionExtension = extension
          , compositionModifierExtension = modifierExtension
          , compositionIdentifier = identifier
          , compositionStatus =      fromCompositionStatus status
          , compositionType = ty
          , compositionCategory = category
          , compositionSubject = subject
          , compositionEncounter = encounter
          , compositionDate =      fromDateTime date
          , compositionAuthor = author
          , compositionTitle =      fromString title
          , compositionConfidentiality = fmap fromCompositionConfidentiality confidentiality
          , compositionAttester = attester
          , compositionCustodian = custodian
          , compositionRelatesTo = relatesTo
          , compositionEvent = event
          , compositionSection = section
          }



data CompositionSectionMode
    = CSMWorking
    | CSMSnapshot
    | CSMChanges
  deriving (Eq, Show)

instance ToJSON CompositionSectionMode where
    toJSON CSMWorking = String "working"
    toJSON CSMSnapshot = String "snapshot"
    toJSON CSMChanges = String "changes"
instance FromJSON CompositionSectionMode where
    parseJSON "working" = return CSMWorking
    parseJSON "snapshot" = return CSMSnapshot
    parseJSON "changes" = return CSMChanges

toCompositionSectionMode CSMWorking = "working"
toCompositionSectionMode CSMSnapshot = "snapshot"
toCompositionSectionMode CSMChanges = "changes"
fromCompositionSectionMode "working" = CSMWorking
fromCompositionSectionMode "snapshot" = CSMSnapshot
fromCompositionSectionMode "changes" = CSMChanges


data CompositionSection = CompositionSection {
    compositionSectionAttrId :: Maybe Text
  , compositionSectionExtension :: [Extension]
  , compositionSectionModifierExtension :: [Extension]
  , compositionSectionTitle :: Maybe Text
  , compositionSectionCode :: Maybe CodeableConcept
  , compositionSectionAuthor :: [Reference]
  , compositionSectionFocus :: Maybe Reference
  , compositionSectionText :: Maybe Narrative
  , compositionSectionMode :: Maybe CompositionSectionMode
  , compositionSectionOrderedBy :: Maybe CodeableConcept
  , compositionSectionEntry :: [Reference]
  , compositionSectionEmptyReason :: Maybe CodeableConcept
  , compositionSectionSection :: [CompositionSection]
  }
  deriving (Eq, Show)
--

instance ToJSON CompositionSection where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (compositionSectionAttrId p)
    ,  "extension" .= toJSON (compositionSectionExtension p)
    ,  "modifierExtension" .= toJSON (compositionSectionModifierExtension p)
    ,  "title" .= toJSON (compositionSectionTitle p)
    ,  "code" .= toJSON (compositionSectionCode p)
    ,  "author" .= toJSON (compositionSectionAuthor p)
    ,  "focus" .= toJSON (compositionSectionFocus p)
    ,  "text" .= toJSON (compositionSectionText p)
    ,  "mode" .= toJSON (compositionSectionMode p)
    ,  "orderedBy" .= toJSON (compositionSectionOrderedBy p)
    ,  "entry" .= toJSON (compositionSectionEntry p)
    ,  "emptyReason" .= toJSON (compositionSectionEmptyReason p)
    ,  "section" .= toJSON (compositionSectionSection p)
    ]
instance FromJSON CompositionSection where
  parseJSON = withObject "CompositionSection" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        title <- o .:? "title"
        code <- o .:? "code"
        author <- o .:? "author" .!= []
        focus <- o .:? "focus"
        text <- o .:? "text"
        mode <- o .:? "mode"
        orderedBy <- o .:? "orderedBy"
        entry <- o .:? "entry" .!= []
        emptyReason <- o .:? "emptyReason"
        section <- o .:? "section" .!= []
        return CompositionSection{
            compositionSectionAttrId = id
          , compositionSectionExtension = extension
          , compositionSectionModifierExtension = modifierExtension
          , compositionSectionTitle = title
          , compositionSectionCode = code
          , compositionSectionAuthor = author
          , compositionSectionFocus = focus
          , compositionSectionText = text
          , compositionSectionMode = mode
          , compositionSectionOrderedBy = orderedBy
          , compositionSectionEntry = entry
          , compositionSectionEmptyReason = emptyReason
          , compositionSectionSection = section
          }
instance Xmlbf.ToXml CompositionSection where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (compositionSectionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (compositionSectionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (compositionSectionModifierExtension p))
             , OptVal   "title" (fmap toString (compositionSectionTitle p))
             , OptProp  "code" (fmap Xmlbf.toXml (compositionSectionCode p))
             , PropList "author" (fmap Xmlbf.toXml (compositionSectionAuthor p))
             , OptProp  "focus" (fmap Xmlbf.toXml (compositionSectionFocus p))
             , OptProp  "text" (fmap Xmlbf.toXml (compositionSectionText p))
             , OptVal   "mode" (fmap toCompositionSectionMode (compositionSectionMode p))
             , OptProp  "orderedBy" (fmap Xmlbf.toXml (compositionSectionOrderedBy p))
             , PropList "entry" (fmap Xmlbf.toXml (compositionSectionEntry p))
             , OptProp  "emptyReason" (fmap Xmlbf.toXml (compositionSectionEmptyReason p))
             , PropList "section" (fmap Xmlbf.toXml (compositionSectionSection p))
             ]
instance Xmlbf.FromXml CompositionSection where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    title <- optional $ Xmlbf.pElement "title" (Xmlbf.pAttr "value")
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    author <- many     $ Xmlbf.pElement "author" Xmlbf.fromXml
    focus <- optional $ Xmlbf.pElement "focus" Xmlbf.fromXml
    text <- optional $ Xmlbf.pElement "text" Xmlbf.fromXml
    mode <- optional $ Xmlbf.pElement "mode" (Xmlbf.pAttr "value")
    orderedBy <- optional $ Xmlbf.pElement "orderedBy" Xmlbf.fromXml
    entry <- many     $ Xmlbf.pElement "entry" Xmlbf.fromXml
    emptyReason <- optional $ Xmlbf.pElement "emptyReason" Xmlbf.fromXml
    section <- many     $ Xmlbf.pElement "section" Xmlbf.fromXml
    return CompositionSection {
            compositionSectionAttrId = id
          , compositionSectionExtension = extension
          , compositionSectionModifierExtension = modifierExtension
          , compositionSectionTitle = fmap fromString title
          , compositionSectionCode = code
          , compositionSectionAuthor = author
          , compositionSectionFocus = focus
          , compositionSectionText = text
          , compositionSectionMode = fmap fromCompositionSectionMode mode
          , compositionSectionOrderedBy = orderedBy
          , compositionSectionEntry = entry
          , compositionSectionEmptyReason = emptyReason
          , compositionSectionSection = section
          }



data CompositionRelatesToCode
    = CRTCReplaces
    | CRTCTransforms
    | CRTCSigns
    | CRTCAppends
  deriving (Eq, Show)

instance ToJSON CompositionRelatesToCode where
    toJSON CRTCReplaces = String "replaces"
    toJSON CRTCTransforms = String "transforms"
    toJSON CRTCSigns = String "signs"
    toJSON CRTCAppends = String "appends"
instance FromJSON CompositionRelatesToCode where
    parseJSON "replaces" = return CRTCReplaces
    parseJSON "transforms" = return CRTCTransforms
    parseJSON "signs" = return CRTCSigns
    parseJSON "appends" = return CRTCAppends

toCompositionRelatesToCode CRTCReplaces = "replaces"
toCompositionRelatesToCode CRTCTransforms = "transforms"
toCompositionRelatesToCode CRTCSigns = "signs"
toCompositionRelatesToCode CRTCAppends = "appends"
fromCompositionRelatesToCode "replaces" = CRTCReplaces
fromCompositionRelatesToCode "transforms" = CRTCTransforms
fromCompositionRelatesToCode "signs" = CRTCSigns
fromCompositionRelatesToCode "appends" = CRTCAppends


data CompositionRelatesToTarget
    = CompositionRelatesToTargetIdentifier Identifier
    | CompositionRelatesToTargetReference Reference
    deriving (Eq, Show)

data CompositionRelatesTo = CompositionRelatesTo {
    compositionRelatesToAttrId :: Maybe Text
  , compositionRelatesToExtension :: [Extension]
  , compositionRelatesToModifierExtension :: [Extension]
  , compositionRelatesToCode :: CompositionRelatesToCode
  , compositionRelatesToTargetIdentifier :: Identifier
  , compositionRelatesToTargetReference :: Reference
  }
  deriving (Eq, Show)
--

instance ToJSON CompositionRelatesTo where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (compositionRelatesToAttrId p)
    ,  "extension" .= toJSON (compositionRelatesToExtension p)
    ,  "modifierExtension" .= toJSON (compositionRelatesToModifierExtension p)
    ,  "code" .= toJSON (compositionRelatesToCode p)
    ,  "targetIdentifier" .= toJSON (compositionRelatesToTargetIdentifier p)
    ,  "targetReference" .= toJSON (compositionRelatesToTargetReference p)
    ]
instance FromJSON CompositionRelatesTo where
  parseJSON = withObject "CompositionRelatesTo" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:  "code"
        targetIdentifier <- o .:  "targetIdentifier"
        targetReference <- o .:  "targetReference"
        return CompositionRelatesTo{
            compositionRelatesToAttrId = id
          , compositionRelatesToExtension = extension
          , compositionRelatesToModifierExtension = modifierExtension
          , compositionRelatesToCode = code
          , compositionRelatesToTargetIdentifier = targetIdentifier
          , compositionRelatesToTargetReference = targetReference
          }
instance Xmlbf.ToXml CompositionRelatesTo where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (compositionRelatesToAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (compositionRelatesToExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (compositionRelatesToModifierExtension p))
             , Val      "code" (     toCompositionRelatesToCode (compositionRelatesToCode p))
             , Prop     "targetIdentifier" (HM.empty, Xmlbf.toXml (compositionRelatesToTargetIdentifier p))
             , Prop     "targetReference" (HM.empty, Xmlbf.toXml (compositionRelatesToTargetReference p))
             ]
instance Xmlbf.FromXml CompositionRelatesTo where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" (Xmlbf.pAttr "value")
    targetIdentifier <-            Xmlbf.pElement "targetIdentifier" Xmlbf.fromXml
    targetReference <-            Xmlbf.pElement "targetReference" Xmlbf.fromXml
    return CompositionRelatesTo {
            compositionRelatesToAttrId = id
          , compositionRelatesToExtension = extension
          , compositionRelatesToModifierExtension = modifierExtension
          , compositionRelatesToCode =      fromCompositionRelatesToCode code
          , compositionRelatesToTargetIdentifier = targetIdentifier
          , compositionRelatesToTargetReference = targetReference
          }



data CompositionAttesterMode
    = CAMPersonal
    | CAMProfessional
    | CAMLegal
    | CAMOfficial
  deriving (Eq, Show)

instance ToJSON CompositionAttesterMode where
    toJSON CAMPersonal = String "personal"
    toJSON CAMProfessional = String "professional"
    toJSON CAMLegal = String "legal"
    toJSON CAMOfficial = String "official"
instance FromJSON CompositionAttesterMode where
    parseJSON "personal" = return CAMPersonal
    parseJSON "professional" = return CAMProfessional
    parseJSON "legal" = return CAMLegal
    parseJSON "official" = return CAMOfficial

toCompositionAttesterMode CAMPersonal = "personal"
toCompositionAttesterMode CAMProfessional = "professional"
toCompositionAttesterMode CAMLegal = "legal"
toCompositionAttesterMode CAMOfficial = "official"
fromCompositionAttesterMode "personal" = CAMPersonal
fromCompositionAttesterMode "professional" = CAMProfessional
fromCompositionAttesterMode "legal" = CAMLegal
fromCompositionAttesterMode "official" = CAMOfficial


data CompositionAttester = CompositionAttester {
    compositionAttesterAttrId :: Maybe Text
  , compositionAttesterExtension :: [Extension]
  , compositionAttesterModifierExtension :: [Extension]
  , compositionAttesterMode :: CompositionAttesterMode
  , compositionAttesterTime :: Maybe DateTime
  , compositionAttesterParty :: Maybe Reference
  }
  deriving (Eq, Show)
--

instance ToJSON CompositionAttester where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (compositionAttesterAttrId p)
    ,  "extension" .= toJSON (compositionAttesterExtension p)
    ,  "modifierExtension" .= toJSON (compositionAttesterModifierExtension p)
    ,  "mode" .= toJSON (compositionAttesterMode p)
    ,  "time" .= toJSON (compositionAttesterTime p)
    ,  "party" .= toJSON (compositionAttesterParty p)
    ]
instance FromJSON CompositionAttester where
  parseJSON = withObject "CompositionAttester" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        mode <- o .:  "mode"
        time <- o .:? "time"
        party <- o .:? "party"
        return CompositionAttester{
            compositionAttesterAttrId = id
          , compositionAttesterExtension = extension
          , compositionAttesterModifierExtension = modifierExtension
          , compositionAttesterMode = mode
          , compositionAttesterTime = time
          , compositionAttesterParty = party
          }
instance Xmlbf.ToXml CompositionAttester where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (compositionAttesterAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (compositionAttesterExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (compositionAttesterModifierExtension p))
             , Val      "mode" (     toCompositionAttesterMode (compositionAttesterMode p))
             , OptVal   "time" (fmap toDateTime (compositionAttesterTime p))
             , OptProp  "party" (fmap Xmlbf.toXml (compositionAttesterParty p))
             ]
instance Xmlbf.FromXml CompositionAttester where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    mode <-            Xmlbf.pElement "mode" (Xmlbf.pAttr "value")
    time <- optional $ Xmlbf.pElement "time" (Xmlbf.pAttr "value")
    party <- optional $ Xmlbf.pElement "party" Xmlbf.fromXml
    return CompositionAttester {
            compositionAttesterAttrId = id
          , compositionAttesterExtension = extension
          , compositionAttesterModifierExtension = modifierExtension
          , compositionAttesterMode =      fromCompositionAttesterMode mode
          , compositionAttesterTime = fmap fromDateTime time
          , compositionAttesterParty = party
          }



data CompositionEvent = CompositionEvent {
    compositionEventAttrId :: Maybe Text
  , compositionEventExtension :: [Extension]
  , compositionEventModifierExtension :: [Extension]
  , compositionEventCode :: [CodeableConcept]
  , compositionEventPeriod :: Maybe Period
  , compositionEventDetail :: [Reference]
  }
  deriving (Eq, Show)
--

instance ToJSON CompositionEvent where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (compositionEventAttrId p)
    ,  "extension" .= toJSON (compositionEventExtension p)
    ,  "modifierExtension" .= toJSON (compositionEventModifierExtension p)
    ,  "code" .= toJSON (compositionEventCode p)
    ,  "period" .= toJSON (compositionEventPeriod p)
    ,  "detail" .= toJSON (compositionEventDetail p)
    ]
instance FromJSON CompositionEvent where
  parseJSON = withObject "CompositionEvent" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:? "code" .!= []
        period <- o .:? "period"
        detail <- o .:? "detail" .!= []
        return CompositionEvent{
            compositionEventAttrId = id
          , compositionEventExtension = extension
          , compositionEventModifierExtension = modifierExtension
          , compositionEventCode = code
          , compositionEventPeriod = period
          , compositionEventDetail = detail
          }
instance Xmlbf.ToXml CompositionEvent where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (compositionEventAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (compositionEventExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (compositionEventModifierExtension p))
             , PropList "code" (fmap Xmlbf.toXml (compositionEventCode p))
             , OptProp  "period" (fmap Xmlbf.toXml (compositionEventPeriod p))
             , PropList "detail" (fmap Xmlbf.toXml (compositionEventDetail p))
             ]
instance Xmlbf.FromXml CompositionEvent where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <- many     $ Xmlbf.pElement "code" Xmlbf.fromXml
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    detail <- many     $ Xmlbf.pElement "detail" Xmlbf.fromXml
    return CompositionEvent {
            compositionEventAttrId = id
          , compositionEventExtension = extension
          , compositionEventModifierExtension = modifierExtension
          , compositionEventCode = code
          , compositionEventPeriod = period
          , compositionEventDetail = detail
          }




