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
-- FHIR 4.0.0 OperationOutcome
--

module Data.FHIR.Resources.OperationOutcome where

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

data OperationOutcome = OperationOutcome {
    operationOutcomeId :: Maybe Id
  , operationOutcomeMeta :: Maybe Meta
  , operationOutcomeImplicitRules :: Maybe Uri
  , operationOutcomeLanguage :: Maybe Language
  , operationOutcomeText :: Maybe Narrative
--    operationOutcomeContained :: [ResourceContainer]
  , operationOutcomeExtension :: [Extension]
  , operationOutcomeModifierExtension :: [Extension]
  , operationOutcomeIssue :: [OperationOutcomeIssue]
  } deriving (Eq, Show)
--

instance ToJSON OperationOutcome where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "OperationOutcome")
    ,  "id" .= toJSON (operationOutcomeId p)
    ,  "meta" .= toJSON (operationOutcomeMeta p)
    ,  "implicitRules" .= toJSON (operationOutcomeImplicitRules p)
    ,  "language" .= toJSON (operationOutcomeLanguage p)
    ,  "text" .= toJSON (operationOutcomeText p)
--    , "contained" .= toJSON (operationOutcomeContained p)
    ,  "extension" .= toJSON (operationOutcomeExtension p)
    ,  "modifierExtension" .= toJSON (operationOutcomeModifierExtension p)
    ,  "issue" .= toJSON (operationOutcomeIssue p)
    ]
instance FromJSON OperationOutcome where
  parseJSON = withObject "OperationOutcome" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "OperationOutcome" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        issue <- o .:? "issue" .!= []
        return OperationOutcome{
            operationOutcomeId = id
          , operationOutcomeMeta = meta
          , operationOutcomeImplicitRules = implicitRules
          , operationOutcomeLanguage = language
          , operationOutcomeText = text
--          , operationOutcomeContained = contained
          , operationOutcomeExtension = extension
          , operationOutcomeModifierExtension = modifierExtension
          , operationOutcomeIssue = issue
          }
      _ -> fail "not a OperationOutcome"
instance Xmlbf.ToXml OperationOutcome where
  toXml p = Xmlbf.element "OperationOutcome" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (operationOutcomeId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (operationOutcomeMeta p))
             , OptVal   "implicitRules" (fmap toUri (operationOutcomeImplicitRules p))
             , OptVal   "language" (fmap toLanguage (operationOutcomeLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (operationOutcomeText p))
--             , PropList "contained" (fmap Xmlbf.toXml (operationOutcomeContained p))
             , PropList "extension" (fmap Xmlbf.toXml (operationOutcomeExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (operationOutcomeModifierExtension p))
             , PropList "issue" (fmap Xmlbf.toXml (operationOutcomeIssue p))
             ]
instance Xmlbf.FromXml OperationOutcome where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" Xmlbf.fromXml
--    contained <- many     $ Xmlbf.pElement "contained" Xmlbf.fromXml
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    issue <- many     $ Xmlbf.pElement "issue" Xmlbf.fromXml
    return OperationOutcome {
            operationOutcomeId = fmap fromId id
          , operationOutcomeMeta = meta
          , operationOutcomeImplicitRules = fmap fromUri implicitRules
          , operationOutcomeLanguage = fmap fromLanguage language
          , operationOutcomeText = text
--          , operationOutcomeContained = contained
          , operationOutcomeExtension = extension
          , operationOutcomeModifierExtension = modifierExtension
          , operationOutcomeIssue = issue
          }



data OperationOutcomeIssueSeverity
    = OOISFatal
    | OOISError
    | OOISWarning
    | OOISInformation
  deriving (Eq, Show)

instance ToJSON OperationOutcomeIssueSeverity where
    toJSON OOISFatal = String "fatal"
    toJSON OOISError = String "error"
    toJSON OOISWarning = String "warning"
    toJSON OOISInformation = String "information"
instance FromJSON OperationOutcomeIssueSeverity where
    parseJSON "fatal" = return OOISFatal
    parseJSON "error" = return OOISError
    parseJSON "warning" = return OOISWarning
    parseJSON "information" = return OOISInformation

toOperationOutcomeIssueSeverity OOISFatal = "fatal"
toOperationOutcomeIssueSeverity OOISError = "error"
toOperationOutcomeIssueSeverity OOISWarning = "warning"
toOperationOutcomeIssueSeverity OOISInformation = "information"
fromOperationOutcomeIssueSeverity "fatal" = OOISFatal
fromOperationOutcomeIssueSeverity "error" = OOISError
fromOperationOutcomeIssueSeverity "warning" = OOISWarning
fromOperationOutcomeIssueSeverity "information" = OOISInformation


data OperationOutcomeIssueCode
    = OOICInvalid
    | OOICStructure
    | OOICRequired
    | OOICValue
    | OOICInvariant
    | OOICSecurity
    | OOICLogin
    | OOICUnknown
    | OOICExpired
    | OOICForbidden
    | OOICSuppressed
    | OOICProcessing
    | OOICNotSupported
    | OOICDuplicate
    | OOICMultipleMatches
    | OOICNotFound
    | OOICDeleted
    | OOICTooLong
    | OOICCodeInvalid
    | OOICExtension
    | OOICTooCostly
    | OOICBusinessRule
    | OOICConflict
    | OOICTransient
    | OOICLockError
    | OOICNoStore
    | OOICException
    | OOICTimeout
    | OOICIncomplete
    | OOICThrottled
    | OOICInformational
  deriving (Eq, Show)

instance ToJSON OperationOutcomeIssueCode where
    toJSON OOICInvalid = String "invalid"
    toJSON OOICStructure = String "structure"
    toJSON OOICRequired = String "required"
    toJSON OOICValue = String "value"
    toJSON OOICInvariant = String "invariant"
    toJSON OOICSecurity = String "security"
    toJSON OOICLogin = String "login"
    toJSON OOICUnknown = String "unknown"
    toJSON OOICExpired = String "expired"
    toJSON OOICForbidden = String "forbidden"
    toJSON OOICSuppressed = String "suppressed"
    toJSON OOICProcessing = String "processing"
    toJSON OOICNotSupported = String "not-supported"
    toJSON OOICDuplicate = String "duplicate"
    toJSON OOICMultipleMatches = String "multiple-matches"
    toJSON OOICNotFound = String "not-found"
    toJSON OOICDeleted = String "deleted"
    toJSON OOICTooLong = String "too-long"
    toJSON OOICCodeInvalid = String "code-invalid"
    toJSON OOICExtension = String "extension"
    toJSON OOICTooCostly = String "too-costly"
    toJSON OOICBusinessRule = String "business-rule"
    toJSON OOICConflict = String "conflict"
    toJSON OOICTransient = String "transient"
    toJSON OOICLockError = String "lock-error"
    toJSON OOICNoStore = String "no-store"
    toJSON OOICException = String "exception"
    toJSON OOICTimeout = String "timeout"
    toJSON OOICIncomplete = String "incomplete"
    toJSON OOICThrottled = String "throttled"
    toJSON OOICInformational = String "informational"
instance FromJSON OperationOutcomeIssueCode where
    parseJSON "invalid" = return OOICInvalid
    parseJSON "structure" = return OOICStructure
    parseJSON "required" = return OOICRequired
    parseJSON "value" = return OOICValue
    parseJSON "invariant" = return OOICInvariant
    parseJSON "security" = return OOICSecurity
    parseJSON "login" = return OOICLogin
    parseJSON "unknown" = return OOICUnknown
    parseJSON "expired" = return OOICExpired
    parseJSON "forbidden" = return OOICForbidden
    parseJSON "suppressed" = return OOICSuppressed
    parseJSON "processing" = return OOICProcessing
    parseJSON "not-supported" = return OOICNotSupported
    parseJSON "duplicate" = return OOICDuplicate
    parseJSON "multiple-matches" = return OOICMultipleMatches
    parseJSON "not-found" = return OOICNotFound
    parseJSON "deleted" = return OOICDeleted
    parseJSON "too-long" = return OOICTooLong
    parseJSON "code-invalid" = return OOICCodeInvalid
    parseJSON "extension" = return OOICExtension
    parseJSON "too-costly" = return OOICTooCostly
    parseJSON "business-rule" = return OOICBusinessRule
    parseJSON "conflict" = return OOICConflict
    parseJSON "transient" = return OOICTransient
    parseJSON "lock-error" = return OOICLockError
    parseJSON "no-store" = return OOICNoStore
    parseJSON "exception" = return OOICException
    parseJSON "timeout" = return OOICTimeout
    parseJSON "incomplete" = return OOICIncomplete
    parseJSON "throttled" = return OOICThrottled
    parseJSON "informational" = return OOICInformational

toOperationOutcomeIssueCode OOICInvalid = "invalid"
toOperationOutcomeIssueCode OOICStructure = "structure"
toOperationOutcomeIssueCode OOICRequired = "required"
toOperationOutcomeIssueCode OOICValue = "value"
toOperationOutcomeIssueCode OOICInvariant = "invariant"
toOperationOutcomeIssueCode OOICSecurity = "security"
toOperationOutcomeIssueCode OOICLogin = "login"
toOperationOutcomeIssueCode OOICUnknown = "unknown"
toOperationOutcomeIssueCode OOICExpired = "expired"
toOperationOutcomeIssueCode OOICForbidden = "forbidden"
toOperationOutcomeIssueCode OOICSuppressed = "suppressed"
toOperationOutcomeIssueCode OOICProcessing = "processing"
toOperationOutcomeIssueCode OOICNotSupported = "not-supported"
toOperationOutcomeIssueCode OOICDuplicate = "duplicate"
toOperationOutcomeIssueCode OOICMultipleMatches = "multiple-matches"
toOperationOutcomeIssueCode OOICNotFound = "not-found"
toOperationOutcomeIssueCode OOICDeleted = "deleted"
toOperationOutcomeIssueCode OOICTooLong = "too-long"
toOperationOutcomeIssueCode OOICCodeInvalid = "code-invalid"
toOperationOutcomeIssueCode OOICExtension = "extension"
toOperationOutcomeIssueCode OOICTooCostly = "too-costly"
toOperationOutcomeIssueCode OOICBusinessRule = "business-rule"
toOperationOutcomeIssueCode OOICConflict = "conflict"
toOperationOutcomeIssueCode OOICTransient = "transient"
toOperationOutcomeIssueCode OOICLockError = "lock-error"
toOperationOutcomeIssueCode OOICNoStore = "no-store"
toOperationOutcomeIssueCode OOICException = "exception"
toOperationOutcomeIssueCode OOICTimeout = "timeout"
toOperationOutcomeIssueCode OOICIncomplete = "incomplete"
toOperationOutcomeIssueCode OOICThrottled = "throttled"
toOperationOutcomeIssueCode OOICInformational = "informational"
fromOperationOutcomeIssueCode "invalid" = OOICInvalid
fromOperationOutcomeIssueCode "structure" = OOICStructure
fromOperationOutcomeIssueCode "required" = OOICRequired
fromOperationOutcomeIssueCode "value" = OOICValue
fromOperationOutcomeIssueCode "invariant" = OOICInvariant
fromOperationOutcomeIssueCode "security" = OOICSecurity
fromOperationOutcomeIssueCode "login" = OOICLogin
fromOperationOutcomeIssueCode "unknown" = OOICUnknown
fromOperationOutcomeIssueCode "expired" = OOICExpired
fromOperationOutcomeIssueCode "forbidden" = OOICForbidden
fromOperationOutcomeIssueCode "suppressed" = OOICSuppressed
fromOperationOutcomeIssueCode "processing" = OOICProcessing
fromOperationOutcomeIssueCode "not-supported" = OOICNotSupported
fromOperationOutcomeIssueCode "duplicate" = OOICDuplicate
fromOperationOutcomeIssueCode "multiple-matches" = OOICMultipleMatches
fromOperationOutcomeIssueCode "not-found" = OOICNotFound
fromOperationOutcomeIssueCode "deleted" = OOICDeleted
fromOperationOutcomeIssueCode "too-long" = OOICTooLong
fromOperationOutcomeIssueCode "code-invalid" = OOICCodeInvalid
fromOperationOutcomeIssueCode "extension" = OOICExtension
fromOperationOutcomeIssueCode "too-costly" = OOICTooCostly
fromOperationOutcomeIssueCode "business-rule" = OOICBusinessRule
fromOperationOutcomeIssueCode "conflict" = OOICConflict
fromOperationOutcomeIssueCode "transient" = OOICTransient
fromOperationOutcomeIssueCode "lock-error" = OOICLockError
fromOperationOutcomeIssueCode "no-store" = OOICNoStore
fromOperationOutcomeIssueCode "exception" = OOICException
fromOperationOutcomeIssueCode "timeout" = OOICTimeout
fromOperationOutcomeIssueCode "incomplete" = OOICIncomplete
fromOperationOutcomeIssueCode "throttled" = OOICThrottled
fromOperationOutcomeIssueCode "informational" = OOICInformational


data OperationOutcomeIssue = OperationOutcomeIssue {
    operationOutcomeIssueAttrId :: Maybe Text
  , operationOutcomeIssueExtension :: [Extension]
  , operationOutcomeIssueModifierExtension :: [Extension]
  , operationOutcomeIssueSeverity :: OperationOutcomeIssueSeverity
  , operationOutcomeIssueCode :: OperationOutcomeIssueCode
  , operationOutcomeIssueDetails :: Maybe CodeableConcept
  , operationOutcomeIssueDiagnostics :: Maybe Text
  , operationOutcomeIssueLocation :: [Text]
  , operationOutcomeIssueExpression :: [Text]
  } deriving (Eq, Show)
--

instance ToJSON OperationOutcomeIssue where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (operationOutcomeIssueAttrId p)
    ,  "extension" .= toJSON (operationOutcomeIssueExtension p)
    ,  "modifierExtension" .= toJSON (operationOutcomeIssueModifierExtension p)
    ,  "severity" .= toJSON (operationOutcomeIssueSeverity p)
    ,  "code" .= toJSON (operationOutcomeIssueCode p)
    ,  "details" .= toJSON (operationOutcomeIssueDetails p)
    ,  "diagnostics" .= toJSON (operationOutcomeIssueDiagnostics p)
    ,  "location" .= toJSON (operationOutcomeIssueLocation p)
    ,  "expression" .= toJSON (operationOutcomeIssueExpression p)
    ]
instance FromJSON OperationOutcomeIssue where
  parseJSON = withObject "OperationOutcomeIssue" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        severity <- o .:  "severity"
        code <- o .:  "code"
        details <- o .:? "details"
        diagnostics <- o .:? "diagnostics"
        location <- o .:? "location" .!= []
        expression <- o .:? "expression" .!= []
        return OperationOutcomeIssue{
            operationOutcomeIssueAttrId = id
          , operationOutcomeIssueExtension = extension
          , operationOutcomeIssueModifierExtension = modifierExtension
          , operationOutcomeIssueSeverity = severity
          , operationOutcomeIssueCode = code
          , operationOutcomeIssueDetails = details
          , operationOutcomeIssueDiagnostics = diagnostics
          , operationOutcomeIssueLocation = location
          , operationOutcomeIssueExpression = expression
          }
instance Xmlbf.ToXml OperationOutcomeIssue where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (operationOutcomeIssueAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (operationOutcomeIssueExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (operationOutcomeIssueModifierExtension p))
             , Val      "severity" (     toOperationOutcomeIssueSeverity (operationOutcomeIssueSeverity p))
             , Val      "code" (     toOperationOutcomeIssueCode (operationOutcomeIssueCode p))
             , OptProp  "details" (fmap Xmlbf.toXml (operationOutcomeIssueDetails p))
             , OptVal   "diagnostics" (fmap toString (operationOutcomeIssueDiagnostics p))
             , ValList  "location" (fmap toString (operationOutcomeIssueLocation p))
             , ValList  "expression" (fmap toString (operationOutcomeIssueExpression p))
             ]
instance Xmlbf.FromXml OperationOutcomeIssue where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    severity <-            Xmlbf.pElement "severity" (Xmlbf.pAttr "value")
    code <-            Xmlbf.pElement "code" (Xmlbf.pAttr "value")
    details <- optional $ Xmlbf.pElement "details" Xmlbf.fromXml
    diagnostics <- optional $ Xmlbf.pElement "diagnostics" (Xmlbf.pAttr "value")
    location <- many     $ Xmlbf.pElement "location" (Xmlbf.pAttr "value")
    expression <- many     $ Xmlbf.pElement "expression" (Xmlbf.pAttr "value")
    return OperationOutcomeIssue {
            operationOutcomeIssueAttrId = id
          , operationOutcomeIssueExtension = extension
          , operationOutcomeIssueModifierExtension = modifierExtension
          , operationOutcomeIssueSeverity =      fromOperationOutcomeIssueSeverity severity
          , operationOutcomeIssueCode =      fromOperationOutcomeIssueCode code
          , operationOutcomeIssueDetails = details
          , operationOutcomeIssueDiagnostics = fmap fromString diagnostics
          , operationOutcomeIssueLocation = fmap fromString location
          , operationOutcomeIssueExpression = fmap fromString expression
          }




