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
-- FHIR 4.0.0 ExplanationOfBenefit
--

module Data.FHIR.Resources.ExplanationOfBenefit where

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

data ExplanationOfBenefitStatus
    = EOBSActive
    | EOBSCancelled
    | EOBSDraft
    | EOBSEnteredInError
  deriving (Eq, Show)

instance ToJSON ExplanationOfBenefitStatus where
    toJSON EOBSActive = String "active"
    toJSON EOBSCancelled = String "cancelled"
    toJSON EOBSDraft = String "draft"
    toJSON EOBSEnteredInError = String "entered-in-error"
instance FromJSON ExplanationOfBenefitStatus where
    parseJSON "active" = return EOBSActive
    parseJSON "cancelled" = return EOBSCancelled
    parseJSON "draft" = return EOBSDraft
    parseJSON "entered-in-error" = return EOBSEnteredInError

toExplanationOfBenefitStatus EOBSActive = "active"
toExplanationOfBenefitStatus EOBSCancelled = "cancelled"
toExplanationOfBenefitStatus EOBSDraft = "draft"
toExplanationOfBenefitStatus EOBSEnteredInError = "entered-in-error"
fromExplanationOfBenefitStatus "active" = EOBSActive
fromExplanationOfBenefitStatus "cancelled" = EOBSCancelled
fromExplanationOfBenefitStatus "draft" = EOBSDraft
fromExplanationOfBenefitStatus "entered-in-error" = EOBSEnteredInError


data ExplanationOfBenefitUse
    = EOBUClaim
    | EOBUPreauthorization
    | EOBUPredetermination
  deriving (Eq, Show)

instance ToJSON ExplanationOfBenefitUse where
    toJSON EOBUClaim = String "claim"
    toJSON EOBUPreauthorization = String "preauthorization"
    toJSON EOBUPredetermination = String "predetermination"
instance FromJSON ExplanationOfBenefitUse where
    parseJSON "claim" = return EOBUClaim
    parseJSON "preauthorization" = return EOBUPreauthorization
    parseJSON "predetermination" = return EOBUPredetermination

toExplanationOfBenefitUse EOBUClaim = "claim"
toExplanationOfBenefitUse EOBUPreauthorization = "preauthorization"
toExplanationOfBenefitUse EOBUPredetermination = "predetermination"
fromExplanationOfBenefitUse "claim" = EOBUClaim
fromExplanationOfBenefitUse "preauthorization" = EOBUPreauthorization
fromExplanationOfBenefitUse "predetermination" = EOBUPredetermination


data ExplanationOfBenefitOutcome
    = EOBOQueued
    | EOBOComplete
    | EOBOError
    | EOBOPartial
  deriving (Eq, Show)

instance ToJSON ExplanationOfBenefitOutcome where
    toJSON EOBOQueued = String "queued"
    toJSON EOBOComplete = String "complete"
    toJSON EOBOError = String "error"
    toJSON EOBOPartial = String "partial"
instance FromJSON ExplanationOfBenefitOutcome where
    parseJSON "queued" = return EOBOQueued
    parseJSON "complete" = return EOBOComplete
    parseJSON "error" = return EOBOError
    parseJSON "partial" = return EOBOPartial

toExplanationOfBenefitOutcome EOBOQueued = "queued"
toExplanationOfBenefitOutcome EOBOComplete = "complete"
toExplanationOfBenefitOutcome EOBOError = "error"
toExplanationOfBenefitOutcome EOBOPartial = "partial"
fromExplanationOfBenefitOutcome "queued" = EOBOQueued
fromExplanationOfBenefitOutcome "complete" = EOBOComplete
fromExplanationOfBenefitOutcome "error" = EOBOError
fromExplanationOfBenefitOutcome "partial" = EOBOPartial


data ExplanationOfBenefit = ExplanationOfBenefit {
    explanationOfBenefitId :: Maybe Id
  , explanationOfBenefitMeta :: Maybe Meta
  , explanationOfBenefitImplicitRules :: Maybe Uri
  , explanationOfBenefitLanguage :: Maybe Language
  , explanationOfBenefitText :: Maybe Narrative
--    explanationOfBenefitContained :: [ResourceContainer]
  , explanationOfBenefitExtension :: [Extension]
  , explanationOfBenefitModifierExtension :: [Extension]
  , explanationOfBenefitIdentifier :: [Identifier]
  , explanationOfBenefitStatus :: ExplanationOfBenefitStatus
  , explanationOfBenefitType :: CodeableConcept
  , explanationOfBenefitSubType :: Maybe CodeableConcept
  , explanationOfBenefitUse :: ExplanationOfBenefitUse
  , explanationOfBenefitPatient :: Reference
  , explanationOfBenefitBillablePeriod :: Maybe Period
  , explanationOfBenefitCreated :: DateTime
  , explanationOfBenefitEnterer :: Maybe Reference
  , explanationOfBenefitInsurer :: Reference
  , explanationOfBenefitProvider :: Reference
  , explanationOfBenefitPriority :: Maybe CodeableConcept
  , explanationOfBenefitFundsReserveRequested :: Maybe CodeableConcept
  , explanationOfBenefitFundsReserve :: Maybe CodeableConcept
  , explanationOfBenefitRelated :: [ExplanationOfBenefitRelated]
  , explanationOfBenefitPrescription :: Maybe Reference
  , explanationOfBenefitOriginalPrescription :: Maybe Reference
  , explanationOfBenefitPayee :: Maybe ExplanationOfBenefitPayee
  , explanationOfBenefitReferral :: Maybe Reference
  , explanationOfBenefitFacility :: Maybe Reference
  , explanationOfBenefitClaim :: Maybe Reference
  , explanationOfBenefitClaimResponse :: Maybe Reference
  , explanationOfBenefitOutcome :: ExplanationOfBenefitOutcome
  , explanationOfBenefitDisposition :: Maybe Text
  , explanationOfBenefitPreAuthRef :: [Text]
  , explanationOfBenefitPreAuthRefPeriod :: [Period]
  , explanationOfBenefitCareTeam :: [ExplanationOfBenefitCareTeam]
  , explanationOfBenefitSupportingInfo :: [ExplanationOfBenefitSupportingInfo]
  , explanationOfBenefitDiagnosis :: [ExplanationOfBenefitDiagnosis]
  , explanationOfBenefitProcedure :: [ExplanationOfBenefitProcedure]
  , explanationOfBenefitPrecedence :: Maybe PositiveInt
  , explanationOfBenefitInsurance :: [ExplanationOfBenefitInsurance]
  , explanationOfBenefitAccident :: Maybe ExplanationOfBenefitAccident
  , explanationOfBenefitItem :: [ExplanationOfBenefitItem]
  , explanationOfBenefitAddItem :: [ExplanationOfBenefitAddItem]
  , explanationOfBenefitAdjudication :: [ExplanationOfBenefitAdjudication]
  , explanationOfBenefitTotal :: [ExplanationOfBenefitTotal]
  , explanationOfBenefitPayment :: Maybe ExplanationOfBenefitPayment
  , explanationOfBenefitFormCode :: Maybe CodeableConcept
  , explanationOfBenefitForm :: Maybe Attachment
  , explanationOfBenefitProcessNote :: [ExplanationOfBenefitProcessNote]
  , explanationOfBenefitBenefitPeriod :: Maybe Period
  , explanationOfBenefitBenefitBalance :: [ExplanationOfBenefitBenefitBalance]
  }
--

instance ToJSON ExplanationOfBenefit where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "ExplanationOfBenefit")
    ,  "id" .= toJSON (explanationOfBenefitId p)
    ,  "meta" .= toJSON (explanationOfBenefitMeta p)
    ,  "implicitRules" .= toJSON (explanationOfBenefitImplicitRules p)
    ,  "language" .= toJSON (explanationOfBenefitLanguage p)
    ,  "text" .= toJSON (explanationOfBenefitText p)
--    , "contained" .= toJSON (explanationOfBenefitContained p)
    ,  "extension" .= toJSON (explanationOfBenefitExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitModifierExtension p)
    ,  "identifier" .= toJSON (explanationOfBenefitIdentifier p)
    ,  "status" .= toJSON (explanationOfBenefitStatus p)
    ,  "type" .= toJSON (explanationOfBenefitType p)
    ,  "subType" .= toJSON (explanationOfBenefitSubType p)
    ,  "use" .= toJSON (explanationOfBenefitUse p)
    ,  "patient" .= toJSON (explanationOfBenefitPatient p)
    ,  "billablePeriod" .= toJSON (explanationOfBenefitBillablePeriod p)
    ,  "created" .= toJSON (explanationOfBenefitCreated p)
    ,  "enterer" .= toJSON (explanationOfBenefitEnterer p)
    ,  "insurer" .= toJSON (explanationOfBenefitInsurer p)
    ,  "provider" .= toJSON (explanationOfBenefitProvider p)
    ,  "priority" .= toJSON (explanationOfBenefitPriority p)
    ,  "fundsReserveRequested" .= toJSON (explanationOfBenefitFundsReserveRequested p)
    ,  "fundsReserve" .= toJSON (explanationOfBenefitFundsReserve p)
    ,  "related" .= toJSON (explanationOfBenefitRelated p)
    ,  "prescription" .= toJSON (explanationOfBenefitPrescription p)
    ,  "originalPrescription" .= toJSON (explanationOfBenefitOriginalPrescription p)
    ,  "payee" .= toJSON (explanationOfBenefitPayee p)
    ,  "referral" .= toJSON (explanationOfBenefitReferral p)
    ,  "facility" .= toJSON (explanationOfBenefitFacility p)
    ,  "claim" .= toJSON (explanationOfBenefitClaim p)
    ,  "claimResponse" .= toJSON (explanationOfBenefitClaimResponse p)
    ,  "outcome" .= toJSON (explanationOfBenefitOutcome p)
    ,  "disposition" .= toJSON (explanationOfBenefitDisposition p)
    ,  "preAuthRef" .= toJSON (explanationOfBenefitPreAuthRef p)
    ,  "preAuthRefPeriod" .= toJSON (explanationOfBenefitPreAuthRefPeriod p)
    ,  "careTeam" .= toJSON (explanationOfBenefitCareTeam p)
    ,  "supportingInfo" .= toJSON (explanationOfBenefitSupportingInfo p)
    ,  "diagnosis" .= toJSON (explanationOfBenefitDiagnosis p)
    ,  "procedure" .= toJSON (explanationOfBenefitProcedure p)
    ,  "precedence" .= toJSON (explanationOfBenefitPrecedence p)
    ,  "insurance" .= toJSON (explanationOfBenefitInsurance p)
    ,  "accident" .= toJSON (explanationOfBenefitAccident p)
    ,  "item" .= toJSON (explanationOfBenefitItem p)
    ,  "addItem" .= toJSON (explanationOfBenefitAddItem p)
    ,  "adjudication" .= toJSON (explanationOfBenefitAdjudication p)
    ,  "total" .= toJSON (explanationOfBenefitTotal p)
    ,  "payment" .= toJSON (explanationOfBenefitPayment p)
    ,  "formCode" .= toJSON (explanationOfBenefitFormCode p)
    ,  "form" .= toJSON (explanationOfBenefitForm p)
    ,  "processNote" .= toJSON (explanationOfBenefitProcessNote p)
    ,  "benefitPeriod" .= toJSON (explanationOfBenefitBenefitPeriod p)
    ,  "benefitBalance" .= toJSON (explanationOfBenefitBenefitBalance p)
    ]
instance FromJSON ExplanationOfBenefit where
  parseJSON = withObject "ExplanationOfBenefit" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "ExplanationOfBenefit" -> do
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
        ty <- o .:  "type"
        subType <- o .:? "subType"
        use <- o .:  "use"
        patient <- o .:  "patient"
        billablePeriod <- o .:? "billablePeriod"
        created <- o .:  "created"
        enterer <- o .:? "enterer"
        insurer <- o .:  "insurer"
        provider <- o .:  "provider"
        priority <- o .:? "priority"
        fundsReserveRequested <- o .:? "fundsReserveRequested"
        fundsReserve <- o .:? "fundsReserve"
        related <- o .:? "related" .!= []
        prescription <- o .:? "prescription"
        originalPrescription <- o .:? "originalPrescription"
        payee <- o .:? "payee"
        referral <- o .:? "referral"
        facility <- o .:? "facility"
        claim <- o .:? "claim"
        claimResponse <- o .:? "claimResponse"
        outcome <- o .:  "outcome"
        disposition <- o .:? "disposition"
        preAuthRef <- o .:? "preAuthRef" .!= []
        preAuthRefPeriod <- o .:? "preAuthRefPeriod" .!= []
        careTeam <- o .:? "careTeam" .!= []
        supportingInfo <- o .:? "supportingInfo" .!= []
        diagnosis <- o .:? "diagnosis" .!= []
        procedure <- o .:? "procedure" .!= []
        precedence <- o .:? "precedence"
        insurance <- o .:? "insurance" .!= []
        accident <- o .:? "accident"
        item <- o .:? "item" .!= []
        addItem <- o .:? "addItem" .!= []
        adjudication <- o .:? "adjudication" .!= []
        total <- o .:? "total" .!= []
        payment <- o .:? "payment"
        formCode <- o .:? "formCode"
        form <- o .:? "form"
        processNote <- o .:? "processNote" .!= []
        benefitPeriod <- o .:? "benefitPeriod"
        benefitBalance <- o .:? "benefitBalance" .!= []
        return ExplanationOfBenefit{
            explanationOfBenefitId = id
          , explanationOfBenefitMeta = meta
          , explanationOfBenefitImplicitRules = implicitRules
          , explanationOfBenefitLanguage = language
          , explanationOfBenefitText = text
--          , explanationOfBenefitContained = contained
          , explanationOfBenefitExtension = extension
          , explanationOfBenefitModifierExtension = modifierExtension
          , explanationOfBenefitIdentifier = identifier
          , explanationOfBenefitStatus = status
          , explanationOfBenefitType = ty
          , explanationOfBenefitSubType = subType
          , explanationOfBenefitUse = use
          , explanationOfBenefitPatient = patient
          , explanationOfBenefitBillablePeriod = billablePeriod
          , explanationOfBenefitCreated = created
          , explanationOfBenefitEnterer = enterer
          , explanationOfBenefitInsurer = insurer
          , explanationOfBenefitProvider = provider
          , explanationOfBenefitPriority = priority
          , explanationOfBenefitFundsReserveRequested = fundsReserveRequested
          , explanationOfBenefitFundsReserve = fundsReserve
          , explanationOfBenefitRelated = related
          , explanationOfBenefitPrescription = prescription
          , explanationOfBenefitOriginalPrescription = originalPrescription
          , explanationOfBenefitPayee = payee
          , explanationOfBenefitReferral = referral
          , explanationOfBenefitFacility = facility
          , explanationOfBenefitClaim = claim
          , explanationOfBenefitClaimResponse = claimResponse
          , explanationOfBenefitOutcome = outcome
          , explanationOfBenefitDisposition = disposition
          , explanationOfBenefitPreAuthRef = preAuthRef
          , explanationOfBenefitPreAuthRefPeriod = preAuthRefPeriod
          , explanationOfBenefitCareTeam = careTeam
          , explanationOfBenefitSupportingInfo = supportingInfo
          , explanationOfBenefitDiagnosis = diagnosis
          , explanationOfBenefitProcedure = procedure
          , explanationOfBenefitPrecedence = precedence
          , explanationOfBenefitInsurance = insurance
          , explanationOfBenefitAccident = accident
          , explanationOfBenefitItem = item
          , explanationOfBenefitAddItem = addItem
          , explanationOfBenefitAdjudication = adjudication
          , explanationOfBenefitTotal = total
          , explanationOfBenefitPayment = payment
          , explanationOfBenefitFormCode = formCode
          , explanationOfBenefitForm = form
          , explanationOfBenefitProcessNote = processNote
          , explanationOfBenefitBenefitPeriod = benefitPeriod
          , explanationOfBenefitBenefitBalance = benefitBalance
          }
      _ -> fail "not a ExplanationOfBenefit"
instance Xmlbf.ToXml ExplanationOfBenefit where
  toXml p = Xmlbf.element "ExplanationOfBenefit" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (explanationOfBenefitId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (explanationOfBenefitMeta p))
             , OptVal   "implicitRules" (fmap toUri (explanationOfBenefitImplicitRules p))
             , OptVal   "language" (fmap toLanguage (explanationOfBenefitLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (explanationOfBenefitText p))
--             , PropList "contained" (fmap Xmlbf.toXml (explanationOfBenefitContained p))
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (explanationOfBenefitIdentifier p))
             , Val      "status" (     toExplanationOfBenefitStatus (explanationOfBenefitStatus p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (explanationOfBenefitType p))
             , OptProp  "subType" (fmap Xmlbf.toXml (explanationOfBenefitSubType p))
             , Val      "use" (     toExplanationOfBenefitUse (explanationOfBenefitUse p))
             , Prop     "patient" (HM.empty, Xmlbf.toXml (explanationOfBenefitPatient p))
             , OptProp  "billablePeriod" (fmap Xmlbf.toXml (explanationOfBenefitBillablePeriod p))
             , Val      "created" (     toDateTime (explanationOfBenefitCreated p))
             , OptProp  "enterer" (fmap Xmlbf.toXml (explanationOfBenefitEnterer p))
             , Prop     "insurer" (HM.empty, Xmlbf.toXml (explanationOfBenefitInsurer p))
             , Prop     "provider" (HM.empty, Xmlbf.toXml (explanationOfBenefitProvider p))
             , OptProp  "priority" (fmap Xmlbf.toXml (explanationOfBenefitPriority p))
             , OptProp  "fundsReserveRequested" (fmap Xmlbf.toXml (explanationOfBenefitFundsReserveRequested p))
             , OptProp  "fundsReserve" (fmap Xmlbf.toXml (explanationOfBenefitFundsReserve p))
             , PropList "related" (fmap Xmlbf.toXml (explanationOfBenefitRelated p))
             , OptProp  "prescription" (fmap Xmlbf.toXml (explanationOfBenefitPrescription p))
             , OptProp  "originalPrescription" (fmap Xmlbf.toXml (explanationOfBenefitOriginalPrescription p))
             , OptProp  "payee" (fmap Xmlbf.toXml (explanationOfBenefitPayee p))
             , OptProp  "referral" (fmap Xmlbf.toXml (explanationOfBenefitReferral p))
             , OptProp  "facility" (fmap Xmlbf.toXml (explanationOfBenefitFacility p))
             , OptProp  "claim" (fmap Xmlbf.toXml (explanationOfBenefitClaim p))
             , OptProp  "claimResponse" (fmap Xmlbf.toXml (explanationOfBenefitClaimResponse p))
             , Val      "outcome" (     toExplanationOfBenefitOutcome (explanationOfBenefitOutcome p))
             , OptVal   "disposition" (fmap toString (explanationOfBenefitDisposition p))
             , ValList  "preAuthRef" (fmap toString (explanationOfBenefitPreAuthRef p))
             , PropList "preAuthRefPeriod" (fmap Xmlbf.toXml (explanationOfBenefitPreAuthRefPeriod p))
             , PropList "careTeam" (fmap Xmlbf.toXml (explanationOfBenefitCareTeam p))
             , PropList "supportingInfo" (fmap Xmlbf.toXml (explanationOfBenefitSupportingInfo p))
             , PropList "diagnosis" (fmap Xmlbf.toXml (explanationOfBenefitDiagnosis p))
             , PropList "procedure" (fmap Xmlbf.toXml (explanationOfBenefitProcedure p))
             , OptVal   "precedence" (fmap toPositiveInt (explanationOfBenefitPrecedence p))
             , PropList "insurance" (fmap Xmlbf.toXml (explanationOfBenefitInsurance p))
             , OptProp  "accident" (fmap Xmlbf.toXml (explanationOfBenefitAccident p))
             , PropList "item" (fmap Xmlbf.toXml (explanationOfBenefitItem p))
             , PropList "addItem" (fmap Xmlbf.toXml (explanationOfBenefitAddItem p))
             , PropList "adjudication" (fmap Xmlbf.toXml (explanationOfBenefitAdjudication p))
             , PropList "total" (fmap Xmlbf.toXml (explanationOfBenefitTotal p))
             , OptProp  "payment" (fmap Xmlbf.toXml (explanationOfBenefitPayment p))
             , OptProp  "formCode" (fmap Xmlbf.toXml (explanationOfBenefitFormCode p))
             , OptProp  "form" (fmap Xmlbf.toXml (explanationOfBenefitForm p))
             , PropList "processNote" (fmap Xmlbf.toXml (explanationOfBenefitProcessNote p))
             , OptProp  "benefitPeriod" (fmap Xmlbf.toXml (explanationOfBenefitBenefitPeriod p))
             , PropList "benefitBalance" (fmap Xmlbf.toXml (explanationOfBenefitBenefitBalance p))
             ]
instance Xmlbf.FromXml ExplanationOfBenefit where
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
    ty <-            Xmlbf.pElement "type" Xmlbf.fromXml
    subType <- optional $ Xmlbf.pElement "subType" Xmlbf.fromXml
    use <-            Xmlbf.pElement "use" (Xmlbf.pAttr "value")
    patient <-            Xmlbf.pElement "patient" Xmlbf.fromXml
    billablePeriod <- optional $ Xmlbf.pElement "billablePeriod" Xmlbf.fromXml
    created <-            Xmlbf.pElement "created" (Xmlbf.pAttr "value")
    enterer <- optional $ Xmlbf.pElement "enterer" Xmlbf.fromXml
    insurer <-            Xmlbf.pElement "insurer" Xmlbf.fromXml
    provider <-            Xmlbf.pElement "provider" Xmlbf.fromXml
    priority <- optional $ Xmlbf.pElement "priority" Xmlbf.fromXml
    fundsReserveRequested <- optional $ Xmlbf.pElement "fundsReserveRequested" Xmlbf.fromXml
    fundsReserve <- optional $ Xmlbf.pElement "fundsReserve" Xmlbf.fromXml
    related <- many     $ Xmlbf.pElement "related" Xmlbf.fromXml
    prescription <- optional $ Xmlbf.pElement "prescription" Xmlbf.fromXml
    originalPrescription <- optional $ Xmlbf.pElement "originalPrescription" Xmlbf.fromXml
    payee <- optional $ Xmlbf.pElement "payee" Xmlbf.fromXml
    referral <- optional $ Xmlbf.pElement "referral" Xmlbf.fromXml
    facility <- optional $ Xmlbf.pElement "facility" Xmlbf.fromXml
    claim <- optional $ Xmlbf.pElement "claim" Xmlbf.fromXml
    claimResponse <- optional $ Xmlbf.pElement "claimResponse" Xmlbf.fromXml
    outcome <-            Xmlbf.pElement "outcome" (Xmlbf.pAttr "value")
    disposition <- optional $ Xmlbf.pElement "disposition" (Xmlbf.pAttr "value")
    preAuthRef <- many     $ Xmlbf.pElement "preAuthRef" (Xmlbf.pAttr "value")
    preAuthRefPeriod <- many     $ Xmlbf.pElement "preAuthRefPeriod" Xmlbf.fromXml
    careTeam <- many     $ Xmlbf.pElement "careTeam" Xmlbf.fromXml
    supportingInfo <- many     $ Xmlbf.pElement "supportingInfo" Xmlbf.fromXml
    diagnosis <- many     $ Xmlbf.pElement "diagnosis" Xmlbf.fromXml
    procedure <- many     $ Xmlbf.pElement "procedure" Xmlbf.fromXml
    precedence <- optional $ Xmlbf.pElement "precedence" (Xmlbf.pAttr "value")
    insurance <- many     $ Xmlbf.pElement "insurance" Xmlbf.fromXml
    accident <- optional $ Xmlbf.pElement "accident" Xmlbf.fromXml
    item <- many     $ Xmlbf.pElement "item" Xmlbf.fromXml
    addItem <- many     $ Xmlbf.pElement "addItem" Xmlbf.fromXml
    adjudication <- many     $ Xmlbf.pElement "adjudication" Xmlbf.fromXml
    total <- many     $ Xmlbf.pElement "total" Xmlbf.fromXml
    payment <- optional $ Xmlbf.pElement "payment" Xmlbf.fromXml
    formCode <- optional $ Xmlbf.pElement "formCode" Xmlbf.fromXml
    form <- optional $ Xmlbf.pElement "form" Xmlbf.fromXml
    processNote <- many     $ Xmlbf.pElement "processNote" Xmlbf.fromXml
    benefitPeriod <- optional $ Xmlbf.pElement "benefitPeriod" Xmlbf.fromXml
    benefitBalance <- many     $ Xmlbf.pElement "benefitBalance" Xmlbf.fromXml
    return ExplanationOfBenefit {
            explanationOfBenefitId = fmap fromId id
          , explanationOfBenefitMeta = meta
          , explanationOfBenefitImplicitRules = fmap fromUri implicitRules
          , explanationOfBenefitLanguage = fmap fromLanguage language
          , explanationOfBenefitText = text
--          , explanationOfBenefitContained = contained
          , explanationOfBenefitExtension = extension
          , explanationOfBenefitModifierExtension = modifierExtension
          , explanationOfBenefitIdentifier = identifier
          , explanationOfBenefitStatus =      fromExplanationOfBenefitStatus status
          , explanationOfBenefitType = ty
          , explanationOfBenefitSubType = subType
          , explanationOfBenefitUse =      fromExplanationOfBenefitUse use
          , explanationOfBenefitPatient = patient
          , explanationOfBenefitBillablePeriod = billablePeriod
          , explanationOfBenefitCreated =      fromDateTime created
          , explanationOfBenefitEnterer = enterer
          , explanationOfBenefitInsurer = insurer
          , explanationOfBenefitProvider = provider
          , explanationOfBenefitPriority = priority
          , explanationOfBenefitFundsReserveRequested = fundsReserveRequested
          , explanationOfBenefitFundsReserve = fundsReserve
          , explanationOfBenefitRelated = related
          , explanationOfBenefitPrescription = prescription
          , explanationOfBenefitOriginalPrescription = originalPrescription
          , explanationOfBenefitPayee = payee
          , explanationOfBenefitReferral = referral
          , explanationOfBenefitFacility = facility
          , explanationOfBenefitClaim = claim
          , explanationOfBenefitClaimResponse = claimResponse
          , explanationOfBenefitOutcome =      fromExplanationOfBenefitOutcome outcome
          , explanationOfBenefitDisposition = fmap fromString disposition
          , explanationOfBenefitPreAuthRef = fmap fromString preAuthRef
          , explanationOfBenefitPreAuthRefPeriod = preAuthRefPeriod
          , explanationOfBenefitCareTeam = careTeam
          , explanationOfBenefitSupportingInfo = supportingInfo
          , explanationOfBenefitDiagnosis = diagnosis
          , explanationOfBenefitProcedure = procedure
          , explanationOfBenefitPrecedence = fmap fromPositiveInt precedence
          , explanationOfBenefitInsurance = insurance
          , explanationOfBenefitAccident = accident
          , explanationOfBenefitItem = item
          , explanationOfBenefitAddItem = addItem
          , explanationOfBenefitAdjudication = adjudication
          , explanationOfBenefitTotal = total
          , explanationOfBenefitPayment = payment
          , explanationOfBenefitFormCode = formCode
          , explanationOfBenefitForm = form
          , explanationOfBenefitProcessNote = processNote
          , explanationOfBenefitBenefitPeriod = benefitPeriod
          , explanationOfBenefitBenefitBalance = benefitBalance
          }



data ExplanationOfBenefitInsurance = ExplanationOfBenefitInsurance {
    explanationOfBenefitInsuranceAttrId :: Maybe Text
  , explanationOfBenefitInsuranceExtension :: [Extension]
  , explanationOfBenefitInsuranceModifierExtension :: [Extension]
  , explanationOfBenefitInsuranceFocal :: Boolean
  , explanationOfBenefitInsuranceCoverage :: Reference
  , explanationOfBenefitInsurancePreAuthRef :: [Text]
  }
--

instance ToJSON ExplanationOfBenefitInsurance where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitInsuranceAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitInsuranceExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitInsuranceModifierExtension p)
    ,  "focal" .= toJSON (explanationOfBenefitInsuranceFocal p)
    ,  "coverage" .= toJSON (explanationOfBenefitInsuranceCoverage p)
    ,  "preAuthRef" .= toJSON (explanationOfBenefitInsurancePreAuthRef p)
    ]
instance FromJSON ExplanationOfBenefitInsurance where
  parseJSON = withObject "ExplanationOfBenefitInsurance" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        focal <- o .:  "focal"
        coverage <- o .:  "coverage"
        preAuthRef <- o .:? "preAuthRef" .!= []
        return ExplanationOfBenefitInsurance{
            explanationOfBenefitInsuranceAttrId = id
          , explanationOfBenefitInsuranceExtension = extension
          , explanationOfBenefitInsuranceModifierExtension = modifierExtension
          , explanationOfBenefitInsuranceFocal = focal
          , explanationOfBenefitInsuranceCoverage = coverage
          , explanationOfBenefitInsurancePreAuthRef = preAuthRef
          }
instance Xmlbf.ToXml ExplanationOfBenefitInsurance where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitInsuranceAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitInsuranceExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitInsuranceModifierExtension p))
             , Val      "focal" (     toBoolean (explanationOfBenefitInsuranceFocal p))
             , Prop     "coverage" (HM.empty, Xmlbf.toXml (explanationOfBenefitInsuranceCoverage p))
             , ValList  "preAuthRef" (fmap toString (explanationOfBenefitInsurancePreAuthRef p))
             ]
instance Xmlbf.FromXml ExplanationOfBenefitInsurance where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    focal <-            Xmlbf.pElement "focal" (Xmlbf.pAttr "value")
    coverage <-            Xmlbf.pElement "coverage" Xmlbf.fromXml
    preAuthRef <- many     $ Xmlbf.pElement "preAuthRef" (Xmlbf.pAttr "value")
    return ExplanationOfBenefitInsurance {
            explanationOfBenefitInsuranceAttrId = id
          , explanationOfBenefitInsuranceExtension = extension
          , explanationOfBenefitInsuranceModifierExtension = modifierExtension
          , explanationOfBenefitInsuranceFocal =      fromBoolean focal
          , explanationOfBenefitInsuranceCoverage = coverage
          , explanationOfBenefitInsurancePreAuthRef = fmap fromString preAuthRef
          }



data ExplanationOfBenefitAddItemServiced
    = ExplanationOfBenefitAddItemServicedDate Date
    | ExplanationOfBenefitAddItemServicedPeriod Period
    deriving (Eq, Show)

data ExplanationOfBenefitAddItemLocation
    = ExplanationOfBenefitAddItemLocationCodeableConcept CodeableConcept
    | ExplanationOfBenefitAddItemLocationAddress Address
    | ExplanationOfBenefitAddItemLocationReference Reference
    deriving (Eq, Show)

data ExplanationOfBenefitAddItem = ExplanationOfBenefitAddItem {
    explanationOfBenefitAddItemAttrId :: Maybe Text
  , explanationOfBenefitAddItemExtension :: [Extension]
  , explanationOfBenefitAddItemModifierExtension :: [Extension]
  , explanationOfBenefitAddItemItemSequence :: [PositiveInt]
  , explanationOfBenefitAddItemDetailSequence :: [PositiveInt]
  , explanationOfBenefitAddItemSubDetailSequence :: [PositiveInt]
  , explanationOfBenefitAddItemProvider :: [Reference]
  , explanationOfBenefitAddItemProductOrService :: CodeableConcept
  , explanationOfBenefitAddItemModifier :: [CodeableConcept]
  , explanationOfBenefitAddItemProgramCode :: [CodeableConcept]
  , explanationOfBenefitAddItemServiced :: Maybe ExplanationOfBenefitAddItemServiced
  , explanationOfBenefitAddItemLocation :: Maybe ExplanationOfBenefitAddItemLocation
  , explanationOfBenefitAddItemQuantity :: Maybe Quantity
  , explanationOfBenefitAddItemUnitPrice :: Maybe Money
  , explanationOfBenefitAddItemFactor :: Maybe Decimal
  , explanationOfBenefitAddItemNet :: Maybe Money
  , explanationOfBenefitAddItemBodySite :: Maybe CodeableConcept
  , explanationOfBenefitAddItemSubSite :: [CodeableConcept]
  , explanationOfBenefitAddItemNoteNumber :: [PositiveInt]
  , explanationOfBenefitAddItemAdjudication :: [ExplanationOfBenefitAdjudication]
  , explanationOfBenefitAddItemDetail :: [ExplanationOfBenefitDetail1]
  }
--

instance ToJSON ExplanationOfBenefitAddItem where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitAddItemAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitAddItemExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitAddItemModifierExtension p)
    ,  "itemSequence" .= toJSON (explanationOfBenefitAddItemItemSequence p)
    ,  "detailSequence" .= toJSON (explanationOfBenefitAddItemDetailSequence p)
    ,  "subDetailSequence" .= toJSON (explanationOfBenefitAddItemSubDetailSequence p)
    ,  "provider" .= toJSON (explanationOfBenefitAddItemProvider p)
    ,  "productOrService" .= toJSON (explanationOfBenefitAddItemProductOrService p)
    ,  "modifier" .= toJSON (explanationOfBenefitAddItemModifier p)
    ,  "programCode" .= toJSON (explanationOfBenefitAddItemProgramCode p)
    , toServicedJSON (explanationOfBenefitAddItemServiced p)
    , toLocationJSON (explanationOfBenefitAddItemLocation p)
    ,  "quantity" .= toJSON (explanationOfBenefitAddItemQuantity p)
    ,  "unitPrice" .= toJSON (explanationOfBenefitAddItemUnitPrice p)
    ,  "factor" .= toJSON (explanationOfBenefitAddItemFactor p)
    ,  "net" .= toJSON (explanationOfBenefitAddItemNet p)
    ,  "bodySite" .= toJSON (explanationOfBenefitAddItemBodySite p)
    ,  "subSite" .= toJSON (explanationOfBenefitAddItemSubSite p)
    ,  "noteNumber" .= toJSON (explanationOfBenefitAddItemNoteNumber p)
    ,  "adjudication" .= toJSON (explanationOfBenefitAddItemAdjudication p)
    ,  "detail" .= toJSON (explanationOfBenefitAddItemDetail p)
    ]
    where 
      toServicedJSON (     Nothing   ) = ("serviced", Null)
      toServicedJSON (Just (ExplanationOfBenefitAddItemServicedDate c)) = ("serviced", toJSON c)
      toServicedJSON (Just (ExplanationOfBenefitAddItemServicedPeriod c)) = ("serviced", toJSON c)
      toLocationJSON (     Nothing   ) = ("location", Null)
      toLocationJSON (Just (ExplanationOfBenefitAddItemLocationCodeableConcept c)) = ("location", toJSON c)
      toLocationJSON (Just (ExplanationOfBenefitAddItemLocationAddress c)) = ("location", toJSON c)
      toLocationJSON (Just (ExplanationOfBenefitAddItemLocationReference c)) = ("location", toJSON c)
instance FromJSON ExplanationOfBenefitAddItem where
  parseJSON = withObject "ExplanationOfBenefitAddItem" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        itemSequence <- o .:? "itemSequence" .!= []
        detailSequence <- o .:? "detailSequence" .!= []
        subDetailSequence <- o .:? "subDetailSequence" .!= []
        provider <- o .:? "provider" .!= []
        productOrService <- o .:  "productOrService"
        modifier <- o .:? "modifier" .!= []
        programCode <- o .:? "programCode" .!= []
        serviced <- parseServiced o
        location <- parseLocation o
        quantity <- o .:? "quantity"
        unitPrice <- o .:? "unitPrice"
        factor <- o .:? "factor"
        net <- o .:? "net"
        bodySite <- o .:? "bodySite"
        subSite <- o .:? "subSite" .!= []
        noteNumber <- o .:? "noteNumber" .!= []
        adjudication <- o .:? "adjudication" .!= []
        detail <- o .:? "detail" .!= []
        return ExplanationOfBenefitAddItem{
            explanationOfBenefitAddItemAttrId = id
          , explanationOfBenefitAddItemExtension = extension
          , explanationOfBenefitAddItemModifierExtension = modifierExtension
          , explanationOfBenefitAddItemItemSequence = itemSequence
          , explanationOfBenefitAddItemDetailSequence = detailSequence
          , explanationOfBenefitAddItemSubDetailSequence = subDetailSequence
          , explanationOfBenefitAddItemProvider = provider
          , explanationOfBenefitAddItemProductOrService = productOrService
          , explanationOfBenefitAddItemModifier = modifier
          , explanationOfBenefitAddItemProgramCode = programCode
          , explanationOfBenefitAddItemServiced = serviced
          , explanationOfBenefitAddItemLocation = location
          , explanationOfBenefitAddItemQuantity = quantity
          , explanationOfBenefitAddItemUnitPrice = unitPrice
          , explanationOfBenefitAddItemFactor = factor
          , explanationOfBenefitAddItemNet = net
          , explanationOfBenefitAddItemBodySite = bodySite
          , explanationOfBenefitAddItemSubSite = subSite
          , explanationOfBenefitAddItemNoteNumber = noteNumber
          , explanationOfBenefitAddItemAdjudication = adjudication
          , explanationOfBenefitAddItemDetail = detail
          }
    where 
      parseServiced o = parseServicedDate o <|> parseServicedPeriod o
      parseServicedDate o = do
                has <- o .: "servicedDate"
                return $ Just (ExplanationOfBenefitAddItemServicedDate has)
      parseServicedPeriod o = do
                has <- o .: "servicedPeriod"
                return $ Just (ExplanationOfBenefitAddItemServicedPeriod has)
      parseLocation o = parseLocationCodeableConcept o <|> parseLocationAddress o <|> parseLocationReference o
      parseLocationCodeableConcept o = do
                has <- o .: "locationCodeableConcept"
                return $ Just (ExplanationOfBenefitAddItemLocationCodeableConcept has)
      parseLocationAddress o = do
                has <- o .: "locationAddress"
                return $ Just (ExplanationOfBenefitAddItemLocationAddress has)
      parseLocationReference o = do
                has <- o .: "locationReference"
                return $ Just (ExplanationOfBenefitAddItemLocationReference has)
instance Xmlbf.ToXml ExplanationOfBenefitAddItem where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitAddItemAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitAddItemExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitAddItemModifierExtension p))
             , ValList  "itemSequence" (fmap toPositiveInt (explanationOfBenefitAddItemItemSequence p))
             , ValList  "detailSequence" (fmap toPositiveInt (explanationOfBenefitAddItemDetailSequence p))
             , ValList  "subDetailSequence" (fmap toPositiveInt (explanationOfBenefitAddItemSubDetailSequence p))
             , PropList "provider" (fmap Xmlbf.toXml (explanationOfBenefitAddItemProvider p))
             , Prop     "productOrService" (HM.empty, Xmlbf.toXml (explanationOfBenefitAddItemProductOrService p))
             , PropList "modifier" (fmap Xmlbf.toXml (explanationOfBenefitAddItemModifier p))
             , PropList "programCode" (fmap Xmlbf.toXml (explanationOfBenefitAddItemProgramCode p))
             , toServicedXml (explanationOfBenefitAddItemServiced p)
             , toLocationXml (explanationOfBenefitAddItemLocation p)
             , OptProp  "quantity" (fmap Xmlbf.toXml (explanationOfBenefitAddItemQuantity p))
             , OptProp  "unitPrice" (fmap Xmlbf.toXml (explanationOfBenefitAddItemUnitPrice p))
             , OptVal   "factor" (fmap toDecimal (explanationOfBenefitAddItemFactor p))
             , OptProp  "net" (fmap Xmlbf.toXml (explanationOfBenefitAddItemNet p))
             , OptProp  "bodySite" (fmap Xmlbf.toXml (explanationOfBenefitAddItemBodySite p))
             , PropList "subSite" (fmap Xmlbf.toXml (explanationOfBenefitAddItemSubSite p))
             , ValList  "noteNumber" (fmap toPositiveInt (explanationOfBenefitAddItemNoteNumber p))
             , PropList "adjudication" (fmap Xmlbf.toXml (explanationOfBenefitAddItemAdjudication p))
             , PropList "detail" (fmap Xmlbf.toXml (explanationOfBenefitAddItemDetail p))
             ]
       where 
          toServicedXml ( Nothing   ) = (OptVal "serviced" Nothing)
          toServicedXml (Just (ExplanationOfBenefitAddItemServicedDate p)) = Val   "servicedDate" (toDate p)
          toServicedXml (Just (ExplanationOfBenefitAddItemServicedPeriod p)) = Prop  "servicedPeriod" (HM.empty, Xmlbf.toXml p)
          toLocationXml ( Nothing   ) = (OptVal "location" Nothing)
          toLocationXml (Just (ExplanationOfBenefitAddItemLocationCodeableConcept p)) = Prop  "locationCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toLocationXml (Just (ExplanationOfBenefitAddItemLocationAddress p)) = Prop  "locationAddress" (HM.empty, Xmlbf.toXml p)
          toLocationXml (Just (ExplanationOfBenefitAddItemLocationReference p)) = Prop  "locationReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml ExplanationOfBenefitAddItem where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    itemSequence <- many     $ Xmlbf.pElement "itemSequence" (Xmlbf.pAttr "value")
    detailSequence <- many     $ Xmlbf.pElement "detailSequence" (Xmlbf.pAttr "value")
    subDetailSequence <- many     $ Xmlbf.pElement "subDetailSequence" (Xmlbf.pAttr "value")
    provider <- many     $ Xmlbf.pElement "provider" Xmlbf.fromXml
    productOrService <-            Xmlbf.pElement "productOrService" Xmlbf.fromXml
    modifier <- many     $ Xmlbf.pElement "modifier" Xmlbf.fromXml
    programCode <- many     $ Xmlbf.pElement "programCode" Xmlbf.fromXml
    serviced <- fromServicedXml
    location <- fromLocationXml
    quantity <- optional $ Xmlbf.pElement "quantity" Xmlbf.fromXml
    unitPrice <- optional $ Xmlbf.pElement "unitPrice" Xmlbf.fromXml
    factor <- optional $ Xmlbf.pElement "factor" (Xmlbf.pAttr "value")
    net <- optional $ Xmlbf.pElement "net" Xmlbf.fromXml
    bodySite <- optional $ Xmlbf.pElement "bodySite" Xmlbf.fromXml
    subSite <- many     $ Xmlbf.pElement "subSite" Xmlbf.fromXml
    noteNumber <- many     $ Xmlbf.pElement "noteNumber" (Xmlbf.pAttr "value")
    adjudication <- many     $ Xmlbf.pElement "adjudication" Xmlbf.fromXml
    detail <- many     $ Xmlbf.pElement "detail" Xmlbf.fromXml
    return ExplanationOfBenefitAddItem {
            explanationOfBenefitAddItemAttrId = id
          , explanationOfBenefitAddItemExtension = extension
          , explanationOfBenefitAddItemModifierExtension = modifierExtension
          , explanationOfBenefitAddItemItemSequence = fmap fromPositiveInt itemSequence
          , explanationOfBenefitAddItemDetailSequence = fmap fromPositiveInt detailSequence
          , explanationOfBenefitAddItemSubDetailSequence = fmap fromPositiveInt subDetailSequence
          , explanationOfBenefitAddItemProvider = provider
          , explanationOfBenefitAddItemProductOrService = productOrService
          , explanationOfBenefitAddItemModifier = modifier
          , explanationOfBenefitAddItemProgramCode = programCode
          , explanationOfBenefitAddItemServiced = serviced
          , explanationOfBenefitAddItemLocation = location
          , explanationOfBenefitAddItemQuantity = quantity
          , explanationOfBenefitAddItemUnitPrice = unitPrice
          , explanationOfBenefitAddItemFactor = fmap fromDecimal factor
          , explanationOfBenefitAddItemNet = net
          , explanationOfBenefitAddItemBodySite = bodySite
          , explanationOfBenefitAddItemSubSite = subSite
          , explanationOfBenefitAddItemNoteNumber = fmap fromPositiveInt noteNumber
          , explanationOfBenefitAddItemAdjudication = adjudication
          , explanationOfBenefitAddItemDetail = detail
          }

    where 
      fromServicedXml = parseServicedDate <|> parseServicedPeriod <|> pure Nothing
      parseServicedDate = do
                has <- Xmlbf.pElement "servicedDate" (Xmlbf.pAttr "value")
                return $ Just (ExplanationOfBenefitAddItemServicedDate (     toDate has))
      parseServicedPeriod = do
                has <- Xmlbf.pElement "servicedPeriod" Xmlbf.fromXml
                return $ Just (ExplanationOfBenefitAddItemServicedPeriod (                      has))
      fromLocationXml = parseLocationCodeableConcept <|> parseLocationAddress <|> parseLocationReference <|> pure Nothing
      parseLocationCodeableConcept = do
                has <- Xmlbf.pElement "locationCodeableConcept" Xmlbf.fromXml
                return $ Just (ExplanationOfBenefitAddItemLocationCodeableConcept (                      has))
      parseLocationAddress = do
                has <- Xmlbf.pElement "locationAddress" Xmlbf.fromXml
                return $ Just (ExplanationOfBenefitAddItemLocationAddress (                      has))
      parseLocationReference = do
                has <- Xmlbf.pElement "locationReference" Xmlbf.fromXml
                return $ Just (ExplanationOfBenefitAddItemLocationReference (                      has))


data ExplanationOfBenefitDetail = ExplanationOfBenefitDetail {
    explanationOfBenefitDetailAttrId :: Maybe Text
  , explanationOfBenefitDetailExtension :: [Extension]
  , explanationOfBenefitDetailModifierExtension :: [Extension]
  , explanationOfBenefitDetailSequence :: PositiveInt
  , explanationOfBenefitDetailRevenue :: Maybe CodeableConcept
  , explanationOfBenefitDetailCategory :: Maybe CodeableConcept
  , explanationOfBenefitDetailProductOrService :: CodeableConcept
  , explanationOfBenefitDetailModifier :: [CodeableConcept]
  , explanationOfBenefitDetailProgramCode :: [CodeableConcept]
  , explanationOfBenefitDetailQuantity :: Maybe Quantity
  , explanationOfBenefitDetailUnitPrice :: Maybe Money
  , explanationOfBenefitDetailFactor :: Maybe Decimal
  , explanationOfBenefitDetailNet :: Maybe Money
  , explanationOfBenefitDetailUdi :: [Reference]
  , explanationOfBenefitDetailNoteNumber :: [PositiveInt]
  , explanationOfBenefitDetailAdjudication :: [ExplanationOfBenefitAdjudication]
  , explanationOfBenefitDetailSubDetail :: [ExplanationOfBenefitSubDetail]
  }
--

instance ToJSON ExplanationOfBenefitDetail where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitDetailAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitDetailExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitDetailModifierExtension p)
    ,  "sequence" .= toJSON (explanationOfBenefitDetailSequence p)
    ,  "revenue" .= toJSON (explanationOfBenefitDetailRevenue p)
    ,  "category" .= toJSON (explanationOfBenefitDetailCategory p)
    ,  "productOrService" .= toJSON (explanationOfBenefitDetailProductOrService p)
    ,  "modifier" .= toJSON (explanationOfBenefitDetailModifier p)
    ,  "programCode" .= toJSON (explanationOfBenefitDetailProgramCode p)
    ,  "quantity" .= toJSON (explanationOfBenefitDetailQuantity p)
    ,  "unitPrice" .= toJSON (explanationOfBenefitDetailUnitPrice p)
    ,  "factor" .= toJSON (explanationOfBenefitDetailFactor p)
    ,  "net" .= toJSON (explanationOfBenefitDetailNet p)
    ,  "udi" .= toJSON (explanationOfBenefitDetailUdi p)
    ,  "noteNumber" .= toJSON (explanationOfBenefitDetailNoteNumber p)
    ,  "adjudication" .= toJSON (explanationOfBenefitDetailAdjudication p)
    ,  "subDetail" .= toJSON (explanationOfBenefitDetailSubDetail p)
    ]
instance FromJSON ExplanationOfBenefitDetail where
  parseJSON = withObject "ExplanationOfBenefitDetail" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        sequence <- o .:  "sequence"
        revenue <- o .:? "revenue"
        category <- o .:? "category"
        productOrService <- o .:  "productOrService"
        modifier <- o .:? "modifier" .!= []
        programCode <- o .:? "programCode" .!= []
        quantity <- o .:? "quantity"
        unitPrice <- o .:? "unitPrice"
        factor <- o .:? "factor"
        net <- o .:? "net"
        udi <- o .:? "udi" .!= []
        noteNumber <- o .:? "noteNumber" .!= []
        adjudication <- o .:? "adjudication" .!= []
        subDetail <- o .:? "subDetail" .!= []
        return ExplanationOfBenefitDetail{
            explanationOfBenefitDetailAttrId = id
          , explanationOfBenefitDetailExtension = extension
          , explanationOfBenefitDetailModifierExtension = modifierExtension
          , explanationOfBenefitDetailSequence = sequence
          , explanationOfBenefitDetailRevenue = revenue
          , explanationOfBenefitDetailCategory = category
          , explanationOfBenefitDetailProductOrService = productOrService
          , explanationOfBenefitDetailModifier = modifier
          , explanationOfBenefitDetailProgramCode = programCode
          , explanationOfBenefitDetailQuantity = quantity
          , explanationOfBenefitDetailUnitPrice = unitPrice
          , explanationOfBenefitDetailFactor = factor
          , explanationOfBenefitDetailNet = net
          , explanationOfBenefitDetailUdi = udi
          , explanationOfBenefitDetailNoteNumber = noteNumber
          , explanationOfBenefitDetailAdjudication = adjudication
          , explanationOfBenefitDetailSubDetail = subDetail
          }
instance Xmlbf.ToXml ExplanationOfBenefitDetail where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitDetailAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitDetailExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitDetailModifierExtension p))
             , Val      "sequence" (     toPositiveInt (explanationOfBenefitDetailSequence p))
             , OptProp  "revenue" (fmap Xmlbf.toXml (explanationOfBenefitDetailRevenue p))
             , OptProp  "category" (fmap Xmlbf.toXml (explanationOfBenefitDetailCategory p))
             , Prop     "productOrService" (HM.empty, Xmlbf.toXml (explanationOfBenefitDetailProductOrService p))
             , PropList "modifier" (fmap Xmlbf.toXml (explanationOfBenefitDetailModifier p))
             , PropList "programCode" (fmap Xmlbf.toXml (explanationOfBenefitDetailProgramCode p))
             , OptProp  "quantity" (fmap Xmlbf.toXml (explanationOfBenefitDetailQuantity p))
             , OptProp  "unitPrice" (fmap Xmlbf.toXml (explanationOfBenefitDetailUnitPrice p))
             , OptVal   "factor" (fmap toDecimal (explanationOfBenefitDetailFactor p))
             , OptProp  "net" (fmap Xmlbf.toXml (explanationOfBenefitDetailNet p))
             , PropList "udi" (fmap Xmlbf.toXml (explanationOfBenefitDetailUdi p))
             , ValList  "noteNumber" (fmap toPositiveInt (explanationOfBenefitDetailNoteNumber p))
             , PropList "adjudication" (fmap Xmlbf.toXml (explanationOfBenefitDetailAdjudication p))
             , PropList "subDetail" (fmap Xmlbf.toXml (explanationOfBenefitDetailSubDetail p))
             ]
instance Xmlbf.FromXml ExplanationOfBenefitDetail where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    sequence <-            Xmlbf.pElement "sequence" (Xmlbf.pAttr "value")
    revenue <- optional $ Xmlbf.pElement "revenue" Xmlbf.fromXml
    category <- optional $ Xmlbf.pElement "category" Xmlbf.fromXml
    productOrService <-            Xmlbf.pElement "productOrService" Xmlbf.fromXml
    modifier <- many     $ Xmlbf.pElement "modifier" Xmlbf.fromXml
    programCode <- many     $ Xmlbf.pElement "programCode" Xmlbf.fromXml
    quantity <- optional $ Xmlbf.pElement "quantity" Xmlbf.fromXml
    unitPrice <- optional $ Xmlbf.pElement "unitPrice" Xmlbf.fromXml
    factor <- optional $ Xmlbf.pElement "factor" (Xmlbf.pAttr "value")
    net <- optional $ Xmlbf.pElement "net" Xmlbf.fromXml
    udi <- many     $ Xmlbf.pElement "udi" Xmlbf.fromXml
    noteNumber <- many     $ Xmlbf.pElement "noteNumber" (Xmlbf.pAttr "value")
    adjudication <- many     $ Xmlbf.pElement "adjudication" Xmlbf.fromXml
    subDetail <- many     $ Xmlbf.pElement "subDetail" Xmlbf.fromXml
    return ExplanationOfBenefitDetail {
            explanationOfBenefitDetailAttrId = id
          , explanationOfBenefitDetailExtension = extension
          , explanationOfBenefitDetailModifierExtension = modifierExtension
          , explanationOfBenefitDetailSequence =      fromPositiveInt sequence
          , explanationOfBenefitDetailRevenue = revenue
          , explanationOfBenefitDetailCategory = category
          , explanationOfBenefitDetailProductOrService = productOrService
          , explanationOfBenefitDetailModifier = modifier
          , explanationOfBenefitDetailProgramCode = programCode
          , explanationOfBenefitDetailQuantity = quantity
          , explanationOfBenefitDetailUnitPrice = unitPrice
          , explanationOfBenefitDetailFactor = fmap fromDecimal factor
          , explanationOfBenefitDetailNet = net
          , explanationOfBenefitDetailUdi = udi
          , explanationOfBenefitDetailNoteNumber = fmap fromPositiveInt noteNumber
          , explanationOfBenefitDetailAdjudication = adjudication
          , explanationOfBenefitDetailSubDetail = subDetail
          }



data ExplanationOfBenefitAccidentLocation
    = ExplanationOfBenefitAccidentLocationAddress Address
    | ExplanationOfBenefitAccidentLocationReference Reference
    deriving (Eq, Show)

data ExplanationOfBenefitAccident = ExplanationOfBenefitAccident {
    explanationOfBenefitAccidentAttrId :: Maybe Text
  , explanationOfBenefitAccidentExtension :: [Extension]
  , explanationOfBenefitAccidentModifierExtension :: [Extension]
  , explanationOfBenefitAccidentDate :: Maybe Date
  , explanationOfBenefitAccidentType :: Maybe CodeableConcept
  , explanationOfBenefitAccidentLocation :: Maybe ExplanationOfBenefitAccidentLocation
  }
--

instance ToJSON ExplanationOfBenefitAccident where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitAccidentAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitAccidentExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitAccidentModifierExtension p)
    ,  "date" .= toJSON (explanationOfBenefitAccidentDate p)
    ,  "type" .= toJSON (explanationOfBenefitAccidentType p)
    , toLocationJSON (explanationOfBenefitAccidentLocation p)
    ]
    where 
      toLocationJSON (     Nothing   ) = ("location", Null)
      toLocationJSON (Just (ExplanationOfBenefitAccidentLocationAddress c)) = ("location", toJSON c)
      toLocationJSON (Just (ExplanationOfBenefitAccidentLocationReference c)) = ("location", toJSON c)
instance FromJSON ExplanationOfBenefitAccident where
  parseJSON = withObject "ExplanationOfBenefitAccident" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        date <- o .:? "date"
        ty <- o .:? "type"
        location <- parseLocation o
        return ExplanationOfBenefitAccident{
            explanationOfBenefitAccidentAttrId = id
          , explanationOfBenefitAccidentExtension = extension
          , explanationOfBenefitAccidentModifierExtension = modifierExtension
          , explanationOfBenefitAccidentDate = date
          , explanationOfBenefitAccidentType = ty
          , explanationOfBenefitAccidentLocation = location
          }
    where 
      parseLocation o = parseLocationAddress o <|> parseLocationReference o
      parseLocationAddress o = do
                has <- o .: "locationAddress"
                return $ Just (ExplanationOfBenefitAccidentLocationAddress has)
      parseLocationReference o = do
                has <- o .: "locationReference"
                return $ Just (ExplanationOfBenefitAccidentLocationReference has)
instance Xmlbf.ToXml ExplanationOfBenefitAccident where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitAccidentAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitAccidentExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitAccidentModifierExtension p))
             , OptVal   "date" (fmap toDate (explanationOfBenefitAccidentDate p))
             , OptProp  "type" (fmap Xmlbf.toXml (explanationOfBenefitAccidentType p))
             , toLocationXml (explanationOfBenefitAccidentLocation p)
             ]
       where 
          toLocationXml ( Nothing   ) = (OptVal "location" Nothing)
          toLocationXml (Just (ExplanationOfBenefitAccidentLocationAddress p)) = Prop  "locationAddress" (HM.empty, Xmlbf.toXml p)
          toLocationXml (Just (ExplanationOfBenefitAccidentLocationReference p)) = Prop  "locationReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml ExplanationOfBenefitAccident where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    date <- optional $ Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    ty <- optional $ Xmlbf.pElement "type" Xmlbf.fromXml
    location <- fromLocationXml
    return ExplanationOfBenefitAccident {
            explanationOfBenefitAccidentAttrId = id
          , explanationOfBenefitAccidentExtension = extension
          , explanationOfBenefitAccidentModifierExtension = modifierExtension
          , explanationOfBenefitAccidentDate = fmap fromDate date
          , explanationOfBenefitAccidentType = ty
          , explanationOfBenefitAccidentLocation = location
          }

    where 
      fromLocationXml = parseLocationAddress <|> parseLocationReference <|> pure Nothing
      parseLocationAddress = do
                has <- Xmlbf.pElement "locationAddress" Xmlbf.fromXml
                return $ Just (ExplanationOfBenefitAccidentLocationAddress (                      has))
      parseLocationReference = do
                has <- Xmlbf.pElement "locationReference" Xmlbf.fromXml
                return $ Just (ExplanationOfBenefitAccidentLocationReference (                      has))


data ExplanationOfBenefitTotal = ExplanationOfBenefitTotal {
    explanationOfBenefitTotalAttrId :: Maybe Text
  , explanationOfBenefitTotalExtension :: [Extension]
  , explanationOfBenefitTotalModifierExtension :: [Extension]
  , explanationOfBenefitTotalCategory :: CodeableConcept
  , explanationOfBenefitTotalAmount :: Money
  }
--

instance ToJSON ExplanationOfBenefitTotal where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitTotalAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitTotalExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitTotalModifierExtension p)
    ,  "category" .= toJSON (explanationOfBenefitTotalCategory p)
    ,  "amount" .= toJSON (explanationOfBenefitTotalAmount p)
    ]
instance FromJSON ExplanationOfBenefitTotal where
  parseJSON = withObject "ExplanationOfBenefitTotal" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        category <- o .:  "category"
        amount <- o .:  "amount"
        return ExplanationOfBenefitTotal{
            explanationOfBenefitTotalAttrId = id
          , explanationOfBenefitTotalExtension = extension
          , explanationOfBenefitTotalModifierExtension = modifierExtension
          , explanationOfBenefitTotalCategory = category
          , explanationOfBenefitTotalAmount = amount
          }
instance Xmlbf.ToXml ExplanationOfBenefitTotal where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitTotalAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitTotalExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitTotalModifierExtension p))
             , Prop     "category" (HM.empty, Xmlbf.toXml (explanationOfBenefitTotalCategory p))
             , Prop     "amount" (HM.empty, Xmlbf.toXml (explanationOfBenefitTotalAmount p))
             ]
instance Xmlbf.FromXml ExplanationOfBenefitTotal where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    category <-            Xmlbf.pElement "category" Xmlbf.fromXml
    amount <-            Xmlbf.pElement "amount" Xmlbf.fromXml
    return ExplanationOfBenefitTotal {
            explanationOfBenefitTotalAttrId = id
          , explanationOfBenefitTotalExtension = extension
          , explanationOfBenefitTotalModifierExtension = modifierExtension
          , explanationOfBenefitTotalCategory = category
          , explanationOfBenefitTotalAmount = amount
          }



data ExplanationOfBenefitPayment = ExplanationOfBenefitPayment {
    explanationOfBenefitPaymentAttrId :: Maybe Text
  , explanationOfBenefitPaymentExtension :: [Extension]
  , explanationOfBenefitPaymentModifierExtension :: [Extension]
  , explanationOfBenefitPaymentType :: Maybe CodeableConcept
  , explanationOfBenefitPaymentAdjustment :: Maybe Money
  , explanationOfBenefitPaymentAdjustmentReason :: Maybe CodeableConcept
  , explanationOfBenefitPaymentDate :: Maybe Date
  , explanationOfBenefitPaymentAmount :: Maybe Money
  , explanationOfBenefitPaymentIdentifier :: Maybe Identifier
  }
--

instance ToJSON ExplanationOfBenefitPayment where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitPaymentAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitPaymentExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitPaymentModifierExtension p)
    ,  "type" .= toJSON (explanationOfBenefitPaymentType p)
    ,  "adjustment" .= toJSON (explanationOfBenefitPaymentAdjustment p)
    ,  "adjustmentReason" .= toJSON (explanationOfBenefitPaymentAdjustmentReason p)
    ,  "date" .= toJSON (explanationOfBenefitPaymentDate p)
    ,  "amount" .= toJSON (explanationOfBenefitPaymentAmount p)
    ,  "identifier" .= toJSON (explanationOfBenefitPaymentIdentifier p)
    ]
instance FromJSON ExplanationOfBenefitPayment where
  parseJSON = withObject "ExplanationOfBenefitPayment" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:? "type"
        adjustment <- o .:? "adjustment"
        adjustmentReason <- o .:? "adjustmentReason"
        date <- o .:? "date"
        amount <- o .:? "amount"
        identifier <- o .:? "identifier"
        return ExplanationOfBenefitPayment{
            explanationOfBenefitPaymentAttrId = id
          , explanationOfBenefitPaymentExtension = extension
          , explanationOfBenefitPaymentModifierExtension = modifierExtension
          , explanationOfBenefitPaymentType = ty
          , explanationOfBenefitPaymentAdjustment = adjustment
          , explanationOfBenefitPaymentAdjustmentReason = adjustmentReason
          , explanationOfBenefitPaymentDate = date
          , explanationOfBenefitPaymentAmount = amount
          , explanationOfBenefitPaymentIdentifier = identifier
          }
instance Xmlbf.ToXml ExplanationOfBenefitPayment where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitPaymentAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitPaymentExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitPaymentModifierExtension p))
             , OptProp  "type" (fmap Xmlbf.toXml (explanationOfBenefitPaymentType p))
             , OptProp  "adjustment" (fmap Xmlbf.toXml (explanationOfBenefitPaymentAdjustment p))
             , OptProp  "adjustmentReason" (fmap Xmlbf.toXml (explanationOfBenefitPaymentAdjustmentReason p))
             , OptVal   "date" (fmap toDate (explanationOfBenefitPaymentDate p))
             , OptProp  "amount" (fmap Xmlbf.toXml (explanationOfBenefitPaymentAmount p))
             , OptProp  "identifier" (fmap Xmlbf.toXml (explanationOfBenefitPaymentIdentifier p))
             ]
instance Xmlbf.FromXml ExplanationOfBenefitPayment where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <- optional $ Xmlbf.pElement "type" Xmlbf.fromXml
    adjustment <- optional $ Xmlbf.pElement "adjustment" Xmlbf.fromXml
    adjustmentReason <- optional $ Xmlbf.pElement "adjustmentReason" Xmlbf.fromXml
    date <- optional $ Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    amount <- optional $ Xmlbf.pElement "amount" Xmlbf.fromXml
    identifier <- optional $ Xmlbf.pElement "identifier" Xmlbf.fromXml
    return ExplanationOfBenefitPayment {
            explanationOfBenefitPaymentAttrId = id
          , explanationOfBenefitPaymentExtension = extension
          , explanationOfBenefitPaymentModifierExtension = modifierExtension
          , explanationOfBenefitPaymentType = ty
          , explanationOfBenefitPaymentAdjustment = adjustment
          , explanationOfBenefitPaymentAdjustmentReason = adjustmentReason
          , explanationOfBenefitPaymentDate = fmap fromDate date
          , explanationOfBenefitPaymentAmount = amount
          , explanationOfBenefitPaymentIdentifier = identifier
          }



data ExplanationOfBenefitAdjudication = ExplanationOfBenefitAdjudication {
    explanationOfBenefitAdjudicationAttrId :: Maybe Text
  , explanationOfBenefitAdjudicationExtension :: [Extension]
  , explanationOfBenefitAdjudicationModifierExtension :: [Extension]
  , explanationOfBenefitAdjudicationCategory :: CodeableConcept
  , explanationOfBenefitAdjudicationReason :: Maybe CodeableConcept
  , explanationOfBenefitAdjudicationAmount :: Maybe Money
  , explanationOfBenefitAdjudicationValue :: Maybe Decimal
  }
--

instance ToJSON ExplanationOfBenefitAdjudication where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitAdjudicationAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitAdjudicationExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitAdjudicationModifierExtension p)
    ,  "category" .= toJSON (explanationOfBenefitAdjudicationCategory p)
    ,  "reason" .= toJSON (explanationOfBenefitAdjudicationReason p)
    ,  "amount" .= toJSON (explanationOfBenefitAdjudicationAmount p)
    ,  "value" .= toJSON (explanationOfBenefitAdjudicationValue p)
    ]
instance FromJSON ExplanationOfBenefitAdjudication where
  parseJSON = withObject "ExplanationOfBenefitAdjudication" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        category <- o .:  "category"
        reason <- o .:? "reason"
        amount <- o .:? "amount"
        value <- o .:? "value"
        return ExplanationOfBenefitAdjudication{
            explanationOfBenefitAdjudicationAttrId = id
          , explanationOfBenefitAdjudicationExtension = extension
          , explanationOfBenefitAdjudicationModifierExtension = modifierExtension
          , explanationOfBenefitAdjudicationCategory = category
          , explanationOfBenefitAdjudicationReason = reason
          , explanationOfBenefitAdjudicationAmount = amount
          , explanationOfBenefitAdjudicationValue = value
          }
instance Xmlbf.ToXml ExplanationOfBenefitAdjudication where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitAdjudicationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitAdjudicationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitAdjudicationModifierExtension p))
             , Prop     "category" (HM.empty, Xmlbf.toXml (explanationOfBenefitAdjudicationCategory p))
             , OptProp  "reason" (fmap Xmlbf.toXml (explanationOfBenefitAdjudicationReason p))
             , OptProp  "amount" (fmap Xmlbf.toXml (explanationOfBenefitAdjudicationAmount p))
             , OptVal   "value" (fmap toDecimal (explanationOfBenefitAdjudicationValue p))
             ]
instance Xmlbf.FromXml ExplanationOfBenefitAdjudication where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    category <-            Xmlbf.pElement "category" Xmlbf.fromXml
    reason <- optional $ Xmlbf.pElement "reason" Xmlbf.fromXml
    amount <- optional $ Xmlbf.pElement "amount" Xmlbf.fromXml
    value <- optional $ Xmlbf.pElement "value" (Xmlbf.pAttr "value")
    return ExplanationOfBenefitAdjudication {
            explanationOfBenefitAdjudicationAttrId = id
          , explanationOfBenefitAdjudicationExtension = extension
          , explanationOfBenefitAdjudicationModifierExtension = modifierExtension
          , explanationOfBenefitAdjudicationCategory = category
          , explanationOfBenefitAdjudicationReason = reason
          , explanationOfBenefitAdjudicationAmount = amount
          , explanationOfBenefitAdjudicationValue = fmap fromDecimal value
          }



data ExplanationOfBenefitDiagnosisDiagnosis
    = ExplanationOfBenefitDiagnosisDiagnosisCodeableConcept CodeableConcept
    | ExplanationOfBenefitDiagnosisDiagnosisReference Reference
    deriving (Eq, Show)

data ExplanationOfBenefitDiagnosis = ExplanationOfBenefitDiagnosis {
    explanationOfBenefitDiagnosisAttrId :: Maybe Text
  , explanationOfBenefitDiagnosisExtension :: [Extension]
  , explanationOfBenefitDiagnosisModifierExtension :: [Extension]
  , explanationOfBenefitDiagnosisSequence :: PositiveInt
  , explanationOfBenefitDiagnosisDiagnosis :: ExplanationOfBenefitDiagnosisDiagnosis
  , explanationOfBenefitDiagnosisType :: [CodeableConcept]
  , explanationOfBenefitDiagnosisOnAdmission :: Maybe CodeableConcept
  , explanationOfBenefitDiagnosisPackageCode :: Maybe CodeableConcept
  }
--

instance ToJSON ExplanationOfBenefitDiagnosis where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitDiagnosisAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitDiagnosisExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitDiagnosisModifierExtension p)
    ,  "sequence" .= toJSON (explanationOfBenefitDiagnosisSequence p)
    , toDiagnosisJSON (explanationOfBenefitDiagnosisDiagnosis p)
    ,  "type" .= toJSON (explanationOfBenefitDiagnosisType p)
    ,  "onAdmission" .= toJSON (explanationOfBenefitDiagnosisOnAdmission p)
    ,  "packageCode" .= toJSON (explanationOfBenefitDiagnosisPackageCode p)
    ]
    where 
      toDiagnosisJSON (     (ExplanationOfBenefitDiagnosisDiagnosisCodeableConcept c)) = ("diagnosis", toJSON c)
      toDiagnosisJSON (     (ExplanationOfBenefitDiagnosisDiagnosisReference c)) = ("diagnosis", toJSON c)
instance FromJSON ExplanationOfBenefitDiagnosis where
  parseJSON = withObject "ExplanationOfBenefitDiagnosis" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        sequence <- o .:  "sequence"
        diagnosis <- parseDiagnosis o
        ty <- o .:? "type" .!= []
        onAdmission <- o .:? "onAdmission"
        packageCode <- o .:? "packageCode"
        return ExplanationOfBenefitDiagnosis{
            explanationOfBenefitDiagnosisAttrId = id
          , explanationOfBenefitDiagnosisExtension = extension
          , explanationOfBenefitDiagnosisModifierExtension = modifierExtension
          , explanationOfBenefitDiagnosisSequence = sequence
          , explanationOfBenefitDiagnosisDiagnosis = diagnosis
          , explanationOfBenefitDiagnosisType = ty
          , explanationOfBenefitDiagnosisOnAdmission = onAdmission
          , explanationOfBenefitDiagnosisPackageCode = packageCode
          }
    where 
      parseDiagnosis o = parseDiagnosisCodeableConcept o <|> parseDiagnosisReference o
      parseDiagnosisCodeableConcept o = do
                has <- o .: "diagnosisCodeableConcept"
                return $ ExplanationOfBenefitDiagnosisDiagnosisCodeableConcept has
      parseDiagnosisReference o = do
                has <- o .: "diagnosisReference"
                return $ ExplanationOfBenefitDiagnosisDiagnosisReference has
instance Xmlbf.ToXml ExplanationOfBenefitDiagnosis where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitDiagnosisAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitDiagnosisExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitDiagnosisModifierExtension p))
             , Val      "sequence" (     toPositiveInt (explanationOfBenefitDiagnosisSequence p))
             , toDiagnosisXml (explanationOfBenefitDiagnosisDiagnosis p)
             , PropList "type" (fmap Xmlbf.toXml (explanationOfBenefitDiagnosisType p))
             , OptProp  "onAdmission" (fmap Xmlbf.toXml (explanationOfBenefitDiagnosisOnAdmission p))
             , OptProp  "packageCode" (fmap Xmlbf.toXml (explanationOfBenefitDiagnosisPackageCode p))
             ]
       where 
          toDiagnosisXml (     (ExplanationOfBenefitDiagnosisDiagnosisCodeableConcept p)) = Prop     "diagnosisCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toDiagnosisXml (     (ExplanationOfBenefitDiagnosisDiagnosisReference p)) = Prop     "diagnosisReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml ExplanationOfBenefitDiagnosis where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    sequence <-            Xmlbf.pElement "sequence" (Xmlbf.pAttr "value")
    diagnosis <- fromDiagnosisXml
    ty <- many     $ Xmlbf.pElement "type" Xmlbf.fromXml
    onAdmission <- optional $ Xmlbf.pElement "onAdmission" Xmlbf.fromXml
    packageCode <- optional $ Xmlbf.pElement "packageCode" Xmlbf.fromXml
    return ExplanationOfBenefitDiagnosis {
            explanationOfBenefitDiagnosisAttrId = id
          , explanationOfBenefitDiagnosisExtension = extension
          , explanationOfBenefitDiagnosisModifierExtension = modifierExtension
          , explanationOfBenefitDiagnosisSequence =      fromPositiveInt sequence
          , explanationOfBenefitDiagnosisDiagnosis = diagnosis
          , explanationOfBenefitDiagnosisType = ty
          , explanationOfBenefitDiagnosisOnAdmission = onAdmission
          , explanationOfBenefitDiagnosisPackageCode = packageCode
          }

    where 
      fromDiagnosisXml = parseDiagnosisCodeableConcept <|> parseDiagnosisReference
      parseDiagnosisCodeableConcept = do
                has <- Xmlbf.pElement "diagnosisCodeableConcept" Xmlbf.fromXml
                return $ ExplanationOfBenefitDiagnosisDiagnosisCodeableConcept (                      has)
      parseDiagnosisReference = do
                has <- Xmlbf.pElement "diagnosisReference" Xmlbf.fromXml
                return $ ExplanationOfBenefitDiagnosisDiagnosisReference (                      has)


data ExplanationOfBenefitSupportingInfoTiming
    = ExplanationOfBenefitSupportingInfoTimingDate Date
    | ExplanationOfBenefitSupportingInfoTimingPeriod Period
    deriving (Eq, Show)

data ExplanationOfBenefitSupportingInfoValue
    = ExplanationOfBenefitSupportingInfoValueBoolean Boolean
    | ExplanationOfBenefitSupportingInfoValueString Text
    | ExplanationOfBenefitSupportingInfoValueQuantity Quantity
    | ExplanationOfBenefitSupportingInfoValueAttachment Attachment
    | ExplanationOfBenefitSupportingInfoValueReference Reference
    deriving (Eq, Show)

data ExplanationOfBenefitSupportingInfo = ExplanationOfBenefitSupportingInfo {
    explanationOfBenefitSupportingInfoAttrId :: Maybe Text
  , explanationOfBenefitSupportingInfoExtension :: [Extension]
  , explanationOfBenefitSupportingInfoModifierExtension :: [Extension]
  , explanationOfBenefitSupportingInfoSequence :: PositiveInt
  , explanationOfBenefitSupportingInfoCategory :: CodeableConcept
  , explanationOfBenefitSupportingInfoCode :: Maybe CodeableConcept
  , explanationOfBenefitSupportingInfoTimingDate :: Maybe Date
  , explanationOfBenefitSupportingInfoTimingPeriod :: Maybe Period
  , explanationOfBenefitSupportingInfoValueBoolean :: Maybe Boolean
  , explanationOfBenefitSupportingInfoValueString :: Maybe Text
  , explanationOfBenefitSupportingInfoValueQuantity :: Maybe Quantity
  , explanationOfBenefitSupportingInfoValueAttachment :: Maybe Attachment
  , explanationOfBenefitSupportingInfoValueReference :: Maybe Reference
  , explanationOfBenefitSupportingInfoReason :: Maybe Coding
  }
--

instance ToJSON ExplanationOfBenefitSupportingInfo where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitSupportingInfoAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitSupportingInfoExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitSupportingInfoModifierExtension p)
    ,  "sequence" .= toJSON (explanationOfBenefitSupportingInfoSequence p)
    ,  "category" .= toJSON (explanationOfBenefitSupportingInfoCategory p)
    ,  "code" .= toJSON (explanationOfBenefitSupportingInfoCode p)
    ,  "timingDate" .= toJSON (explanationOfBenefitSupportingInfoTimingDate p)
    ,  "timingPeriod" .= toJSON (explanationOfBenefitSupportingInfoTimingPeriod p)
    ,  "valueBoolean" .= toJSON (explanationOfBenefitSupportingInfoValueBoolean p)
    ,  "valueString" .= toJSON (explanationOfBenefitSupportingInfoValueString p)
    ,  "valueQuantity" .= toJSON (explanationOfBenefitSupportingInfoValueQuantity p)
    ,  "valueAttachment" .= toJSON (explanationOfBenefitSupportingInfoValueAttachment p)
    ,  "valueReference" .= toJSON (explanationOfBenefitSupportingInfoValueReference p)
    ,  "reason" .= toJSON (explanationOfBenefitSupportingInfoReason p)
    ]
instance FromJSON ExplanationOfBenefitSupportingInfo where
  parseJSON = withObject "ExplanationOfBenefitSupportingInfo" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        sequence <- o .:  "sequence"
        category <- o .:  "category"
        code <- o .:? "code"
        timingDate <- o .:? "timingDate"
        timingPeriod <- o .:? "timingPeriod"
        valueBoolean <- o .:? "valueBoolean"
        valueString <- o .:? "valueString"
        valueQuantity <- o .:? "valueQuantity"
        valueAttachment <- o .:? "valueAttachment"
        valueReference <- o .:? "valueReference"
        reason <- o .:? "reason"
        return ExplanationOfBenefitSupportingInfo{
            explanationOfBenefitSupportingInfoAttrId = id
          , explanationOfBenefitSupportingInfoExtension = extension
          , explanationOfBenefitSupportingInfoModifierExtension = modifierExtension
          , explanationOfBenefitSupportingInfoSequence = sequence
          , explanationOfBenefitSupportingInfoCategory = category
          , explanationOfBenefitSupportingInfoCode = code
          , explanationOfBenefitSupportingInfoTimingDate = timingDate
          , explanationOfBenefitSupportingInfoTimingPeriod = timingPeriod
          , explanationOfBenefitSupportingInfoValueBoolean = valueBoolean
          , explanationOfBenefitSupportingInfoValueString = valueString
          , explanationOfBenefitSupportingInfoValueQuantity = valueQuantity
          , explanationOfBenefitSupportingInfoValueAttachment = valueAttachment
          , explanationOfBenefitSupportingInfoValueReference = valueReference
          , explanationOfBenefitSupportingInfoReason = reason
          }
instance Xmlbf.ToXml ExplanationOfBenefitSupportingInfo where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitSupportingInfoAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitSupportingInfoExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitSupportingInfoModifierExtension p))
             , Val      "sequence" (     toPositiveInt (explanationOfBenefitSupportingInfoSequence p))
             , Prop     "category" (HM.empty, Xmlbf.toXml (explanationOfBenefitSupportingInfoCategory p))
             , OptProp  "code" (fmap Xmlbf.toXml (explanationOfBenefitSupportingInfoCode p))
             , OptVal   "timingDate" (fmap toDate (explanationOfBenefitSupportingInfoTimingDate p))
             , OptProp  "timingPeriod" (fmap Xmlbf.toXml (explanationOfBenefitSupportingInfoTimingPeriod p))
             , OptVal   "valueBoolean" (fmap toBoolean (explanationOfBenefitSupportingInfoValueBoolean p))
             , OptVal   "valueString" (fmap toString (explanationOfBenefitSupportingInfoValueString p))
             , OptProp  "valueQuantity" (fmap Xmlbf.toXml (explanationOfBenefitSupportingInfoValueQuantity p))
             , OptProp  "valueAttachment" (fmap Xmlbf.toXml (explanationOfBenefitSupportingInfoValueAttachment p))
             , OptProp  "valueReference" (fmap Xmlbf.toXml (explanationOfBenefitSupportingInfoValueReference p))
             , OptProp  "reason" (fmap Xmlbf.toXml (explanationOfBenefitSupportingInfoReason p))
             ]
instance Xmlbf.FromXml ExplanationOfBenefitSupportingInfo where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    sequence <-            Xmlbf.pElement "sequence" (Xmlbf.pAttr "value")
    category <-            Xmlbf.pElement "category" Xmlbf.fromXml
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    timingDate <- optional $ Xmlbf.pElement "timingDate" (Xmlbf.pAttr "value")
    timingPeriod <- optional $ Xmlbf.pElement "timingPeriod" Xmlbf.fromXml
    valueBoolean <- optional $ Xmlbf.pElement "valueBoolean" (Xmlbf.pAttr "value")
    valueString <- optional $ Xmlbf.pElement "valueString" (Xmlbf.pAttr "value")
    valueQuantity <- optional $ Xmlbf.pElement "valueQuantity" Xmlbf.fromXml
    valueAttachment <- optional $ Xmlbf.pElement "valueAttachment" Xmlbf.fromXml
    valueReference <- optional $ Xmlbf.pElement "valueReference" Xmlbf.fromXml
    reason <- optional $ Xmlbf.pElement "reason" Xmlbf.fromXml
    return ExplanationOfBenefitSupportingInfo {
            explanationOfBenefitSupportingInfoAttrId = id
          , explanationOfBenefitSupportingInfoExtension = extension
          , explanationOfBenefitSupportingInfoModifierExtension = modifierExtension
          , explanationOfBenefitSupportingInfoSequence =      fromPositiveInt sequence
          , explanationOfBenefitSupportingInfoCategory = category
          , explanationOfBenefitSupportingInfoCode = code
          , explanationOfBenefitSupportingInfoTimingDate = fmap fromDate timingDate
          , explanationOfBenefitSupportingInfoTimingPeriod = timingPeriod
          , explanationOfBenefitSupportingInfoValueBoolean = fmap fromBoolean valueBoolean
          , explanationOfBenefitSupportingInfoValueString = fmap fromString valueString
          , explanationOfBenefitSupportingInfoValueQuantity = valueQuantity
          , explanationOfBenefitSupportingInfoValueAttachment = valueAttachment
          , explanationOfBenefitSupportingInfoValueReference = valueReference
          , explanationOfBenefitSupportingInfoReason = reason
          }



data ExplanationOfBenefitProcessNoteType
    = EOBPNTDisplay
    | EOBPNTPrint
    | EOBPNTPrintoper
  deriving (Eq, Show)

instance ToJSON ExplanationOfBenefitProcessNoteType where
    toJSON EOBPNTDisplay = String "display"
    toJSON EOBPNTPrint = String "print"
    toJSON EOBPNTPrintoper = String "printoper"
instance FromJSON ExplanationOfBenefitProcessNoteType where
    parseJSON "display" = return EOBPNTDisplay
    parseJSON "print" = return EOBPNTPrint
    parseJSON "printoper" = return EOBPNTPrintoper

toExplanationOfBenefitProcessNoteType EOBPNTDisplay = "display"
toExplanationOfBenefitProcessNoteType EOBPNTPrint = "print"
toExplanationOfBenefitProcessNoteType EOBPNTPrintoper = "printoper"
fromExplanationOfBenefitProcessNoteType "display" = EOBPNTDisplay
fromExplanationOfBenefitProcessNoteType "print" = EOBPNTPrint
fromExplanationOfBenefitProcessNoteType "printoper" = EOBPNTPrintoper


data ExplanationOfBenefitProcessNote = ExplanationOfBenefitProcessNote {
    explanationOfBenefitProcessNoteAttrId :: Maybe Text
  , explanationOfBenefitProcessNoteExtension :: [Extension]
  , explanationOfBenefitProcessNoteModifierExtension :: [Extension]
  , explanationOfBenefitProcessNoteNumber :: Maybe PositiveInt
  , explanationOfBenefitProcessNoteType :: Maybe ExplanationOfBenefitProcessNoteType
  , explanationOfBenefitProcessNoteText :: Maybe Text
  , explanationOfBenefitProcessNoteLanguage :: Maybe CodeableConcept
  }
--

instance ToJSON ExplanationOfBenefitProcessNote where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitProcessNoteAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitProcessNoteExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitProcessNoteModifierExtension p)
    ,  "number" .= toJSON (explanationOfBenefitProcessNoteNumber p)
    ,  "type" .= toJSON (explanationOfBenefitProcessNoteType p)
    ,  "text" .= toJSON (explanationOfBenefitProcessNoteText p)
    ,  "language" .= toJSON (explanationOfBenefitProcessNoteLanguage p)
    ]
instance FromJSON ExplanationOfBenefitProcessNote where
  parseJSON = withObject "ExplanationOfBenefitProcessNote" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        number <- o .:? "number"
        ty <- o .:? "type"
        text <- o .:? "text"
        language <- o .:? "language"
        return ExplanationOfBenefitProcessNote{
            explanationOfBenefitProcessNoteAttrId = id
          , explanationOfBenefitProcessNoteExtension = extension
          , explanationOfBenefitProcessNoteModifierExtension = modifierExtension
          , explanationOfBenefitProcessNoteNumber = number
          , explanationOfBenefitProcessNoteType = ty
          , explanationOfBenefitProcessNoteText = text
          , explanationOfBenefitProcessNoteLanguage = language
          }
instance Xmlbf.ToXml ExplanationOfBenefitProcessNote where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitProcessNoteAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitProcessNoteExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitProcessNoteModifierExtension p))
             , OptVal   "number" (fmap toPositiveInt (explanationOfBenefitProcessNoteNumber p))
             , OptVal   "type" (fmap toExplanationOfBenefitProcessNoteType (explanationOfBenefitProcessNoteType p))
             , OptVal   "text" (fmap toString (explanationOfBenefitProcessNoteText p))
             , OptProp  "language" (fmap Xmlbf.toXml (explanationOfBenefitProcessNoteLanguage p))
             ]
instance Xmlbf.FromXml ExplanationOfBenefitProcessNote where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    number <- optional $ Xmlbf.pElement "number" (Xmlbf.pAttr "value")
    ty <- optional $ Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" Xmlbf.fromXml
    return ExplanationOfBenefitProcessNote {
            explanationOfBenefitProcessNoteAttrId = id
          , explanationOfBenefitProcessNoteExtension = extension
          , explanationOfBenefitProcessNoteModifierExtension = modifierExtension
          , explanationOfBenefitProcessNoteNumber = fmap fromPositiveInt number
          , explanationOfBenefitProcessNoteType = fmap fromExplanationOfBenefitProcessNoteType ty
          , explanationOfBenefitProcessNoteText = fmap fromString text
          , explanationOfBenefitProcessNoteLanguage = language
          }



data ExplanationOfBenefitPayee = ExplanationOfBenefitPayee {
    explanationOfBenefitPayeeAttrId :: Maybe Text
  , explanationOfBenefitPayeeExtension :: [Extension]
  , explanationOfBenefitPayeeModifierExtension :: [Extension]
  , explanationOfBenefitPayeeType :: Maybe CodeableConcept
  , explanationOfBenefitPayeeParty :: Maybe Reference
  }
--

instance ToJSON ExplanationOfBenefitPayee where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitPayeeAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitPayeeExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitPayeeModifierExtension p)
    ,  "type" .= toJSON (explanationOfBenefitPayeeType p)
    ,  "party" .= toJSON (explanationOfBenefitPayeeParty p)
    ]
instance FromJSON ExplanationOfBenefitPayee where
  parseJSON = withObject "ExplanationOfBenefitPayee" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:? "type"
        party <- o .:? "party"
        return ExplanationOfBenefitPayee{
            explanationOfBenefitPayeeAttrId = id
          , explanationOfBenefitPayeeExtension = extension
          , explanationOfBenefitPayeeModifierExtension = modifierExtension
          , explanationOfBenefitPayeeType = ty
          , explanationOfBenefitPayeeParty = party
          }
instance Xmlbf.ToXml ExplanationOfBenefitPayee where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitPayeeAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitPayeeExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitPayeeModifierExtension p))
             , OptProp  "type" (fmap Xmlbf.toXml (explanationOfBenefitPayeeType p))
             , OptProp  "party" (fmap Xmlbf.toXml (explanationOfBenefitPayeeParty p))
             ]
instance Xmlbf.FromXml ExplanationOfBenefitPayee where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <- optional $ Xmlbf.pElement "type" Xmlbf.fromXml
    party <- optional $ Xmlbf.pElement "party" Xmlbf.fromXml
    return ExplanationOfBenefitPayee {
            explanationOfBenefitPayeeAttrId = id
          , explanationOfBenefitPayeeExtension = extension
          , explanationOfBenefitPayeeModifierExtension = modifierExtension
          , explanationOfBenefitPayeeType = ty
          , explanationOfBenefitPayeeParty = party
          }



data ExplanationOfBenefitRelated = ExplanationOfBenefitRelated {
    explanationOfBenefitRelatedAttrId :: Maybe Text
  , explanationOfBenefitRelatedExtension :: [Extension]
  , explanationOfBenefitRelatedModifierExtension :: [Extension]
  , explanationOfBenefitRelatedClaim :: Maybe Reference
  , explanationOfBenefitRelatedRelationship :: Maybe CodeableConcept
  , explanationOfBenefitRelatedReference :: Maybe Identifier
  }
--

instance ToJSON ExplanationOfBenefitRelated where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitRelatedAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitRelatedExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitRelatedModifierExtension p)
    ,  "claim" .= toJSON (explanationOfBenefitRelatedClaim p)
    ,  "relationship" .= toJSON (explanationOfBenefitRelatedRelationship p)
    ,  "reference" .= toJSON (explanationOfBenefitRelatedReference p)
    ]
instance FromJSON ExplanationOfBenefitRelated where
  parseJSON = withObject "ExplanationOfBenefitRelated" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        claim <- o .:? "claim"
        relationship <- o .:? "relationship"
        reference <- o .:? "reference"
        return ExplanationOfBenefitRelated{
            explanationOfBenefitRelatedAttrId = id
          , explanationOfBenefitRelatedExtension = extension
          , explanationOfBenefitRelatedModifierExtension = modifierExtension
          , explanationOfBenefitRelatedClaim = claim
          , explanationOfBenefitRelatedRelationship = relationship
          , explanationOfBenefitRelatedReference = reference
          }
instance Xmlbf.ToXml ExplanationOfBenefitRelated where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitRelatedAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitRelatedExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitRelatedModifierExtension p))
             , OptProp  "claim" (fmap Xmlbf.toXml (explanationOfBenefitRelatedClaim p))
             , OptProp  "relationship" (fmap Xmlbf.toXml (explanationOfBenefitRelatedRelationship p))
             , OptProp  "reference" (fmap Xmlbf.toXml (explanationOfBenefitRelatedReference p))
             ]
instance Xmlbf.FromXml ExplanationOfBenefitRelated where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    claim <- optional $ Xmlbf.pElement "claim" Xmlbf.fromXml
    relationship <- optional $ Xmlbf.pElement "relationship" Xmlbf.fromXml
    reference <- optional $ Xmlbf.pElement "reference" Xmlbf.fromXml
    return ExplanationOfBenefitRelated {
            explanationOfBenefitRelatedAttrId = id
          , explanationOfBenefitRelatedExtension = extension
          , explanationOfBenefitRelatedModifierExtension = modifierExtension
          , explanationOfBenefitRelatedClaim = claim
          , explanationOfBenefitRelatedRelationship = relationship
          , explanationOfBenefitRelatedReference = reference
          }



data ExplanationOfBenefitFinancialAllowed
    = ExplanationOfBenefitFinancialAllowedUnsignedInt UnsignedInt
    | ExplanationOfBenefitFinancialAllowedString Text
    | ExplanationOfBenefitFinancialAllowedMoney Money
    deriving (Eq, Show)

data ExplanationOfBenefitFinancialUsed
    = ExplanationOfBenefitFinancialUsedUnsignedInt UnsignedInt
    | ExplanationOfBenefitFinancialUsedMoney Money
    deriving (Eq, Show)

data ExplanationOfBenefitFinancial = ExplanationOfBenefitFinancial {
    explanationOfBenefitFinancialAttrId :: Maybe Text
  , explanationOfBenefitFinancialExtension :: [Extension]
  , explanationOfBenefitFinancialModifierExtension :: [Extension]
  , explanationOfBenefitFinancialType :: CodeableConcept
  , explanationOfBenefitFinancialAllowed :: Maybe ExplanationOfBenefitFinancialAllowed
  , explanationOfBenefitFinancialUsed :: Maybe ExplanationOfBenefitFinancialUsed
  }
--

instance ToJSON ExplanationOfBenefitFinancial where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitFinancialAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitFinancialExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitFinancialModifierExtension p)
    ,  "type" .= toJSON (explanationOfBenefitFinancialType p)
    , toAllowedJSON (explanationOfBenefitFinancialAllowed p)
    , toUsedJSON (explanationOfBenefitFinancialUsed p)
    ]
    where 
      toAllowedJSON (     Nothing   ) = ("allowed", Null)
      toAllowedJSON (Just (ExplanationOfBenefitFinancialAllowedUnsignedInt c)) = ("allowed", toJSON c)
      toAllowedJSON (Just (ExplanationOfBenefitFinancialAllowedString c)) = ("allowed", toJSON c)
      toAllowedJSON (Just (ExplanationOfBenefitFinancialAllowedMoney c)) = ("allowed", toJSON c)
      toUsedJSON (     Nothing   ) = ("used", Null)
      toUsedJSON (Just (ExplanationOfBenefitFinancialUsedUnsignedInt c)) = ("used", toJSON c)
      toUsedJSON (Just (ExplanationOfBenefitFinancialUsedMoney c)) = ("used", toJSON c)
instance FromJSON ExplanationOfBenefitFinancial where
  parseJSON = withObject "ExplanationOfBenefitFinancial" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:  "type"
        allowed <- parseAllowed o
        used <- parseUsed o
        return ExplanationOfBenefitFinancial{
            explanationOfBenefitFinancialAttrId = id
          , explanationOfBenefitFinancialExtension = extension
          , explanationOfBenefitFinancialModifierExtension = modifierExtension
          , explanationOfBenefitFinancialType = ty
          , explanationOfBenefitFinancialAllowed = allowed
          , explanationOfBenefitFinancialUsed = used
          }
    where 
      parseAllowed o = parseAllowedUnsignedInt o <|> parseAllowedString o <|> parseAllowedMoney o
      parseAllowedUnsignedInt o = do
                has <- o .: "allowedUnsignedInt"
                return $ Just (ExplanationOfBenefitFinancialAllowedUnsignedInt has)
      parseAllowedString o = do
                has <- o .: "allowedString"
                return $ Just (ExplanationOfBenefitFinancialAllowedString has)
      parseAllowedMoney o = do
                has <- o .: "allowedMoney"
                return $ Just (ExplanationOfBenefitFinancialAllowedMoney has)
      parseUsed o = parseUsedUnsignedInt o <|> parseUsedMoney o
      parseUsedUnsignedInt o = do
                has <- o .: "usedUnsignedInt"
                return $ Just (ExplanationOfBenefitFinancialUsedUnsignedInt has)
      parseUsedMoney o = do
                has <- o .: "usedMoney"
                return $ Just (ExplanationOfBenefitFinancialUsedMoney has)
instance Xmlbf.ToXml ExplanationOfBenefitFinancial where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitFinancialAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitFinancialExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitFinancialModifierExtension p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (explanationOfBenefitFinancialType p))
             , toAllowedXml (explanationOfBenefitFinancialAllowed p)
             , toUsedXml (explanationOfBenefitFinancialUsed p)
             ]
       where 
          toAllowedXml ( Nothing   ) = (OptVal "allowed" Nothing)
          toAllowedXml (Just (ExplanationOfBenefitFinancialAllowedUnsignedInt p)) = Val   "allowedUnsignedInt" (toUnsignedInt p)
          toAllowedXml (Just (ExplanationOfBenefitFinancialAllowedString p)) = Val   "allowedString" (toString p)
          toAllowedXml (Just (ExplanationOfBenefitFinancialAllowedMoney p)) = Prop  "allowedMoney" (HM.empty, Xmlbf.toXml p)
          toUsedXml ( Nothing   ) = (OptVal "used" Nothing)
          toUsedXml (Just (ExplanationOfBenefitFinancialUsedUnsignedInt p)) = Val   "usedUnsignedInt" (toUnsignedInt p)
          toUsedXml (Just (ExplanationOfBenefitFinancialUsedMoney p)) = Prop  "usedMoney" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml ExplanationOfBenefitFinancial where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" Xmlbf.fromXml
    allowed <- fromAllowedXml
    used <- fromUsedXml
    return ExplanationOfBenefitFinancial {
            explanationOfBenefitFinancialAttrId = id
          , explanationOfBenefitFinancialExtension = extension
          , explanationOfBenefitFinancialModifierExtension = modifierExtension
          , explanationOfBenefitFinancialType = ty
          , explanationOfBenefitFinancialAllowed = allowed
          , explanationOfBenefitFinancialUsed = used
          }

    where 
      fromAllowedXml = parseAllowedUnsignedInt <|> parseAllowedString <|> parseAllowedMoney <|> pure Nothing
      parseAllowedUnsignedInt = do
                has <- Xmlbf.pElement "allowedUnsignedInt" (Xmlbf.pAttr "value")
                return $ Just (ExplanationOfBenefitFinancialAllowedUnsignedInt (     fromUnsignedInt has))
      parseAllowedString = do
                has <- Xmlbf.pElement "allowedString" (Xmlbf.pAttr "value")
                return $ Just (ExplanationOfBenefitFinancialAllowedString (     fromString has))
      parseAllowedMoney = do
                has <- Xmlbf.pElement "allowedMoney" Xmlbf.fromXml
                return $ Just (ExplanationOfBenefitFinancialAllowedMoney (                      has))
      fromUsedXml = parseUsedUnsignedInt <|> parseUsedMoney <|> pure Nothing
      parseUsedUnsignedInt = do
                has <- Xmlbf.pElement "usedUnsignedInt" (Xmlbf.pAttr "value")
                return $ Just (ExplanationOfBenefitFinancialUsedUnsignedInt (     fromUnsignedInt has))
      parseUsedMoney = do
                has <- Xmlbf.pElement "usedMoney" Xmlbf.fromXml
                return $ Just (ExplanationOfBenefitFinancialUsedMoney (                      has))


data ExplanationOfBenefitProcedureProcedure
    = ExplanationOfBenefitProcedureProcedureCodeableConcept CodeableConcept
    | ExplanationOfBenefitProcedureProcedureReference Reference
    deriving (Eq, Show)

data ExplanationOfBenefitProcedure = ExplanationOfBenefitProcedure {
    explanationOfBenefitProcedureAttrId :: Maybe Text
  , explanationOfBenefitProcedureExtension :: [Extension]
  , explanationOfBenefitProcedureModifierExtension :: [Extension]
  , explanationOfBenefitProcedureSequence :: PositiveInt
  , explanationOfBenefitProcedureType :: [CodeableConcept]
  , explanationOfBenefitProcedureDate :: Maybe DateTime
  , explanationOfBenefitProcedureProcedure :: ExplanationOfBenefitProcedureProcedure
  , explanationOfBenefitProcedureUdi :: [Reference]
  }
--

instance ToJSON ExplanationOfBenefitProcedure where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitProcedureAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitProcedureExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitProcedureModifierExtension p)
    ,  "sequence" .= toJSON (explanationOfBenefitProcedureSequence p)
    ,  "type" .= toJSON (explanationOfBenefitProcedureType p)
    ,  "date" .= toJSON (explanationOfBenefitProcedureDate p)
    , toProcedureJSON (explanationOfBenefitProcedureProcedure p)
    ,  "udi" .= toJSON (explanationOfBenefitProcedureUdi p)
    ]
    where 
      toProcedureJSON (     (ExplanationOfBenefitProcedureProcedureCodeableConcept c)) = ("procedure", toJSON c)
      toProcedureJSON (     (ExplanationOfBenefitProcedureProcedureReference c)) = ("procedure", toJSON c)
instance FromJSON ExplanationOfBenefitProcedure where
  parseJSON = withObject "ExplanationOfBenefitProcedure" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        sequence <- o .:  "sequence"
        ty <- o .:? "type" .!= []
        date <- o .:? "date"
        procedure <- parseProcedure o
        udi <- o .:? "udi" .!= []
        return ExplanationOfBenefitProcedure{
            explanationOfBenefitProcedureAttrId = id
          , explanationOfBenefitProcedureExtension = extension
          , explanationOfBenefitProcedureModifierExtension = modifierExtension
          , explanationOfBenefitProcedureSequence = sequence
          , explanationOfBenefitProcedureType = ty
          , explanationOfBenefitProcedureDate = date
          , explanationOfBenefitProcedureProcedure = procedure
          , explanationOfBenefitProcedureUdi = udi
          }
    where 
      parseProcedure o = parseProcedureCodeableConcept o <|> parseProcedureReference o
      parseProcedureCodeableConcept o = do
                has <- o .: "procedureCodeableConcept"
                return $ ExplanationOfBenefitProcedureProcedureCodeableConcept has
      parseProcedureReference o = do
                has <- o .: "procedureReference"
                return $ ExplanationOfBenefitProcedureProcedureReference has
instance Xmlbf.ToXml ExplanationOfBenefitProcedure where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitProcedureAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitProcedureExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitProcedureModifierExtension p))
             , Val      "sequence" (     toPositiveInt (explanationOfBenefitProcedureSequence p))
             , PropList "type" (fmap Xmlbf.toXml (explanationOfBenefitProcedureType p))
             , OptVal   "date" (fmap toDateTime (explanationOfBenefitProcedureDate p))
             , toProcedureXml (explanationOfBenefitProcedureProcedure p)
             , PropList "udi" (fmap Xmlbf.toXml (explanationOfBenefitProcedureUdi p))
             ]
       where 
          toProcedureXml (     (ExplanationOfBenefitProcedureProcedureCodeableConcept p)) = Prop     "procedureCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toProcedureXml (     (ExplanationOfBenefitProcedureProcedureReference p)) = Prop     "procedureReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml ExplanationOfBenefitProcedure where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    sequence <-            Xmlbf.pElement "sequence" (Xmlbf.pAttr "value")
    ty <- many     $ Xmlbf.pElement "type" Xmlbf.fromXml
    date <- optional $ Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    procedure <- fromProcedureXml
    udi <- many     $ Xmlbf.pElement "udi" Xmlbf.fromXml
    return ExplanationOfBenefitProcedure {
            explanationOfBenefitProcedureAttrId = id
          , explanationOfBenefitProcedureExtension = extension
          , explanationOfBenefitProcedureModifierExtension = modifierExtension
          , explanationOfBenefitProcedureSequence =      fromPositiveInt sequence
          , explanationOfBenefitProcedureType = ty
          , explanationOfBenefitProcedureDate = fmap fromDateTime date
          , explanationOfBenefitProcedureProcedure = procedure
          , explanationOfBenefitProcedureUdi = udi
          }

    where 
      fromProcedureXml = parseProcedureCodeableConcept <|> parseProcedureReference
      parseProcedureCodeableConcept = do
                has <- Xmlbf.pElement "procedureCodeableConcept" Xmlbf.fromXml
                return $ ExplanationOfBenefitProcedureProcedureCodeableConcept (                      has)
      parseProcedureReference = do
                has <- Xmlbf.pElement "procedureReference" Xmlbf.fromXml
                return $ ExplanationOfBenefitProcedureProcedureReference (                      has)


data ExplanationOfBenefitItemServiced
    = ExplanationOfBenefitItemServicedDate Date
    | ExplanationOfBenefitItemServicedPeriod Period
    deriving (Eq, Show)

data ExplanationOfBenefitItemLocation
    = ExplanationOfBenefitItemLocationCodeableConcept CodeableConcept
    | ExplanationOfBenefitItemLocationAddress Address
    | ExplanationOfBenefitItemLocationReference Reference
    deriving (Eq, Show)

data ExplanationOfBenefitItem = ExplanationOfBenefitItem {
    explanationOfBenefitItemAttrId :: Maybe Text
  , explanationOfBenefitItemExtension :: [Extension]
  , explanationOfBenefitItemModifierExtension :: [Extension]
  , explanationOfBenefitItemSequence :: PositiveInt
  , explanationOfBenefitItemCareTeamSequence :: [PositiveInt]
  , explanationOfBenefitItemDiagnosisSequence :: [PositiveInt]
  , explanationOfBenefitItemProcedureSequence :: [PositiveInt]
  , explanationOfBenefitItemInformationSequence :: [PositiveInt]
  , explanationOfBenefitItemRevenue :: Maybe CodeableConcept
  , explanationOfBenefitItemCategory :: Maybe CodeableConcept
  , explanationOfBenefitItemProductOrService :: CodeableConcept
  , explanationOfBenefitItemModifier :: [CodeableConcept]
  , explanationOfBenefitItemProgramCode :: [CodeableConcept]
  , explanationOfBenefitItemServiced :: Maybe ExplanationOfBenefitItemServiced
  , explanationOfBenefitItemLocation :: Maybe ExplanationOfBenefitItemLocation
  , explanationOfBenefitItemQuantity :: Maybe Quantity
  , explanationOfBenefitItemUnitPrice :: Maybe Money
  , explanationOfBenefitItemFactor :: Maybe Decimal
  , explanationOfBenefitItemNet :: Maybe Money
  , explanationOfBenefitItemUdi :: [Reference]
  , explanationOfBenefitItemBodySite :: Maybe CodeableConcept
  , explanationOfBenefitItemSubSite :: [CodeableConcept]
  , explanationOfBenefitItemEncounter :: [Reference]
  , explanationOfBenefitItemNoteNumber :: [PositiveInt]
  , explanationOfBenefitItemAdjudication :: [ExplanationOfBenefitAdjudication]
  , explanationOfBenefitItemDetail :: [ExplanationOfBenefitDetail]
  }
--

instance ToJSON ExplanationOfBenefitItem where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitItemAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitItemExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitItemModifierExtension p)
    ,  "sequence" .= toJSON (explanationOfBenefitItemSequence p)
    ,  "careTeamSequence" .= toJSON (explanationOfBenefitItemCareTeamSequence p)
    ,  "diagnosisSequence" .= toJSON (explanationOfBenefitItemDiagnosisSequence p)
    ,  "procedureSequence" .= toJSON (explanationOfBenefitItemProcedureSequence p)
    ,  "informationSequence" .= toJSON (explanationOfBenefitItemInformationSequence p)
    ,  "revenue" .= toJSON (explanationOfBenefitItemRevenue p)
    ,  "category" .= toJSON (explanationOfBenefitItemCategory p)
    ,  "productOrService" .= toJSON (explanationOfBenefitItemProductOrService p)
    ,  "modifier" .= toJSON (explanationOfBenefitItemModifier p)
    ,  "programCode" .= toJSON (explanationOfBenefitItemProgramCode p)
    , toServicedJSON (explanationOfBenefitItemServiced p)
    , toLocationJSON (explanationOfBenefitItemLocation p)
    ,  "quantity" .= toJSON (explanationOfBenefitItemQuantity p)
    ,  "unitPrice" .= toJSON (explanationOfBenefitItemUnitPrice p)
    ,  "factor" .= toJSON (explanationOfBenefitItemFactor p)
    ,  "net" .= toJSON (explanationOfBenefitItemNet p)
    ,  "udi" .= toJSON (explanationOfBenefitItemUdi p)
    ,  "bodySite" .= toJSON (explanationOfBenefitItemBodySite p)
    ,  "subSite" .= toJSON (explanationOfBenefitItemSubSite p)
    ,  "encounter" .= toJSON (explanationOfBenefitItemEncounter p)
    ,  "noteNumber" .= toJSON (explanationOfBenefitItemNoteNumber p)
    ,  "adjudication" .= toJSON (explanationOfBenefitItemAdjudication p)
    ,  "detail" .= toJSON (explanationOfBenefitItemDetail p)
    ]
    where 
      toServicedJSON (     Nothing   ) = ("serviced", Null)
      toServicedJSON (Just (ExplanationOfBenefitItemServicedDate c)) = ("serviced", toJSON c)
      toServicedJSON (Just (ExplanationOfBenefitItemServicedPeriod c)) = ("serviced", toJSON c)
      toLocationJSON (     Nothing   ) = ("location", Null)
      toLocationJSON (Just (ExplanationOfBenefitItemLocationCodeableConcept c)) = ("location", toJSON c)
      toLocationJSON (Just (ExplanationOfBenefitItemLocationAddress c)) = ("location", toJSON c)
      toLocationJSON (Just (ExplanationOfBenefitItemLocationReference c)) = ("location", toJSON c)
instance FromJSON ExplanationOfBenefitItem where
  parseJSON = withObject "ExplanationOfBenefitItem" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        sequence <- o .:  "sequence"
        careTeamSequence <- o .:? "careTeamSequence" .!= []
        diagnosisSequence <- o .:? "diagnosisSequence" .!= []
        procedureSequence <- o .:? "procedureSequence" .!= []
        informationSequence <- o .:? "informationSequence" .!= []
        revenue <- o .:? "revenue"
        category <- o .:? "category"
        productOrService <- o .:  "productOrService"
        modifier <- o .:? "modifier" .!= []
        programCode <- o .:? "programCode" .!= []
        serviced <- parseServiced o
        location <- parseLocation o
        quantity <- o .:? "quantity"
        unitPrice <- o .:? "unitPrice"
        factor <- o .:? "factor"
        net <- o .:? "net"
        udi <- o .:? "udi" .!= []
        bodySite <- o .:? "bodySite"
        subSite <- o .:? "subSite" .!= []
        encounter <- o .:? "encounter" .!= []
        noteNumber <- o .:? "noteNumber" .!= []
        adjudication <- o .:? "adjudication" .!= []
        detail <- o .:? "detail" .!= []
        return ExplanationOfBenefitItem{
            explanationOfBenefitItemAttrId = id
          , explanationOfBenefitItemExtension = extension
          , explanationOfBenefitItemModifierExtension = modifierExtension
          , explanationOfBenefitItemSequence = sequence
          , explanationOfBenefitItemCareTeamSequence = careTeamSequence
          , explanationOfBenefitItemDiagnosisSequence = diagnosisSequence
          , explanationOfBenefitItemProcedureSequence = procedureSequence
          , explanationOfBenefitItemInformationSequence = informationSequence
          , explanationOfBenefitItemRevenue = revenue
          , explanationOfBenefitItemCategory = category
          , explanationOfBenefitItemProductOrService = productOrService
          , explanationOfBenefitItemModifier = modifier
          , explanationOfBenefitItemProgramCode = programCode
          , explanationOfBenefitItemServiced = serviced
          , explanationOfBenefitItemLocation = location
          , explanationOfBenefitItemQuantity = quantity
          , explanationOfBenefitItemUnitPrice = unitPrice
          , explanationOfBenefitItemFactor = factor
          , explanationOfBenefitItemNet = net
          , explanationOfBenefitItemUdi = udi
          , explanationOfBenefitItemBodySite = bodySite
          , explanationOfBenefitItemSubSite = subSite
          , explanationOfBenefitItemEncounter = encounter
          , explanationOfBenefitItemNoteNumber = noteNumber
          , explanationOfBenefitItemAdjudication = adjudication
          , explanationOfBenefitItemDetail = detail
          }
    where 
      parseServiced o = parseServicedDate o <|> parseServicedPeriod o
      parseServicedDate o = do
                has <- o .: "servicedDate"
                return $ Just (ExplanationOfBenefitItemServicedDate has)
      parseServicedPeriod o = do
                has <- o .: "servicedPeriod"
                return $ Just (ExplanationOfBenefitItemServicedPeriod has)
      parseLocation o = parseLocationCodeableConcept o <|> parseLocationAddress o <|> parseLocationReference o
      parseLocationCodeableConcept o = do
                has <- o .: "locationCodeableConcept"
                return $ Just (ExplanationOfBenefitItemLocationCodeableConcept has)
      parseLocationAddress o = do
                has <- o .: "locationAddress"
                return $ Just (ExplanationOfBenefitItemLocationAddress has)
      parseLocationReference o = do
                has <- o .: "locationReference"
                return $ Just (ExplanationOfBenefitItemLocationReference has)
instance Xmlbf.ToXml ExplanationOfBenefitItem where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitItemAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitItemExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitItemModifierExtension p))
             , Val      "sequence" (     toPositiveInt (explanationOfBenefitItemSequence p))
             , ValList  "careTeamSequence" (fmap toPositiveInt (explanationOfBenefitItemCareTeamSequence p))
             , ValList  "diagnosisSequence" (fmap toPositiveInt (explanationOfBenefitItemDiagnosisSequence p))
             , ValList  "procedureSequence" (fmap toPositiveInt (explanationOfBenefitItemProcedureSequence p))
             , ValList  "informationSequence" (fmap toPositiveInt (explanationOfBenefitItemInformationSequence p))
             , OptProp  "revenue" (fmap Xmlbf.toXml (explanationOfBenefitItemRevenue p))
             , OptProp  "category" (fmap Xmlbf.toXml (explanationOfBenefitItemCategory p))
             , Prop     "productOrService" (HM.empty, Xmlbf.toXml (explanationOfBenefitItemProductOrService p))
             , PropList "modifier" (fmap Xmlbf.toXml (explanationOfBenefitItemModifier p))
             , PropList "programCode" (fmap Xmlbf.toXml (explanationOfBenefitItemProgramCode p))
             , toServicedXml (explanationOfBenefitItemServiced p)
             , toLocationXml (explanationOfBenefitItemLocation p)
             , OptProp  "quantity" (fmap Xmlbf.toXml (explanationOfBenefitItemQuantity p))
             , OptProp  "unitPrice" (fmap Xmlbf.toXml (explanationOfBenefitItemUnitPrice p))
             , OptVal   "factor" (fmap toDecimal (explanationOfBenefitItemFactor p))
             , OptProp  "net" (fmap Xmlbf.toXml (explanationOfBenefitItemNet p))
             , PropList "udi" (fmap Xmlbf.toXml (explanationOfBenefitItemUdi p))
             , OptProp  "bodySite" (fmap Xmlbf.toXml (explanationOfBenefitItemBodySite p))
             , PropList "subSite" (fmap Xmlbf.toXml (explanationOfBenefitItemSubSite p))
             , PropList "encounter" (fmap Xmlbf.toXml (explanationOfBenefitItemEncounter p))
             , ValList  "noteNumber" (fmap toPositiveInt (explanationOfBenefitItemNoteNumber p))
             , PropList "adjudication" (fmap Xmlbf.toXml (explanationOfBenefitItemAdjudication p))
             , PropList "detail" (fmap Xmlbf.toXml (explanationOfBenefitItemDetail p))
             ]
       where 
          toServicedXml ( Nothing   ) = (OptVal "serviced" Nothing)
          toServicedXml (Just (ExplanationOfBenefitItemServicedDate p)) = Val   "servicedDate" (toDate p)
          toServicedXml (Just (ExplanationOfBenefitItemServicedPeriod p)) = Prop  "servicedPeriod" (HM.empty, Xmlbf.toXml p)
          toLocationXml ( Nothing   ) = (OptVal "location" Nothing)
          toLocationXml (Just (ExplanationOfBenefitItemLocationCodeableConcept p)) = Prop  "locationCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toLocationXml (Just (ExplanationOfBenefitItemLocationAddress p)) = Prop  "locationAddress" (HM.empty, Xmlbf.toXml p)
          toLocationXml (Just (ExplanationOfBenefitItemLocationReference p)) = Prop  "locationReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml ExplanationOfBenefitItem where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    sequence <-            Xmlbf.pElement "sequence" (Xmlbf.pAttr "value")
    careTeamSequence <- many     $ Xmlbf.pElement "careTeamSequence" (Xmlbf.pAttr "value")
    diagnosisSequence <- many     $ Xmlbf.pElement "diagnosisSequence" (Xmlbf.pAttr "value")
    procedureSequence <- many     $ Xmlbf.pElement "procedureSequence" (Xmlbf.pAttr "value")
    informationSequence <- many     $ Xmlbf.pElement "informationSequence" (Xmlbf.pAttr "value")
    revenue <- optional $ Xmlbf.pElement "revenue" Xmlbf.fromXml
    category <- optional $ Xmlbf.pElement "category" Xmlbf.fromXml
    productOrService <-            Xmlbf.pElement "productOrService" Xmlbf.fromXml
    modifier <- many     $ Xmlbf.pElement "modifier" Xmlbf.fromXml
    programCode <- many     $ Xmlbf.pElement "programCode" Xmlbf.fromXml
    serviced <- fromServicedXml
    location <- fromLocationXml
    quantity <- optional $ Xmlbf.pElement "quantity" Xmlbf.fromXml
    unitPrice <- optional $ Xmlbf.pElement "unitPrice" Xmlbf.fromXml
    factor <- optional $ Xmlbf.pElement "factor" (Xmlbf.pAttr "value")
    net <- optional $ Xmlbf.pElement "net" Xmlbf.fromXml
    udi <- many     $ Xmlbf.pElement "udi" Xmlbf.fromXml
    bodySite <- optional $ Xmlbf.pElement "bodySite" Xmlbf.fromXml
    subSite <- many     $ Xmlbf.pElement "subSite" Xmlbf.fromXml
    encounter <- many     $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    noteNumber <- many     $ Xmlbf.pElement "noteNumber" (Xmlbf.pAttr "value")
    adjudication <- many     $ Xmlbf.pElement "adjudication" Xmlbf.fromXml
    detail <- many     $ Xmlbf.pElement "detail" Xmlbf.fromXml
    return ExplanationOfBenefitItem {
            explanationOfBenefitItemAttrId = id
          , explanationOfBenefitItemExtension = extension
          , explanationOfBenefitItemModifierExtension = modifierExtension
          , explanationOfBenefitItemSequence =      fromPositiveInt sequence
          , explanationOfBenefitItemCareTeamSequence = fmap fromPositiveInt careTeamSequence
          , explanationOfBenefitItemDiagnosisSequence = fmap fromPositiveInt diagnosisSequence
          , explanationOfBenefitItemProcedureSequence = fmap fromPositiveInt procedureSequence
          , explanationOfBenefitItemInformationSequence = fmap fromPositiveInt informationSequence
          , explanationOfBenefitItemRevenue = revenue
          , explanationOfBenefitItemCategory = category
          , explanationOfBenefitItemProductOrService = productOrService
          , explanationOfBenefitItemModifier = modifier
          , explanationOfBenefitItemProgramCode = programCode
          , explanationOfBenefitItemServiced = serviced
          , explanationOfBenefitItemLocation = location
          , explanationOfBenefitItemQuantity = quantity
          , explanationOfBenefitItemUnitPrice = unitPrice
          , explanationOfBenefitItemFactor = fmap fromDecimal factor
          , explanationOfBenefitItemNet = net
          , explanationOfBenefitItemUdi = udi
          , explanationOfBenefitItemBodySite = bodySite
          , explanationOfBenefitItemSubSite = subSite
          , explanationOfBenefitItemEncounter = encounter
          , explanationOfBenefitItemNoteNumber = fmap fromPositiveInt noteNumber
          , explanationOfBenefitItemAdjudication = adjudication
          , explanationOfBenefitItemDetail = detail
          }

    where 
      fromServicedXml = parseServicedDate <|> parseServicedPeriod <|> pure Nothing
      parseServicedDate = do
                has <- Xmlbf.pElement "servicedDate" (Xmlbf.pAttr "value")
                return $ Just (ExplanationOfBenefitItemServicedDate (     toDate has))
      parseServicedPeriod = do
                has <- Xmlbf.pElement "servicedPeriod" Xmlbf.fromXml
                return $ Just (ExplanationOfBenefitItemServicedPeriod (                      has))
      fromLocationXml = parseLocationCodeableConcept <|> parseLocationAddress <|> parseLocationReference <|> pure Nothing
      parseLocationCodeableConcept = do
                has <- Xmlbf.pElement "locationCodeableConcept" Xmlbf.fromXml
                return $ Just (ExplanationOfBenefitItemLocationCodeableConcept (                      has))
      parseLocationAddress = do
                has <- Xmlbf.pElement "locationAddress" Xmlbf.fromXml
                return $ Just (ExplanationOfBenefitItemLocationAddress (                      has))
      parseLocationReference = do
                has <- Xmlbf.pElement "locationReference" Xmlbf.fromXml
                return $ Just (ExplanationOfBenefitItemLocationReference (                      has))


data ExplanationOfBenefitSubDetail1 = ExplanationOfBenefitSubDetail1 {
    explanationOfBenefitSubDetail1AttrId :: Maybe Text
  , explanationOfBenefitSubDetail1Extension :: [Extension]
  , explanationOfBenefitSubDetail1ModifierExtension :: [Extension]
  , explanationOfBenefitSubDetail1ProductOrService :: CodeableConcept
  , explanationOfBenefitSubDetail1Modifier :: [CodeableConcept]
  , explanationOfBenefitSubDetail1Quantity :: Maybe Quantity
  , explanationOfBenefitSubDetail1UnitPrice :: Maybe Money
  , explanationOfBenefitSubDetail1Factor :: Maybe Decimal
  , explanationOfBenefitSubDetail1Net :: Maybe Money
  , explanationOfBenefitSubDetail1NoteNumber :: [PositiveInt]
  , explanationOfBenefitSubDetail1Adjudication :: [ExplanationOfBenefitAdjudication]
  }
--

instance ToJSON ExplanationOfBenefitSubDetail1 where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitSubDetail1AttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitSubDetail1Extension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitSubDetail1ModifierExtension p)
    ,  "productOrService" .= toJSON (explanationOfBenefitSubDetail1ProductOrService p)
    ,  "modifier" .= toJSON (explanationOfBenefitSubDetail1Modifier p)
    ,  "quantity" .= toJSON (explanationOfBenefitSubDetail1Quantity p)
    ,  "unitPrice" .= toJSON (explanationOfBenefitSubDetail1UnitPrice p)
    ,  "factor" .= toJSON (explanationOfBenefitSubDetail1Factor p)
    ,  "net" .= toJSON (explanationOfBenefitSubDetail1Net p)
    ,  "noteNumber" .= toJSON (explanationOfBenefitSubDetail1NoteNumber p)
    ,  "adjudication" .= toJSON (explanationOfBenefitSubDetail1Adjudication p)
    ]
instance FromJSON ExplanationOfBenefitSubDetail1 where
  parseJSON = withObject "ExplanationOfBenefitSubDetail1" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        productOrService <- o .:  "productOrService"
        modifier <- o .:? "modifier" .!= []
        quantity <- o .:? "quantity"
        unitPrice <- o .:? "unitPrice"
        factor <- o .:? "factor"
        net <- o .:? "net"
        noteNumber <- o .:? "noteNumber" .!= []
        adjudication <- o .:? "adjudication" .!= []
        return ExplanationOfBenefitSubDetail1{
            explanationOfBenefitSubDetail1AttrId = id
          , explanationOfBenefitSubDetail1Extension = extension
          , explanationOfBenefitSubDetail1ModifierExtension = modifierExtension
          , explanationOfBenefitSubDetail1ProductOrService = productOrService
          , explanationOfBenefitSubDetail1Modifier = modifier
          , explanationOfBenefitSubDetail1Quantity = quantity
          , explanationOfBenefitSubDetail1UnitPrice = unitPrice
          , explanationOfBenefitSubDetail1Factor = factor
          , explanationOfBenefitSubDetail1Net = net
          , explanationOfBenefitSubDetail1NoteNumber = noteNumber
          , explanationOfBenefitSubDetail1Adjudication = adjudication
          }
instance Xmlbf.ToXml ExplanationOfBenefitSubDetail1 where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitSubDetail1AttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitSubDetail1Extension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitSubDetail1ModifierExtension p))
             , Prop     "productOrService" (HM.empty, Xmlbf.toXml (explanationOfBenefitSubDetail1ProductOrService p))
             , PropList "modifier" (fmap Xmlbf.toXml (explanationOfBenefitSubDetail1Modifier p))
             , OptProp  "quantity" (fmap Xmlbf.toXml (explanationOfBenefitSubDetail1Quantity p))
             , OptProp  "unitPrice" (fmap Xmlbf.toXml (explanationOfBenefitSubDetail1UnitPrice p))
             , OptVal   "factor" (fmap toDecimal (explanationOfBenefitSubDetail1Factor p))
             , OptProp  "net" (fmap Xmlbf.toXml (explanationOfBenefitSubDetail1Net p))
             , ValList  "noteNumber" (fmap toPositiveInt (explanationOfBenefitSubDetail1NoteNumber p))
             , PropList "adjudication" (fmap Xmlbf.toXml (explanationOfBenefitSubDetail1Adjudication p))
             ]
instance Xmlbf.FromXml ExplanationOfBenefitSubDetail1 where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    productOrService <-            Xmlbf.pElement "productOrService" Xmlbf.fromXml
    modifier <- many     $ Xmlbf.pElement "modifier" Xmlbf.fromXml
    quantity <- optional $ Xmlbf.pElement "quantity" Xmlbf.fromXml
    unitPrice <- optional $ Xmlbf.pElement "unitPrice" Xmlbf.fromXml
    factor <- optional $ Xmlbf.pElement "factor" (Xmlbf.pAttr "value")
    net <- optional $ Xmlbf.pElement "net" Xmlbf.fromXml
    noteNumber <- many     $ Xmlbf.pElement "noteNumber" (Xmlbf.pAttr "value")
    adjudication <- many     $ Xmlbf.pElement "adjudication" Xmlbf.fromXml
    return ExplanationOfBenefitSubDetail1 {
            explanationOfBenefitSubDetail1AttrId = id
          , explanationOfBenefitSubDetail1Extension = extension
          , explanationOfBenefitSubDetail1ModifierExtension = modifierExtension
          , explanationOfBenefitSubDetail1ProductOrService = productOrService
          , explanationOfBenefitSubDetail1Modifier = modifier
          , explanationOfBenefitSubDetail1Quantity = quantity
          , explanationOfBenefitSubDetail1UnitPrice = unitPrice
          , explanationOfBenefitSubDetail1Factor = fmap fromDecimal factor
          , explanationOfBenefitSubDetail1Net = net
          , explanationOfBenefitSubDetail1NoteNumber = fmap fromPositiveInt noteNumber
          , explanationOfBenefitSubDetail1Adjudication = adjudication
          }



data ExplanationOfBenefitSubDetail = ExplanationOfBenefitSubDetail {
    explanationOfBenefitSubDetailAttrId :: Maybe Text
  , explanationOfBenefitSubDetailExtension :: [Extension]
  , explanationOfBenefitSubDetailModifierExtension :: [Extension]
  , explanationOfBenefitSubDetailSequence :: PositiveInt
  , explanationOfBenefitSubDetailRevenue :: Maybe CodeableConcept
  , explanationOfBenefitSubDetailCategory :: Maybe CodeableConcept
  , explanationOfBenefitSubDetailProductOrService :: CodeableConcept
  , explanationOfBenefitSubDetailModifier :: [CodeableConcept]
  , explanationOfBenefitSubDetailProgramCode :: [CodeableConcept]
  , explanationOfBenefitSubDetailQuantity :: Maybe Quantity
  , explanationOfBenefitSubDetailUnitPrice :: Maybe Money
  , explanationOfBenefitSubDetailFactor :: Maybe Decimal
  , explanationOfBenefitSubDetailNet :: Maybe Money
  , explanationOfBenefitSubDetailUdi :: [Reference]
  , explanationOfBenefitSubDetailNoteNumber :: [PositiveInt]
  , explanationOfBenefitSubDetailAdjudication :: [ExplanationOfBenefitAdjudication]
  }
--

instance ToJSON ExplanationOfBenefitSubDetail where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitSubDetailAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitSubDetailExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitSubDetailModifierExtension p)
    ,  "sequence" .= toJSON (explanationOfBenefitSubDetailSequence p)
    ,  "revenue" .= toJSON (explanationOfBenefitSubDetailRevenue p)
    ,  "category" .= toJSON (explanationOfBenefitSubDetailCategory p)
    ,  "productOrService" .= toJSON (explanationOfBenefitSubDetailProductOrService p)
    ,  "modifier" .= toJSON (explanationOfBenefitSubDetailModifier p)
    ,  "programCode" .= toJSON (explanationOfBenefitSubDetailProgramCode p)
    ,  "quantity" .= toJSON (explanationOfBenefitSubDetailQuantity p)
    ,  "unitPrice" .= toJSON (explanationOfBenefitSubDetailUnitPrice p)
    ,  "factor" .= toJSON (explanationOfBenefitSubDetailFactor p)
    ,  "net" .= toJSON (explanationOfBenefitSubDetailNet p)
    ,  "udi" .= toJSON (explanationOfBenefitSubDetailUdi p)
    ,  "noteNumber" .= toJSON (explanationOfBenefitSubDetailNoteNumber p)
    ,  "adjudication" .= toJSON (explanationOfBenefitSubDetailAdjudication p)
    ]
instance FromJSON ExplanationOfBenefitSubDetail where
  parseJSON = withObject "ExplanationOfBenefitSubDetail" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        sequence <- o .:  "sequence"
        revenue <- o .:? "revenue"
        category <- o .:? "category"
        productOrService <- o .:  "productOrService"
        modifier <- o .:? "modifier" .!= []
        programCode <- o .:? "programCode" .!= []
        quantity <- o .:? "quantity"
        unitPrice <- o .:? "unitPrice"
        factor <- o .:? "factor"
        net <- o .:? "net"
        udi <- o .:? "udi" .!= []
        noteNumber <- o .:? "noteNumber" .!= []
        adjudication <- o .:? "adjudication" .!= []
        return ExplanationOfBenefitSubDetail{
            explanationOfBenefitSubDetailAttrId = id
          , explanationOfBenefitSubDetailExtension = extension
          , explanationOfBenefitSubDetailModifierExtension = modifierExtension
          , explanationOfBenefitSubDetailSequence = sequence
          , explanationOfBenefitSubDetailRevenue = revenue
          , explanationOfBenefitSubDetailCategory = category
          , explanationOfBenefitSubDetailProductOrService = productOrService
          , explanationOfBenefitSubDetailModifier = modifier
          , explanationOfBenefitSubDetailProgramCode = programCode
          , explanationOfBenefitSubDetailQuantity = quantity
          , explanationOfBenefitSubDetailUnitPrice = unitPrice
          , explanationOfBenefitSubDetailFactor = factor
          , explanationOfBenefitSubDetailNet = net
          , explanationOfBenefitSubDetailUdi = udi
          , explanationOfBenefitSubDetailNoteNumber = noteNumber
          , explanationOfBenefitSubDetailAdjudication = adjudication
          }
instance Xmlbf.ToXml ExplanationOfBenefitSubDetail where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitSubDetailAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitSubDetailExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitSubDetailModifierExtension p))
             , Val      "sequence" (     toPositiveInt (explanationOfBenefitSubDetailSequence p))
             , OptProp  "revenue" (fmap Xmlbf.toXml (explanationOfBenefitSubDetailRevenue p))
             , OptProp  "category" (fmap Xmlbf.toXml (explanationOfBenefitSubDetailCategory p))
             , Prop     "productOrService" (HM.empty, Xmlbf.toXml (explanationOfBenefitSubDetailProductOrService p))
             , PropList "modifier" (fmap Xmlbf.toXml (explanationOfBenefitSubDetailModifier p))
             , PropList "programCode" (fmap Xmlbf.toXml (explanationOfBenefitSubDetailProgramCode p))
             , OptProp  "quantity" (fmap Xmlbf.toXml (explanationOfBenefitSubDetailQuantity p))
             , OptProp  "unitPrice" (fmap Xmlbf.toXml (explanationOfBenefitSubDetailUnitPrice p))
             , OptVal   "factor" (fmap toDecimal (explanationOfBenefitSubDetailFactor p))
             , OptProp  "net" (fmap Xmlbf.toXml (explanationOfBenefitSubDetailNet p))
             , PropList "udi" (fmap Xmlbf.toXml (explanationOfBenefitSubDetailUdi p))
             , ValList  "noteNumber" (fmap toPositiveInt (explanationOfBenefitSubDetailNoteNumber p))
             , PropList "adjudication" (fmap Xmlbf.toXml (explanationOfBenefitSubDetailAdjudication p))
             ]
instance Xmlbf.FromXml ExplanationOfBenefitSubDetail where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    sequence <-            Xmlbf.pElement "sequence" (Xmlbf.pAttr "value")
    revenue <- optional $ Xmlbf.pElement "revenue" Xmlbf.fromXml
    category <- optional $ Xmlbf.pElement "category" Xmlbf.fromXml
    productOrService <-            Xmlbf.pElement "productOrService" Xmlbf.fromXml
    modifier <- many     $ Xmlbf.pElement "modifier" Xmlbf.fromXml
    programCode <- many     $ Xmlbf.pElement "programCode" Xmlbf.fromXml
    quantity <- optional $ Xmlbf.pElement "quantity" Xmlbf.fromXml
    unitPrice <- optional $ Xmlbf.pElement "unitPrice" Xmlbf.fromXml
    factor <- optional $ Xmlbf.pElement "factor" (Xmlbf.pAttr "value")
    net <- optional $ Xmlbf.pElement "net" Xmlbf.fromXml
    udi <- many     $ Xmlbf.pElement "udi" Xmlbf.fromXml
    noteNumber <- many     $ Xmlbf.pElement "noteNumber" (Xmlbf.pAttr "value")
    adjudication <- many     $ Xmlbf.pElement "adjudication" Xmlbf.fromXml
    return ExplanationOfBenefitSubDetail {
            explanationOfBenefitSubDetailAttrId = id
          , explanationOfBenefitSubDetailExtension = extension
          , explanationOfBenefitSubDetailModifierExtension = modifierExtension
          , explanationOfBenefitSubDetailSequence =      fromPositiveInt sequence
          , explanationOfBenefitSubDetailRevenue = revenue
          , explanationOfBenefitSubDetailCategory = category
          , explanationOfBenefitSubDetailProductOrService = productOrService
          , explanationOfBenefitSubDetailModifier = modifier
          , explanationOfBenefitSubDetailProgramCode = programCode
          , explanationOfBenefitSubDetailQuantity = quantity
          , explanationOfBenefitSubDetailUnitPrice = unitPrice
          , explanationOfBenefitSubDetailFactor = fmap fromDecimal factor
          , explanationOfBenefitSubDetailNet = net
          , explanationOfBenefitSubDetailUdi = udi
          , explanationOfBenefitSubDetailNoteNumber = fmap fromPositiveInt noteNumber
          , explanationOfBenefitSubDetailAdjudication = adjudication
          }



data ExplanationOfBenefitDetail1 = ExplanationOfBenefitDetail1 {
    explanationOfBenefitDetail1AttrId :: Maybe Text
  , explanationOfBenefitDetail1Extension :: [Extension]
  , explanationOfBenefitDetail1ModifierExtension :: [Extension]
  , explanationOfBenefitDetail1ProductOrService :: CodeableConcept
  , explanationOfBenefitDetail1Modifier :: [CodeableConcept]
  , explanationOfBenefitDetail1Quantity :: Maybe Quantity
  , explanationOfBenefitDetail1UnitPrice :: Maybe Money
  , explanationOfBenefitDetail1Factor :: Maybe Decimal
  , explanationOfBenefitDetail1Net :: Maybe Money
  , explanationOfBenefitDetail1NoteNumber :: [PositiveInt]
  , explanationOfBenefitDetail1Adjudication :: [ExplanationOfBenefitAdjudication]
  , explanationOfBenefitDetail1SubDetail :: [ExplanationOfBenefitSubDetail1]
  }
--

instance ToJSON ExplanationOfBenefitDetail1 where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitDetail1AttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitDetail1Extension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitDetail1ModifierExtension p)
    ,  "productOrService" .= toJSON (explanationOfBenefitDetail1ProductOrService p)
    ,  "modifier" .= toJSON (explanationOfBenefitDetail1Modifier p)
    ,  "quantity" .= toJSON (explanationOfBenefitDetail1Quantity p)
    ,  "unitPrice" .= toJSON (explanationOfBenefitDetail1UnitPrice p)
    ,  "factor" .= toJSON (explanationOfBenefitDetail1Factor p)
    ,  "net" .= toJSON (explanationOfBenefitDetail1Net p)
    ,  "noteNumber" .= toJSON (explanationOfBenefitDetail1NoteNumber p)
    ,  "adjudication" .= toJSON (explanationOfBenefitDetail1Adjudication p)
    ,  "subDetail" .= toJSON (explanationOfBenefitDetail1SubDetail p)
    ]
instance FromJSON ExplanationOfBenefitDetail1 where
  parseJSON = withObject "ExplanationOfBenefitDetail1" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        productOrService <- o .:  "productOrService"
        modifier <- o .:? "modifier" .!= []
        quantity <- o .:? "quantity"
        unitPrice <- o .:? "unitPrice"
        factor <- o .:? "factor"
        net <- o .:? "net"
        noteNumber <- o .:? "noteNumber" .!= []
        adjudication <- o .:? "adjudication" .!= []
        subDetail <- o .:? "subDetail" .!= []
        return ExplanationOfBenefitDetail1{
            explanationOfBenefitDetail1AttrId = id
          , explanationOfBenefitDetail1Extension = extension
          , explanationOfBenefitDetail1ModifierExtension = modifierExtension
          , explanationOfBenefitDetail1ProductOrService = productOrService
          , explanationOfBenefitDetail1Modifier = modifier
          , explanationOfBenefitDetail1Quantity = quantity
          , explanationOfBenefitDetail1UnitPrice = unitPrice
          , explanationOfBenefitDetail1Factor = factor
          , explanationOfBenefitDetail1Net = net
          , explanationOfBenefitDetail1NoteNumber = noteNumber
          , explanationOfBenefitDetail1Adjudication = adjudication
          , explanationOfBenefitDetail1SubDetail = subDetail
          }
instance Xmlbf.ToXml ExplanationOfBenefitDetail1 where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitDetail1AttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitDetail1Extension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitDetail1ModifierExtension p))
             , Prop     "productOrService" (HM.empty, Xmlbf.toXml (explanationOfBenefitDetail1ProductOrService p))
             , PropList "modifier" (fmap Xmlbf.toXml (explanationOfBenefitDetail1Modifier p))
             , OptProp  "quantity" (fmap Xmlbf.toXml (explanationOfBenefitDetail1Quantity p))
             , OptProp  "unitPrice" (fmap Xmlbf.toXml (explanationOfBenefitDetail1UnitPrice p))
             , OptVal   "factor" (fmap toDecimal (explanationOfBenefitDetail1Factor p))
             , OptProp  "net" (fmap Xmlbf.toXml (explanationOfBenefitDetail1Net p))
             , ValList  "noteNumber" (fmap toPositiveInt (explanationOfBenefitDetail1NoteNumber p))
             , PropList "adjudication" (fmap Xmlbf.toXml (explanationOfBenefitDetail1Adjudication p))
             , PropList "subDetail" (fmap Xmlbf.toXml (explanationOfBenefitDetail1SubDetail p))
             ]
instance Xmlbf.FromXml ExplanationOfBenefitDetail1 where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    productOrService <-            Xmlbf.pElement "productOrService" Xmlbf.fromXml
    modifier <- many     $ Xmlbf.pElement "modifier" Xmlbf.fromXml
    quantity <- optional $ Xmlbf.pElement "quantity" Xmlbf.fromXml
    unitPrice <- optional $ Xmlbf.pElement "unitPrice" Xmlbf.fromXml
    factor <- optional $ Xmlbf.pElement "factor" (Xmlbf.pAttr "value")
    net <- optional $ Xmlbf.pElement "net" Xmlbf.fromXml
    noteNumber <- many     $ Xmlbf.pElement "noteNumber" (Xmlbf.pAttr "value")
    adjudication <- many     $ Xmlbf.pElement "adjudication" Xmlbf.fromXml
    subDetail <- many     $ Xmlbf.pElement "subDetail" Xmlbf.fromXml
    return ExplanationOfBenefitDetail1 {
            explanationOfBenefitDetail1AttrId = id
          , explanationOfBenefitDetail1Extension = extension
          , explanationOfBenefitDetail1ModifierExtension = modifierExtension
          , explanationOfBenefitDetail1ProductOrService = productOrService
          , explanationOfBenefitDetail1Modifier = modifier
          , explanationOfBenefitDetail1Quantity = quantity
          , explanationOfBenefitDetail1UnitPrice = unitPrice
          , explanationOfBenefitDetail1Factor = fmap fromDecimal factor
          , explanationOfBenefitDetail1Net = net
          , explanationOfBenefitDetail1NoteNumber = fmap fromPositiveInt noteNumber
          , explanationOfBenefitDetail1Adjudication = adjudication
          , explanationOfBenefitDetail1SubDetail = subDetail
          }



data ExplanationOfBenefitBenefitBalance = ExplanationOfBenefitBenefitBalance {
    explanationOfBenefitBenefitBalanceAttrId :: Maybe Text
  , explanationOfBenefitBenefitBalanceExtension :: [Extension]
  , explanationOfBenefitBenefitBalanceModifierExtension :: [Extension]
  , explanationOfBenefitBenefitBalanceCategory :: CodeableConcept
  , explanationOfBenefitBenefitBalanceExcluded :: Maybe Boolean
  , explanationOfBenefitBenefitBalanceName :: Maybe Text
  , explanationOfBenefitBenefitBalanceDescription :: Maybe Text
  , explanationOfBenefitBenefitBalanceNetwork :: Maybe CodeableConcept
  , explanationOfBenefitBenefitBalanceUnit :: Maybe CodeableConcept
  , explanationOfBenefitBenefitBalanceTerm :: Maybe CodeableConcept
  , explanationOfBenefitBenefitBalanceFinancial :: [ExplanationOfBenefitFinancial]
  }
--

instance ToJSON ExplanationOfBenefitBenefitBalance where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitBenefitBalanceAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitBenefitBalanceExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitBenefitBalanceModifierExtension p)
    ,  "category" .= toJSON (explanationOfBenefitBenefitBalanceCategory p)
    ,  "excluded" .= toJSON (explanationOfBenefitBenefitBalanceExcluded p)
    ,  "name" .= toJSON (explanationOfBenefitBenefitBalanceName p)
    ,  "description" .= toJSON (explanationOfBenefitBenefitBalanceDescription p)
    ,  "network" .= toJSON (explanationOfBenefitBenefitBalanceNetwork p)
    ,  "unit" .= toJSON (explanationOfBenefitBenefitBalanceUnit p)
    ,  "term" .= toJSON (explanationOfBenefitBenefitBalanceTerm p)
    ,  "financial" .= toJSON (explanationOfBenefitBenefitBalanceFinancial p)
    ]
instance FromJSON ExplanationOfBenefitBenefitBalance where
  parseJSON = withObject "ExplanationOfBenefitBenefitBalance" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        category <- o .:  "category"
        excluded <- o .:? "excluded"
        name <- o .:? "name"
        description <- o .:? "description"
        network <- o .:? "network"
        unit <- o .:? "unit"
        term <- o .:? "term"
        financial <- o .:? "financial" .!= []
        return ExplanationOfBenefitBenefitBalance{
            explanationOfBenefitBenefitBalanceAttrId = id
          , explanationOfBenefitBenefitBalanceExtension = extension
          , explanationOfBenefitBenefitBalanceModifierExtension = modifierExtension
          , explanationOfBenefitBenefitBalanceCategory = category
          , explanationOfBenefitBenefitBalanceExcluded = excluded
          , explanationOfBenefitBenefitBalanceName = name
          , explanationOfBenefitBenefitBalanceDescription = description
          , explanationOfBenefitBenefitBalanceNetwork = network
          , explanationOfBenefitBenefitBalanceUnit = unit
          , explanationOfBenefitBenefitBalanceTerm = term
          , explanationOfBenefitBenefitBalanceFinancial = financial
          }
instance Xmlbf.ToXml ExplanationOfBenefitBenefitBalance where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitBenefitBalanceAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitBenefitBalanceExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitBenefitBalanceModifierExtension p))
             , Prop     "category" (HM.empty, Xmlbf.toXml (explanationOfBenefitBenefitBalanceCategory p))
             , OptVal   "excluded" (fmap toBoolean (explanationOfBenefitBenefitBalanceExcluded p))
             , OptVal   "name" (fmap toString (explanationOfBenefitBenefitBalanceName p))
             , OptVal   "description" (fmap toString (explanationOfBenefitBenefitBalanceDescription p))
             , OptProp  "network" (fmap Xmlbf.toXml (explanationOfBenefitBenefitBalanceNetwork p))
             , OptProp  "unit" (fmap Xmlbf.toXml (explanationOfBenefitBenefitBalanceUnit p))
             , OptProp  "term" (fmap Xmlbf.toXml (explanationOfBenefitBenefitBalanceTerm p))
             , PropList "financial" (fmap Xmlbf.toXml (explanationOfBenefitBenefitBalanceFinancial p))
             ]
instance Xmlbf.FromXml ExplanationOfBenefitBenefitBalance where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    category <-            Xmlbf.pElement "category" Xmlbf.fromXml
    excluded <- optional $ Xmlbf.pElement "excluded" (Xmlbf.pAttr "value")
    name <- optional $ Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    network <- optional $ Xmlbf.pElement "network" Xmlbf.fromXml
    unit <- optional $ Xmlbf.pElement "unit" Xmlbf.fromXml
    term <- optional $ Xmlbf.pElement "term" Xmlbf.fromXml
    financial <- many     $ Xmlbf.pElement "financial" Xmlbf.fromXml
    return ExplanationOfBenefitBenefitBalance {
            explanationOfBenefitBenefitBalanceAttrId = id
          , explanationOfBenefitBenefitBalanceExtension = extension
          , explanationOfBenefitBenefitBalanceModifierExtension = modifierExtension
          , explanationOfBenefitBenefitBalanceCategory = category
          , explanationOfBenefitBenefitBalanceExcluded = fmap fromBoolean excluded
          , explanationOfBenefitBenefitBalanceName = fmap fromString name
          , explanationOfBenefitBenefitBalanceDescription = fmap fromString description
          , explanationOfBenefitBenefitBalanceNetwork = network
          , explanationOfBenefitBenefitBalanceUnit = unit
          , explanationOfBenefitBenefitBalanceTerm = term
          , explanationOfBenefitBenefitBalanceFinancial = financial
          }



data ExplanationOfBenefitCareTeam = ExplanationOfBenefitCareTeam {
    explanationOfBenefitCareTeamAttrId :: Maybe Text
  , explanationOfBenefitCareTeamExtension :: [Extension]
  , explanationOfBenefitCareTeamModifierExtension :: [Extension]
  , explanationOfBenefitCareTeamSequence :: PositiveInt
  , explanationOfBenefitCareTeamProvider :: Reference
  , explanationOfBenefitCareTeamResponsible :: Maybe Boolean
  , explanationOfBenefitCareTeamRole :: Maybe CodeableConcept
  , explanationOfBenefitCareTeamQualification :: Maybe CodeableConcept
  }
--

instance ToJSON ExplanationOfBenefitCareTeam where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (explanationOfBenefitCareTeamAttrId p)
    ,  "extension" .= toJSON (explanationOfBenefitCareTeamExtension p)
    ,  "modifierExtension" .= toJSON (explanationOfBenefitCareTeamModifierExtension p)
    ,  "sequence" .= toJSON (explanationOfBenefitCareTeamSequence p)
    ,  "provider" .= toJSON (explanationOfBenefitCareTeamProvider p)
    ,  "responsible" .= toJSON (explanationOfBenefitCareTeamResponsible p)
    ,  "role" .= toJSON (explanationOfBenefitCareTeamRole p)
    ,  "qualification" .= toJSON (explanationOfBenefitCareTeamQualification p)
    ]
instance FromJSON ExplanationOfBenefitCareTeam where
  parseJSON = withObject "ExplanationOfBenefitCareTeam" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        sequence <- o .:  "sequence"
        provider <- o .:  "provider"
        responsible <- o .:? "responsible"
        role <- o .:? "role"
        qualification <- o .:? "qualification"
        return ExplanationOfBenefitCareTeam{
            explanationOfBenefitCareTeamAttrId = id
          , explanationOfBenefitCareTeamExtension = extension
          , explanationOfBenefitCareTeamModifierExtension = modifierExtension
          , explanationOfBenefitCareTeamSequence = sequence
          , explanationOfBenefitCareTeamProvider = provider
          , explanationOfBenefitCareTeamResponsible = responsible
          , explanationOfBenefitCareTeamRole = role
          , explanationOfBenefitCareTeamQualification = qualification
          }
instance Xmlbf.ToXml ExplanationOfBenefitCareTeam where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (explanationOfBenefitCareTeamAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (explanationOfBenefitCareTeamExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (explanationOfBenefitCareTeamModifierExtension p))
             , Val      "sequence" (     toPositiveInt (explanationOfBenefitCareTeamSequence p))
             , Prop     "provider" (HM.empty, Xmlbf.toXml (explanationOfBenefitCareTeamProvider p))
             , OptVal   "responsible" (fmap toBoolean (explanationOfBenefitCareTeamResponsible p))
             , OptProp  "role" (fmap Xmlbf.toXml (explanationOfBenefitCareTeamRole p))
             , OptProp  "qualification" (fmap Xmlbf.toXml (explanationOfBenefitCareTeamQualification p))
             ]
instance Xmlbf.FromXml ExplanationOfBenefitCareTeam where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    sequence <-            Xmlbf.pElement "sequence" (Xmlbf.pAttr "value")
    provider <-            Xmlbf.pElement "provider" Xmlbf.fromXml
    responsible <- optional $ Xmlbf.pElement "responsible" (Xmlbf.pAttr "value")
    role <- optional $ Xmlbf.pElement "role" Xmlbf.fromXml
    qualification <- optional $ Xmlbf.pElement "qualification" Xmlbf.fromXml
    return ExplanationOfBenefitCareTeam {
            explanationOfBenefitCareTeamAttrId = id
          , explanationOfBenefitCareTeamExtension = extension
          , explanationOfBenefitCareTeamModifierExtension = modifierExtension
          , explanationOfBenefitCareTeamSequence =      fromPositiveInt sequence
          , explanationOfBenefitCareTeamProvider = provider
          , explanationOfBenefitCareTeamResponsible = fmap fromBoolean responsible
          , explanationOfBenefitCareTeamRole = role
          , explanationOfBenefitCareTeamQualification = qualification
          }




