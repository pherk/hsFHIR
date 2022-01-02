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
-- FHIR 4.0.0 Claim
--

module Data.FHIR.Resources.Claim where

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

data ClaimStatus
    = CSActive
    | CSCancelled
    | CSDraft
    | CSEnteredInError
  deriving (Eq, Show)

instance ToJSON ClaimStatus where
    toJSON CSActive = String "active"
    toJSON CSCancelled = String "cancelled"
    toJSON CSDraft = String "draft"
    toJSON CSEnteredInError = String "entered-in-error"
instance FromJSON ClaimStatus where
    parseJSON "active" = return CSActive
    parseJSON "cancelled" = return CSCancelled
    parseJSON "draft" = return CSDraft
    parseJSON "entered-in-error" = return CSEnteredInError

toClaimStatus CSActive = "active"
toClaimStatus CSCancelled = "cancelled"
toClaimStatus CSDraft = "draft"
toClaimStatus CSEnteredInError = "entered-in-error"
fromClaimStatus "active" = CSActive
fromClaimStatus "cancelled" = CSCancelled
fromClaimStatus "draft" = CSDraft
fromClaimStatus "entered-in-error" = CSEnteredInError


data ClaimUse
    = CUClaim
    | CUPreauthorization
    | CUPredetermination
  deriving (Eq, Show)

instance ToJSON ClaimUse where
    toJSON CUClaim = String "claim"
    toJSON CUPreauthorization = String "preauthorization"
    toJSON CUPredetermination = String "predetermination"
instance FromJSON ClaimUse where
    parseJSON "claim" = return CUClaim
    parseJSON "preauthorization" = return CUPreauthorization
    parseJSON "predetermination" = return CUPredetermination

toClaimUse CUClaim = "claim"
toClaimUse CUPreauthorization = "preauthorization"
toClaimUse CUPredetermination = "predetermination"
fromClaimUse "claim" = CUClaim
fromClaimUse "preauthorization" = CUPreauthorization
fromClaimUse "predetermination" = CUPredetermination


data Claim = Claim {
    claimId :: Maybe Id
  , claimMeta :: Maybe Meta
  , claimImplicitRules :: Maybe Uri
  , claimLanguage :: Maybe Language
  , claimText :: Maybe Narrative
--    claimContained :: [ResourceContainer]
  , claimExtension :: [Extension]
  , claimModifierExtension :: [Extension]
  , claimIdentifier :: [Identifier]
  , claimStatus :: ClaimStatus
  , claimType :: CodeableConcept
  , claimSubType :: Maybe CodeableConcept
  , claimUse :: ClaimUse
  , claimPatient :: Reference
  , claimBillablePeriod :: Maybe Period
  , claimCreated :: DateTime
  , claimEnterer :: Maybe Reference
  , claimInsurer :: Maybe Reference
  , claimProvider :: Reference
  , claimPriority :: CodeableConcept
  , claimFundsReserve :: Maybe CodeableConcept
  , claimRelated :: [ClaimRelated]
  , claimPrescription :: Maybe Reference
  , claimOriginalPrescription :: Maybe Reference
  , claimPayee :: Maybe ClaimPayee
  , claimReferral :: Maybe Reference
  , claimFacility :: Maybe Reference
  , claimCareTeam :: [ClaimCareTeam]
  , claimSupportingInfo :: [ClaimSupportingInfo]
  , claimDiagnosis :: [ClaimDiagnosis]
  , claimProcedure :: [ClaimProcedure]
  , claimInsurance :: [ClaimInsurance]
  , claimAccident :: Maybe ClaimAccident
  , claimItem :: [ClaimItem]
  , claimTotal :: Maybe Money
  }
--

instance ToJSON Claim where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Claim")
    ,  "id" .= toJSON (claimId p)
    ,  "meta" .= toJSON (claimMeta p)
    ,  "implicitRules" .= toJSON (claimImplicitRules p)
    ,  "language" .= toJSON (claimLanguage p)
    ,  "text" .= toJSON (claimText p)
--    , "contained" .= toJSON (claimContained p)
    ,  "extension" .= toJSON (claimExtension p)
    ,  "modifierExtension" .= toJSON (claimModifierExtension p)
    ,  "identifier" .= toJSON (claimIdentifier p)
    ,  "status" .= toJSON (claimStatus p)
    ,  "type" .= toJSON (claimType p)
    ,  "subType" .= toJSON (claimSubType p)
    ,  "use" .= toJSON (claimUse p)
    ,  "patient" .= toJSON (claimPatient p)
    ,  "billablePeriod" .= toJSON (claimBillablePeriod p)
    ,  "created" .= toJSON (claimCreated p)
    ,  "enterer" .= toJSON (claimEnterer p)
    ,  "insurer" .= toJSON (claimInsurer p)
    ,  "provider" .= toJSON (claimProvider p)
    ,  "priority" .= toJSON (claimPriority p)
    ,  "fundsReserve" .= toJSON (claimFundsReserve p)
    ,  "related" .= toJSON (claimRelated p)
    ,  "prescription" .= toJSON (claimPrescription p)
    ,  "originalPrescription" .= toJSON (claimOriginalPrescription p)
    ,  "payee" .= toJSON (claimPayee p)
    ,  "referral" .= toJSON (claimReferral p)
    ,  "facility" .= toJSON (claimFacility p)
    ,  "careTeam" .= toJSON (claimCareTeam p)
    ,  "supportingInfo" .= toJSON (claimSupportingInfo p)
    ,  "diagnosis" .= toJSON (claimDiagnosis p)
    ,  "procedure" .= toJSON (claimProcedure p)
    ,  "insurance" .= toJSON (claimInsurance p)
    ,  "accident" .= toJSON (claimAccident p)
    ,  "item" .= toJSON (claimItem p)
    ,  "total" .= toJSON (claimTotal p)
    ]
instance FromJSON Claim where
  parseJSON = withObject "Claim" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Claim" -> do
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
        insurer <- o .:? "insurer"
        provider <- o .:  "provider"
        priority <- o .:  "priority"
        fundsReserve <- o .:? "fundsReserve"
        related <- o .:? "related" .!= []
        prescription <- o .:? "prescription"
        originalPrescription <- o .:? "originalPrescription"
        payee <- o .:? "payee"
        referral <- o .:? "referral"
        facility <- o .:? "facility"
        careTeam <- o .:? "careTeam" .!= []
        supportingInfo <- o .:? "supportingInfo" .!= []
        diagnosis <- o .:? "diagnosis" .!= []
        procedure <- o .:? "procedure" .!= []
        insurance <- o .:? "insurance" .!= []
        accident <- o .:? "accident"
        item <- o .:? "item" .!= []
        total <- o .:? "total"
        return Claim{
            claimId = id
          , claimMeta = meta
          , claimImplicitRules = implicitRules
          , claimLanguage = language
          , claimText = text
--          , claimContained = contained
          , claimExtension = extension
          , claimModifierExtension = modifierExtension
          , claimIdentifier = identifier
          , claimStatus = status
          , claimType = ty
          , claimSubType = subType
          , claimUse = use
          , claimPatient = patient
          , claimBillablePeriod = billablePeriod
          , claimCreated = created
          , claimEnterer = enterer
          , claimInsurer = insurer
          , claimProvider = provider
          , claimPriority = priority
          , claimFundsReserve = fundsReserve
          , claimRelated = related
          , claimPrescription = prescription
          , claimOriginalPrescription = originalPrescription
          , claimPayee = payee
          , claimReferral = referral
          , claimFacility = facility
          , claimCareTeam = careTeam
          , claimSupportingInfo = supportingInfo
          , claimDiagnosis = diagnosis
          , claimProcedure = procedure
          , claimInsurance = insurance
          , claimAccident = accident
          , claimItem = item
          , claimTotal = total
          }
      _ -> fail "not a Claim"
instance Xmlbf.ToXml Claim where
  toXml p = Xmlbf.element "Claim" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (claimId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (claimMeta p))
             , OptVal   "implicitRules" (fmap toUri (claimImplicitRules p))
             , OptVal   "language" (fmap toLanguage (claimLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (claimText p))
--             , PropList "contained" (fmap Xmlbf.toXml (claimContained p))
             , PropList "extension" (fmap Xmlbf.toXml (claimExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (claimIdentifier p))
             , Val      "status" (     toClaimStatus (claimStatus p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (claimType p))
             , OptProp  "subType" (fmap Xmlbf.toXml (claimSubType p))
             , Val      "use" (     toClaimUse (claimUse p))
             , Prop     "patient" (HM.empty, Xmlbf.toXml (claimPatient p))
             , OptProp  "billablePeriod" (fmap Xmlbf.toXml (claimBillablePeriod p))
             , Val      "created" (     toDateTime (claimCreated p))
             , OptProp  "enterer" (fmap Xmlbf.toXml (claimEnterer p))
             , OptProp  "insurer" (fmap Xmlbf.toXml (claimInsurer p))
             , Prop     "provider" (HM.empty, Xmlbf.toXml (claimProvider p))
             , Prop     "priority" (HM.empty, Xmlbf.toXml (claimPriority p))
             , OptProp  "fundsReserve" (fmap Xmlbf.toXml (claimFundsReserve p))
             , PropList "related" (fmap Xmlbf.toXml (claimRelated p))
             , OptProp  "prescription" (fmap Xmlbf.toXml (claimPrescription p))
             , OptProp  "originalPrescription" (fmap Xmlbf.toXml (claimOriginalPrescription p))
             , OptProp  "payee" (fmap Xmlbf.toXml (claimPayee p))
             , OptProp  "referral" (fmap Xmlbf.toXml (claimReferral p))
             , OptProp  "facility" (fmap Xmlbf.toXml (claimFacility p))
             , PropList "careTeam" (fmap Xmlbf.toXml (claimCareTeam p))
             , PropList "supportingInfo" (fmap Xmlbf.toXml (claimSupportingInfo p))
             , PropList "diagnosis" (fmap Xmlbf.toXml (claimDiagnosis p))
             , PropList "procedure" (fmap Xmlbf.toXml (claimProcedure p))
             , PropList "insurance" (fmap Xmlbf.toXml (claimInsurance p))
             , OptProp  "accident" (fmap Xmlbf.toXml (claimAccident p))
             , PropList "item" (fmap Xmlbf.toXml (claimItem p))
             , OptProp  "total" (fmap Xmlbf.toXml (claimTotal p))
             ]
instance Xmlbf.FromXml Claim where
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
    insurer <- optional $ Xmlbf.pElement "insurer" Xmlbf.fromXml
    provider <-            Xmlbf.pElement "provider" Xmlbf.fromXml
    priority <-            Xmlbf.pElement "priority" Xmlbf.fromXml
    fundsReserve <- optional $ Xmlbf.pElement "fundsReserve" Xmlbf.fromXml
    related <- many     $ Xmlbf.pElement "related" Xmlbf.fromXml
    prescription <- optional $ Xmlbf.pElement "prescription" Xmlbf.fromXml
    originalPrescription <- optional $ Xmlbf.pElement "originalPrescription" Xmlbf.fromXml
    payee <- optional $ Xmlbf.pElement "payee" Xmlbf.fromXml
    referral <- optional $ Xmlbf.pElement "referral" Xmlbf.fromXml
    facility <- optional $ Xmlbf.pElement "facility" Xmlbf.fromXml
    careTeam <- many     $ Xmlbf.pElement "careTeam" Xmlbf.fromXml
    supportingInfo <- many     $ Xmlbf.pElement "supportingInfo" Xmlbf.fromXml
    diagnosis <- many     $ Xmlbf.pElement "diagnosis" Xmlbf.fromXml
    procedure <- many     $ Xmlbf.pElement "procedure" Xmlbf.fromXml
    insurance <- many     $ Xmlbf.pElement "insurance" Xmlbf.fromXml
    accident <- optional $ Xmlbf.pElement "accident" Xmlbf.fromXml
    item <- many     $ Xmlbf.pElement "item" Xmlbf.fromXml
    total <- optional $ Xmlbf.pElement "total" Xmlbf.fromXml
    return Claim {
            claimId = fmap fromId id
          , claimMeta = meta
          , claimImplicitRules = fmap fromUri implicitRules
          , claimLanguage = fmap fromLanguage language
          , claimText = text
--          , claimContained = contained
          , claimExtension = extension
          , claimModifierExtension = modifierExtension
          , claimIdentifier = identifier
          , claimStatus =      fromClaimStatus status
          , claimType = ty
          , claimSubType = subType
          , claimUse =      fromClaimUse use
          , claimPatient = patient
          , claimBillablePeriod = billablePeriod
          , claimCreated =      fromDateTime created
          , claimEnterer = enterer
          , claimInsurer = insurer
          , claimProvider = provider
          , claimPriority = priority
          , claimFundsReserve = fundsReserve
          , claimRelated = related
          , claimPrescription = prescription
          , claimOriginalPrescription = originalPrescription
          , claimPayee = payee
          , claimReferral = referral
          , claimFacility = facility
          , claimCareTeam = careTeam
          , claimSupportingInfo = supportingInfo
          , claimDiagnosis = diagnosis
          , claimProcedure = procedure
          , claimInsurance = insurance
          , claimAccident = accident
          , claimItem = item
          , claimTotal = total
          }



data ClaimResponsePayment = ClaimResponsePayment {
    claimResponsePaymentAttrId :: Maybe Text
  , claimResponsePaymentExtension :: [Extension]
  , claimResponsePaymentModifierExtension :: [Extension]
  , claimResponsePaymentType :: CodeableConcept
  , claimResponsePaymentAdjustment :: Maybe Money
  , claimResponsePaymentAdjustmentReason :: Maybe CodeableConcept
  , claimResponsePaymentDate :: Maybe Date
  , claimResponsePaymentAmount :: Money
  , claimResponsePaymentIdentifier :: Maybe Identifier
  }
--

instance ToJSON ClaimResponsePayment where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimResponsePaymentAttrId p)
    ,  "extension" .= toJSON (claimResponsePaymentExtension p)
    ,  "modifierExtension" .= toJSON (claimResponsePaymentModifierExtension p)
    ,  "type" .= toJSON (claimResponsePaymentType p)
    ,  "adjustment" .= toJSON (claimResponsePaymentAdjustment p)
    ,  "adjustmentReason" .= toJSON (claimResponsePaymentAdjustmentReason p)
    ,  "date" .= toJSON (claimResponsePaymentDate p)
    ,  "amount" .= toJSON (claimResponsePaymentAmount p)
    ,  "identifier" .= toJSON (claimResponsePaymentIdentifier p)
    ]
instance FromJSON ClaimResponsePayment where
  parseJSON = withObject "ClaimResponsePayment" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:  "type"
        adjustment <- o .:? "adjustment"
        adjustmentReason <- o .:? "adjustmentReason"
        date <- o .:? "date"
        amount <- o .:  "amount"
        identifier <- o .:? "identifier"
        return ClaimResponsePayment{
            claimResponsePaymentAttrId = id
          , claimResponsePaymentExtension = extension
          , claimResponsePaymentModifierExtension = modifierExtension
          , claimResponsePaymentType = ty
          , claimResponsePaymentAdjustment = adjustment
          , claimResponsePaymentAdjustmentReason = adjustmentReason
          , claimResponsePaymentDate = date
          , claimResponsePaymentAmount = amount
          , claimResponsePaymentIdentifier = identifier
          }
instance Xmlbf.ToXml ClaimResponsePayment where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimResponsePaymentAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimResponsePaymentExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimResponsePaymentModifierExtension p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (claimResponsePaymentType p))
             , OptProp  "adjustment" (fmap Xmlbf.toXml (claimResponsePaymentAdjustment p))
             , OptProp  "adjustmentReason" (fmap Xmlbf.toXml (claimResponsePaymentAdjustmentReason p))
             , OptVal   "date" (fmap toDate (claimResponsePaymentDate p))
             , Prop     "amount" (HM.empty, Xmlbf.toXml (claimResponsePaymentAmount p))
             , OptProp  "identifier" (fmap Xmlbf.toXml (claimResponsePaymentIdentifier p))
             ]
instance Xmlbf.FromXml ClaimResponsePayment where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" Xmlbf.fromXml
    adjustment <- optional $ Xmlbf.pElement "adjustment" Xmlbf.fromXml
    adjustmentReason <- optional $ Xmlbf.pElement "adjustmentReason" Xmlbf.fromXml
    date <- optional $ Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    amount <-            Xmlbf.pElement "amount" Xmlbf.fromXml
    identifier <- optional $ Xmlbf.pElement "identifier" Xmlbf.fromXml
    return ClaimResponsePayment {
            claimResponsePaymentAttrId = id
          , claimResponsePaymentExtension = extension
          , claimResponsePaymentModifierExtension = modifierExtension
          , claimResponsePaymentType = ty
          , claimResponsePaymentAdjustment = adjustment
          , claimResponsePaymentAdjustmentReason = adjustmentReason
          , claimResponsePaymentDate = fmap fromDate date
          , claimResponsePaymentAmount = amount
          , claimResponsePaymentIdentifier = identifier
          }



data ClaimItemServiced
    = ClaimItemServicedDate Date
    | ClaimItemServicedPeriod Period
    deriving (Eq, Show)

data ClaimItemLocation
    = ClaimItemLocationCodeableConcept CodeableConcept
    | ClaimItemLocationAddress Address
    | ClaimItemLocationReference Reference
    deriving (Eq, Show)

data ClaimItem = ClaimItem {
    claimItemAttrId :: Maybe Text
  , claimItemExtension :: [Extension]
  , claimItemModifierExtension :: [Extension]
  , claimItemSequence :: PositiveInt
  , claimItemCareTeamSequence :: [PositiveInt]
  , claimItemDiagnosisSequence :: [PositiveInt]
  , claimItemProcedureSequence :: [PositiveInt]
  , claimItemInformationSequence :: [PositiveInt]
  , claimItemRevenue :: Maybe CodeableConcept
  , claimItemCategory :: Maybe CodeableConcept
  , claimItemProductOrService :: CodeableConcept
  , claimItemModifier :: [CodeableConcept]
  , claimItemProgramCode :: [CodeableConcept]
  , claimItemServiced :: Maybe ClaimItemServiced
  , claimItemLocation :: Maybe ClaimItemLocation
  , claimItemQuantity :: Maybe Quantity
  , claimItemUnitPrice :: Maybe Money
  , claimItemFactor :: Maybe Decimal
  , claimItemNet :: Maybe Money
  , claimItemUdi :: [Reference]
  , claimItemBodySite :: Maybe CodeableConcept
  , claimItemSubSite :: [CodeableConcept]
  , claimItemEncounter :: [Reference]
  , claimItemDetail :: [ClaimDetail]
  }
--

instance ToJSON ClaimItem where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimItemAttrId p)
    ,  "extension" .= toJSON (claimItemExtension p)
    ,  "modifierExtension" .= toJSON (claimItemModifierExtension p)
    ,  "sequence" .= toJSON (claimItemSequence p)
    ,  "careTeamSequence" .= toJSON (claimItemCareTeamSequence p)
    ,  "diagnosisSequence" .= toJSON (claimItemDiagnosisSequence p)
    ,  "procedureSequence" .= toJSON (claimItemProcedureSequence p)
    ,  "informationSequence" .= toJSON (claimItemInformationSequence p)
    ,  "revenue" .= toJSON (claimItemRevenue p)
    ,  "category" .= toJSON (claimItemCategory p)
    ,  "productOrService" .= toJSON (claimItemProductOrService p)
    ,  "modifier" .= toJSON (claimItemModifier p)
    ,  "programCode" .= toJSON (claimItemProgramCode p)
    , toServicedJSON (claimItemServiced p)
    , toLocationJSON (claimItemLocation p)
    ,  "quantity" .= toJSON (claimItemQuantity p)
    ,  "unitPrice" .= toJSON (claimItemUnitPrice p)
    ,  "factor" .= toJSON (claimItemFactor p)
    ,  "net" .= toJSON (claimItemNet p)
    ,  "udi" .= toJSON (claimItemUdi p)
    ,  "bodySite" .= toJSON (claimItemBodySite p)
    ,  "subSite" .= toJSON (claimItemSubSite p)
    ,  "encounter" .= toJSON (claimItemEncounter p)
    ,  "detail" .= toJSON (claimItemDetail p)
    ]
    where 
      toServicedJSON (     Nothing   ) = ("serviced", Null)
      toServicedJSON (Just (ClaimItemServicedDate c)) = ("serviced", toJSON c)
      toServicedJSON (Just (ClaimItemServicedPeriod c)) = ("serviced", toJSON c)
      toLocationJSON (     Nothing   ) = ("location", Null)
      toLocationJSON (Just (ClaimItemLocationCodeableConcept c)) = ("location", toJSON c)
      toLocationJSON (Just (ClaimItemLocationAddress c)) = ("location", toJSON c)
      toLocationJSON (Just (ClaimItemLocationReference c)) = ("location", toJSON c)
instance FromJSON ClaimItem where
  parseJSON = withObject "ClaimItem" $ \o -> do
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
        detail <- o .:? "detail" .!= []
        return ClaimItem{
            claimItemAttrId = id
          , claimItemExtension = extension
          , claimItemModifierExtension = modifierExtension
          , claimItemSequence = sequence
          , claimItemCareTeamSequence = careTeamSequence
          , claimItemDiagnosisSequence = diagnosisSequence
          , claimItemProcedureSequence = procedureSequence
          , claimItemInformationSequence = informationSequence
          , claimItemRevenue = revenue
          , claimItemCategory = category
          , claimItemProductOrService = productOrService
          , claimItemModifier = modifier
          , claimItemProgramCode = programCode
          , claimItemServiced = serviced
          , claimItemLocation = location
          , claimItemQuantity = quantity
          , claimItemUnitPrice = unitPrice
          , claimItemFactor = factor
          , claimItemNet = net
          , claimItemUdi = udi
          , claimItemBodySite = bodySite
          , claimItemSubSite = subSite
          , claimItemEncounter = encounter
          , claimItemDetail = detail
          }
    where 
      parseServiced o = parseServicedDate o <|> parseServicedPeriod o
      parseServicedDate o = do
                has <- o .: "servicedDate"
                return $ Just (ClaimItemServicedDate has)
      parseServicedPeriod o = do
                has <- o .: "servicedPeriod"
                return $ Just (ClaimItemServicedPeriod has)
      parseLocation o = parseLocationCodeableConcept o <|> parseLocationAddress o <|> parseLocationReference o
      parseLocationCodeableConcept o = do
                has <- o .: "locationCodeableConcept"
                return $ Just (ClaimItemLocationCodeableConcept has)
      parseLocationAddress o = do
                has <- o .: "locationAddress"
                return $ Just (ClaimItemLocationAddress has)
      parseLocationReference o = do
                has <- o .: "locationReference"
                return $ Just (ClaimItemLocationReference has)
instance Xmlbf.ToXml ClaimItem where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimItemAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimItemExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimItemModifierExtension p))
             , Val      "sequence" (     toPositiveInt (claimItemSequence p))
             , ValList  "careTeamSequence" (fmap toPositiveInt (claimItemCareTeamSequence p))
             , ValList  "diagnosisSequence" (fmap toPositiveInt (claimItemDiagnosisSequence p))
             , ValList  "procedureSequence" (fmap toPositiveInt (claimItemProcedureSequence p))
             , ValList  "informationSequence" (fmap toPositiveInt (claimItemInformationSequence p))
             , OptProp  "revenue" (fmap Xmlbf.toXml (claimItemRevenue p))
             , OptProp  "category" (fmap Xmlbf.toXml (claimItemCategory p))
             , Prop     "productOrService" (HM.empty, Xmlbf.toXml (claimItemProductOrService p))
             , PropList "modifier" (fmap Xmlbf.toXml (claimItemModifier p))
             , PropList "programCode" (fmap Xmlbf.toXml (claimItemProgramCode p))
             , toServicedXml (claimItemServiced p)
             , toLocationXml (claimItemLocation p)
             , OptProp  "quantity" (fmap Xmlbf.toXml (claimItemQuantity p))
             , OptProp  "unitPrice" (fmap Xmlbf.toXml (claimItemUnitPrice p))
             , OptVal   "factor" (fmap toDecimal (claimItemFactor p))
             , OptProp  "net" (fmap Xmlbf.toXml (claimItemNet p))
             , PropList "udi" (fmap Xmlbf.toXml (claimItemUdi p))
             , OptProp  "bodySite" (fmap Xmlbf.toXml (claimItemBodySite p))
             , PropList "subSite" (fmap Xmlbf.toXml (claimItemSubSite p))
             , PropList "encounter" (fmap Xmlbf.toXml (claimItemEncounter p))
             , PropList "detail" (fmap Xmlbf.toXml (claimItemDetail p))
             ]
       where 
          toServicedXml ( Nothing   ) = (OptVal "serviced" Nothing)
          toServicedXml (Just (ClaimItemServicedDate p)) = Val   "servicedDate" (toDate p)
          toServicedXml (Just (ClaimItemServicedPeriod p)) = Prop  "servicedPeriod" (HM.empty, Xmlbf.toXml p)
          toLocationXml ( Nothing   ) = (OptVal "location" Nothing)
          toLocationXml (Just (ClaimItemLocationCodeableConcept p)) = Prop  "locationCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toLocationXml (Just (ClaimItemLocationAddress p)) = Prop  "locationAddress" (HM.empty, Xmlbf.toXml p)
          toLocationXml (Just (ClaimItemLocationReference p)) = Prop  "locationReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml ClaimItem where
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
    detail <- many     $ Xmlbf.pElement "detail" Xmlbf.fromXml
    return ClaimItem {
            claimItemAttrId = id
          , claimItemExtension = extension
          , claimItemModifierExtension = modifierExtension
          , claimItemSequence =      fromPositiveInt sequence
          , claimItemCareTeamSequence = fmap fromPositiveInt careTeamSequence
          , claimItemDiagnosisSequence = fmap fromPositiveInt diagnosisSequence
          , claimItemProcedureSequence = fmap fromPositiveInt procedureSequence
          , claimItemInformationSequence = fmap fromPositiveInt informationSequence
          , claimItemRevenue = revenue
          , claimItemCategory = category
          , claimItemProductOrService = productOrService
          , claimItemModifier = modifier
          , claimItemProgramCode = programCode
          , claimItemServiced = serviced
          , claimItemLocation = location
          , claimItemQuantity = quantity
          , claimItemUnitPrice = unitPrice
          , claimItemFactor = fmap fromDecimal factor
          , claimItemNet = net
          , claimItemUdi = udi
          , claimItemBodySite = bodySite
          , claimItemSubSite = subSite
          , claimItemEncounter = encounter
          , claimItemDetail = detail
          }

    where 
      fromServicedXml = parseServicedDate <|> parseServicedPeriod <|> pure Nothing
      parseServicedDate = do
                has <- Xmlbf.pElement "servicedDate" (Xmlbf.pAttr "value")
                return $ Just (ClaimItemServicedDate (     toDate has))
      parseServicedPeriod = do
                has <- Xmlbf.pElement "servicedPeriod" Xmlbf.fromXml
                return $ Just (ClaimItemServicedPeriod (                      has))
      fromLocationXml = parseLocationCodeableConcept <|> parseLocationAddress <|> parseLocationReference <|> pure Nothing
      parseLocationCodeableConcept = do
                has <- Xmlbf.pElement "locationCodeableConcept" Xmlbf.fromXml
                return $ Just (ClaimItemLocationCodeableConcept (                      has))
      parseLocationAddress = do
                has <- Xmlbf.pElement "locationAddress" Xmlbf.fromXml
                return $ Just (ClaimItemLocationAddress (                      has))
      parseLocationReference = do
                has <- Xmlbf.pElement "locationReference" Xmlbf.fromXml
                return $ Just (ClaimItemLocationReference (                      has))


data ClaimResponseError = ClaimResponseError {
    claimResponseErrorAttrId :: Maybe Text
  , claimResponseErrorExtension :: [Extension]
  , claimResponseErrorModifierExtension :: [Extension]
  , claimResponseErrorItemSequence :: Maybe PositiveInt
  , claimResponseErrorDetailSequence :: Maybe PositiveInt
  , claimResponseErrorSubDetailSequence :: Maybe PositiveInt
  , claimResponseErrorCode :: CodeableConcept
  }
--

instance ToJSON ClaimResponseError where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimResponseErrorAttrId p)
    ,  "extension" .= toJSON (claimResponseErrorExtension p)
    ,  "modifierExtension" .= toJSON (claimResponseErrorModifierExtension p)
    ,  "itemSequence" .= toJSON (claimResponseErrorItemSequence p)
    ,  "detailSequence" .= toJSON (claimResponseErrorDetailSequence p)
    ,  "subDetailSequence" .= toJSON (claimResponseErrorSubDetailSequence p)
    ,  "code" .= toJSON (claimResponseErrorCode p)
    ]
instance FromJSON ClaimResponseError where
  parseJSON = withObject "ClaimResponseError" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        itemSequence <- o .:? "itemSequence"
        detailSequence <- o .:? "detailSequence"
        subDetailSequence <- o .:? "subDetailSequence"
        code <- o .:  "code"
        return ClaimResponseError{
            claimResponseErrorAttrId = id
          , claimResponseErrorExtension = extension
          , claimResponseErrorModifierExtension = modifierExtension
          , claimResponseErrorItemSequence = itemSequence
          , claimResponseErrorDetailSequence = detailSequence
          , claimResponseErrorSubDetailSequence = subDetailSequence
          , claimResponseErrorCode = code
          }
instance Xmlbf.ToXml ClaimResponseError where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimResponseErrorAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimResponseErrorExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimResponseErrorModifierExtension p))
             , OptVal   "itemSequence" (fmap toPositiveInt (claimResponseErrorItemSequence p))
             , OptVal   "detailSequence" (fmap toPositiveInt (claimResponseErrorDetailSequence p))
             , OptVal   "subDetailSequence" (fmap toPositiveInt (claimResponseErrorSubDetailSequence p))
             , Prop     "code" (HM.empty, Xmlbf.toXml (claimResponseErrorCode p))
             ]
instance Xmlbf.FromXml ClaimResponseError where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    itemSequence <- optional $ Xmlbf.pElement "itemSequence" (Xmlbf.pAttr "value")
    detailSequence <- optional $ Xmlbf.pElement "detailSequence" (Xmlbf.pAttr "value")
    subDetailSequence <- optional $ Xmlbf.pElement "subDetailSequence" (Xmlbf.pAttr "value")
    code <-            Xmlbf.pElement "code" Xmlbf.fromXml
    return ClaimResponseError {
            claimResponseErrorAttrId = id
          , claimResponseErrorExtension = extension
          , claimResponseErrorModifierExtension = modifierExtension
          , claimResponseErrorItemSequence = fmap fromPositiveInt itemSequence
          , claimResponseErrorDetailSequence = fmap fromPositiveInt detailSequence
          , claimResponseErrorSubDetailSequence = fmap fromPositiveInt subDetailSequence
          , claimResponseErrorCode = code
          }



data ClaimResponseSubDetail = ClaimResponseSubDetail {
    claimResponseSubDetailAttrId :: Maybe Text
  , claimResponseSubDetailExtension :: [Extension]
  , claimResponseSubDetailModifierExtension :: [Extension]
  , claimResponseSubDetailSubDetailSequence :: PositiveInt
  , claimResponseSubDetailNoteNumber :: [PositiveInt]
  , claimResponseSubDetailAdjudication :: [ClaimResponseAdjudication]
  }
--

instance ToJSON ClaimResponseSubDetail where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimResponseSubDetailAttrId p)
    ,  "extension" .= toJSON (claimResponseSubDetailExtension p)
    ,  "modifierExtension" .= toJSON (claimResponseSubDetailModifierExtension p)
    ,  "subDetailSequence" .= toJSON (claimResponseSubDetailSubDetailSequence p)
    ,  "noteNumber" .= toJSON (claimResponseSubDetailNoteNumber p)
    ,  "adjudication" .= toJSON (claimResponseSubDetailAdjudication p)
    ]
instance FromJSON ClaimResponseSubDetail where
  parseJSON = withObject "ClaimResponseSubDetail" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        subDetailSequence <- o .:  "subDetailSequence"
        noteNumber <- o .:? "noteNumber" .!= []
        adjudication <- o .:? "adjudication" .!= []
        return ClaimResponseSubDetail{
            claimResponseSubDetailAttrId = id
          , claimResponseSubDetailExtension = extension
          , claimResponseSubDetailModifierExtension = modifierExtension
          , claimResponseSubDetailSubDetailSequence = subDetailSequence
          , claimResponseSubDetailNoteNumber = noteNumber
          , claimResponseSubDetailAdjudication = adjudication
          }
instance Xmlbf.ToXml ClaimResponseSubDetail where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimResponseSubDetailAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimResponseSubDetailExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimResponseSubDetailModifierExtension p))
             , Val      "subDetailSequence" (     toPositiveInt (claimResponseSubDetailSubDetailSequence p))
             , ValList  "noteNumber" (fmap toPositiveInt (claimResponseSubDetailNoteNumber p))
             , PropList "adjudication" (fmap Xmlbf.toXml (claimResponseSubDetailAdjudication p))
             ]
instance Xmlbf.FromXml ClaimResponseSubDetail where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    subDetailSequence <-            Xmlbf.pElement "subDetailSequence" (Xmlbf.pAttr "value")
    noteNumber <- many     $ Xmlbf.pElement "noteNumber" (Xmlbf.pAttr "value")
    adjudication <- many     $ Xmlbf.pElement "adjudication" Xmlbf.fromXml
    return ClaimResponseSubDetail {
            claimResponseSubDetailAttrId = id
          , claimResponseSubDetailExtension = extension
          , claimResponseSubDetailModifierExtension = modifierExtension
          , claimResponseSubDetailSubDetailSequence =      fromPositiveInt subDetailSequence
          , claimResponseSubDetailNoteNumber = fmap fromPositiveInt noteNumber
          , claimResponseSubDetailAdjudication = adjudication
          }



data ClaimCareTeam = ClaimCareTeam {
    claimCareTeamAttrId :: Maybe Text
  , claimCareTeamExtension :: [Extension]
  , claimCareTeamModifierExtension :: [Extension]
  , claimCareTeamSequence :: PositiveInt
  , claimCareTeamProvider :: Reference
  , claimCareTeamResponsible :: Maybe Boolean
  , claimCareTeamRole :: Maybe CodeableConcept
  , claimCareTeamQualification :: Maybe CodeableConcept
  }
--

instance ToJSON ClaimCareTeam where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimCareTeamAttrId p)
    ,  "extension" .= toJSON (claimCareTeamExtension p)
    ,  "modifierExtension" .= toJSON (claimCareTeamModifierExtension p)
    ,  "sequence" .= toJSON (claimCareTeamSequence p)
    ,  "provider" .= toJSON (claimCareTeamProvider p)
    ,  "responsible" .= toJSON (claimCareTeamResponsible p)
    ,  "role" .= toJSON (claimCareTeamRole p)
    ,  "qualification" .= toJSON (claimCareTeamQualification p)
    ]
instance FromJSON ClaimCareTeam where
  parseJSON = withObject "ClaimCareTeam" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        sequence <- o .:  "sequence"
        provider <- o .:  "provider"
        responsible <- o .:? "responsible"
        role <- o .:? "role"
        qualification <- o .:? "qualification"
        return ClaimCareTeam{
            claimCareTeamAttrId = id
          , claimCareTeamExtension = extension
          , claimCareTeamModifierExtension = modifierExtension
          , claimCareTeamSequence = sequence
          , claimCareTeamProvider = provider
          , claimCareTeamResponsible = responsible
          , claimCareTeamRole = role
          , claimCareTeamQualification = qualification
          }
instance Xmlbf.ToXml ClaimCareTeam where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimCareTeamAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimCareTeamExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimCareTeamModifierExtension p))
             , Val      "sequence" (     toPositiveInt (claimCareTeamSequence p))
             , Prop     "provider" (HM.empty, Xmlbf.toXml (claimCareTeamProvider p))
             , OptVal   "responsible" (fmap toBoolean (claimCareTeamResponsible p))
             , OptProp  "role" (fmap Xmlbf.toXml (claimCareTeamRole p))
             , OptProp  "qualification" (fmap Xmlbf.toXml (claimCareTeamQualification p))
             ]
instance Xmlbf.FromXml ClaimCareTeam where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    sequence <-            Xmlbf.pElement "sequence" (Xmlbf.pAttr "value")
    provider <-            Xmlbf.pElement "provider" Xmlbf.fromXml
    responsible <- optional $ Xmlbf.pElement "responsible" (Xmlbf.pAttr "value")
    role <- optional $ Xmlbf.pElement "role" Xmlbf.fromXml
    qualification <- optional $ Xmlbf.pElement "qualification" Xmlbf.fromXml
    return ClaimCareTeam {
            claimCareTeamAttrId = id
          , claimCareTeamExtension = extension
          , claimCareTeamModifierExtension = modifierExtension
          , claimCareTeamSequence =      fromPositiveInt sequence
          , claimCareTeamProvider = provider
          , claimCareTeamResponsible = fmap fromBoolean responsible
          , claimCareTeamRole = role
          , claimCareTeamQualification = qualification
          }



data ClaimRelated = ClaimRelated {
    claimRelatedAttrId :: Maybe Text
  , claimRelatedExtension :: [Extension]
  , claimRelatedModifierExtension :: [Extension]
  , claimRelatedClaim :: Maybe Reference
  , claimRelatedRelationship :: Maybe CodeableConcept
  , claimRelatedReference :: Maybe Identifier
  }
--

instance ToJSON ClaimRelated where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimRelatedAttrId p)
    ,  "extension" .= toJSON (claimRelatedExtension p)
    ,  "modifierExtension" .= toJSON (claimRelatedModifierExtension p)
    ,  "claim" .= toJSON (claimRelatedClaim p)
    ,  "relationship" .= toJSON (claimRelatedRelationship p)
    ,  "reference" .= toJSON (claimRelatedReference p)
    ]
instance FromJSON ClaimRelated where
  parseJSON = withObject "ClaimRelated" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        claim <- o .:? "claim"
        relationship <- o .:? "relationship"
        reference <- o .:? "reference"
        return ClaimRelated{
            claimRelatedAttrId = id
          , claimRelatedExtension = extension
          , claimRelatedModifierExtension = modifierExtension
          , claimRelatedClaim = claim
          , claimRelatedRelationship = relationship
          , claimRelatedReference = reference
          }
instance Xmlbf.ToXml ClaimRelated where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimRelatedAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimRelatedExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimRelatedModifierExtension p))
             , OptProp  "claim" (fmap Xmlbf.toXml (claimRelatedClaim p))
             , OptProp  "relationship" (fmap Xmlbf.toXml (claimRelatedRelationship p))
             , OptProp  "reference" (fmap Xmlbf.toXml (claimRelatedReference p))
             ]
instance Xmlbf.FromXml ClaimRelated where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    claim <- optional $ Xmlbf.pElement "claim" Xmlbf.fromXml
    relationship <- optional $ Xmlbf.pElement "relationship" Xmlbf.fromXml
    reference <- optional $ Xmlbf.pElement "reference" Xmlbf.fromXml
    return ClaimRelated {
            claimRelatedAttrId = id
          , claimRelatedExtension = extension
          , claimRelatedModifierExtension = modifierExtension
          , claimRelatedClaim = claim
          , claimRelatedRelationship = relationship
          , claimRelatedReference = reference
          }



data ClaimResponseProcessNoteType
    = CRPNTDisplay
    | CRPNTPrint
    | CRPNTPrintoper
  deriving (Eq, Show)

instance ToJSON ClaimResponseProcessNoteType where
    toJSON CRPNTDisplay = String "display"
    toJSON CRPNTPrint = String "print"
    toJSON CRPNTPrintoper = String "printoper"
instance FromJSON ClaimResponseProcessNoteType where
    parseJSON "display" = return CRPNTDisplay
    parseJSON "print" = return CRPNTPrint
    parseJSON "printoper" = return CRPNTPrintoper

toClaimResponseProcessNoteType CRPNTDisplay = "display"
toClaimResponseProcessNoteType CRPNTPrint = "print"
toClaimResponseProcessNoteType CRPNTPrintoper = "printoper"
fromClaimResponseProcessNoteType "display" = CRPNTDisplay
fromClaimResponseProcessNoteType "print" = CRPNTPrint
fromClaimResponseProcessNoteType "printoper" = CRPNTPrintoper


data ClaimResponseProcessNote = ClaimResponseProcessNote {
    claimResponseProcessNoteAttrId :: Maybe Text
  , claimResponseProcessNoteExtension :: [Extension]
  , claimResponseProcessNoteModifierExtension :: [Extension]
  , claimResponseProcessNoteNumber :: Maybe PositiveInt
  , claimResponseProcessNoteType :: Maybe ClaimResponseProcessNoteType
  , claimResponseProcessNoteText :: Text
  , claimResponseProcessNoteLanguage :: Maybe CodeableConcept
  }
--

instance ToJSON ClaimResponseProcessNote where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimResponseProcessNoteAttrId p)
    ,  "extension" .= toJSON (claimResponseProcessNoteExtension p)
    ,  "modifierExtension" .= toJSON (claimResponseProcessNoteModifierExtension p)
    ,  "number" .= toJSON (claimResponseProcessNoteNumber p)
    ,  "type" .= toJSON (claimResponseProcessNoteType p)
    ,  "text" .= toJSON (claimResponseProcessNoteText p)
    ,  "language" .= toJSON (claimResponseProcessNoteLanguage p)
    ]
instance FromJSON ClaimResponseProcessNote where
  parseJSON = withObject "ClaimResponseProcessNote" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        number <- o .:? "number"
        ty <- o .:? "type"
        text <- o .:  "text"
        language <- o .:? "language"
        return ClaimResponseProcessNote{
            claimResponseProcessNoteAttrId = id
          , claimResponseProcessNoteExtension = extension
          , claimResponseProcessNoteModifierExtension = modifierExtension
          , claimResponseProcessNoteNumber = number
          , claimResponseProcessNoteType = ty
          , claimResponseProcessNoteText = text
          , claimResponseProcessNoteLanguage = language
          }
instance Xmlbf.ToXml ClaimResponseProcessNote where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimResponseProcessNoteAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimResponseProcessNoteExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimResponseProcessNoteModifierExtension p))
             , OptVal   "number" (fmap toPositiveInt (claimResponseProcessNoteNumber p))
             , OptVal   "type" (fmap toClaimResponseProcessNoteType (claimResponseProcessNoteType p))
             , Val      "text" (     toString (claimResponseProcessNoteText p))
             , OptProp  "language" (fmap Xmlbf.toXml (claimResponseProcessNoteLanguage p))
             ]
instance Xmlbf.FromXml ClaimResponseProcessNote where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    number <- optional $ Xmlbf.pElement "number" (Xmlbf.pAttr "value")
    ty <- optional $ Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    text <-            Xmlbf.pElement "text" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" Xmlbf.fromXml
    return ClaimResponseProcessNote {
            claimResponseProcessNoteAttrId = id
          , claimResponseProcessNoteExtension = extension
          , claimResponseProcessNoteModifierExtension = modifierExtension
          , claimResponseProcessNoteNumber = fmap fromPositiveInt number
          , claimResponseProcessNoteType = fmap fromClaimResponseProcessNoteType ty
          , claimResponseProcessNoteText =      fromString text
          , claimResponseProcessNoteLanguage = language
          }



data ClaimResponseAddItemServiced
    = ClaimResponseAddItemServicedDate Date
    | ClaimResponseAddItemServicedPeriod Period
    deriving (Eq, Show)

data ClaimResponseAddItemLocation
    = ClaimResponseAddItemLocationCodeableConcept CodeableConcept
    | ClaimResponseAddItemLocationAddress Address
    | ClaimResponseAddItemLocationReference Reference
    deriving (Eq, Show)

data ClaimResponseAddItem = ClaimResponseAddItem {
    claimResponseAddItemAttrId :: Maybe Text
  , claimResponseAddItemExtension :: [Extension]
  , claimResponseAddItemModifierExtension :: [Extension]
  , claimResponseAddItemItemSequence :: [PositiveInt]
  , claimResponseAddItemDetailSequence :: [PositiveInt]
  , claimResponseAddItemSubdetailSequence :: [PositiveInt]
  , claimResponseAddItemProvider :: [Reference]
  , claimResponseAddItemProductOrService :: CodeableConcept
  , claimResponseAddItemModifier :: [CodeableConcept]
  , claimResponseAddItemProgramCode :: [CodeableConcept]
  , claimResponseAddItemServicedDate :: Maybe Date
  , claimResponseAddItemServicedPeriod :: Maybe Period
  , claimResponseAddItemLocationCodeableConcept :: Maybe CodeableConcept
  , claimResponseAddItemLocationAddress :: Maybe Address
  , claimResponseAddItemLocationReference :: Maybe Reference
  , claimResponseAddItemQuantity :: Maybe Quantity
  , claimResponseAddItemUnitPrice :: Maybe Money
  , claimResponseAddItemFactor :: Maybe Decimal
  , claimResponseAddItemNet :: Maybe Money
  , claimResponseAddItemBodySite :: Maybe CodeableConcept
  , claimResponseAddItemSubSite :: [CodeableConcept]
  , claimResponseAddItemNoteNumber :: [PositiveInt]
  , claimResponseAddItemAdjudication :: [ClaimResponseAdjudication]
  , claimResponseAddItemDetail :: [ClaimResponseDetail1]
  }
--

instance ToJSON ClaimResponseAddItem where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimResponseAddItemAttrId p)
    ,  "extension" .= toJSON (claimResponseAddItemExtension p)
    ,  "modifierExtension" .= toJSON (claimResponseAddItemModifierExtension p)
    ,  "itemSequence" .= toJSON (claimResponseAddItemItemSequence p)
    ,  "detailSequence" .= toJSON (claimResponseAddItemDetailSequence p)
    ,  "subdetailSequence" .= toJSON (claimResponseAddItemSubdetailSequence p)
    ,  "provider" .= toJSON (claimResponseAddItemProvider p)
    ,  "productOrService" .= toJSON (claimResponseAddItemProductOrService p)
    ,  "modifier" .= toJSON (claimResponseAddItemModifier p)
    ,  "programCode" .= toJSON (claimResponseAddItemProgramCode p)
    ,  "servicedDate" .= toJSON (claimResponseAddItemServicedDate p)
    ,  "servicedPeriod" .= toJSON (claimResponseAddItemServicedPeriod p)
    ,  "locationCodeableConcept" .= toJSON (claimResponseAddItemLocationCodeableConcept p)
    ,  "locationAddress" .= toJSON (claimResponseAddItemLocationAddress p)
    ,  "locationReference" .= toJSON (claimResponseAddItemLocationReference p)
    ,  "quantity" .= toJSON (claimResponseAddItemQuantity p)
    ,  "unitPrice" .= toJSON (claimResponseAddItemUnitPrice p)
    ,  "factor" .= toJSON (claimResponseAddItemFactor p)
    ,  "net" .= toJSON (claimResponseAddItemNet p)
    ,  "bodySite" .= toJSON (claimResponseAddItemBodySite p)
    ,  "subSite" .= toJSON (claimResponseAddItemSubSite p)
    ,  "noteNumber" .= toJSON (claimResponseAddItemNoteNumber p)
    ,  "adjudication" .= toJSON (claimResponseAddItemAdjudication p)
    ,  "detail" .= toJSON (claimResponseAddItemDetail p)
    ]
instance FromJSON ClaimResponseAddItem where
  parseJSON = withObject "ClaimResponseAddItem" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        itemSequence <- o .:? "itemSequence" .!= []
        detailSequence <- o .:? "detailSequence" .!= []
        subdetailSequence <- o .:? "subdetailSequence" .!= []
        provider <- o .:? "provider" .!= []
        productOrService <- o .:  "productOrService"
        modifier <- o .:? "modifier" .!= []
        programCode <- o .:? "programCode" .!= []
        servicedDate <- o .:? "servicedDate"
        servicedPeriod <- o .:? "servicedPeriod"
        locationCodeableConcept <- o .:? "locationCodeableConcept"
        locationAddress <- o .:? "locationAddress"
        locationReference <- o .:? "locationReference"
        quantity <- o .:? "quantity"
        unitPrice <- o .:? "unitPrice"
        factor <- o .:? "factor"
        net <- o .:? "net"
        bodySite <- o .:? "bodySite"
        subSite <- o .:? "subSite" .!= []
        noteNumber <- o .:? "noteNumber" .!= []
        adjudication <- o .:? "adjudication" .!= []
        detail <- o .:? "detail" .!= []
        return ClaimResponseAddItem{
            claimResponseAddItemAttrId = id
          , claimResponseAddItemExtension = extension
          , claimResponseAddItemModifierExtension = modifierExtension
          , claimResponseAddItemItemSequence = itemSequence
          , claimResponseAddItemDetailSequence = detailSequence
          , claimResponseAddItemSubdetailSequence = subdetailSequence
          , claimResponseAddItemProvider = provider
          , claimResponseAddItemProductOrService = productOrService
          , claimResponseAddItemModifier = modifier
          , claimResponseAddItemProgramCode = programCode
          , claimResponseAddItemServicedDate = servicedDate
          , claimResponseAddItemServicedPeriod = servicedPeriod
          , claimResponseAddItemLocationCodeableConcept = locationCodeableConcept
          , claimResponseAddItemLocationAddress = locationAddress
          , claimResponseAddItemLocationReference = locationReference
          , claimResponseAddItemQuantity = quantity
          , claimResponseAddItemUnitPrice = unitPrice
          , claimResponseAddItemFactor = factor
          , claimResponseAddItemNet = net
          , claimResponseAddItemBodySite = bodySite
          , claimResponseAddItemSubSite = subSite
          , claimResponseAddItemNoteNumber = noteNumber
          , claimResponseAddItemAdjudication = adjudication
          , claimResponseAddItemDetail = detail
          }
instance Xmlbf.ToXml ClaimResponseAddItem where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimResponseAddItemAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimResponseAddItemExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimResponseAddItemModifierExtension p))
             , ValList  "itemSequence" (fmap toPositiveInt (claimResponseAddItemItemSequence p))
             , ValList  "detailSequence" (fmap toPositiveInt (claimResponseAddItemDetailSequence p))
             , ValList  "subdetailSequence" (fmap toPositiveInt (claimResponseAddItemSubdetailSequence p))
             , PropList "provider" (fmap Xmlbf.toXml (claimResponseAddItemProvider p))
             , Prop     "productOrService" (HM.empty, Xmlbf.toXml (claimResponseAddItemProductOrService p))
             , PropList "modifier" (fmap Xmlbf.toXml (claimResponseAddItemModifier p))
             , PropList "programCode" (fmap Xmlbf.toXml (claimResponseAddItemProgramCode p))
             , OptVal   "servicedDate" (fmap toDate (claimResponseAddItemServicedDate p))
             , OptProp  "servicedPeriod" (fmap Xmlbf.toXml (claimResponseAddItemServicedPeriod p))
             , OptProp  "locationCodeableConcept" (fmap Xmlbf.toXml (claimResponseAddItemLocationCodeableConcept p))
             , OptProp  "locationAddress" (fmap Xmlbf.toXml (claimResponseAddItemLocationAddress p))
             , OptProp  "locationReference" (fmap Xmlbf.toXml (claimResponseAddItemLocationReference p))
             , OptProp  "quantity" (fmap Xmlbf.toXml (claimResponseAddItemQuantity p))
             , OptProp  "unitPrice" (fmap Xmlbf.toXml (claimResponseAddItemUnitPrice p))
             , OptVal   "factor" (fmap toDecimal (claimResponseAddItemFactor p))
             , OptProp  "net" (fmap Xmlbf.toXml (claimResponseAddItemNet p))
             , OptProp  "bodySite" (fmap Xmlbf.toXml (claimResponseAddItemBodySite p))
             , PropList "subSite" (fmap Xmlbf.toXml (claimResponseAddItemSubSite p))
             , ValList  "noteNumber" (fmap toPositiveInt (claimResponseAddItemNoteNumber p))
             , PropList "adjudication" (fmap Xmlbf.toXml (claimResponseAddItemAdjudication p))
             , PropList "detail" (fmap Xmlbf.toXml (claimResponseAddItemDetail p))
             ]
instance Xmlbf.FromXml ClaimResponseAddItem where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    itemSequence <- many     $ Xmlbf.pElement "itemSequence" (Xmlbf.pAttr "value")
    detailSequence <- many     $ Xmlbf.pElement "detailSequence" (Xmlbf.pAttr "value")
    subdetailSequence <- many     $ Xmlbf.pElement "subdetailSequence" (Xmlbf.pAttr "value")
    provider <- many     $ Xmlbf.pElement "provider" Xmlbf.fromXml
    productOrService <-            Xmlbf.pElement "productOrService" Xmlbf.fromXml
    modifier <- many     $ Xmlbf.pElement "modifier" Xmlbf.fromXml
    programCode <- many     $ Xmlbf.pElement "programCode" Xmlbf.fromXml
    servicedDate <- optional $ Xmlbf.pElement "servicedDate" (Xmlbf.pAttr "value")
    servicedPeriod <- optional $ Xmlbf.pElement "servicedPeriod" Xmlbf.fromXml
    locationCodeableConcept <- optional $ Xmlbf.pElement "locationCodeableConcept" Xmlbf.fromXml
    locationAddress <- optional $ Xmlbf.pElement "locationAddress" Xmlbf.fromXml
    locationReference <- optional $ Xmlbf.pElement "locationReference" Xmlbf.fromXml
    quantity <- optional $ Xmlbf.pElement "quantity" Xmlbf.fromXml
    unitPrice <- optional $ Xmlbf.pElement "unitPrice" Xmlbf.fromXml
    factor <- optional $ Xmlbf.pElement "factor" (Xmlbf.pAttr "value")
    net <- optional $ Xmlbf.pElement "net" Xmlbf.fromXml
    bodySite <- optional $ Xmlbf.pElement "bodySite" Xmlbf.fromXml
    subSite <- many     $ Xmlbf.pElement "subSite" Xmlbf.fromXml
    noteNumber <- many     $ Xmlbf.pElement "noteNumber" (Xmlbf.pAttr "value")
    adjudication <- many     $ Xmlbf.pElement "adjudication" Xmlbf.fromXml
    detail <- many     $ Xmlbf.pElement "detail" Xmlbf.fromXml
    return ClaimResponseAddItem {
            claimResponseAddItemAttrId = id
          , claimResponseAddItemExtension = extension
          , claimResponseAddItemModifierExtension = modifierExtension
          , claimResponseAddItemItemSequence = fmap fromPositiveInt itemSequence
          , claimResponseAddItemDetailSequence = fmap fromPositiveInt detailSequence
          , claimResponseAddItemSubdetailSequence = fmap fromPositiveInt subdetailSequence
          , claimResponseAddItemProvider = provider
          , claimResponseAddItemProductOrService = productOrService
          , claimResponseAddItemModifier = modifier
          , claimResponseAddItemProgramCode = programCode
          , claimResponseAddItemServicedDate = fmap fromDate servicedDate
          , claimResponseAddItemServicedPeriod = servicedPeriod
          , claimResponseAddItemLocationCodeableConcept = locationCodeableConcept
          , claimResponseAddItemLocationAddress = locationAddress
          , claimResponseAddItemLocationReference = locationReference
          , claimResponseAddItemQuantity = quantity
          , claimResponseAddItemUnitPrice = unitPrice
          , claimResponseAddItemFactor = fmap fromDecimal factor
          , claimResponseAddItemNet = net
          , claimResponseAddItemBodySite = bodySite
          , claimResponseAddItemSubSite = subSite
          , claimResponseAddItemNoteNumber = fmap fromPositiveInt noteNumber
          , claimResponseAddItemAdjudication = adjudication
          , claimResponseAddItemDetail = detail
          }



data ClaimResponseAdjudication = ClaimResponseAdjudication {
    claimResponseAdjudicationAttrId :: Maybe Text
  , claimResponseAdjudicationExtension :: [Extension]
  , claimResponseAdjudicationModifierExtension :: [Extension]
  , claimResponseAdjudicationCategory :: CodeableConcept
  , claimResponseAdjudicationReason :: Maybe CodeableConcept
  , claimResponseAdjudicationAmount :: Maybe Money
  , claimResponseAdjudicationValue :: Maybe Decimal
  }
--

instance ToJSON ClaimResponseAdjudication where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimResponseAdjudicationAttrId p)
    ,  "extension" .= toJSON (claimResponseAdjudicationExtension p)
    ,  "modifierExtension" .= toJSON (claimResponseAdjudicationModifierExtension p)
    ,  "category" .= toJSON (claimResponseAdjudicationCategory p)
    ,  "reason" .= toJSON (claimResponseAdjudicationReason p)
    ,  "amount" .= toJSON (claimResponseAdjudicationAmount p)
    ,  "value" .= toJSON (claimResponseAdjudicationValue p)
    ]
instance FromJSON ClaimResponseAdjudication where
  parseJSON = withObject "ClaimResponseAdjudication" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        category <- o .:  "category"
        reason <- o .:? "reason"
        amount <- o .:? "amount"
        value <- o .:? "value"
        return ClaimResponseAdjudication{
            claimResponseAdjudicationAttrId = id
          , claimResponseAdjudicationExtension = extension
          , claimResponseAdjudicationModifierExtension = modifierExtension
          , claimResponseAdjudicationCategory = category
          , claimResponseAdjudicationReason = reason
          , claimResponseAdjudicationAmount = amount
          , claimResponseAdjudicationValue = value
          }
instance Xmlbf.ToXml ClaimResponseAdjudication where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimResponseAdjudicationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimResponseAdjudicationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimResponseAdjudicationModifierExtension p))
             , Prop     "category" (HM.empty, Xmlbf.toXml (claimResponseAdjudicationCategory p))
             , OptProp  "reason" (fmap Xmlbf.toXml (claimResponseAdjudicationReason p))
             , OptProp  "amount" (fmap Xmlbf.toXml (claimResponseAdjudicationAmount p))
             , OptVal   "value" (fmap toDecimal (claimResponseAdjudicationValue p))
             ]
instance Xmlbf.FromXml ClaimResponseAdjudication where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    category <-            Xmlbf.pElement "category" Xmlbf.fromXml
    reason <- optional $ Xmlbf.pElement "reason" Xmlbf.fromXml
    amount <- optional $ Xmlbf.pElement "amount" Xmlbf.fromXml
    value <- optional $ Xmlbf.pElement "value" (Xmlbf.pAttr "value")
    return ClaimResponseAdjudication {
            claimResponseAdjudicationAttrId = id
          , claimResponseAdjudicationExtension = extension
          , claimResponseAdjudicationModifierExtension = modifierExtension
          , claimResponseAdjudicationCategory = category
          , claimResponseAdjudicationReason = reason
          , claimResponseAdjudicationAmount = amount
          , claimResponseAdjudicationValue = fmap fromDecimal value
          }



data ClaimResponseInsurance = ClaimResponseInsurance {
    claimResponseInsuranceAttrId :: Maybe Text
  , claimResponseInsuranceExtension :: [Extension]
  , claimResponseInsuranceModifierExtension :: [Extension]
  , claimResponseInsuranceSequence :: PositiveInt
  , claimResponseInsuranceFocal :: Boolean
  , claimResponseInsuranceCoverage :: Reference
  , claimResponseInsuranceBusinessArrangement :: Maybe Text
  , claimResponseInsuranceClaimResponse :: Maybe Reference
  }
--

instance ToJSON ClaimResponseInsurance where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimResponseInsuranceAttrId p)
    ,  "extension" .= toJSON (claimResponseInsuranceExtension p)
    ,  "modifierExtension" .= toJSON (claimResponseInsuranceModifierExtension p)
    ,  "sequence" .= toJSON (claimResponseInsuranceSequence p)
    ,  "focal" .= toJSON (claimResponseInsuranceFocal p)
    ,  "coverage" .= toJSON (claimResponseInsuranceCoverage p)
    ,  "businessArrangement" .= toJSON (claimResponseInsuranceBusinessArrangement p)
    ,  "claimResponse" .= toJSON (claimResponseInsuranceClaimResponse p)
    ]
instance FromJSON ClaimResponseInsurance where
  parseJSON = withObject "ClaimResponseInsurance" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        sequence <- o .:  "sequence"
        focal <- o .:  "focal"
        coverage <- o .:  "coverage"
        businessArrangement <- o .:? "businessArrangement"
        claimResponse <- o .:? "claimResponse"
        return ClaimResponseInsurance{
            claimResponseInsuranceAttrId = id
          , claimResponseInsuranceExtension = extension
          , claimResponseInsuranceModifierExtension = modifierExtension
          , claimResponseInsuranceSequence = sequence
          , claimResponseInsuranceFocal = focal
          , claimResponseInsuranceCoverage = coverage
          , claimResponseInsuranceBusinessArrangement = businessArrangement
          , claimResponseInsuranceClaimResponse = claimResponse
          }
instance Xmlbf.ToXml ClaimResponseInsurance where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimResponseInsuranceAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimResponseInsuranceExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimResponseInsuranceModifierExtension p))
             , Val      "sequence" (     toPositiveInt (claimResponseInsuranceSequence p))
             , Val      "focal" (     toBoolean (claimResponseInsuranceFocal p))
             , Prop     "coverage" (HM.empty, Xmlbf.toXml (claimResponseInsuranceCoverage p))
             , OptVal   "businessArrangement" (fmap toString (claimResponseInsuranceBusinessArrangement p))
             , OptProp  "claimResponse" (fmap Xmlbf.toXml (claimResponseInsuranceClaimResponse p))
             ]
instance Xmlbf.FromXml ClaimResponseInsurance where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    sequence <-            Xmlbf.pElement "sequence" (Xmlbf.pAttr "value")
    focal <-            Xmlbf.pElement "focal" (Xmlbf.pAttr "value")
    coverage <-            Xmlbf.pElement "coverage" Xmlbf.fromXml
    businessArrangement <- optional $ Xmlbf.pElement "businessArrangement" (Xmlbf.pAttr "value")
    claimResponse <- optional $ Xmlbf.pElement "claimResponse" Xmlbf.fromXml
    return ClaimResponseInsurance {
            claimResponseInsuranceAttrId = id
          , claimResponseInsuranceExtension = extension
          , claimResponseInsuranceModifierExtension = modifierExtension
          , claimResponseInsuranceSequence =      fromPositiveInt sequence
          , claimResponseInsuranceFocal =      fromBoolean focal
          , claimResponseInsuranceCoverage = coverage
          , claimResponseInsuranceBusinessArrangement = fmap fromString businessArrangement
          , claimResponseInsuranceClaimResponse = claimResponse
          }



data ClaimPayee = ClaimPayee {
    claimPayeeAttrId :: Maybe Text
  , claimPayeeExtension :: [Extension]
  , claimPayeeModifierExtension :: [Extension]
  , claimPayeeType :: CodeableConcept
  , claimPayeeParty :: Maybe Reference
  }
--

instance ToJSON ClaimPayee where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimPayeeAttrId p)
    ,  "extension" .= toJSON (claimPayeeExtension p)
    ,  "modifierExtension" .= toJSON (claimPayeeModifierExtension p)
    ,  "type" .= toJSON (claimPayeeType p)
    ,  "party" .= toJSON (claimPayeeParty p)
    ]
instance FromJSON ClaimPayee where
  parseJSON = withObject "ClaimPayee" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:  "type"
        party <- o .:? "party"
        return ClaimPayee{
            claimPayeeAttrId = id
          , claimPayeeExtension = extension
          , claimPayeeModifierExtension = modifierExtension
          , claimPayeeType = ty
          , claimPayeeParty = party
          }
instance Xmlbf.ToXml ClaimPayee where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimPayeeAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimPayeeExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimPayeeModifierExtension p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (claimPayeeType p))
             , OptProp  "party" (fmap Xmlbf.toXml (claimPayeeParty p))
             ]
instance Xmlbf.FromXml ClaimPayee where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" Xmlbf.fromXml
    party <- optional $ Xmlbf.pElement "party" Xmlbf.fromXml
    return ClaimPayee {
            claimPayeeAttrId = id
          , claimPayeeExtension = extension
          , claimPayeeModifierExtension = modifierExtension
          , claimPayeeType = ty
          , claimPayeeParty = party
          }



data ClaimSubDetail = ClaimSubDetail {
    claimSubDetailAttrId :: Maybe Text
  , claimSubDetailExtension :: [Extension]
  , claimSubDetailModifierExtension :: [Extension]
  , claimSubDetailSequence :: PositiveInt
  , claimSubDetailRevenue :: Maybe CodeableConcept
  , claimSubDetailCategory :: Maybe CodeableConcept
  , claimSubDetailProductOrService :: CodeableConcept
  , claimSubDetailModifier :: [CodeableConcept]
  , claimSubDetailProgramCode :: [CodeableConcept]
  , claimSubDetailQuantity :: Maybe Quantity
  , claimSubDetailUnitPrice :: Maybe Money
  , claimSubDetailFactor :: Maybe Decimal
  , claimSubDetailNet :: Maybe Money
  , claimSubDetailUdi :: [Reference]
  }
--

instance ToJSON ClaimSubDetail where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimSubDetailAttrId p)
    ,  "extension" .= toJSON (claimSubDetailExtension p)
    ,  "modifierExtension" .= toJSON (claimSubDetailModifierExtension p)
    ,  "sequence" .= toJSON (claimSubDetailSequence p)
    ,  "revenue" .= toJSON (claimSubDetailRevenue p)
    ,  "category" .= toJSON (claimSubDetailCategory p)
    ,  "productOrService" .= toJSON (claimSubDetailProductOrService p)
    ,  "modifier" .= toJSON (claimSubDetailModifier p)
    ,  "programCode" .= toJSON (claimSubDetailProgramCode p)
    ,  "quantity" .= toJSON (claimSubDetailQuantity p)
    ,  "unitPrice" .= toJSON (claimSubDetailUnitPrice p)
    ,  "factor" .= toJSON (claimSubDetailFactor p)
    ,  "net" .= toJSON (claimSubDetailNet p)
    ,  "udi" .= toJSON (claimSubDetailUdi p)
    ]
instance FromJSON ClaimSubDetail where
  parseJSON = withObject "ClaimSubDetail" $ \o -> do
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
        return ClaimSubDetail{
            claimSubDetailAttrId = id
          , claimSubDetailExtension = extension
          , claimSubDetailModifierExtension = modifierExtension
          , claimSubDetailSequence = sequence
          , claimSubDetailRevenue = revenue
          , claimSubDetailCategory = category
          , claimSubDetailProductOrService = productOrService
          , claimSubDetailModifier = modifier
          , claimSubDetailProgramCode = programCode
          , claimSubDetailQuantity = quantity
          , claimSubDetailUnitPrice = unitPrice
          , claimSubDetailFactor = factor
          , claimSubDetailNet = net
          , claimSubDetailUdi = udi
          }
instance Xmlbf.ToXml ClaimSubDetail where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimSubDetailAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimSubDetailExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimSubDetailModifierExtension p))
             , Val      "sequence" (     toPositiveInt (claimSubDetailSequence p))
             , OptProp  "revenue" (fmap Xmlbf.toXml (claimSubDetailRevenue p))
             , OptProp  "category" (fmap Xmlbf.toXml (claimSubDetailCategory p))
             , Prop     "productOrService" (HM.empty, Xmlbf.toXml (claimSubDetailProductOrService p))
             , PropList "modifier" (fmap Xmlbf.toXml (claimSubDetailModifier p))
             , PropList "programCode" (fmap Xmlbf.toXml (claimSubDetailProgramCode p))
             , OptProp  "quantity" (fmap Xmlbf.toXml (claimSubDetailQuantity p))
             , OptProp  "unitPrice" (fmap Xmlbf.toXml (claimSubDetailUnitPrice p))
             , OptVal   "factor" (fmap toDecimal (claimSubDetailFactor p))
             , OptProp  "net" (fmap Xmlbf.toXml (claimSubDetailNet p))
             , PropList "udi" (fmap Xmlbf.toXml (claimSubDetailUdi p))
             ]
instance Xmlbf.FromXml ClaimSubDetail where
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
    return ClaimSubDetail {
            claimSubDetailAttrId = id
          , claimSubDetailExtension = extension
          , claimSubDetailModifierExtension = modifierExtension
          , claimSubDetailSequence =      fromPositiveInt sequence
          , claimSubDetailRevenue = revenue
          , claimSubDetailCategory = category
          , claimSubDetailProductOrService = productOrService
          , claimSubDetailModifier = modifier
          , claimSubDetailProgramCode = programCode
          , claimSubDetailQuantity = quantity
          , claimSubDetailUnitPrice = unitPrice
          , claimSubDetailFactor = fmap fromDecimal factor
          , claimSubDetailNet = net
          , claimSubDetailUdi = udi
          }



data ClaimProcedureProcedure
    = ClaimProcedureProcedureCodeableConcept CodeableConcept
    | ClaimProcedureProcedureReference Reference
    deriving (Eq, Show)

data ClaimProcedure = ClaimProcedure {
    claimProcedureAttrId :: Maybe Text
  , claimProcedureExtension :: [Extension]
  , claimProcedureModifierExtension :: [Extension]
  , claimProcedureSequence :: PositiveInt
  , claimProcedureType :: [CodeableConcept]
  , claimProcedureDate :: Maybe DateTime
  , claimProcedureProcedure :: ClaimProcedureProcedure
  , claimProcedureUdi :: [Reference]
  }
--

instance ToJSON ClaimProcedure where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimProcedureAttrId p)
    ,  "extension" .= toJSON (claimProcedureExtension p)
    ,  "modifierExtension" .= toJSON (claimProcedureModifierExtension p)
    ,  "sequence" .= toJSON (claimProcedureSequence p)
    ,  "type" .= toJSON (claimProcedureType p)
    ,  "date" .= toJSON (claimProcedureDate p)
    , toProcedureJSON (claimProcedureProcedure p)
    ,  "udi" .= toJSON (claimProcedureUdi p)
    ]
    where 
      toProcedureJSON (     (ClaimProcedureProcedureCodeableConcept c)) = ("procedure", toJSON c)
      toProcedureJSON (     (ClaimProcedureProcedureReference c)) = ("procedure", toJSON c)
instance FromJSON ClaimProcedure where
  parseJSON = withObject "ClaimProcedure" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        sequence <- o .:  "sequence"
        ty <- o .:? "type" .!= []
        date <- o .:? "date"
        procedure <- parseProcedure o
        udi <- o .:? "udi" .!= []
        return ClaimProcedure{
            claimProcedureAttrId = id
          , claimProcedureExtension = extension
          , claimProcedureModifierExtension = modifierExtension
          , claimProcedureSequence = sequence
          , claimProcedureType = ty
          , claimProcedureDate = date
          , claimProcedureProcedure = procedure
          , claimProcedureUdi = udi
          }
    where 
      parseProcedure o = parseProcedureCodeableConcept o <|> parseProcedureReference o
      parseProcedureCodeableConcept o = do
                has <- o .: "procedureCodeableConcept"
                return $ ClaimProcedureProcedureCodeableConcept has
      parseProcedureReference o = do
                has <- o .: "procedureReference"
                return $ ClaimProcedureProcedureReference has
instance Xmlbf.ToXml ClaimProcedure where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimProcedureAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimProcedureExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimProcedureModifierExtension p))
             , Val      "sequence" (     toPositiveInt (claimProcedureSequence p))
             , PropList "type" (fmap Xmlbf.toXml (claimProcedureType p))
             , OptVal   "date" (fmap toDateTime (claimProcedureDate p))
             , toProcedureXml (claimProcedureProcedure p)
             , PropList "udi" (fmap Xmlbf.toXml (claimProcedureUdi p))
             ]
       where 
          toProcedureXml (     (ClaimProcedureProcedureCodeableConcept p)) = Prop     "procedureCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toProcedureXml (     (ClaimProcedureProcedureReference p)) = Prop     "procedureReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml ClaimProcedure where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    sequence <-            Xmlbf.pElement "sequence" (Xmlbf.pAttr "value")
    ty <- many     $ Xmlbf.pElement "type" Xmlbf.fromXml
    date <- optional $ Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    procedure <- fromProcedureXml
    udi <- many     $ Xmlbf.pElement "udi" Xmlbf.fromXml
    return ClaimProcedure {
            claimProcedureAttrId = id
          , claimProcedureExtension = extension
          , claimProcedureModifierExtension = modifierExtension
          , claimProcedureSequence =      fromPositiveInt sequence
          , claimProcedureType = ty
          , claimProcedureDate = fmap fromDateTime date
          , claimProcedureProcedure = procedure
          , claimProcedureUdi = udi
          }

    where 
      fromProcedureXml = parseProcedureCodeableConcept <|> parseProcedureReference
      parseProcedureCodeableConcept = do
                has <- Xmlbf.pElement "procedureCodeableConcept" Xmlbf.fromXml
                return $ ClaimProcedureProcedureCodeableConcept (                      has)
      parseProcedureReference = do
                has <- Xmlbf.pElement "procedureReference" Xmlbf.fromXml
                return $ ClaimProcedureProcedureReference (                      has)


data ClaimInsurance = ClaimInsurance {
    claimInsuranceAttrId :: Maybe Text
  , claimInsuranceExtension :: [Extension]
  , claimInsuranceModifierExtension :: [Extension]
  , claimInsuranceSequence :: PositiveInt
  , claimInsuranceFocal :: Boolean
  , claimInsuranceIdentifier :: Maybe Identifier
  , claimInsuranceCoverage :: Reference
  , claimInsuranceBusinessArrangement :: Maybe Text
  , claimInsurancePreAuthRef :: [Text]
  , claimInsuranceClaimResponse :: Maybe Reference
  }
--

instance ToJSON ClaimInsurance where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimInsuranceAttrId p)
    ,  "extension" .= toJSON (claimInsuranceExtension p)
    ,  "modifierExtension" .= toJSON (claimInsuranceModifierExtension p)
    ,  "sequence" .= toJSON (claimInsuranceSequence p)
    ,  "focal" .= toJSON (claimInsuranceFocal p)
    ,  "identifier" .= toJSON (claimInsuranceIdentifier p)
    ,  "coverage" .= toJSON (claimInsuranceCoverage p)
    ,  "businessArrangement" .= toJSON (claimInsuranceBusinessArrangement p)
    ,  "preAuthRef" .= toJSON (claimInsurancePreAuthRef p)
    ,  "claimResponse" .= toJSON (claimInsuranceClaimResponse p)
    ]
instance FromJSON ClaimInsurance where
  parseJSON = withObject "ClaimInsurance" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        sequence <- o .:  "sequence"
        focal <- o .:  "focal"
        identifier <- o .:? "identifier"
        coverage <- o .:  "coverage"
        businessArrangement <- o .:? "businessArrangement"
        preAuthRef <- o .:? "preAuthRef" .!= []
        claimResponse <- o .:? "claimResponse"
        return ClaimInsurance{
            claimInsuranceAttrId = id
          , claimInsuranceExtension = extension
          , claimInsuranceModifierExtension = modifierExtension
          , claimInsuranceSequence = sequence
          , claimInsuranceFocal = focal
          , claimInsuranceIdentifier = identifier
          , claimInsuranceCoverage = coverage
          , claimInsuranceBusinessArrangement = businessArrangement
          , claimInsurancePreAuthRef = preAuthRef
          , claimInsuranceClaimResponse = claimResponse
          }
instance Xmlbf.ToXml ClaimInsurance where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimInsuranceAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimInsuranceExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimInsuranceModifierExtension p))
             , Val      "sequence" (     toPositiveInt (claimInsuranceSequence p))
             , Val      "focal" (     toBoolean (claimInsuranceFocal p))
             , OptProp  "identifier" (fmap Xmlbf.toXml (claimInsuranceIdentifier p))
             , Prop     "coverage" (HM.empty, Xmlbf.toXml (claimInsuranceCoverage p))
             , OptVal   "businessArrangement" (fmap toString (claimInsuranceBusinessArrangement p))
             , ValList  "preAuthRef" (fmap toString (claimInsurancePreAuthRef p))
             , OptProp  "claimResponse" (fmap Xmlbf.toXml (claimInsuranceClaimResponse p))
             ]
instance Xmlbf.FromXml ClaimInsurance where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    sequence <-            Xmlbf.pElement "sequence" (Xmlbf.pAttr "value")
    focal <-            Xmlbf.pElement "focal" (Xmlbf.pAttr "value")
    identifier <- optional $ Xmlbf.pElement "identifier" Xmlbf.fromXml
    coverage <-            Xmlbf.pElement "coverage" Xmlbf.fromXml
    businessArrangement <- optional $ Xmlbf.pElement "businessArrangement" (Xmlbf.pAttr "value")
    preAuthRef <- many     $ Xmlbf.pElement "preAuthRef" (Xmlbf.pAttr "value")
    claimResponse <- optional $ Xmlbf.pElement "claimResponse" Xmlbf.fromXml
    return ClaimInsurance {
            claimInsuranceAttrId = id
          , claimInsuranceExtension = extension
          , claimInsuranceModifierExtension = modifierExtension
          , claimInsuranceSequence =      fromPositiveInt sequence
          , claimInsuranceFocal =      fromBoolean focal
          , claimInsuranceIdentifier = identifier
          , claimInsuranceCoverage = coverage
          , claimInsuranceBusinessArrangement = fmap fromString businessArrangement
          , claimInsurancePreAuthRef = fmap fromString preAuthRef
          , claimInsuranceClaimResponse = claimResponse
          }



data ClaimSupportingInfoTiming
    = ClaimSupportingInfoTimingDate Date
    | ClaimSupportingInfoTimingPeriod Period
    deriving (Eq, Show)

data ClaimSupportingInfoValue
    = ClaimSupportingInfoValueBoolean Boolean
    | ClaimSupportingInfoValueString Text
    | ClaimSupportingInfoValueQuantity Quantity
    | ClaimSupportingInfoValueAttachment Attachment
    | ClaimSupportingInfoValueReference Reference
    deriving (Eq, Show)

data ClaimSupportingInfo = ClaimSupportingInfo {
    claimSupportingInfoAttrId :: Maybe Text
  , claimSupportingInfoExtension :: [Extension]
  , claimSupportingInfoModifierExtension :: [Extension]
  , claimSupportingInfoSequence :: PositiveInt
  , claimSupportingInfoCategory :: CodeableConcept
  , claimSupportingInfoCode :: Maybe CodeableConcept
  , claimSupportingInfoTiming :: Maybe ClaimSupportingInfoTiming
  , claimSupportingInfoValue :: Maybe ClaimSupportingInfoValue
  , claimSupportingInfoReason :: Maybe CodeableConcept
  }
--

instance ToJSON ClaimSupportingInfo where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimSupportingInfoAttrId p)
    ,  "extension" .= toJSON (claimSupportingInfoExtension p)
    ,  "modifierExtension" .= toJSON (claimSupportingInfoModifierExtension p)
    ,  "sequence" .= toJSON (claimSupportingInfoSequence p)
    ,  "category" .= toJSON (claimSupportingInfoCategory p)
    ,  "code" .= toJSON (claimSupportingInfoCode p)
    , toTimingJSON (claimSupportingInfoTiming p)
    , toValueJSON (claimSupportingInfoValue p)
    ,  "reason" .= toJSON (claimSupportingInfoReason p)
    ]
    where 
      toTimingJSON (     Nothing   ) = ("timing", Null)
      toTimingJSON (Just (ClaimSupportingInfoTimingDate c)) = ("timing", toJSON c)
      toTimingJSON (Just (ClaimSupportingInfoTimingPeriod c)) = ("timing", toJSON c)
      toValueJSON (     Nothing   ) = ("value", Null)
      toValueJSON (Just (ClaimSupportingInfoValueBoolean c)) = ("value", toJSON c)
      toValueJSON (Just (ClaimSupportingInfoValueString c)) = ("value", toJSON c)
      toValueJSON (Just (ClaimSupportingInfoValueQuantity c)) = ("value", toJSON c)
      toValueJSON (Just (ClaimSupportingInfoValueAttachment c)) = ("value", toJSON c)
      toValueJSON (Just (ClaimSupportingInfoValueReference c)) = ("value", toJSON c)
instance FromJSON ClaimSupportingInfo where
  parseJSON = withObject "ClaimSupportingInfo" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        sequence <- o .:  "sequence"
        category <- o .:  "category"
        code <- o .:? "code"
        timing <- parseTiming o
        value <- parseValue o
        reason <- o .:? "reason"
        return ClaimSupportingInfo{
            claimSupportingInfoAttrId = id
          , claimSupportingInfoExtension = extension
          , claimSupportingInfoModifierExtension = modifierExtension
          , claimSupportingInfoSequence = sequence
          , claimSupportingInfoCategory = category
          , claimSupportingInfoCode = code
          , claimSupportingInfoTiming = timing
          , claimSupportingInfoValue = value
          , claimSupportingInfoReason = reason
          }
    where 
      parseTiming o = parseTimingDate o <|> parseTimingPeriod o
      parseTimingDate o = do
                has <- o .: "timingDate"
                return $ Just (ClaimSupportingInfoTimingDate has)
      parseTimingPeriod o = do
                has <- o .: "timingPeriod"
                return $ Just (ClaimSupportingInfoTimingPeriod has)
      parseValue o = parseValueBoolean o <|> parseValueString o <|> parseValueQuantity o <|> parseValueAttachment o <|> parseValueReference o
      parseValueBoolean o = do
                has <- o .: "valueBoolean"
                return $ Just (ClaimSupportingInfoValueBoolean has)
      parseValueString o = do
                has <- o .: "valueString"
                return $ Just (ClaimSupportingInfoValueString has)
      parseValueQuantity o = do
                has <- o .: "valueQuantity"
                return $ Just (ClaimSupportingInfoValueQuantity has)
      parseValueAttachment o = do
                has <- o .: "valueAttachment"
                return $ Just (ClaimSupportingInfoValueAttachment has)
      parseValueReference o = do
                has <- o .: "valueReference"
                return $ Just (ClaimSupportingInfoValueReference has)
instance Xmlbf.ToXml ClaimSupportingInfo where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimSupportingInfoAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimSupportingInfoExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimSupportingInfoModifierExtension p))
             , Val      "sequence" (     toPositiveInt (claimSupportingInfoSequence p))
             , Prop     "category" (HM.empty, Xmlbf.toXml (claimSupportingInfoCategory p))
             , OptProp  "code" (fmap Xmlbf.toXml (claimSupportingInfoCode p))
             , toTimingXml (claimSupportingInfoTiming p)
             , toValueXml (claimSupportingInfoValue p)
             , OptProp  "reason" (fmap Xmlbf.toXml (claimSupportingInfoReason p))
             ]
       where 
          toTimingXml ( Nothing   ) = (OptVal "timing" Nothing)
          toTimingXml (Just (ClaimSupportingInfoTimingDate p)) = Val   "timingDate" (toDate p)
          toTimingXml (Just (ClaimSupportingInfoTimingPeriod p)) = Prop  "timingPeriod" (HM.empty, Xmlbf.toXml p)
          toValueXml ( Nothing   ) = (OptVal "value" Nothing)
          toValueXml (Just (ClaimSupportingInfoValueBoolean p)) = Val   "valueBoolean" (toBoolean p)
          toValueXml (Just (ClaimSupportingInfoValueString p)) = Val   "valueString" (toString p)
          toValueXml (Just (ClaimSupportingInfoValueQuantity p)) = Prop  "valueQuantity" (HM.empty, Xmlbf.toXml p)
          toValueXml (Just (ClaimSupportingInfoValueAttachment p)) = Prop  "valueAttachment" (HM.empty, Xmlbf.toXml p)
          toValueXml (Just (ClaimSupportingInfoValueReference p)) = Prop  "valueReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml ClaimSupportingInfo where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    sequence <-            Xmlbf.pElement "sequence" (Xmlbf.pAttr "value")
    category <-            Xmlbf.pElement "category" Xmlbf.fromXml
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    timing <- fromTimingXml
    value <- fromValueXml
    reason <- optional $ Xmlbf.pElement "reason" Xmlbf.fromXml
    return ClaimSupportingInfo {
            claimSupportingInfoAttrId = id
          , claimSupportingInfoExtension = extension
          , claimSupportingInfoModifierExtension = modifierExtension
          , claimSupportingInfoSequence =      fromPositiveInt sequence
          , claimSupportingInfoCategory = category
          , claimSupportingInfoCode = code
          , claimSupportingInfoTiming = timing
          , claimSupportingInfoValue = value
          , claimSupportingInfoReason = reason
          }

    where 
      fromTimingXml = parseTimingDate <|> parseTimingPeriod <|> pure Nothing
      parseTimingDate = do
                has <- Xmlbf.pElement "timingDate" (Xmlbf.pAttr "value")
                return $ Just (ClaimSupportingInfoTimingDate (     fromDate has))
      parseTimingPeriod = do
                has <- Xmlbf.pElement "timingPeriod" Xmlbf.fromXml
                return $ Just (ClaimSupportingInfoTimingPeriod (                      has))
      fromValueXml = parseValueBoolean <|> parseValueString <|> parseValueQuantity <|> parseValueAttachment <|> parseValueReference <|> pure Nothing
      parseValueBoolean = do
                has <- Xmlbf.pElement "valueBoolean" (Xmlbf.pAttr "value")
                return $ Just (ClaimSupportingInfoValueBoolean (     fromBoolean has))
      parseValueString = do
                has <- Xmlbf.pElement "valueString" (Xmlbf.pAttr "value")
                return $ Just (ClaimSupportingInfoValueString (     fromString has))
      parseValueQuantity = do
                has <- Xmlbf.pElement "valueQuantity" Xmlbf.fromXml
                return $ Just (ClaimSupportingInfoValueQuantity (                      has))
      parseValueAttachment = do
                has <- Xmlbf.pElement "valueAttachment" Xmlbf.fromXml
                return $ Just (ClaimSupportingInfoValueAttachment (                      has))
      parseValueReference = do
                has <- Xmlbf.pElement "valueReference" Xmlbf.fromXml
                return $ Just (ClaimSupportingInfoValueReference (                      has))


data ClaimResponseTotal = ClaimResponseTotal {
    claimResponseTotalAttrId :: Maybe Text
  , claimResponseTotalExtension :: [Extension]
  , claimResponseTotalModifierExtension :: [Extension]
  , claimResponseTotalCategory :: CodeableConcept
  , claimResponseTotalAmount :: Money
  }
--

instance ToJSON ClaimResponseTotal where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimResponseTotalAttrId p)
    ,  "extension" .= toJSON (claimResponseTotalExtension p)
    ,  "modifierExtension" .= toJSON (claimResponseTotalModifierExtension p)
    ,  "category" .= toJSON (claimResponseTotalCategory p)
    ,  "amount" .= toJSON (claimResponseTotalAmount p)
    ]
instance FromJSON ClaimResponseTotal where
  parseJSON = withObject "ClaimResponseTotal" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        category <- o .:  "category"
        amount <- o .:  "amount"
        return ClaimResponseTotal{
            claimResponseTotalAttrId = id
          , claimResponseTotalExtension = extension
          , claimResponseTotalModifierExtension = modifierExtension
          , claimResponseTotalCategory = category
          , claimResponseTotalAmount = amount
          }
instance Xmlbf.ToXml ClaimResponseTotal where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimResponseTotalAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimResponseTotalExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimResponseTotalModifierExtension p))
             , Prop     "category" (HM.empty, Xmlbf.toXml (claimResponseTotalCategory p))
             , Prop     "amount" (HM.empty, Xmlbf.toXml (claimResponseTotalAmount p))
             ]
instance Xmlbf.FromXml ClaimResponseTotal where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    category <-            Xmlbf.pElement "category" Xmlbf.fromXml
    amount <-            Xmlbf.pElement "amount" Xmlbf.fromXml
    return ClaimResponseTotal {
            claimResponseTotalAttrId = id
          , claimResponseTotalExtension = extension
          , claimResponseTotalModifierExtension = modifierExtension
          , claimResponseTotalCategory = category
          , claimResponseTotalAmount = amount
          }



data ClaimResponseDetail1 = ClaimResponseDetail1 {
    claimResponseDetail1AttrId :: Maybe Text
  , claimResponseDetail1Extension :: [Extension]
  , claimResponseDetail1ModifierExtension :: [Extension]
  , claimResponseDetail1ProductOrService :: CodeableConcept
  , claimResponseDetail1Modifier :: [CodeableConcept]
  , claimResponseDetail1Quantity :: Maybe Quantity
  , claimResponseDetail1UnitPrice :: Maybe Money
  , claimResponseDetail1Factor :: Maybe Decimal
  , claimResponseDetail1Net :: Maybe Money
  , claimResponseDetail1NoteNumber :: [PositiveInt]
  , claimResponseDetail1Adjudication :: [ClaimResponseAdjudication]
  , claimResponseDetail1SubDetail :: [ClaimResponseSubDetail1]
  }
--

instance ToJSON ClaimResponseDetail1 where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimResponseDetail1AttrId p)
    ,  "extension" .= toJSON (claimResponseDetail1Extension p)
    ,  "modifierExtension" .= toJSON (claimResponseDetail1ModifierExtension p)
    ,  "productOrService" .= toJSON (claimResponseDetail1ProductOrService p)
    ,  "modifier" .= toJSON (claimResponseDetail1Modifier p)
    ,  "quantity" .= toJSON (claimResponseDetail1Quantity p)
    ,  "unitPrice" .= toJSON (claimResponseDetail1UnitPrice p)
    ,  "factor" .= toJSON (claimResponseDetail1Factor p)
    ,  "net" .= toJSON (claimResponseDetail1Net p)
    ,  "noteNumber" .= toJSON (claimResponseDetail1NoteNumber p)
    ,  "adjudication" .= toJSON (claimResponseDetail1Adjudication p)
    ,  "subDetail" .= toJSON (claimResponseDetail1SubDetail p)
    ]
instance FromJSON ClaimResponseDetail1 where
  parseJSON = withObject "ClaimResponseDetail1" $ \o -> do
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
        return ClaimResponseDetail1{
            claimResponseDetail1AttrId = id
          , claimResponseDetail1Extension = extension
          , claimResponseDetail1ModifierExtension = modifierExtension
          , claimResponseDetail1ProductOrService = productOrService
          , claimResponseDetail1Modifier = modifier
          , claimResponseDetail1Quantity = quantity
          , claimResponseDetail1UnitPrice = unitPrice
          , claimResponseDetail1Factor = factor
          , claimResponseDetail1Net = net
          , claimResponseDetail1NoteNumber = noteNumber
          , claimResponseDetail1Adjudication = adjudication
          , claimResponseDetail1SubDetail = subDetail
          }
instance Xmlbf.ToXml ClaimResponseDetail1 where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimResponseDetail1AttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimResponseDetail1Extension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimResponseDetail1ModifierExtension p))
             , Prop     "productOrService" (HM.empty, Xmlbf.toXml (claimResponseDetail1ProductOrService p))
             , PropList "modifier" (fmap Xmlbf.toXml (claimResponseDetail1Modifier p))
             , OptProp  "quantity" (fmap Xmlbf.toXml (claimResponseDetail1Quantity p))
             , OptProp  "unitPrice" (fmap Xmlbf.toXml (claimResponseDetail1UnitPrice p))
             , OptVal   "factor" (fmap toDecimal (claimResponseDetail1Factor p))
             , OptProp  "net" (fmap Xmlbf.toXml (claimResponseDetail1Net p))
             , ValList  "noteNumber" (fmap toPositiveInt (claimResponseDetail1NoteNumber p))
             , PropList "adjudication" (fmap Xmlbf.toXml (claimResponseDetail1Adjudication p))
             , PropList "subDetail" (fmap Xmlbf.toXml (claimResponseDetail1SubDetail p))
             ]
instance Xmlbf.FromXml ClaimResponseDetail1 where
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
    return ClaimResponseDetail1 {
            claimResponseDetail1AttrId = id
          , claimResponseDetail1Extension = extension
          , claimResponseDetail1ModifierExtension = modifierExtension
          , claimResponseDetail1ProductOrService = productOrService
          , claimResponseDetail1Modifier = modifier
          , claimResponseDetail1Quantity = quantity
          , claimResponseDetail1UnitPrice = unitPrice
          , claimResponseDetail1Factor = fmap fromDecimal factor
          , claimResponseDetail1Net = net
          , claimResponseDetail1NoteNumber = fmap fromPositiveInt noteNumber
          , claimResponseDetail1Adjudication = adjudication
          , claimResponseDetail1SubDetail = subDetail
          }



data ClaimResponseItem = ClaimResponseItem {
    claimResponseItemAttrId :: Maybe Text
  , claimResponseItemExtension :: [Extension]
  , claimResponseItemModifierExtension :: [Extension]
  , claimResponseItemItemSequence :: PositiveInt
  , claimResponseItemNoteNumber :: [PositiveInt]
  , claimResponseItemAdjudication :: [ClaimResponseAdjudication]
  , claimResponseItemDetail :: [ClaimResponseDetail]
  }
--

instance ToJSON ClaimResponseItem where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimResponseItemAttrId p)
    ,  "extension" .= toJSON (claimResponseItemExtension p)
    ,  "modifierExtension" .= toJSON (claimResponseItemModifierExtension p)
    ,  "itemSequence" .= toJSON (claimResponseItemItemSequence p)
    ,  "noteNumber" .= toJSON (claimResponseItemNoteNumber p)
    ,  "adjudication" .= toJSON (claimResponseItemAdjudication p)
    ,  "detail" .= toJSON (claimResponseItemDetail p)
    ]
instance FromJSON ClaimResponseItem where
  parseJSON = withObject "ClaimResponseItem" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        itemSequence <- o .:  "itemSequence"
        noteNumber <- o .:? "noteNumber" .!= []
        adjudication <- o .:? "adjudication" .!= []
        detail <- o .:? "detail" .!= []
        return ClaimResponseItem{
            claimResponseItemAttrId = id
          , claimResponseItemExtension = extension
          , claimResponseItemModifierExtension = modifierExtension
          , claimResponseItemItemSequence = itemSequence
          , claimResponseItemNoteNumber = noteNumber
          , claimResponseItemAdjudication = adjudication
          , claimResponseItemDetail = detail
          }
instance Xmlbf.ToXml ClaimResponseItem where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimResponseItemAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimResponseItemExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimResponseItemModifierExtension p))
             , Val      "itemSequence" (     toPositiveInt (claimResponseItemItemSequence p))
             , ValList  "noteNumber" (fmap toPositiveInt (claimResponseItemNoteNumber p))
             , PropList "adjudication" (fmap Xmlbf.toXml (claimResponseItemAdjudication p))
             , PropList "detail" (fmap Xmlbf.toXml (claimResponseItemDetail p))
             ]
instance Xmlbf.FromXml ClaimResponseItem where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    itemSequence <-            Xmlbf.pElement "itemSequence" (Xmlbf.pAttr "value")
    noteNumber <- many     $ Xmlbf.pElement "noteNumber" (Xmlbf.pAttr "value")
    adjudication <- many     $ Xmlbf.pElement "adjudication" Xmlbf.fromXml
    detail <- many     $ Xmlbf.pElement "detail" Xmlbf.fromXml
    return ClaimResponseItem {
            claimResponseItemAttrId = id
          , claimResponseItemExtension = extension
          , claimResponseItemModifierExtension = modifierExtension
          , claimResponseItemItemSequence =      fromPositiveInt itemSequence
          , claimResponseItemNoteNumber = fmap fromPositiveInt noteNumber
          , claimResponseItemAdjudication = adjudication
          , claimResponseItemDetail = detail
          }



data ClaimResponseStatus
    = CRSActive
    | CRSCancelled
    | CRSDraft
    | CRSEnteredInError
  deriving (Eq, Show)

instance ToJSON ClaimResponseStatus where
    toJSON CRSActive = String "active"
    toJSON CRSCancelled = String "cancelled"
    toJSON CRSDraft = String "draft"
    toJSON CRSEnteredInError = String "entered-in-error"
instance FromJSON ClaimResponseStatus where
    parseJSON "active" = return CRSActive
    parseJSON "cancelled" = return CRSCancelled
    parseJSON "draft" = return CRSDraft
    parseJSON "entered-in-error" = return CRSEnteredInError

toClaimResponseStatus CRSActive = "active"
toClaimResponseStatus CRSCancelled = "cancelled"
toClaimResponseStatus CRSDraft = "draft"
toClaimResponseStatus CRSEnteredInError = "entered-in-error"
fromClaimResponseStatus "active" = CRSActive
fromClaimResponseStatus "cancelled" = CRSCancelled
fromClaimResponseStatus "draft" = CRSDraft
fromClaimResponseStatus "entered-in-error" = CRSEnteredInError


data ClaimResponseUse
    = CRUClaim
    | CRUPreauthorization
    | CRUPredetermination
  deriving (Eq, Show)

instance ToJSON ClaimResponseUse where
    toJSON CRUClaim = String "claim"
    toJSON CRUPreauthorization = String "preauthorization"
    toJSON CRUPredetermination = String "predetermination"
instance FromJSON ClaimResponseUse where
    parseJSON "claim" = return CRUClaim
    parseJSON "preauthorization" = return CRUPreauthorization
    parseJSON "predetermination" = return CRUPredetermination

toClaimResponseUse CRUClaim = "claim"
toClaimResponseUse CRUPreauthorization = "preauthorization"
toClaimResponseUse CRUPredetermination = "predetermination"
fromClaimResponseUse "claim" = CRUClaim
fromClaimResponseUse "preauthorization" = CRUPreauthorization
fromClaimResponseUse "predetermination" = CRUPredetermination


data ClaimResponseOutcome
    = CROQueued
    | CROComplete
    | CROError
    | CROPartial
  deriving (Eq, Show)

instance ToJSON ClaimResponseOutcome where
    toJSON CROQueued = String "queued"
    toJSON CROComplete = String "complete"
    toJSON CROError = String "error"
    toJSON CROPartial = String "partial"
instance FromJSON ClaimResponseOutcome where
    parseJSON "queued" = return CROQueued
    parseJSON "complete" = return CROComplete
    parseJSON "error" = return CROError
    parseJSON "partial" = return CROPartial

toClaimResponseOutcome CROQueued = "queued"
toClaimResponseOutcome CROComplete = "complete"
toClaimResponseOutcome CROError = "error"
toClaimResponseOutcome CROPartial = "partial"
fromClaimResponseOutcome "queued" = CROQueued
fromClaimResponseOutcome "complete" = CROComplete
fromClaimResponseOutcome "error" = CROError
fromClaimResponseOutcome "partial" = CROPartial


data ClaimResponse = ClaimResponse {
    claimResponseId :: Maybe Id
  , claimResponseMeta :: Maybe Meta
  , claimResponseImplicitRules :: Maybe Uri
  , claimResponseLanguage :: Maybe Language
  , claimResponseText :: Maybe Narrative
--    claimResponseContained :: [ResourceContainer]
  , claimResponseExtension :: [Extension]
  , claimResponseModifierExtension :: [Extension]
  , claimResponseIdentifier :: [Identifier]
  , claimResponseStatus :: ClaimResponseStatus
  , claimResponseType :: CodeableConcept
  , claimResponseSubType :: Maybe CodeableConcept
  , claimResponseUse :: ClaimResponseUse
  , claimResponsePatient :: Reference
  , claimResponseCreated :: DateTime
  , claimResponseInsurer :: Reference
  , claimResponseRequestor :: Maybe Reference
  , claimResponseRequest :: Maybe Reference
  , claimResponseOutcome :: ClaimResponseOutcome
  , claimResponseDisposition :: Maybe Text
  , claimResponsePreAuthRef :: Maybe Text
  , claimResponsePreAuthPeriod :: Maybe Period
  , claimResponsePayeeType :: Maybe CodeableConcept
  , claimResponseItem :: [ClaimResponseItem]
  , claimResponseAddItem :: [ClaimResponseAddItem]
  , claimResponseAdjudication :: [ClaimResponseAdjudication]
  , claimResponseTotal :: [ClaimResponseTotal]
  , claimResponsePayment :: Maybe ClaimResponsePayment
  , claimResponseFundsReserve :: Maybe CodeableConcept
  , claimResponseFormCode :: Maybe CodeableConcept
  , claimResponseForm :: Maybe Attachment
  , claimResponseProcessNote :: [ClaimResponseProcessNote]
  , claimResponseCommunicationRequest :: [Reference]
  , claimResponseInsurance :: [ClaimResponseInsurance]
  , claimResponseError :: [ClaimResponseError]
  }
--

instance ToJSON ClaimResponse where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "ClaimResponse")
    ,  "id" .= toJSON (claimResponseId p)
    ,  "meta" .= toJSON (claimResponseMeta p)
    ,  "implicitRules" .= toJSON (claimResponseImplicitRules p)
    ,  "language" .= toJSON (claimResponseLanguage p)
    ,  "text" .= toJSON (claimResponseText p)
--    , "contained" .= toJSON (claimResponseContained p)
    ,  "extension" .= toJSON (claimResponseExtension p)
    ,  "modifierExtension" .= toJSON (claimResponseModifierExtension p)
    ,  "identifier" .= toJSON (claimResponseIdentifier p)
    ,  "status" .= toJSON (claimResponseStatus p)
    ,  "type" .= toJSON (claimResponseType p)
    ,  "subType" .= toJSON (claimResponseSubType p)
    ,  "use" .= toJSON (claimResponseUse p)
    ,  "patient" .= toJSON (claimResponsePatient p)
    ,  "created" .= toJSON (claimResponseCreated p)
    ,  "insurer" .= toJSON (claimResponseInsurer p)
    ,  "requestor" .= toJSON (claimResponseRequestor p)
    ,  "request" .= toJSON (claimResponseRequest p)
    ,  "outcome" .= toJSON (claimResponseOutcome p)
    ,  "disposition" .= toJSON (claimResponseDisposition p)
    ,  "preAuthRef" .= toJSON (claimResponsePreAuthRef p)
    ,  "preAuthPeriod" .= toJSON (claimResponsePreAuthPeriod p)
    ,  "payeeType" .= toJSON (claimResponsePayeeType p)
    ,  "item" .= toJSON (claimResponseItem p)
    ,  "addItem" .= toJSON (claimResponseAddItem p)
    ,  "adjudication" .= toJSON (claimResponseAdjudication p)
    ,  "total" .= toJSON (claimResponseTotal p)
    ,  "payment" .= toJSON (claimResponsePayment p)
    ,  "fundsReserve" .= toJSON (claimResponseFundsReserve p)
    ,  "formCode" .= toJSON (claimResponseFormCode p)
    ,  "form" .= toJSON (claimResponseForm p)
    ,  "processNote" .= toJSON (claimResponseProcessNote p)
    ,  "communicationRequest" .= toJSON (claimResponseCommunicationRequest p)
    ,  "insurance" .= toJSON (claimResponseInsurance p)
    ,  "error" .= toJSON (claimResponseError p)
    ]
instance FromJSON ClaimResponse where
  parseJSON = withObject "ClaimResponse" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "ClaimResponse" -> do
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
        created <- o .:  "created"
        insurer <- o .:  "insurer"
        requestor <- o .:? "requestor"
        request <- o .:? "request"
        outcome <- o .:  "outcome"
        disposition <- o .:? "disposition"
        preAuthRef <- o .:? "preAuthRef"
        preAuthPeriod <- o .:? "preAuthPeriod"
        payeeType <- o .:? "payeeType"
        item <- o .:? "item" .!= []
        addItem <- o .:? "addItem" .!= []
        adjudication <- o .:? "adjudication" .!= []
        total <- o .:? "total" .!= []
        payment <- o .:? "payment"
        fundsReserve <- o .:? "fundsReserve"
        formCode <- o .:? "formCode"
        form <- o .:? "form"
        processNote <- o .:? "processNote" .!= []
        communicationRequest <- o .:? "communicationRequest" .!= []
        insurance <- o .:? "insurance" .!= []
        error <- o .:? "error" .!= []
        return ClaimResponse{
            claimResponseId = id
          , claimResponseMeta = meta
          , claimResponseImplicitRules = implicitRules
          , claimResponseLanguage = language
          , claimResponseText = text
--          , claimResponseContained = contained
          , claimResponseExtension = extension
          , claimResponseModifierExtension = modifierExtension
          , claimResponseIdentifier = identifier
          , claimResponseStatus = status
          , claimResponseType = ty
          , claimResponseSubType = subType
          , claimResponseUse = use
          , claimResponsePatient = patient
          , claimResponseCreated = created
          , claimResponseInsurer = insurer
          , claimResponseRequestor = requestor
          , claimResponseRequest = request
          , claimResponseOutcome = outcome
          , claimResponseDisposition = disposition
          , claimResponsePreAuthRef = preAuthRef
          , claimResponsePreAuthPeriod = preAuthPeriod
          , claimResponsePayeeType = payeeType
          , claimResponseItem = item
          , claimResponseAddItem = addItem
          , claimResponseAdjudication = adjudication
          , claimResponseTotal = total
          , claimResponsePayment = payment
          , claimResponseFundsReserve = fundsReserve
          , claimResponseFormCode = formCode
          , claimResponseForm = form
          , claimResponseProcessNote = processNote
          , claimResponseCommunicationRequest = communicationRequest
          , claimResponseInsurance = insurance
          , claimResponseError = error
          }
      _ -> fail "not a ClaimResponse"
instance Xmlbf.ToXml ClaimResponse where
  toXml p = Xmlbf.element "ClaimResponse" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (claimResponseId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (claimResponseMeta p))
             , OptVal   "implicitRules" (fmap toUri (claimResponseImplicitRules p))
             , OptVal   "language" (fmap toLanguage (claimResponseLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (claimResponseText p))
--             , PropList "contained" (fmap Xmlbf.toXml (claimResponseContained p))
             , PropList "extension" (fmap Xmlbf.toXml (claimResponseExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimResponseModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (claimResponseIdentifier p))
             , Val      "status" (     toClaimResponseStatus (claimResponseStatus p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (claimResponseType p))
             , OptProp  "subType" (fmap Xmlbf.toXml (claimResponseSubType p))
             , Val      "use" (     toClaimResponseUse (claimResponseUse p))
             , Prop     "patient" (HM.empty, Xmlbf.toXml (claimResponsePatient p))
             , Val      "created" (     toDateTime (claimResponseCreated p))
             , Prop     "insurer" (HM.empty, Xmlbf.toXml (claimResponseInsurer p))
             , OptProp  "requestor" (fmap Xmlbf.toXml (claimResponseRequestor p))
             , OptProp  "request" (fmap Xmlbf.toXml (claimResponseRequest p))
             , Val      "outcome" (     toClaimResponseOutcome (claimResponseOutcome p))
             , OptVal   "disposition" (fmap toString (claimResponseDisposition p))
             , OptVal   "preAuthRef" (fmap toString (claimResponsePreAuthRef p))
             , OptProp  "preAuthPeriod" (fmap Xmlbf.toXml (claimResponsePreAuthPeriod p))
             , OptProp  "payeeType" (fmap Xmlbf.toXml (claimResponsePayeeType p))
             , PropList "item" (fmap Xmlbf.toXml (claimResponseItem p))
             , PropList "addItem" (fmap Xmlbf.toXml (claimResponseAddItem p))
             , PropList "adjudication" (fmap Xmlbf.toXml (claimResponseAdjudication p))
             , PropList "total" (fmap Xmlbf.toXml (claimResponseTotal p))
             , OptProp  "payment" (fmap Xmlbf.toXml (claimResponsePayment p))
             , OptProp  "fundsReserve" (fmap Xmlbf.toXml (claimResponseFundsReserve p))
             , OptProp  "formCode" (fmap Xmlbf.toXml (claimResponseFormCode p))
             , OptProp  "form" (fmap Xmlbf.toXml (claimResponseForm p))
             , PropList "processNote" (fmap Xmlbf.toXml (claimResponseProcessNote p))
             , PropList "communicationRequest" (fmap Xmlbf.toXml (claimResponseCommunicationRequest p))
             , PropList "insurance" (fmap Xmlbf.toXml (claimResponseInsurance p))
             , PropList "error" (fmap Xmlbf.toXml (claimResponseError p))
             ]
instance Xmlbf.FromXml ClaimResponse where
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
    created <-            Xmlbf.pElement "created" (Xmlbf.pAttr "value")
    insurer <-            Xmlbf.pElement "insurer" Xmlbf.fromXml
    requestor <- optional $ Xmlbf.pElement "requestor" Xmlbf.fromXml
    request <- optional $ Xmlbf.pElement "request" Xmlbf.fromXml
    outcome <-            Xmlbf.pElement "outcome" (Xmlbf.pAttr "value")
    disposition <- optional $ Xmlbf.pElement "disposition" (Xmlbf.pAttr "value")
    preAuthRef <- optional $ Xmlbf.pElement "preAuthRef" (Xmlbf.pAttr "value")
    preAuthPeriod <- optional $ Xmlbf.pElement "preAuthPeriod" Xmlbf.fromXml
    payeeType <- optional $ Xmlbf.pElement "payeeType" Xmlbf.fromXml
    item <- many     $ Xmlbf.pElement "item" Xmlbf.fromXml
    addItem <- many     $ Xmlbf.pElement "addItem" Xmlbf.fromXml
    adjudication <- many     $ Xmlbf.pElement "adjudication" Xmlbf.fromXml
    total <- many     $ Xmlbf.pElement "total" Xmlbf.fromXml
    payment <- optional $ Xmlbf.pElement "payment" Xmlbf.fromXml
    fundsReserve <- optional $ Xmlbf.pElement "fundsReserve" Xmlbf.fromXml
    formCode <- optional $ Xmlbf.pElement "formCode" Xmlbf.fromXml
    form <- optional $ Xmlbf.pElement "form" Xmlbf.fromXml
    processNote <- many     $ Xmlbf.pElement "processNote" Xmlbf.fromXml
    communicationRequest <- many     $ Xmlbf.pElement "communicationRequest" Xmlbf.fromXml
    insurance <- many     $ Xmlbf.pElement "insurance" Xmlbf.fromXml
    error <- many     $ Xmlbf.pElement "error" Xmlbf.fromXml
    return ClaimResponse {
            claimResponseId = fmap fromId id
          , claimResponseMeta = meta
          , claimResponseImplicitRules = fmap fromUri implicitRules
          , claimResponseLanguage = fmap fromLanguage language
          , claimResponseText = text
--          , claimResponseContained = contained
          , claimResponseExtension = extension
          , claimResponseModifierExtension = modifierExtension
          , claimResponseIdentifier = identifier
          , claimResponseStatus =      fromClaimResponseStatus status
          , claimResponseType = ty
          , claimResponseSubType = subType
          , claimResponseUse =      fromClaimResponseUse use
          , claimResponsePatient = patient
          , claimResponseCreated =      fromDateTime created
          , claimResponseInsurer = insurer
          , claimResponseRequestor = requestor
          , claimResponseRequest = request
          , claimResponseOutcome =      fromClaimResponseOutcome outcome
          , claimResponseDisposition = fmap fromString disposition
          , claimResponsePreAuthRef = fmap fromString preAuthRef
          , claimResponsePreAuthPeriod = preAuthPeriod
          , claimResponsePayeeType = payeeType
          , claimResponseItem = item
          , claimResponseAddItem = addItem
          , claimResponseAdjudication = adjudication
          , claimResponseTotal = total
          , claimResponsePayment = payment
          , claimResponseFundsReserve = fundsReserve
          , claimResponseFormCode = formCode
          , claimResponseForm = form
          , claimResponseProcessNote = processNote
          , claimResponseCommunicationRequest = communicationRequest
          , claimResponseInsurance = insurance
          , claimResponseError = error
          }



data ClaimResponseSubDetail1 = ClaimResponseSubDetail1 {
    claimResponseSubDetail1AttrId :: Maybe Text
  , claimResponseSubDetail1Extension :: [Extension]
  , claimResponseSubDetail1ModifierExtension :: [Extension]
  , claimResponseSubDetail1ProductOrService :: CodeableConcept
  , claimResponseSubDetail1Modifier :: [CodeableConcept]
  , claimResponseSubDetail1Quantity :: Maybe Quantity
  , claimResponseSubDetail1UnitPrice :: Maybe Money
  , claimResponseSubDetail1Factor :: Maybe Decimal
  , claimResponseSubDetail1Net :: Maybe Money
  , claimResponseSubDetail1NoteNumber :: [PositiveInt]
  , claimResponseSubDetail1Adjudication :: [ClaimResponseAdjudication]
  }
--

instance ToJSON ClaimResponseSubDetail1 where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimResponseSubDetail1AttrId p)
    ,  "extension" .= toJSON (claimResponseSubDetail1Extension p)
    ,  "modifierExtension" .= toJSON (claimResponseSubDetail1ModifierExtension p)
    ,  "productOrService" .= toJSON (claimResponseSubDetail1ProductOrService p)
    ,  "modifier" .= toJSON (claimResponseSubDetail1Modifier p)
    ,  "quantity" .= toJSON (claimResponseSubDetail1Quantity p)
    ,  "unitPrice" .= toJSON (claimResponseSubDetail1UnitPrice p)
    ,  "factor" .= toJSON (claimResponseSubDetail1Factor p)
    ,  "net" .= toJSON (claimResponseSubDetail1Net p)
    ,  "noteNumber" .= toJSON (claimResponseSubDetail1NoteNumber p)
    ,  "adjudication" .= toJSON (claimResponseSubDetail1Adjudication p)
    ]
instance FromJSON ClaimResponseSubDetail1 where
  parseJSON = withObject "ClaimResponseSubDetail1" $ \o -> do
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
        return ClaimResponseSubDetail1{
            claimResponseSubDetail1AttrId = id
          , claimResponseSubDetail1Extension = extension
          , claimResponseSubDetail1ModifierExtension = modifierExtension
          , claimResponseSubDetail1ProductOrService = productOrService
          , claimResponseSubDetail1Modifier = modifier
          , claimResponseSubDetail1Quantity = quantity
          , claimResponseSubDetail1UnitPrice = unitPrice
          , claimResponseSubDetail1Factor = factor
          , claimResponseSubDetail1Net = net
          , claimResponseSubDetail1NoteNumber = noteNumber
          , claimResponseSubDetail1Adjudication = adjudication
          }
instance Xmlbf.ToXml ClaimResponseSubDetail1 where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimResponseSubDetail1AttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimResponseSubDetail1Extension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimResponseSubDetail1ModifierExtension p))
             , Prop     "productOrService" (HM.empty, Xmlbf.toXml (claimResponseSubDetail1ProductOrService p))
             , PropList "modifier" (fmap Xmlbf.toXml (claimResponseSubDetail1Modifier p))
             , OptProp  "quantity" (fmap Xmlbf.toXml (claimResponseSubDetail1Quantity p))
             , OptProp  "unitPrice" (fmap Xmlbf.toXml (claimResponseSubDetail1UnitPrice p))
             , OptVal   "factor" (fmap toDecimal (claimResponseSubDetail1Factor p))
             , OptProp  "net" (fmap Xmlbf.toXml (claimResponseSubDetail1Net p))
             , ValList  "noteNumber" (fmap toPositiveInt (claimResponseSubDetail1NoteNumber p))
             , PropList "adjudication" (fmap Xmlbf.toXml (claimResponseSubDetail1Adjudication p))
             ]
instance Xmlbf.FromXml ClaimResponseSubDetail1 where
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
    return ClaimResponseSubDetail1 {
            claimResponseSubDetail1AttrId = id
          , claimResponseSubDetail1Extension = extension
          , claimResponseSubDetail1ModifierExtension = modifierExtension
          , claimResponseSubDetail1ProductOrService = productOrService
          , claimResponseSubDetail1Modifier = modifier
          , claimResponseSubDetail1Quantity = quantity
          , claimResponseSubDetail1UnitPrice = unitPrice
          , claimResponseSubDetail1Factor = fmap fromDecimal factor
          , claimResponseSubDetail1Net = net
          , claimResponseSubDetail1NoteNumber = fmap fromPositiveInt noteNumber
          , claimResponseSubDetail1Adjudication = adjudication
          }



data ClaimAccidentLocation
    = ClaimAccidentLocationAddress Address
    | ClaimAccidentLocationReference Reference
    deriving (Eq, Show)

data ClaimAccident = ClaimAccident {
    claimAccidentAttrId :: Maybe Text
  , claimAccidentExtension :: [Extension]
  , claimAccidentModifierExtension :: [Extension]
  , claimAccidentDate :: Date
  , claimAccidentType :: Maybe CodeableConcept
  , claimAccidentLocation :: Maybe ClaimAccidentLocation
  }
--

instance ToJSON ClaimAccident where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimAccidentAttrId p)
    ,  "extension" .= toJSON (claimAccidentExtension p)
    ,  "modifierExtension" .= toJSON (claimAccidentModifierExtension p)
    ,  "date" .= toJSON (claimAccidentDate p)
    ,  "type" .= toJSON (claimAccidentType p)
    , toLocationJSON (claimAccidentLocation p)
    ]
    where 
      toLocationJSON (     Nothing   ) = ("location", Null)
      toLocationJSON (Just (ClaimAccidentLocationAddress c)) = ("location", toJSON c)
      toLocationJSON (Just (ClaimAccidentLocationReference c)) = ("location", toJSON c)
instance FromJSON ClaimAccident where
  parseJSON = withObject "ClaimAccident" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        date <- o .:  "date"
        ty <- o .:? "type"
        location <- parseLocation o
        return ClaimAccident{
            claimAccidentAttrId = id
          , claimAccidentExtension = extension
          , claimAccidentModifierExtension = modifierExtension
          , claimAccidentDate = date
          , claimAccidentType = ty
          , claimAccidentLocation = location
          }
    where 
      parseLocation o = parseLocationAddress o <|> parseLocationReference o
      parseLocationAddress o = do
                has <- o .: "locationAddress"
                return $ Just (ClaimAccidentLocationAddress has)
      parseLocationReference o = do
                has <- o .: "locationReference"
                return $ Just (ClaimAccidentLocationReference has)
instance Xmlbf.ToXml ClaimAccident where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimAccidentAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimAccidentExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimAccidentModifierExtension p))
             , Val      "date" (     toDate (claimAccidentDate p))
             , OptProp  "type" (fmap Xmlbf.toXml (claimAccidentType p))
             , toLocationXml (claimAccidentLocation p)
             ]
       where 
          toLocationXml ( Nothing   ) = (OptVal "location" Nothing)
          toLocationXml (Just (ClaimAccidentLocationAddress p)) = Prop  "locationAddress" (HM.empty, Xmlbf.toXml p)
          toLocationXml (Just (ClaimAccidentLocationReference p)) = Prop  "locationReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml ClaimAccident where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    date <-            Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    ty <- optional $ Xmlbf.pElement "type" Xmlbf.fromXml
    location <- fromLocationXml
    return ClaimAccident {
            claimAccidentAttrId = id
          , claimAccidentExtension = extension
          , claimAccidentModifierExtension = modifierExtension
          , claimAccidentDate =      fromDate date
          , claimAccidentType = ty
          , claimAccidentLocation = location
          }

    where 
      fromLocationXml = parseLocationAddress <|> parseLocationReference <|> pure Nothing
      parseLocationAddress = do
                has <- Xmlbf.pElement "locationAddress" Xmlbf.fromXml
                return $ Just (ClaimAccidentLocationAddress (                      has))
      parseLocationReference = do
                has <- Xmlbf.pElement "locationReference" Xmlbf.fromXml
                return $ Just (ClaimAccidentLocationReference (                      has))


data ClaimResponseDetail = ClaimResponseDetail {
    claimResponseDetailAttrId :: Maybe Text
  , claimResponseDetailExtension :: [Extension]
  , claimResponseDetailModifierExtension :: [Extension]
  , claimResponseDetailDetailSequence :: PositiveInt
  , claimResponseDetailNoteNumber :: [PositiveInt]
  , claimResponseDetailAdjudication :: [ClaimResponseAdjudication]
  , claimResponseDetailSubDetail :: [ClaimResponseSubDetail]
  }
--

instance ToJSON ClaimResponseDetail where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimResponseDetailAttrId p)
    ,  "extension" .= toJSON (claimResponseDetailExtension p)
    ,  "modifierExtension" .= toJSON (claimResponseDetailModifierExtension p)
    ,  "detailSequence" .= toJSON (claimResponseDetailDetailSequence p)
    ,  "noteNumber" .= toJSON (claimResponseDetailNoteNumber p)
    ,  "adjudication" .= toJSON (claimResponseDetailAdjudication p)
    ,  "subDetail" .= toJSON (claimResponseDetailSubDetail p)
    ]
instance FromJSON ClaimResponseDetail where
  parseJSON = withObject "ClaimResponseDetail" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        detailSequence <- o .:  "detailSequence"
        noteNumber <- o .:? "noteNumber" .!= []
        adjudication <- o .:? "adjudication" .!= []
        subDetail <- o .:? "subDetail" .!= []
        return ClaimResponseDetail{
            claimResponseDetailAttrId = id
          , claimResponseDetailExtension = extension
          , claimResponseDetailModifierExtension = modifierExtension
          , claimResponseDetailDetailSequence = detailSequence
          , claimResponseDetailNoteNumber = noteNumber
          , claimResponseDetailAdjudication = adjudication
          , claimResponseDetailSubDetail = subDetail
          }
instance Xmlbf.ToXml ClaimResponseDetail where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimResponseDetailAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimResponseDetailExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimResponseDetailModifierExtension p))
             , Val      "detailSequence" (     toPositiveInt (claimResponseDetailDetailSequence p))
             , ValList  "noteNumber" (fmap toPositiveInt (claimResponseDetailNoteNumber p))
             , PropList "adjudication" (fmap Xmlbf.toXml (claimResponseDetailAdjudication p))
             , PropList "subDetail" (fmap Xmlbf.toXml (claimResponseDetailSubDetail p))
             ]
instance Xmlbf.FromXml ClaimResponseDetail where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    detailSequence <-            Xmlbf.pElement "detailSequence" (Xmlbf.pAttr "value")
    noteNumber <- many     $ Xmlbf.pElement "noteNumber" (Xmlbf.pAttr "value")
    adjudication <- many     $ Xmlbf.pElement "adjudication" Xmlbf.fromXml
    subDetail <- many     $ Xmlbf.pElement "subDetail" Xmlbf.fromXml
    return ClaimResponseDetail {
            claimResponseDetailAttrId = id
          , claimResponseDetailExtension = extension
          , claimResponseDetailModifierExtension = modifierExtension
          , claimResponseDetailDetailSequence =      fromPositiveInt detailSequence
          , claimResponseDetailNoteNumber = fmap fromPositiveInt noteNumber
          , claimResponseDetailAdjudication = adjudication
          , claimResponseDetailSubDetail = subDetail
          }



data ClaimDetail = ClaimDetail {
    claimDetailAttrId :: Maybe Text
  , claimDetailExtension :: [Extension]
  , claimDetailModifierExtension :: [Extension]
  , claimDetailSequence :: PositiveInt
  , claimDetailRevenue :: Maybe CodeableConcept
  , claimDetailCategory :: Maybe CodeableConcept
  , claimDetailProductOrService :: CodeableConcept
  , claimDetailModifier :: [CodeableConcept]
  , claimDetailProgramCode :: [CodeableConcept]
  , claimDetailQuantity :: Maybe Quantity
  , claimDetailUnitPrice :: Maybe Money
  , claimDetailFactor :: Maybe Decimal
  , claimDetailNet :: Maybe Money
  , claimDetailUdi :: [Reference]
  , claimDetailSubDetail :: [ClaimSubDetail]
  }
--

instance ToJSON ClaimDetail where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimDetailAttrId p)
    ,  "extension" .= toJSON (claimDetailExtension p)
    ,  "modifierExtension" .= toJSON (claimDetailModifierExtension p)
    ,  "sequence" .= toJSON (claimDetailSequence p)
    ,  "revenue" .= toJSON (claimDetailRevenue p)
    ,  "category" .= toJSON (claimDetailCategory p)
    ,  "productOrService" .= toJSON (claimDetailProductOrService p)
    ,  "modifier" .= toJSON (claimDetailModifier p)
    ,  "programCode" .= toJSON (claimDetailProgramCode p)
    ,  "quantity" .= toJSON (claimDetailQuantity p)
    ,  "unitPrice" .= toJSON (claimDetailUnitPrice p)
    ,  "factor" .= toJSON (claimDetailFactor p)
    ,  "net" .= toJSON (claimDetailNet p)
    ,  "udi" .= toJSON (claimDetailUdi p)
    ,  "subDetail" .= toJSON (claimDetailSubDetail p)
    ]
instance FromJSON ClaimDetail where
  parseJSON = withObject "ClaimDetail" $ \o -> do
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
        subDetail <- o .:? "subDetail" .!= []
        return ClaimDetail{
            claimDetailAttrId = id
          , claimDetailExtension = extension
          , claimDetailModifierExtension = modifierExtension
          , claimDetailSequence = sequence
          , claimDetailRevenue = revenue
          , claimDetailCategory = category
          , claimDetailProductOrService = productOrService
          , claimDetailModifier = modifier
          , claimDetailProgramCode = programCode
          , claimDetailQuantity = quantity
          , claimDetailUnitPrice = unitPrice
          , claimDetailFactor = factor
          , claimDetailNet = net
          , claimDetailUdi = udi
          , claimDetailSubDetail = subDetail
          }
instance Xmlbf.ToXml ClaimDetail where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimDetailAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimDetailExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimDetailModifierExtension p))
             , Val      "sequence" (     toPositiveInt (claimDetailSequence p))
             , OptProp  "revenue" (fmap Xmlbf.toXml (claimDetailRevenue p))
             , OptProp  "category" (fmap Xmlbf.toXml (claimDetailCategory p))
             , Prop     "productOrService" (HM.empty, Xmlbf.toXml (claimDetailProductOrService p))
             , PropList "modifier" (fmap Xmlbf.toXml (claimDetailModifier p))
             , PropList "programCode" (fmap Xmlbf.toXml (claimDetailProgramCode p))
             , OptProp  "quantity" (fmap Xmlbf.toXml (claimDetailQuantity p))
             , OptProp  "unitPrice" (fmap Xmlbf.toXml (claimDetailUnitPrice p))
             , OptVal   "factor" (fmap toDecimal (claimDetailFactor p))
             , OptProp  "net" (fmap Xmlbf.toXml (claimDetailNet p))
             , PropList "udi" (fmap Xmlbf.toXml (claimDetailUdi p))
             , PropList "subDetail" (fmap Xmlbf.toXml (claimDetailSubDetail p))
             ]
instance Xmlbf.FromXml ClaimDetail where
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
    subDetail <- many     $ Xmlbf.pElement "subDetail" Xmlbf.fromXml
    return ClaimDetail {
            claimDetailAttrId = id
          , claimDetailExtension = extension
          , claimDetailModifierExtension = modifierExtension
          , claimDetailSequence =      fromPositiveInt sequence
          , claimDetailRevenue = revenue
          , claimDetailCategory = category
          , claimDetailProductOrService = productOrService
          , claimDetailModifier = modifier
          , claimDetailProgramCode = programCode
          , claimDetailQuantity = quantity
          , claimDetailUnitPrice = unitPrice
          , claimDetailFactor = fmap fromDecimal factor
          , claimDetailNet = net
          , claimDetailUdi = udi
          , claimDetailSubDetail = subDetail
          }



data ClaimDiagnosisDiagnosis
    = ClaimDiagnosisDiagnosisCodeableConcept CodeableConcept
    | ClaimDiagnosisDiagnosisReference Reference
    deriving (Eq, Show)

data ClaimDiagnosis = ClaimDiagnosis {
    claimDiagnosisAttrId :: Maybe Text
  , claimDiagnosisExtension :: [Extension]
  , claimDiagnosisModifierExtension :: [Extension]
  , claimDiagnosisSequence :: PositiveInt
  , claimDiagnosisDiagnosis :: ClaimDiagnosisDiagnosis
  , claimDiagnosisType :: [CodeableConcept]
  , claimDiagnosisOnAdmission :: Maybe CodeableConcept
  , claimDiagnosisPackageCode :: Maybe CodeableConcept
  }
--

instance ToJSON ClaimDiagnosis where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (claimDiagnosisAttrId p)
    ,  "extension" .= toJSON (claimDiagnosisExtension p)
    ,  "modifierExtension" .= toJSON (claimDiagnosisModifierExtension p)
    ,  "sequence" .= toJSON (claimDiagnosisSequence p)
    , toDiagnosisJSON (claimDiagnosisDiagnosis p)
    ,  "type" .= toJSON (claimDiagnosisType p)
    ,  "onAdmission" .= toJSON (claimDiagnosisOnAdmission p)
    ,  "packageCode" .= toJSON (claimDiagnosisPackageCode p)
    ]
    where 
      toDiagnosisJSON (     (ClaimDiagnosisDiagnosisCodeableConcept c)) = ("diagnosis", toJSON c)
      toDiagnosisJSON (     (ClaimDiagnosisDiagnosisReference c)) = ("diagnosis", toJSON c)
instance FromJSON ClaimDiagnosis where
  parseJSON = withObject "ClaimDiagnosis" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        sequence <- o .:  "sequence"
        diagnosis <- parseDiagnosis o
        ty <- o .:? "type" .!= []
        onAdmission <- o .:? "onAdmission"
        packageCode <- o .:? "packageCode"
        return ClaimDiagnosis{
            claimDiagnosisAttrId = id
          , claimDiagnosisExtension = extension
          , claimDiagnosisModifierExtension = modifierExtension
          , claimDiagnosisSequence = sequence
          , claimDiagnosisDiagnosis = diagnosis
          , claimDiagnosisType = ty
          , claimDiagnosisOnAdmission = onAdmission
          , claimDiagnosisPackageCode = packageCode
          }
    where 
      parseDiagnosis o = parseDiagnosisCodeableConcept o <|> parseDiagnosisReference o
      parseDiagnosisCodeableConcept o = do
                has <- o .: "diagnosisCodeableConcept"
                return $ ClaimDiagnosisDiagnosisCodeableConcept has
      parseDiagnosisReference o = do
                has <- o .: "diagnosisReference"
                return $ ClaimDiagnosisDiagnosisReference has
instance Xmlbf.ToXml ClaimDiagnosis where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (claimDiagnosisAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (claimDiagnosisExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (claimDiagnosisModifierExtension p))
             , Val      "sequence" (     toPositiveInt (claimDiagnosisSequence p))
             , toDiagnosisXml (claimDiagnosisDiagnosis p)
             , PropList "type" (fmap Xmlbf.toXml (claimDiagnosisType p))
             , OptProp  "onAdmission" (fmap Xmlbf.toXml (claimDiagnosisOnAdmission p))
             , OptProp  "packageCode" (fmap Xmlbf.toXml (claimDiagnosisPackageCode p))
             ]
       where 
          toDiagnosisXml (     (ClaimDiagnosisDiagnosisCodeableConcept p)) = Prop     "diagnosisCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toDiagnosisXml (     (ClaimDiagnosisDiagnosisReference p)) = Prop     "diagnosisReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml ClaimDiagnosis where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    sequence <-            Xmlbf.pElement "sequence" (Xmlbf.pAttr "value")
    diagnosis <- fromDiagnosisXml
    ty <- many     $ Xmlbf.pElement "type" Xmlbf.fromXml
    onAdmission <- optional $ Xmlbf.pElement "onAdmission" Xmlbf.fromXml
    packageCode <- optional $ Xmlbf.pElement "packageCode" Xmlbf.fromXml
    return ClaimDiagnosis {
            claimDiagnosisAttrId = id
          , claimDiagnosisExtension = extension
          , claimDiagnosisModifierExtension = modifierExtension
          , claimDiagnosisSequence =      fromPositiveInt sequence
          , claimDiagnosisDiagnosis = diagnosis
          , claimDiagnosisType = ty
          , claimDiagnosisOnAdmission = onAdmission
          , claimDiagnosisPackageCode = packageCode
          }

    where 
      fromDiagnosisXml = parseDiagnosisCodeableConcept <|> parseDiagnosisReference
      parseDiagnosisCodeableConcept = do
                has <- Xmlbf.pElement "diagnosisCodeableConcept" Xmlbf.fromXml
                return $ ClaimDiagnosisDiagnosisCodeableConcept (                      has)
      parseDiagnosisReference = do
                has <- Xmlbf.pElement "diagnosisReference" Xmlbf.fromXml
                return $ ClaimDiagnosisDiagnosisReference (                      has)



