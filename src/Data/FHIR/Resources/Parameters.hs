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
-- FHIR 4.0.0 Parameters
--

module Data.FHIR.Resources.Parameters where

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

data Parameters = Parameters {
    parametersId :: Maybe Id
  , parametersMeta :: Maybe Meta
  , parametersImplicitRules :: Maybe Uri
  , parametersLanguage :: Maybe Language
  , parametersParameter :: [ParametersParameter]
  }
--

instance ToJSON Parameters where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Parameters")
    ,  "id" .= toJSON (parametersId p)
    ,  "meta" .= toJSON (parametersMeta p)
    ,  "implicitRules" .= toJSON (parametersImplicitRules p)
    ,  "language" .= toJSON (parametersLanguage p)
    ,  "parameter" .= toJSON (parametersParameter p)
    ]
instance FromJSON Parameters where
  parseJSON = withObject "Parameters" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Parameters" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        parameter <- o .:? "parameter" .!= []
        return Parameters{
            parametersId = id
          , parametersMeta = meta
          , parametersImplicitRules = implicitRules
          , parametersLanguage = language
          , parametersParameter = parameter
          }
      _ -> fail "not a Parameters"
instance Xmlbf.ToXml Parameters where
  toXml p = Xmlbf.element "Parameters" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (parametersId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (parametersMeta p))
             , OptVal   "implicitRules" (fmap toUri (parametersImplicitRules p))
             , OptVal   "language" (fmap toLanguage (parametersLanguage p))
             , PropList "parameter" (fmap Xmlbf.toXml (parametersParameter p))
             ]
instance Xmlbf.FromXml Parameters where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    parameter <- many     $ Xmlbf.pElement "parameter" Xmlbf.fromXml
    return Parameters {
            parametersId = fmap fromId id
          , parametersMeta = meta
          , parametersImplicitRules = fmap fromUri implicitRules
          , parametersLanguage = fmap fromLanguage language
          , parametersParameter = parameter
          }



data ParametersParameterValue
    = ParametersParameterValueBase64Binary Base64Binary
    | ParametersParameterValueBoolean Boolean
    | ParametersParameterValueCanonical Canonical
    | ParametersParameterValueCode Code
    | ParametersParameterValueDate Date
    | ParametersParameterValueDateTime DateTime
    | ParametersParameterValueDecimal Decimal
    | ParametersParameterValueId Id
    | ParametersParameterValueInstant Instant
    | ParametersParameterValueInteger Integer
    | ParametersParameterValueMarkdown Markdown
    | ParametersParameterValueOid Oid
    | ParametersParameterValuePositiveInt PositiveInt
    | ParametersParameterValueString Text
    | ParametersParameterValueTime Time
    | ParametersParameterValueUnsignedInt UnsignedInt
    | ParametersParameterValueUri Uri
    | ParametersParameterValueUrl Url
    | ParametersParameterValueUuid Uuid
    | ParametersParameterValueAddress Address
    | ParametersParameterValueAge Age
    | ParametersParameterValueAnnotation Annotation
    | ParametersParameterValueAttachment Attachment
    | ParametersParameterValueCodeableConcept CodeableConcept
    | ParametersParameterValueCoding Coding
    | ParametersParameterValueContactPoint ContactPoint
    | ParametersParameterValueCount Count
    | ParametersParameterValueDistance Distance
    | ParametersParameterValueDuration Duration
    | ParametersParameterValueHumanName HumanName
    | ParametersParameterValueIdentifier Identifier
    | ParametersParameterValueMoney Money
    | ParametersParameterValuePeriod Period
    | ParametersParameterValueQuantity Quantity
    | ParametersParameterValueRange Range
    | ParametersParameterValueRatio Ratio
    | ParametersParameterValueReference Reference
    | ParametersParameterValueSampledData SampledData
    | ParametersParameterValueSignature Signature
    | ParametersParameterValueTiming Timing
    | ParametersParameterValueContactDetail ContactDetail
    | ParametersParameterValueContributor Contributor
    | ParametersParameterValueDataRequirement DataRequirement
    | ParametersParameterValueExpression Expression
    | ParametersParameterValueParameterDefinition ParameterDefinition
    | ParametersParameterValueRelatedArtifact RelatedArtifact
    | ParametersParameterValueTriggerDefinition TriggerDefinition
    | ParametersParameterValueUsageContext UsageContext
    | ParametersParameterValueDosage Dosage
    deriving (Eq, Show)

data ParametersParameter = ParametersParameter {
    parametersParameterAttrId :: Maybe Text
  , parametersParameterExtension :: [Extension]
  , parametersParameterModifierExtension :: [Extension]
  , parametersParameterName :: Text
  , parametersParameterValueBase64Binary :: Maybe Base64Binary
  , parametersParameterValueBoolean :: Maybe Boolean
  , parametersParameterValueCanonical :: Maybe Canonical
  , parametersParameterValueCode :: Maybe Code
  , parametersParameterValueDate :: Maybe Date
  , parametersParameterValueDateTime :: Maybe DateTime
  , parametersParameterValueDecimal :: Maybe Decimal
  , parametersParameterValueId :: Maybe Id
  , parametersParameterValueInstant :: Maybe Instant
  , parametersParameterValueInteger :: Maybe Integer
  , parametersParameterValueMarkdown :: Maybe Markdown
  , parametersParameterValueOid :: Maybe Oid
  , parametersParameterValuePositiveInt :: Maybe PositiveInt
  , parametersParameterValueString :: Maybe Text
  , parametersParameterValueTime :: Maybe Time
  , parametersParameterValueUnsignedInt :: Maybe UnsignedInt
  , parametersParameterValueUri :: Maybe Uri
  , parametersParameterValueUrl :: Maybe Url
  , parametersParameterValueUuid :: Maybe Uuid
  , parametersParameterValueAddress :: Maybe Address
  , parametersParameterValueAge :: Maybe Age
  , parametersParameterValueAnnotation :: Maybe Annotation
  , parametersParameterValueAttachment :: Maybe Attachment
  , parametersParameterValueCodeableConcept :: Maybe CodeableConcept
  , parametersParameterValueCoding :: Maybe Coding
  , parametersParameterValueContactPoint :: Maybe ContactPoint
  , parametersParameterValueCount :: Maybe Count
  , parametersParameterValueDistance :: Maybe Distance
  , parametersParameterValueDuration :: Maybe Duration
  , parametersParameterValueHumanName :: Maybe HumanName
  , parametersParameterValueIdentifier :: Maybe Identifier
  , parametersParameterValueMoney :: Maybe Money
  , parametersParameterValuePeriod :: Maybe Period
  , parametersParameterValueQuantity :: Maybe Quantity
  , parametersParameterValueRange :: Maybe Range
  , parametersParameterValueRatio :: Maybe Ratio
  , parametersParameterValueReference :: Maybe Reference
  , parametersParameterValueSampledData :: Maybe SampledData
  , parametersParameterValueSignature :: Maybe Signature
  , parametersParameterValueTiming :: Maybe Timing
  , parametersParameterValueContactDetail :: Maybe ContactDetail
  , parametersParameterValueContributor :: Maybe Contributor
  , parametersParameterValueDataRequirement :: Maybe DataRequirement
  , parametersParameterValueExpression :: Maybe Expression
  , parametersParameterValueParameterDefinition :: Maybe ParameterDefinition
  , parametersParameterValueRelatedArtifact :: Maybe RelatedArtifact
  , parametersParameterValueTriggerDefinition :: Maybe TriggerDefinition
  , parametersParameterValueUsageContext :: Maybe UsageContext
  , parametersParameterValueDosage :: Maybe Dosage
  , parametersParameterResource :: Maybe ResourceContainer
  , parametersParameterPart :: [ParametersParameter]
  }
--

instance ToJSON ParametersParameter where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (parametersParameterAttrId p)
    ,  "extension" .= toJSON (parametersParameterExtension p)
    ,  "modifierExtension" .= toJSON (parametersParameterModifierExtension p)
    ,  "name" .= toJSON (parametersParameterName p)
    ,  "valueBase64Binary" .= toJSON (parametersParameterValueBase64Binary p)
    ,  "valueBoolean" .= toJSON (parametersParameterValueBoolean p)
    ,  "valueCanonical" .= toJSON (parametersParameterValueCanonical p)
    ,  "valueCode" .= toJSON (parametersParameterValueCode p)
    ,  "valueDate" .= toJSON (parametersParameterValueDate p)
    ,  "valueDateTime" .= toJSON (parametersParameterValueDateTime p)
    ,  "valueDecimal" .= toJSON (parametersParameterValueDecimal p)
    ,  "valueId" .= toJSON (parametersParameterValueId p)
    ,  "valueInstant" .= toJSON (parametersParameterValueInstant p)
    ,  "valueInteger" .= toJSON (parametersParameterValueInteger p)
    ,  "valueMarkdown" .= toJSON (parametersParameterValueMarkdown p)
    ,  "valueOid" .= toJSON (parametersParameterValueOid p)
    ,  "valuePositiveInt" .= toJSON (parametersParameterValuePositiveInt p)
    ,  "valueString" .= toJSON (parametersParameterValueString p)
    ,  "valueTime" .= toJSON (parametersParameterValueTime p)
    ,  "valueUnsignedInt" .= toJSON (parametersParameterValueUnsignedInt p)
    ,  "valueUri" .= toJSON (parametersParameterValueUri p)
    ,  "valueUrl" .= toJSON (parametersParameterValueUrl p)
    ,  "valueUuid" .= toJSON (parametersParameterValueUuid p)
    ,  "valueAddress" .= toJSON (parametersParameterValueAddress p)
    ,  "valueAge" .= toJSON (parametersParameterValueAge p)
    ,  "valueAnnotation" .= toJSON (parametersParameterValueAnnotation p)
    ,  "valueAttachment" .= toJSON (parametersParameterValueAttachment p)
    ,  "valueCodeableConcept" .= toJSON (parametersParameterValueCodeableConcept p)
    ,  "valueCoding" .= toJSON (parametersParameterValueCoding p)
    ,  "valueContactPoint" .= toJSON (parametersParameterValueContactPoint p)
    ,  "valueCount" .= toJSON (parametersParameterValueCount p)
    ,  "valueDistance" .= toJSON (parametersParameterValueDistance p)
    ,  "valueDuration" .= toJSON (parametersParameterValueDuration p)
    ,  "valueHumanName" .= toJSON (parametersParameterValueHumanName p)
    ,  "valueIdentifier" .= toJSON (parametersParameterValueIdentifier p)
    ,  "valueMoney" .= toJSON (parametersParameterValueMoney p)
    ,  "valuePeriod" .= toJSON (parametersParameterValuePeriod p)
    ,  "valueQuantity" .= toJSON (parametersParameterValueQuantity p)
    ,  "valueRange" .= toJSON (parametersParameterValueRange p)
    ,  "valueRatio" .= toJSON (parametersParameterValueRatio p)
    ,  "valueReference" .= toJSON (parametersParameterValueReference p)
    ,  "valueSampledData" .= toJSON (parametersParameterValueSampledData p)
    ,  "valueSignature" .= toJSON (parametersParameterValueSignature p)
    ,  "valueTiming" .= toJSON (parametersParameterValueTiming p)
    ,  "valueContactDetail" .= toJSON (parametersParameterValueContactDetail p)
    ,  "valueContributor" .= toJSON (parametersParameterValueContributor p)
    ,  "valueDataRequirement" .= toJSON (parametersParameterValueDataRequirement p)
    ,  "valueExpression" .= toJSON (parametersParameterValueExpression p)
    ,  "valueParameterDefinition" .= toJSON (parametersParameterValueParameterDefinition p)
    ,  "valueRelatedArtifact" .= toJSON (parametersParameterValueRelatedArtifact p)
    ,  "valueTriggerDefinition" .= toJSON (parametersParameterValueTriggerDefinition p)
    ,  "valueUsageContext" .= toJSON (parametersParameterValueUsageContext p)
    ,  "valueDosage" .= toJSON (parametersParameterValueDosage p)
    ,  "resource" .= toJSON (parametersParameterResource p)
    ,  "part" .= toJSON (parametersParameterPart p)
    ]
instance FromJSON ParametersParameter where
  parseJSON = withObject "ParametersParameter" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        name <- o .:  "name"
        valueBase64Binary <- o .:? "valueBase64Binary"
        valueBoolean <- o .:? "valueBoolean"
        valueCanonical <- o .:? "valueCanonical"
        valueCode <- o .:? "valueCode"
        valueDate <- o .:? "valueDate"
        valueDateTime <- o .:? "valueDateTime"
        valueDecimal <- o .:? "valueDecimal"
        valueId <- o .:? "valueId"
        valueInstant <- o .:? "valueInstant"
        valueInteger <- o .:? "valueInteger"
        valueMarkdown <- o .:? "valueMarkdown"
        valueOid <- o .:? "valueOid"
        valuePositiveInt <- o .:? "valuePositiveInt"
        valueString <- o .:? "valueString"
        valueTime <- o .:? "valueTime"
        valueUnsignedInt <- o .:? "valueUnsignedInt"
        valueUri <- o .:? "valueUri"
        valueUrl <- o .:? "valueUrl"
        valueUuid <- o .:? "valueUuid"
        valueAddress <- o .:? "valueAddress"
        valueAge <- o .:? "valueAge"
        valueAnnotation <- o .:? "valueAnnotation"
        valueAttachment <- o .:? "valueAttachment"
        valueCodeableConcept <- o .:? "valueCodeableConcept"
        valueCoding <- o .:? "valueCoding"
        valueContactPoint <- o .:? "valueContactPoint"
        valueCount <- o .:? "valueCount"
        valueDistance <- o .:? "valueDistance"
        valueDuration <- o .:? "valueDuration"
        valueHumanName <- o .:? "valueHumanName"
        valueIdentifier <- o .:? "valueIdentifier"
        valueMoney <- o .:? "valueMoney"
        valuePeriod <- o .:? "valuePeriod"
        valueQuantity <- o .:? "valueQuantity"
        valueRange <- o .:? "valueRange"
        valueRatio <- o .:? "valueRatio"
        valueReference <- o .:? "valueReference"
        valueSampledData <- o .:? "valueSampledData"
        valueSignature <- o .:? "valueSignature"
        valueTiming <- o .:? "valueTiming"
        valueContactDetail <- o .:? "valueContactDetail"
        valueContributor <- o .:? "valueContributor"
        valueDataRequirement <- o .:? "valueDataRequirement"
        valueExpression <- o .:? "valueExpression"
        valueParameterDefinition <- o .:? "valueParameterDefinition"
        valueRelatedArtifact <- o .:? "valueRelatedArtifact"
        valueTriggerDefinition <- o .:? "valueTriggerDefinition"
        valueUsageContext <- o .:? "valueUsageContext"
        valueDosage <- o .:? "valueDosage"
        resource <- o .:? "resource"
        part <- o .:? "part" .!= []
        return ParametersParameter{
            parametersParameterAttrId = id
          , parametersParameterExtension = extension
          , parametersParameterModifierExtension = modifierExtension
          , parametersParameterName = name
          , parametersParameterValueBase64Binary = valueBase64Binary
          , parametersParameterValueBoolean = valueBoolean
          , parametersParameterValueCanonical = valueCanonical
          , parametersParameterValueCode = valueCode
          , parametersParameterValueDate = valueDate
          , parametersParameterValueDateTime = valueDateTime
          , parametersParameterValueDecimal = valueDecimal
          , parametersParameterValueId = valueId
          , parametersParameterValueInstant = valueInstant
          , parametersParameterValueInteger = valueInteger
          , parametersParameterValueMarkdown = valueMarkdown
          , parametersParameterValueOid = valueOid
          , parametersParameterValuePositiveInt = valuePositiveInt
          , parametersParameterValueString = valueString
          , parametersParameterValueTime = valueTime
          , parametersParameterValueUnsignedInt = valueUnsignedInt
          , parametersParameterValueUri = valueUri
          , parametersParameterValueUrl = valueUrl
          , parametersParameterValueUuid = valueUuid
          , parametersParameterValueAddress = valueAddress
          , parametersParameterValueAge = valueAge
          , parametersParameterValueAnnotation = valueAnnotation
          , parametersParameterValueAttachment = valueAttachment
          , parametersParameterValueCodeableConcept = valueCodeableConcept
          , parametersParameterValueCoding = valueCoding
          , parametersParameterValueContactPoint = valueContactPoint
          , parametersParameterValueCount = valueCount
          , parametersParameterValueDistance = valueDistance
          , parametersParameterValueDuration = valueDuration
          , parametersParameterValueHumanName = valueHumanName
          , parametersParameterValueIdentifier = valueIdentifier
          , parametersParameterValueMoney = valueMoney
          , parametersParameterValuePeriod = valuePeriod
          , parametersParameterValueQuantity = valueQuantity
          , parametersParameterValueRange = valueRange
          , parametersParameterValueRatio = valueRatio
          , parametersParameterValueReference = valueReference
          , parametersParameterValueSampledData = valueSampledData
          , parametersParameterValueSignature = valueSignature
          , parametersParameterValueTiming = valueTiming
          , parametersParameterValueContactDetail = valueContactDetail
          , parametersParameterValueContributor = valueContributor
          , parametersParameterValueDataRequirement = valueDataRequirement
          , parametersParameterValueExpression = valueExpression
          , parametersParameterValueParameterDefinition = valueParameterDefinition
          , parametersParameterValueRelatedArtifact = valueRelatedArtifact
          , parametersParameterValueTriggerDefinition = valueTriggerDefinition
          , parametersParameterValueUsageContext = valueUsageContext
          , parametersParameterValueDosage = valueDosage
          , parametersParameterResource = resource
          , parametersParameterPart = part
          }
instance Xmlbf.ToXml ParametersParameter where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (parametersParameterAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (parametersParameterExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (parametersParameterModifierExtension p))
             , Val      "name" (     toString (parametersParameterName p))
             , OptVal   "valueBase64Binary" (fmap toBase64Binary (parametersParameterValueBase64Binary p))
             , OptVal   "valueBoolean" (fmap toBoolean (parametersParameterValueBoolean p))
             , OptVal   "valueCanonical" (fmap toCanonical (parametersParameterValueCanonical p))
             , OptVal   "valueCode" (fmap toCode (parametersParameterValueCode p))
             , OptVal   "valueDate" (fmap toDate (parametersParameterValueDate p))
             , OptVal   "valueDateTime" (fmap toDateTime (parametersParameterValueDateTime p))
             , OptVal   "valueDecimal" (fmap toDecimal (parametersParameterValueDecimal p))
             , OptVal   "valueId" (fmap toId (parametersParameterValueId p))
             , OptVal   "valueInstant" (fmap toInstant (parametersParameterValueInstant p))
             , OptVal   "valueInteger" (fmap toInteger (parametersParameterValueInteger p))
             , OptVal   "valueMarkdown" (fmap toMarkdown (parametersParameterValueMarkdown p))
             , OptVal   "valueOid" (fmap toOid (parametersParameterValueOid p))
             , OptVal   "valuePositiveInt" (fmap toPositiveInt (parametersParameterValuePositiveInt p))
             , OptVal   "valueString" (fmap toString (parametersParameterValueString p))
             , OptVal   "valueTime" (fmap toTime (parametersParameterValueTime p))
             , OptVal   "valueUnsignedInt" (fmap toUnsignedInt (parametersParameterValueUnsignedInt p))
             , OptVal   "valueUri" (fmap toUri (parametersParameterValueUri p))
             , OptVal   "valueUrl" (fmap toUrl (parametersParameterValueUrl p))
             , OptVal   "valueUuid" (fmap toUuid (parametersParameterValueUuid p))
             , OptProp  "valueAddress" (fmap Xmlbf.toXml (parametersParameterValueAddress p))
             , OptProp  "valueAge" (fmap Xmlbf.toXml (parametersParameterValueAge p))
             , OptProp  "valueAnnotation" (fmap Xmlbf.toXml (parametersParameterValueAnnotation p))
             , OptProp  "valueAttachment" (fmap Xmlbf.toXml (parametersParameterValueAttachment p))
             , OptProp  "valueCodeableConcept" (fmap Xmlbf.toXml (parametersParameterValueCodeableConcept p))
             , OptProp  "valueCoding" (fmap Xmlbf.toXml (parametersParameterValueCoding p))
             , OptProp  "valueContactPoint" (fmap Xmlbf.toXml (parametersParameterValueContactPoint p))
             , OptProp  "valueCount" (fmap Xmlbf.toXml (parametersParameterValueCount p))
             , OptProp  "valueDistance" (fmap Xmlbf.toXml (parametersParameterValueDistance p))
             , OptProp  "valueDuration" (fmap Xmlbf.toXml (parametersParameterValueDuration p))
             , OptProp  "valueHumanName" (fmap Xmlbf.toXml (parametersParameterValueHumanName p))
             , OptProp  "valueIdentifier" (fmap Xmlbf.toXml (parametersParameterValueIdentifier p))
             , OptProp  "valueMoney" (fmap Xmlbf.toXml (parametersParameterValueMoney p))
             , OptProp  "valuePeriod" (fmap Xmlbf.toXml (parametersParameterValuePeriod p))
             , OptProp  "valueQuantity" (fmap Xmlbf.toXml (parametersParameterValueQuantity p))
             , OptProp  "valueRange" (fmap Xmlbf.toXml (parametersParameterValueRange p))
             , OptProp  "valueRatio" (fmap Xmlbf.toXml (parametersParameterValueRatio p))
             , OptProp  "valueReference" (fmap Xmlbf.toXml (parametersParameterValueReference p))
             , OptProp  "valueSampledData" (fmap Xmlbf.toXml (parametersParameterValueSampledData p))
             , OptProp  "valueSignature" (fmap Xmlbf.toXml (parametersParameterValueSignature p))
             , OptProp  "valueTiming" (fmap Xmlbf.toXml (parametersParameterValueTiming p))
             , OptProp  "valueContactDetail" (fmap Xmlbf.toXml (parametersParameterValueContactDetail p))
             , OptProp  "valueContributor" (fmap Xmlbf.toXml (parametersParameterValueContributor p))
             , OptProp  "valueDataRequirement" (fmap Xmlbf.toXml (parametersParameterValueDataRequirement p))
             , OptProp  "valueExpression" (fmap Xmlbf.toXml (parametersParameterValueExpression p))
             , OptProp  "valueParameterDefinition" (fmap Xmlbf.toXml (parametersParameterValueParameterDefinition p))
             , OptProp  "valueRelatedArtifact" (fmap Xmlbf.toXml (parametersParameterValueRelatedArtifact p))
             , OptProp  "valueTriggerDefinition" (fmap Xmlbf.toXml (parametersParameterValueTriggerDefinition p))
             , OptProp  "valueUsageContext" (fmap Xmlbf.toXml (parametersParameterValueUsageContext p))
             , OptProp  "valueDosage" (fmap Xmlbf.toXml (parametersParameterValueDosage p))
             , OptProp  "resource" (fmap Xmlbf.toXml (parametersParameterResource p))
             , PropList "part" (fmap Xmlbf.toXml (parametersParameterPart p))
             ]
instance Xmlbf.FromXml ParametersParameter where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    name <-            Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    valueBase64Binary <- optional $ Xmlbf.pElement "valueBase64Binary" (Xmlbf.pAttr "value")
    valueBoolean <- optional $ Xmlbf.pElement "valueBoolean" (Xmlbf.pAttr "value")
    valueCanonical <- optional $ Xmlbf.pElement "valueCanonical" (Xmlbf.pAttr "value")
    valueCode <- optional $ Xmlbf.pElement "valueCode" (Xmlbf.pAttr "value")
    valueDate <- optional $ Xmlbf.pElement "valueDate" (Xmlbf.pAttr "value")
    valueDateTime <- optional $ Xmlbf.pElement "valueDateTime" (Xmlbf.pAttr "value")
    valueDecimal <- optional $ Xmlbf.pElement "valueDecimal" (Xmlbf.pAttr "value")
    valueId <- optional $ Xmlbf.pElement "valueId" (Xmlbf.pAttr "value")
    valueInstant <- optional $ Xmlbf.pElement "valueInstant" (Xmlbf.pAttr "value")
    valueInteger <- optional $ Xmlbf.pElement "valueInteger" (Xmlbf.pAttr "value")
    valueMarkdown <- optional $ Xmlbf.pElement "valueMarkdown" (Xmlbf.pAttr "value")
    valueOid <- optional $ Xmlbf.pElement "valueOid" (Xmlbf.pAttr "value")
    valuePositiveInt <- optional $ Xmlbf.pElement "valuePositiveInt" (Xmlbf.pAttr "value")
    valueString <- optional $ Xmlbf.pElement "valueString" (Xmlbf.pAttr "value")
    valueTime <- optional $ Xmlbf.pElement "valueTime" (Xmlbf.pAttr "value")
    valueUnsignedInt <- optional $ Xmlbf.pElement "valueUnsignedInt" (Xmlbf.pAttr "value")
    valueUri <- optional $ Xmlbf.pElement "valueUri" (Xmlbf.pAttr "value")
    valueUrl <- optional $ Xmlbf.pElement "valueUrl" (Xmlbf.pAttr "value")
    valueUuid <- optional $ Xmlbf.pElement "valueUuid" (Xmlbf.pAttr "value")
    valueAddress <- optional $ Xmlbf.pElement "valueAddress" Xmlbf.fromXml
    valueAge <- optional $ Xmlbf.pElement "valueAge" Xmlbf.fromXml
    valueAnnotation <- optional $ Xmlbf.pElement "valueAnnotation" Xmlbf.fromXml
    valueAttachment <- optional $ Xmlbf.pElement "valueAttachment" Xmlbf.fromXml
    valueCodeableConcept <- optional $ Xmlbf.pElement "valueCodeableConcept" Xmlbf.fromXml
    valueCoding <- optional $ Xmlbf.pElement "valueCoding" Xmlbf.fromXml
    valueContactPoint <- optional $ Xmlbf.pElement "valueContactPoint" Xmlbf.fromXml
    valueCount <- optional $ Xmlbf.pElement "valueCount" Xmlbf.fromXml
    valueDistance <- optional $ Xmlbf.pElement "valueDistance" Xmlbf.fromXml
    valueDuration <- optional $ Xmlbf.pElement "valueDuration" Xmlbf.fromXml
    valueHumanName <- optional $ Xmlbf.pElement "valueHumanName" Xmlbf.fromXml
    valueIdentifier <- optional $ Xmlbf.pElement "valueIdentifier" Xmlbf.fromXml
    valueMoney <- optional $ Xmlbf.pElement "valueMoney" Xmlbf.fromXml
    valuePeriod <- optional $ Xmlbf.pElement "valuePeriod" Xmlbf.fromXml
    valueQuantity <- optional $ Xmlbf.pElement "valueQuantity" Xmlbf.fromXml
    valueRange <- optional $ Xmlbf.pElement "valueRange" Xmlbf.fromXml
    valueRatio <- optional $ Xmlbf.pElement "valueRatio" Xmlbf.fromXml
    valueReference <- optional $ Xmlbf.pElement "valueReference" Xmlbf.fromXml
    valueSampledData <- optional $ Xmlbf.pElement "valueSampledData" Xmlbf.fromXml
    valueSignature <- optional $ Xmlbf.pElement "valueSignature" Xmlbf.fromXml
    valueTiming <- optional $ Xmlbf.pElement "valueTiming" Xmlbf.fromXml
    valueContactDetail <- optional $ Xmlbf.pElement "valueContactDetail" Xmlbf.fromXml
    valueContributor <- optional $ Xmlbf.pElement "valueContributor" Xmlbf.fromXml
    valueDataRequirement <- optional $ Xmlbf.pElement "valueDataRequirement" Xmlbf.fromXml
    valueExpression <- optional $ Xmlbf.pElement "valueExpression" Xmlbf.fromXml
    valueParameterDefinition <- optional $ Xmlbf.pElement "valueParameterDefinition" Xmlbf.fromXml
    valueRelatedArtifact <- optional $ Xmlbf.pElement "valueRelatedArtifact" Xmlbf.fromXml
    valueTriggerDefinition <- optional $ Xmlbf.pElement "valueTriggerDefinition" Xmlbf.fromXml
    valueUsageContext <- optional $ Xmlbf.pElement "valueUsageContext" Xmlbf.fromXml
    valueDosage <- optional $ Xmlbf.pElement "valueDosage" Xmlbf.fromXml
    resource <- optional $ Xmlbf.pElement "resource" Xmlbf.fromXml
    part <- many     $ Xmlbf.pElement "part" Xmlbf.fromXml
    return ParametersParameter {
            parametersParameterAttrId = id
          , parametersParameterExtension = extension
          , parametersParameterModifierExtension = modifierExtension
          , parametersParameterName =      fromString name
          , parametersParameterValueBase64Binary = fmap fromBase64Binary valueBase64Binary
          , parametersParameterValueBoolean = fmap fromBoolean valueBoolean
          , parametersParameterValueCanonical = fmap fromCanonical valueCanonical
          , parametersParameterValueCode = fmap fromCode valueCode
          , parametersParameterValueDate = fmap fromDate valueDate
          , parametersParameterValueDateTime = fmap fromDateTime valueDateTime
          , parametersParameterValueDecimal = fmap fromDecimal valueDecimal
          , parametersParameterValueId = fmap fromId valueId
          , parametersParameterValueInstant = fmap fromInstant valueInstant
          , parametersParameterValueInteger = fmap fromInteger valueInteger
          , parametersParameterValueMarkdown = fmap fromMarkdown valueMarkdown
          , parametersParameterValueOid = fmap fromOid valueOid
          , parametersParameterValuePositiveInt = fmap fromPositiveInt valuePositiveInt
          , parametersParameterValueString = fmap fromString valueString
          , parametersParameterValueTime = fmap fromTime valueTime
          , parametersParameterValueUnsignedInt = fmap fromUnsignedInt valueUnsignedInt
          , parametersParameterValueUri = fmap fromUri valueUri
          , parametersParameterValueUrl = fmap fromUrl valueUrl
          , parametersParameterValueUuid = fmap fromUuid valueUuid
          , parametersParameterValueAddress = valueAddress
          , parametersParameterValueAge = valueAge
          , parametersParameterValueAnnotation = valueAnnotation
          , parametersParameterValueAttachment = valueAttachment
          , parametersParameterValueCodeableConcept = valueCodeableConcept
          , parametersParameterValueCoding = valueCoding
          , parametersParameterValueContactPoint = valueContactPoint
          , parametersParameterValueCount = valueCount
          , parametersParameterValueDistance = valueDistance
          , parametersParameterValueDuration = valueDuration
          , parametersParameterValueHumanName = valueHumanName
          , parametersParameterValueIdentifier = valueIdentifier
          , parametersParameterValueMoney = valueMoney
          , parametersParameterValuePeriod = valuePeriod
          , parametersParameterValueQuantity = valueQuantity
          , parametersParameterValueRange = valueRange
          , parametersParameterValueRatio = valueRatio
          , parametersParameterValueReference = valueReference
          , parametersParameterValueSampledData = valueSampledData
          , parametersParameterValueSignature = valueSignature
          , parametersParameterValueTiming = valueTiming
          , parametersParameterValueContactDetail = valueContactDetail
          , parametersParameterValueContributor = valueContributor
          , parametersParameterValueDataRequirement = valueDataRequirement
          , parametersParameterValueExpression = valueExpression
          , parametersParameterValueParameterDefinition = valueParameterDefinition
          , parametersParameterValueRelatedArtifact = valueRelatedArtifact
          , parametersParameterValueTriggerDefinition = valueTriggerDefinition
          , parametersParameterValueUsageContext = valueUsageContext
          , parametersParameterValueDosage = valueDosage
          , parametersParameterResource = resource
          , parametersParameterPart = part
          }




