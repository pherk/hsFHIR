{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators     #-}

{-
 - FHIR Datatypes v4.0.1
 - deviations
 -   primitive datatype cannot have extensions
 -   date and dateTime
 -   super element id folded in, but not all types take extensions yet
 -     only HumanName, Identifier
 -   super backBoneElement id folded in, but not all types take extensions yet
 - Todos
 -   real toJSON and parseJSON instances
 -   toXMl, parseXML instances 
 -   Extension props when TH elminated
-}
module Data.FHIR.Datatypes.ElementDefinition where

import Data.Aeson  
import Data.Aeson.Types hiding (parseJSON)

import Data.Int(Int64)
import Data.Scientific (Scientific)
--import Data.Time.ISO8601.Duration
--import Errors
import Data.Time.Calendar(Day)
import Data.Time.Clock
--import Control.DeepSeq
import Control.Monad.Fix
import Control.Monad.ST
import qualified Data.STRef as STRef
import Data.ByteString (ByteString)
import Data.Functor.Identity
import Data.Time.Format
import Data.Time.LocalTime(ZonedTime)
import Data.Semigroup hiding (Product)
import Data.Word
import qualified GHC.Generics as G
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Xeno.DOM as Xeno
import RIO
import qualified RIO.Text  as T
import qualified RIO.Vector as V

import Data.FHIR.Datatypes.Internal

{-
 - ElementDefinition from metadata
-}

data ReferenceVersionRules
    = RvrEither | RvrIndependent | RvrSpecific
    deriving (Eq, Show)




data ElementDefinitionBase 
    = ElementDefinitionBase { elementDefinitionBaseId :: Maybe Id
            , elementDefinitionBaseExtension :: [Extension]
            , elementDefinitionBaseModifierExtension :: [Extension]
            , elementDefinitionBasePath :: Text
            , elementDefinitionBaseMin :: UnsignedInt
            , elementDefinitionBaseMax :: Text}
    deriving (Eq, Show)

data BindingStrength
    = BsExample | BsExtensible | BsPreferred | BsRequired
    deriving (Eq, Show)



data ElementDefinitionBinding
    = ElementDefinitionBinding { elementDefinitionBindingId :: Maybe Id
            , elementDefinitionBindingExtension :: [Extension]
            , elementDefinitionBindingModifierExtension :: [Extension]
            , elementDefinitionBindingStrength :: BindingStrength
            , elementDefinitionBindingDescription :: Maybe Text
            , elementDefinitionBindingValueSet :: Maybe Canonical}
    deriving (Eq, Show)


data ElementDefinitionConstraint
    = ElementDefinitionConstraint { elementDefinitionConstraintId :: Maybe Id
            , elementDefinitionConstraintExtension :: [Extension]
            , elementDefinitionConstraintModifierExtension :: [Extension]
            , elementDefinitionConstraintKey :: Id
            , elementDefinitionConstraintRequirements :: Maybe Text
            , elementDefinitionConstraintSeverity :: ConstraintSeverity
            , elementDefinitionConstraintHuman :: Text
            , elementDefinitionConstraintExpression :: Maybe Text
            , elementDefinitionConstraintXpath :: Maybe Text
            , elementDefinitionConstraintSource :: Maybe Canonical}
    deriving (Eq, Show)


data ElementDefinitionDiscriminator
    = ElementDefinitionDiscriminator { elementDefinitionDiscriminatorId :: Maybe Id
            , elementDefinitionDiscriminatorExtension :: [Extension]
            , elementDefinitionDiscriminatorModifierExtension :: [Extension]
            , elementDefinitionDiscriminatorType :: DiscriminatorType
            , elementDefinitionDiscriminatorPath :: Text}
    deriving (Eq, Show)


data ElementDefinitionExample
    = ElementDefinitionExample { elementDefinitionExampleId :: Maybe Id
            , elementDefinitionExampleExtension :: [Extension]
            , elementDefinitionExampleModifierExtension :: [Extension]
            , elementDefinitionExamplelabel :: Text
            , elementDefinitionExampleValue :: ElementDefinitionValue}
    deriving (Eq, Show)

data ElementDefinitionMapping
    = ElementDefinitionMapping { elementDefinitionMappingId :: Maybe Id
             , elementDefinitionMappingExtension :: [Extension]
             , elementDefinitionMappingModifierExtension :: [Extension]
             , elementDefinitionMappingIdentity :: Id
             , elementDefinitionMappingLanguage :: Maybe Code
             , elementDefinitionMappingMap :: Text
             , elementDefinitionMappingComment :: Maybe Text}
    deriving (Eq, Show)


data ElementDefinitionSlicing
    = ElementDefinitionSlicing { elementDefinitionSlicingId :: Maybe Id
             , elementDefinitionSlicingExtension :: [Extension]
             , elementDefinitionSlicingModifierExtension :: [Extension]
             , elementDefinitionSlicingDiscriminator :: [ElementDefinitionDiscriminator]
             , elementDefinitionSlicingDescription :: Maybe Text
             , elementDefinitionSlicingOrdered :: Maybe Boolean
             , elementDefinitionSlicingRules :: SlicingRules}
    deriving (Eq, Show)


data ElementDefinitionType
    = ElementDefinitionType { elementDefinitionTypeId :: Maybe Id
             , elementDefinitionTypeExtension :: [Extension]
             , elementDefinitionTypeModifierExtension :: [Extension]
             , elementDefinitionTypeCode :: Uri
             , elementDefinitionTypeProfile :: [Canonical]
             , elementDefinitionTypeTargetProfile :: [Canonical]
             , elementDefinitionTypeAggregation :: [AggregationMode]
             , elementDefinitionTypeVersioning :: Maybe ReferenceVersionRules}
    deriving (Eq, Show)


data ElementDefinitionMinMaxValue
    = ElementDefinitionMinMaxValueDate Date
    | ElementDefinitionMinMaxValueDateTime DateTime
    | ElementDefinitionMinMaxValueInstant Instant
    | ElementDefinitionMinMaxValueTime Time
    | ElementDefinitionMinMaxValueDecimal Decimal
    | ElementDefinitionMinMaxValueInteger Integer
    | ElementDefinitionMinMaxValuePositiveInt PositiveInt
    | ElementDefinitionMinMaxValueUnsignedInt UnsignedInt
    | ElementDefinitionMinMaxValueQuantity Quantity
    deriving (Eq, Show)


data ElementDefinitionValue
    = ElementDefinitionValueBase64Binary Base64Binary
    | ElementDefinitionValueBoolean Boolean
    | ElementDefinitionValueCanonical Canonical
    | ElementDefinitionValueCode Code
    | ElementDefinitionValueDate Date
    | ElementDefinitionValueDateTime DateTime
    | ElementDefinitionValueDecimal Decimal
    | ElementDefinitionValueId Id
    | ElementDefinitionValueInstant Instant
    | ElementDefinitionValueInteger Integer
    | ElementDefinitionValueMarkdown Markdown
    | ElementDefinitionValueOid Oid
    | ElementDefinitionValuePositiveInt PositiveInt
    | ElementDefinitionValueString Text
    | ElementDefinitionValueTime Time
    | ElementDefinitionValueUnsignedInt UnsignedInt
    | ElementDefinitionValueUri Uri
    | ElementDefinitionValueUrl Url
    | ElementDefinitionValueUuid Uuid
    | ElementDefinitionValueAddress Address
    | ElementDefinitionValueAge Age
    | ElementDefinitionValueAnnotation Annotation
    | ElementDefinitionValueAttachment Attachment
    | ElementDefinitionValueCodeableConcept CodeableConcept
    | ElementDefinitionValueCoding Coding
    | ElementDefinitionValueContactPoint ContactPoint
    | ElementDefinitionValueCount Count
    | ElementDefinitionValueDistance Distance
    | ElementDefinitionValueDuration Duration
    | ElementDefinitionValueHumanName HumanName
    | ElementDefinitionValueIdentifier Identifier
    | ElementDefinitionValueMoney Money
    | ElementDefinitionValuePeriod Period
    | ElementDefinitionValueQuantity Quantity
    | ElementDefinitionValueRange Range
    | ElementDefinitionValueRatio Ratio
    | ElementDefinitionValueReference Reference
    | ElementDefinitionValueSampledData SampledData
    | ElementDefinitionValueSignature Signature
    | ElementDefinitionValueTiming Timing
    | ElementDefinitionValueContactDetail ContactDetail
    | ElementDefinitionValueContributor Contributor
    | ElementDefinitionValueDataRequirement DataRequirement
    | ElementDefinitionValueExpression Expression
    | ElementDefinitionValueParameterDefinition ParameterDefinition
    | ElementDefinitionValueRelatedArtifact RelatedArtifact
    | ElementDefinitionValueTriggerDefinition TriggerDefinition
    | ElementDefinitionValueUsageContext UsageContext
    | ElementDefinitionValueDosage Dosage
    | ElementDefinitionValueMeta Meta
    deriving (Eq, Show)


data ElementDefinition
    = ElementDefinition { elementDefinitionId :: Maybe Id
            , elementDefinitionExtension :: [Extension]
            , elementDefinitionModifierExtension :: [Extension]
            , elementDefinitionPath :: Text
            , elementDefinitionRepresentation :: [PropertyRepresentation]
            , elementDefinitionSliceName :: Maybe Text
            , elementDefinitionSliceIsConstraining :: Maybe Boolean
            , elementDefinitionLabel :: Maybe Text
            , elementDefinitionCode :: [Coding]
            , elementDefinitionSlicing :: Maybe ElementDefinitionSlicing
            , elementDefinitionShort :: Maybe Text
            , elementDefinitionDefinition :: Maybe Markdown
            , elementDefinitionComment :: Maybe Markdown
            , elementDefinitionRequirements :: Maybe Markdown
            , elementDefinitionAlias :: [Text]
            , elementDefinitionMin :: Maybe UnsignedInt
            , elementDefinitionMax :: Maybe Text
            , elementDefinitionBase :: Maybe ElementDefinitionBase
            , elementDefinitionContentReference :: Maybe Uri
            , elementDefinitionType :: [ElementDefinitionType]
            , elementDefinitionDefaultValue :: ElementDefinitionValue
            , elementDefinitionMeaningWhenMissing :: Maybe Markdown
            , elementDefinitionOrderMeaning :: Maybe Text
            , elementDefinitionelementDefinitionFixed :: ElementDefinitionValue
            , elementDefinitionelementDefinitionPattern :: ElementDefinitionValue
            , elementDefinitionExample :: [ElementDefinitionExample]
            , elementDefinitionelementDefinitionMinValue :: ElementDefinitionMinMaxValue
            , elementDefinitionelementDefinitionMaxValue :: ElementDefinitionMinMaxValue
            , elementDefinitionMaxLength :: Maybe Integer
            , elementDefinitionCondition :: [Id]
            , elementDefinitionConstraint :: [ElementDefinitionConstraint]
            , elementDefinitionMustSupport :: Maybe Boolean
            , elementDefinitionIsModifier :: Maybe Boolean
            , elementDefinitionIsModifierReason :: Maybe Text
            , elementDefinitionIsSummary :: Maybe Boolean
            , elementDefinitionBinding :: Maybe ElementDefinitionBinding
            , elementDefinitionMapping :: [ElementDefinitionMapping]}
    deriving (Eq, Show)

