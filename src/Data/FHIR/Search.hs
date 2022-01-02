{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings   #-}

module Data.FHIR.Search where

import qualified Data.Text                   as T
import           Data.Yaml
import           RIO
import qualified RIO.Map                 as HM

data FHIRDataType
  = FdtId 
  | FdtIdentifier
  | FdtBoolean 
  | FdtCode | FdtCoding | FdtCodeableConcept 
  | FdtDate 
  | FdtMode
  | FdtPeriod 
  | FdtQuantity
  | FdtReference 
  | FdtString 
  | FdtTiming
  | FdtToken
  | FdtUri 
  | FdtSpecial
  deriving (Generic, Eq, Show)

instance FromJSON FHIRDataType where
  parseJSON "Id" = return FdtId 
  parseJSON "Identifier" = return FdtIdentifier
  parseJSON "Boolean" = return FdtBoolean 
  parseJSON "Code" = return FdtCode
  parseJSON "Coding" = return FdtCoding
  parseJSON "CodeableConcept" = return FdtCodeableConcept 
  parseJSON "Date" = return FdtDate 
  parseJSON "Mode" = return FdtMode
  parseJSON "Period" = return FdtPeriod 
  parseJSON "Quantity" = return FdtQuantity
  parseJSON "Reference" = return FdtReference 
  parseJSON "String" = return FdtString 
  parseJSON "Timing" = return FdtTiming 
  parseJSON "Token" = return FdtToken
  parseJSON "Uri" = return FdtUri 
  parseJSON "Special" = return FdtSpecial
  parseJSON (String s) = fail $ "invalid FHIR type: " ++ show s

data SearchImplementationStatus
  = SisNyi
  | SisPartial
  | SisFull
  deriving (Generic, Eq, Show)

instance FromJSON SearchImplementationStatus where
  parseJSON "nyi"     = return SisNyi
  parseJSON "partial" = return SisPartial
  parseJSON "full"    = return SisFull
  parseJSON (String s) = fail $ "invalid SearchImplementationStatus: " ++ show s

data SearchDataType
  = SdtBoolean
  | SdtDate
  | SdtNumber
  | SdtReference
  | SdtString
  | SdtToken
  | SdtComposite
  | SdtQuantity
  | SdtUri
  | SdtSpecial
  deriving (Generic, Eq, Show)

instance FromJSON SearchDataType where
  parseJSON "Boolean"   = return SdtBoolean
  parseJSON "Date"      = return SdtDate
  parseJSON "Number"    = return SdtNumber
  parseJSON "Reference" = return SdtReference
  parseJSON "String"    = return SdtString
  parseJSON "Token"     = return SdtToken
  parseJSON "Composite" = return SdtComposite
  parseJSON "Quantity"  = return SdtQuantity
  parseJSON "Uri"       = return SdtUri
  parseJSON "Special"   = return SdtSpecial
  parseJSON (String s) = fail $ "invalid SearchDataType: " ++ show s


data SearchComparator
  = ScEq
  | ScNe
  | ScGt
  | ScLt
  | ScGe
  | ScLe
  | ScSa
  | ScEb
  | ScAp
  deriving (Generic, Eq, Show)

instance FromJSON SearchComparator where
  parseJSON "eq" = return ScEq
  parseJSON "ne" = return ScNe
  parseJSON "gt" = return ScGt
  parseJSON "lt" = return ScLt
  parseJSON "ge" = return ScGe
  parseJSON "le" = return ScLe
  parseJSON "sa" = return ScSa
  parseJSON "eb" = return ScEb
  parseJSON "ap" = return ScAp
  parseJSON (String s) = fail $ "invalid SearchComparator: " ++ show s

data SearchModifier
  = SmMissing
  | SmExact
  | SmContains
  | SmNot
  | SmText
  | SmIn
  | SmNotIn
  | SmBelow
  | SmAbove
  | SmType
  | SmIdentifier
  | SmOfType
  deriving (Generic, Eq, Show)

instance FromJSON SearchModifier where
  parseJSON "missing" = return SmMissing
  parseJSON "exact" = return SmExact
  parseJSON "contains" = return SmContains
  parseJSON "not" = return SmNot
  parseJSON "text" = return SmText
  parseJSON "in" = return SmIn
  parseJSON "not-in" = return SmNotIn
  parseJSON "below" = return SmBelow
  parseJSON "above" = return SmAbove
  parseJSON "type" = return SmType
  parseJSON "identifier" = return SmIdentifier
  parseJSON "of-type" = return SmOfType
  parseJSON (String s) = fail $ "invalid SearchModifier: " ++ show s

data SearchKeyType
  = SktParam
  | SktControl
  | SktElements
  | SktResult
  | SktSpecial
  | SktProp
  | SktOther
  deriving (Generic, Eq, Show, Data, Typeable)
  
instance FromJSON SearchKeyType where
  parseJSON (String "Param") = return SktParam
  parseJSON (String "Control") = return SktControl
  parseJSON (String "Elements") = return SktElements
  parseJSON (String "Result") = return SktResult
  parseJSON (String "Special") = return SktSpecial
  parseJSON (String "Prop") = return SktProp
  parseJSON (String "Other") = return SktOther
  parseJSON (String s) = fail $ "invalid SearchKeyType: " ++ show s

data SearchParameter = SearchParameter 
  { siName           :: Text
  , siFhirDataType   :: FHIRDataType
  , siSearchDataType :: SearchDataType
  , siPath           :: [Text]
  , siTarget         :: [Text]
  , siMultipleOr     :: Bool
  , siMultipleAnd    :: Bool
  , siComparator     :: [SearchComparator]
  , siModifier       :: [SearchModifier]
  } deriving (Generic, Eq, Show)

