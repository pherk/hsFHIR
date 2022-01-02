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
-- FHIR 4.0.0 SearchParameter
--

module Data.FHIR.Resources.SearchParameter where

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

data SearchParameterType
    = SPTNumber
    | SPTDate
    | SPTString
    | SPTToken
    | SPTReference
    | SPTComposite
    | SPTQuantity
    | SPTUri
    | SPTSpecial
  deriving (Eq, Show)

instance ToJSON SearchParameterType where
    toJSON SPTNumber = String "number"
    toJSON SPTDate = String "date"
    toJSON SPTString = String "string"
    toJSON SPTToken = String "token"
    toJSON SPTReference = String "reference"
    toJSON SPTComposite = String "composite"
    toJSON SPTQuantity = String "quantity"
    toJSON SPTUri = String "uri"
    toJSON SPTSpecial = String "special"
instance FromJSON SearchParameterType where
    parseJSON "number" = return SPTNumber
    parseJSON "date" = return SPTDate
    parseJSON "string" = return SPTString
    parseJSON "token" = return SPTToken
    parseJSON "reference" = return SPTReference
    parseJSON "composite" = return SPTComposite
    parseJSON "quantity" = return SPTQuantity
    parseJSON "uri" = return SPTUri
    parseJSON "special" = return SPTSpecial

toSearchParameterType SPTNumber = "number"
toSearchParameterType SPTDate = "date"
toSearchParameterType SPTString = "string"
toSearchParameterType SPTToken = "token"
toSearchParameterType SPTReference = "reference"
toSearchParameterType SPTComposite = "composite"
toSearchParameterType SPTQuantity = "quantity"
toSearchParameterType SPTUri = "uri"
toSearchParameterType SPTSpecial = "special"
fromSearchParameterType "number" = SPTNumber
fromSearchParameterType "date" = SPTDate
fromSearchParameterType "string" = SPTString
fromSearchParameterType "token" = SPTToken
fromSearchParameterType "reference" = SPTReference
fromSearchParameterType "composite" = SPTComposite
fromSearchParameterType "quantity" = SPTQuantity
fromSearchParameterType "uri" = SPTUri
fromSearchParameterType "special" = SPTSpecial


data SearchParameterXpathUsage
    = SPXUNormal
    | SPXUPhonetic
    | SPXUNearby
    | SPXUDistance
    | SPXUOther
  deriving (Eq, Show)

instance ToJSON SearchParameterXpathUsage where
    toJSON SPXUNormal = String "normal"
    toJSON SPXUPhonetic = String "phonetic"
    toJSON SPXUNearby = String "nearby"
    toJSON SPXUDistance = String "distance"
    toJSON SPXUOther = String "other"
instance FromJSON SearchParameterXpathUsage where
    parseJSON "normal" = return SPXUNormal
    parseJSON "phonetic" = return SPXUPhonetic
    parseJSON "nearby" = return SPXUNearby
    parseJSON "distance" = return SPXUDistance
    parseJSON "other" = return SPXUOther

toSearchParameterXpathUsage SPXUNormal = "normal"
toSearchParameterXpathUsage SPXUPhonetic = "phonetic"
toSearchParameterXpathUsage SPXUNearby = "nearby"
toSearchParameterXpathUsage SPXUDistance = "distance"
toSearchParameterXpathUsage SPXUOther = "other"
fromSearchParameterXpathUsage "normal" = SPXUNormal
fromSearchParameterXpathUsage "phonetic" = SPXUPhonetic
fromSearchParameterXpathUsage "nearby" = SPXUNearby
fromSearchParameterXpathUsage "distance" = SPXUDistance
fromSearchParameterXpathUsage "other" = SPXUOther


data SearchParameterComparator
    = SPCEq
    | SPCNe
    | SPCGt
    | SPCLt
    | SPCGe
    | SPCLe
    | SPCSa
    | SPCEb
    | SPCAp
  deriving (Eq, Show)

instance ToJSON SearchParameterComparator where
    toJSON SPCEq = String "eq"
    toJSON SPCNe = String "ne"
    toJSON SPCGt = String "gt"
    toJSON SPCLt = String "lt"
    toJSON SPCGe = String "ge"
    toJSON SPCLe = String "le"
    toJSON SPCSa = String "sa"
    toJSON SPCEb = String "eb"
    toJSON SPCAp = String "ap"
instance FromJSON SearchParameterComparator where
    parseJSON "eq" = return SPCEq
    parseJSON "ne" = return SPCNe
    parseJSON "gt" = return SPCGt
    parseJSON "lt" = return SPCLt
    parseJSON "ge" = return SPCGe
    parseJSON "le" = return SPCLe
    parseJSON "sa" = return SPCSa
    parseJSON "eb" = return SPCEb
    parseJSON "ap" = return SPCAp

toSearchParameterComparator SPCEq = "eq"
toSearchParameterComparator SPCNe = "ne"
toSearchParameterComparator SPCGt = "gt"
toSearchParameterComparator SPCLt = "lt"
toSearchParameterComparator SPCGe = "ge"
toSearchParameterComparator SPCLe = "le"
toSearchParameterComparator SPCSa = "sa"
toSearchParameterComparator SPCEb = "eb"
toSearchParameterComparator SPCAp = "ap"
fromSearchParameterComparator "eq" = SPCEq
fromSearchParameterComparator "ne" = SPCNe
fromSearchParameterComparator "gt" = SPCGt
fromSearchParameterComparator "lt" = SPCLt
fromSearchParameterComparator "ge" = SPCGe
fromSearchParameterComparator "le" = SPCLe
fromSearchParameterComparator "sa" = SPCSa
fromSearchParameterComparator "eb" = SPCEb
fromSearchParameterComparator "ap" = SPCAp


data SearchParameterModifier
    = SPMMissing
    | SPMExact
    | SPMContains
    | SPMNot
    | SPMText
    | SPMIn
    | SPMNotIn
    | SPMBelow
    | SPMAbove
    | SPMType
    | SPMIdentifier
    | SPMOfType
  deriving (Eq, Show)

instance ToJSON SearchParameterModifier where
    toJSON SPMMissing = String "missing"
    toJSON SPMExact = String "exact"
    toJSON SPMContains = String "contains"
    toJSON SPMNot = String "not"
    toJSON SPMText = String "text"
    toJSON SPMIn = String "in"
    toJSON SPMNotIn = String "not-in"
    toJSON SPMBelow = String "below"
    toJSON SPMAbove = String "above"
    toJSON SPMType = String "type"
    toJSON SPMIdentifier = String "identifier"
    toJSON SPMOfType = String "ofType"
instance FromJSON SearchParameterModifier where
    parseJSON "missing" = return SPMMissing
    parseJSON "exact" = return SPMExact
    parseJSON "contains" = return SPMContains
    parseJSON "not" = return SPMNot
    parseJSON "text" = return SPMText
    parseJSON "in" = return SPMIn
    parseJSON "not-in" = return SPMNotIn
    parseJSON "below" = return SPMBelow
    parseJSON "above" = return SPMAbove
    parseJSON "type" = return SPMType
    parseJSON "identifier" = return SPMIdentifier
    parseJSON "ofType" = return SPMOfType

toSearchParameterModifier SPMMissing = "missing"
toSearchParameterModifier SPMExact = "exact"
toSearchParameterModifier SPMContains = "contains"
toSearchParameterModifier SPMNot = "not"
toSearchParameterModifier SPMText = "text"
toSearchParameterModifier SPMIn = "in"
toSearchParameterModifier SPMNotIn = "not-in"
toSearchParameterModifier SPMBelow = "below"
toSearchParameterModifier SPMAbove = "above"
toSearchParameterModifier SPMType = "type"
toSearchParameterModifier SPMIdentifier = "identifier"
toSearchParameterModifier SPMOfType = "ofType"
fromSearchParameterModifier "missing" = SPMMissing
fromSearchParameterModifier "exact" = SPMExact
fromSearchParameterModifier "contains" = SPMContains
fromSearchParameterModifier "not" = SPMNot
fromSearchParameterModifier "text" = SPMText
fromSearchParameterModifier "in" = SPMIn
fromSearchParameterModifier "not-in" = SPMNotIn
fromSearchParameterModifier "below" = SPMBelow
fromSearchParameterModifier "above" = SPMAbove
fromSearchParameterModifier "type" = SPMType
fromSearchParameterModifier "identifier" = SPMIdentifier
fromSearchParameterModifier "ofType" = SPMOfType


data SearchParameter = SearchParameter {
    searchParameterId :: Maybe Id
  , searchParameterMeta :: Maybe Meta
  , searchParameterImplicitRules :: Maybe Uri
  , searchParameterLanguage :: Maybe Language
  , searchParameterText :: Maybe Narrative
--    searchParameterContained :: [ResourceContainer]
  , searchParameterExtension :: [Extension]
  , searchParameterModifierExtension :: [Extension]
  , searchParameterUrl :: Uri
  , searchParameterVersion :: Maybe Text
  , searchParameterName :: Text
  , searchParameterDerivedFrom :: Maybe Canonical
  , searchParameterStatus :: PublicationStatus
  , searchParameterExperimental :: Maybe Boolean
  , searchParameterDate :: Maybe DateTime
  , searchParameterPublisher :: Maybe Text
  , searchParameterContact :: [ContactDetail]
  , searchParameterDescription :: Markdown
  , searchParameterUseContext :: [UsageContext]
  , searchParameterJurisdiction :: [CodeableConcept]
  , searchParameterPurpose :: Maybe Markdown
  , searchParameterCode :: Code
  , searchParameterBase :: [Code]
  , searchParameterType :: SearchParameterType
  , searchParameterExpression :: Maybe Text
  , searchParameterXpath :: Maybe Text
  , searchParameterXpathUsage :: Maybe SearchParameterXpathUsage
  , searchParameterTarget :: [Code]
  , searchParameterMultipleOr :: Maybe Boolean
  , searchParameterMultipleAnd :: Maybe Boolean
  , searchParameterComparator :: [SearchParameterComparator]
  , searchParameterModifier :: [SearchParameterModifier]
  , searchParameterChain :: [Text]
  , searchParameterComponent :: [SearchParameterComponent]
  }
--

instance ToJSON SearchParameter where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "SearchParameter")
    ,  "id" .= toJSON (searchParameterId p)
    ,  "meta" .= toJSON (searchParameterMeta p)
    ,  "implicitRules" .= toJSON (searchParameterImplicitRules p)
    ,  "language" .= toJSON (searchParameterLanguage p)
    ,  "text" .= toJSON (searchParameterText p)
--    , "contained" .= toJSON (searchParameterContained p)
    ,  "extension" .= toJSON (searchParameterExtension p)
    ,  "modifierExtension" .= toJSON (searchParameterModifierExtension p)
    ,  "url" .= toJSON (searchParameterUrl p)
    ,  "version" .= toJSON (searchParameterVersion p)
    ,  "name" .= toJSON (searchParameterName p)
    ,  "derivedFrom" .= toJSON (searchParameterDerivedFrom p)
    ,  "status" .= toJSON (searchParameterStatus p)
    ,  "experimental" .= toJSON (searchParameterExperimental p)
    ,  "date" .= toJSON (searchParameterDate p)
    ,  "publisher" .= toJSON (searchParameterPublisher p)
    ,  "contact" .= toJSON (searchParameterContact p)
    ,  "description" .= toJSON (searchParameterDescription p)
    ,  "useContext" .= toJSON (searchParameterUseContext p)
    ,  "jurisdiction" .= toJSON (searchParameterJurisdiction p)
    ,  "purpose" .= toJSON (searchParameterPurpose p)
    ,  "code" .= toJSON (searchParameterCode p)
    ,  "base" .= toJSON (searchParameterBase p)
    ,  "type" .= toJSON (searchParameterType p)
    ,  "expression" .= toJSON (searchParameterExpression p)
    ,  "xpath" .= toJSON (searchParameterXpath p)
    ,  "xpathUsage" .= toJSON (searchParameterXpathUsage p)
    ,  "target" .= toJSON (searchParameterTarget p)
    ,  "multipleOr" .= toJSON (searchParameterMultipleOr p)
    ,  "multipleAnd" .= toJSON (searchParameterMultipleAnd p)
    ,  "comparator" .= toJSON (searchParameterComparator p)
    ,  "modifier" .= toJSON (searchParameterModifier p)
    ,  "chain" .= toJSON (searchParameterChain p)
    ,  "component" .= toJSON (searchParameterComponent p)
    ]
instance FromJSON SearchParameter where
  parseJSON = withObject "SearchParameter" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "SearchParameter" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        url <- o .:  "url"
        version <- o .:? "version"
        name <- o .:  "name"
        derivedFrom <- o .:? "derivedFrom"
        status <- o .:  "status"
        experimental <- o .:? "experimental"
        date <- o .:? "date"
        publisher <- o .:? "publisher"
        contact <- o .:? "contact" .!= []
        description <- o .:  "description"
        useContext <- o .:? "useContext" .!= []
        jurisdiction <- o .:? "jurisdiction" .!= []
        purpose <- o .:? "purpose"
        code <- o .:  "code"
        base <- o .:? "base" .!= []
        ty <- o .:  "type"
        expression <- o .:? "expression"
        xpath <- o .:? "xpath"
        xpathUsage <- o .:? "xpathUsage"
        target <- o .:? "target" .!= []
        multipleOr <- o .:? "multipleOr"
        multipleAnd <- o .:? "multipleAnd"
        comparator <- o .:? "comparator" .!= []
        modifier <- o .:? "modifier" .!= []
        chain <- o .:? "chain" .!= []
        component <- o .:? "component" .!= []
        return SearchParameter{
            searchParameterId = id
          , searchParameterMeta = meta
          , searchParameterImplicitRules = implicitRules
          , searchParameterLanguage = language
          , searchParameterText = text
--          , searchParameterContained = contained
          , searchParameterExtension = extension
          , searchParameterModifierExtension = modifierExtension
          , searchParameterUrl = url
          , searchParameterVersion = version
          , searchParameterName = name
          , searchParameterDerivedFrom = derivedFrom
          , searchParameterStatus = status
          , searchParameterExperimental = experimental
          , searchParameterDate = date
          , searchParameterPublisher = publisher
          , searchParameterContact = contact
          , searchParameterDescription = description
          , searchParameterUseContext = useContext
          , searchParameterJurisdiction = jurisdiction
          , searchParameterPurpose = purpose
          , searchParameterCode = code
          , searchParameterBase = base
          , searchParameterType = ty
          , searchParameterExpression = expression
          , searchParameterXpath = xpath
          , searchParameterXpathUsage = xpathUsage
          , searchParameterTarget = target
          , searchParameterMultipleOr = multipleOr
          , searchParameterMultipleAnd = multipleAnd
          , searchParameterComparator = comparator
          , searchParameterModifier = modifier
          , searchParameterChain = chain
          , searchParameterComponent = component
          }
      _ -> fail "not a SearchParameter"
instance Xmlbf.ToXml SearchParameter where
  toXml p = Xmlbf.element "SearchParameter" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (searchParameterId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (searchParameterMeta p))
             , OptVal   "implicitRules" (fmap toUri (searchParameterImplicitRules p))
             , OptVal   "language" (fmap toLanguage (searchParameterLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (searchParameterText p))
--             , PropList "contained" (fmap Xmlbf.toXml (searchParameterContained p))
             , PropList "extension" (fmap Xmlbf.toXml (searchParameterExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (searchParameterModifierExtension p))
             , Val      "url" (     toUri (searchParameterUrl p))
             , OptVal   "version" (fmap toString (searchParameterVersion p))
             , Val      "name" (     toString (searchParameterName p))
             , OptVal   "derivedFrom" (fmap toCanonical (searchParameterDerivedFrom p))
             , Val      "status" (     toPublicationStatus (searchParameterStatus p))
             , OptVal   "experimental" (fmap toBoolean (searchParameterExperimental p))
             , OptVal   "date" (fmap toDateTime (searchParameterDate p))
             , OptVal   "publisher" (fmap toString (searchParameterPublisher p))
             , PropList "contact" (fmap Xmlbf.toXml (searchParameterContact p))
             , Val      "description" (     toMarkdown (searchParameterDescription p))
             , PropList "useContext" (fmap Xmlbf.toXml (searchParameterUseContext p))
             , PropList "jurisdiction" (fmap Xmlbf.toXml (searchParameterJurisdiction p))
             , OptVal   "purpose" (fmap toMarkdown (searchParameterPurpose p))
             , Val      "code" (     toCode (searchParameterCode p))
             , ValList  "base" (fmap toCode (searchParameterBase p))
             , Val      "type" (     toSearchParameterType (searchParameterType p))
             , OptVal   "expression" (fmap toString (searchParameterExpression p))
             , OptVal   "xpath" (fmap toString (searchParameterXpath p))
             , OptVal   "xpathUsage" (fmap toSearchParameterXpathUsage (searchParameterXpathUsage p))
             , ValList  "target" (fmap toCode (searchParameterTarget p))
             , OptVal   "multipleOr" (fmap toBoolean (searchParameterMultipleOr p))
             , OptVal   "multipleAnd" (fmap toBoolean (searchParameterMultipleAnd p))
             , ValList  "comparator" (fmap toSearchParameterComparator (searchParameterComparator p))
             , ValList  "modifier" (fmap toSearchParameterModifier (searchParameterModifier p))
             , ValList  "chain" (fmap toString (searchParameterChain p))
             , PropList "component" (fmap Xmlbf.toXml (searchParameterComponent p))
             ]
instance Xmlbf.FromXml SearchParameter where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" Xmlbf.fromXml
--    contained <- many     $ Xmlbf.pElement "contained" Xmlbf.fromXml
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    url <-            Xmlbf.pElement "url" (Xmlbf.pAttr "value")
    version <- optional $ Xmlbf.pElement "version" (Xmlbf.pAttr "value")
    name <-            Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    derivedFrom <- optional $ Xmlbf.pElement "derivedFrom" (Xmlbf.pAttr "value")
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    experimental <- optional $ Xmlbf.pElement "experimental" (Xmlbf.pAttr "value")
    date <- optional $ Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    publisher <- optional $ Xmlbf.pElement "publisher" (Xmlbf.pAttr "value")
    contact <- many     $ Xmlbf.pElement "contact" Xmlbf.fromXml
    description <-            Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    useContext <- many     $ Xmlbf.pElement "useContext" Xmlbf.fromXml
    jurisdiction <- many     $ Xmlbf.pElement "jurisdiction" Xmlbf.fromXml
    purpose <- optional $ Xmlbf.pElement "purpose" (Xmlbf.pAttr "value")
    code <-            Xmlbf.pElement "code" (Xmlbf.pAttr "value")
    base <- many     $ Xmlbf.pElement "base" (Xmlbf.pAttr "value")
    ty <-            Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    expression <- optional $ Xmlbf.pElement "expression" (Xmlbf.pAttr "value")
    xpath <- optional $ Xmlbf.pElement "xpath" (Xmlbf.pAttr "value")
    xpathUsage <- optional $ Xmlbf.pElement "xpathUsage" (Xmlbf.pAttr "value")
    target <- many     $ Xmlbf.pElement "target" (Xmlbf.pAttr "value")
    multipleOr <- optional $ Xmlbf.pElement "multipleOr" (Xmlbf.pAttr "value")
    multipleAnd <- optional $ Xmlbf.pElement "multipleAnd" (Xmlbf.pAttr "value")
    comparator <- many     $ Xmlbf.pElement "comparator" (Xmlbf.pAttr "value")
    modifier <- many     $ Xmlbf.pElement "modifier" (Xmlbf.pAttr "value")
    chain <- many     $ Xmlbf.pElement "chain" (Xmlbf.pAttr "value")
    component <- many     $ Xmlbf.pElement "component" Xmlbf.fromXml
    return SearchParameter {
            searchParameterId = fmap fromId id
          , searchParameterMeta = meta
          , searchParameterImplicitRules = fmap fromUri implicitRules
          , searchParameterLanguage = fmap fromLanguage language
          , searchParameterText = text
--          , searchParameterContained = contained
          , searchParameterExtension = extension
          , searchParameterModifierExtension = modifierExtension
          , searchParameterUrl =      fromUri url
          , searchParameterVersion = fmap fromString version
          , searchParameterName =      fromString name
          , searchParameterDerivedFrom = fmap fromCanonical derivedFrom
          , searchParameterStatus =      fromPublicationStatus status
          , searchParameterExperimental = fmap fromBoolean experimental
          , searchParameterDate = fmap fromDateTime date
          , searchParameterPublisher = fmap fromString publisher
          , searchParameterContact = contact
          , searchParameterDescription =      fromMarkdown description
          , searchParameterUseContext = useContext
          , searchParameterJurisdiction = jurisdiction
          , searchParameterPurpose = fmap fromMarkdown purpose
          , searchParameterCode =      fromCode code
          , searchParameterBase = fmap fromCode base
          , searchParameterType =      fromSearchParameterType ty
          , searchParameterExpression = fmap fromString expression
          , searchParameterXpath = fmap fromString xpath
          , searchParameterXpathUsage = fmap fromSearchParameterXpathUsage xpathUsage
          , searchParameterTarget = fmap fromCode target
          , searchParameterMultipleOr = fmap fromBoolean multipleOr
          , searchParameterMultipleAnd = fmap fromBoolean multipleAnd
          , searchParameterComparator = fmap fromSearchParameterComparator comparator
          , searchParameterModifier = fmap fromSearchParameterModifier modifier
          , searchParameterChain = fmap fromString chain
          , searchParameterComponent = component
          }



data SearchParameterComponent = SearchParameterComponent {
    searchParameterComponentAttrId :: Maybe Text
  , searchParameterComponentExtension :: [Extension]
  , searchParameterComponentModifierExtension :: [Extension]
  , searchParameterComponentDefinition :: Canonical
  , searchParameterComponentExpression :: Text
  }
--

instance ToJSON SearchParameterComponent where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (searchParameterComponentAttrId p)
    ,  "extension" .= toJSON (searchParameterComponentExtension p)
    ,  "modifierExtension" .= toJSON (searchParameterComponentModifierExtension p)
    ,  "definition" .= toJSON (searchParameterComponentDefinition p)
    ,  "expression" .= toJSON (searchParameterComponentExpression p)
    ]
instance FromJSON SearchParameterComponent where
  parseJSON = withObject "SearchParameterComponent" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        definition <- o .:  "definition"
        expression <- o .:  "expression"
        return SearchParameterComponent{
            searchParameterComponentAttrId = id
          , searchParameterComponentExtension = extension
          , searchParameterComponentModifierExtension = modifierExtension
          , searchParameterComponentDefinition = definition
          , searchParameterComponentExpression = expression
          }
instance Xmlbf.ToXml SearchParameterComponent where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (searchParameterComponentAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (searchParameterComponentExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (searchParameterComponentModifierExtension p))
             , Val      "definition" (     toCanonical (searchParameterComponentDefinition p))
             , Val      "expression" (     toString (searchParameterComponentExpression p))
             ]
instance Xmlbf.FromXml SearchParameterComponent where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    definition <-            Xmlbf.pElement "definition" (Xmlbf.pAttr "value")
    expression <-            Xmlbf.pElement "expression" (Xmlbf.pAttr "value")
    return SearchParameterComponent {
            searchParameterComponentAttrId = id
          , searchParameterComponentExtension = extension
          , searchParameterComponentModifierExtension = modifierExtension
          , searchParameterComponentDefinition =      fromCanonical definition
          , searchParameterComponentExpression =      fromString expression
          }




