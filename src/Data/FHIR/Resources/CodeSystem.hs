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
-- FHIR 4.0.0 CodeSystem
--

module Data.FHIR.Resources.CodeSystem where

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

data CodeSystemHierarchyMeaning
    = CSHMGroupedBy
    | CSHMIsA
    | CSHMPartOf
    | CSHMClassifiedWith
  deriving (Eq, Show)

instance ToJSON CodeSystemHierarchyMeaning where
    toJSON CSHMGroupedBy = String "grouped-by"
    toJSON CSHMIsA = String "is-a"
    toJSON CSHMPartOf = String "part-of"
    toJSON CSHMClassifiedWith = String "classified-with"
instance FromJSON CodeSystemHierarchyMeaning where
    parseJSON "grouped-by" = return CSHMGroupedBy
    parseJSON "is-a" = return CSHMIsA
    parseJSON "part-of" = return CSHMPartOf
    parseJSON "classified-with" = return CSHMClassifiedWith

toCodeSystemHierarchyMeaning CSHMGroupedBy = "grouped-by"
toCodeSystemHierarchyMeaning CSHMIsA = "is-a"
toCodeSystemHierarchyMeaning CSHMPartOf = "part-of"
toCodeSystemHierarchyMeaning CSHMClassifiedWith = "classified-with"
fromCodeSystemHierarchyMeaning "grouped-by" = CSHMGroupedBy
fromCodeSystemHierarchyMeaning "is-a" = CSHMIsA
fromCodeSystemHierarchyMeaning "part-of" = CSHMPartOf
fromCodeSystemHierarchyMeaning "classified-with" = CSHMClassifiedWith


data CodeSystemContent
    = CSCNotPresent
    | CSCExample
    | CSCFragment
    | CSCComplete
    | CSCSupplement
  deriving (Eq, Show)

instance ToJSON CodeSystemContent where
    toJSON CSCNotPresent = String "not-present"
    toJSON CSCExample = String "example"
    toJSON CSCFragment = String "fragment"
    toJSON CSCComplete = String "complete"
    toJSON CSCSupplement = String "supplement"
instance FromJSON CodeSystemContent where
    parseJSON "not-present" = return CSCNotPresent
    parseJSON "example" = return CSCExample
    parseJSON "fragment" = return CSCFragment
    parseJSON "complete" = return CSCComplete
    parseJSON "supplement" = return CSCSupplement

toCodeSystemContent CSCNotPresent = "not-present"
toCodeSystemContent CSCExample = "example"
toCodeSystemContent CSCFragment = "fragment"
toCodeSystemContent CSCComplete = "complete"
toCodeSystemContent CSCSupplement = "supplement"
fromCodeSystemContent "not-present" = CSCNotPresent
fromCodeSystemContent "example" = CSCExample
fromCodeSystemContent "fragment" = CSCFragment
fromCodeSystemContent "complete" = CSCComplete
fromCodeSystemContent "supplement" = CSCSupplement


data CodeSystem = CodeSystem {
    codeSystemId :: Maybe Id
  , codeSystemMeta :: Maybe Meta
  , codeSystemImplicitRules :: Maybe Uri
  , codeSystemLanguage :: Maybe Language
  , codeSystemText :: Maybe Narrative
--    codeSystemContained :: [ResourceContainer]
  , codeSystemExtension :: [Extension]
  , codeSystemModifierExtension :: [Extension]
  , codeSystemUrl :: Maybe Uri
  , codeSystemIdentifier :: [Identifier]
  , codeSystemVersion :: Maybe Text
  , codeSystemName :: Maybe Text
  , codeSystemTitle :: Maybe Text
  , codeSystemStatus :: PublicationStatus
  , codeSystemExperimental :: Maybe Boolean
  , codeSystemDate :: Maybe DateTime
  , codeSystemPublisher :: Maybe Text
  , codeSystemContact :: [ContactDetail]
  , codeSystemDescription :: Maybe Markdown
  , codeSystemUseContext :: [UsageContext]
  , codeSystemJurisdiction :: [CodeableConcept]
  , codeSystemPurpose :: Maybe Markdown
  , codeSystemCopyright :: Maybe Markdown
  , codeSystemCaseSensitive :: Maybe Boolean
  , codeSystemValueSet :: Maybe Canonical
  , codeSystemHierarchyMeaning :: Maybe CodeSystemHierarchyMeaning
  , codeSystemCompositional :: Maybe Boolean
  , codeSystemVersionNeeded :: Maybe Boolean
  , codeSystemContent :: CodeSystemContent
  , codeSystemSupplements :: Maybe Canonical
  , codeSystemCount :: Maybe UnsignedInt
  , codeSystemFilter :: [CodeSystemFilter]
  , codeSystemProperty :: [CodeSystemProperty]
  , codeSystemConcept :: [CodeSystemConcept]
  }
--

instance ToJSON CodeSystem where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "CodeSystem")
    ,  "id" .= toJSON (codeSystemId p)
    ,  "meta" .= toJSON (codeSystemMeta p)
    ,  "implicitRules" .= toJSON (codeSystemImplicitRules p)
    ,  "language" .= toJSON (codeSystemLanguage p)
    ,  "text" .= toJSON (codeSystemText p)
--    , "contained" .= toJSON (codeSystemContained p)
    ,  "extension" .= toJSON (codeSystemExtension p)
    ,  "modifierExtension" .= toJSON (codeSystemModifierExtension p)
    ,  "url" .= toJSON (codeSystemUrl p)
    ,  "identifier" .= toJSON (codeSystemIdentifier p)
    ,  "version" .= toJSON (codeSystemVersion p)
    ,  "name" .= toJSON (codeSystemName p)
    ,  "title" .= toJSON (codeSystemTitle p)
    ,  "status" .= toJSON (codeSystemStatus p)
    ,  "experimental" .= toJSON (codeSystemExperimental p)
    ,  "date" .= toJSON (codeSystemDate p)
    ,  "publisher" .= toJSON (codeSystemPublisher p)
    ,  "contact" .= toJSON (codeSystemContact p)
    ,  "description" .= toJSON (codeSystemDescription p)
    ,  "useContext" .= toJSON (codeSystemUseContext p)
    ,  "jurisdiction" .= toJSON (codeSystemJurisdiction p)
    ,  "purpose" .= toJSON (codeSystemPurpose p)
    ,  "copyright" .= toJSON (codeSystemCopyright p)
    ,  "caseSensitive" .= toJSON (codeSystemCaseSensitive p)
    ,  "valueSet" .= toJSON (codeSystemValueSet p)
    ,  "hierarchyMeaning" .= toJSON (codeSystemHierarchyMeaning p)
    ,  "compositional" .= toJSON (codeSystemCompositional p)
    ,  "versionNeeded" .= toJSON (codeSystemVersionNeeded p)
    ,  "content" .= toJSON (codeSystemContent p)
    ,  "supplements" .= toJSON (codeSystemSupplements p)
    ,  "count" .= toJSON (codeSystemCount p)
    ,  "filter" .= toJSON (codeSystemFilter p)
    ,  "property" .= toJSON (codeSystemProperty p)
    ,  "concept" .= toJSON (codeSystemConcept p)
    ]
instance FromJSON CodeSystem where
  parseJSON = withObject "CodeSystem" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "CodeSystem" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        url <- o .:? "url"
        identifier <- o .:? "identifier" .!= []
        version <- o .:? "version"
        name <- o .:? "name"
        title <- o .:? "title"
        status <- o .:  "status"
        experimental <- o .:? "experimental"
        date <- o .:? "date"
        publisher <- o .:? "publisher"
        contact <- o .:? "contact" .!= []
        description <- o .:? "description"
        useContext <- o .:? "useContext" .!= []
        jurisdiction <- o .:? "jurisdiction" .!= []
        purpose <- o .:? "purpose"
        copyright <- o .:? "copyright"
        caseSensitive <- o .:? "caseSensitive"
        valueSet <- o .:? "valueSet"
        hierarchyMeaning <- o .:? "hierarchyMeaning"
        compositional <- o .:? "compositional"
        versionNeeded <- o .:? "versionNeeded"
        content <- o .:  "content"
        supplements <- o .:? "supplements"
        count <- o .:? "count"
        filter <- o .:? "filter" .!= []
        property <- o .:? "property" .!= []
        concept <- o .:? "concept" .!= []
        return CodeSystem{
            codeSystemId = id
          , codeSystemMeta = meta
          , codeSystemImplicitRules = implicitRules
          , codeSystemLanguage = language
          , codeSystemText = text
--          , codeSystemContained = contained
          , codeSystemExtension = extension
          , codeSystemModifierExtension = modifierExtension
          , codeSystemUrl = url
          , codeSystemIdentifier = identifier
          , codeSystemVersion = version
          , codeSystemName = name
          , codeSystemTitle = title
          , codeSystemStatus = status
          , codeSystemExperimental = experimental
          , codeSystemDate = date
          , codeSystemPublisher = publisher
          , codeSystemContact = contact
          , codeSystemDescription = description
          , codeSystemUseContext = useContext
          , codeSystemJurisdiction = jurisdiction
          , codeSystemPurpose = purpose
          , codeSystemCopyright = copyright
          , codeSystemCaseSensitive = caseSensitive
          , codeSystemValueSet = valueSet
          , codeSystemHierarchyMeaning = hierarchyMeaning
          , codeSystemCompositional = compositional
          , codeSystemVersionNeeded = versionNeeded
          , codeSystemContent = content
          , codeSystemSupplements = supplements
          , codeSystemCount = count
          , codeSystemFilter = filter
          , codeSystemProperty = property
          , codeSystemConcept = concept
          }
      _ -> fail "not a CodeSystem"
instance Xmlbf.ToXml CodeSystem where
  toXml p = Xmlbf.element "CodeSystem" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (codeSystemId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (codeSystemMeta p))
             , OptVal   "implicitRules" (fmap toUri (codeSystemImplicitRules p))
             , OptVal   "language" (fmap toLanguage (codeSystemLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (codeSystemText p))
--             , PropList "contained" (fmap Xmlbf.toXml (codeSystemContained p))
             , PropList "extension" (fmap Xmlbf.toXml (codeSystemExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (codeSystemModifierExtension p))
             , OptVal   "url" (fmap toUri (codeSystemUrl p))
             , PropList "identifier" (fmap Xmlbf.toXml (codeSystemIdentifier p))
             , OptVal   "version" (fmap toString (codeSystemVersion p))
             , OptVal   "name" (fmap toString (codeSystemName p))
             , OptVal   "title" (fmap toString (codeSystemTitle p))
             , Val      "status" (     toPublicationStatus (codeSystemStatus p))
             , OptVal   "experimental" (fmap toBoolean (codeSystemExperimental p))
             , OptVal   "date" (fmap toDateTime (codeSystemDate p))
             , OptVal   "publisher" (fmap toString (codeSystemPublisher p))
             , PropList "contact" (fmap Xmlbf.toXml (codeSystemContact p))
             , OptVal   "description" (fmap toMarkdown (codeSystemDescription p))
             , PropList "useContext" (fmap Xmlbf.toXml (codeSystemUseContext p))
             , PropList "jurisdiction" (fmap Xmlbf.toXml (codeSystemJurisdiction p))
             , OptVal   "purpose" (fmap toMarkdown (codeSystemPurpose p))
             , OptVal   "copyright" (fmap toMarkdown (codeSystemCopyright p))
             , OptVal   "caseSensitive" (fmap toBoolean (codeSystemCaseSensitive p))
             , OptVal   "valueSet" (fmap toCanonical (codeSystemValueSet p))
             , OptVal   "hierarchyMeaning" (fmap toCodeSystemHierarchyMeaning (codeSystemHierarchyMeaning p))
             , OptVal   "compositional" (fmap toBoolean (codeSystemCompositional p))
             , OptVal   "versionNeeded" (fmap toBoolean (codeSystemVersionNeeded p))
             , Val      "content" (     toCodeSystemContent (codeSystemContent p))
             , OptVal   "supplements" (fmap toCanonical (codeSystemSupplements p))
             , OptVal   "count" (fmap toUnsignedInt (codeSystemCount p))
             , PropList "filter" (fmap Xmlbf.toXml (codeSystemFilter p))
             , PropList "property" (fmap Xmlbf.toXml (codeSystemProperty p))
             , PropList "concept" (fmap Xmlbf.toXml (codeSystemConcept p))
             ]
instance Xmlbf.FromXml CodeSystem where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" Xmlbf.fromXml
--    contained <- many     $ Xmlbf.pElement "contained" Xmlbf.fromXml
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    url <- optional $ Xmlbf.pElement "url" (Xmlbf.pAttr "value")
    identifier <- many     $ Xmlbf.pElement "identifier" Xmlbf.fromXml
    version <- optional $ Xmlbf.pElement "version" (Xmlbf.pAttr "value")
    name <- optional $ Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    title <- optional $ Xmlbf.pElement "title" (Xmlbf.pAttr "value")
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    experimental <- optional $ Xmlbf.pElement "experimental" (Xmlbf.pAttr "value")
    date <- optional $ Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    publisher <- optional $ Xmlbf.pElement "publisher" (Xmlbf.pAttr "value")
    contact <- many     $ Xmlbf.pElement "contact" Xmlbf.fromXml
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    useContext <- many     $ Xmlbf.pElement "useContext" Xmlbf.fromXml
    jurisdiction <- many     $ Xmlbf.pElement "jurisdiction" Xmlbf.fromXml
    purpose <- optional $ Xmlbf.pElement "purpose" (Xmlbf.pAttr "value")
    copyright <- optional $ Xmlbf.pElement "copyright" (Xmlbf.pAttr "value")
    caseSensitive <- optional $ Xmlbf.pElement "caseSensitive" (Xmlbf.pAttr "value")
    valueSet <- optional $ Xmlbf.pElement "valueSet" (Xmlbf.pAttr "value")
    hierarchyMeaning <- optional $ Xmlbf.pElement "hierarchyMeaning" (Xmlbf.pAttr "value")
    compositional <- optional $ Xmlbf.pElement "compositional" (Xmlbf.pAttr "value")
    versionNeeded <- optional $ Xmlbf.pElement "versionNeeded" (Xmlbf.pAttr "value")
    content <-            Xmlbf.pElement "content" (Xmlbf.pAttr "value")
    supplements <- optional $ Xmlbf.pElement "supplements" (Xmlbf.pAttr "value")
    count <- optional $ Xmlbf.pElement "count" (Xmlbf.pAttr "value")
    filter <- many     $ Xmlbf.pElement "filter" Xmlbf.fromXml
    property <- many     $ Xmlbf.pElement "property" Xmlbf.fromXml
    concept <- many     $ Xmlbf.pElement "concept" Xmlbf.fromXml
    return CodeSystem {
            codeSystemId = fmap fromId id
          , codeSystemMeta = meta
          , codeSystemImplicitRules = fmap fromUri implicitRules
          , codeSystemLanguage = fmap fromLanguage language
          , codeSystemText = text
--          , codeSystemContained = contained
          , codeSystemExtension = extension
          , codeSystemModifierExtension = modifierExtension
          , codeSystemUrl = fmap fromUri url
          , codeSystemIdentifier = identifier
          , codeSystemVersion = fmap fromString version
          , codeSystemName = fmap fromString name
          , codeSystemTitle = fmap fromString title
          , codeSystemStatus =      fromPublicationStatus status
          , codeSystemExperimental = fmap fromBoolean experimental
          , codeSystemDate = fmap fromDateTime date
          , codeSystemPublisher = fmap fromString publisher
          , codeSystemContact = contact
          , codeSystemDescription = fmap fromMarkdown description
          , codeSystemUseContext = useContext
          , codeSystemJurisdiction = jurisdiction
          , codeSystemPurpose = fmap fromMarkdown purpose
          , codeSystemCopyright = fmap fromMarkdown copyright
          , codeSystemCaseSensitive = fmap fromBoolean caseSensitive
          , codeSystemValueSet = fmap fromCanonical valueSet
          , codeSystemHierarchyMeaning = fmap fromCodeSystemHierarchyMeaning hierarchyMeaning
          , codeSystemCompositional = fmap fromBoolean compositional
          , codeSystemVersionNeeded = fmap fromBoolean versionNeeded
          , codeSystemContent =      fromCodeSystemContent content
          , codeSystemSupplements = fmap fromCanonical supplements
          , codeSystemCount = fmap fromUnsignedInt count
          , codeSystemFilter = filter
          , codeSystemProperty = property
          , codeSystemConcept = concept
          }



data CodeSystemPropertyType
    = CSPTCode
    | CSPTCoding
    | CSPTString
    | CSPTInteger
    | CSPTBoolean
    | CSPTDateTime
    | CSPTDecimal
  deriving (Eq, Show)

instance ToJSON CodeSystemPropertyType where
    toJSON CSPTCode = String "code"
    toJSON CSPTCoding = String "Coding"
    toJSON CSPTString = String "string"
    toJSON CSPTInteger = String "integer"
    toJSON CSPTBoolean = String "boolean"
    toJSON CSPTDateTime = String "dateTime"
    toJSON CSPTDecimal = String "decimal"
instance FromJSON CodeSystemPropertyType where
    parseJSON "code" = return CSPTCode
    parseJSON "Coding" = return CSPTCoding
    parseJSON "string" = return CSPTString
    parseJSON "integer" = return CSPTInteger
    parseJSON "boolean" = return CSPTBoolean
    parseJSON "dateTime" = return CSPTDateTime
    parseJSON "decimal" = return CSPTDecimal

toCodeSystemPropertyType CSPTCode = "code"
toCodeSystemPropertyType CSPTCoding = "Coding"
toCodeSystemPropertyType CSPTString = "string"
toCodeSystemPropertyType CSPTInteger = "integer"
toCodeSystemPropertyType CSPTBoolean = "boolean"
toCodeSystemPropertyType CSPTDateTime = "dateTime"
toCodeSystemPropertyType CSPTDecimal = "decimal"
fromCodeSystemPropertyType "code" = CSPTCode
fromCodeSystemPropertyType "Coding" = CSPTCoding
fromCodeSystemPropertyType "string" = CSPTString
fromCodeSystemPropertyType "integer" = CSPTInteger
fromCodeSystemPropertyType "boolean" = CSPTBoolean
fromCodeSystemPropertyType "dateTime" = CSPTDateTime
fromCodeSystemPropertyType "decimal" = CSPTDecimal


data CodeSystemProperty = CodeSystemProperty {
    codeSystemPropertyAttrId :: Maybe Text
  , codeSystemPropertyExtension :: [Extension]
  , codeSystemPropertyModifierExtension :: [Extension]
  , codeSystemPropertyCode :: Code
  , codeSystemPropertyUri :: Maybe Uri
  , codeSystemPropertyDescription :: Maybe Text
  , codeSystemPropertyType :: CodeSystemPropertyType
  }
--

instance ToJSON CodeSystemProperty where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (codeSystemPropertyAttrId p)
    ,  "extension" .= toJSON (codeSystemPropertyExtension p)
    ,  "modifierExtension" .= toJSON (codeSystemPropertyModifierExtension p)
    ,  "code" .= toJSON (codeSystemPropertyCode p)
    ,  "uri" .= toJSON (codeSystemPropertyUri p)
    ,  "description" .= toJSON (codeSystemPropertyDescription p)
    ,  "type" .= toJSON (codeSystemPropertyType p)
    ]
instance FromJSON CodeSystemProperty where
  parseJSON = withObject "CodeSystemProperty" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:  "code"
        uri <- o .:? "uri"
        description <- o .:? "description"
        ty <- o .:  "type"
        return CodeSystemProperty{
            codeSystemPropertyAttrId = id
          , codeSystemPropertyExtension = extension
          , codeSystemPropertyModifierExtension = modifierExtension
          , codeSystemPropertyCode = code
          , codeSystemPropertyUri = uri
          , codeSystemPropertyDescription = description
          , codeSystemPropertyType = ty
          }
instance Xmlbf.ToXml CodeSystemProperty where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (codeSystemPropertyAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (codeSystemPropertyExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (codeSystemPropertyModifierExtension p))
             , Val      "code" (     toCode (codeSystemPropertyCode p))
             , OptVal   "uri" (fmap toUri (codeSystemPropertyUri p))
             , OptVal   "description" (fmap toString (codeSystemPropertyDescription p))
             , Val      "type" (     toCodeSystemPropertyType (codeSystemPropertyType p))
             ]
instance Xmlbf.FromXml CodeSystemProperty where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" (Xmlbf.pAttr "value")
    uri <- optional $ Xmlbf.pElement "uri" (Xmlbf.pAttr "value")
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    ty <-            Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    return CodeSystemProperty {
            codeSystemPropertyAttrId = id
          , codeSystemPropertyExtension = extension
          , codeSystemPropertyModifierExtension = modifierExtension
          , codeSystemPropertyCode =      fromCode code
          , codeSystemPropertyUri = fmap fromUri uri
          , codeSystemPropertyDescription = fmap fromString description
          , codeSystemPropertyType =      fromCodeSystemPropertyType ty
          }



data CodeSystemFilterOperator
    = CSFOEq
    | CSFOIsA
    | CSFODescendentOf
    | CSFOIsNotA
    | CSFORegex
    | CSFOIn
    | CSFONotIn
    | CSFOGeneralizes
    | CSFOExists
  deriving (Eq, Show)

instance ToJSON CodeSystemFilterOperator where
    toJSON CSFOEq = String "="
    toJSON CSFOIsA = String "is-a"
    toJSON CSFODescendentOf = String "descendent-of"
    toJSON CSFOIsNotA = String "is-not-a"
    toJSON CSFORegex = String "regex"
    toJSON CSFOIn = String "in"
    toJSON CSFONotIn = String "not-in"
    toJSON CSFOGeneralizes = String "generalizes"
    toJSON CSFOExists = String "exists"
instance FromJSON CodeSystemFilterOperator where
    parseJSON "=" = return CSFOEq
    parseJSON "is-a" = return CSFOIsA
    parseJSON "descendent-of" = return CSFODescendentOf
    parseJSON "is-not-a" = return CSFOIsNotA
    parseJSON "regex" = return CSFORegex
    parseJSON "in" = return CSFOIn
    parseJSON "not-in" = return CSFONotIn
    parseJSON "generalizes" = return CSFOGeneralizes
    parseJSON "exists" = return CSFOExists

toCodeSystemFilterOperator CSFOEq = "="
toCodeSystemFilterOperator CSFOIsA = "is-a"
toCodeSystemFilterOperator CSFODescendentOf = "descendent-of"
toCodeSystemFilterOperator CSFOIsNotA = "is-not-a"
toCodeSystemFilterOperator CSFORegex = "regex"
toCodeSystemFilterOperator CSFOIn = "in"
toCodeSystemFilterOperator CSFONotIn = "not-in"
toCodeSystemFilterOperator CSFOGeneralizes = "generalizes"
toCodeSystemFilterOperator CSFOExists = "exists"
fromCodeSystemFilterOperator "=" = CSFOEq
fromCodeSystemFilterOperator "is-a" = CSFOIsA
fromCodeSystemFilterOperator "descendent-of" = CSFODescendentOf
fromCodeSystemFilterOperator "is-not-a" = CSFOIsNotA
fromCodeSystemFilterOperator "regex" = CSFORegex
fromCodeSystemFilterOperator "in" = CSFOIn
fromCodeSystemFilterOperator "not-in" = CSFONotIn
fromCodeSystemFilterOperator "generalizes" = CSFOGeneralizes
fromCodeSystemFilterOperator "exists" = CSFOExists


data CodeSystemFilter = CodeSystemFilter {
    codeSystemFilterAttrId :: Maybe Text
  , codeSystemFilterExtension :: [Extension]
  , codeSystemFilterModifierExtension :: [Extension]
  , codeSystemFilterCode :: Code
  , codeSystemFilterDescription :: Maybe Text
  , codeSystemFilterOperator :: [CodeSystemFilterOperator]
  , codeSystemFilterValue :: Text
  }
--

instance ToJSON CodeSystemFilter where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (codeSystemFilterAttrId p)
    ,  "extension" .= toJSON (codeSystemFilterExtension p)
    ,  "modifierExtension" .= toJSON (codeSystemFilterModifierExtension p)
    ,  "code" .= toJSON (codeSystemFilterCode p)
    ,  "description" .= toJSON (codeSystemFilterDescription p)
    ,  "operator" .= toJSON (codeSystemFilterOperator p)
    ,  "value" .= toJSON (codeSystemFilterValue p)
    ]
instance FromJSON CodeSystemFilter where
  parseJSON = withObject "CodeSystemFilter" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:  "code"
        description <- o .:? "description"
        operator <- o .:? "operator" .!= []
        value <- o .:  "value"
        return CodeSystemFilter{
            codeSystemFilterAttrId = id
          , codeSystemFilterExtension = extension
          , codeSystemFilterModifierExtension = modifierExtension
          , codeSystemFilterCode = code
          , codeSystemFilterDescription = description
          , codeSystemFilterOperator = operator
          , codeSystemFilterValue = value
          }
instance Xmlbf.ToXml CodeSystemFilter where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (codeSystemFilterAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (codeSystemFilterExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (codeSystemFilterModifierExtension p))
             , Val      "code" (     toCode (codeSystemFilterCode p))
             , OptVal   "description" (fmap toString (codeSystemFilterDescription p))
             , ValList  "operator" (fmap toCodeSystemFilterOperator (codeSystemFilterOperator p))
             , Val      "value" (     toString (codeSystemFilterValue p))
             ]
instance Xmlbf.FromXml CodeSystemFilter where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" (Xmlbf.pAttr "value")
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    operator <- many     $ Xmlbf.pElement "operator" (Xmlbf.pAttr "value")
    value <-            Xmlbf.pElement "value" (Xmlbf.pAttr "value")
    return CodeSystemFilter {
            codeSystemFilterAttrId = id
          , codeSystemFilterExtension = extension
          , codeSystemFilterModifierExtension = modifierExtension
          , codeSystemFilterCode =      fromCode code
          , codeSystemFilterDescription = fmap fromString description
          , codeSystemFilterOperator = fmap fromCodeSystemFilterOperator operator
          , codeSystemFilterValue =      fromString value
          }



data CodeSystemProperty1Value
    = CodeSystemProperty1ValueCode Code
    | CodeSystemProperty1ValueCoding Coding
    | CodeSystemProperty1ValueString Text
    | CodeSystemProperty1ValueInteger Integer
    | CodeSystemProperty1ValueBoolean Boolean
    | CodeSystemProperty1ValueDateTime DateTime
    | CodeSystemProperty1ValueDecimal Decimal
    deriving (Eq, Show)

data CodeSystemProperty1 = CodeSystemProperty1 {
    codeSystemProperty1AttrId :: Maybe Text
  , codeSystemProperty1Extension :: [Extension]
  , codeSystemProperty1ModifierExtension :: [Extension]
  , codeSystemProperty1Code :: Code
  , codeSystemProperty1Value :: CodeSystemProperty1Value
  }
--

instance ToJSON CodeSystemProperty1 where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (codeSystemProperty1AttrId p)
    ,  "extension" .= toJSON (codeSystemProperty1Extension p)
    ,  "modifierExtension" .= toJSON (codeSystemProperty1ModifierExtension p)
    ,  "code" .= toJSON (codeSystemProperty1Code p)
    , toValueJSON (codeSystemProperty1Value p)
    ]
    where 
      toValueJSON (     (CodeSystemProperty1ValueCode c)) = ("value", toJSON c)
      toValueJSON (     (CodeSystemProperty1ValueCoding c)) = ("value", toJSON c)
      toValueJSON (     (CodeSystemProperty1ValueString c)) = ("value", toJSON c)
      toValueJSON (     (CodeSystemProperty1ValueInteger c)) = ("value", toJSON c)
      toValueJSON (     (CodeSystemProperty1ValueBoolean c)) = ("value", toJSON c)
      toValueJSON (     (CodeSystemProperty1ValueDateTime c)) = ("value", toJSON c)
      toValueJSON (     (CodeSystemProperty1ValueDecimal c)) = ("value", toJSON c)
instance FromJSON CodeSystemProperty1 where
  parseJSON = withObject "CodeSystemProperty1" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:  "code"
        value <- parseValue o
        return CodeSystemProperty1{
            codeSystemProperty1AttrId = id
          , codeSystemProperty1Extension = extension
          , codeSystemProperty1ModifierExtension = modifierExtension
          , codeSystemProperty1Code = code
          , codeSystemProperty1Value = value
          }
    where 
      parseValue o = parseValueCode o <|> parseValueCoding o <|> parseValueString o <|> parseValueInteger o <|> parseValueBoolean o <|> parseValueDateTime o <|> parseValueDecimal o
      parseValueCode o = do
                has <- o .: "valueCode"
                return $ CodeSystemProperty1ValueCode has
      parseValueCoding o = do
                has <- o .: "valueCoding"
                return $ CodeSystemProperty1ValueCoding has
      parseValueString o = do
                has <- o .: "valueString"
                return $ CodeSystemProperty1ValueString has
      parseValueInteger o = do
                has <- o .: "valueInteger"
                return $ CodeSystemProperty1ValueInteger has
      parseValueBoolean o = do
                has <- o .: "valueBoolean"
                return $ CodeSystemProperty1ValueBoolean has
      parseValueDateTime o = do
                has <- o .: "valueDateTime"
                return $ CodeSystemProperty1ValueDateTime has
      parseValueDecimal o = do
                has <- o .: "valueDecimal"
                return $ CodeSystemProperty1ValueDecimal has
instance Xmlbf.ToXml CodeSystemProperty1 where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (codeSystemProperty1AttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (codeSystemProperty1Extension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (codeSystemProperty1ModifierExtension p))
             , Val      "code" (     toCode (codeSystemProperty1Code p))
             , toValueXml (codeSystemProperty1Value p)
             ]
       where 
          toValueXml (     (CodeSystemProperty1ValueCode p)) = Val      "valueCode" (     toCode p)
          toValueXml (     (CodeSystemProperty1ValueCoding p)) = Prop     "valueCoding" (HM.empty, Xmlbf.toXml p)
          toValueXml (     (CodeSystemProperty1ValueString p)) = Val      "valueString" (     toString p)
          toValueXml (     (CodeSystemProperty1ValueInteger p)) = Val      "valueInteger" (     toInt p)
          toValueXml (     (CodeSystemProperty1ValueBoolean p)) = Val      "valueBoolean" (     toBoolean p)
          toValueXml (     (CodeSystemProperty1ValueDateTime p)) = Val      "valueDateTime" (     toDateTime p)
          toValueXml (     (CodeSystemProperty1ValueDecimal p)) = Val      "valueDecimal" (     toDecimal p)
instance Xmlbf.FromXml CodeSystemProperty1 where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" (Xmlbf.pAttr "value")
    value <- fromValueXml
    return CodeSystemProperty1 {
            codeSystemProperty1AttrId = id
          , codeSystemProperty1Extension = extension
          , codeSystemProperty1ModifierExtension = modifierExtension
          , codeSystemProperty1Code =      fromCode code
          , codeSystemProperty1Value = value
          }

    where 
      fromValueXml = parseValueCode <|> parseValueCoding <|> parseValueString <|> parseValueInteger <|> parseValueBoolean <|> parseValueDateTime <|> parseValueDecimal
      parseValueCode = do
                has <- Xmlbf.pElement "valueCode" (Xmlbf.pAttr "value")
                return $ CodeSystemProperty1ValueCode (     fromCode has)
      parseValueCoding = do
                has <- Xmlbf.pElement "valueCoding" Xmlbf.fromXml
                return $ CodeSystemProperty1ValueCoding (                      has)
      parseValueString = do
                has <- Xmlbf.pElement "valueString" (Xmlbf.pAttr "value")
                return $ CodeSystemProperty1ValueString (     fromString has)
      parseValueInteger = do
                has <- Xmlbf.pElement "valueInteger" (Xmlbf.pAttr "value")
                return $ CodeSystemProperty1ValueInteger (     fromInt has)
      parseValueBoolean = do
                has <- Xmlbf.pElement "valueBoolean" (Xmlbf.pAttr "value")
                return $ CodeSystemProperty1ValueBoolean (     fromBoolean has)
      parseValueDateTime = do
                has <- Xmlbf.pElement "valueDateTime" (Xmlbf.pAttr "value")
                return $ CodeSystemProperty1ValueDateTime (     fromDateTime has)
      parseValueDecimal = do
                has <- Xmlbf.pElement "valueDecimal" (Xmlbf.pAttr "value")
                return $ CodeSystemProperty1ValueDecimal (     fromDecimal has)


data CodeSystemDesignation = CodeSystemDesignation {
    codeSystemDesignationAttrId :: Maybe Text
  , codeSystemDesignationExtension :: [Extension]
  , codeSystemDesignationModifierExtension :: [Extension]
  , codeSystemDesignationLanguage :: Maybe Code
  , codeSystemDesignationUse :: Maybe Coding
  , codeSystemDesignationValue :: Text
  }
--

instance ToJSON CodeSystemDesignation where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (codeSystemDesignationAttrId p)
    ,  "extension" .= toJSON (codeSystemDesignationExtension p)
    ,  "modifierExtension" .= toJSON (codeSystemDesignationModifierExtension p)
    ,  "language" .= toJSON (codeSystemDesignationLanguage p)
    ,  "use" .= toJSON (codeSystemDesignationUse p)
    ,  "value" .= toJSON (codeSystemDesignationValue p)
    ]
instance FromJSON CodeSystemDesignation where
  parseJSON = withObject "CodeSystemDesignation" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        language <- o .:? "language"
        use <- o .:? "use"
        value <- o .:  "value"
        return CodeSystemDesignation{
            codeSystemDesignationAttrId = id
          , codeSystemDesignationExtension = extension
          , codeSystemDesignationModifierExtension = modifierExtension
          , codeSystemDesignationLanguage = language
          , codeSystemDesignationUse = use
          , codeSystemDesignationValue = value
          }
instance Xmlbf.ToXml CodeSystemDesignation where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (codeSystemDesignationAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (codeSystemDesignationExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (codeSystemDesignationModifierExtension p))
             , OptVal   "language" (fmap toCode (codeSystemDesignationLanguage p))
             , OptProp  "use" (fmap Xmlbf.toXml (codeSystemDesignationUse p))
             , Val      "value" (     toString (codeSystemDesignationValue p))
             ]
instance Xmlbf.FromXml CodeSystemDesignation where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    use <- optional $ Xmlbf.pElement "use" Xmlbf.fromXml
    value <-            Xmlbf.pElement "value" (Xmlbf.pAttr "value")
    return CodeSystemDesignation {
            codeSystemDesignationAttrId = id
          , codeSystemDesignationExtension = extension
          , codeSystemDesignationModifierExtension = modifierExtension
          , codeSystemDesignationLanguage = fmap fromCode language
          , codeSystemDesignationUse = use
          , codeSystemDesignationValue =      fromString value
          }



data CodeSystemConcept = CodeSystemConcept {
    codeSystemConceptAttrId :: Maybe Text
  , codeSystemConceptExtension :: [Extension]
  , codeSystemConceptModifierExtension :: [Extension]
  , codeSystemConceptCode :: Code
  , codeSystemConceptDisplay :: Maybe Text
  , codeSystemConceptDefinition :: Maybe Text
  , codeSystemConceptDesignation :: [CodeSystemDesignation]
  , codeSystemConceptProperty :: [CodeSystemProperty1]
  , codeSystemConceptConcept :: [CodeSystemConcept]
  }
--

instance ToJSON CodeSystemConcept where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (codeSystemConceptAttrId p)
    ,  "extension" .= toJSON (codeSystemConceptExtension p)
    ,  "modifierExtension" .= toJSON (codeSystemConceptModifierExtension p)
    ,  "code" .= toJSON (codeSystemConceptCode p)
    ,  "display" .= toJSON (codeSystemConceptDisplay p)
    ,  "definition" .= toJSON (codeSystemConceptDefinition p)
    ,  "designation" .= toJSON (codeSystemConceptDesignation p)
    ,  "property" .= toJSON (codeSystemConceptProperty p)
    ,  "concept" .= toJSON (codeSystemConceptConcept p)
    ]
instance FromJSON CodeSystemConcept where
  parseJSON = withObject "CodeSystemConcept" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:  "code"
        display <- o .:? "display"
        definition <- o .:? "definition"
        designation <- o .:? "designation" .!= []
        property <- o .:? "property" .!= []
        concept <- o .:? "concept" .!= []
        return CodeSystemConcept{
            codeSystemConceptAttrId = id
          , codeSystemConceptExtension = extension
          , codeSystemConceptModifierExtension = modifierExtension
          , codeSystemConceptCode = code
          , codeSystemConceptDisplay = display
          , codeSystemConceptDefinition = definition
          , codeSystemConceptDesignation = designation
          , codeSystemConceptProperty = property
          , codeSystemConceptConcept = concept
          }
instance Xmlbf.ToXml CodeSystemConcept where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (codeSystemConceptAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (codeSystemConceptExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (codeSystemConceptModifierExtension p))
             , Val      "code" (     toCode (codeSystemConceptCode p))
             , OptVal   "display" (fmap toString (codeSystemConceptDisplay p))
             , OptVal   "definition" (fmap toString (codeSystemConceptDefinition p))
             , PropList "designation" (fmap Xmlbf.toXml (codeSystemConceptDesignation p))
             , PropList "property" (fmap Xmlbf.toXml (codeSystemConceptProperty p))
             , PropList "concept" (fmap Xmlbf.toXml (codeSystemConceptConcept p))
             ]
instance Xmlbf.FromXml CodeSystemConcept where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" (Xmlbf.pAttr "value")
    display <- optional $ Xmlbf.pElement "display" (Xmlbf.pAttr "value")
    definition <- optional $ Xmlbf.pElement "definition" (Xmlbf.pAttr "value")
    designation <- many     $ Xmlbf.pElement "designation" Xmlbf.fromXml
    property <- many     $ Xmlbf.pElement "property" Xmlbf.fromXml
    concept <- many     $ Xmlbf.pElement "concept" Xmlbf.fromXml
    return CodeSystemConcept {
            codeSystemConceptAttrId = id
          , codeSystemConceptExtension = extension
          , codeSystemConceptModifierExtension = modifierExtension
          , codeSystemConceptCode =      fromCode code
          , codeSystemConceptDisplay = fmap fromString display
          , codeSystemConceptDefinition = fmap fromString definition
          , codeSystemConceptDesignation = designation
          , codeSystemConceptProperty = property
          , codeSystemConceptConcept = concept
          }




