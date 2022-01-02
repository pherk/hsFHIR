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
-- FHIR 4.0.0 Library
--

module Data.FHIR.Resources.Library where

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

data LibrarySubject
    = LibrarySubjectCodeableConcept CodeableConcept
    | LibrarySubjectReference Reference
    deriving (Eq, Show)

data Library = Library {
    libraryId :: Maybe Id
  , libraryMeta :: Maybe Meta
  , libraryImplicitRules :: Maybe Uri
  , libraryLanguage :: Maybe Language
  , libraryText :: Maybe Narrative
--    libraryContained :: [ResourceContainer]
  , libraryExtension :: [Extension]
  , libraryModifierExtension :: [Extension]
  , libraryUrl :: Maybe Uri
  , libraryIdentifier :: [Identifier]
  , libraryVersion :: Maybe Text
  , libraryName :: Maybe Text
  , libraryTitle :: Maybe Text
  , librarySubtitle :: Maybe Text
  , libraryStatus :: PublicationStatus
  , libraryExperimental :: Maybe Boolean
  , libraryType :: CodeableConcept
  , librarySubject :: Maybe LibrarySubject
  , libraryDate :: Maybe DateTime
  , libraryPublisher :: Maybe Text
  , libraryContact :: [ContactDetail]
  , libraryDescription :: Maybe Markdown
  , libraryUseContext :: [UsageContext]
  , libraryJurisdiction :: [CodeableConcept]
  , libraryPurpose :: Maybe Markdown
  , libraryUsage :: Maybe Text
  , libraryCopyright :: Maybe Markdown
  , libraryApprovalDate :: Maybe Date
  , libraryLastReviewDate :: Maybe Date
  , libraryEffectivePeriod :: Maybe Period
  , libraryTopic :: [CodeableConcept]
  , libraryAuthor :: [ContactDetail]
  , libraryEditor :: [ContactDetail]
  , libraryReviewer :: [ContactDetail]
  , libraryEndorser :: [ContactDetail]
  , libraryRelatedArtifact :: [RelatedArtifact]
  , libraryParameter :: [ParameterDefinition]
  , libraryDataRequirement :: [DataRequirement]
  , libraryContent :: [Attachment]
  }
--

instance ToJSON Library where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Library")
    ,  "id" .= toJSON (libraryId p)
    ,  "meta" .= toJSON (libraryMeta p)
    ,  "implicitRules" .= toJSON (libraryImplicitRules p)
    ,  "language" .= toJSON (libraryLanguage p)
    ,  "text" .= toJSON (libraryText p)
--    , "contained" .= toJSON (libraryContained p)
    ,  "extension" .= toJSON (libraryExtension p)
    ,  "modifierExtension" .= toJSON (libraryModifierExtension p)
    ,  "url" .= toJSON (libraryUrl p)
    ,  "identifier" .= toJSON (libraryIdentifier p)
    ,  "version" .= toJSON (libraryVersion p)
    ,  "name" .= toJSON (libraryName p)
    ,  "title" .= toJSON (libraryTitle p)
    ,  "subtitle" .= toJSON (librarySubtitle p)
    ,  "status" .= toJSON (libraryStatus p)
    ,  "experimental" .= toJSON (libraryExperimental p)
    ,  "type" .= toJSON (libraryType p)
    , toSubjectJSON (librarySubject p)
    ,  "date" .= toJSON (libraryDate p)
    ,  "publisher" .= toJSON (libraryPublisher p)
    ,  "contact" .= toJSON (libraryContact p)
    ,  "description" .= toJSON (libraryDescription p)
    ,  "useContext" .= toJSON (libraryUseContext p)
    ,  "jurisdiction" .= toJSON (libraryJurisdiction p)
    ,  "purpose" .= toJSON (libraryPurpose p)
    ,  "usage" .= toJSON (libraryUsage p)
    ,  "copyright" .= toJSON (libraryCopyright p)
    ,  "approvalDate" .= toJSON (libraryApprovalDate p)
    ,  "lastReviewDate" .= toJSON (libraryLastReviewDate p)
    ,  "effectivePeriod" .= toJSON (libraryEffectivePeriod p)
    ,  "topic" .= toJSON (libraryTopic p)
    ,  "author" .= toJSON (libraryAuthor p)
    ,  "editor" .= toJSON (libraryEditor p)
    ,  "reviewer" .= toJSON (libraryReviewer p)
    ,  "endorser" .= toJSON (libraryEndorser p)
    ,  "relatedArtifact" .= toJSON (libraryRelatedArtifact p)
    ,  "parameter" .= toJSON (libraryParameter p)
    ,  "dataRequirement" .= toJSON (libraryDataRequirement p)
    ,  "content" .= toJSON (libraryContent p)
    ]
    where 
      toSubjectJSON (     Nothing   ) = ("subject", Null)
      toSubjectJSON (Just (LibrarySubjectCodeableConcept c)) = ("subject", toJSON c)
      toSubjectJSON (Just (LibrarySubjectReference c)) = ("subject", toJSON c)
instance FromJSON Library where
  parseJSON = withObject "Library" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Library" -> do
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
        subtitle <- o .:? "subtitle"
        status <- o .:  "status"
        experimental <- o .:? "experimental"
        ty <- o .:  "type"
        subject <- parseSubject o
        date <- o .:? "date"
        publisher <- o .:? "publisher"
        contact <- o .:? "contact" .!= []
        description <- o .:? "description"
        useContext <- o .:? "useContext" .!= []
        jurisdiction <- o .:? "jurisdiction" .!= []
        purpose <- o .:? "purpose"
        usage <- o .:? "usage"
        copyright <- o .:? "copyright"
        approvalDate <- o .:? "approvalDate"
        lastReviewDate <- o .:? "lastReviewDate"
        effectivePeriod <- o .:? "effectivePeriod"
        topic <- o .:? "topic" .!= []
        author <- o .:? "author" .!= []
        editor <- o .:? "editor" .!= []
        reviewer <- o .:? "reviewer" .!= []
        endorser <- o .:? "endorser" .!= []
        relatedArtifact <- o .:? "relatedArtifact" .!= []
        parameter <- o .:? "parameter" .!= []
        dataRequirement <- o .:? "dataRequirement" .!= []
        content <- o .:? "content" .!= []
        return Library{
            libraryId = id
          , libraryMeta = meta
          , libraryImplicitRules = implicitRules
          , libraryLanguage = language
          , libraryText = text
--          , libraryContained = contained
          , libraryExtension = extension
          , libraryModifierExtension = modifierExtension
          , libraryUrl = url
          , libraryIdentifier = identifier
          , libraryVersion = version
          , libraryName = name
          , libraryTitle = title
          , librarySubtitle = subtitle
          , libraryStatus = status
          , libraryExperimental = experimental
          , libraryType = ty
          , librarySubject = subject
          , libraryDate = date
          , libraryPublisher = publisher
          , libraryContact = contact
          , libraryDescription = description
          , libraryUseContext = useContext
          , libraryJurisdiction = jurisdiction
          , libraryPurpose = purpose
          , libraryUsage = usage
          , libraryCopyright = copyright
          , libraryApprovalDate = approvalDate
          , libraryLastReviewDate = lastReviewDate
          , libraryEffectivePeriod = effectivePeriod
          , libraryTopic = topic
          , libraryAuthor = author
          , libraryEditor = editor
          , libraryReviewer = reviewer
          , libraryEndorser = endorser
          , libraryRelatedArtifact = relatedArtifact
          , libraryParameter = parameter
          , libraryDataRequirement = dataRequirement
          , libraryContent = content
          }
      _ -> fail "not a Library"
    where 
      parseSubject o = parseSubjectCodeableConcept o <|> parseSubjectReference o
      parseSubjectCodeableConcept o = do
                has <- o .: "subjectCodeableConcept"
                return $ Just (LibrarySubjectCodeableConcept has)
      parseSubjectReference o = do
                has <- o .: "subjectReference"
                return $ Just (LibrarySubjectReference has)
instance Xmlbf.ToXml Library where
  toXml p = Xmlbf.element "Library" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (libraryId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (libraryMeta p))
             , OptVal   "implicitRules" (fmap toUri (libraryImplicitRules p))
             , OptVal   "language" (fmap toLanguage (libraryLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (libraryText p))
--             , PropList "contained" (fmap Xmlbf.toXml (libraryContained p))
             , PropList "extension" (fmap Xmlbf.toXml (libraryExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (libraryModifierExtension p))
             , OptVal   "url" (fmap toUri (libraryUrl p))
             , PropList "identifier" (fmap Xmlbf.toXml (libraryIdentifier p))
             , OptVal   "version" (fmap toString (libraryVersion p))
             , OptVal   "name" (fmap toString (libraryName p))
             , OptVal   "title" (fmap toString (libraryTitle p))
             , OptVal   "subtitle" (fmap toString (librarySubtitle p))
             , Val      "status" (     toPublicationStatus (libraryStatus p))
             , OptVal   "experimental" (fmap toBoolean (libraryExperimental p))
             , Prop     "type" (HM.empty, Xmlbf.toXml (libraryType p))
             , toSubjectXml (librarySubject p)
             , OptVal   "date" (fmap toDateTime (libraryDate p))
             , OptVal   "publisher" (fmap toString (libraryPublisher p))
             , PropList "contact" (fmap Xmlbf.toXml (libraryContact p))
             , OptVal   "description" (fmap toMarkdown (libraryDescription p))
             , PropList "useContext" (fmap Xmlbf.toXml (libraryUseContext p))
             , PropList "jurisdiction" (fmap Xmlbf.toXml (libraryJurisdiction p))
             , OptVal   "purpose" (fmap toMarkdown (libraryPurpose p))
             , OptVal   "usage" (fmap toString (libraryUsage p))
             , OptVal   "copyright" (fmap toMarkdown (libraryCopyright p))
             , OptVal   "approvalDate" (fmap toDate (libraryApprovalDate p))
             , OptVal   "lastReviewDate" (fmap toDate (libraryLastReviewDate p))
             , OptProp  "effectivePeriod" (fmap Xmlbf.toXml (libraryEffectivePeriod p))
             , PropList "topic" (fmap Xmlbf.toXml (libraryTopic p))
             , PropList "author" (fmap Xmlbf.toXml (libraryAuthor p))
             , PropList "editor" (fmap Xmlbf.toXml (libraryEditor p))
             , PropList "reviewer" (fmap Xmlbf.toXml (libraryReviewer p))
             , PropList "endorser" (fmap Xmlbf.toXml (libraryEndorser p))
             , PropList "relatedArtifact" (fmap Xmlbf.toXml (libraryRelatedArtifact p))
             , PropList "parameter" (fmap Xmlbf.toXml (libraryParameter p))
             , PropList "dataRequirement" (fmap Xmlbf.toXml (libraryDataRequirement p))
             , PropList "content" (fmap Xmlbf.toXml (libraryContent p))
             ]
          toSubjectXml ( Nothing   ) = (OptVal "subject" Nothing)
          toSubjectXml (Just (LibrarySubjectCodeableConcept p)) = Prop  "subjectCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toSubjectXml (Just (LibrarySubjectReference p)) = Prop  "subjectReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml Library where
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
    subtitle <- optional $ Xmlbf.pElement "subtitle" (Xmlbf.pAttr "value")
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    experimental <- optional $ Xmlbf.pElement "experimental" (Xmlbf.pAttr "value")
    ty <-            Xmlbf.pElement "type" Xmlbf.fromXml
    subject <- fromSubjectXml
    date <- optional $ Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    publisher <- optional $ Xmlbf.pElement "publisher" (Xmlbf.pAttr "value")
    contact <- many     $ Xmlbf.pElement "contact" Xmlbf.fromXml
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    useContext <- many     $ Xmlbf.pElement "useContext" Xmlbf.fromXml
    jurisdiction <- many     $ Xmlbf.pElement "jurisdiction" Xmlbf.fromXml
    purpose <- optional $ Xmlbf.pElement "purpose" (Xmlbf.pAttr "value")
    usage <- optional $ Xmlbf.pElement "usage" (Xmlbf.pAttr "value")
    copyright <- optional $ Xmlbf.pElement "copyright" (Xmlbf.pAttr "value")
    approvalDate <- optional $ Xmlbf.pElement "approvalDate" (Xmlbf.pAttr "value")
    lastReviewDate <- optional $ Xmlbf.pElement "lastReviewDate" (Xmlbf.pAttr "value")
    effectivePeriod <- optional $ Xmlbf.pElement "effectivePeriod" Xmlbf.fromXml
    topic <- many     $ Xmlbf.pElement "topic" Xmlbf.fromXml
    author <- many     $ Xmlbf.pElement "author" Xmlbf.fromXml
    editor <- many     $ Xmlbf.pElement "editor" Xmlbf.fromXml
    reviewer <- many     $ Xmlbf.pElement "reviewer" Xmlbf.fromXml
    endorser <- many     $ Xmlbf.pElement "endorser" Xmlbf.fromXml
    relatedArtifact <- many     $ Xmlbf.pElement "relatedArtifact" Xmlbf.fromXml
    parameter <- many     $ Xmlbf.pElement "parameter" Xmlbf.fromXml
    dataRequirement <- many     $ Xmlbf.pElement "dataRequirement" Xmlbf.fromXml
    content <- many     $ Xmlbf.pElement "content" Xmlbf.fromXml
    return Library {
            libraryId = fmap fromId id
          , libraryMeta = meta
          , libraryImplicitRules = fmap fromUri implicitRules
          , libraryLanguage = fmap fromLanguage language
          , libraryText = text
--          , libraryContained = contained
          , libraryExtension = extension
          , libraryModifierExtension = modifierExtension
          , libraryUrl = fmap fromUri url
          , libraryIdentifier = identifier
          , libraryVersion = fmap fromString version
          , libraryName = fmap fromString name
          , libraryTitle = fmap fromString title
          , librarySubtitle = fmap fromString subtitle
          , libraryStatus =      fromPublicationStatus status
          , libraryExperimental = fmap fromBoolean experimental
          , libraryType = ty
          , librarySubject = subject
          , libraryDate = fmap fromDateTime date
          , libraryPublisher = fmap fromString publisher
          , libraryContact = contact
          , libraryDescription = fmap fromMarkdown description
          , libraryUseContext = useContext
          , libraryJurisdiction = jurisdiction
          , libraryPurpose = fmap fromMarkdown purpose
          , libraryUsage = fmap fromString usage
          , libraryCopyright = fmap fromMarkdown copyright
          , libraryApprovalDate = fmap fromDate approvalDate
          , libraryLastReviewDate = fmap fromDate lastReviewDate
          , libraryEffectivePeriod = effectivePeriod
          , libraryTopic = topic
          , libraryAuthor = author
          , libraryEditor = editor
          , libraryReviewer = reviewer
          , libraryEndorser = endorser
          , libraryRelatedArtifact = relatedArtifact
          , libraryParameter = parameter
          , libraryDataRequirement = dataRequirement
          , libraryContent = content
          }

    where 
      fromSubjectXml = parseSubjectCodeableConcept <|> parseSubjectReference <|> pure Nothing
      parseSubjectCodeableConcept = do
                has <- Xmlbf.pElement "subjectCodeableConcept" Xmlbf.fromXml
                return $ Just (LibrarySubjectCodeableConcept (                      has))
      parseSubjectReference = do
                has <- Xmlbf.pElement "subjectReference" Xmlbf.fromXml
                return $ Just (LibrarySubjectReference (                      has))



