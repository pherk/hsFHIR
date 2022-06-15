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
-- FHIR 4.0.0 PlanDefinition
--

module Data.FHIR.Resources.PlanDefinition where

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

data PlanDefinitionSubject
    = PlanDefinitionSubjectCodeableConcept CodeableConcept
    | PlanDefinitionSubjectReference Reference
    deriving (Eq, Show)

data PlanDefinition = PlanDefinition {
    planDefinitionId :: Maybe Id
  , planDefinitionMeta :: Maybe Meta
  , planDefinitionImplicitRules :: Maybe Uri
  , planDefinitionLanguage :: Maybe Language
  , planDefinitionText :: Maybe Narrative
--    planDefinitionContained :: [ResourceContainer]
  , planDefinitionExtension :: [Extension]
  , planDefinitionModifierExtension :: [Extension]
  , planDefinitionUrl :: Maybe Uri
  , planDefinitionIdentifier :: [Identifier]
  , planDefinitionVersion :: Maybe Text
  , planDefinitionName :: Maybe Text
  , planDefinitionTitle :: Maybe Text
  , planDefinitionSubtitle :: Maybe Text
  , planDefinitionType :: Maybe CodeableConcept
  , planDefinitionStatus :: PublicationStatus
  , planDefinitionExperimental :: Maybe Boolean
  , planDefinitionSubject :: Maybe PlanDefinitionSubject
  , planDefinitionDate :: Maybe DateTime
  , planDefinitionPublisher :: Maybe Text
  , planDefinitionContact :: [ContactDetail]
  , planDefinitionDescription :: Maybe Markdown
  , planDefinitionUseContext :: [UsageContext]
  , planDefinitionJurisdiction :: [CodeableConcept]
  , planDefinitionPurpose :: Maybe Markdown
  , planDefinitionUsage :: Maybe Text
  , planDefinitionCopyright :: Maybe Markdown
  , planDefinitionApprovalDate :: Maybe Date
  , planDefinitionLastReviewDate :: Maybe Date
  , planDefinitionEffectivePeriod :: Maybe Period
  , planDefinitionTopic :: [CodeableConcept]
  , planDefinitionAuthor :: [ContactDetail]
  , planDefinitionEditor :: [ContactDetail]
  , planDefinitionReviewer :: [ContactDetail]
  , planDefinitionEndorser :: [ContactDetail]
  , planDefinitionRelatedArtifact :: [RelatedArtifact]
  , planDefinitionLibrary :: [Canonical]
  , planDefinitionGoal :: [PlanDefinitionGoal]
  , planDefinitionAction :: [PlanDefinitionAction]
  }
  deriving (Eq, Show)
--

instance ToJSON PlanDefinition where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "PlanDefinition")
    ,  "id" .= toJSON (planDefinitionId p)
    ,  "meta" .= toJSON (planDefinitionMeta p)
    ,  "implicitRules" .= toJSON (planDefinitionImplicitRules p)
    ,  "language" .= toJSON (planDefinitionLanguage p)
    ,  "text" .= toJSON (planDefinitionText p)
--    , "contained" .= toJSON (planDefinitionContained p)
    ,  "extension" .= toJSON (planDefinitionExtension p)
    ,  "modifierExtension" .= toJSON (planDefinitionModifierExtension p)
    ,  "url" .= toJSON (planDefinitionUrl p)
    ,  "identifier" .= toJSON (planDefinitionIdentifier p)
    ,  "version" .= toJSON (planDefinitionVersion p)
    ,  "name" .= toJSON (planDefinitionName p)
    ,  "title" .= toJSON (planDefinitionTitle p)
    ,  "subtitle" .= toJSON (planDefinitionSubtitle p)
    ,  "type" .= toJSON (planDefinitionType p)
    ,  "status" .= toJSON (planDefinitionStatus p)
    ,  "experimental" .= toJSON (planDefinitionExperimental p)
    , toSubjectJSON (planDefinitionSubject p)
    ,  "date" .= toJSON (planDefinitionDate p)
    ,  "publisher" .= toJSON (planDefinitionPublisher p)
    ,  "contact" .= toJSON (planDefinitionContact p)
    ,  "description" .= toJSON (planDefinitionDescription p)
    ,  "useContext" .= toJSON (planDefinitionUseContext p)
    ,  "jurisdiction" .= toJSON (planDefinitionJurisdiction p)
    ,  "purpose" .= toJSON (planDefinitionPurpose p)
    ,  "usage" .= toJSON (planDefinitionUsage p)
    ,  "copyright" .= toJSON (planDefinitionCopyright p)
    ,  "approvalDate" .= toJSON (planDefinitionApprovalDate p)
    ,  "lastReviewDate" .= toJSON (planDefinitionLastReviewDate p)
    ,  "effectivePeriod" .= toJSON (planDefinitionEffectivePeriod p)
    ,  "topic" .= toJSON (planDefinitionTopic p)
    ,  "author" .= toJSON (planDefinitionAuthor p)
    ,  "editor" .= toJSON (planDefinitionEditor p)
    ,  "reviewer" .= toJSON (planDefinitionReviewer p)
    ,  "endorser" .= toJSON (planDefinitionEndorser p)
    ,  "relatedArtifact" .= toJSON (planDefinitionRelatedArtifact p)
    ,  "library" .= toJSON (planDefinitionLibrary p)
    ,  "goal" .= toJSON (planDefinitionGoal p)
    ,  "action" .= toJSON (planDefinitionAction p)
    ]
    where 
      toSubjectJSON (     Nothing   ) = ("subject", Null)
      toSubjectJSON (Just (PlanDefinitionSubjectCodeableConcept c)) = ("subject", toJSON c)
      toSubjectJSON (Just (PlanDefinitionSubjectReference c)) = ("subject", toJSON c)
instance FromJSON PlanDefinition where
  parseJSON = withObject "PlanDefinition" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "PlanDefinition" -> do
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
        ty <- o .:? "type"
        status <- o .:  "status"
        experimental <- o .:? "experimental"
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
        library <- o .:? "library" .!= []
        goal <- o .:? "goal" .!= []
        action <- o .:? "action" .!= []
        return PlanDefinition{
            planDefinitionId = id
          , planDefinitionMeta = meta
          , planDefinitionImplicitRules = implicitRules
          , planDefinitionLanguage = language
          , planDefinitionText = text
--          , planDefinitionContained = contained
          , planDefinitionExtension = extension
          , planDefinitionModifierExtension = modifierExtension
          , planDefinitionUrl = url
          , planDefinitionIdentifier = identifier
          , planDefinitionVersion = version
          , planDefinitionName = name
          , planDefinitionTitle = title
          , planDefinitionSubtitle = subtitle
          , planDefinitionType = ty
          , planDefinitionStatus = status
          , planDefinitionExperimental = experimental
          , planDefinitionSubject = subject
          , planDefinitionDate = date
          , planDefinitionPublisher = publisher
          , planDefinitionContact = contact
          , planDefinitionDescription = description
          , planDefinitionUseContext = useContext
          , planDefinitionJurisdiction = jurisdiction
          , planDefinitionPurpose = purpose
          , planDefinitionUsage = usage
          , planDefinitionCopyright = copyright
          , planDefinitionApprovalDate = approvalDate
          , planDefinitionLastReviewDate = lastReviewDate
          , planDefinitionEffectivePeriod = effectivePeriod
          , planDefinitionTopic = topic
          , planDefinitionAuthor = author
          , planDefinitionEditor = editor
          , planDefinitionReviewer = reviewer
          , planDefinitionEndorser = endorser
          , planDefinitionRelatedArtifact = relatedArtifact
          , planDefinitionLibrary = library
          , planDefinitionGoal = goal
          , planDefinitionAction = action
          }
      _ -> fail "not a PlanDefinition"
    where 
      parseSubject o = parseSubjectCodeableConcept o <|> parseSubjectReference o
      parseSubjectCodeableConcept o = do
                has <- o .: "subjectCodeableConcept"
                return $ Just (PlanDefinitionSubjectCodeableConcept has)
      parseSubjectReference o = do
                has <- o .: "subjectReference"
                return $ Just (PlanDefinitionSubjectReference has)
instance Xmlbf.ToXml PlanDefinition where
  toXml p = Xmlbf.element "PlanDefinition" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (planDefinitionId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (planDefinitionMeta p))
             , OptVal   "implicitRules" (fmap toUri (planDefinitionImplicitRules p))
             , OptVal   "language" (fmap toLanguage (planDefinitionLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (planDefinitionText p))
--             , PropList "contained" (fmap Xmlbf.toXml (planDefinitionContained p))
             , PropList "extension" (fmap Xmlbf.toXml (planDefinitionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (planDefinitionModifierExtension p))
             , OptVal   "url" (fmap toUri (planDefinitionUrl p))
             , PropList "identifier" (fmap Xmlbf.toXml (planDefinitionIdentifier p))
             , OptVal   "version" (fmap toString (planDefinitionVersion p))
             , OptVal   "name" (fmap toString (planDefinitionName p))
             , OptVal   "title" (fmap toString (planDefinitionTitle p))
             , OptVal   "subtitle" (fmap toString (planDefinitionSubtitle p))
             , OptProp  "type" (fmap Xmlbf.toXml (planDefinitionType p))
             , Val      "status" (     toPublicationStatus (planDefinitionStatus p))
             , OptVal   "experimental" (fmap toBoolean (planDefinitionExperimental p))
             , toSubjectXml (planDefinitionSubject p)
             , OptVal   "date" (fmap toDateTime (planDefinitionDate p))
             , OptVal   "publisher" (fmap toString (planDefinitionPublisher p))
             , PropList "contact" (fmap Xmlbf.toXml (planDefinitionContact p))
             , OptVal   "description" (fmap toMarkdown (planDefinitionDescription p))
             , PropList "useContext" (fmap Xmlbf.toXml (planDefinitionUseContext p))
             , PropList "jurisdiction" (fmap Xmlbf.toXml (planDefinitionJurisdiction p))
             , OptVal   "purpose" (fmap toMarkdown (planDefinitionPurpose p))
             , OptVal   "usage" (fmap toString (planDefinitionUsage p))
             , OptVal   "copyright" (fmap toMarkdown (planDefinitionCopyright p))
             , OptVal   "approvalDate" (fmap toDate (planDefinitionApprovalDate p))
             , OptVal   "lastReviewDate" (fmap toDate (planDefinitionLastReviewDate p))
             , OptProp  "effectivePeriod" (fmap Xmlbf.toXml (planDefinitionEffectivePeriod p))
             , PropList "topic" (fmap Xmlbf.toXml (planDefinitionTopic p))
             , PropList "author" (fmap Xmlbf.toXml (planDefinitionAuthor p))
             , PropList "editor" (fmap Xmlbf.toXml (planDefinitionEditor p))
             , PropList "reviewer" (fmap Xmlbf.toXml (planDefinitionReviewer p))
             , PropList "endorser" (fmap Xmlbf.toXml (planDefinitionEndorser p))
             , PropList "relatedArtifact" (fmap Xmlbf.toXml (planDefinitionRelatedArtifact p))
             , ValList  "library" (fmap toCanonical (planDefinitionLibrary p))
             , PropList "goal" (fmap Xmlbf.toXml (planDefinitionGoal p))
             , PropList "action" (fmap Xmlbf.toXml (planDefinitionAction p))
             ]
          toSubjectXml ( Nothing   ) = (OptVal "subject" Nothing)
          toSubjectXml (Just (PlanDefinitionSubjectCodeableConcept p)) = Prop  "subjectCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toSubjectXml (Just (PlanDefinitionSubjectReference p)) = Prop  "subjectReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml PlanDefinition where
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
    ty <- optional $ Xmlbf.pElement "type" Xmlbf.fromXml
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    experimental <- optional $ Xmlbf.pElement "experimental" (Xmlbf.pAttr "value")
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
    library <- many     $ Xmlbf.pElement "library" (Xmlbf.pAttr "value")
    goal <- many     $ Xmlbf.pElement "goal" Xmlbf.fromXml
    action <- many     $ Xmlbf.pElement "action" Xmlbf.fromXml
    return PlanDefinition {
            planDefinitionId = fmap fromId id
          , planDefinitionMeta = meta
          , planDefinitionImplicitRules = fmap fromUri implicitRules
          , planDefinitionLanguage = fmap fromLanguage language
          , planDefinitionText = text
--          , planDefinitionContained = contained
          , planDefinitionExtension = extension
          , planDefinitionModifierExtension = modifierExtension
          , planDefinitionUrl = fmap fromUri url
          , planDefinitionIdentifier = identifier
          , planDefinitionVersion = fmap fromString version
          , planDefinitionName = fmap fromString name
          , planDefinitionTitle = fmap fromString title
          , planDefinitionSubtitle = fmap fromString subtitle
          , planDefinitionType = ty
          , planDefinitionStatus =      fromPublicationStatus status
          , planDefinitionExperimental = fmap fromBoolean experimental
          , planDefinitionSubject = subject
          , planDefinitionDate = fmap fromDateTime date
          , planDefinitionPublisher = fmap fromString publisher
          , planDefinitionContact = contact
          , planDefinitionDescription = fmap fromMarkdown description
          , planDefinitionUseContext = useContext
          , planDefinitionJurisdiction = jurisdiction
          , planDefinitionPurpose = fmap fromMarkdown purpose
          , planDefinitionUsage = fmap fromString usage
          , planDefinitionCopyright = fmap fromMarkdown copyright
          , planDefinitionApprovalDate = fmap fromDate approvalDate
          , planDefinitionLastReviewDate = fmap fromDate lastReviewDate
          , planDefinitionEffectivePeriod = effectivePeriod
          , planDefinitionTopic = topic
          , planDefinitionAuthor = author
          , planDefinitionEditor = editor
          , planDefinitionReviewer = reviewer
          , planDefinitionEndorser = endorser
          , planDefinitionRelatedArtifact = relatedArtifact
          , planDefinitionLibrary = fmap fromCanonical library
          , planDefinitionGoal = goal
          , planDefinitionAction = action
          }

    where 
      fromSubjectXml = parseSubjectCodeableConcept <|> parseSubjectReference <|> pure Nothing
      parseSubjectCodeableConcept = do
                has <- Xmlbf.pElement "subjectCodeableConcept" Xmlbf.fromXml
                return $ Just (PlanDefinitionSubjectCodeableConcept (                      has))
      parseSubjectReference = do
                has <- Xmlbf.pElement "subjectReference" Xmlbf.fromXml
                return $ Just (PlanDefinitionSubjectReference (                      has))


data PlanDefinitionConditionKind
    = PDCKApplicability
    | PDCKStart
    | PDCKStop
  deriving (Eq, Show)

instance ToJSON PlanDefinitionConditionKind where
    toJSON PDCKApplicability = String "applicability"
    toJSON PDCKStart = String "start"
    toJSON PDCKStop = String "stop"
instance FromJSON PlanDefinitionConditionKind where
    parseJSON "applicability" = return PDCKApplicability
    parseJSON "start" = return PDCKStart
    parseJSON "stop" = return PDCKStop

toPlanDefinitionConditionKind PDCKApplicability = "applicability"
toPlanDefinitionConditionKind PDCKStart = "start"
toPlanDefinitionConditionKind PDCKStop = "stop"
fromPlanDefinitionConditionKind "applicability" = PDCKApplicability
fromPlanDefinitionConditionKind "start" = PDCKStart
fromPlanDefinitionConditionKind "stop" = PDCKStop


data PlanDefinitionCondition = PlanDefinitionCondition {
    planDefinitionConditionAttrId :: Maybe Text
  , planDefinitionConditionExtension :: [Extension]
  , planDefinitionConditionModifierExtension :: [Extension]
  , planDefinitionConditionKind :: PlanDefinitionConditionKind
  , planDefinitionConditionExpression :: Maybe Expression
  }
  deriving (Eq, Show)
--

instance ToJSON PlanDefinitionCondition where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (planDefinitionConditionAttrId p)
    ,  "extension" .= toJSON (planDefinitionConditionExtension p)
    ,  "modifierExtension" .= toJSON (planDefinitionConditionModifierExtension p)
    ,  "kind" .= toJSON (planDefinitionConditionKind p)
    ,  "expression" .= toJSON (planDefinitionConditionExpression p)
    ]
instance FromJSON PlanDefinitionCondition where
  parseJSON = withObject "PlanDefinitionCondition" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        kind <- o .:  "kind"
        expression <- o .:? "expression"
        return PlanDefinitionCondition{
            planDefinitionConditionAttrId = id
          , planDefinitionConditionExtension = extension
          , planDefinitionConditionModifierExtension = modifierExtension
          , planDefinitionConditionKind = kind
          , planDefinitionConditionExpression = expression
          }
instance Xmlbf.ToXml PlanDefinitionCondition where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (planDefinitionConditionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (planDefinitionConditionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (planDefinitionConditionModifierExtension p))
             , Val      "kind" (     toPlanDefinitionConditionKind (planDefinitionConditionKind p))
             , OptProp  "expression" (fmap Xmlbf.toXml (planDefinitionConditionExpression p))
             ]
instance Xmlbf.FromXml PlanDefinitionCondition where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    kind <-            Xmlbf.pElement "kind" (Xmlbf.pAttr "value")
    expression <- optional $ Xmlbf.pElement "expression" Xmlbf.fromXml
    return PlanDefinitionCondition {
            planDefinitionConditionAttrId = id
          , planDefinitionConditionExtension = extension
          , planDefinitionConditionModifierExtension = modifierExtension
          , planDefinitionConditionKind =      fromPlanDefinitionConditionKind kind
          , planDefinitionConditionExpression = expression
          }



data PlanDefinitionRelatedActionRelationship
    = PDRARBeforeStart
    | PDRARBefore
    | PDRARBeforeEnd
    | PDRARConcurrentWithStart
    | PDRARConcurrent
    | PDRARConcurrentWithEnd
    | PDRARAfterStart
    | PDRARAfter
    | PDRARAfterEnd
  deriving (Eq, Show)

instance ToJSON PlanDefinitionRelatedActionRelationship where
    toJSON PDRARBeforeStart = String "before-start"
    toJSON PDRARBefore = String "before"
    toJSON PDRARBeforeEnd = String "before-end"
    toJSON PDRARConcurrentWithStart = String "concurrent-with-start"
    toJSON PDRARConcurrent = String "concurrent"
    toJSON PDRARConcurrentWithEnd = String "concurrent-with-end"
    toJSON PDRARAfterStart = String "after-start"
    toJSON PDRARAfter = String "after"
    toJSON PDRARAfterEnd = String "after-end"
instance FromJSON PlanDefinitionRelatedActionRelationship where
    parseJSON "before-start" = return PDRARBeforeStart
    parseJSON "before" = return PDRARBefore
    parseJSON "before-end" = return PDRARBeforeEnd
    parseJSON "concurrent-with-start" = return PDRARConcurrentWithStart
    parseJSON "concurrent" = return PDRARConcurrent
    parseJSON "concurrent-with-end" = return PDRARConcurrentWithEnd
    parseJSON "after-start" = return PDRARAfterStart
    parseJSON "after" = return PDRARAfter
    parseJSON "after-end" = return PDRARAfterEnd

toPlanDefinitionRelatedActionRelationship PDRARBeforeStart = "before-start"
toPlanDefinitionRelatedActionRelationship PDRARBefore = "before"
toPlanDefinitionRelatedActionRelationship PDRARBeforeEnd = "before-end"
toPlanDefinitionRelatedActionRelationship PDRARConcurrentWithStart = "concurrent-with-start"
toPlanDefinitionRelatedActionRelationship PDRARConcurrent = "concurrent"
toPlanDefinitionRelatedActionRelationship PDRARConcurrentWithEnd = "concurrent-with-end"
toPlanDefinitionRelatedActionRelationship PDRARAfterStart = "after-start"
toPlanDefinitionRelatedActionRelationship PDRARAfter = "after"
toPlanDefinitionRelatedActionRelationship PDRARAfterEnd = "after-end"
fromPlanDefinitionRelatedActionRelationship "before-start" = PDRARBeforeStart
fromPlanDefinitionRelatedActionRelationship "before" = PDRARBefore
fromPlanDefinitionRelatedActionRelationship "before-end" = PDRARBeforeEnd
fromPlanDefinitionRelatedActionRelationship "concurrent-with-start" = PDRARConcurrentWithStart
fromPlanDefinitionRelatedActionRelationship "concurrent" = PDRARConcurrent
fromPlanDefinitionRelatedActionRelationship "concurrent-with-end" = PDRARConcurrentWithEnd
fromPlanDefinitionRelatedActionRelationship "after-start" = PDRARAfterStart
fromPlanDefinitionRelatedActionRelationship "after" = PDRARAfter
fromPlanDefinitionRelatedActionRelationship "after-end" = PDRARAfterEnd


data PlanDefinitionRelatedActionOffset
    = PlanDefinitionRelatedActionOffsetDuration Duration
    | PlanDefinitionRelatedActionOffsetRange Range
    deriving (Eq, Show)

data PlanDefinitionRelatedAction = PlanDefinitionRelatedAction {
    planDefinitionRelatedActionAttrId :: Maybe Text
  , planDefinitionRelatedActionExtension :: [Extension]
  , planDefinitionRelatedActionModifierExtension :: [Extension]
  , planDefinitionRelatedActionActionId :: Id
  , planDefinitionRelatedActionRelationship :: PlanDefinitionRelatedActionRelationship
  , planDefinitionRelatedActionOffset :: Maybe PlanDefinitionRelatedActionOffset
  }
  deriving (Eq, Show)
--

instance ToJSON PlanDefinitionRelatedAction where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (planDefinitionRelatedActionAttrId p)
    ,  "extension" .= toJSON (planDefinitionRelatedActionExtension p)
    ,  "modifierExtension" .= toJSON (planDefinitionRelatedActionModifierExtension p)
    ,  "actionId" .= toJSON (planDefinitionRelatedActionActionId p)
    ,  "relationship" .= toJSON (planDefinitionRelatedActionRelationship p)
    , toOffsetJSON (planDefinitionRelatedActionOffset p)
    ]
    where 
      toOffsetJSON (     Nothing   ) = ("offset", Null)
      toOffsetJSON (Just (PlanDefinitionRelatedActionOffsetDuration c)) = ("offset", toJSON c)
      toOffsetJSON (Just (PlanDefinitionRelatedActionOffsetRange c)) = ("offset", toJSON c)
instance FromJSON PlanDefinitionRelatedAction where
  parseJSON = withObject "PlanDefinitionRelatedAction" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        actionId <- o .:  "actionId"
        relationship <- o .:  "relationship"
        offset <- parseOffset o
        return PlanDefinitionRelatedAction{
            planDefinitionRelatedActionAttrId = id
          , planDefinitionRelatedActionExtension = extension
          , planDefinitionRelatedActionModifierExtension = modifierExtension
          , planDefinitionRelatedActionActionId = actionId
          , planDefinitionRelatedActionRelationship = relationship
          , planDefinitionRelatedActionOffset = offset
          }
    where 
      parseOffset o = parseOffsetDuration o <|> parseOffsetRange o
      parseOffsetDuration o = do
                has <- o .: "offsetDuration"
                return $ Just (PlanDefinitionRelatedActionOffsetDuration has)
      parseOffsetRange o = do
                has <- o .: "offsetRange"
                return $ Just (PlanDefinitionRelatedActionOffsetRange has)
instance Xmlbf.ToXml PlanDefinitionRelatedAction where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (planDefinitionRelatedActionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (planDefinitionRelatedActionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (planDefinitionRelatedActionModifierExtension p))
             , Val      "actionId" (     toId (planDefinitionRelatedActionActionId p))
             , Val      "relationship" (     toPlanDefinitionRelatedActionRelationship (planDefinitionRelatedActionRelationship p))
             , toOffsetXml (planDefinitionRelatedActionOffset p)
             ]
       where 
          toOffsetXml ( Nothing   ) = (OptVal "offset" Nothing)
          toOffsetXml (Just (PlanDefinitionRelatedActionOffsetDuration p)) = Prop  "offsetDuration" (HM.empty, Xmlbf.toXml p)
          toOffsetXml (Just (PlanDefinitionRelatedActionOffsetRange p)) = Prop  "offsetRange" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml PlanDefinitionRelatedAction where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    actionId <-            Xmlbf.pElement "actionId" (Xmlbf.pAttr "value")
    relationship <-            Xmlbf.pElement "relationship" (Xmlbf.pAttr "value")
    offset <- fromOffsetXml
    return PlanDefinitionRelatedAction {
            planDefinitionRelatedActionAttrId = id
          , planDefinitionRelatedActionExtension = extension
          , planDefinitionRelatedActionModifierExtension = modifierExtension
          , planDefinitionRelatedActionActionId =      fromId actionId
          , planDefinitionRelatedActionRelationship =      fromPlanDefinitionRelatedActionRelationship relationship
          , planDefinitionRelatedActionOffset = offset
          }

    where 
      fromOffsetXml = parseOffsetDuration <|> parseOffsetRange <|> pure Nothing
      parseOffsetDuration = do
                has <- Xmlbf.pElement "offsetDuration" Xmlbf.fromXml
                return $ Just (PlanDefinitionRelatedActionOffsetDuration (                      has))
      parseOffsetRange = do
                has <- Xmlbf.pElement "offsetRange" Xmlbf.fromXml
                return $ Just (PlanDefinitionRelatedActionOffsetRange (                      has))


data PlanDefinitionGoal = PlanDefinitionGoal {
    planDefinitionGoalAttrId :: Maybe Text
  , planDefinitionGoalExtension :: [Extension]
  , planDefinitionGoalModifierExtension :: [Extension]
  , planDefinitionGoalCategory :: Maybe CodeableConcept
  , planDefinitionGoalDescription :: CodeableConcept
  , planDefinitionGoalPriority :: Maybe CodeableConcept
  , planDefinitionGoalStart :: Maybe CodeableConcept
  , planDefinitionGoalAddresses :: [CodeableConcept]
  , planDefinitionGoalDocumentation :: [RelatedArtifact]
  , planDefinitionGoalTarget :: [PlanDefinitionTarget]
  }
  deriving (Eq, Show)
--

instance ToJSON PlanDefinitionGoal where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (planDefinitionGoalAttrId p)
    ,  "extension" .= toJSON (planDefinitionGoalExtension p)
    ,  "modifierExtension" .= toJSON (planDefinitionGoalModifierExtension p)
    ,  "category" .= toJSON (planDefinitionGoalCategory p)
    ,  "description" .= toJSON (planDefinitionGoalDescription p)
    ,  "priority" .= toJSON (planDefinitionGoalPriority p)
    ,  "start" .= toJSON (planDefinitionGoalStart p)
    ,  "addresses" .= toJSON (planDefinitionGoalAddresses p)
    ,  "documentation" .= toJSON (planDefinitionGoalDocumentation p)
    ,  "target" .= toJSON (planDefinitionGoalTarget p)
    ]
instance FromJSON PlanDefinitionGoal where
  parseJSON = withObject "PlanDefinitionGoal" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        category <- o .:? "category"
        description <- o .:  "description"
        priority <- o .:? "priority"
        start <- o .:? "start"
        addresses <- o .:? "addresses" .!= []
        documentation <- o .:? "documentation" .!= []
        target <- o .:? "target" .!= []
        return PlanDefinitionGoal{
            planDefinitionGoalAttrId = id
          , planDefinitionGoalExtension = extension
          , planDefinitionGoalModifierExtension = modifierExtension
          , planDefinitionGoalCategory = category
          , planDefinitionGoalDescription = description
          , planDefinitionGoalPriority = priority
          , planDefinitionGoalStart = start
          , planDefinitionGoalAddresses = addresses
          , planDefinitionGoalDocumentation = documentation
          , planDefinitionGoalTarget = target
          }
instance Xmlbf.ToXml PlanDefinitionGoal where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (planDefinitionGoalAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (planDefinitionGoalExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (planDefinitionGoalModifierExtension p))
             , OptProp  "category" (fmap Xmlbf.toXml (planDefinitionGoalCategory p))
             , Prop     "description" (HM.empty, Xmlbf.toXml (planDefinitionGoalDescription p))
             , OptProp  "priority" (fmap Xmlbf.toXml (planDefinitionGoalPriority p))
             , OptProp  "start" (fmap Xmlbf.toXml (planDefinitionGoalStart p))
             , PropList "addresses" (fmap Xmlbf.toXml (planDefinitionGoalAddresses p))
             , PropList "documentation" (fmap Xmlbf.toXml (planDefinitionGoalDocumentation p))
             , PropList "target" (fmap Xmlbf.toXml (planDefinitionGoalTarget p))
             ]
instance Xmlbf.FromXml PlanDefinitionGoal where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    category <- optional $ Xmlbf.pElement "category" Xmlbf.fromXml
    description <-            Xmlbf.pElement "description" Xmlbf.fromXml
    priority <- optional $ Xmlbf.pElement "priority" Xmlbf.fromXml
    start <- optional $ Xmlbf.pElement "start" Xmlbf.fromXml
    addresses <- many     $ Xmlbf.pElement "addresses" Xmlbf.fromXml
    documentation <- many     $ Xmlbf.pElement "documentation" Xmlbf.fromXml
    target <- many     $ Xmlbf.pElement "target" Xmlbf.fromXml
    return PlanDefinitionGoal {
            planDefinitionGoalAttrId = id
          , planDefinitionGoalExtension = extension
          , planDefinitionGoalModifierExtension = modifierExtension
          , planDefinitionGoalCategory = category
          , planDefinitionGoalDescription = description
          , planDefinitionGoalPriority = priority
          , planDefinitionGoalStart = start
          , planDefinitionGoalAddresses = addresses
          , planDefinitionGoalDocumentation = documentation
          , planDefinitionGoalTarget = target
          }



data PlanDefinitionTargetDetail
    = PlanDefinitionTargetDetailQuantity Quantity
    | PlanDefinitionTargetDetailRange Range
    | PlanDefinitionTargetDetailCodeableConcept CodeableConcept
    deriving (Eq, Show)

data PlanDefinitionTarget = PlanDefinitionTarget {
    planDefinitionTargetAttrId :: Maybe Text
  , planDefinitionTargetExtension :: [Extension]
  , planDefinitionTargetModifierExtension :: [Extension]
  , planDefinitionTargetMeasure :: Maybe CodeableConcept
  , planDefinitionTargetDetail :: Maybe PlanDefinitionTargetDetail
  , planDefinitionTargetDue :: Maybe Duration
  }
  deriving (Eq, Show)
--

instance ToJSON PlanDefinitionTarget where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (planDefinitionTargetAttrId p)
    ,  "extension" .= toJSON (planDefinitionTargetExtension p)
    ,  "modifierExtension" .= toJSON (planDefinitionTargetModifierExtension p)
    ,  "measure" .= toJSON (planDefinitionTargetMeasure p)
    , toDetailJSON (planDefinitionTargetDetail p)
    ,  "due" .= toJSON (planDefinitionTargetDue p)
    ]
    where 
      toDetailJSON (     Nothing   ) = ("detail", Null)
      toDetailJSON (Just (PlanDefinitionTargetDetailQuantity c)) = ("detail", toJSON c)
      toDetailJSON (Just (PlanDefinitionTargetDetailRange c)) = ("detail", toJSON c)
      toDetailJSON (Just (PlanDefinitionTargetDetailCodeableConcept c)) = ("detail", toJSON c)
instance FromJSON PlanDefinitionTarget where
  parseJSON = withObject "PlanDefinitionTarget" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        measure <- o .:? "measure"
        detail <- parseDetail o
        due <- o .:? "due"
        return PlanDefinitionTarget{
            planDefinitionTargetAttrId = id
          , planDefinitionTargetExtension = extension
          , planDefinitionTargetModifierExtension = modifierExtension
          , planDefinitionTargetMeasure = measure
          , planDefinitionTargetDetail = detail
          , planDefinitionTargetDue = due
          }
    where 
      parseDetail o = parseDetailQuantity o <|> parseDetailRange o <|> parseDetailCodeableConcept o
      parseDetailQuantity o = do
                has <- o .: "detailQuantity"
                return $ Just (PlanDefinitionTargetDetailQuantity has)
      parseDetailRange o = do
                has <- o .: "detailRange"
                return $ Just (PlanDefinitionTargetDetailRange has)
      parseDetailCodeableConcept o = do
                has <- o .: "detailCodeableConcept"
                return $ Just (PlanDefinitionTargetDetailCodeableConcept has)
instance Xmlbf.ToXml PlanDefinitionTarget where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (planDefinitionTargetAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (planDefinitionTargetExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (planDefinitionTargetModifierExtension p))
             , OptProp  "measure" (fmap Xmlbf.toXml (planDefinitionTargetMeasure p))
             , toDetailXml (planDefinitionTargetDetail p)
             , OptProp  "due" (fmap Xmlbf.toXml (planDefinitionTargetDue p))
             ]
       where 
          toDetailXml ( Nothing   ) = (OptVal "detail" Nothing)
          toDetailXml (Just (PlanDefinitionTargetDetailQuantity p)) = Prop  "detailQuantity" (HM.empty, Xmlbf.toXml p)
          toDetailXml (Just (PlanDefinitionTargetDetailRange p)) = Prop  "detailRange" (HM.empty, Xmlbf.toXml p)
          toDetailXml (Just (PlanDefinitionTargetDetailCodeableConcept p)) = Prop  "detailCodeableConcept" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml PlanDefinitionTarget where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    measure <- optional $ Xmlbf.pElement "measure" Xmlbf.fromXml
    detail <- fromDetailXml
    due <- optional $ Xmlbf.pElement "due" Xmlbf.fromXml
    return PlanDefinitionTarget {
            planDefinitionTargetAttrId = id
          , planDefinitionTargetExtension = extension
          , planDefinitionTargetModifierExtension = modifierExtension
          , planDefinitionTargetMeasure = measure
          , planDefinitionTargetDetail = detail
          , planDefinitionTargetDue = due
          }

    where 
      fromDetailXml = parseDetailQuantity <|> parseDetailRange <|> parseDetailCodeableConcept <|> pure Nothing
      parseDetailQuantity = do
                has <- Xmlbf.pElement "detailQuantity" Xmlbf.fromXml
                return $ Just (PlanDefinitionTargetDetailQuantity (                      has))
      parseDetailRange = do
                has <- Xmlbf.pElement "detailRange" Xmlbf.fromXml
                return $ Just (PlanDefinitionTargetDetailRange (                      has))
      parseDetailCodeableConcept = do
                has <- Xmlbf.pElement "detailCodeableConcept" Xmlbf.fromXml
                return $ Just (PlanDefinitionTargetDetailCodeableConcept (                      has))


data PlanDefinitionParticipantType
    = PDPTPatient
    | PDPTPractitioner
    | PDPTRelatedPerson
    | PDPTDevice
  deriving (Eq, Show)

instance ToJSON PlanDefinitionParticipantType where
    toJSON PDPTPatient = String "patient"
    toJSON PDPTPractitioner = String "practitioner"
    toJSON PDPTRelatedPerson = String "related-person"
    toJSON PDPTDevice = String "device"
instance FromJSON PlanDefinitionParticipantType where
    parseJSON "patient" = return PDPTPatient
    parseJSON "practitioner" = return PDPTPractitioner
    parseJSON "related-person" = return PDPTRelatedPerson
    parseJSON "device" = return PDPTDevice

toPlanDefinitionParticipantType PDPTPatient = "patient"
toPlanDefinitionParticipantType PDPTPractitioner = "practitioner"
toPlanDefinitionParticipantType PDPTRelatedPerson = "related-person"
toPlanDefinitionParticipantType PDPTDevice = "device"
fromPlanDefinitionParticipantType "patient" = PDPTPatient
fromPlanDefinitionParticipantType "practitioner" = PDPTPractitioner
fromPlanDefinitionParticipantType "related-person" = PDPTRelatedPerson
fromPlanDefinitionParticipantType "device" = PDPTDevice


data PlanDefinitionParticipant = PlanDefinitionParticipant {
    planDefinitionParticipantAttrId :: Maybe Text
  , planDefinitionParticipantExtension :: [Extension]
  , planDefinitionParticipantModifierExtension :: [Extension]
  , planDefinitionParticipantType :: PlanDefinitionParticipantType
  , planDefinitionParticipantRole :: Maybe CodeableConcept
  }
  deriving (Eq, Show)
--

instance ToJSON PlanDefinitionParticipant where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (planDefinitionParticipantAttrId p)
    ,  "extension" .= toJSON (planDefinitionParticipantExtension p)
    ,  "modifierExtension" .= toJSON (planDefinitionParticipantModifierExtension p)
    ,  "type" .= toJSON (planDefinitionParticipantType p)
    ,  "role" .= toJSON (planDefinitionParticipantRole p)
    ]
instance FromJSON PlanDefinitionParticipant where
  parseJSON = withObject "PlanDefinitionParticipant" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:  "type"
        role <- o .:? "role"
        return PlanDefinitionParticipant{
            planDefinitionParticipantAttrId = id
          , planDefinitionParticipantExtension = extension
          , planDefinitionParticipantModifierExtension = modifierExtension
          , planDefinitionParticipantType = ty
          , planDefinitionParticipantRole = role
          }
instance Xmlbf.ToXml PlanDefinitionParticipant where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (planDefinitionParticipantAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (planDefinitionParticipantExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (planDefinitionParticipantModifierExtension p))
             , Val      "type" (     toPlanDefinitionParticipantType (planDefinitionParticipantType p))
             , OptProp  "role" (fmap Xmlbf.toXml (planDefinitionParticipantRole p))
             ]
instance Xmlbf.FromXml PlanDefinitionParticipant where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    role <- optional $ Xmlbf.pElement "role" Xmlbf.fromXml
    return PlanDefinitionParticipant {
            planDefinitionParticipantAttrId = id
          , planDefinitionParticipantExtension = extension
          , planDefinitionParticipantModifierExtension = modifierExtension
          , planDefinitionParticipantType =      fromPlanDefinitionParticipantType ty
          , planDefinitionParticipantRole = role
          }



data PlanDefinitionDynamicValue = PlanDefinitionDynamicValue {
    planDefinitionDynamicValueAttrId :: Maybe Text
  , planDefinitionDynamicValueExtension :: [Extension]
  , planDefinitionDynamicValueModifierExtension :: [Extension]
  , planDefinitionDynamicValuePath :: Maybe Text
  , planDefinitionDynamicValueExpression :: Maybe Expression
  }
  deriving (Eq, Show)
--

instance ToJSON PlanDefinitionDynamicValue where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (planDefinitionDynamicValueAttrId p)
    ,  "extension" .= toJSON (planDefinitionDynamicValueExtension p)
    ,  "modifierExtension" .= toJSON (planDefinitionDynamicValueModifierExtension p)
    ,  "path" .= toJSON (planDefinitionDynamicValuePath p)
    ,  "expression" .= toJSON (planDefinitionDynamicValueExpression p)
    ]
instance FromJSON PlanDefinitionDynamicValue where
  parseJSON = withObject "PlanDefinitionDynamicValue" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        path <- o .:? "path"
        expression <- o .:? "expression"
        return PlanDefinitionDynamicValue{
            planDefinitionDynamicValueAttrId = id
          , planDefinitionDynamicValueExtension = extension
          , planDefinitionDynamicValueModifierExtension = modifierExtension
          , planDefinitionDynamicValuePath = path
          , planDefinitionDynamicValueExpression = expression
          }
instance Xmlbf.ToXml PlanDefinitionDynamicValue where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (planDefinitionDynamicValueAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (planDefinitionDynamicValueExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (planDefinitionDynamicValueModifierExtension p))
             , OptVal   "path" (fmap toString (planDefinitionDynamicValuePath p))
             , OptProp  "expression" (fmap Xmlbf.toXml (planDefinitionDynamicValueExpression p))
             ]
instance Xmlbf.FromXml PlanDefinitionDynamicValue where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    path <- optional $ Xmlbf.pElement "path" (Xmlbf.pAttr "value")
    expression <- optional $ Xmlbf.pElement "expression" Xmlbf.fromXml
    return PlanDefinitionDynamicValue {
            planDefinitionDynamicValueAttrId = id
          , planDefinitionDynamicValueExtension = extension
          , planDefinitionDynamicValueModifierExtension = modifierExtension
          , planDefinitionDynamicValuePath = fmap fromString path
          , planDefinitionDynamicValueExpression = expression
          }



data PlanDefinitionActionPriority
    = PDAPRoutine
    | PDAPUrgent
    | PDAPAsap
    | PDAPStat
  deriving (Eq, Show)

instance ToJSON PlanDefinitionActionPriority where
    toJSON PDAPRoutine = String "routine"
    toJSON PDAPUrgent = String "urgent"
    toJSON PDAPAsap = String "asap"
    toJSON PDAPStat = String "stat"
instance FromJSON PlanDefinitionActionPriority where
    parseJSON "routine" = return PDAPRoutine
    parseJSON "urgent" = return PDAPUrgent
    parseJSON "asap" = return PDAPAsap
    parseJSON "stat" = return PDAPStat

toPlanDefinitionActionPriority PDAPRoutine = "routine"
toPlanDefinitionActionPriority PDAPUrgent = "urgent"
toPlanDefinitionActionPriority PDAPAsap = "asap"
toPlanDefinitionActionPriority PDAPStat = "stat"
fromPlanDefinitionActionPriority "routine" = PDAPRoutine
fromPlanDefinitionActionPriority "urgent" = PDAPUrgent
fromPlanDefinitionActionPriority "asap" = PDAPAsap
fromPlanDefinitionActionPriority "stat" = PDAPStat


data PlanDefinitionActionGroupingBehavior
    = PDAGBVisualGroup
    | PDAGBLogicalGroup
    | PDAGBSentenceGroup
  deriving (Eq, Show)

instance ToJSON PlanDefinitionActionGroupingBehavior where
    toJSON PDAGBVisualGroup = String "visual-group"
    toJSON PDAGBLogicalGroup = String "logical-group"
    toJSON PDAGBSentenceGroup = String "sentence-group"
instance FromJSON PlanDefinitionActionGroupingBehavior where
    parseJSON "visual-group" = return PDAGBVisualGroup
    parseJSON "logical-group" = return PDAGBLogicalGroup
    parseJSON "sentence-group" = return PDAGBSentenceGroup

toPlanDefinitionActionGroupingBehavior PDAGBVisualGroup = "visual-group"
toPlanDefinitionActionGroupingBehavior PDAGBLogicalGroup = "logical-group"
toPlanDefinitionActionGroupingBehavior PDAGBSentenceGroup = "sentence-group"
fromPlanDefinitionActionGroupingBehavior "visual-group" = PDAGBVisualGroup
fromPlanDefinitionActionGroupingBehavior "logical-group" = PDAGBLogicalGroup
fromPlanDefinitionActionGroupingBehavior "sentence-group" = PDAGBSentenceGroup


data PlanDefinitionActionSelectionBehavior
    = PDASBAny
    | PDASBAll
    | PDASBAllOrNone
    | PDASBExactlyOne
    | PDASBAtMostOne
    | PDASBOneOrMore
  deriving (Eq, Show)

instance ToJSON PlanDefinitionActionSelectionBehavior where
    toJSON PDASBAny = String "any"
    toJSON PDASBAll = String "all"
    toJSON PDASBAllOrNone = String "all-or-none"
    toJSON PDASBExactlyOne = String "exactly-one"
    toJSON PDASBAtMostOne = String "at-most-one"
    toJSON PDASBOneOrMore = String "one-or-more"
instance FromJSON PlanDefinitionActionSelectionBehavior where
    parseJSON "any" = return PDASBAny
    parseJSON "all" = return PDASBAll
    parseJSON "all-or-none" = return PDASBAllOrNone
    parseJSON "exactly-one" = return PDASBExactlyOne
    parseJSON "at-most-one" = return PDASBAtMostOne
    parseJSON "one-or-more" = return PDASBOneOrMore

toPlanDefinitionActionSelectionBehavior PDASBAny = "any"
toPlanDefinitionActionSelectionBehavior PDASBAll = "all"
toPlanDefinitionActionSelectionBehavior PDASBAllOrNone = "all-or-none"
toPlanDefinitionActionSelectionBehavior PDASBExactlyOne = "exactly-one"
toPlanDefinitionActionSelectionBehavior PDASBAtMostOne = "at-most-one"
toPlanDefinitionActionSelectionBehavior PDASBOneOrMore = "one-or-more"
fromPlanDefinitionActionSelectionBehavior "any" = PDASBAny
fromPlanDefinitionActionSelectionBehavior "all" = PDASBAll
fromPlanDefinitionActionSelectionBehavior "all-or-none" = PDASBAllOrNone
fromPlanDefinitionActionSelectionBehavior "exactly-one" = PDASBExactlyOne
fromPlanDefinitionActionSelectionBehavior "at-most-one" = PDASBAtMostOne
fromPlanDefinitionActionSelectionBehavior "one-or-more" = PDASBOneOrMore


data PlanDefinitionActionRequiredBehavior
    = PDARBMust
    | PDARBCould
    | PDARBMustUnlessDocumented
  deriving (Eq, Show)

instance ToJSON PlanDefinitionActionRequiredBehavior where
    toJSON PDARBMust = String "must"
    toJSON PDARBCould = String "could"
    toJSON PDARBMustUnlessDocumented = String "must-unless-documented"
instance FromJSON PlanDefinitionActionRequiredBehavior where
    parseJSON "must" = return PDARBMust
    parseJSON "could" = return PDARBCould
    parseJSON "must-unless-documented" = return PDARBMustUnlessDocumented

toPlanDefinitionActionRequiredBehavior PDARBMust = "must"
toPlanDefinitionActionRequiredBehavior PDARBCould = "could"
toPlanDefinitionActionRequiredBehavior PDARBMustUnlessDocumented = "must-unless-documented"
fromPlanDefinitionActionRequiredBehavior "must" = PDARBMust
fromPlanDefinitionActionRequiredBehavior "could" = PDARBCould
fromPlanDefinitionActionRequiredBehavior "must-unless-documented" = PDARBMustUnlessDocumented


data PlanDefinitionActionPrecheckBehavior
    = PDAPBYes
    | PDAPBNo
  deriving (Eq, Show)

instance ToJSON PlanDefinitionActionPrecheckBehavior where
    toJSON PDAPBYes = String "yes"
    toJSON PDAPBNo = String "no"
instance FromJSON PlanDefinitionActionPrecheckBehavior where
    parseJSON "yes" = return PDAPBYes
    parseJSON "no" = return PDAPBNo

toPlanDefinitionActionPrecheckBehavior PDAPBYes = "yes"
toPlanDefinitionActionPrecheckBehavior PDAPBNo = "no"
fromPlanDefinitionActionPrecheckBehavior "yes" = PDAPBYes
fromPlanDefinitionActionPrecheckBehavior "no" = PDAPBNo


data PlanDefinitionActionCardinalityBehavior
    = PDACBSingle
    | PDACBMultiple
  deriving (Eq, Show)

instance ToJSON PlanDefinitionActionCardinalityBehavior where
    toJSON PDACBSingle = String "single"
    toJSON PDACBMultiple = String "multiple"
instance FromJSON PlanDefinitionActionCardinalityBehavior where
    parseJSON "single" = return PDACBSingle
    parseJSON "multiple" = return PDACBMultiple

toPlanDefinitionActionCardinalityBehavior PDACBSingle = "single"
toPlanDefinitionActionCardinalityBehavior PDACBMultiple = "multiple"
fromPlanDefinitionActionCardinalityBehavior "single" = PDACBSingle
fromPlanDefinitionActionCardinalityBehavior "multiple" = PDACBMultiple


data PlanDefinitionActionSubject
    = PlanDefinitionActionSubjectCodeableConcept CodeableConcept
    | PlanDefinitionActionSubjectReference Reference
    deriving (Eq, Show)

data PlanDefinitionActionTiming
    = PlanDefinitionActionTimingDateTime DateTime
    | PlanDefinitionActionTimingAge Age
    | PlanDefinitionActionTimingPeriod Period
    | PlanDefinitionActionTimingDuration Duration
    | PlanDefinitionActionTimingRange Range
    | PlanDefinitionActionTimingTiming Timing
    deriving (Eq, Show)

data PlanDefinitionActionDefinition
    = PlanDefinitionActionDefinitionCanonical Canonical
    | PlanDefinitionActionDefinitionUri Uri
    deriving (Eq, Show)

data PlanDefinitionAction = PlanDefinitionAction {
    planDefinitionActionAttrId :: Maybe Text
  , planDefinitionActionExtension :: [Extension]
  , planDefinitionActionModifierExtension :: [Extension]
  , planDefinitionActionPrefix :: Maybe Text
  , planDefinitionActionTitle :: Maybe Text
  , planDefinitionActionDescription :: Maybe Text
  , planDefinitionActionTextEquivalent :: Maybe Text
  , planDefinitionActionPriority :: Maybe PlanDefinitionActionPriority
  , planDefinitionActionCode :: [CodeableConcept]
  , planDefinitionActionReason :: [CodeableConcept]
  , planDefinitionActionDocumentation :: [RelatedArtifact]
  , planDefinitionActionGoalId :: [Id]
  , planDefinitionActionSubject :: Maybe PlanDefinitionActionSubject
  , planDefinitionActionTrigger :: [TriggerDefinition]
  , planDefinitionActionCondition :: [PlanDefinitionCondition]
  , planDefinitionActionInput :: [DataRequirement]
  , planDefinitionActionOutput :: [DataRequirement]
  , planDefinitionActionRelatedAction :: [PlanDefinitionRelatedAction]
  , planDefinitionActionTiming :: Maybe PlanDefinitionActionTiming
  , planDefinitionActionParticipant :: [PlanDefinitionParticipant]
  , planDefinitionActionType :: Maybe CodeableConcept
  , planDefinitionActionGroupingBehavior :: Maybe PlanDefinitionActionGroupingBehavior
  , planDefinitionActionSelectionBehavior :: Maybe PlanDefinitionActionSelectionBehavior
  , planDefinitionActionRequiredBehavior :: Maybe PlanDefinitionActionRequiredBehavior
  , planDefinitionActionPrecheckBehavior :: Maybe PlanDefinitionActionPrecheckBehavior
  , planDefinitionActionCardinalityBehavior :: Maybe PlanDefinitionActionCardinalityBehavior
  , planDefinitionActionDefinition :: Maybe PlanDefinitionActionDefinition
  , planDefinitionActionTransform :: Maybe Canonical
  , planDefinitionActionDynamicValue :: [PlanDefinitionDynamicValue]
  , planDefinitionActionAction :: [PlanDefinitionAction]
  }
  deriving (Eq, Show)
--

instance ToJSON PlanDefinitionAction where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (planDefinitionActionAttrId p)
    ,  "extension" .= toJSON (planDefinitionActionExtension p)
    ,  "modifierExtension" .= toJSON (planDefinitionActionModifierExtension p)
    ,  "prefix" .= toJSON (planDefinitionActionPrefix p)
    ,  "title" .= toJSON (planDefinitionActionTitle p)
    ,  "description" .= toJSON (planDefinitionActionDescription p)
    ,  "textEquivalent" .= toJSON (planDefinitionActionTextEquivalent p)
    ,  "priority" .= toJSON (planDefinitionActionPriority p)
    ,  "code" .= toJSON (planDefinitionActionCode p)
    ,  "reason" .= toJSON (planDefinitionActionReason p)
    ,  "documentation" .= toJSON (planDefinitionActionDocumentation p)
    ,  "goalId" .= toJSON (planDefinitionActionGoalId p)
    , toSubjectJSON (planDefinitionActionSubject p)
    ,  "trigger" .= toJSON (planDefinitionActionTrigger p)
    ,  "condition" .= toJSON (planDefinitionActionCondition p)
    ,  "input" .= toJSON (planDefinitionActionInput p)
    ,  "output" .= toJSON (planDefinitionActionOutput p)
    ,  "relatedAction" .= toJSON (planDefinitionActionRelatedAction p)
    , toTimingJSON (planDefinitionActionTiming p)
    ,  "participant" .= toJSON (planDefinitionActionParticipant p)
    ,  "type" .= toJSON (planDefinitionActionType p)
    ,  "groupingBehavior" .= toJSON (planDefinitionActionGroupingBehavior p)
    ,  "selectionBehavior" .= toJSON (planDefinitionActionSelectionBehavior p)
    ,  "requiredBehavior" .= toJSON (planDefinitionActionRequiredBehavior p)
    ,  "precheckBehavior" .= toJSON (planDefinitionActionPrecheckBehavior p)
    ,  "cardinalityBehavior" .= toJSON (planDefinitionActionCardinalityBehavior p)
    , toDefinitionJSON (planDefinitionActionDefinition p)
    ,  "transform" .= toJSON (planDefinitionActionTransform p)
    ,  "dynamicValue" .= toJSON (planDefinitionActionDynamicValue p)
    ,  "action" .= toJSON (planDefinitionActionAction p)
    ]
    where 
      toSubjectJSON (     Nothing   ) = ("subject", Null)
      toSubjectJSON (Just (PlanDefinitionActionSubjectCodeableConcept c)) = ("subject", toJSON c)
      toSubjectJSON (Just (PlanDefinitionActionSubjectReference c)) = ("subject", toJSON c)
      toTimingJSON (     Nothing   ) = ("timing", Null)
      toTimingJSON (Just (PlanDefinitionActionTimingDateTime c)) = ("timing", toJSON c)
      toTimingJSON (Just (PlanDefinitionActionTimingAge c)) = ("timing", toJSON c)
      toTimingJSON (Just (PlanDefinitionActionTimingPeriod c)) = ("timing", toJSON c)
      toTimingJSON (Just (PlanDefinitionActionTimingDuration c)) = ("timing", toJSON c)
      toTimingJSON (Just (PlanDefinitionActionTimingRange c)) = ("timing", toJSON c)
      toTimingJSON (Just (PlanDefinitionActionTimingTiming c)) = ("timing", toJSON c)
      toDefinitionJSON (     Nothing   ) = ("definition", Null)
      toDefinitionJSON (Just (PlanDefinitionActionDefinitionCanonical c)) = ("definition", toJSON c)
      toDefinitionJSON (Just (PlanDefinitionActionDefinitionUri c)) = ("definition", toJSON c)
instance FromJSON PlanDefinitionAction where
  parseJSON = withObject "PlanDefinitionAction" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        prefix <- o .:? "prefix"
        title <- o .:? "title"
        description <- o .:? "description"
        textEquivalent <- o .:? "textEquivalent"
        priority <- o .:? "priority"
        code <- o .:? "code" .!= []
        reason <- o .:? "reason" .!= []
        documentation <- o .:? "documentation" .!= []
        goalId <- o .:? "goalId" .!= []
        subject <- parseSubject o
        trigger <- o .:? "trigger" .!= []
        condition <- o .:? "condition" .!= []
        input <- o .:? "input" .!= []
        output <- o .:? "output" .!= []
        relatedAction <- o .:? "relatedAction" .!= []
        timing <- parseTiming o
        participant <- o .:? "participant" .!= []
        ty <- o .:? "type"
        groupingBehavior <- o .:? "groupingBehavior"
        selectionBehavior <- o .:? "selectionBehavior"
        requiredBehavior <- o .:? "requiredBehavior"
        precheckBehavior <- o .:? "precheckBehavior"
        cardinalityBehavior <- o .:? "cardinalityBehavior"
        definition <- parseDefinition o
        transform <- o .:? "transform"
        dynamicValue <- o .:? "dynamicValue" .!= []
        action <- o .:? "action" .!= []
        return PlanDefinitionAction{
            planDefinitionActionAttrId = id
          , planDefinitionActionExtension = extension
          , planDefinitionActionModifierExtension = modifierExtension
          , planDefinitionActionPrefix = prefix
          , planDefinitionActionTitle = title
          , planDefinitionActionDescription = description
          , planDefinitionActionTextEquivalent = textEquivalent
          , planDefinitionActionPriority = priority
          , planDefinitionActionCode = code
          , planDefinitionActionReason = reason
          , planDefinitionActionDocumentation = documentation
          , planDefinitionActionGoalId = goalId
          , planDefinitionActionSubject = subject
          , planDefinitionActionTrigger = trigger
          , planDefinitionActionCondition = condition
          , planDefinitionActionInput = input
          , planDefinitionActionOutput = output
          , planDefinitionActionRelatedAction = relatedAction
          , planDefinitionActionTiming = timing
          , planDefinitionActionParticipant = participant
          , planDefinitionActionType = ty
          , planDefinitionActionGroupingBehavior = groupingBehavior
          , planDefinitionActionSelectionBehavior = selectionBehavior
          , planDefinitionActionRequiredBehavior = requiredBehavior
          , planDefinitionActionPrecheckBehavior = precheckBehavior
          , planDefinitionActionCardinalityBehavior = cardinalityBehavior
          , planDefinitionActionDefinition = definition
          , planDefinitionActionTransform = transform
          , planDefinitionActionDynamicValue = dynamicValue
          , planDefinitionActionAction = action
          }
    where 
      parseSubject o = parseSubjectCodeableConcept o <|> parseSubjectReference o
      parseSubjectCodeableConcept o = do
                has <- o .: "subjectCodeableConcept"
                return $ Just (PlanDefinitionActionSubjectCodeableConcept has)
      parseSubjectReference o = do
                has <- o .: "subjectReference"
                return $ Just (PlanDefinitionActionSubjectReference has)
      parseTiming o = parseTimingDateTime o <|> parseTimingAge o <|> parseTimingPeriod o <|> parseTimingDuration o <|> parseTimingRange o <|> parseTimingTiming o
      parseTimingDateTime o = do
                has <- o .: "timingDateTime"
                return $ Just (PlanDefinitionActionTimingDateTime has)
      parseTimingAge o = do
                has <- o .: "timingAge"
                return $ Just (PlanDefinitionActionTimingAge has)
      parseTimingPeriod o = do
                has <- o .: "timingPeriod"
                return $ Just (PlanDefinitionActionTimingPeriod has)
      parseTimingDuration o = do
                has <- o .: "timingDuration"
                return $ Just (PlanDefinitionActionTimingDuration has)
      parseTimingRange o = do
                has <- o .: "timingRange"
                return $ Just (PlanDefinitionActionTimingRange has)
      parseTimingTiming o = do
                has <- o .: "timingTiming"
                return $ Just (PlanDefinitionActionTimingTiming has)
      parseDefinition o = parseDefinitionCanonical o <|> parseDefinitionUri o
      parseDefinitionCanonical o = do
                has <- o .: "definitionCanonical"
                return $ Just (PlanDefinitionActionDefinitionCanonical has)
      parseDefinitionUri o = do
                has <- o .: "definitionUri"
                return $ Just (PlanDefinitionActionDefinitionUri has)
instance Xmlbf.ToXml PlanDefinitionAction where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (planDefinitionActionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (planDefinitionActionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (planDefinitionActionModifierExtension p))
             , OptVal   "prefix" (fmap toString (planDefinitionActionPrefix p))
             , OptVal   "title" (fmap toString (planDefinitionActionTitle p))
             , OptVal   "description" (fmap toString (planDefinitionActionDescription p))
             , OptVal   "textEquivalent" (fmap toString (planDefinitionActionTextEquivalent p))
             , OptVal   "priority" (fmap toPlanDefinitionActionPriority (planDefinitionActionPriority p))
             , PropList "code" (fmap Xmlbf.toXml (planDefinitionActionCode p))
             , PropList "reason" (fmap Xmlbf.toXml (planDefinitionActionReason p))
             , PropList "documentation" (fmap Xmlbf.toXml (planDefinitionActionDocumentation p))
             , ValList  "goalId" (fmap toId (planDefinitionActionGoalId p))
             , toSubjectXml (planDefinitionActionSubject p)
             , PropList "trigger" (fmap Xmlbf.toXml (planDefinitionActionTrigger p))
             , PropList "condition" (fmap Xmlbf.toXml (planDefinitionActionCondition p))
             , PropList "input" (fmap Xmlbf.toXml (planDefinitionActionInput p))
             , PropList "output" (fmap Xmlbf.toXml (planDefinitionActionOutput p))
             , PropList "relatedAction" (fmap Xmlbf.toXml (planDefinitionActionRelatedAction p))
             , toTimingXml (planDefinitionActionTiming p)
             , PropList "participant" (fmap Xmlbf.toXml (planDefinitionActionParticipant p))
             , OptProp  "type" (fmap Xmlbf.toXml (planDefinitionActionType p))
             , OptVal   "groupingBehavior" (fmap toPlanDefinitionActionGroupingBehavior (planDefinitionActionGroupingBehavior p))
             , OptVal   "selectionBehavior" (fmap toPlanDefinitionActionSelectionBehavior (planDefinitionActionSelectionBehavior p))
             , OptVal   "requiredBehavior" (fmap toPlanDefinitionActionRequiredBehavior (planDefinitionActionRequiredBehavior p))
             , OptVal   "precheckBehavior" (fmap toPlanDefinitionActionPrecheckBehavior (planDefinitionActionPrecheckBehavior p))
             , OptVal   "cardinalityBehavior" (fmap toPlanDefinitionActionCardinalityBehavior (planDefinitionActionCardinalityBehavior p))
             , toDefinitionXml (planDefinitionActionDefinition p)
             , OptVal   "transform" (fmap toCanonical (planDefinitionActionTransform p))
             , PropList "dynamicValue" (fmap Xmlbf.toXml (planDefinitionActionDynamicValue p))
             , PropList "action" (fmap Xmlbf.toXml (planDefinitionActionAction p))
             ]
       where 
          toSubjectXml ( Nothing   ) = (OptVal "subject" Nothing)
          toSubjectXml (Just (PlanDefinitionActionSubjectCodeableConcept p)) = Prop  "subjectCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toSubjectXml (Just (PlanDefinitionActionSubjectReference p)) = Prop  "subjectReference" (HM.empty, Xmlbf.toXml p)
          toTimingXml ( Nothing   ) = (OptVal "timing" Nothing)
          toTimingXml (Just (PlanDefinitionActionTimingDateTime p)) = Val   "timingDateTime" (toDateTime p)
          toTimingXml (Just (PlanDefinitionActionTimingAge p)) = Prop  "timingAge" (HM.empty, Xmlbf.toXml p)
          toTimingXml (Just (PlanDefinitionActionTimingPeriod p)) = Prop  "timingPeriod" (HM.empty, Xmlbf.toXml p)
          toTimingXml (Just (PlanDefinitionActionTimingDuration p)) = Prop  "timingDuration" (HM.empty, Xmlbf.toXml p)
          toTimingXml (Just (PlanDefinitionActionTimingRange p)) = Prop  "timingRange" (HM.empty, Xmlbf.toXml p)
          toTimingXml (Just (PlanDefinitionActionTimingTiming p)) = Prop  "timingTiming" (HM.empty, Xmlbf.toXml p)
          toDefinitionXml ( Nothing   ) = (OptVal "definition" Nothing)
          toDefinitionXml (Just (PlanDefinitionActionDefinitionCanonical p)) = Val   "definitionCanonical" (toCanonical p)
          toDefinitionXml (Just (PlanDefinitionActionDefinitionUri p)) = Val   "definitionUri" (toUri p)
instance Xmlbf.FromXml PlanDefinitionAction where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    prefix <- optional $ Xmlbf.pElement "prefix" (Xmlbf.pAttr "value")
    title <- optional $ Xmlbf.pElement "title" (Xmlbf.pAttr "value")
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    textEquivalent <- optional $ Xmlbf.pElement "textEquivalent" (Xmlbf.pAttr "value")
    priority <- optional $ Xmlbf.pElement "priority" (Xmlbf.pAttr "value")
    code <- many     $ Xmlbf.pElement "code" Xmlbf.fromXml
    reason <- many     $ Xmlbf.pElement "reason" Xmlbf.fromXml
    documentation <- many     $ Xmlbf.pElement "documentation" Xmlbf.fromXml
    goalId <- many     $ Xmlbf.pElement "goalId" (Xmlbf.pAttr "value")
    subject <- fromSubjectXml
    trigger <- many     $ Xmlbf.pElement "trigger" Xmlbf.fromXml
    condition <- many     $ Xmlbf.pElement "condition" Xmlbf.fromXml
    input <- many     $ Xmlbf.pElement "input" Xmlbf.fromXml
    output <- many     $ Xmlbf.pElement "output" Xmlbf.fromXml
    relatedAction <- many     $ Xmlbf.pElement "relatedAction" Xmlbf.fromXml
    timing <- fromTimingXml
    participant <- many     $ Xmlbf.pElement "participant" Xmlbf.fromXml
    ty <- optional $ Xmlbf.pElement "type" Xmlbf.fromXml
    groupingBehavior <- optional $ Xmlbf.pElement "groupingBehavior" (Xmlbf.pAttr "value")
    selectionBehavior <- optional $ Xmlbf.pElement "selectionBehavior" (Xmlbf.pAttr "value")
    requiredBehavior <- optional $ Xmlbf.pElement "requiredBehavior" (Xmlbf.pAttr "value")
    precheckBehavior <- optional $ Xmlbf.pElement "precheckBehavior" (Xmlbf.pAttr "value")
    cardinalityBehavior <- optional $ Xmlbf.pElement "cardinalityBehavior" (Xmlbf.pAttr "value")
    definition <- fromDefinitionXml
    transform <- optional $ Xmlbf.pElement "transform" (Xmlbf.pAttr "value")
    dynamicValue <- many     $ Xmlbf.pElement "dynamicValue" Xmlbf.fromXml
    action <- many     $ Xmlbf.pElement "action" Xmlbf.fromXml
    return PlanDefinitionAction {
            planDefinitionActionAttrId = id
          , planDefinitionActionExtension = extension
          , planDefinitionActionModifierExtension = modifierExtension
          , planDefinitionActionPrefix = fmap fromString prefix
          , planDefinitionActionTitle = fmap fromString title
          , planDefinitionActionDescription = fmap fromString description
          , planDefinitionActionTextEquivalent = fmap fromString textEquivalent
          , planDefinitionActionPriority = fmap fromPlanDefinitionActionPriority priority
          , planDefinitionActionCode = code
          , planDefinitionActionReason = reason
          , planDefinitionActionDocumentation = documentation
          , planDefinitionActionGoalId = fmap fromId goalId
          , planDefinitionActionSubject = subject
          , planDefinitionActionTrigger = trigger
          , planDefinitionActionCondition = condition
          , planDefinitionActionInput = input
          , planDefinitionActionOutput = output
          , planDefinitionActionRelatedAction = relatedAction
          , planDefinitionActionTiming = timing
          , planDefinitionActionParticipant = participant
          , planDefinitionActionType = ty
          , planDefinitionActionGroupingBehavior = fmap fromPlanDefinitionActionGroupingBehavior groupingBehavior
          , planDefinitionActionSelectionBehavior = fmap fromPlanDefinitionActionSelectionBehavior selectionBehavior
          , planDefinitionActionRequiredBehavior = fmap fromPlanDefinitionActionRequiredBehavior requiredBehavior
          , planDefinitionActionPrecheckBehavior = fmap fromPlanDefinitionActionPrecheckBehavior precheckBehavior
          , planDefinitionActionCardinalityBehavior = fmap fromPlanDefinitionActionCardinalityBehavior cardinalityBehavior
          , planDefinitionActionDefinition = definition
          , planDefinitionActionTransform = fmap fromCanonical transform
          , planDefinitionActionDynamicValue = dynamicValue
          , planDefinitionActionAction = action
          }

    where 
      fromSubjectXml = parseSubjectCodeableConcept <|> parseSubjectReference <|> pure Nothing
      parseSubjectCodeableConcept = do
                has <- Xmlbf.pElement "subjectCodeableConcept" Xmlbf.fromXml
                return $ Just (PlanDefinitionActionSubjectCodeableConcept (                      has))
      parseSubjectReference = do
                has <- Xmlbf.pElement "subjectReference" Xmlbf.fromXml
                return $ Just (PlanDefinitionActionSubjectReference (                      has))
      fromTimingXml = parseTimingDateTime <|> parseTimingAge <|> parseTimingPeriod <|> parseTimingDuration <|> parseTimingRange <|> parseTimingTiming <|> pure Nothing
      parseTimingDateTime = do
                has <- Xmlbf.pElement "timingDateTime" (Xmlbf.pAttr "value")
                return $ Just (PlanDefinitionActionTimingDateTime (     toDateTime has))
      parseTimingAge = do
                has <- Xmlbf.pElement "timingAge" Xmlbf.fromXml
                return $ Just (PlanDefinitionActionTimingAge (                      has))
      parseTimingPeriod = do
                has <- Xmlbf.pElement "timingPeriod" Xmlbf.fromXml
                return $ Just (PlanDefinitionActionTimingPeriod (                      has))
      parseTimingDuration = do
                has <- Xmlbf.pElement "timingDuration" Xmlbf.fromXml
                return $ Just (PlanDefinitionActionTimingDuration (                      has))
      parseTimingRange = do
                has <- Xmlbf.pElement "timingRange" Xmlbf.fromXml
                return $ Just (PlanDefinitionActionTimingRange (                      has))
      parseTimingTiming = do
                has <- Xmlbf.pElement "timingTiming" Xmlbf.fromXml
                return $ Just (PlanDefinitionActionTimingTiming (                      has))
      fromDefinitionXml = parseDefinitionCanonical <|> parseDefinitionUri <|> pure Nothing
      parseDefinitionCanonical = do
                has <- Xmlbf.pElement "definitionCanonical" (Xmlbf.pAttr "value")
                return $ Just (PlanDefinitionActionDefinitionCanonical (     toCanonical has))
      parseDefinitionUri = do
                has <- Xmlbf.pElement "definitionUri" (Xmlbf.pAttr "value")
                return $ Just (PlanDefinitionActionDefinitionUri (     toUri has))



