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
-- FHIR 4.0.0 Questionnaire
--

module Data.FHIR.Resources.Questionnaire where

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

data Questionnaire = Questionnaire {
    questionnaireId :: Maybe Id
  , questionnaireMeta :: Maybe Meta
  , questionnaireImplicitRules :: Maybe Uri
  , questionnaireLanguage :: Maybe Language
  , questionnaireText :: Maybe Narrative
--    questionnaireContained :: [ResourceContainer]
  , questionnaireExtension :: [Extension]
  , questionnaireModifierExtension :: [Extension]
  , questionnaireUrl :: Maybe Uri
  , questionnaireIdentifier :: [Identifier]
  , questionnaireVersion :: Maybe Text
  , questionnaireName :: Maybe Text
  , questionnaireTitle :: Maybe Text
  , questionnaireDerivedFrom :: [Canonical]
  , questionnaireStatus :: PublicationStatus
  , questionnaireExperimental :: Maybe Boolean
  , questionnaireSubjectType :: [Code]
  , questionnaireDate :: Maybe DateTime
  , questionnairePublisher :: Maybe Text
  , questionnaireContact :: [ContactDetail]
  , questionnaireDescription :: Maybe Markdown
  , questionnaireUseContext :: [UsageContext]
  , questionnaireJurisdiction :: [CodeableConcept]
  , questionnairePurpose :: Maybe Markdown
  , questionnaireCopyright :: Maybe Markdown
  , questionnaireApprovalDate :: Maybe Date
  , questionnaireLastReviewDate :: Maybe Date
  , questionnaireEffectivePeriod :: Maybe Period
  , questionnaireCode :: [Coding]
  , questionnaireItem :: [QuestionnaireItem]
  } deriving (Eq, Show)
--

instance ToJSON Questionnaire where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Questionnaire")
    ,  "id" .= toJSON (questionnaireId p)
    ,  "meta" .= toJSON (questionnaireMeta p)
    ,  "implicitRules" .= toJSON (questionnaireImplicitRules p)
    ,  "language" .= toJSON (questionnaireLanguage p)
    ,  "text" .= toJSON (questionnaireText p)
--    , "contained" .= toJSON (questionnaireContained p)
    ,  "extension" .= toJSON (questionnaireExtension p)
    ,  "modifierExtension" .= toJSON (questionnaireModifierExtension p)
    ,  "url" .= toJSON (questionnaireUrl p)
    ,  "identifier" .= toJSON (questionnaireIdentifier p)
    ,  "version" .= toJSON (questionnaireVersion p)
    ,  "name" .= toJSON (questionnaireName p)
    ,  "title" .= toJSON (questionnaireTitle p)
    ,  "derivedFrom" .= toJSON (questionnaireDerivedFrom p)
    ,  "status" .= toJSON (questionnaireStatus p)
    ,  "experimental" .= toJSON (questionnaireExperimental p)
    ,  "subjectType" .= toJSON (questionnaireSubjectType p)
    ,  "date" .= toJSON (questionnaireDate p)
    ,  "publisher" .= toJSON (questionnairePublisher p)
    ,  "contact" .= toJSON (questionnaireContact p)
    ,  "description" .= toJSON (questionnaireDescription p)
    ,  "useContext" .= toJSON (questionnaireUseContext p)
    ,  "jurisdiction" .= toJSON (questionnaireJurisdiction p)
    ,  "purpose" .= toJSON (questionnairePurpose p)
    ,  "copyright" .= toJSON (questionnaireCopyright p)
    ,  "approvalDate" .= toJSON (questionnaireApprovalDate p)
    ,  "lastReviewDate" .= toJSON (questionnaireLastReviewDate p)
    ,  "effectivePeriod" .= toJSON (questionnaireEffectivePeriod p)
    ,  "code" .= toJSON (questionnaireCode p)
    ,  "item" .= toJSON (questionnaireItem p)
    ]
instance FromJSON Questionnaire where
  parseJSON = withObject "Questionnaire" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Questionnaire" -> do
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
        derivedFrom <- o .:? "derivedFrom" .!= []
        status <- o .:  "status"
        experimental <- o .:? "experimental"
        subjectType <- o .:? "subjectType" .!= []
        date <- o .:? "date"
        publisher <- o .:? "publisher"
        contact <- o .:? "contact" .!= []
        description <- o .:? "description"
        useContext <- o .:? "useContext" .!= []
        jurisdiction <- o .:? "jurisdiction" .!= []
        purpose <- o .:? "purpose"
        copyright <- o .:? "copyright"
        approvalDate <- o .:? "approvalDate"
        lastReviewDate <- o .:? "lastReviewDate"
        effectivePeriod <- o .:? "effectivePeriod"
        code <- o .:? "code" .!= []
        item <- o .:? "item" .!= []
        return Questionnaire{
            questionnaireId = id
          , questionnaireMeta = meta
          , questionnaireImplicitRules = implicitRules
          , questionnaireLanguage = language
          , questionnaireText = text
--          , questionnaireContained = contained
          , questionnaireExtension = extension
          , questionnaireModifierExtension = modifierExtension
          , questionnaireUrl = url
          , questionnaireIdentifier = identifier
          , questionnaireVersion = version
          , questionnaireName = name
          , questionnaireTitle = title
          , questionnaireDerivedFrom = derivedFrom
          , questionnaireStatus = status
          , questionnaireExperimental = experimental
          , questionnaireSubjectType = subjectType
          , questionnaireDate = date
          , questionnairePublisher = publisher
          , questionnaireContact = contact
          , questionnaireDescription = description
          , questionnaireUseContext = useContext
          , questionnaireJurisdiction = jurisdiction
          , questionnairePurpose = purpose
          , questionnaireCopyright = copyright
          , questionnaireApprovalDate = approvalDate
          , questionnaireLastReviewDate = lastReviewDate
          , questionnaireEffectivePeriod = effectivePeriod
          , questionnaireCode = code
          , questionnaireItem = item
          }
      _ -> fail "not a Questionnaire"
instance Xmlbf.ToXml Questionnaire where
  toXml p = Xmlbf.element "Questionnaire" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (questionnaireId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (questionnaireMeta p))
             , OptVal   "implicitRules" (fmap toUri (questionnaireImplicitRules p))
             , OptVal   "language" (fmap toLanguage (questionnaireLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (questionnaireText p))
--             , PropList "contained" (fmap Xmlbf.toXml (questionnaireContained p))
             , PropList "extension" (fmap Xmlbf.toXml (questionnaireExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (questionnaireModifierExtension p))
             , OptVal   "url" (fmap toUri (questionnaireUrl p))
             , PropList "identifier" (fmap Xmlbf.toXml (questionnaireIdentifier p))
             , OptVal   "version" (fmap toString (questionnaireVersion p))
             , OptVal   "name" (fmap toString (questionnaireName p))
             , OptVal   "title" (fmap toString (questionnaireTitle p))
             , ValList  "derivedFrom" (fmap toCanonical (questionnaireDerivedFrom p))
             , Val      "status" (     toPublicationStatus (questionnaireStatus p))
             , OptVal   "experimental" (fmap toBoolean (questionnaireExperimental p))
             , ValList  "subjectType" (fmap toCode (questionnaireSubjectType p))
             , OptVal   "date" (fmap toDateTime (questionnaireDate p))
             , OptVal   "publisher" (fmap toString (questionnairePublisher p))
             , PropList "contact" (fmap Xmlbf.toXml (questionnaireContact p))
             , OptVal   "description" (fmap toMarkdown (questionnaireDescription p))
             , PropList "useContext" (fmap Xmlbf.toXml (questionnaireUseContext p))
             , PropList "jurisdiction" (fmap Xmlbf.toXml (questionnaireJurisdiction p))
             , OptVal   "purpose" (fmap toMarkdown (questionnairePurpose p))
             , OptVal   "copyright" (fmap toMarkdown (questionnaireCopyright p))
             , OptVal   "approvalDate" (fmap toDate (questionnaireApprovalDate p))
             , OptVal   "lastReviewDate" (fmap toDate (questionnaireLastReviewDate p))
             , OptProp  "effectivePeriod" (fmap Xmlbf.toXml (questionnaireEffectivePeriod p))
             , PropList "code" (fmap Xmlbf.toXml (questionnaireCode p))
             , PropList "item" (fmap Xmlbf.toXml (questionnaireItem p))
             ]
instance Xmlbf.FromXml Questionnaire where
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
    derivedFrom <- many     $ Xmlbf.pElement "derivedFrom" (Xmlbf.pAttr "value")
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    experimental <- optional $ Xmlbf.pElement "experimental" (Xmlbf.pAttr "value")
    subjectType <- many     $ Xmlbf.pElement "subjectType" (Xmlbf.pAttr "value")
    date <- optional $ Xmlbf.pElement "date" (Xmlbf.pAttr "value")
    publisher <- optional $ Xmlbf.pElement "publisher" (Xmlbf.pAttr "value")
    contact <- many     $ Xmlbf.pElement "contact" Xmlbf.fromXml
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    useContext <- many     $ Xmlbf.pElement "useContext" Xmlbf.fromXml
    jurisdiction <- many     $ Xmlbf.pElement "jurisdiction" Xmlbf.fromXml
    purpose <- optional $ Xmlbf.pElement "purpose" (Xmlbf.pAttr "value")
    copyright <- optional $ Xmlbf.pElement "copyright" (Xmlbf.pAttr "value")
    approvalDate <- optional $ Xmlbf.pElement "approvalDate" (Xmlbf.pAttr "value")
    lastReviewDate <- optional $ Xmlbf.pElement "lastReviewDate" (Xmlbf.pAttr "value")
    effectivePeriod <- optional $ Xmlbf.pElement "effectivePeriod" Xmlbf.fromXml
    code <- many     $ Xmlbf.pElement "code" Xmlbf.fromXml
    item <- many     $ Xmlbf.pElement "item" Xmlbf.fromXml
    return Questionnaire {
            questionnaireId = fmap fromId id
          , questionnaireMeta = meta
          , questionnaireImplicitRules = fmap fromUri implicitRules
          , questionnaireLanguage = fmap fromLanguage language
          , questionnaireText = text
--          , questionnaireContained = contained
          , questionnaireExtension = extension
          , questionnaireModifierExtension = modifierExtension
          , questionnaireUrl = fmap fromUri url
          , questionnaireIdentifier = identifier
          , questionnaireVersion = fmap fromString version
          , questionnaireName = fmap fromString name
          , questionnaireTitle = fmap fromString title
          , questionnaireDerivedFrom = fmap fromCanonical derivedFrom
          , questionnaireStatus =      fromPublicationStatus status
          , questionnaireExperimental = fmap fromBoolean experimental
          , questionnaireSubjectType = fmap fromCode subjectType
          , questionnaireDate = fmap fromDateTime date
          , questionnairePublisher = fmap fromString publisher
          , questionnaireContact = contact
          , questionnaireDescription = fmap fromMarkdown description
          , questionnaireUseContext = useContext
          , questionnaireJurisdiction = jurisdiction
          , questionnairePurpose = fmap fromMarkdown purpose
          , questionnaireCopyright = fmap fromMarkdown copyright
          , questionnaireApprovalDate = fmap fromDate approvalDate
          , questionnaireLastReviewDate = fmap fromDate lastReviewDate
          , questionnaireEffectivePeriod = effectivePeriod
          , questionnaireCode = code
          , questionnaireItem = item
          }




data QuestionnaireEnableWhenOperator
    = QEWOExists
    | QEWOEq
    | QEWONotEq
    | QEWOGe
    | QEWOLe
    | QEWOGT
    | QEWOLT
  deriving (Eq, Show)

instance ToJSON QuestionnaireEnableWhenOperator where
    toJSON QEWOExists = String "exists"
    toJSON QEWOEq = String "="
    toJSON QEWONotEq = String "!="
    toJSON QEWOGe = String ">"
    toJSON QEWOLe = String "<"
    toJSON QEWOGT = String ">="
    toJSON QEWOLT = String "<="
instance FromJSON QuestionnaireEnableWhenOperator where
    parseJSON "exists" = return QEWOExists
    parseJSON "=" = return QEWOEq
    parseJSON "!=" = return QEWONotEq
    parseJSON ">" = return QEWOGe
    parseJSON "<" = return QEWOLe
    parseJSON ">=" = return QEWOGT
    parseJSON "<=" = return QEWOLT

toQuestionnaireEnableWhenOperator QEWOExists = "exists"
toQuestionnaireEnableWhenOperator QEWOEq = "="
toQuestionnaireEnableWhenOperator QEWONotEq = "!="
toQuestionnaireEnableWhenOperator QEWOGe = ">"
toQuestionnaireEnableWhenOperator QEWOLe = "<"
toQuestionnaireEnableWhenOperator QEWOGT = ">="
toQuestionnaireEnableWhenOperator QEWOLT = "<="
fromQuestionnaireEnableWhenOperator "exists" = QEWOExists
fromQuestionnaireEnableWhenOperator "=" = QEWOEq
fromQuestionnaireEnableWhenOperator "!=" = QEWONotEq
fromQuestionnaireEnableWhenOperator ">" = QEWOGe
fromQuestionnaireEnableWhenOperator "<" = QEWOLe
fromQuestionnaireEnableWhenOperator ">=" = QEWOGT
fromQuestionnaireEnableWhenOperator "<=" = QEWOLT


data QuestionnaireEnableWhenAnswer
    = QuestionnaireEnableWhenAnswerBoolean Boolean
    | QuestionnaireEnableWhenAnswerDecimal Decimal
    | QuestionnaireEnableWhenAnswerInteger Integer
    | QuestionnaireEnableWhenAnswerDate Date
    | QuestionnaireEnableWhenAnswerDateTime DateTime
    | QuestionnaireEnableWhenAnswerTime Time
    | QuestionnaireEnableWhenAnswerString Text
    | QuestionnaireEnableWhenAnswerCoding Coding
    | QuestionnaireEnableWhenAnswerQuantity Quantity
    | QuestionnaireEnableWhenAnswerReference Reference
    deriving (Eq, Show)

data QuestionnaireEnableWhen = QuestionnaireEnableWhen {
    questionnaireEnableWhenAttrId :: Maybe Text
  , questionnaireEnableWhenExtension :: [Extension]
  , questionnaireEnableWhenModifierExtension :: [Extension]
  , questionnaireEnableWhenQuestion :: Text
  , questionnaireEnableWhenOperator :: QuestionnaireEnableWhenOperator
  , questionnaireEnableWhenAnswer :: QuestionnaireEnableWhenAnswer
  } deriving (Eq, Show)
--

instance ToJSON QuestionnaireEnableWhen where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (questionnaireEnableWhenAttrId p)
    ,  "extension" .= toJSON (questionnaireEnableWhenExtension p)
    ,  "modifierExtension" .= toJSON (questionnaireEnableWhenModifierExtension p)
    ,  "question" .= toJSON (questionnaireEnableWhenQuestion p)
    ,  "operator" .= toJSON (questionnaireEnableWhenOperator p)
    , toAnswerJSON (questionnaireEnableWhenAnswer p)
    ]
    where 
      toAnswerJSON (     (QuestionnaireEnableWhenAnswerBoolean c)) = ("answerBoolean", toJSON c)
      toAnswerJSON (     (QuestionnaireEnableWhenAnswerDecimal c)) = ("answerDecimal", toJSON c)
      toAnswerJSON (     (QuestionnaireEnableWhenAnswerInteger c)) = ("answerInteger", toJSON c)
      toAnswerJSON (     (QuestionnaireEnableWhenAnswerDate c)) = ("answerDate", toJSON c)
      toAnswerJSON (     (QuestionnaireEnableWhenAnswerDateTime c)) = ("answerDateTime", toJSON c)
      toAnswerJSON (     (QuestionnaireEnableWhenAnswerTime c)) = ("answerTime", toJSON c)
      toAnswerJSON (     (QuestionnaireEnableWhenAnswerString c)) = ("answerString", toJSON c)
      toAnswerJSON (     (QuestionnaireEnableWhenAnswerCoding c)) = ("answerCoding", toJSON c)
      toAnswerJSON (     (QuestionnaireEnableWhenAnswerQuantity c)) = ("answerQuantity", toJSON c)
      toAnswerJSON (     (QuestionnaireEnableWhenAnswerReference c)) = ("answerReference", toJSON c)
instance FromJSON QuestionnaireEnableWhen where
  parseJSON = withObject "QuestionnaireEnableWhen" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        question <- o .:  "question"
        operator <- o .:  "operator"
        answer <- parseAnswer o
        return QuestionnaireEnableWhen{
            questionnaireEnableWhenAttrId = id
          , questionnaireEnableWhenExtension = extension
          , questionnaireEnableWhenModifierExtension = modifierExtension
          , questionnaireEnableWhenQuestion = question
          , questionnaireEnableWhenOperator = operator
          , questionnaireEnableWhenAnswer = answer
          }
    where 
      parseAnswer o = parseAnswerBoolean o <|> parseAnswerDecimal o <|> parseAnswerInteger o <|> parseAnswerDate o <|> parseAnswerDateTime o <|> parseAnswerTime o <|> parseAnswerString o <|> parseAnswerCoding o <|> parseAnswerQuantity o <|> parseAnswerReference o
      parseAnswerBoolean o = do
                has <- o .: "answerBoolean"
                return $ QuestionnaireEnableWhenAnswerBoolean has
      parseAnswerDecimal o = do
                has <- o .: "answerDecimal"
                return $ QuestionnaireEnableWhenAnswerDecimal has
      parseAnswerInteger o = do
                has <- o .: "answerInteger"
                return $ QuestionnaireEnableWhenAnswerInteger has
      parseAnswerDate o = do
                has <- o .: "answerDate"
                return $ QuestionnaireEnableWhenAnswerDate has
      parseAnswerDateTime o = do
                has <- o .: "answerDateTime"
                return $ QuestionnaireEnableWhenAnswerDateTime has
      parseAnswerTime o = do
                has <- o .: "answerTime"
                return $ QuestionnaireEnableWhenAnswerTime has
      parseAnswerString o = do
                has <- o .: "answerString"
                return $ QuestionnaireEnableWhenAnswerString has
      parseAnswerCoding o = do
                has <- o .: "answerCoding"
                return $ QuestionnaireEnableWhenAnswerCoding has
      parseAnswerQuantity o = do
                has <- o .: "answerQuantity"
                return $ QuestionnaireEnableWhenAnswerQuantity has
      parseAnswerReference o = do
                has <- o .: "answerReference"
                return $ QuestionnaireEnableWhenAnswerReference has
instance Xmlbf.ToXml QuestionnaireEnableWhen where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (questionnaireEnableWhenAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (questionnaireEnableWhenExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (questionnaireEnableWhenModifierExtension p))
             , Val      "question" (     toString (questionnaireEnableWhenQuestion p))
             , Val      "operator" (     toQuestionnaireEnableWhenOperator (questionnaireEnableWhenOperator p))
             , toAnswerXml (questionnaireEnableWhenAnswer p)
             ]
       where 
          toAnswerXml (     (QuestionnaireEnableWhenAnswerBoolean p)) = Val      "answerBoolean" (     toBoolean p)
          toAnswerXml (     (QuestionnaireEnableWhenAnswerDecimal p)) = Val      "answerDecimal" (     toDecimal p)
          toAnswerXml (     (QuestionnaireEnableWhenAnswerInteger p)) = Val      "answerInteger" (     toInt p)
          toAnswerXml (     (QuestionnaireEnableWhenAnswerDate p)) = Val      "answerDate" (     toDate p)
          toAnswerXml (     (QuestionnaireEnableWhenAnswerDateTime p)) = Val      "answerDateTime" (     toDateTime p)
          toAnswerXml (     (QuestionnaireEnableWhenAnswerTime p)) = Val      "answerTime" (     toTime p)
          toAnswerXml (     (QuestionnaireEnableWhenAnswerString p)) = Val      "answerString" (     toString p)
          toAnswerXml (     (QuestionnaireEnableWhenAnswerCoding p)) = Prop     "answerCoding" (HM.empty, Xmlbf.toXml p)
          toAnswerXml (     (QuestionnaireEnableWhenAnswerQuantity p)) = Prop     "answerQuantity" (HM.empty, Xmlbf.toXml p)
          toAnswerXml (     (QuestionnaireEnableWhenAnswerReference p)) = Prop     "answerReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml QuestionnaireEnableWhen where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    question <-            Xmlbf.pElement "question" (Xmlbf.pAttr "value")
    operator <-            Xmlbf.pElement "operator" (Xmlbf.pAttr "value")
    answer <- fromAnswerXml
    return QuestionnaireEnableWhen {
            questionnaireEnableWhenAttrId = id
          , questionnaireEnableWhenExtension = extension
          , questionnaireEnableWhenModifierExtension = modifierExtension
          , questionnaireEnableWhenQuestion =      fromString question
          , questionnaireEnableWhenOperator =      fromQuestionnaireEnableWhenOperator operator
          , questionnaireEnableWhenAnswer = answer
          }

    where 
      fromAnswerXml = parseAnswerBoolean <|> parseAnswerDecimal <|> parseAnswerInteger <|> parseAnswerDate <|> parseAnswerDateTime <|> parseAnswerTime <|> parseAnswerString <|> parseAnswerCoding <|> parseAnswerQuantity <|> parseAnswerReference
      parseAnswerBoolean = do
                has <- Xmlbf.pElement "answerBoolean" (Xmlbf.pAttr "value")
                return $ QuestionnaireEnableWhenAnswerBoolean (     fromBoolean has)
      parseAnswerDecimal = do
                has <- Xmlbf.pElement "answerDecimal" (Xmlbf.pAttr "value")
                return $ QuestionnaireEnableWhenAnswerDecimal (     fromDecimal has)
      parseAnswerInteger = do
                has <- Xmlbf.pElement "answerInteger" (Xmlbf.pAttr "value")
                return $ QuestionnaireEnableWhenAnswerInteger (     fromInt has)
      parseAnswerDate = do
                has <- Xmlbf.pElement "answerDate" (Xmlbf.pAttr "value")
                return $ QuestionnaireEnableWhenAnswerDate (     fromDate has)
      parseAnswerDateTime = do
                has <- Xmlbf.pElement "answerDateTime" (Xmlbf.pAttr "value")
                return $ QuestionnaireEnableWhenAnswerDateTime (     fromDateTime has)
      parseAnswerTime = do
                has <- Xmlbf.pElement "answerTime" (Xmlbf.pAttr "value")
                return $ QuestionnaireEnableWhenAnswerTime (     fromTime has)
      parseAnswerString = do
                has <- Xmlbf.pElement "answerString" (Xmlbf.pAttr "value")
                return $ QuestionnaireEnableWhenAnswerString (     fromString has)
      parseAnswerCoding = do
                has <- Xmlbf.pElement "answerCoding" Xmlbf.fromXml
                return $ QuestionnaireEnableWhenAnswerCoding (                      has)
      parseAnswerQuantity = do
                has <- Xmlbf.pElement "answerQuantity" Xmlbf.fromXml
                return $ QuestionnaireEnableWhenAnswerQuantity (                      has)
      parseAnswerReference = do
                has <- Xmlbf.pElement "answerReference" Xmlbf.fromXml
                return $ QuestionnaireEnableWhenAnswerReference (                      has)


data QuestionnaireInitialValue
    = QuestionnaireInitialValueBoolean Boolean
    | QuestionnaireInitialValueDecimal Decimal
    | QuestionnaireInitialValueInteger Integer
    | QuestionnaireInitialValueDate Date
    | QuestionnaireInitialValueDateTime DateTime
    | QuestionnaireInitialValueTime Time
    | QuestionnaireInitialValueString Text
    | QuestionnaireInitialValueUri Uri
    | QuestionnaireInitialValueAttachment Attachment
    | QuestionnaireInitialValueCoding Coding
    | QuestionnaireInitialValueQuantity Quantity
    | QuestionnaireInitialValueReference Reference
    deriving (Eq, Show)

data QuestionnaireInitial = QuestionnaireInitial {
    questionnaireInitialAttrId :: Maybe Text
  , questionnaireInitialExtension :: [Extension]
  , questionnaireInitialModifierExtension :: [Extension]
  , questionnaireInitialValue :: QuestionnaireInitialValue
  } deriving (Eq, Show)
--

instance ToJSON QuestionnaireInitial where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (questionnaireInitialAttrId p)
    ,  "extension" .= toJSON (questionnaireInitialExtension p)
    ,  "modifierExtension" .= toJSON (questionnaireInitialModifierExtension p)
    , toValueJSON (questionnaireInitialValue p)
    ]
    where 
      toValueJSON (     (QuestionnaireInitialValueBoolean c)) = ("valueBoolean", toJSON c)
      toValueJSON (     (QuestionnaireInitialValueDecimal c)) = ("valueDecimal", toJSON c)
      toValueJSON (     (QuestionnaireInitialValueInteger c)) = ("valueInteger", toJSON c)
      toValueJSON (     (QuestionnaireInitialValueDate c)) = ("valueDate", toJSON c)
      toValueJSON (     (QuestionnaireInitialValueDateTime c)) = ("valueDateTime", toJSON c)
      toValueJSON (     (QuestionnaireInitialValueTime c)) = ("valueTime", toJSON c)
      toValueJSON (     (QuestionnaireInitialValueString c)) = ("valueString", toJSON c)
      toValueJSON (     (QuestionnaireInitialValueUri c)) = ("valueUri", toJSON c)
      toValueJSON (     (QuestionnaireInitialValueAttachment c)) = ("valueAttachment", toJSON c)
      toValueJSON (     (QuestionnaireInitialValueCoding c)) = ("valueCoding", toJSON c)
      toValueJSON (     (QuestionnaireInitialValueQuantity c)) = ("valueQuantity", toJSON c)
      toValueJSON (     (QuestionnaireInitialValueReference c)) = ("valueReference", toJSON c)
instance FromJSON QuestionnaireInitial where
  parseJSON = withObject "QuestionnaireInitial" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        value <- parseValue o
        return QuestionnaireInitial{
            questionnaireInitialAttrId = id
          , questionnaireInitialExtension = extension
          , questionnaireInitialModifierExtension = modifierExtension
          , questionnaireInitialValue = value
          }
    where 
      parseValue o = parseValueBoolean o <|> parseValueDecimal o <|> parseValueInteger o <|> parseValueDate o <|> parseValueDateTime o <|> parseValueTime o <|> parseValueString o <|> parseValueUri o <|> parseValueAttachment o <|> parseValueCoding o <|> parseValueQuantity o <|> parseValueReference o
      parseValueBoolean o = do
                has <- o .: "valueBoolean"
                return $ QuestionnaireInitialValueBoolean has
      parseValueDecimal o = do
                has <- o .: "valueDecimal"
                return $ QuestionnaireInitialValueDecimal has
      parseValueInteger o = do
                has <- o .: "valueInteger"
                return $ QuestionnaireInitialValueInteger has
      parseValueDate o = do
                has <- o .: "valueDate"
                return $ QuestionnaireInitialValueDate has
      parseValueDateTime o = do
                has <- o .: "valueDateTime"
                return $ QuestionnaireInitialValueDateTime has
      parseValueTime o = do
                has <- o .: "valueTime"
                return $ QuestionnaireInitialValueTime has
      parseValueString o = do
                has <- o .: "valueString"
                return $ QuestionnaireInitialValueString has
      parseValueUri o = do
                has <- o .: "valueUri"
                return $ QuestionnaireInitialValueUri has
      parseValueAttachment o = do
                has <- o .: "valueAttachment"
                return $ QuestionnaireInitialValueAttachment has
      parseValueCoding o = do
                has <- o .: "valueCoding"
                return $ QuestionnaireInitialValueCoding has
      parseValueQuantity o = do
                has <- o .: "valueQuantity"
                return $ QuestionnaireInitialValueQuantity has
      parseValueReference o = do
                has <- o .: "valueReference"
                return $ QuestionnaireInitialValueReference has
instance Xmlbf.ToXml QuestionnaireInitial where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (questionnaireInitialAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (questionnaireInitialExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (questionnaireInitialModifierExtension p))
             , toValueXml (questionnaireInitialValue p)
             ]
       where 
          toValueXml (     (QuestionnaireInitialValueBoolean p)) = Val      "valueBoolean" (     toBoolean p)
          toValueXml (     (QuestionnaireInitialValueDecimal p)) = Val      "valueDecimal" (     toDecimal p)
          toValueXml (     (QuestionnaireInitialValueInteger p)) = Val      "valueInteger" (     toInt p)
          toValueXml (     (QuestionnaireInitialValueDate p)) = Val      "valueDate" (     toDate p)
          toValueXml (     (QuestionnaireInitialValueDateTime p)) = Val      "valueDateTime" (     toDateTime p)
          toValueXml (     (QuestionnaireInitialValueTime p)) = Val      "valueTime" (     toTime p)
          toValueXml (     (QuestionnaireInitialValueString p)) = Val      "valueString" (     toString p)
          toValueXml (     (QuestionnaireInitialValueUri p)) = Val      "valueUri" (     toUri p)
          toValueXml (     (QuestionnaireInitialValueAttachment p)) = Prop     "valueAttachment" (HM.empty, Xmlbf.toXml p)
          toValueXml (     (QuestionnaireInitialValueCoding p)) = Prop     "valueCoding" (HM.empty, Xmlbf.toXml p)
          toValueXml (     (QuestionnaireInitialValueQuantity p)) = Prop     "valueQuantity" (HM.empty, Xmlbf.toXml p)
          toValueXml (     (QuestionnaireInitialValueReference p)) = Prop     "valueReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml QuestionnaireInitial where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    value <- fromValueXml
    return QuestionnaireInitial {
            questionnaireInitialAttrId = id
          , questionnaireInitialExtension = extension
          , questionnaireInitialModifierExtension = modifierExtension
          , questionnaireInitialValue = value
          }

    where 
      fromValueXml = parseValueBoolean <|> parseValueDecimal <|> parseValueInteger <|> parseValueDate <|> parseValueDateTime <|> parseValueTime <|> parseValueString <|> parseValueUri <|> parseValueAttachment <|> parseValueCoding <|> parseValueQuantity <|> parseValueReference
      parseValueBoolean = do
                has <- Xmlbf.pElement "valueBoolean" (Xmlbf.pAttr "value")
                return $ QuestionnaireInitialValueBoolean (     fromBoolean has)
      parseValueDecimal = do
                has <- Xmlbf.pElement "valueDecimal" (Xmlbf.pAttr "value")
                return $ QuestionnaireInitialValueDecimal (     fromDecimal has)
      parseValueInteger = do
                has <- Xmlbf.pElement "valueInteger" (Xmlbf.pAttr "value")
                return $ QuestionnaireInitialValueInteger (     fromInt has)
      parseValueDate = do
                has <- Xmlbf.pElement "valueDate" (Xmlbf.pAttr "value")
                return $ QuestionnaireInitialValueDate (     fromDate has)
      parseValueDateTime = do
                has <- Xmlbf.pElement "valueDateTime" (Xmlbf.pAttr "value")
                return $ QuestionnaireInitialValueDateTime (     fromDateTime has)
      parseValueTime = do
                has <- Xmlbf.pElement "valueTime" (Xmlbf.pAttr "value")
                return $ QuestionnaireInitialValueTime (     fromTime has)
      parseValueString = do
                has <- Xmlbf.pElement "valueString" (Xmlbf.pAttr "value")
                return $ QuestionnaireInitialValueString (     fromString has)
      parseValueUri = do
                has <- Xmlbf.pElement "valueUri" (Xmlbf.pAttr "value")
                return $ QuestionnaireInitialValueUri (     fromUri has)
      parseValueAttachment = do
                has <- Xmlbf.pElement "valueAttachment" Xmlbf.fromXml
                return $ QuestionnaireInitialValueAttachment (                      has)
      parseValueCoding = do
                has <- Xmlbf.pElement "valueCoding" Xmlbf.fromXml
                return $ QuestionnaireInitialValueCoding (                      has)
      parseValueQuantity = do
                has <- Xmlbf.pElement "valueQuantity" Xmlbf.fromXml
                return $ QuestionnaireInitialValueQuantity (                      has)
      parseValueReference = do
                has <- Xmlbf.pElement "valueReference" Xmlbf.fromXml
                return $ QuestionnaireInitialValueReference (                      has)


data QuestionnaireAnswerOptionValue
    = QuestionnaireAnswerOptionValueInteger Integer
    | QuestionnaireAnswerOptionValueDate Date
    | QuestionnaireAnswerOptionValueTime Time
    | QuestionnaireAnswerOptionValueString Text
    | QuestionnaireAnswerOptionValueCoding Coding
    | QuestionnaireAnswerOptionValueReference Reference
    deriving (Eq, Show)

data QuestionnaireAnswerOption = QuestionnaireAnswerOption {
    questionnaireAnswerOptionAttrId :: Maybe Text
  , questionnaireAnswerOptionExtension :: [Extension]
  , questionnaireAnswerOptionModifierExtension :: [Extension]
  , questionnaireAnswerOptionValue :: QuestionnaireAnswerOptionValue
  , questionnaireAnswerOptionInitialSelected :: Maybe Boolean
  } deriving (Eq, Show)
--

instance ToJSON QuestionnaireAnswerOption where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (questionnaireAnswerOptionAttrId p)
    ,  "extension" .= toJSON (questionnaireAnswerOptionExtension p)
    ,  "modifierExtension" .= toJSON (questionnaireAnswerOptionModifierExtension p)
    , toValueJSON (questionnaireAnswerOptionValue p)
    ,  "initialSelected" .= toJSON (questionnaireAnswerOptionInitialSelected p)
    ]
    where 
      toValueJSON (     (QuestionnaireAnswerOptionValueInteger c)) = ("valueInteger", toJSON c)
      toValueJSON (     (QuestionnaireAnswerOptionValueDate c)) = ("valueDate", toJSON c)
      toValueJSON (     (QuestionnaireAnswerOptionValueTime c)) = ("valueTime", toJSON c)
      toValueJSON (     (QuestionnaireAnswerOptionValueString c)) = ("valueString", toJSON c)
      toValueJSON (     (QuestionnaireAnswerOptionValueCoding c)) = ("valueCoding", toJSON c)
      toValueJSON (     (QuestionnaireAnswerOptionValueReference c)) = ("valueReference", toJSON c)
instance FromJSON QuestionnaireAnswerOption where
  parseJSON = withObject "QuestionnaireAnswerOption" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        value <- parseValue o
        initialSelected <- o .:? "initialSelected"
        return QuestionnaireAnswerOption{
            questionnaireAnswerOptionAttrId = id
          , questionnaireAnswerOptionExtension = extension
          , questionnaireAnswerOptionModifierExtension = modifierExtension
          , questionnaireAnswerOptionValue = value
          , questionnaireAnswerOptionInitialSelected = initialSelected
          }
    where 
      parseValue o = parseValueInteger o <|> parseValueDate o <|> parseValueTime o <|> parseValueString o <|> parseValueCoding o <|> parseValueReference o
      parseValueInteger o = do
                has <- o .: "valueInteger"
                return $ QuestionnaireAnswerOptionValueInteger has
      parseValueDate o = do
                has <- o .: "valueDate"
                return $ QuestionnaireAnswerOptionValueDate has
      parseValueTime o = do
                has <- o .: "valueTime"
                return $ QuestionnaireAnswerOptionValueTime has
      parseValueString o = do
                has <- o .: "valueString"
                return $ QuestionnaireAnswerOptionValueString has
      parseValueCoding o = do
                has <- o .: "valueCoding"
                return $ QuestionnaireAnswerOptionValueCoding has
      parseValueReference o = do
                has <- o .: "valueReference"
                return $ QuestionnaireAnswerOptionValueReference has
instance Xmlbf.ToXml QuestionnaireAnswerOption where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (questionnaireAnswerOptionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (questionnaireAnswerOptionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (questionnaireAnswerOptionModifierExtension p))
             , toValueXml (questionnaireAnswerOptionValue p)
             , OptVal   "initialSelected" (fmap toBoolean (questionnaireAnswerOptionInitialSelected p))
             ]
       where 
          toValueXml (     (QuestionnaireAnswerOptionValueInteger p)) = Val      "valueInteger" (     toInt p)
          toValueXml (     (QuestionnaireAnswerOptionValueDate p)) = Val      "valueDate" (     toDate p)
          toValueXml (     (QuestionnaireAnswerOptionValueTime p)) = Val      "valueTime" (     toTime p)
          toValueXml (     (QuestionnaireAnswerOptionValueString p)) = Val      "valueString" (     toString p)
          toValueXml (     (QuestionnaireAnswerOptionValueCoding p)) = Prop     "valueCoding" (HM.empty, Xmlbf.toXml p)
          toValueXml (     (QuestionnaireAnswerOptionValueReference p)) = Prop     "valueReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml QuestionnaireAnswerOption where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    value <- fromValueXml
    initialSelected <- optional $ Xmlbf.pElement "initialSelected" (Xmlbf.pAttr "value")
    return QuestionnaireAnswerOption {
            questionnaireAnswerOptionAttrId = id
          , questionnaireAnswerOptionExtension = extension
          , questionnaireAnswerOptionModifierExtension = modifierExtension
          , questionnaireAnswerOptionValue = value
          , questionnaireAnswerOptionInitialSelected = fmap fromBoolean initialSelected
          }

    where 
      fromValueXml = parseValueInteger <|> parseValueDate <|> parseValueTime <|> parseValueString <|> parseValueCoding <|> parseValueReference
      parseValueInteger = do
                has <- Xmlbf.pElement "valueInteger" (Xmlbf.pAttr "value")
                return $ QuestionnaireAnswerOptionValueInteger (     fromInt has)
      parseValueDate = do
                has <- Xmlbf.pElement "valueDate" (Xmlbf.pAttr "value")
                return $ QuestionnaireAnswerOptionValueDate (     fromDate has)
      parseValueTime = do
                has <- Xmlbf.pElement "valueTime" (Xmlbf.pAttr "value")
                return $ QuestionnaireAnswerOptionValueTime (     fromTime has)
      parseValueString = do
                has <- Xmlbf.pElement "valueString" (Xmlbf.pAttr "value")
                return $ QuestionnaireAnswerOptionValueString (     fromString has)
      parseValueCoding = do
                has <- Xmlbf.pElement "valueCoding" Xmlbf.fromXml
                return $ QuestionnaireAnswerOptionValueCoding (                      has)
      parseValueReference = do
                has <- Xmlbf.pElement "valueReference" Xmlbf.fromXml
                return $ QuestionnaireAnswerOptionValueReference (                      has)


data QuestionnaireItemType
    = QITGroup
    | QITDisplay
    | QITBoolean
    | QITDecimal
    | QITInteger
    | QITDate
    | QITDateTime
    | QITTime
    | QITString
    | QITText
    | QITUrl
    | QITChoice
    | QITOpenChoice
    | QITAttachment
    | QITReference
    | QITQuantity
  deriving (Eq, Show)

instance ToJSON QuestionnaireItemType where
    toJSON QITGroup = String "group"
    toJSON QITDisplay = String "display"
    toJSON QITBoolean = String "boolean"
    toJSON QITDecimal = String "decimal"
    toJSON QITInteger = String "integer"
    toJSON QITDate = String "date"
    toJSON QITDateTime = String "dateTime"
    toJSON QITTime = String "time"
    toJSON QITString = String "string"
    toJSON QITText = String "text"
    toJSON QITUrl = String "url"
    toJSON QITChoice = String "choice"
    toJSON QITOpenChoice = String "open-choice"
    toJSON QITAttachment = String "attachment"
    toJSON QITReference = String "reference"
    toJSON QITQuantity = String "quantity"
instance FromJSON QuestionnaireItemType where
    parseJSON "group" = return QITGroup
    parseJSON "display" = return QITDisplay
    parseJSON "boolean" = return QITBoolean
    parseJSON "decimal" = return QITDecimal
    parseJSON "integer" = return QITInteger
    parseJSON "date" = return QITDate
    parseJSON "dateTime" = return QITDateTime
    parseJSON "time" = return QITTime
    parseJSON "string" = return QITString
    parseJSON "text" = return QITText
    parseJSON "url" = return QITUrl
    parseJSON "choice" = return QITChoice
    parseJSON "open-choice" = return QITOpenChoice
    parseJSON "attachment" = return QITAttachment
    parseJSON "reference" = return QITReference
    parseJSON "quantity" = return QITQuantity

toQuestionnaireItemType QITGroup = "group"
toQuestionnaireItemType QITDisplay = "display"
toQuestionnaireItemType QITBoolean = "boolean"
toQuestionnaireItemType QITDecimal = "decimal"
toQuestionnaireItemType QITInteger = "integer"
toQuestionnaireItemType QITDate = "date"
toQuestionnaireItemType QITDateTime = "dateTime"
toQuestionnaireItemType QITTime = "time"
toQuestionnaireItemType QITString = "string"
toQuestionnaireItemType QITText = "text"
toQuestionnaireItemType QITUrl = "url"
toQuestionnaireItemType QITChoice = "choice"
toQuestionnaireItemType QITOpenChoice = "open-choice"
toQuestionnaireItemType QITAttachment = "attachment"
toQuestionnaireItemType QITReference = "reference"
toQuestionnaireItemType QITQuantity = "quantity"
fromQuestionnaireItemType "group" = QITGroup
fromQuestionnaireItemType "display" = QITDisplay
fromQuestionnaireItemType "boolean" = QITBoolean
fromQuestionnaireItemType "decimal" = QITDecimal
fromQuestionnaireItemType "integer" = QITInteger
fromQuestionnaireItemType "date" = QITDate
fromQuestionnaireItemType "dateTime" = QITDateTime
fromQuestionnaireItemType "time" = QITTime
fromQuestionnaireItemType "string" = QITString
fromQuestionnaireItemType "text" = QITText
fromQuestionnaireItemType "url" = QITUrl
fromQuestionnaireItemType "choice" = QITChoice
fromQuestionnaireItemType "open-choice" = QITOpenChoice
fromQuestionnaireItemType "attachment" = QITAttachment
fromQuestionnaireItemType "reference" = QITReference
fromQuestionnaireItemType "quantity" = QITQuantity


data QuestionnaireItemEnableBehavior
    = QIEBAll
    | QIEBAny
  deriving (Eq, Show)

instance ToJSON QuestionnaireItemEnableBehavior where
    toJSON QIEBAll = String "all"
    toJSON QIEBAny = String "any"
instance FromJSON QuestionnaireItemEnableBehavior where
    parseJSON "all" = return QIEBAll
    parseJSON "any" = return QIEBAny

toQuestionnaireItemEnableBehavior QIEBAll = "all"
toQuestionnaireItemEnableBehavior QIEBAny = "any"
fromQuestionnaireItemEnableBehavior "all" = QIEBAll
fromQuestionnaireItemEnableBehavior "any" = QIEBAny


data QuestionnaireItem = QuestionnaireItem {
    questionnaireItemAttrId :: Maybe Text
  , questionnaireItemExtension :: [Extension]
  , questionnaireItemModifierExtension :: [Extension]
  , questionnaireItemLinkId :: Text
  , questionnaireItemDefinition :: Maybe Uri
  , questionnaireItemCode :: [Coding]
  , questionnaireItemPrefix :: Maybe Text
  , questionnaireItemText :: Maybe Text
  , questionnaireItemType :: QuestionnaireItemType
  , questionnaireItemEnableWhen :: [QuestionnaireEnableWhen]
  , questionnaireItemEnableBehavior :: Maybe QuestionnaireItemEnableBehavior
  , questionnaireItemRequired :: Maybe Boolean
  , questionnaireItemRepeats :: Maybe Boolean
  , questionnaireItemReadOnly :: Maybe Boolean
  , questionnaireItemMaxLength :: Maybe Integer
  , questionnaireItemAnswerValueSet :: Maybe Canonical
  , questionnaireItemAnswerOption :: [QuestionnaireAnswerOption]
  , questionnaireItemInitial :: [QuestionnaireInitial]
  , questionnaireItemItem :: [QuestionnaireItem]
  } deriving (Eq, Show)
--

instance ToJSON QuestionnaireItem where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (questionnaireItemAttrId p)
    ,  "extension" .= toJSON (questionnaireItemExtension p)
    ,  "modifierExtension" .= toJSON (questionnaireItemModifierExtension p)
    ,  "linkId" .= toJSON (questionnaireItemLinkId p)
    ,  "definition" .= toJSON (questionnaireItemDefinition p)
    ,  "code" .= toJSON (questionnaireItemCode p)
    ,  "prefix" .= toJSON (questionnaireItemPrefix p)
    ,  "text" .= toJSON (questionnaireItemText p)
    ,  "type" .= toJSON (questionnaireItemType p)
    ,  "enableWhen" .= toJSON (questionnaireItemEnableWhen p)
    ,  "enableBehavior" .= toJSON (questionnaireItemEnableBehavior p)
    ,  "required" .= toJSON (questionnaireItemRequired p)
    ,  "repeats" .= toJSON (questionnaireItemRepeats p)
    ,  "readOnly" .= toJSON (questionnaireItemReadOnly p)
    ,  "maxLength" .= toJSON (questionnaireItemMaxLength p)
    ,  "answerValueSet" .= toJSON (questionnaireItemAnswerValueSet p)
    ,  "answerOption" .= toJSON (questionnaireItemAnswerOption p)
    ,  "initial" .= toJSON (questionnaireItemInitial p)
    ,  "item" .= toJSON (questionnaireItemItem p)
    ]
instance FromJSON QuestionnaireItem where
  parseJSON = withObject "QuestionnaireItem" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        linkId <- o .:  "linkId"
        definition <- o .:? "definition"
        code <- o .:? "code" .!= []
        prefix <- o .:? "prefix"
        text <- o .:? "text"
        ty <- o .:  "type"
        enableWhen <- o .:? "enableWhen" .!= []
        enableBehavior <- o .:? "enableBehavior"
        required <- o .:? "required"
        repeats <- o .:? "repeats"
        readOnly <- o .:? "readOnly"
        maxLength <- o .:? "maxLength"
        answerValueSet <- o .:? "answerValueSet"
        answerOption <- o .:? "answerOption" .!= []
        initial <- o .:? "initial" .!= []
        item <- o .:? "item" .!= []
        return QuestionnaireItem{
            questionnaireItemAttrId = id
          , questionnaireItemExtension = extension
          , questionnaireItemModifierExtension = modifierExtension
          , questionnaireItemLinkId = linkId
          , questionnaireItemDefinition = definition
          , questionnaireItemCode = code
          , questionnaireItemPrefix = prefix
          , questionnaireItemText = text
          , questionnaireItemType = ty
          , questionnaireItemEnableWhen = enableWhen
          , questionnaireItemEnableBehavior = enableBehavior
          , questionnaireItemRequired = required
          , questionnaireItemRepeats = repeats
          , questionnaireItemReadOnly = readOnly
          , questionnaireItemMaxLength = maxLength
          , questionnaireItemAnswerValueSet = answerValueSet
          , questionnaireItemAnswerOption = answerOption
          , questionnaireItemInitial = initial
          , questionnaireItemItem = item
          }
instance Xmlbf.ToXml QuestionnaireItem where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (questionnaireItemAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (questionnaireItemExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (questionnaireItemModifierExtension p))
             , Val      "linkId" (     toString (questionnaireItemLinkId p))
             , OptVal   "definition" (fmap toUri (questionnaireItemDefinition p))
             , PropList "code" (fmap Xmlbf.toXml (questionnaireItemCode p))
             , OptVal   "prefix" (fmap toString (questionnaireItemPrefix p))
             , OptVal   "text" (fmap toString (questionnaireItemText p))
             , Val      "type" (     toQuestionnaireItemType (questionnaireItemType p))
             , PropList "enableWhen" (fmap Xmlbf.toXml (questionnaireItemEnableWhen p))
             , OptVal   "enableBehavior" (fmap toQuestionnaireItemEnableBehavior (questionnaireItemEnableBehavior p))
             , OptVal   "required" (fmap toBoolean (questionnaireItemRequired p))
             , OptVal   "repeats" (fmap toBoolean (questionnaireItemRepeats p))
             , OptVal   "readOnly" (fmap toBoolean (questionnaireItemReadOnly p))
             , OptVal   "maxLength" (fmap toInt (questionnaireItemMaxLength p))
             , OptVal   "answerValueSet" (fmap toCanonical (questionnaireItemAnswerValueSet p))
             , PropList "answerOption" (fmap Xmlbf.toXml (questionnaireItemAnswerOption p))
             , PropList "initial" (fmap Xmlbf.toXml (questionnaireItemInitial p))
             , PropList "item" (fmap Xmlbf.toXml (questionnaireItemItem p))
             ]
instance Xmlbf.FromXml QuestionnaireItem where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    linkId <-            Xmlbf.pElement "linkId" (Xmlbf.pAttr "value")
    definition <- optional $ Xmlbf.pElement "definition" (Xmlbf.pAttr "value")
    code <- many     $ Xmlbf.pElement "code" Xmlbf.fromXml
    prefix <- optional $ Xmlbf.pElement "prefix" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" (Xmlbf.pAttr "value")
    ty <-            Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    enableWhen <- many     $ Xmlbf.pElement "enableWhen" Xmlbf.fromXml
    enableBehavior <- optional $ Xmlbf.pElement "enableBehavior" (Xmlbf.pAttr "value")
    required <- optional $ Xmlbf.pElement "required" (Xmlbf.pAttr "value")
    repeats <- optional $ Xmlbf.pElement "repeats" (Xmlbf.pAttr "value")
    readOnly <- optional $ Xmlbf.pElement "readOnly" (Xmlbf.pAttr "value")
    maxLength <- optional $ Xmlbf.pElement "maxLength" (Xmlbf.pAttr "value")
    answerValueSet <- optional $ Xmlbf.pElement "answerValueSet" (Xmlbf.pAttr "value")
    answerOption <- many     $ Xmlbf.pElement "answerOption" Xmlbf.fromXml
    initial <- many     $ Xmlbf.pElement "initial" Xmlbf.fromXml
    item <- many     $ Xmlbf.pElement "item" Xmlbf.fromXml
    return QuestionnaireItem {
            questionnaireItemAttrId = id
          , questionnaireItemExtension = extension
          , questionnaireItemModifierExtension = modifierExtension
          , questionnaireItemLinkId =      fromString linkId
          , questionnaireItemDefinition = fmap fromUri definition
          , questionnaireItemCode = code
          , questionnaireItemPrefix = fmap fromString prefix
          , questionnaireItemText = fmap fromString text
          , questionnaireItemType =      fromQuestionnaireItemType ty
          , questionnaireItemEnableWhen = enableWhen
          , questionnaireItemEnableBehavior = fmap fromQuestionnaireItemEnableBehavior enableBehavior
          , questionnaireItemRequired = fmap fromBoolean required
          , questionnaireItemRepeats = fmap fromBoolean repeats
          , questionnaireItemReadOnly = fmap fromBoolean readOnly
          , questionnaireItemMaxLength = fmap fromInt maxLength
          , questionnaireItemAnswerValueSet = fmap fromCanonical answerValueSet
          , questionnaireItemAnswerOption = answerOption
          , questionnaireItemInitial = initial
          , questionnaireItemItem = item
          }

