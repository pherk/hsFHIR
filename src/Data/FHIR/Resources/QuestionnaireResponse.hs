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
-- FHIR 4.0.0 QuestionnaireResponse
--

module Data.FHIR.Resources.QuestionnaireResponse where

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

data QuestionnaireResponseStatus
    = QRSInProgress
    | QRSCompleted
    | QRSAmended
    | QRSEnteredInError
    | QRSStopped
  deriving (Eq, Show)

instance ToJSON QuestionnaireResponseStatus where
    toJSON QRSInProgress = String "in-progress"
    toJSON QRSCompleted = String "completed"
    toJSON QRSAmended = String "amended"
    toJSON QRSEnteredInError = String "entered-in-error"
    toJSON QRSStopped = String "stopped"
instance FromJSON QuestionnaireResponseStatus where
    parseJSON "in-progress" = return QRSInProgress
    parseJSON "completed" = return QRSCompleted
    parseJSON "amended" = return QRSAmended
    parseJSON "entered-in-error" = return QRSEnteredInError
    parseJSON "stopped" = return QRSStopped

toQuestionnaireResponseStatus QRSInProgress = "in-progress"
toQuestionnaireResponseStatus QRSCompleted = "completed"
toQuestionnaireResponseStatus QRSAmended = "amended"
toQuestionnaireResponseStatus QRSEnteredInError = "entered-in-error"
toQuestionnaireResponseStatus QRSStopped = "stopped"
fromQuestionnaireResponseStatus "in-progress" = QRSInProgress
fromQuestionnaireResponseStatus "completed" = QRSCompleted
fromQuestionnaireResponseStatus "amended" = QRSAmended
fromQuestionnaireResponseStatus "entered-in-error" = QRSEnteredInError
fromQuestionnaireResponseStatus "stopped" = QRSStopped


data QuestionnaireResponse = QuestionnaireResponse {
    questionnaireResponseId :: Maybe Id
  , questionnaireResponseMeta :: Maybe Meta
  , questionnaireResponseImplicitRules :: Maybe Uri
  , questionnaireResponseLanguage :: Maybe Language
  , questionnaireResponseText :: Maybe Narrative
--    questionnaireResponseContained :: [ResourceContainer]
  , questionnaireResponseExtension :: [Extension]
  , questionnaireResponseModifierExtension :: [Extension]
  , questionnaireResponseIdentifier :: Maybe Identifier
  , questionnaireResponseBasedOn :: [Reference]
  , questionnaireResponsePartOf :: [Reference]
  , questionnaireResponseQuestionnaire :: Maybe Canonical
  , questionnaireResponseStatus :: QuestionnaireResponseStatus
  , questionnaireResponseSubject :: Maybe Reference
  , questionnaireResponseEncounter :: Maybe Reference
  , questionnaireResponseAuthored :: Maybe DateTime
  , questionnaireResponseAuthor :: Maybe Reference
  , questionnaireResponseSource :: Maybe Reference
  , questionnaireResponseItem :: [QuestionnaireResponseItem]
  } deriving (Eq, Show)
--

instance ToJSON QuestionnaireResponse where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "QuestionnaireResponse")
    ,  "id" .= toJSON (questionnaireResponseId p)
    ,  "meta" .= toJSON (questionnaireResponseMeta p)
    ,  "implicitRules" .= toJSON (questionnaireResponseImplicitRules p)
    ,  "language" .= toJSON (questionnaireResponseLanguage p)
    ,  "text" .= toJSON (questionnaireResponseText p)
--    , "contained" .= toJSON (questionnaireResponseContained p)
    ,  "extension" .= toJSON (questionnaireResponseExtension p)
    ,  "modifierExtension" .= toJSON (questionnaireResponseModifierExtension p)
    ,  "identifier" .= toJSON (questionnaireResponseIdentifier p)
    ,  "basedOn" .= toJSON (questionnaireResponseBasedOn p)
    ,  "partOf" .= toJSON (questionnaireResponsePartOf p)
    ,  "questionnaire" .= toJSON (questionnaireResponseQuestionnaire p)
    ,  "status" .= toJSON (questionnaireResponseStatus p)
    ,  "subject" .= toJSON (questionnaireResponseSubject p)
    ,  "encounter" .= toJSON (questionnaireResponseEncounter p)
    ,  "authored" .= toJSON (questionnaireResponseAuthored p)
    ,  "author" .= toJSON (questionnaireResponseAuthor p)
    ,  "source" .= toJSON (questionnaireResponseSource p)
    ,  "item" .= toJSON (questionnaireResponseItem p)
    ]
instance FromJSON QuestionnaireResponse where
  parseJSON = withObject "QuestionnaireResponse" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "QuestionnaireResponse" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier"
        basedOn <- o .:? "basedOn" .!= []
        partOf <- o .:? "partOf" .!= []
        questionnaire <- o .:? "questionnaire"
        status <- o .:  "status"
        subject <- o .:? "subject"
        encounter <- o .:? "encounter"
        authored <- o .:? "authored"
        author <- o .:? "author"
        source <- o .:? "source"
        item <- o .:? "item" .!= []
        return QuestionnaireResponse{
            questionnaireResponseId = id
          , questionnaireResponseMeta = meta
          , questionnaireResponseImplicitRules = implicitRules
          , questionnaireResponseLanguage = language
          , questionnaireResponseText = text
--          , questionnaireResponseContained = contained
          , questionnaireResponseExtension = extension
          , questionnaireResponseModifierExtension = modifierExtension
          , questionnaireResponseIdentifier = identifier
          , questionnaireResponseBasedOn = basedOn
          , questionnaireResponsePartOf = partOf
          , questionnaireResponseQuestionnaire = questionnaire
          , questionnaireResponseStatus = status
          , questionnaireResponseSubject = subject
          , questionnaireResponseEncounter = encounter
          , questionnaireResponseAuthored = authored
          , questionnaireResponseAuthor = author
          , questionnaireResponseSource = source
          , questionnaireResponseItem = item
          }
      _ -> fail "not a QuestionnaireResponse"
instance Xmlbf.ToXml QuestionnaireResponse where
  toXml p = Xmlbf.element "QuestionnaireResponse" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (questionnaireResponseId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (questionnaireResponseMeta p))
             , OptVal   "implicitRules" (fmap toUri (questionnaireResponseImplicitRules p))
             , OptVal   "language" (fmap toLanguage (questionnaireResponseLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (questionnaireResponseText p))
--             , PropList "contained" (fmap Xmlbf.toXml (questionnaireResponseContained p))
             , PropList "extension" (fmap Xmlbf.toXml (questionnaireResponseExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (questionnaireResponseModifierExtension p))
             , OptProp  "identifier" (fmap Xmlbf.toXml (questionnaireResponseIdentifier p))
             , PropList "basedOn" (fmap Xmlbf.toXml (questionnaireResponseBasedOn p))
             , PropList "partOf" (fmap Xmlbf.toXml (questionnaireResponsePartOf p))
             , OptVal   "questionnaire" (fmap toCanonical (questionnaireResponseQuestionnaire p))
             , Val      "status" (     toQuestionnaireResponseStatus (questionnaireResponseStatus p))
             , OptProp  "subject" (fmap Xmlbf.toXml (questionnaireResponseSubject p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (questionnaireResponseEncounter p))
             , OptVal   "authored" (fmap toDateTime (questionnaireResponseAuthored p))
             , OptProp  "author" (fmap Xmlbf.toXml (questionnaireResponseAuthor p))
             , OptProp  "source" (fmap Xmlbf.toXml (questionnaireResponseSource p))
             , PropList "item" (fmap Xmlbf.toXml (questionnaireResponseItem p))
             ]
instance Xmlbf.FromXml QuestionnaireResponse where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" Xmlbf.fromXml
--    contained <- many     $ Xmlbf.pElement "contained" Xmlbf.fromXml
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    identifier <- optional $ Xmlbf.pElement "identifier" Xmlbf.fromXml
    basedOn <- many     $ Xmlbf.pElement "basedOn" Xmlbf.fromXml
    partOf <- many     $ Xmlbf.pElement "partOf" Xmlbf.fromXml
    questionnaire <- optional $ Xmlbf.pElement "questionnaire" (Xmlbf.pAttr "value")
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    subject <- optional $ Xmlbf.pElement "subject" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    authored <- optional $ Xmlbf.pElement "authored" (Xmlbf.pAttr "value")
    author <- optional $ Xmlbf.pElement "author" Xmlbf.fromXml
    source <- optional $ Xmlbf.pElement "source" Xmlbf.fromXml
    item <- many     $ Xmlbf.pElement "item" Xmlbf.fromXml
    return QuestionnaireResponse {
            questionnaireResponseId = fmap fromId id
          , questionnaireResponseMeta = meta
          , questionnaireResponseImplicitRules = fmap fromUri implicitRules
          , questionnaireResponseLanguage = fmap fromLanguage language
          , questionnaireResponseText = text
--          , questionnaireResponseContained = contained
          , questionnaireResponseExtension = extension
          , questionnaireResponseModifierExtension = modifierExtension
          , questionnaireResponseIdentifier = identifier
          , questionnaireResponseBasedOn = basedOn
          , questionnaireResponsePartOf = partOf
          , questionnaireResponseQuestionnaire = fmap fromCanonical questionnaire
          , questionnaireResponseStatus =      fromQuestionnaireResponseStatus status
          , questionnaireResponseSubject = subject
          , questionnaireResponseEncounter = encounter
          , questionnaireResponseAuthored = fmap fromDateTime authored
          , questionnaireResponseAuthor = author
          , questionnaireResponseSource = source
          , questionnaireResponseItem = item
          }



data QuestionnaireResponseItem = QuestionnaireResponseItem {
    questionnaireResponseItemAttrId :: Maybe Text
  , questionnaireResponseItemExtension :: [Extension]
  , questionnaireResponseItemModifierExtension :: [Extension]
  , questionnaireResponseItemLinkId :: Text
  , questionnaireResponseItemDefinition :: Maybe Uri
  , questionnaireResponseItemText :: Maybe Text
  , questionnaireResponseItemAnswer :: [QuestionnaireResponseAnswer]
  , questionnaireResponseItemItem :: [QuestionnaireResponseItem]
  } deriving (Eq, Show)
--

instance ToJSON QuestionnaireResponseItem where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (questionnaireResponseItemAttrId p)
    ,  "extension" .= toJSON (questionnaireResponseItemExtension p)
    ,  "modifierExtension" .= toJSON (questionnaireResponseItemModifierExtension p)
    ,  "linkId" .= toJSON (questionnaireResponseItemLinkId p)
    ,  "definition" .= toJSON (questionnaireResponseItemDefinition p)
    ,  "text" .= toJSON (questionnaireResponseItemText p)
    ,  "answer" .= toJSON (questionnaireResponseItemAnswer p)
    ,  "item" .= toJSON (questionnaireResponseItemItem p)
    ]
instance FromJSON QuestionnaireResponseItem where
  parseJSON = withObject "QuestionnaireResponseItem" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        linkId <- o .:  "linkId"
        definition <- o .:? "definition"
        text <- o .:? "text"
        answer <- o .:? "answer" .!= []
        item <- o .:? "item" .!= []
        return QuestionnaireResponseItem{
            questionnaireResponseItemAttrId = id
          , questionnaireResponseItemExtension = extension
          , questionnaireResponseItemModifierExtension = modifierExtension
          , questionnaireResponseItemLinkId = linkId
          , questionnaireResponseItemDefinition = definition
          , questionnaireResponseItemText = text
          , questionnaireResponseItemAnswer = answer
          , questionnaireResponseItemItem = item
          }
instance Xmlbf.ToXml QuestionnaireResponseItem where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (questionnaireResponseItemAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (questionnaireResponseItemExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (questionnaireResponseItemModifierExtension p))
             , Val      "linkId" (     toString (questionnaireResponseItemLinkId p))
             , OptVal   "definition" (fmap toUri (questionnaireResponseItemDefinition p))
             , OptVal   "text" (fmap toString (questionnaireResponseItemText p))
             , PropList "answer" (fmap Xmlbf.toXml (questionnaireResponseItemAnswer p))
             , PropList "item" (fmap Xmlbf.toXml (questionnaireResponseItemItem p))
             ]
instance Xmlbf.FromXml QuestionnaireResponseItem where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    linkId <-            Xmlbf.pElement "linkId" (Xmlbf.pAttr "value")
    definition <- optional $ Xmlbf.pElement "definition" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" (Xmlbf.pAttr "value")
    answer <- many     $ Xmlbf.pElement "answer" Xmlbf.fromXml
    item <- many     $ Xmlbf.pElement "item" Xmlbf.fromXml
    return QuestionnaireResponseItem {
            questionnaireResponseItemAttrId = id
          , questionnaireResponseItemExtension = extension
          , questionnaireResponseItemModifierExtension = modifierExtension
          , questionnaireResponseItemLinkId =      fromString linkId
          , questionnaireResponseItemDefinition = fmap fromUri definition
          , questionnaireResponseItemText = fmap fromString text
          , questionnaireResponseItemAnswer = answer
          , questionnaireResponseItemItem = item
          }



data QuestionnaireResponseAnswerValue
    = QuestionnaireResponseAnswerValueBoolean Boolean
    | QuestionnaireResponseAnswerValueDecimal Decimal
    | QuestionnaireResponseAnswerValueInteger Integer
    | QuestionnaireResponseAnswerValueDate Date
    | QuestionnaireResponseAnswerValueDateTime DateTime
    | QuestionnaireResponseAnswerValueTime Time
    | QuestionnaireResponseAnswerValueString Text
    | QuestionnaireResponseAnswerValueUri Uri
    | QuestionnaireResponseAnswerValueAttachment Attachment
    | QuestionnaireResponseAnswerValueCoding Coding
    | QuestionnaireResponseAnswerValueQuantity Quantity
    | QuestionnaireResponseAnswerValueReference Reference
    deriving (Eq, Show)

data QuestionnaireResponseAnswer = QuestionnaireResponseAnswer {
    questionnaireResponseAnswerAttrId :: Maybe Text
  , questionnaireResponseAnswerExtension :: [Extension]
  , questionnaireResponseAnswerModifierExtension :: [Extension]
  , questionnaireResponseAnswerValue :: Maybe QuestionnaireResponseAnswerValue
  , questionnaireResponseAnswerItem :: [QuestionnaireResponseItem]
  } deriving (Eq, Show)
--

instance ToJSON QuestionnaireResponseAnswer where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (questionnaireResponseAnswerAttrId p)
    ,  "extension" .= toJSON (questionnaireResponseAnswerExtension p)
    ,  "modifierExtension" .= toJSON (questionnaireResponseAnswerModifierExtension p)
    , toValueJSON (questionnaireResponseAnswerValue p)
    ,  "item" .= toJSON (questionnaireResponseAnswerItem p)
    ]
    where 
      toValueJSON (     Nothing   ) = ("value", Null)
      toValueJSON (Just (QuestionnaireResponseAnswerValueBoolean c)) = ("valueBoolean", toJSON c)
      toValueJSON (Just (QuestionnaireResponseAnswerValueDecimal c)) = ("valueDecimal", toJSON c)
      toValueJSON (Just (QuestionnaireResponseAnswerValueInteger c)) = ("valueInteger", toJSON c)
      toValueJSON (Just (QuestionnaireResponseAnswerValueDate c)) = ("valueDate", toJSON c)
      toValueJSON (Just (QuestionnaireResponseAnswerValueDateTime c)) = ("valueDateTime", toJSON c)
      toValueJSON (Just (QuestionnaireResponseAnswerValueTime c)) = ("valueTime", toJSON c)
      toValueJSON (Just (QuestionnaireResponseAnswerValueString c)) = ("valueString", toJSON c)
      toValueJSON (Just (QuestionnaireResponseAnswerValueUri c)) = ("valueUri", toJSON c)
      toValueJSON (Just (QuestionnaireResponseAnswerValueAttachment c)) = ("valueAttachment", toJSON c)
      toValueJSON (Just (QuestionnaireResponseAnswerValueCoding c)) = ("valueCoding", toJSON c)
      toValueJSON (Just (QuestionnaireResponseAnswerValueQuantity c)) = ("valueQuantity", toJSON c)
      toValueJSON (Just (QuestionnaireResponseAnswerValueReference c)) = ("valueReference", toJSON c)
instance FromJSON QuestionnaireResponseAnswer where
  parseJSON = withObject "QuestionnaireResponseAnswer" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        value <- parseValue o
        item <- o .:? "item" .!= []
        return QuestionnaireResponseAnswer{
            questionnaireResponseAnswerAttrId = id
          , questionnaireResponseAnswerExtension = extension
          , questionnaireResponseAnswerModifierExtension = modifierExtension
          , questionnaireResponseAnswerValue = value
          , questionnaireResponseAnswerItem = item
          }
    where 
      parseValue o = parseValueBoolean o <|> parseValueDecimal o <|> parseValueInteger o <|> parseValueDate o <|> parseValueDateTime o <|> parseValueTime o <|> parseValueString o <|> parseValueUri o <|> parseValueAttachment o <|> parseValueCoding o <|> parseValueQuantity o <|> parseValueReference o
      parseValueBoolean o = do
                has <- o .: "valueBoolean"
                return $ Just (QuestionnaireResponseAnswerValueBoolean has)
      parseValueDecimal o = do
                has <- o .: "valueDecimal"
                return $ Just (QuestionnaireResponseAnswerValueDecimal has)
      parseValueInteger o = do
                has <- o .: "valueInteger"
                return $ Just (QuestionnaireResponseAnswerValueInteger has)
      parseValueDate o = do
                has <- o .: "valueDate"
                return $ Just (QuestionnaireResponseAnswerValueDate has)
      parseValueDateTime o = do
                has <- o .: "valueDateTime"
                return $ Just (QuestionnaireResponseAnswerValueDateTime has)
      parseValueTime o = do
                has <- o .: "valueTime"
                return $ Just (QuestionnaireResponseAnswerValueTime has)
      parseValueString o = do
                has <- o .: "valueString"
                return $ Just (QuestionnaireResponseAnswerValueString has)
      parseValueUri o = do
                has <- o .: "valueUri"
                return $ Just (QuestionnaireResponseAnswerValueUri has)
      parseValueAttachment o = do
                has <- o .: "valueAttachment"
                return $ Just (QuestionnaireResponseAnswerValueAttachment has)
      parseValueCoding o = do
                has <- o .: "valueCoding"
                return $ Just (QuestionnaireResponseAnswerValueCoding has)
      parseValueQuantity o = do
                has <- o .: "valueQuantity"
                return $ Just (QuestionnaireResponseAnswerValueQuantity has)
      parseValueReference o = do
                has <- o .: "valueReference"
                return $ Just (QuestionnaireResponseAnswerValueReference has)
instance Xmlbf.ToXml QuestionnaireResponseAnswer where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (questionnaireResponseAnswerAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (questionnaireResponseAnswerExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (questionnaireResponseAnswerModifierExtension p))
             , toValueXml (questionnaireResponseAnswerValue p)
             , PropList "item" (fmap Xmlbf.toXml (questionnaireResponseAnswerItem p))
             ]
       where 
          toValueXml ( Nothing   ) = (OptVal "value" Nothing)
          toValueXml (Just (QuestionnaireResponseAnswerValueBoolean p)) = Val   "valueBoolean" (toBoolean p)
          toValueXml (Just (QuestionnaireResponseAnswerValueDecimal p)) = Val   "valueDecimal" (toDecimal p)
          toValueXml (Just (QuestionnaireResponseAnswerValueInteger p)) = Val   "valueInteger" (toInt p)
          toValueXml (Just (QuestionnaireResponseAnswerValueDate p)) = Val   "valueDate" (toDate p)
          toValueXml (Just (QuestionnaireResponseAnswerValueDateTime p)) = Val   "valueDateTime" (toDateTime p)
          toValueXml (Just (QuestionnaireResponseAnswerValueTime p)) = Val   "valueTime" (toTime p)
          toValueXml (Just (QuestionnaireResponseAnswerValueString p)) = Val   "valueString" (toString p)
          toValueXml (Just (QuestionnaireResponseAnswerValueUri p)) = Val   "valueUri" (toUri p)
          toValueXml (Just (QuestionnaireResponseAnswerValueAttachment p)) = Prop  "valueAttachment" (HM.empty, Xmlbf.toXml p)
          toValueXml (Just (QuestionnaireResponseAnswerValueCoding p)) = Prop  "valueCoding" (HM.empty, Xmlbf.toXml p)
          toValueXml (Just (QuestionnaireResponseAnswerValueQuantity p)) = Prop  "valueQuantity" (HM.empty, Xmlbf.toXml p)
          toValueXml (Just (QuestionnaireResponseAnswerValueReference p)) = Prop  "valueReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml QuestionnaireResponseAnswer where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    value <- fromValueXml
    item <- many     $ Xmlbf.pElement "item" Xmlbf.fromXml
    return QuestionnaireResponseAnswer {
            questionnaireResponseAnswerAttrId = id
          , questionnaireResponseAnswerExtension = extension
          , questionnaireResponseAnswerModifierExtension = modifierExtension
          , questionnaireResponseAnswerValue = value
          , questionnaireResponseAnswerItem = item
          }

    where 
      fromValueXml = parseValueBoolean <|> parseValueDecimal <|> parseValueInteger <|> parseValueDate <|> parseValueDateTime <|> parseValueTime <|> parseValueString <|> parseValueUri <|> parseValueAttachment <|> parseValueCoding <|> parseValueQuantity <|> parseValueReference <|> pure Nothing
      parseValueBoolean = do
                has <- Xmlbf.pElement "valueBoolean" (Xmlbf.pAttr "value")
                return $ Just (QuestionnaireResponseAnswerValueBoolean (     fromBoolean has))
      parseValueDecimal = do
                has <- Xmlbf.pElement "valueDecimal" (Xmlbf.pAttr "value")
                return $ Just (QuestionnaireResponseAnswerValueDecimal (     fromDecimal has))
      parseValueInteger = do
                has <- Xmlbf.pElement "valueInteger" (Xmlbf.pAttr "value")
                return $ Just (QuestionnaireResponseAnswerValueInteger (     fromInt has))
      parseValueDate = do
                has <- Xmlbf.pElement "valueDate" (Xmlbf.pAttr "value")
                return $ Just (QuestionnaireResponseAnswerValueDate (     fromDate has))
      parseValueDateTime = do
                has <- Xmlbf.pElement "valueDateTime" (Xmlbf.pAttr "value")
                return $ Just (QuestionnaireResponseAnswerValueDateTime (     fromDateTime has))
      parseValueTime = do
                has <- Xmlbf.pElement "valueTime" (Xmlbf.pAttr "value")
                return $ Just (QuestionnaireResponseAnswerValueTime (     fromTime has))
      parseValueString = do
                has <- Xmlbf.pElement "valueString" (Xmlbf.pAttr "value")
                return $ Just (QuestionnaireResponseAnswerValueString (     fromString has))
      parseValueUri = do
                has <- Xmlbf.pElement "valueUri" (Xmlbf.pAttr "value")
                return $ Just (QuestionnaireResponseAnswerValueUri (     fromUri has))
      parseValueAttachment = do
                has <- Xmlbf.pElement "valueAttachment" Xmlbf.fromXml
                return $ Just (QuestionnaireResponseAnswerValueAttachment (                      has))
      parseValueCoding = do
                has <- Xmlbf.pElement "valueCoding" Xmlbf.fromXml
                return $ Just (QuestionnaireResponseAnswerValueCoding (                      has))
      parseValueQuantity = do
                has <- Xmlbf.pElement "valueQuantity" Xmlbf.fromXml
                return $ Just (QuestionnaireResponseAnswerValueQuantity (                      has))
      parseValueReference = do
                has <- Xmlbf.pElement "valueReference" Xmlbf.fromXml
                return $ Just (QuestionnaireResponseAnswerValueReference (                      has))



