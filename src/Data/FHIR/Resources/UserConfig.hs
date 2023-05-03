{-# LANGUAGE NoImplicitPrelude  #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators     #-}

module Data.FHIR.Resources.UserConfig where

import Data.Aeson  
import Data.Aeson.Types hiding (parseJSON)

import qualified Data.HashMap.Strict as HM
--import GHC.Generics
import GHC.TypeLits

import RIO                  hiding(fromString)
import qualified RIO.Vector as V
-- import Xmlbf
import           Data.FHIR.Datatypes
import           Data.FHIR.Datatypes.XML
import           Data.FHIR.Datatypes.XmlUtils
import           Data.FHIR.Resources.DomainResource
import qualified Xmlbf  as Xmlbf


data UserConfigItem = UserConfigItem {
    userConfigItemAttrId :: Maybe Text
  , userConfigItemExtension :: [Extension]
  , userConfigItemModifierExtension :: [Extension]
  , userConfigItemLinkId :: Text
  , userConfigItemDefinition :: Maybe Uri
  , userConfigItemText :: Maybe Text
  , userConfigItemData :: [UserConfigData]
  , userConfigItemItem :: [UserConfigItem]
  } deriving stock (Eq, Generic, Show)
--



instance ToJSON UserConfigItem where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (userConfigItemAttrId p)
    ,  "extension" .= toJSON (userConfigItemExtension p)
    ,  "modifierExtension" .= toJSON (userConfigItemModifierExtension p)
    ,  "linkId" .= toJSON (userConfigItemLinkId p)
    ,  "definition" .= toJSON (userConfigItemDefinition p)
    ,  "text" .= toJSON (userConfigItemText p)
    ,  "data" .= toJSON (userConfigItemData p)
    ,  "item" .= toJSON (userConfigItemItem p)
    ]
instance FromJSON UserConfigItem where
  parseJSON = withObject "UserConfigItem" $ \o -> do
        iid <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        linkId <- o .:  "linkId"
        definition <- o .:? "definition"
        text <- o .:? "text"
        da <- o .:? "data" .!= []
        item <- o .:? "item" .!= []
        return UserConfigItem{
            userConfigItemAttrId = iid
          , userConfigItemExtension = extension
          , userConfigItemModifierExtension = modifierExtension
          , userConfigItemLinkId = linkId
          , userConfigItemDefinition = definition
          , userConfigItemText = text
          , userConfigItemData = da
          , userConfigItemItem = item
          }
instance Xmlbf.ToXml UserConfigItem where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (userConfigItemAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (userConfigItemExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (userConfigItemModifierExtension p))
             , Val      "linkId" (     toString (userConfigItemLinkId p))
             , OptVal   "definition" (fmap toUri (userConfigItemDefinition p))
             , OptVal   "text" (fmap toString (userConfigItemText p))
             , PropList "data" (fmap Xmlbf.toXml (userConfigItemData p))
             , PropList "item" (fmap Xmlbf.toXml (userConfigItemItem p))
             ]
instance Xmlbf.FromXml UserConfigItem where
  fromXml = do
    iid <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    linkId <-            Xmlbf.pElement "linkId" (Xmlbf.pAttr "value")
    definition <- optional $ Xmlbf.pElement "definition" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" (Xmlbf.pAttr "value")
    da <- many     $ Xmlbf.pElement "data" Xmlbf.fromXml
    item <- many     $ Xmlbf.pElement "item" Xmlbf.fromXml
    return UserConfigItem {
            userConfigItemAttrId = iid
          , userConfigItemExtension = extension
          , userConfigItemModifierExtension = modifierExtension
          , userConfigItemLinkId = fromString linkId
          , userConfigItemDefinition = fmap fromUri definition
          , userConfigItemText = fmap fromString text
          , userConfigItemData = da
          , userConfigItemItem = item
          }



data UserConfigDataValue
    = UserConfigDataValueBoolean Boolean
    | UserConfigDataValueDecimal Decimal
    | UserConfigDataValueInteger Integer
    | UserConfigDataValueDate Date
    | UserConfigDataValueDateTime DateTime
    | UserConfigDataValueTime Time
    | UserConfigDataValueString Text
    | UserConfigDataValueUri Uri
    | UserConfigDataValueAttachment Attachment
    | UserConfigDataValueCoding Coding
    | UserConfigDataValueQuantity Quantity
    | UserConfigDataValueReference Reference
    deriving stock (Eq, Show)

data UserConfigData = UserConfigData {
    userConfigDataAttrId :: Maybe Text
  , userConfigDataExtension :: [Extension]
  , userConfigDataModifierExtension :: [Extension]
  , userConfigDataValue :: Maybe UserConfigDataValue
  , userConfigDataItem :: [UserConfigItem]
  } deriving stock (Eq, Show)
--

instance ToJSON UserConfigData where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (userConfigDataAttrId p)
    ,  "extension" .= toJSON (userConfigDataExtension p)
    ,  "modifierExtension" .= toJSON (userConfigDataModifierExtension p)
    , toValueJSON (userConfigDataValue p)
    ,  "item" .= toJSON (userConfigDataItem p)
    ]
    where 
      toValueJSON (     Nothing   ) = ("value", Null)
      toValueJSON (Just (UserConfigDataValueBoolean c)) = ("valueBoolean", toJSON c)
      toValueJSON (Just (UserConfigDataValueDecimal c)) = ("valueDecimal", toJSON c)
      toValueJSON (Just (UserConfigDataValueInteger c)) = ("valueInteger", toJSON c)
      toValueJSON (Just (UserConfigDataValueDate c)) = ("valueDate", toJSON c)
      toValueJSON (Just (UserConfigDataValueDateTime c)) = ("valueDateTime", toJSON c)
      toValueJSON (Just (UserConfigDataValueTime c)) = ("valueTime", toJSON c)
      toValueJSON (Just (UserConfigDataValueString c)) = ("valueString", toJSON c)
      toValueJSON (Just (UserConfigDataValueUri c)) = ("valueUri", toJSON c)
      toValueJSON (Just (UserConfigDataValueAttachment c)) = ("valueAttachment", toJSON c)
      toValueJSON (Just (UserConfigDataValueCoding c)) = ("valueCoding", toJSON c)
      toValueJSON (Just (UserConfigDataValueQuantity c)) = ("valueQuantity", toJSON c)
      toValueJSON (Just (UserConfigDataValueReference c)) = ("valueReference", toJSON c)
instance FromJSON UserConfigData where
  parseJSON = withObject "UserConfigData" $ \o -> do
        iid <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        value <- parseValue o
        item <- o .:? "item" .!= []
        return UserConfigData{
            userConfigDataAttrId = iid
          , userConfigDataExtension = extension
          , userConfigDataModifierExtension = modifierExtension
          , userConfigDataValue = value
          , userConfigDataItem = item
          }
    where 
      parseValue o = parseValueBoolean o <|> parseValueDecimal o <|> parseValueInteger o <|> parseValueDate o <|> parseValueDateTime o <|> parseValueTime o <|> parseValueString o <|> parseValueUri o <|> parseValueAttachment o <|> parseValueCoding o <|> parseValueQuantity o <|> parseValueReference o
      parseValueBoolean o = do
                has <- o .: "valueBoolean"
                return $ Just (UserConfigDataValueBoolean has)
      parseValueDecimal o = do
                has <- o .: "valueDecimal"
                return $ Just (UserConfigDataValueDecimal has)
      parseValueInteger o = do
                has <- o .: "valueInteger"
                return $ Just (UserConfigDataValueInteger has)
      parseValueDate o = do
                has <- o .: "valueDate"
                return $ Just (UserConfigDataValueDate has)
      parseValueDateTime o = do
                has <- o .: "valueDateTime"
                return $ Just (UserConfigDataValueDateTime has)
      parseValueTime o = do
                has <- o .: "valueTime"
                return $ Just (UserConfigDataValueTime has)
      parseValueString o = do
                has <- o .: "valueString"
                return $ Just (UserConfigDataValueString has)
      parseValueUri o = do
                has <- o .: "valueUri"
                return $ Just (UserConfigDataValueUri has)
      parseValueAttachment o = do
                has <- o .: "valueAttachment"
                return $ Just (UserConfigDataValueAttachment has)
      parseValueCoding o = do
                has <- o .: "valueCoding"
                return $ Just (UserConfigDataValueCoding has)
      parseValueQuantity o = do
                has <- o .: "valueQuantity"
                return $ Just (UserConfigDataValueQuantity has)
      parseValueReference o = do
                has <- o .: "valueReference"
                return $ Just (UserConfigDataValueReference has)
instance Xmlbf.ToXml UserConfigData where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (userConfigDataAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (userConfigDataExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (userConfigDataModifierExtension p))
             , toValueXml (userConfigDataValue p)
             , PropList "item" (fmap Xmlbf.toXml (userConfigDataItem p))
             ]
       where 
          toValueXml ( Nothing   ) = (OptVal "value" Nothing)
          toValueXml (Just (UserConfigDataValueBoolean v)) = Val   "valueBoolean" (toBoolean v)
          toValueXml (Just (UserConfigDataValueDecimal v)) = Val   "valueDecimal" (toDecimal v)
          toValueXml (Just (UserConfigDataValueInteger v)) = Val   "valueInteger" (toInt v)
          toValueXml (Just (UserConfigDataValueDate v)) = Val   "valueDate" (toDate v)
          toValueXml (Just (UserConfigDataValueDateTime v)) = Val   "valueDateTime" (toDateTime v)
          toValueXml (Just (UserConfigDataValueTime v)) = Val   "valueTime" (toTime v)
          toValueXml (Just (UserConfigDataValueString v)) = Val   "valueString" (toString v)
          toValueXml (Just (UserConfigDataValueUri v)) = Val   "valueUri" (toUri v)
          toValueXml (Just (UserConfigDataValueAttachment v)) = Prop  "valueAttachment" (HM.empty, Xmlbf.toXml v)
          toValueXml (Just (UserConfigDataValueCoding v)) = Prop  "valueCoding" (HM.empty, Xmlbf.toXml v)
          toValueXml (Just (UserConfigDataValueQuantity v)) = Prop  "valueQuantity" (HM.empty, Xmlbf.toXml v)
          toValueXml (Just (UserConfigDataValueReference v)) = Prop  "valueReference" (HM.empty, Xmlbf.toXml v)
instance Xmlbf.FromXml UserConfigData where
  fromXml = do
    iid <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    value <- fromValueXml
    item <- many     $ Xmlbf.pElement "item" Xmlbf.fromXml
    return UserConfigData {
            userConfigDataAttrId = iid
          , userConfigDataExtension = extension
          , userConfigDataModifierExtension = modifierExtension
          , userConfigDataValue = value
          , userConfigDataItem = item
          }

    where 
      fromValueXml = parseValueBoolean <|> parseValueDecimal <|> parseValueInteger <|> parseValueDate <|> parseValueDateTime <|> parseValueTime <|> parseValueString <|> parseValueUri <|> parseValueAttachment <|> parseValueCoding <|> parseValueQuantity <|> parseValueReference <|> pure Nothing
      parseValueBoolean = do
                has <- Xmlbf.pElement "valueBoolean" (Xmlbf.pAttr "value")
                return $ Just (UserConfigDataValueBoolean (     fromBoolean has))
      parseValueDecimal = do
                has <- Xmlbf.pElement "valueDecimal" (Xmlbf.pAttr "value")
                return $ Just (UserConfigDataValueDecimal (     fromDecimal has))
      parseValueInteger = do
                has <- Xmlbf.pElement "valueInteger" (Xmlbf.pAttr "value")
                return $ Just (UserConfigDataValueInteger (     fromInt has))
      parseValueDate = do
                has <- Xmlbf.pElement "valueDate" (Xmlbf.pAttr "value")
                return $ Just (UserConfigDataValueDate (     fromDate has))
      parseValueDateTime = do
                has <- Xmlbf.pElement "valueDateTime" (Xmlbf.pAttr "value")
                return $ Just (UserConfigDataValueDateTime (     fromDateTime has))
      parseValueTime = do
                has <- Xmlbf.pElement "valueTime" (Xmlbf.pAttr "value")
                return $ Just (UserConfigDataValueTime (     fromTime has))
      parseValueString = do
                has <- Xmlbf.pElement "valueString" (Xmlbf.pAttr "value")
                return $ Just (UserConfigDataValueString (     fromString has))
      parseValueUri = do
                has <- Xmlbf.pElement "valueUri" (Xmlbf.pAttr "value")
                return $ Just (UserConfigDataValueUri (     fromUri has))
      parseValueAttachment = do
                has <- Xmlbf.pElement "valueAttachment" Xmlbf.fromXml
                return $ Just (UserConfigDataValueAttachment (                      has))
      parseValueCoding = do
                has <- Xmlbf.pElement "valueCoding" Xmlbf.fromXml
                return $ Just (UserConfigDataValueCoding (                      has))
      parseValueQuantity = do
                has <- Xmlbf.pElement "valueQuantity" Xmlbf.fromXml
                return $ Just (UserConfigDataValueQuantity (                      has))
      parseValueReference = do
                has <- Xmlbf.pElement "valueReference" Xmlbf.fromXml
                return $ Just (UserConfigDataValueReference (                      has))




data UserConfigCard = UserConfigCard {
    userConfigCardAttrId :: Maybe Text
--  , userConfigCardExtension :: [Extension]
--  , userConfigCardModifierExtension :: [Extension]
  , userConfigCardCard :: Id
  , userConfigCardTitle :: Maybe Text
  , userConfigCardSubtitle :: Maybe Text
  , userConfigCardUser :: Text
  , userConfigCardModel :: Text
  , userConfigCardConfig :: [UserConfigItem]
  , userConfigCardData :: [UserConfigItem]
--  , userConfigCardContext :: [Text]
--  , userConfigCardParentt :: Maybe Text
--  , userConfigCardContext :: [Text]
  , userConfigCardIcon  :: Maybe Text
  , userConfigCardIconColor :: Maybe Text
  , userConfigCardPersistence :: Maybe Text
  , userConfigCardStatus :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON UserConfigCard where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "_id" .= toJSON (userConfigCardAttrId p)
    , "card" .= toJSON (userConfigCardCard p)
    , "title" .= toJSON (userConfigCardTitle p)
    , "subtitle" .= toJSON (userConfigCardSubtitle p)
    , "user" .= toJSON (userConfigCardUser p)
    , "model" .= toJSON (userConfigCardModel p)
    , "config" .= toJSON (userConfigCardConfig p)
    , "data" .= toJSON (userConfigCardData p)
    , "icon" .= toJSON (userConfigCardIcon p)
    , "iconColor" .= toJSON (userConfigCardIconColor p)
    , "persistence" .= toJSON (userConfigCardPersistence p)
    , "status" .= toJSON (userConfigCardStatus p)
    ]
instance FromJSON UserConfigCard where
  parseJSON = withObject "UserConfigCard" $ \o -> do
        iid <- o .:? "_id"
        c  <- o .:  "card"
        t  <- o .:? "title"
        st <- o .:? "subtitle"
        u  <- o .:  "user"
        m  <- o .:  "model"
        co <- o .:? "config" .!= []
        d  <- o .:? "data" .!= []
        i  <- o .:? "icon"
        ic <- o .:? "iconColor"
        p  <- o .:? "persistence"
        s  <- o .:? "status"
        return UserConfigCard{
            userConfigCardAttrId = iid
          , userConfigCardCard = c
          , userConfigCardTitle= t
          , userConfigCardSubtitle = st
          , userConfigCardUser= u
          , userConfigCardModel = m
          , userConfigCardConfig = co
          , userConfigCardData = d
          , userConfigCardIcon = i
          , userConfigCardIconColor = ic
          , userConfigCardPersistence = p
          , userConfigCardStatus = s
          }
instance Xmlbf.ToXml UserConfigCard where
  toXml p = concatMap toElement $
             [
               OptVal "_id"   (userConfigCardAttrId p)
             , Val     "card"     (toString (userConfigCardCard p))
             , OptVal  "title"    (fmap toString (userConfigCardTitle p))
             , OptVal  "subtitle" (fmap toString (userConfigCardSubtitle p))
             , Val     "user    " (toString (userConfigCardUser p))
             , Val     "model"    (toString (userConfigCardModel p))
             , PropList "config"  (fmap Xmlbf.toXml (userConfigCardConfig p))
             , PropList "data"    (fmap Xmlbf.toXml (userConfigCardData p))
             , OptVal  "icon"     (fmap toString (userConfigCardIcon p))
             , OptVal  "iconColor"   (fmap toString (userConfigCardIconColor p))
             , OptVal  "persistence" (fmap toString (userConfigCardPersistence p))
             , OptVal  "status"   (fmap toString (userConfigCardStatus p))
             ]
instance Xmlbf.FromXml UserConfigCard where
  fromXml = do
    iid <- optional $ Xmlbf.pAttr "id"
    c  <-            Xmlbf.pElement "card" (Xmlbf.pAttr "value")
    t  <- optional $ Xmlbf.pElement "title" (Xmlbf.pAttr "value")
    st <- optional $ Xmlbf.pElement "subtitle" (Xmlbf.pAttr "value")
    u  <-            Xmlbf.pElement "user" (Xmlbf.pAttr "value")
    m  <-            Xmlbf.pElement "model" (Xmlbf.pAttr "value")
    co <- many     $ Xmlbf.pElement "config" Xmlbf.fromXml
    d  <- many     $ Xmlbf.pElement "data" Xmlbf.fromXml
    i  <- optional $ Xmlbf.pElement "icon" (Xmlbf.pAttr "value")
    ic <- optional $ Xmlbf.pElement "iconColor" (Xmlbf.pAttr "value")
    p  <- optional $ Xmlbf.pElement "peristence" (Xmlbf.pAttr "value")
    s  <- optional $ Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    return UserConfigCard{
            userConfigCardAttrId = iid
          , userConfigCardCard = c
          , userConfigCardTitle= t
          , userConfigCardSubtitle = st
          , userConfigCardUser= u
          , userConfigCardModel = m
          , userConfigCardConfig = co
          , userConfigCardData = d
          , userConfigCardIcon = i
          , userConfigCardIconColor = ic
          , userConfigCardPersistence = p
          , userConfigCardStatus = s
          }

data UserConfigEditorTemplate = UserConfigEditorTemplate {
    userConfigEditorTemplateAttrId :: Maybe Text
--  , userConfigEditorTemplateExtension :: [Extension]
--  , userConfigEditorTemplateModifierExtension :: [Extension]
  , userConfigEditorTemplateName :: Text
  , userConfigEditorTemplateImage :: Maybe Text
  , userConfigEditorTemplateDescription:: Maybe Text
  , userConfigEditorTemplateTEI :: Narrative
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON UserConfigEditorTemplate where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "_id" .= toJSON (userConfigEditorTemplateAttrId p)
    , "name" .= toJSON (userConfigEditorTemplateName p)
    , "image" .= toJSON (userConfigEditorTemplateImage p)
    , "description" .= toJSON (userConfigEditorTemplateDescription p)
    , "tei" .= toJSON (userConfigEditorTemplateTEI p)
    ]
instance FromJSON UserConfigEditorTemplate where
  parseJSON = withObject "UserConfigEditorTemplate" $ \o -> do
        iid <- o .:? "_id"
        n  <- o .:  "name"
        i  <- o .:? "image"
        d  <- o .:? "description"
        di <- o .:  "tei"
        return UserConfigEditorTemplate{
            userConfigEditorTemplateAttrId = iid
          , userConfigEditorTemplateName= n
          , userConfigEditorTemplateImage= i
          , userConfigEditorTemplateDescription= d
          , userConfigEditorTemplateTEI= di
          }
instance Xmlbf.ToXml UserConfigEditorTemplate where
  toXml p = concatMap toElement $
             [
               OptVal "_id"    (userConfigEditorTemplateAttrId p)
             , Val     "name"  (toString (userConfigEditorTemplateName p))
             , OptVal  "image" (fmap toString (userConfigEditorTemplateImage p))
             , OptVal  "description" (fmap toString (userConfigEditorTemplateDescription p))
             , Prop    "tei"   (HM.empty,Xmlbf.toXml (userConfigEditorTemplateTEI p))
             ]
instance Xmlbf.FromXml UserConfigEditorTemplate where
  fromXml = do
    iid <- optional $ Xmlbf.pAttr "_id"
    n  <-            Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    i  <- optional $ Xmlbf.pElement "image" (Xmlbf.pAttr "value")
    d  <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    di <-            Xmlbf.pElement "tei" Xmlbf.fromXml
    return UserConfigEditorTemplate{
            userConfigEditorTemplateAttrId = iid
          , userConfigEditorTemplateName = n
          , userConfigEditorTemplateImage= i
          , userConfigEditorTemplateDescription= d
          , userConfigEditorTemplateTEI= di
          }


data UserConfig = UserConfig {
    userConfigId :: Maybe Id
  , userConfigMeta :: Maybe Meta
  , userConfigImplicitRules :: Maybe Uri
  , userConfigLanguage :: Maybe Language
--  , userConfigText :: Maybe Narrative
--    userConfigContained :: [ResourceContainer]
--  , userConfigExtension :: [Extension]
--  , userConfigModifierExtension :: [Extension]
  , userConfigIdentifier :: [Identifier]
  , userConfigActive :: Maybe Bool
  , userConfigSubject :: Maybe Reference
  , userConfigWelcome :: Maybe Text
  , userConfigWelcomeSub :: Maybe Text
  , userConfigAvatar :: Maybe Text
  , userConfigEmail       :: Maybe Text
  , userConfigDisplayName :: Maybe Text
  , userConfigTitle       :: Maybe Text
  , userConfigGender      :: Maybe AdministrativeGender
  , userConfigLocation    :: Maybe Reference
  , userConfigEducation   :: [Text]
  , userConfigProfession  :: Maybe Text
  , userConfigRegistered  :: Maybe Date
  , userConfigLastLogin   :: Maybe DateTime
  , userConfigLastRoom    :: Maybe Text
  , userConfigVerified    :: Maybe Bool
  , userConfigRoom        :: [Text]
  , userConfigWorkSet     :: [Reference]
  , userConfigCard        :: [UserConfigCard]
  , userConfigEditorTemplate :: [UserConfigEditorTemplate]
  }
  deriving stock (Eq, Generic, Show)
--

instance ToJSON UserConfig where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "UserConfig")
    , "id" .= toJSON (userConfigId p)
    , "meta" .= toJSON (userConfigMeta p)
    , "implicitRules" .= toJSON (userConfigImplicitRules p)
    , "language" .= toJSON (userConfigLanguage p)
    , "identifier" .= toJSON (userConfigIdentifier p)
    , "active" .= toJSON (userConfigActive p)
    , "subject" .= toJSON (userConfigSubject p)
    , "welcome" .= toJSON (userConfigWelcome p)
    , "welcome_sub" .= toJSON (userConfigWelcomeSub p)
    , "avatar" .= toJSON (userConfigAvatar p)
    , "email" .= toJSON (userConfigEmail p)
    , "displayName" .= toJSON (userConfigDisplayName p)
    , "title" .= toJSON (userConfigTitle p)
    , "gender" .= toJSON (userConfigGender p)
    , "location" .= toJSON (userConfigLocation p)
    , "education" .= toJSON (userConfigEducation  p)
    , "profession" .= toJSON (userConfigProfession p)
    , "registered" .= toJSON (userConfigRegistered p)
    , "lastLogin" .= toJSON (userConfigLastLogin  p)
    , "lastRoom"  .= toJSON (userConfigLastRoom p)
    , "verified" .= toJSON (userConfigVerified p)
    , "room" .= toJSON (userConfigRoom p)
    , "workSet" .= toJSON (userConfigWorkSet p)
    , "card" .= toJSON (userConfigCard p)
    , "editorTemplate" .= toJSON (userConfigEditorTemplate p)
    ]
instance FromJSON UserConfig where
  parseJSON = withObject "UserConfig" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "UserConfig" -> do
        iid <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language   <- o .:? "language"
        identifier <- o .:? "identifier" .!= []
        active     <- o .:? "active"
        subject    <- o .:? "subject"
        welcome    <- o .:? "welcome"
        welcomeSub <- o .:? "welcome_sub"
        avatar     <- o .:? "avatar"
        email      <- o .:? "email"
        displayName <- o .:? "displayName"
        title      <- o .:? "title"
        gender     <- o .:? "gender"
        location   <- o .:? "location"
        education  <- o .:? "education" .!= []
        profession <- o .:? "profession"
        registered <- o .:? "registered"
        lastLogin  <- o .:? "lastLogin"
        lastRoom   <- o .:? "lastRoom"
        verified   <- o .:? "verified"
        room       <- o .:? "room" .!= []
        workset    <- o .:? "workSet" .!= []
        card       <- o .:? "card" .!= []
        template   <- o .:? "editorTemplate" .!= []
        return UserConfig{
            userConfigId = iid
          , userConfigMeta = meta
          , userConfigImplicitRules = implicitRules
          , userConfigLanguage = language
          , userConfigIdentifier = identifier
          , userConfigActive   = active
          , userConfigSubject  = subject
          , userConfigWelcome  = welcome
          , userConfigWelcomeSub = welcomeSub
          , userConfigAvatar   = avatar
          , userConfigEmail = email
          , userConfigDisplayName = displayName
          , userConfigTitle = title
          , userConfigGender = gender
          , userConfigLocation = location
          , userConfigEducation  = education
          , userConfigProfession = profession
          , userConfigRegistered = registered
          , userConfigLastLogin  = lastLogin
          , userConfigLastRoom   = lastRoom
          , userConfigVerified = verified
          , userConfigRoom     = room
          , userConfigWorkSet  = workset
          , userConfigCard     = card
          , userConfigEditorTemplate = template
          }
      _ -> fail "not a UserConfig"
instance Xmlbf.ToXml UserConfig where
  toXml p = Xmlbf.element "UserConfig" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (userConfigId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (userConfigMeta p))
             , OptVal   "implicitRules" (fmap toUri (userConfigImplicitRules p))
             , OptVal   "language"    (fmap toLanguage (userConfigLanguage p))
             , PropList "identifier"  (fmap Xmlbf.toXml (userConfigIdentifier p))
             , OptVal   "active"      (fmap toBoolean   (userConfigActive p))
             , OptProp  "subject"     (fmap Xmlbf.toXml (userConfigSubject p))
             , OptVal   "welcome"     (fmap toString (userConfigWelcome p))
             , OptVal   "welcome_sub" (fmap toString (userConfigWelcomeSub p))
             , OptVal   "avatar"      (fmap toString (userConfigAvatar p))
             , OptVal   "email"       (fmap toString (userConfigEmail p))
             , OptVal   "displayName" (fmap toString (userConfigDisplayName p))
             , OptVal   "title"       (fmap toString (userConfigTitle p))
             , OptVal   "gender"      (fmap toAdministrativeGender (userConfigGender p))
             , OptProp  "location"    (fmap Xmlbf.toXml (userConfigLocation p))
             , ValList  "education"   (fmap toString (userConfigEducation  p))
             , OptVal   "profession"  (fmap toString (userConfigProfession p))
             , OptVal   "registered"  (fmap toDate (userConfigRegistered p))
             , OptVal   "lastLogin"   (fmap toDateTime (userConfigLastLogin p))
             , OptVal   "lastRoom"    (fmap toString   (userConfigLastRoom p))
             , OptVal   "verified"    (fmap toBoolean (userConfigVerified p))
             , ValList  "room"        (fmap toString (userConfigRoom p))
             , PropList "workSet"     (fmap Xmlbf.toXml (userConfigWorkSet p))
             , PropList "card"        (fmap Xmlbf.toXml (userConfigCard p))
             , PropList "editorTemplate" (fmap Xmlbf.toXml (userConfigEditorTemplate p))
             ]
instance Xmlbf.FromXml UserConfig where
  fromXml = do
    iid <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language   <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    identifier <- many     $ Xmlbf.pElement "identifier"  Xmlbf.fromXml
    active     <- optional $ Xmlbf.pElement "active" (Xmlbf.pAttr "value")
    subject    <- optional $ Xmlbf.pElement "subject" Xmlbf.fromXml
    welcome    <- optional $ Xmlbf.pElement "welcome" (Xmlbf.pAttr "value")
    welcomeSub <- optional $ Xmlbf.pElement "welcome_sub" (Xmlbf.pAttr "value")
    avatar     <- optional $ Xmlbf.pElement "avatar" (Xmlbf.pAttr "value")
    email      <- optional $ Xmlbf.pElement "email" (Xmlbf.pAttr "value")
    dispName   <- optional $ Xmlbf.pElement "displayName" (Xmlbf.pAttr "value")
    title      <- optional $ Xmlbf.pElement "title" (Xmlbf.pAttr "value")
    gender     <- optional $ Xmlbf.pElement "gender" (Xmlbf.pAttr "value")
    location   <- optional $ Xmlbf.pElement "location" Xmlbf.fromXml
    education  <- many     $ Xmlbf.pElement "education" (Xmlbf.pAttr "value")
    profession <- optional $ Xmlbf.pElement "profession" (Xmlbf.pAttr "value")
    registered <- optional $ Xmlbf.pElement "registered" (Xmlbf.pAttr "value")
    lastLogin  <- optional $ Xmlbf.pElement "lastLogin" (Xmlbf.pAttr "value")
    lastRoom   <- optional $ Xmlbf.pElement "lastRoom" (Xmlbf.pAttr "value")
    verified   <- optional $ Xmlbf.pElement "verified" (Xmlbf.pAttr "value")
    room       <- many     $ Xmlbf.pElement "room" (Xmlbf.pAttr "value")
    workset    <- many     $ Xmlbf.pElement "workSet" Xmlbf.fromXml
    card       <- many     $ Xmlbf.pElement "card"  Xmlbf.fromXml
    template   <- many     $ Xmlbf.pElement "template"  Xmlbf.fromXml
    return UserConfig {
            userConfigId = fmap fromId iid
          , userConfigMeta = meta
          , userConfigImplicitRules = fmap fromUri implicitRules
          , userConfigLanguage = fmap fromLanguage language
          , userConfigSubject  = subject
          , userConfigIdentifier = identifier
          , userConfigActive   = fmap fromBoolean active
          , userConfigWelcome  = welcome
          , userConfigWelcomeSub = welcomeSub
          , userConfigAvatar   = avatar
          , userConfigEmail = email
          , userConfigDisplayName = dispName
          , userConfigTitle = title
          , userConfigGender = fmap fromAdministrativeGender gender
          , userConfigLocation = location
          , userConfigEducation  = education
          , userConfigProfession = profession
          , userConfigRegistered = fmap fromDate registered
          , userConfigLastLogin  = fmap fromDateTime lastLogin
          , userConfigLastRoom   = lastRoom
          , userConfigVerified = fmap fromBoolean verified
          , userConfigRoom  = room
          , userConfigWorkSet = workset
          , userConfigCard  = card
          , userConfigEditorTemplate = template
          }
