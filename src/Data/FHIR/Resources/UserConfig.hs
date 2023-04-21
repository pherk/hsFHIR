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

module Data.FHIR.Resources.UserConfig where

import Data.Aeson  
import Data.Aeson.Types hiding (parseJSON)

import qualified Data.HashMap.Strict as HM
--import GHC.Generics
import GHC.TypeLits

import RIO
import qualified RIO.Vector as V
-- import Xmlbf
import           Data.FHIR.Datatypes
import           Data.FHIR.Datatypes.XML
import           Data.FHIR.Datatypes.XmlUtils
import           Data.FHIR.Resources.DomainResource
import qualified Xmlbf  as Xmlbf

type KV = (Text,Text)

instance Xmlbf.ToXml KV where
  toXml p = concatMap toElement $
             [
               Val "key"   (toString (fst p))
             , Val "value" (toString (snd p))
             ]
instance Xmlbf.FromXml KV where
  fromXml = do
    k  <- Xmlbf.pElement "key"   (Xmlbf.pAttr "value")
    v  <- Xmlbf.pElement "value" (Xmlbf.pAttr "value")
    return (k,v)
{-
instance ToJSON KVList where
  toJSON kvs = object [ k .= v | (k,v) <- kvs ]
-}

data UserConfigCard = UserConfigCard {
    userConfigCardAttrId :: Maybe Text
--  , userConfigCardExtension :: [Extension]
--  , userConfigCardModifierExtension :: [Extension]
  , userConfigCardCard :: Id
  , userConfigCardTitle :: Maybe Text
  , userConfigCardSubtitle :: Maybe Text
  , userConfigCardUser :: Text
  , userConfigCardModel :: Text
--  , userConfigCardList :: [List]
  , userConfigCardData :: [KV]
--  , userConfigCardContext :: [Text]
--  , userConfigCardParentt :: Maybe Text
--  , userConfigCardContext :: [Text]
  , userConfigCardIcon  :: Maybe Text
  , userConfigCardIconColor :: Maybe Text
  , userConfigCardPersistence :: Maybe Text
  , userConfigCardStatus :: Maybe Text
  }
  deriving (Eq, Show)

instance ToJSON UserConfigCard where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "_id" .= toJSON (userConfigCardAttrId p)
    , "id" .= toJSON (userConfigCardCard p)
    , "title" .= toJSON (userConfigCardTitle p)
    , "subtitle" .= toJSON (userConfigCardSubtitle p)
    , "user" .= toJSON (userConfigCardUser p)
    , "model" .= toJSON (userConfigCardModel p)
    , "data" .= toJSON (userConfigCardData p)
    , "icon" .= toJSON (userConfigCardIcon p)
    , "iconColor" .= toJSON (userConfigCardIconColor p)
    , "persistence" .= toJSON (userConfigCardPersistence p)
    , "status" .= toJSON (userConfigCardStatus p)
    ]
instance FromJSON UserConfigCard where
  parseJSON = withObject "UserConfigCard" $ \o -> do
        id <- o .:? "_id"
        c  <- o .:  "id"
        t  <- o .:? "title"
        st <- o .:? "subtitle"
        u  <- o .:  "user"
        m  <- o .:  "model"
        d  <- o .:? "data" .!= []
        i  <- o .:? "icon"
        ic <- o .:? "iconColor"
        p  <- o .:? "persistence"
        s  <- o .:? "status"
        return UserConfigCard{
            userConfigCardAttrId = id
          , userConfigCardCard = c
          , userConfigCardTitle= t
          , userConfigCardSubtitle = st
          , userConfigCardUser= u
          , userConfigCardModel = m
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
             , Val     "id"     (toString (userConfigCardCard p))
             , OptVal  "title"    (fmap toString (userConfigCardTitle p))
             , OptVal  "subtitle" (fmap toString (userConfigCardSubtitle p))
             , Val     "user    " (toString (userConfigCardUser p))
             , Val     "model"    (toString (userConfigCardModel p))
             , PropList "data"    (fmap Xmlbf.toXml (userConfigCardData p))
             , OptVal  "icon"     (fmap toString (userConfigCardIcon p))
             , OptVal  "iconColor"   (fmap toString (userConfigCardIconColor p))
             , OptVal  "persistence" (fmap toString (userConfigCardPersistence p))
             , OptVal  "status"   (fmap toString (userConfigCardStatus p))
             ]
instance Xmlbf.FromXml UserConfigCard where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    c  <-            Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    t  <- optional $ Xmlbf.pElement "title" (Xmlbf.pAttr "value")
    st <- optional $ Xmlbf.pElement "subtitle" (Xmlbf.pAttr "value")
    u  <-            Xmlbf.pElement "user" (Xmlbf.pAttr "value")
    m  <-            Xmlbf.pElement "model" (Xmlbf.pAttr "value")
    d  <- many     $ Xmlbf.pElement "data" Xmlbf.fromXml
    i  <- optional $ Xmlbf.pElement "icon" (Xmlbf.pAttr "value")
    ic <- optional $ Xmlbf.pElement "iconColor" (Xmlbf.pAttr "value")
    p  <- optional $ Xmlbf.pElement "peristence" (Xmlbf.pAttr "value")
    s  <- optional $ Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    return UserConfigCard{
            userConfigCardAttrId = id
          , userConfigCardCard = c
          , userConfigCardTitle= t
          , userConfigCardSubtitle = st
          , userConfigCardUser= u
          , userConfigCardModel = m
          , userConfigCardData = d
          , userConfigCardIcon = ic
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
  deriving (Eq, Show)

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
        id <- o .:? "_id"
        n  <- o .:  "name"
        i  <- o .:? "image"
        d  <- o .:? "description"
        di <- o .:  "tei"
        return UserConfigEditorTemplate{
            userConfigEditorTemplateAttrId = id
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
    id <- optional $ Xmlbf.pAttr "id"
    n  <-            Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    i  <- optional $ Xmlbf.pElement "image" (Xmlbf.pAttr "value")
    d  <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    di <-            Xmlbf.pElement "tei" Xmlbf.fromXml
    return UserConfigEditorTemplate{
            userConfigEditorTemplateAttrId = id
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
  , userConfigVerified    :: Maybe Boolean
  , userConfigRoom        :: [Text]
  , userConfigWorkSet     :: [Reference]
  , userConfigCard        :: [UserConfigCard]
  , userConfigEditorTemplate :: [UserConfigEditorTemplate]
  }
  deriving (Eq, Show)
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
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language   <- o .:? "language"
        identifier <- o .:  "identifier" .!= []
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
        education  <- o .:  "education" .!= []
        profession <- o .:? "profession"
        registered <- o .:? "registered"
        lastLogin  <- o .:? "lastLogin"
        lastRoom   <- o .:? "lastRoom"
        verified   <- o .:? "verified"
        room       <- o .:  "room" .!= []
        workset    <- o .:  "workSet" .!= []
        card       <- o .:  "card" .!= []
        template   <- o .:  "editorTemplate" .!= []
        return UserConfig{
            userConfigId = id
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
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
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
            userConfigId = fmap fromId id
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
