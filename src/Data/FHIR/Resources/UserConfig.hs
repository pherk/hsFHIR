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


data UserConfig = UserConfig {
    userConfigId :: Maybe Id
  , userConfigMeta :: Maybe Meta
  , userConfigImplicitRules :: Maybe Uri
  , userConfigLanguage :: Maybe Language
--  , userConfigText :: Maybe Narrative
--    encounterContained :: [ResourceContainer]
--  , userConfigExtension :: [Extension]
--  , userCOnfigModifierExtension :: [Extension]
  , userConfigIdentifier :: [Identifier]
  , userConfigActive :: Maybe Bool
  , userConfigSubject :: Maybe Reference
  , userConfigWelcome :: Maybe Text
  , userConfigWelcomeSub :: Maybe Text
  , userConfigAvatar :: Maybe Text
  , userConfigCard :: [UserConfigCard]
  }
  deriving (Eq, Show)
--

instance ToJSON UserConfig where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "UserConfig")
    ,  "id" .= toJSON (userConfigId p)
    ,  "meta" .= toJSON (userConfigMeta p)
    ,  "implicitRules" .= toJSON (userConfigImplicitRules p)
    ,  "language" .= toJSON (userConfigLanguage p)
    ,  "identifier" .= toJSON (userConfigIdentifier p)
    ,  "active" .= toJSON (userConfigActive p)
    ,  "subject" .= toJSON (userConfigSubject p)
    ,  "welcome" .= toJSON (userConfigWelcome p)
    ,  "welcome_sub" .= toJSON (userConfigWelcomeSub p)
    ,  "avatar" .= toJSON (userConfigAvatar p)
    ,  "card" .= toJSON (userConfigCard p)
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
        card       <- o .:  "card" .!= []
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
          , userConfigCard     = card
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
             , PropList "card"        (fmap Xmlbf.toXml (userConfigCard p))
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
    card       <- many     $ Xmlbf.pElement "card"  Xmlbf.fromXml
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
          , userConfigCard     = card
          }




