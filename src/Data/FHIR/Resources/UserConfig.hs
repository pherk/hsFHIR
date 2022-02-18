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

data UserConfig = UserConfig {
    userConfigId :: Maybe Id
  , userConfigMeta :: Maybe Meta
  , userConfigImplicitRules :: Maybe Uri
  , userConfigLanguage :: Maybe Language
  , userConfigContentType :: MimeType
  , userConfigSecurityContext :: Maybe Reference
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
    ,  "contentType" .= toJSON (userConfigContentType p)
    ,  "securityContext" .= toJSON (userConfigSecurityContext p)
    ]
instance FromJSON UserConfig where
  parseJSON = withObject "UserConfig" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "UserConfig" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        contentType <- o .:  "contentType"
        securityContext <- o .:? "securityContext"
        return UserConfig{
            userConfigId = id
          , userConfigMeta = meta
          , userConfigImplicitRules = implicitRules
          , userConfigLanguage = language
          , userConfigContentType = contentType
          , userConfigSecurityContext = securityContext
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
             , OptVal   "language" (fmap toLanguage (userConfigLanguage p))
             , Val      "contentType" (     toMimeType (userConfigContentType p))
             , OptProp  "securityContext" (fmap Xmlbf.toXml (userConfigSecurityContext p))
             ]
instance Xmlbf.FromXml UserConfig where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    contentType <-            Xmlbf.pElement "contentType" (Xmlbf.pAttr "value")
    securityContext <- optional $ Xmlbf.pElement "securityContext" Xmlbf.fromXml
    return UserConfig {
            userConfigId = fmap fromId id
          , userConfigMeta = meta
          , userConfigImplicitRules = fmap fromUri implicitRules
          , userConfigLanguage = fmap fromLanguage language
          , userConfigContentType =      fromMimeType contentType
          , userConfigSecurityContext = securityContext
          }




