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

module Data.FHIR.Resources.FamilyHistory where

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

data FamiliyHistory = FamiliyHistory {
    familiyHistoryId :: Maybe Id
  , familiyHistoryMeta :: Maybe Meta
  , familiyHistoryImplicitRules :: Maybe Uri
  , familiyHistoryLanguage :: Maybe Language
  , familiyHistoryContentType :: MimeType
  , familiyHistorySecurityContext :: Maybe Reference
  }
  deriving (Eq, Show)
--

instance ToJSON FamiliyHistory where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "FamiliyHistory")
    ,  "id" .= toJSON (familiyHistoryId p)
    ,  "meta" .= toJSON (familiyHistoryMeta p)
    ,  "implicitRules" .= toJSON (familiyHistoryImplicitRules p)
    ,  "language" .= toJSON (familiyHistoryLanguage p)
    ,  "contentType" .= toJSON (familiyHistoryContentType p)
    ,  "securityContext" .= toJSON (familiyHistorySecurityContext p)
    ]
instance FromJSON FamiliyHistory where
  parseJSON = withObject "FamiliyHistory" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "FamiliyHistory" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        contentType <- o .:  "contentType"
        securityContext <- o .:? "securityContext"
        return FamiliyHistory{
            familiyHistoryId = id
          , familiyHistoryMeta = meta
          , familiyHistoryImplicitRules = implicitRules
          , familiyHistoryLanguage = language
          , familiyHistoryContentType = contentType
          , familiyHistorySecurityContext = securityContext
          }
      _ -> fail "not a FamiliyHistory"
instance Xmlbf.ToXml FamiliyHistory where
  toXml p = Xmlbf.element "FamiliyHistory" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (familiyHistoryId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (familiyHistoryMeta p))
             , OptVal   "implicitRules" (fmap toUri (familiyHistoryImplicitRules p))
             , OptVal   "language" (fmap toLanguage (familiyHistoryLanguage p))
             , Val      "contentType" (     toMimeType (familiyHistoryContentType p))
             , OptProp  "securityContext" (fmap Xmlbf.toXml (familiyHistorySecurityContext p))
             ]
instance Xmlbf.FromXml FamiliyHistory where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    contentType <-            Xmlbf.pElement "contentType" (Xmlbf.pAttr "value")
    securityContext <- optional $ Xmlbf.pElement "securityContext" Xmlbf.fromXml
    return FamiliyHistory {
            familiyHistoryId = fmap fromId id
          , familiyHistoryMeta = meta
          , familiyHistoryImplicitRules = fmap fromUri implicitRules
          , familiyHistoryLanguage = fmap fromLanguage language
          , familiyHistoryContentType =      fromMimeType contentType
          , familiyHistorySecurityContext = securityContext
          }




