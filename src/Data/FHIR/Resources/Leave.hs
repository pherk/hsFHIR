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

module Data.FHIR.Resources.Leave where

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

data Leave = Leave {
    leaveId :: Maybe Id
  , leaveMeta :: Maybe Meta
  , leaveImplicitRules :: Maybe Uri
  , leaveLanguage :: Maybe Language
  , leaveContentType :: MimeType
  , leaveSecurityContext :: Maybe Reference
  }
  deriving (Eq, Show)
--

instance ToJSON Leave where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Leave")
    ,  "id" .= toJSON (leaveId p)
    ,  "meta" .= toJSON (leaveMeta p)
    ,  "implicitRules" .= toJSON (leaveImplicitRules p)
    ,  "language" .= toJSON (leaveLanguage p)
    ,  "contentType" .= toJSON (leaveContentType p)
    ,  "securityContext" .= toJSON (leaveSecurityContext p)
    ]
instance FromJSON Leave where
  parseJSON = withObject "Leave" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Leave" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        contentType <- o .:  "contentType"
        securityContext <- o .:? "securityContext"
        return Leave{
            leaveId = id
          , leaveMeta = meta
          , leaveImplicitRules = implicitRules
          , leaveLanguage = language
          , leaveContentType = contentType
          , leaveSecurityContext = securityContext
          }
      _ -> fail "not a Leave"
instance Xmlbf.ToXml Leave where
  toXml p = Xmlbf.element "Leave" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (leaveId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (leaveMeta p))
             , OptVal   "implicitRules" (fmap toUri (leaveImplicitRules p))
             , OptVal   "language" (fmap toLanguage (leaveLanguage p))
             , Val      "contentType" (     toMimeType (leaveContentType p))
             , OptProp  "securityContext" (fmap Xmlbf.toXml (leaveSecurityContext p))
             ]
instance Xmlbf.FromXml Leave where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    contentType <-            Xmlbf.pElement "contentType" (Xmlbf.pAttr "value")
    securityContext <- optional $ Xmlbf.pElement "securityContext" Xmlbf.fromXml
    return Leave {
            leaveId = fmap fromId id
          , leaveMeta = meta
          , leaveImplicitRules = fmap fromUri implicitRules
          , leaveLanguage = fmap fromLanguage language
          , leaveContentType =      fromMimeType contentType
          , leaveSecurityContext = securityContext
          }




