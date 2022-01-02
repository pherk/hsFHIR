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
-- FHIR 4.0.0 Binary
--

module Data.FHIR.Resources.Binary where

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

data Binary = Binary {
    binaryId :: Maybe Id
  , binaryMeta :: Maybe Meta
  , binaryImplicitRules :: Maybe Uri
  , binaryLanguage :: Maybe Language
  , binaryContentType :: MimeType
  , binarySecurityContext :: Maybe Reference
  , binaryData :: Maybe Base64Binary
  }
  deriving (Eq, Show)
--

instance ToJSON Binary where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Binary")
    ,  "id" .= toJSON (binaryId p)
    ,  "meta" .= toJSON (binaryMeta p)
    ,  "implicitRules" .= toJSON (binaryImplicitRules p)
    ,  "language" .= toJSON (binaryLanguage p)
    ,  "contentType" .= toJSON (binaryContentType p)
    ,  "securityContext" .= toJSON (binarySecurityContext p)
    ,  "data" .= toJSON (binaryData p)
    ]
instance FromJSON Binary where
  parseJSON = withObject "Binary" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Binary" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        contentType <- o .:  "contentType"
        securityContext <- o .:? "securityContext"
        dt <- o .:? "data"
        return Binary{
            binaryId = id
          , binaryMeta = meta
          , binaryImplicitRules = implicitRules
          , binaryLanguage = language
          , binaryContentType = contentType
          , binarySecurityContext = securityContext
          , binaryData = dt
          }
      _ -> fail "not a Binary"
instance Xmlbf.ToXml Binary where
  toXml p = Xmlbf.element "Binary" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (binaryId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (binaryMeta p))
             , OptVal   "implicitRules" (fmap toUri (binaryImplicitRules p))
             , OptVal   "language" (fmap toLanguage (binaryLanguage p))
             , Val      "contentType" (     toMimeType (binaryContentType p))
             , OptProp  "securityContext" (fmap Xmlbf.toXml (binarySecurityContext p))
             , OptVal   "data" (fmap toBase64Binary (binaryData p))
             ]
instance Xmlbf.FromXml Binary where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    contentType <-            Xmlbf.pElement "contentType" (Xmlbf.pAttr "value")
    securityContext <- optional $ Xmlbf.pElement "securityContext" Xmlbf.fromXml
    dt <- optional $ Xmlbf.pElement "data" (Xmlbf.pAttr "value")
    return Binary {
            binaryId = fmap fromId id
          , binaryMeta = meta
          , binaryImplicitRules = fmap fromUri implicitRules
          , binaryLanguage = fmap fromLanguage language
          , binaryContentType =      fromMimeType contentType
          , binarySecurityContext = securityContext
          , binaryData = fmap fromBase64Binary dt
          }




