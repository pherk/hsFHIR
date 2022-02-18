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

module Data.FHIR.Resources.ICalendar where

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


data ICalendar = ICalendar {
    iCalendarId :: Maybe Id
  , iCalendarMeta :: Maybe Meta
  , iCalendarImplicitRules :: Maybe Uri
  , iCalendarLanguage :: Maybe Language
  , iCalendarContentType :: MimeType
  , iCalendarSecurityContext :: Maybe Reference
  }
  deriving (Eq, Show)
--

instance ToJSON ICalendar where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "ICalendar")
    ,  "id" .= toJSON (iCalendarId p)
    ,  "meta" .= toJSON (iCalendarMeta p)
    ,  "implicitRules" .= toJSON (iCalendarImplicitRules p)
    ,  "language" .= toJSON (iCalendarLanguage p)
    ,  "contentType" .= toJSON (iCalendarContentType p)
    ,  "securityContext" .= toJSON (iCalendarSecurityContext p)
    ]
instance FromJSON ICalendar where
  parseJSON = withObject "ICalendar" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "ICalendar" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        contentType <- o .:  "contentType"
        securityContext <- o .:? "securityContext"
        return ICalendar{
            iCalendarId = id
          , iCalendarMeta = meta
          , iCalendarImplicitRules = implicitRules
          , iCalendarLanguage = language
          , iCalendarContentType = contentType
          , iCalendarSecurityContext = securityContext
          }
      _ -> fail "not a ICalendar"
instance Xmlbf.ToXml ICalendar where
  toXml p = Xmlbf.element "ICalendar" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (iCalendarId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (iCalendarMeta p))
             , OptVal   "implicitRules" (fmap toUri (iCalendarImplicitRules p))
             , OptVal   "language" (fmap toLanguage (iCalendarLanguage p))
             , Val      "contentType" (     toMimeType (iCalendarContentType p))
             , OptProp  "securityContext" (fmap Xmlbf.toXml (iCalendarSecurityContext p))
             ]
instance Xmlbf.FromXml ICalendar where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    contentType <-            Xmlbf.pElement "contentType" (Xmlbf.pAttr "value")
    securityContext <- optional $ Xmlbf.pElement "securityContext" Xmlbf.fromXml
    return ICalendar {
            iCalendarId = fmap fromId id
          , iCalendarMeta = meta
          , iCalendarImplicitRules = fmap fromUri implicitRules
          , iCalendarLanguage = fmap fromLanguage language
          , iCalendarContentType =      fromMimeType contentType
          , iCalendarSecurityContext = securityContext
          }




