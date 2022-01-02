{-# LANGUAGE NoImplicitPrelude #-}

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

module Data.FHIR.Resources.DomainResource where

import Data.Aeson  
import Data.Aeson.Types hiding (parseJSON)

import GHC.TypeLits

import RIO
import qualified RIO.HashMap as HM
import qualified RIO.Text as T

import Data.FHIR.Datatypes
import Data.FHIR.Datatypes.XML
import Data.FHIR.Datatypes.XmlUtils
import qualified Xmlbf as Xmlbf


data DomainResource
    = DomainResource {
          domainResourceFilter :: [Text]
        , domainResourceAttribs :: HM.HashMap Text Text
        , domainResourceId :: Maybe Id
        , domainResourceMeta :: Maybe Meta
        , domainResourceImplicitRules :: Maybe Uri
        , domainResourceLanguage :: Maybe Code
        , domainResourceText :: Maybe Narrative
        , domainResourceExtension :: [Extension]
        , domainResourceModifierExtension :: [Extension]}
    deriving (Eq, Show)

toDomainResourceJSON d = [
          "id"                .= toJSON (domainResourceId d)
        , "meta"              .= toJSON (domainResourceMeta d)
        , "implicitRules"     .= toJSON (domainResourceImplicitRules d)
        , "language"          .= toJSON (domainResourceLanguage d)
        , "text"              .= toJSON (domainResourceText d)
        , "extension"         .= toJSON (domainResourceExtension d)
        , "modifierExtension" .= toJSON (domainResourceModifierExtension d)
        ]

instance FromJSON DomainResource where
    parseJSON = withObject "DomainResource" $ \o -> do 
        id     <- o .:? "id"
        meta   <- o .:? "meta"
        ir     <- o .:? "implicitRules"
        l      <- o .:? "language"
        t      <- o .:? "text"
        e      <- o .:? "extension" .!= []
        me     <- o .:? "modifierExtension" .!= []
        return DomainResource{
                  domainResourceFilter = []
                , domainResourceAttribs=HM.empty
                , domainResourceId=id
                , domainResourceMeta=meta
                , domainResourceImplicitRules=ir
                , domainResourceLanguage=l
                , domainResourceText=t
                , domainResourceExtension=e
                , domainResourceModifierExtension=me
                }

parseDomainResourceJSON o = do 
        id     <- o .:? "id"
        meta   <- o .:? "meta"
        ir     <- o .:? "implicitRules"
        l      <- o .:? "language"
        t      <- o .:? "text"
        e      <- o .:? "extension" .!= []
        me     <- o .:? "modifierExtension" .!= []
        return DomainResource{
                  domainResourceFilter = []
                , domainResourceAttribs=HM.empty
                , domainResourceId=id
                , domainResourceMeta=meta
                , domainResourceImplicitRules=ir
                , domainResourceLanguage=l
                , domainResourceText=t
                , domainResourceExtension=e
                , domainResourceModifierExtension=me
                }

toDomainResourceXml d = [
          OptVal   "id"                 (domainResourceId d)
        , OptProp  "meta"               (fmap Xmlbf.toXml (domainResourceMeta d))
        , OptVal   "implicitRules"      (domainResourceImplicitRules d)
        , OptVal   "language"           (domainResourceLanguage d)
        , OptProp  "text"               (fmap Xmlbf.toXml (domainResourceText d))
        , PropList "extension"          (fmap Xmlbf.toXml (domainResourceExtension d))
        , PropList "modifierExtension"  (fmap Xmlbf.toXml (domainResourceModifierExtension d))
        ]

fromDomainResourceXml = do 
        as     <- Xmlbf.pAttrs
        id     <- optional $ Xmlbf.pElement "id"            (Xmlbf.pAttr "value")
        meta   <- optional $ Xmlbf.pElement "meta"          Xmlbf.fromXml
        ir     <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
        l      <- optional $ Xmlbf.pElement "language"      (Xmlbf.pAttr "value")
        t      <- optional $ Xmlbf.pElement "text"          Xmlbf.fromXml
        e      <- many     $ Xmlbf.pElement "extension"     Xmlbf.fromXml
        me     <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
        return DomainResource{
                  domainResourceFilter = []
                , domainResourceAttribs=as
                , domainResourceId=id
                , domainResourceMeta=meta
                , domainResourceImplicitRules=ir
                , domainResourceLanguage=l
                , domainResourceText=t
                , domainResourceExtension=e
                , domainResourceModifierExtension=me
                }

