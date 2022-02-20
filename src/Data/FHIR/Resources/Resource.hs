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

module Data.FHIR.Resources.Resource where

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

{- 
 - folded in Bundle, Parameters, Binary and DomainResource
data Resource
    = Resource {
          resourceAttribs :: [AnyAttrib]
        , resourceId :: Maybe Id
        , resourceMeta :: Maybe Meta
        , resourceImplicitRules :: Maybe Uri
        , resourceLanguage :: Maybe Code}
    deriving (Eq, Show)

resourceToJSON d = [
          "id"                .= toJSON (resourceId d)
        , "meta"              .= toJSON (resourceMeta d)
        , "implicitRules"     .= toJSON (resourceImplicitRules d)
        , "language"          .= toJSON (resourceLanguage d)
        ]

parseResourceJSON o = do 
        id     <- o .:? "id"
        meta   <- o .:? "meta"
        ir     <- o .:? "implicitRules"
        l      <- o .:? "language"
        return Resource{
                  resourceAttribs=[], resourceId=id
                , resourceMeta=meta, resourceImplicitRules=ir
                , resourceLanguage=l}

instance FromJSON Resource where
    parseJSON = withObject "Resource" $ \o -> do 
        id     <- o .:? "id"
        meta   <- o .:? "meta"
        ir     <- o .:? "implicitRules"
        l      <- o .:? "language"
        return Resource{
                  resourceAttribs=[], resourceId=id
                , resourceMeta=meta, resourceImplicitRules=ir
                , resourceLanguage=l}

-}

