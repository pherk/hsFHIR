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
-- FHIR 4.0.0 Basic
--

module Data.FHIR.Resources.Basic where

import Data.Aeson
import Data.Aeson.Types hiding (parseJSON)

import qualified Data.HashMap.Strict as HM
import GHC.TypeLits

import RIO
import qualified RIO.Vector as V
import           Data.FHIR.Datatypes
import           Data.FHIR.Datatypes.XML
import           Data.FHIR.Datatypes.XmlUtils
import           Data.FHIR.Resources.DomainResource
import qualified Xmlbf  as Xmlbf


data Basic = Basic {
    basicId :: Maybe Id
  , basicMeta :: Maybe Meta
  , basicImplicitRules :: Maybe Uri
  , basicLanguage :: Maybe Language
  , basicText :: Maybe Narrative
  , basicContained :: [ResourceContainer]
  , basicExtension :: [Extension]
  , basicModifierExtension :: [Extension]
  , basicIdentifier :: [Identifier]
  , basicCode :: CodeableConcept
  , basicSubject :: Maybe Reference
  , basicCreated :: Maybe Date
  , basicAuthor :: Maybe Reference
  }
  deriving (Eq, Show)
--

instance ToJSON Basic where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Basic")
    , "id" .= toJSON (basicId p)
    , "meta" .= toJSON (basicMeta p)
    , "implicitRules" .= toJSON (basicImplicitRules p)
    , "language" .= toJSON (basicLanguage p)
    , "text" .= toJSON (basicText p)
    , "contained" .= toJSON (basicContained p)
    , "extension" .= toJSON (basicExtension p)
    , "modifierExtension" .= toJSON (basicModifierExtension p)
    , "identifier" .= toJSON (basicIdentifier p)
    , "code" .= toJSON (basicCode p)
    , "subject" .= toJSON (basicSubject p)
    , "created" .= toJSON (basicCreated p)
    , "author" .= toJSON (basicAuthor p)
    ]
instance FromJSON Basic where
  parseJSON = withObject "Basic" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Basic" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        code <- o .:  "code"
        subject <- o .:? "subject"
        created <- o .:? "created"
        author <- o .:? "author"
        return Basic{
            basicId = id
          , basicMeta = meta
          , basicImplicitRules = implicitRules
          , basicLanguage = language
          , basicText = text
          , basicContained = contained
          , basicExtension = extension
          , basicModifierExtension = modifierExtension
          , basicIdentifier = identifier
          , basicCode = code
          , basicSubject = subject
          , basicCreated = created
          , basicAuthor = author
          }
      _ -> fail "not a Basic"
--

instance Xmlbf.ToXml Basic where
  toXml p = Xmlbf.pElement "Basic" as cs
    where as = HM.fromList $ catMaybes $
            fmap toAttr [
                Val "xmlns" "http://hl7.org/fhir"
                -- OptVal "xml:id" (domainResourceAttribs ps)              ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (basicId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (basicMeta p))
             , OptVal   "implicitRules" (fmap toUri (basicImplicitRules p))
             , OptVal   "language" (fmap toLanguagetype (basicLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (basicText p))
             , PropList "contained" (fmap Xmlbf.toXml (basicContained p))
             , PropList "extension" (fmap Xmlbf.toXml (basicExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (basicModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (basicIdentifier p))
             , Prop     "code" (HM.empty, Xmlbf.toXml (basicCode p))
             , OptProp  "subject" (fmap Xmlbf.toXml (basicSubject p))
             , OptVal   "created" (fmap toDate (basicCreated p))
             , OptProp  "author" (fmap Xmlbf.toXml (basicAuthor p))
             ]
instance Xmlbf.FromXml Basic where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" Xmlbf.fromXml
    contained <- many     $ Xmlbf.pElement "contained" Xmlbf.fromXml
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    identifier <- many     $ Xmlbf.pElement "identifier" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" Xmlbf.fromXml
    subject <- optional $ Xmlbf.pElement "subject" Xmlbf.fromXml
    created <- optional $ Xmlbf.pElement "created" (Xmlbf.pAttr "value")
    author <- optional $ Xmlbf.pElement "author" Xmlbf.fromXml
    return Basic {
            basicId = id
          , basicMeta = meta
          , basicImplicitRules = implicitRules
          , basicLanguage = language
          , basicText = text
          , basicContained = contained
          , basicExtension = extension
          , basicModifierExtension = modifierExtension
          , basicIdentifier = identifier
          , basicCode = code
          , basicSubject = subject
          , basicCreated = created
          , basicAuthor = author
          }


