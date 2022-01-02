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
-- FHIR 4.0.0 Bundle
--

module Data.FHIR.Resources.Bundle where

import Data.Aeson
import Data.Aeson.Types hiding (parseJSON)

import qualified Data.HashMap.Strict as HM
import GHC.TypeLits

import RIO                  hiding(fromString)
import qualified RIO.Vector as V
import           Data.FHIR.Datatypes
import           Data.FHIR.Datatypes.XML
import           Data.FHIR.Datatypes.XmlUtils
import           Data.FHIR.Resources.ResourceContainer
import qualified Xmlbf  as Xmlbf

data BundleType
    = BTDocument
    | BTMessage
    | BTTransaction
    | BTTransactionResponse
    | BTBatch
    | BTBatchResponse
    | BTHistory
    | BTSearchset
    | BTCollection
  deriving (Eq, Show)

instance ToJSON BundleType where
    toJSON BTDocument = String "document"
    toJSON BTMessage = String "message"
    toJSON BTTransaction = String "transaction"
    toJSON BTTransactionResponse = String "transaction-response"
    toJSON BTBatch = String "batch"
    toJSON BTBatchResponse = String "batch-response"
    toJSON BTHistory = String "history"
    toJSON BTSearchset = String "searchset"
    toJSON BTCollection = String "collection"
instance FromJSON BundleType where
    parseJSON "document" = return BTDocument
    parseJSON "message" = return BTMessage
    parseJSON "transaction" = return BTTransaction
    parseJSON "transaction-response" = return BTTransactionResponse
    parseJSON "batch" = return BTBatch
    parseJSON "batch-response" = return BTBatchResponse
    parseJSON "history" = return BTHistory
    parseJSON "searchset" = return BTSearchset
    parseJSON "collection" = return BTCollection

toBundleType BTDocument = "document"
toBundleType BTMessage = "message"
toBundleType BTTransaction = "transaction"
toBundleType BTTransactionResponse = "transaction-response"
toBundleType BTBatch = "batch"
toBundleType BTBatchResponse = "batch-response"
toBundleType BTHistory = "history"
toBundleType BTSearchset = "searchset"
toBundleType BTCollection = "collection"
fromBundleType "document" = BTDocument
fromBundleType "message" = BTMessage
fromBundleType "transaction" = BTTransaction
fromBundleType "transaction-response" = BTTransactionResponse
fromBundleType "batch" = BTBatch
fromBundleType "batch-response" = BTBatchResponse
fromBundleType "history" = BTHistory
fromBundleType "searchset" = BTSearchset
fromBundleType "collection" = BTCollection


data Bundle = Bundle {
    bundleId :: Maybe Id
  , bundleMeta :: Maybe Meta
  , bundleImplicitRules :: Maybe Uri
  , bundleLanguage :: Maybe Language
  , bundleIdentifier :: Maybe Identifier
  , bundleType :: BundleType
  , bundleTimestamp :: Maybe Instant
  , bundleTotal :: Maybe UnsignedInt
  , bundleLink :: [BundleLink]
  , bundleEntry :: [BundleEntry]
  , bundleSignature :: Maybe Signature
  } deriving (Eq, Show)
--

instance ToJSON Bundle where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Bundle")
    ,  "id" .= toJSON (bundleId p)
    ,  "meta" .= toJSON (bundleMeta p)
    ,  "implicitRules" .= toJSON (bundleImplicitRules p)
    ,  "language" .= toJSON (bundleLanguage p)
    ,  "identifier" .= toJSON (bundleIdentifier p)
    ,  "type" .= toJSON (bundleType p)
    ,  "timestamp" .= toJSON (bundleTimestamp p)
    ,  "total" .= toJSON (bundleTotal p)
    ,  "link" .= toJSON (bundleLink p)
    ,  "entry" .= toJSON (bundleEntry p)
    ,  "signature" .= toJSON (bundleSignature p)
    ]
instance FromJSON Bundle where
  parseJSON = withObject "Bundle" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Bundle" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        identifier <- o .:? "identifier"
        ty <- o .:  "type"
        timestamp <- o .:? "timestamp"
        total <- o .:? "total"
        link <- o .:? "link" .!= []
        entry <- o .:? "entry" .!= []
        signature <- o .:? "signature"
        return Bundle{
            bundleId = id
          , bundleMeta = meta
          , bundleImplicitRules = implicitRules
          , bundleLanguage = language
          , bundleIdentifier = identifier
          , bundleType = ty
          , bundleTimestamp = timestamp
          , bundleTotal = total
          , bundleLink = link
          , bundleEntry = entry
          , bundleSignature = signature
          }
      _ -> fail "not a Bundle"
instance Xmlbf.ToXml Bundle where
  toXml p = Xmlbf.element "Bundle" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (bundleId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (bundleMeta p))
             , OptVal   "implicitRules" (fmap toUri (bundleImplicitRules p))
             , OptVal   "language" (fmap toLanguage (bundleLanguage p))
             , OptProp  "identifier" (fmap Xmlbf.toXml (bundleIdentifier p))
             , Val      "type" (     toBundleType (bundleType p))
             , OptVal   "timestamp" (fmap toInstant (bundleTimestamp p))
             , OptVal   "total" (fmap toUnsignedInt (bundleTotal p))
             , PropList "link" (fmap Xmlbf.toXml (bundleLink p))
             , PropList "entry" (fmap Xmlbf.toXml (bundleEntry p))
             , OptProp  "signature" (fmap Xmlbf.toXml (bundleSignature p))
             ]
instance Xmlbf.FromXml Bundle where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    identifier <- optional $ Xmlbf.pElement "identifier" Xmlbf.fromXml
    ty <-            Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    timestamp <- optional $ Xmlbf.pElement "timestamp" (Xmlbf.pAttr "value")
    total <- optional $ Xmlbf.pElement "total" (Xmlbf.pAttr "value")
    link <- many     $ Xmlbf.pElement "link" Xmlbf.fromXml
    entry <- many     $ Xmlbf.pElement "entry" Xmlbf.fromXml
    signature <- optional $ Xmlbf.pElement "signature" Xmlbf.fromXml
    return Bundle {
            bundleId = fmap fromId id
          , bundleMeta = meta
          , bundleImplicitRules = fmap fromUri implicitRules
          , bundleLanguage = fmap fromLanguage language
          , bundleIdentifier = identifier
          , bundleType =      fromBundleType ty
          , bundleTimestamp = fmap fromInstant timestamp
          , bundleTotal = fmap fromUnsignedInt total
          , bundleLink = link
          , bundleEntry = entry
          , bundleSignature = signature
          }



data BundleLink = BundleLink {
    bundleLinkAttrId :: Maybe Text
  , bundleLinkExtension :: [Extension]
  , bundleLinkModifierExtension :: [Extension]
  , bundleLinkRelation :: Text
  , bundleLinkUrl :: Uri
  } deriving (Eq, Show)
--

instance ToJSON BundleLink where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (bundleLinkAttrId p)
    ,  "extension" .= toJSON (bundleLinkExtension p)
    ,  "modifierExtension" .= toJSON (bundleLinkModifierExtension p)
    ,  "relation" .= toJSON (bundleLinkRelation p)
    ,  "url" .= toJSON (bundleLinkUrl p)
    ]
instance FromJSON BundleLink where
  parseJSON = withObject "BundleLink" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        relation <- o .:  "relation"
        url <- o .:  "url"
        return BundleLink{
            bundleLinkAttrId = id
          , bundleLinkExtension = extension
          , bundleLinkModifierExtension = modifierExtension
          , bundleLinkRelation = relation
          , bundleLinkUrl = url
          }
instance Xmlbf.ToXml BundleLink where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (bundleLinkAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (bundleLinkExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (bundleLinkModifierExtension p))
             , Val      "relation" (     toString (bundleLinkRelation p))
             , Val      "url" (     toUri (bundleLinkUrl p))
             ]
instance Xmlbf.FromXml BundleLink where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    relation <-            Xmlbf.pElement "relation" (Xmlbf.pAttr "value")
    url <-            Xmlbf.pElement "url" (Xmlbf.pAttr "value")
    return BundleLink {
            bundleLinkAttrId = id
          , bundleLinkExtension = extension
          , bundleLinkModifierExtension = modifierExtension
          , bundleLinkRelation =      fromString relation
          , bundleLinkUrl =      fromUri url
          }



data BundleEntry = BundleEntry {
    bundleEntryAttrId :: Maybe Text
  , bundleEntryExtension :: [Extension]
  , bundleEntryModifierExtension :: [Extension]
  , bundleEntryLink :: [BundleLink]
  , bundleEntryFullUrl :: Maybe Uri
  , bundleEntryResource :: Maybe DomainResourceC
  , bundleEntrySearch :: Maybe BundleSearch
  , bundleEntryRequest :: Maybe BundleRequest
  , bundleEntryResponse :: Maybe BundleResponse
  } deriving (Eq, Show)
--

instance ToJSON BundleEntry where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (bundleEntryAttrId p)
    ,  "extension" .= toJSON (bundleEntryExtension p)
    ,  "modifierExtension" .= toJSON (bundleEntryModifierExtension p)
    ,  "link" .= toJSON (bundleEntryLink p)
    ,  "fullUrl" .= toJSON (bundleEntryFullUrl p)
    ,  "resource" .= toJSON (bundleEntryResource p)
    ,  "search" .= toJSON (bundleEntrySearch p)
    ,  "request" .= toJSON (bundleEntryRequest p)
    ,  "response" .= toJSON (bundleEntryResponse p)
    ]
instance FromJSON BundleEntry where
  parseJSON = withObject "BundleEntry" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        link <- o .:? "link" .!= []
        fullUrl <- o .:? "fullUrl"
        resource <- o .:? "resource"
        search <- o .:? "search"
        request <- o .:? "request"
        response <- o .:? "response"
        return BundleEntry{
            bundleEntryAttrId = id
          , bundleEntryExtension = extension
          , bundleEntryModifierExtension = modifierExtension
          , bundleEntryLink = link
          , bundleEntryFullUrl = fullUrl
          , bundleEntryResource = resource
          , bundleEntrySearch = search
          , bundleEntryRequest = request
          , bundleEntryResponse = response
          }
instance Xmlbf.ToXml BundleEntry where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (bundleEntryAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (bundleEntryExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (bundleEntryModifierExtension p))
             , PropList "link" (fmap Xmlbf.toXml (bundleEntryLink p))
             , OptVal   "fullUrl" (fmap toUri (bundleEntryFullUrl p))
             , OptProp  "resource" (fmap Xmlbf.toXml (bundleEntryResource p))
             , OptProp  "search" (fmap Xmlbf.toXml (bundleEntrySearch p))
             , OptProp  "request" (fmap Xmlbf.toXml (bundleEntryRequest p))
             , OptProp  "response" (fmap Xmlbf.toXml (bundleEntryResponse p))
             ]
instance Xmlbf.FromXml BundleEntry where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    link <- many     $ Xmlbf.pElement "link" Xmlbf.fromXml
    fullUrl <- optional $ Xmlbf.pElement "fullUrl" (Xmlbf.pAttr "value")
    resource <- optional $ Xmlbf.pElement "resource" Xmlbf.fromXml
    search <- optional $ Xmlbf.pElement "search" Xmlbf.fromXml
    request <- optional $ Xmlbf.pElement "request" Xmlbf.fromXml
    response <- optional $ Xmlbf.pElement "response" Xmlbf.fromXml
    return BundleEntry {
            bundleEntryAttrId = id
          , bundleEntryExtension = extension
          , bundleEntryModifierExtension = modifierExtension
          , bundleEntryLink = link
          , bundleEntryFullUrl = fmap fromUri fullUrl
          , bundleEntryResource = resource
          , bundleEntrySearch = search
          , bundleEntryRequest = request
          , bundleEntryResponse = response
          }



data BundleRequestMethod
    = BRMGET
    | BRMHEAD
    | BRMPOST
    | BRMPUT
    | BRMDELETE
    | BRMPATCH
  deriving (Eq, Show)

instance ToJSON BundleRequestMethod where
    toJSON BRMGET = String "GET"
    toJSON BRMHEAD = String "HEAD"
    toJSON BRMPOST = String "POST"
    toJSON BRMPUT = String "PUT"
    toJSON BRMDELETE = String "DELETE"
    toJSON BRMPATCH = String "PATCH"
instance FromJSON BundleRequestMethod where
    parseJSON "GET" = return BRMGET
    parseJSON "HEAD" = return BRMHEAD
    parseJSON "POST" = return BRMPOST
    parseJSON "PUT" = return BRMPUT
    parseJSON "DELETE" = return BRMDELETE
    parseJSON "PATCH" = return BRMPATCH

toBundleRequestMethod BRMGET = "GET"
toBundleRequestMethod BRMHEAD = "HEAD"
toBundleRequestMethod BRMPOST = "POST"
toBundleRequestMethod BRMPUT = "PUT"
toBundleRequestMethod BRMDELETE = "DELETE"
toBundleRequestMethod BRMPATCH = "PATCH"
fromBundleRequestMethod "GET" = BRMGET
fromBundleRequestMethod "HEAD" = BRMHEAD
fromBundleRequestMethod "POST" = BRMPOST
fromBundleRequestMethod "PUT" = BRMPUT
fromBundleRequestMethod "DELETE" = BRMDELETE
fromBundleRequestMethod "PATCH" = BRMPATCH


data BundleRequest = BundleRequest {
    bundleRequestAttrId :: Maybe Text
  , bundleRequestExtension :: [Extension]
  , bundleRequestModifierExtension :: [Extension]
  , bundleRequestMethod :: BundleRequestMethod
  , bundleRequestUrl :: Uri
  , bundleRequestIfNoneMatch :: Maybe Text
  , bundleRequestIfModifiedSince :: Maybe Instant
  , bundleRequestIfMatch :: Maybe Text
  , bundleRequestIfNoneExist :: Maybe Text
  } deriving (Eq, Show)
--

instance ToJSON BundleRequest where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (bundleRequestAttrId p)
    ,  "extension" .= toJSON (bundleRequestExtension p)
    ,  "modifierExtension" .= toJSON (bundleRequestModifierExtension p)
    ,  "method" .= toJSON (bundleRequestMethod p)
    ,  "url" .= toJSON (bundleRequestUrl p)
    ,  "ifNoneMatch" .= toJSON (bundleRequestIfNoneMatch p)
    ,  "ifModifiedSince" .= toJSON (bundleRequestIfModifiedSince p)
    ,  "ifMatch" .= toJSON (bundleRequestIfMatch p)
    ,  "ifNoneExist" .= toJSON (bundleRequestIfNoneExist p)
    ]
instance FromJSON BundleRequest where
  parseJSON = withObject "BundleRequest" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        method <- o .:  "method"
        url <- o .:  "url"
        ifNoneMatch <- o .:? "ifNoneMatch"
        ifModifiedSince <- o .:? "ifModifiedSince"
        ifMatch <- o .:? "ifMatch"
        ifNoneExist <- o .:? "ifNoneExist"
        return BundleRequest{
            bundleRequestAttrId = id
          , bundleRequestExtension = extension
          , bundleRequestModifierExtension = modifierExtension
          , bundleRequestMethod = method
          , bundleRequestUrl = url
          , bundleRequestIfNoneMatch = ifNoneMatch
          , bundleRequestIfModifiedSince = ifModifiedSince
          , bundleRequestIfMatch = ifMatch
          , bundleRequestIfNoneExist = ifNoneExist
          }
instance Xmlbf.ToXml BundleRequest where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (bundleRequestAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (bundleRequestExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (bundleRequestModifierExtension p))
             , Val      "method" (     toBundleRequestMethod (bundleRequestMethod p))
             , Val      "url" (     toUri (bundleRequestUrl p))
             , OptVal   "ifNoneMatch" (fmap toString (bundleRequestIfNoneMatch p))
             , OptVal   "ifModifiedSince" (fmap toInstant (bundleRequestIfModifiedSince p))
             , OptVal   "ifMatch" (fmap toString (bundleRequestIfMatch p))
             , OptVal   "ifNoneExist" (fmap toString (bundleRequestIfNoneExist p))
             ]
instance Xmlbf.FromXml BundleRequest where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    method <-            Xmlbf.pElement "method" (Xmlbf.pAttr "value")
    url <-            Xmlbf.pElement "url" (Xmlbf.pAttr "value")
    ifNoneMatch <- optional $ Xmlbf.pElement "ifNoneMatch" (Xmlbf.pAttr "value")
    ifModifiedSince <- optional $ Xmlbf.pElement "ifModifiedSince" (Xmlbf.pAttr "value")
    ifMatch <- optional $ Xmlbf.pElement "ifMatch" (Xmlbf.pAttr "value")
    ifNoneExist <- optional $ Xmlbf.pElement "ifNoneExist" (Xmlbf.pAttr "value")
    return BundleRequest {
            bundleRequestAttrId = id
          , bundleRequestExtension = extension
          , bundleRequestModifierExtension = modifierExtension
          , bundleRequestMethod =      fromBundleRequestMethod method
          , bundleRequestUrl =      fromUri url
          , bundleRequestIfNoneMatch = fmap fromString ifNoneMatch
          , bundleRequestIfModifiedSince = fmap fromInstant ifModifiedSince
          , bundleRequestIfMatch = fmap fromString ifMatch
          , bundleRequestIfNoneExist = fmap fromString ifNoneExist
          }



data BundleResponse = BundleResponse {
    bundleResponseAttrId :: Maybe Text
  , bundleResponseExtension :: [Extension]
  , bundleResponseModifierExtension :: [Extension]
  , bundleResponseStatus :: Text
  , bundleResponseLocation :: Maybe Uri
  , bundleResponseEtag :: Maybe Text
  , bundleResponseLastModified :: Maybe Instant
  , bundleResponseOutcome :: Maybe OperationOutcome
  } deriving (Eq, Show)
--

instance ToJSON BundleResponse where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (bundleResponseAttrId p)
    ,  "extension" .= toJSON (bundleResponseExtension p)
    ,  "modifierExtension" .= toJSON (bundleResponseModifierExtension p)
    ,  "status" .= toJSON (bundleResponseStatus p)
    ,  "location" .= toJSON (bundleResponseLocation p)
    ,  "etag" .= toJSON (bundleResponseEtag p)
    ,  "lastModified" .= toJSON (bundleResponseLastModified p)
    ,  "outcome" .= toJSON (bundleResponseOutcome p)
    ]
instance FromJSON BundleResponse where
  parseJSON = withObject "BundleResponse" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        status <- o .:  "status"
        location <- o .:? "location"
        etag <- o .:? "etag"
        lastModified <- o .:? "lastModified"
        outcome <- o .:? "outcome"
        return BundleResponse{
            bundleResponseAttrId = id
          , bundleResponseExtension = extension
          , bundleResponseModifierExtension = modifierExtension
          , bundleResponseStatus = status
          , bundleResponseLocation = location
          , bundleResponseEtag = etag
          , bundleResponseLastModified = lastModified
          , bundleResponseOutcome = outcome
          }
instance Xmlbf.ToXml BundleResponse where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (bundleResponseAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (bundleResponseExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (bundleResponseModifierExtension p))
             , Val      "status" (     toString (bundleResponseStatus p))
             , OptVal   "location" (fmap toUri (bundleResponseLocation p))
             , OptVal   "etag" (fmap toString (bundleResponseEtag p))
             , OptVal   "lastModified" (fmap toInstant (bundleResponseLastModified p))
             , OptProp  "outcome" (fmap Xmlbf.toXml (bundleResponseOutcome p))
             ]
instance Xmlbf.FromXml BundleResponse where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    location <- optional $ Xmlbf.pElement "location" (Xmlbf.pAttr "value")
    etag <- optional $ Xmlbf.pElement "etag" (Xmlbf.pAttr "value")
    lastModified <- optional $ Xmlbf.pElement "lastModified" (Xmlbf.pAttr "value")
    outcome <- optional $ Xmlbf.pElement "outcome" Xmlbf.fromXml
    return BundleResponse {
            bundleResponseAttrId = id
          , bundleResponseExtension = extension
          , bundleResponseModifierExtension = modifierExtension
          , bundleResponseStatus =      fromString status
          , bundleResponseLocation = fmap fromUri location
          , bundleResponseEtag = fmap fromString etag
          , bundleResponseLastModified = fmap fromInstant lastModified
          , bundleResponseOutcome = outcome
          }



data BundleSearchMode
    = BSMMatch
    | BSMInclude
    | BSMOutcome
  deriving (Eq, Show)

instance ToJSON BundleSearchMode where
    toJSON BSMMatch = String "match"
    toJSON BSMInclude = String "include"
    toJSON BSMOutcome = String "outcome"
instance FromJSON BundleSearchMode where
    parseJSON "match" = return BSMMatch
    parseJSON "include" = return BSMInclude
    parseJSON "outcome" = return BSMOutcome

toBundleSearchMode BSMMatch = "match"
toBundleSearchMode BSMInclude = "include"
toBundleSearchMode BSMOutcome = "outcome"
fromBundleSearchMode "match" = BSMMatch
fromBundleSearchMode "include" = BSMInclude
fromBundleSearchMode "outcome" = BSMOutcome


data BundleSearch = BundleSearch {
    bundleSearchAttrId :: Maybe Text
  , bundleSearchExtension :: [Extension]
  , bundleSearchModifierExtension :: [Extension]
  , bundleSearchMode :: Maybe BundleSearchMode
  , bundleSearchScore :: Maybe Decimal
  } deriving (Eq, Show)
--

instance ToJSON BundleSearch where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (bundleSearchAttrId p)
    ,  "extension" .= toJSON (bundleSearchExtension p)
    ,  "modifierExtension" .= toJSON (bundleSearchModifierExtension p)
    ,  "mode" .= toJSON (bundleSearchMode p)
    ,  "score" .= toJSON (bundleSearchScore p)
    ]
instance FromJSON BundleSearch where
  parseJSON = withObject "BundleSearch" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        mode <- o .:? "mode"
        score <- o .:? "score"
        return BundleSearch{
            bundleSearchAttrId = id
          , bundleSearchExtension = extension
          , bundleSearchModifierExtension = modifierExtension
          , bundleSearchMode = mode
          , bundleSearchScore = score
          }
instance Xmlbf.ToXml BundleSearch where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (bundleSearchAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (bundleSearchExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (bundleSearchModifierExtension p))
             , OptVal   "mode" (fmap toBundleSearchMode (bundleSearchMode p))
             , OptVal   "score" (fmap toDecimal (bundleSearchScore p))
             ]
instance Xmlbf.FromXml BundleSearch where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    mode <- optional $ Xmlbf.pElement "mode" (Xmlbf.pAttr "value")
    score <- optional $ Xmlbf.pElement "score" (Xmlbf.pAttr "value")
    return BundleSearch {
            bundleSearchAttrId = id
          , bundleSearchExtension = extension
          , bundleSearchModifierExtension = modifierExtension
          , bundleSearchMode = fmap fromBundleSearchMode mode
          , bundleSearchScore = fmap fromDecimal score
          }




