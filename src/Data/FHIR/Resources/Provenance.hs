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
-- FHIR 4.0.0 Provenance
--

module Data.FHIR.Resources.Provenance where

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

data ProvenanceOccurred
    = ProvenanceOccurredPeriod Period
    | ProvenanceOccurredDateTime DateTime
    deriving (Eq, Show)

data Provenance = Provenance {
    provenanceId :: Maybe Id
  , provenanceMeta :: Maybe Meta
  , provenanceImplicitRules :: Maybe Uri
  , provenanceLanguage :: Maybe Language
  , provenanceText :: Maybe Narrative
--    provenanceContained :: [ResourceContainer]
  , provenanceExtension :: [Extension]
  , provenanceModifierExtension :: [Extension]
  , provenanceTarget :: [Reference]
  , provenanceOccurred :: Maybe ProvenanceOccurred
  , provenanceRecorded :: Instant
  , provenancePolicy :: [Uri]
  , provenanceLocation :: Maybe Reference
  , provenanceReason :: [CodeableConcept]
  , provenanceActivity :: Maybe CodeableConcept
  , provenanceAgent :: [ProvenanceAgent]
  , provenanceEntity :: [ProvenanceEntity]
  , provenanceSignature :: [Signature]
  }
  deriving (Eq, Show)
--

instance ToJSON Provenance where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Provenance")
    ,  "id" .= toJSON (provenanceId p)
    ,  "meta" .= toJSON (provenanceMeta p)
    ,  "implicitRules" .= toJSON (provenanceImplicitRules p)
    ,  "language" .= toJSON (provenanceLanguage p)
    ,  "text" .= toJSON (provenanceText p)
--    , "contained" .= toJSON (provenanceContained p)
    ,  "extension" .= toJSON (provenanceExtension p)
    ,  "modifierExtension" .= toJSON (provenanceModifierExtension p)
    ,  "target" .= toJSON (provenanceTarget p)
    , toOccurredJSON (provenanceOccurred p)
    ,  "recorded" .= toJSON (provenanceRecorded p)
    ,  "policy" .= toJSON (provenancePolicy p)
    ,  "location" .= toJSON (provenanceLocation p)
    ,  "reason" .= toJSON (provenanceReason p)
    ,  "activity" .= toJSON (provenanceActivity p)
    ,  "agent" .= toJSON (provenanceAgent p)
    ,  "entity" .= toJSON (provenanceEntity p)
    ,  "signature" .= toJSON (provenanceSignature p)
    ]
    where 
      toOccurredJSON (     Nothing   ) = ("occurred", Null)
      toOccurredJSON (Just (ProvenanceOccurredPeriod c)) = ("occurred", toJSON c)
      toOccurredJSON (Just (ProvenanceOccurredDateTime c)) = ("occurred", toJSON c)
instance FromJSON Provenance where
  parseJSON = withObject "Provenance" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Provenance" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        target <- o .:? "target" .!= []
        occurred <- parseOccurred o
        recorded <- o .:  "recorded"
        policy <- o .:? "policy" .!= []
        location <- o .:? "location"
        reason <- o .:? "reason" .!= []
        activity <- o .:? "activity"
        agent <- o .:? "agent" .!= []
        entity <- o .:? "entity" .!= []
        signature <- o .:? "signature" .!= []
        return Provenance{
            provenanceId = id
          , provenanceMeta = meta
          , provenanceImplicitRules = implicitRules
          , provenanceLanguage = language
          , provenanceText = text
--          , provenanceContained = contained
          , provenanceExtension = extension
          , provenanceModifierExtension = modifierExtension
          , provenanceTarget = target
          , provenanceOccurred = occurred
          , provenanceRecorded = recorded
          , provenancePolicy = policy
          , provenanceLocation = location
          , provenanceReason = reason
          , provenanceActivity = activity
          , provenanceAgent = agent
          , provenanceEntity = entity
          , provenanceSignature = signature
          }
      _ -> fail "not a Provenance"
    where 
      parseOccurred o = parseOccurredPeriod o <|> parseOccurredDateTime o
      parseOccurredPeriod o = do
                has <- o .: "occurredPeriod"
                return $ Just (ProvenanceOccurredPeriod has)
      parseOccurredDateTime o = do
                has <- o .: "occurredDateTime"
                return $ Just (ProvenanceOccurredDateTime has)
instance Xmlbf.ToXml Provenance where
  toXml p = Xmlbf.element "Provenance" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (provenanceId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (provenanceMeta p))
             , OptVal   "implicitRules" (fmap toUri (provenanceImplicitRules p))
             , OptVal   "language" (fmap toLanguage (provenanceLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (provenanceText p))
--             , PropList "contained" (fmap Xmlbf.toXml (provenanceContained p))
             , PropList "extension" (fmap Xmlbf.toXml (provenanceExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (provenanceModifierExtension p))
             , PropList "target" (fmap Xmlbf.toXml (provenanceTarget p))
             , toOccurredXml (provenanceOccurred p)
             , Val      "recorded" (     toInstant (provenanceRecorded p))
             , ValList  "policy" (fmap toUri (provenancePolicy p))
             , OptProp  "location" (fmap Xmlbf.toXml (provenanceLocation p))
             , PropList "reason" (fmap Xmlbf.toXml (provenanceReason p))
             , OptProp  "activity" (fmap Xmlbf.toXml (provenanceActivity p))
             , PropList "agent" (fmap Xmlbf.toXml (provenanceAgent p))
             , PropList "entity" (fmap Xmlbf.toXml (provenanceEntity p))
             , PropList "signature" (fmap Xmlbf.toXml (provenanceSignature p))
             ]
          toOccurredXml ( Nothing   ) = (OptVal "occurred" Nothing)
          toOccurredXml (Just (ProvenanceOccurredPeriod p)) = Prop  "occurredPeriod" (HM.empty, Xmlbf.toXml p)
          toOccurredXml (Just (ProvenanceOccurredDateTime p)) = Val   "occurredDateTime" (toDateTime p)
instance Xmlbf.FromXml Provenance where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" Xmlbf.fromXml
--    contained <- many     $ Xmlbf.pElement "contained" Xmlbf.fromXml
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    target <- many     $ Xmlbf.pElement "target" Xmlbf.fromXml
    occurred <- fromOccurredXml
    recorded <-            Xmlbf.pElement "recorded" (Xmlbf.pAttr "value")
    policy <- many     $ Xmlbf.pElement "policy" (Xmlbf.pAttr "value")
    location <- optional $ Xmlbf.pElement "location" Xmlbf.fromXml
    reason <- many     $ Xmlbf.pElement "reason" Xmlbf.fromXml
    activity <- optional $ Xmlbf.pElement "activity" Xmlbf.fromXml
    agent <- many     $ Xmlbf.pElement "agent" Xmlbf.fromXml
    entity <- many     $ Xmlbf.pElement "entity" Xmlbf.fromXml
    signature <- many     $ Xmlbf.pElement "signature" Xmlbf.fromXml
    return Provenance {
            provenanceId = fmap fromId id
          , provenanceMeta = meta
          , provenanceImplicitRules = fmap fromUri implicitRules
          , provenanceLanguage = fmap fromLanguage language
          , provenanceText = text
--          , provenanceContained = contained
          , provenanceExtension = extension
          , provenanceModifierExtension = modifierExtension
          , provenanceTarget = target
          , provenanceOccurred = occurred
          , provenanceRecorded =      fromInstant recorded
          , provenancePolicy = fmap fromUri policy
          , provenanceLocation = location
          , provenanceReason = reason
          , provenanceActivity = activity
          , provenanceAgent = agent
          , provenanceEntity = entity
          , provenanceSignature = signature
          }

    where 
      fromOccurredXml = parseOccurredPeriod <|> parseOccurredDateTime <|> pure Nothing
      parseOccurredPeriod = do
                has <- Xmlbf.pElement "occurredPeriod" Xmlbf.fromXml
                return $ Just (ProvenanceOccurredPeriod (                      has))
      parseOccurredDateTime = do
                has <- Xmlbf.pElement "occurredDateTime" (Xmlbf.pAttr "value")
                return $ Just (ProvenanceOccurredDateTime (     toDateTime has))


data ProvenanceEntityRole
    = PERDerivation
    | PERRevision
    | PERQuotation
    | PERSource
    | PERRemoval
  deriving (Eq, Show)

instance ToJSON ProvenanceEntityRole where
    toJSON PERDerivation = String "derivation"
    toJSON PERRevision = String "revision"
    toJSON PERQuotation = String "quotation"
    toJSON PERSource = String "source"
    toJSON PERRemoval = String "removal"
instance FromJSON ProvenanceEntityRole where
    parseJSON "derivation" = return PERDerivation
    parseJSON "revision" = return PERRevision
    parseJSON "quotation" = return PERQuotation
    parseJSON "source" = return PERSource
    parseJSON "removal" = return PERRemoval

toProvenanceEntityRole PERDerivation = "derivation"
toProvenanceEntityRole PERRevision = "revision"
toProvenanceEntityRole PERQuotation = "quotation"
toProvenanceEntityRole PERSource = "source"
toProvenanceEntityRole PERRemoval = "removal"
fromProvenanceEntityRole "derivation" = PERDerivation
fromProvenanceEntityRole "revision" = PERRevision
fromProvenanceEntityRole "quotation" = PERQuotation
fromProvenanceEntityRole "source" = PERSource
fromProvenanceEntityRole "removal" = PERRemoval


data ProvenanceEntity = ProvenanceEntity {
    provenanceEntityAttrId :: Maybe Text
  , provenanceEntityExtension :: [Extension]
  , provenanceEntityModifierExtension :: [Extension]
  , provenanceEntityRole :: ProvenanceEntityRole
  , provenanceEntityWhat :: Reference
  , provenanceEntityAgent :: [ProvenanceAgent]
  }
  deriving (Eq, Show)
--

instance ToJSON ProvenanceEntity where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (provenanceEntityAttrId p)
    ,  "extension" .= toJSON (provenanceEntityExtension p)
    ,  "modifierExtension" .= toJSON (provenanceEntityModifierExtension p)
    ,  "role" .= toJSON (provenanceEntityRole p)
    ,  "what" .= toJSON (provenanceEntityWhat p)
    ,  "agent" .= toJSON (provenanceEntityAgent p)
    ]
instance FromJSON ProvenanceEntity where
  parseJSON = withObject "ProvenanceEntity" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        role <- o .:  "role"
        what <- o .:  "what"
        agent <- o .:? "agent" .!= []
        return ProvenanceEntity{
            provenanceEntityAttrId = id
          , provenanceEntityExtension = extension
          , provenanceEntityModifierExtension = modifierExtension
          , provenanceEntityRole = role
          , provenanceEntityWhat = what
          , provenanceEntityAgent = agent
          }
instance Xmlbf.ToXml ProvenanceEntity where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (provenanceEntityAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (provenanceEntityExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (provenanceEntityModifierExtension p))
             , Val      "role" (     toProvenanceEntityRole (provenanceEntityRole p))
             , Prop     "what" (HM.empty, Xmlbf.toXml (provenanceEntityWhat p))
             , PropList "agent" (fmap Xmlbf.toXml (provenanceEntityAgent p))
             ]
instance Xmlbf.FromXml ProvenanceEntity where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    role <-            Xmlbf.pElement "role" (Xmlbf.pAttr "value")
    what <-            Xmlbf.pElement "what" Xmlbf.fromXml
    agent <- many     $ Xmlbf.pElement "agent" Xmlbf.fromXml
    return ProvenanceEntity {
            provenanceEntityAttrId = id
          , provenanceEntityExtension = extension
          , provenanceEntityModifierExtension = modifierExtension
          , provenanceEntityRole =      fromProvenanceEntityRole role
          , provenanceEntityWhat = what
          , provenanceEntityAgent = agent
          }



data ProvenanceAgent = ProvenanceAgent {
    provenanceAgentAttrId :: Maybe Text
  , provenanceAgentExtension :: [Extension]
  , provenanceAgentModifierExtension :: [Extension]
  , provenanceAgentType :: Maybe CodeableConcept
  , provenanceAgentRole :: [CodeableConcept]
  , provenanceAgentWho :: Reference
  , provenanceAgentOnBehalfOf :: Maybe Reference
  }
  deriving (Eq, Show)
--

instance ToJSON ProvenanceAgent where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (provenanceAgentAttrId p)
    ,  "extension" .= toJSON (provenanceAgentExtension p)
    ,  "modifierExtension" .= toJSON (provenanceAgentModifierExtension p)
    ,  "type" .= toJSON (provenanceAgentType p)
    ,  "role" .= toJSON (provenanceAgentRole p)
    ,  "who" .= toJSON (provenanceAgentWho p)
    ,  "onBehalfOf" .= toJSON (provenanceAgentOnBehalfOf p)
    ]
instance FromJSON ProvenanceAgent where
  parseJSON = withObject "ProvenanceAgent" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        ty <- o .:? "type"
        role <- o .:? "role" .!= []
        who <- o .:  "who"
        onBehalfOf <- o .:? "onBehalfOf"
        return ProvenanceAgent{
            provenanceAgentAttrId = id
          , provenanceAgentExtension = extension
          , provenanceAgentModifierExtension = modifierExtension
          , provenanceAgentType = ty
          , provenanceAgentRole = role
          , provenanceAgentWho = who
          , provenanceAgentOnBehalfOf = onBehalfOf
          }
instance Xmlbf.ToXml ProvenanceAgent where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (provenanceAgentAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (provenanceAgentExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (provenanceAgentModifierExtension p))
             , OptProp  "type" (fmap Xmlbf.toXml (provenanceAgentType p))
             , PropList "role" (fmap Xmlbf.toXml (provenanceAgentRole p))
             , Prop     "who" (HM.empty, Xmlbf.toXml (provenanceAgentWho p))
             , OptProp  "onBehalfOf" (fmap Xmlbf.toXml (provenanceAgentOnBehalfOf p))
             ]
instance Xmlbf.FromXml ProvenanceAgent where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    ty <- optional $ Xmlbf.pElement "type" Xmlbf.fromXml
    role <- many     $ Xmlbf.pElement "role" Xmlbf.fromXml
    who <-            Xmlbf.pElement "who" Xmlbf.fromXml
    onBehalfOf <- optional $ Xmlbf.pElement "onBehalfOf" Xmlbf.fromXml
    return ProvenanceAgent {
            provenanceAgentAttrId = id
          , provenanceAgentExtension = extension
          , provenanceAgentModifierExtension = modifierExtension
          , provenanceAgentType = ty
          , provenanceAgentRole = role
          , provenanceAgentWho = who
          , provenanceAgentOnBehalfOf = onBehalfOf
          }




