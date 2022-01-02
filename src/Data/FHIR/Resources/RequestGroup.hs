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
-- FHIR 4.0.0 RequestGroup
--

module Data.FHIR.Resources.RequestGroup where

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

data RequestGroupStatus
    = RGSDraft
    | RGSActive
    | RGSOnHold
    | RGSRevoked
    | RGSCompleted
    | RGSEnteredInError
    | RGSUnknown
  deriving (Eq, Show)

instance ToJSON RequestGroupStatus where
    toJSON RGSDraft = String "draft"
    toJSON RGSActive = String "active"
    toJSON RGSOnHold = String "on-hold"
    toJSON RGSRevoked = String "revoked"
    toJSON RGSCompleted = String "completed"
    toJSON RGSEnteredInError = String "entered-in-error"
    toJSON RGSUnknown = String "unknown"
instance FromJSON RequestGroupStatus where
    parseJSON "draft" = return RGSDraft
    parseJSON "active" = return RGSActive
    parseJSON "on-hold" = return RGSOnHold
    parseJSON "revoked" = return RGSRevoked
    parseJSON "completed" = return RGSCompleted
    parseJSON "entered-in-error" = return RGSEnteredInError
    parseJSON "unknown" = return RGSUnknown

toRequestGroupStatus RGSDraft = "draft"
toRequestGroupStatus RGSActive = "active"
toRequestGroupStatus RGSOnHold = "on-hold"
toRequestGroupStatus RGSRevoked = "revoked"
toRequestGroupStatus RGSCompleted = "completed"
toRequestGroupStatus RGSEnteredInError = "entered-in-error"
toRequestGroupStatus RGSUnknown = "unknown"
fromRequestGroupStatus "draft" = RGSDraft
fromRequestGroupStatus "active" = RGSActive
fromRequestGroupStatus "on-hold" = RGSOnHold
fromRequestGroupStatus "revoked" = RGSRevoked
fromRequestGroupStatus "completed" = RGSCompleted
fromRequestGroupStatus "entered-in-error" = RGSEnteredInError
fromRequestGroupStatus "unknown" = RGSUnknown


data RequestGroupIntent
    = RGIProposal
    | RGIPlan
    | RGIDirective
    | RGIOrder
    | RGIOriginalOrder
    | RGIReflexOrder
    | RGIFillerOrder
    | RGIInstanceOrder
    | RGIOption
  deriving (Eq, Show)

instance ToJSON RequestGroupIntent where
    toJSON RGIProposal = String "proposal"
    toJSON RGIPlan = String "plan"
    toJSON RGIDirective = String "directive"
    toJSON RGIOrder = String "order"
    toJSON RGIOriginalOrder = String "original-order"
    toJSON RGIReflexOrder = String "reflex-order"
    toJSON RGIFillerOrder = String "filler-order"
    toJSON RGIInstanceOrder = String "instance-order"
    toJSON RGIOption = String "option"
instance FromJSON RequestGroupIntent where
    parseJSON "proposal" = return RGIProposal
    parseJSON "plan" = return RGIPlan
    parseJSON "directive" = return RGIDirective
    parseJSON "order" = return RGIOrder
    parseJSON "original-order" = return RGIOriginalOrder
    parseJSON "reflex-order" = return RGIReflexOrder
    parseJSON "filler-order" = return RGIFillerOrder
    parseJSON "instance-order" = return RGIInstanceOrder
    parseJSON "option" = return RGIOption

toRequestGroupIntent RGIProposal = "proposal"
toRequestGroupIntent RGIPlan = "plan"
toRequestGroupIntent RGIDirective = "directive"
toRequestGroupIntent RGIOrder = "order"
toRequestGroupIntent RGIOriginalOrder = "original-order"
toRequestGroupIntent RGIReflexOrder = "reflex-order"
toRequestGroupIntent RGIFillerOrder = "filler-order"
toRequestGroupIntent RGIInstanceOrder = "instance-order"
toRequestGroupIntent RGIOption = "option"
fromRequestGroupIntent "proposal" = RGIProposal
fromRequestGroupIntent "plan" = RGIPlan
fromRequestGroupIntent "directive" = RGIDirective
fromRequestGroupIntent "order" = RGIOrder
fromRequestGroupIntent "original-order" = RGIOriginalOrder
fromRequestGroupIntent "reflex-order" = RGIReflexOrder
fromRequestGroupIntent "filler-order" = RGIFillerOrder
fromRequestGroupIntent "instance-order" = RGIInstanceOrder
fromRequestGroupIntent "option" = RGIOption


data RequestGroupPriority
    = RGPRoutine
    | RGPUrgent
    | RGPAsap
    | RGPStat
  deriving (Eq, Show)

instance ToJSON RequestGroupPriority where
    toJSON RGPRoutine = String "routine"
    toJSON RGPUrgent = String "urgent"
    toJSON RGPAsap = String "asap"
    toJSON RGPStat = String "stat"
instance FromJSON RequestGroupPriority where
    parseJSON "routine" = return RGPRoutine
    parseJSON "urgent" = return RGPUrgent
    parseJSON "asap" = return RGPAsap
    parseJSON "stat" = return RGPStat

toRequestGroupPriority RGPRoutine = "routine"
toRequestGroupPriority RGPUrgent = "urgent"
toRequestGroupPriority RGPAsap = "asap"
toRequestGroupPriority RGPStat = "stat"
fromRequestGroupPriority "routine" = RGPRoutine
fromRequestGroupPriority "urgent" = RGPUrgent
fromRequestGroupPriority "asap" = RGPAsap
fromRequestGroupPriority "stat" = RGPStat


data RequestGroup = RequestGroup {
    requestGroupId :: Maybe Id
  , requestGroupMeta :: Maybe Meta
  , requestGroupImplicitRules :: Maybe Uri
  , requestGroupLanguage :: Maybe Language
  , requestGroupText :: Maybe Narrative
--    requestGroupContained :: [ResourceContainer]
  , requestGroupExtension :: [Extension]
  , requestGroupModifierExtension :: [Extension]
  , requestGroupIdentifier :: [Identifier]
  , requestGroupInstantiatesCanonical :: [Canonical]
  , requestGroupInstantiatesUri :: [Uri]
  , requestGroupBasedOn :: [Reference]
  , requestGroupReplaces :: [Reference]
  , requestGroupGroupIdentifier :: Maybe Identifier
  , requestGroupStatus :: RequestGroupStatus
  , requestGroupIntent :: RequestGroupIntent
  , requestGroupPriority :: Maybe RequestGroupPriority
  , requestGroupCode :: Maybe CodeableConcept
  , requestGroupSubject :: Maybe Reference
  , requestGroupEncounter :: Maybe Reference
  , requestGroupAuthoredOn :: Maybe DateTime
  , requestGroupAuthor :: Maybe Reference
  , requestGroupReasonCode :: [CodeableConcept]
  , requestGroupReasonReference :: [Reference]
  , requestGroupNote :: [Annotation]
  , requestGroupAction :: [RequestGroupAction]
  }
--

instance ToJSON RequestGroup where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "RequestGroup")
    ,  "id" .= toJSON (requestGroupId p)
    ,  "meta" .= toJSON (requestGroupMeta p)
    ,  "implicitRules" .= toJSON (requestGroupImplicitRules p)
    ,  "language" .= toJSON (requestGroupLanguage p)
    ,  "text" .= toJSON (requestGroupText p)
--    , "contained" .= toJSON (requestGroupContained p)
    ,  "extension" .= toJSON (requestGroupExtension p)
    ,  "modifierExtension" .= toJSON (requestGroupModifierExtension p)
    ,  "identifier" .= toJSON (requestGroupIdentifier p)
    ,  "instantiatesCanonical" .= toJSON (requestGroupInstantiatesCanonical p)
    ,  "instantiatesUri" .= toJSON (requestGroupInstantiatesUri p)
    ,  "basedOn" .= toJSON (requestGroupBasedOn p)
    ,  "replaces" .= toJSON (requestGroupReplaces p)
    ,  "groupIdentifier" .= toJSON (requestGroupGroupIdentifier p)
    ,  "status" .= toJSON (requestGroupStatus p)
    ,  "intent" .= toJSON (requestGroupIntent p)
    ,  "priority" .= toJSON (requestGroupPriority p)
    ,  "code" .= toJSON (requestGroupCode p)
    ,  "subject" .= toJSON (requestGroupSubject p)
    ,  "encounter" .= toJSON (requestGroupEncounter p)
    ,  "authoredOn" .= toJSON (requestGroupAuthoredOn p)
    ,  "author" .= toJSON (requestGroupAuthor p)
    ,  "reasonCode" .= toJSON (requestGroupReasonCode p)
    ,  "reasonReference" .= toJSON (requestGroupReasonReference p)
    ,  "note" .= toJSON (requestGroupNote p)
    ,  "action" .= toJSON (requestGroupAction p)
    ]
instance FromJSON RequestGroup where
  parseJSON = withObject "RequestGroup" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "RequestGroup" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        instantiatesCanonical <- o .:? "instantiatesCanonical" .!= []
        instantiatesUri <- o .:? "instantiatesUri" .!= []
        basedOn <- o .:? "basedOn" .!= []
        replaces <- o .:? "replaces" .!= []
        groupIdentifier <- o .:? "groupIdentifier"
        status <- o .:  "status"
        intent <- o .:  "intent"
        priority <- o .:? "priority"
        code <- o .:? "code"
        subject <- o .:? "subject"
        encounter <- o .:? "encounter"
        authoredOn <- o .:? "authoredOn"
        author <- o .:? "author"
        reasonCode <- o .:? "reasonCode" .!= []
        reasonReference <- o .:? "reasonReference" .!= []
        note <- o .:? "note" .!= []
        action <- o .:? "action" .!= []
        return RequestGroup{
            requestGroupId = id
          , requestGroupMeta = meta
          , requestGroupImplicitRules = implicitRules
          , requestGroupLanguage = language
          , requestGroupText = text
--          , requestGroupContained = contained
          , requestGroupExtension = extension
          , requestGroupModifierExtension = modifierExtension
          , requestGroupIdentifier = identifier
          , requestGroupInstantiatesCanonical = instantiatesCanonical
          , requestGroupInstantiatesUri = instantiatesUri
          , requestGroupBasedOn = basedOn
          , requestGroupReplaces = replaces
          , requestGroupGroupIdentifier = groupIdentifier
          , requestGroupStatus = status
          , requestGroupIntent = intent
          , requestGroupPriority = priority
          , requestGroupCode = code
          , requestGroupSubject = subject
          , requestGroupEncounter = encounter
          , requestGroupAuthoredOn = authoredOn
          , requestGroupAuthor = author
          , requestGroupReasonCode = reasonCode
          , requestGroupReasonReference = reasonReference
          , requestGroupNote = note
          , requestGroupAction = action
          }
      _ -> fail "not a RequestGroup"
instance Xmlbf.ToXml RequestGroup where
  toXml p = Xmlbf.element "RequestGroup" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (requestGroupId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (requestGroupMeta p))
             , OptVal   "implicitRules" (fmap toUri (requestGroupImplicitRules p))
             , OptVal   "language" (fmap toLanguage (requestGroupLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (requestGroupText p))
--             , PropList "contained" (fmap Xmlbf.toXml (requestGroupContained p))
             , PropList "extension" (fmap Xmlbf.toXml (requestGroupExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (requestGroupModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (requestGroupIdentifier p))
             , ValList  "instantiatesCanonical" (fmap toCanonical (requestGroupInstantiatesCanonical p))
             , ValList  "instantiatesUri" (fmap toUri (requestGroupInstantiatesUri p))
             , PropList "basedOn" (fmap Xmlbf.toXml (requestGroupBasedOn p))
             , PropList "replaces" (fmap Xmlbf.toXml (requestGroupReplaces p))
             , OptProp  "groupIdentifier" (fmap Xmlbf.toXml (requestGroupGroupIdentifier p))
             , Val      "status" (     toRequestGroupStatus (requestGroupStatus p))
             , Val      "intent" (     toRequestGroupIntent (requestGroupIntent p))
             , OptVal   "priority" (fmap toRequestGroupPriority (requestGroupPriority p))
             , OptProp  "code" (fmap Xmlbf.toXml (requestGroupCode p))
             , OptProp  "subject" (fmap Xmlbf.toXml (requestGroupSubject p))
             , OptProp  "encounter" (fmap Xmlbf.toXml (requestGroupEncounter p))
             , OptVal   "authoredOn" (fmap toDateTime (requestGroupAuthoredOn p))
             , OptProp  "author" (fmap Xmlbf.toXml (requestGroupAuthor p))
             , PropList "reasonCode" (fmap Xmlbf.toXml (requestGroupReasonCode p))
             , PropList "reasonReference" (fmap Xmlbf.toXml (requestGroupReasonReference p))
             , PropList "note" (fmap Xmlbf.toXml (requestGroupNote p))
             , PropList "action" (fmap Xmlbf.toXml (requestGroupAction p))
             ]
instance Xmlbf.FromXml RequestGroup where
  fromXml = do
    id <- optional $ Xmlbf.pElement "id" (Xmlbf.pAttr "value")
    meta <- optional $ Xmlbf.pElement "meta" Xmlbf.fromXml
    implicitRules <- optional $ Xmlbf.pElement "implicitRules" (Xmlbf.pAttr "value")
    language <- optional $ Xmlbf.pElement "language" (Xmlbf.pAttr "value")
    text <- optional $ Xmlbf.pElement "text" Xmlbf.fromXml
--    contained <- many     $ Xmlbf.pElement "contained" Xmlbf.fromXml
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    identifier <- many     $ Xmlbf.pElement "identifier" Xmlbf.fromXml
    instantiatesCanonical <- many     $ Xmlbf.pElement "instantiatesCanonical" (Xmlbf.pAttr "value")
    instantiatesUri <- many     $ Xmlbf.pElement "instantiatesUri" (Xmlbf.pAttr "value")
    basedOn <- many     $ Xmlbf.pElement "basedOn" Xmlbf.fromXml
    replaces <- many     $ Xmlbf.pElement "replaces" Xmlbf.fromXml
    groupIdentifier <- optional $ Xmlbf.pElement "groupIdentifier" Xmlbf.fromXml
    status <-            Xmlbf.pElement "status" (Xmlbf.pAttr "value")
    intent <-            Xmlbf.pElement "intent" (Xmlbf.pAttr "value")
    priority <- optional $ Xmlbf.pElement "priority" (Xmlbf.pAttr "value")
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    subject <- optional $ Xmlbf.pElement "subject" Xmlbf.fromXml
    encounter <- optional $ Xmlbf.pElement "encounter" Xmlbf.fromXml
    authoredOn <- optional $ Xmlbf.pElement "authoredOn" (Xmlbf.pAttr "value")
    author <- optional $ Xmlbf.pElement "author" Xmlbf.fromXml
    reasonCode <- many     $ Xmlbf.pElement "reasonCode" Xmlbf.fromXml
    reasonReference <- many     $ Xmlbf.pElement "reasonReference" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    action <- many     $ Xmlbf.pElement "action" Xmlbf.fromXml
    return RequestGroup {
            requestGroupId = fmap fromId id
          , requestGroupMeta = meta
          , requestGroupImplicitRules = fmap fromUri implicitRules
          , requestGroupLanguage = fmap fromLanguage language
          , requestGroupText = text
--          , requestGroupContained = contained
          , requestGroupExtension = extension
          , requestGroupModifierExtension = modifierExtension
          , requestGroupIdentifier = identifier
          , requestGroupInstantiatesCanonical = fmap fromCanonical instantiatesCanonical
          , requestGroupInstantiatesUri = fmap fromUri instantiatesUri
          , requestGroupBasedOn = basedOn
          , requestGroupReplaces = replaces
          , requestGroupGroupIdentifier = groupIdentifier
          , requestGroupStatus =      fromRequestGroupStatus status
          , requestGroupIntent =      fromRequestGroupIntent intent
          , requestGroupPriority = fmap fromRequestGroupPriority priority
          , requestGroupCode = code
          , requestGroupSubject = subject
          , requestGroupEncounter = encounter
          , requestGroupAuthoredOn = fmap fromDateTime authoredOn
          , requestGroupAuthor = author
          , requestGroupReasonCode = reasonCode
          , requestGroupReasonReference = reasonReference
          , requestGroupNote = note
          , requestGroupAction = action
          }



data RequestGroupConditionKind
    = RGCKApplicability
    | RGCKStart
    | RGCKStop
  deriving (Eq, Show)

instance ToJSON RequestGroupConditionKind where
    toJSON RGCKApplicability = String "applicability"
    toJSON RGCKStart = String "start"
    toJSON RGCKStop = String "stop"
instance FromJSON RequestGroupConditionKind where
    parseJSON "applicability" = return RGCKApplicability
    parseJSON "start" = return RGCKStart
    parseJSON "stop" = return RGCKStop

toRequestGroupConditionKind RGCKApplicability = "applicability"
toRequestGroupConditionKind RGCKStart = "start"
toRequestGroupConditionKind RGCKStop = "stop"
fromRequestGroupConditionKind "applicability" = RGCKApplicability
fromRequestGroupConditionKind "start" = RGCKStart
fromRequestGroupConditionKind "stop" = RGCKStop


data RequestGroupCondition = RequestGroupCondition {
    requestGroupConditionAttrId :: Maybe Text
  , requestGroupConditionExtension :: [Extension]
  , requestGroupConditionModifierExtension :: [Extension]
  , requestGroupConditionKind :: RequestGroupConditionKind
  , requestGroupConditionExpression :: Maybe Expression
  }
--

instance ToJSON RequestGroupCondition where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (requestGroupConditionAttrId p)
    ,  "extension" .= toJSON (requestGroupConditionExtension p)
    ,  "modifierExtension" .= toJSON (requestGroupConditionModifierExtension p)
    ,  "kind" .= toJSON (requestGroupConditionKind p)
    ,  "expression" .= toJSON (requestGroupConditionExpression p)
    ]
instance FromJSON RequestGroupCondition where
  parseJSON = withObject "RequestGroupCondition" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        kind <- o .:  "kind"
        expression <- o .:? "expression"
        return RequestGroupCondition{
            requestGroupConditionAttrId = id
          , requestGroupConditionExtension = extension
          , requestGroupConditionModifierExtension = modifierExtension
          , requestGroupConditionKind = kind
          , requestGroupConditionExpression = expression
          }
instance Xmlbf.ToXml RequestGroupCondition where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (requestGroupConditionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (requestGroupConditionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (requestGroupConditionModifierExtension p))
             , Val      "kind" (     toRequestGroupConditionKind (requestGroupConditionKind p))
             , OptProp  "expression" (fmap Xmlbf.toXml (requestGroupConditionExpression p))
             ]
instance Xmlbf.FromXml RequestGroupCondition where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    kind <-            Xmlbf.pElement "kind" (Xmlbf.pAttr "value")
    expression <- optional $ Xmlbf.pElement "expression" Xmlbf.fromXml
    return RequestGroupCondition {
            requestGroupConditionAttrId = id
          , requestGroupConditionExtension = extension
          , requestGroupConditionModifierExtension = modifierExtension
          , requestGroupConditionKind =      fromRequestGroupConditionKind kind
          , requestGroupConditionExpression = expression
          }



data RequestGroupActionPriority
    = RGAPRoutine
    | RGAPUrgent
    | RGAPAsap
    | RGAPStat
  deriving (Eq, Show)

instance ToJSON RequestGroupActionPriority where
    toJSON RGAPRoutine = String "routine"
    toJSON RGAPUrgent = String "urgent"
    toJSON RGAPAsap = String "asap"
    toJSON RGAPStat = String "stat"
instance FromJSON RequestGroupActionPriority where
    parseJSON "routine" = return RGAPRoutine
    parseJSON "urgent" = return RGAPUrgent
    parseJSON "asap" = return RGAPAsap
    parseJSON "stat" = return RGAPStat

toRequestGroupActionPriority RGAPRoutine = "routine"
toRequestGroupActionPriority RGAPUrgent = "urgent"
toRequestGroupActionPriority RGAPAsap = "asap"
toRequestGroupActionPriority RGAPStat = "stat"
fromRequestGroupActionPriority "routine" = RGAPRoutine
fromRequestGroupActionPriority "urgent" = RGAPUrgent
fromRequestGroupActionPriority "asap" = RGAPAsap
fromRequestGroupActionPriority "stat" = RGAPStat


data RequestGroupActionGroupingBehavior
    = RGAGBVisualGroup
    | RGAGBLogicalGroup
    | RGAGBSentenceGroup
  deriving (Eq, Show)

instance ToJSON RequestGroupActionGroupingBehavior where
    toJSON RGAGBVisualGroup = String "visual-group"
    toJSON RGAGBLogicalGroup = String "logical-group"
    toJSON RGAGBSentenceGroup = String "sentence-group"
instance FromJSON RequestGroupActionGroupingBehavior where
    parseJSON "visual-group" = return RGAGBVisualGroup
    parseJSON "logical-group" = return RGAGBLogicalGroup
    parseJSON "sentence-group" = return RGAGBSentenceGroup

toRequestGroupActionGroupingBehavior RGAGBVisualGroup = "visual-group"
toRequestGroupActionGroupingBehavior RGAGBLogicalGroup = "logical-group"
toRequestGroupActionGroupingBehavior RGAGBSentenceGroup = "sentence-group"
fromRequestGroupActionGroupingBehavior "visual-group" = RGAGBVisualGroup
fromRequestGroupActionGroupingBehavior "logical-group" = RGAGBLogicalGroup
fromRequestGroupActionGroupingBehavior "sentence-group" = RGAGBSentenceGroup


data RequestGroupActionSelectionBehavior
    = RGASBAny
    | RGASBAll
    | RGASBAllOrNone
    | RGASBExactlyOne
    | RGASBAtMostOne
    | RGASBOneOrMore
  deriving (Eq, Show)

instance ToJSON RequestGroupActionSelectionBehavior where
    toJSON RGASBAny = String "any"
    toJSON RGASBAll = String "all"
    toJSON RGASBAllOrNone = String "all-or-none"
    toJSON RGASBExactlyOne = String "exactly-one"
    toJSON RGASBAtMostOne = String "at-most-one"
    toJSON RGASBOneOrMore = String "one-or-more"
instance FromJSON RequestGroupActionSelectionBehavior where
    parseJSON "any" = return RGASBAny
    parseJSON "all" = return RGASBAll
    parseJSON "all-or-none" = return RGASBAllOrNone
    parseJSON "exactly-one" = return RGASBExactlyOne
    parseJSON "at-most-one" = return RGASBAtMostOne
    parseJSON "one-or-more" = return RGASBOneOrMore

toRequestGroupActionSelectionBehavior RGASBAny = "any"
toRequestGroupActionSelectionBehavior RGASBAll = "all"
toRequestGroupActionSelectionBehavior RGASBAllOrNone = "all-or-none"
toRequestGroupActionSelectionBehavior RGASBExactlyOne = "exactly-one"
toRequestGroupActionSelectionBehavior RGASBAtMostOne = "at-most-one"
toRequestGroupActionSelectionBehavior RGASBOneOrMore = "one-or-more"
fromRequestGroupActionSelectionBehavior "any" = RGASBAny
fromRequestGroupActionSelectionBehavior "all" = RGASBAll
fromRequestGroupActionSelectionBehavior "all-or-none" = RGASBAllOrNone
fromRequestGroupActionSelectionBehavior "exactly-one" = RGASBExactlyOne
fromRequestGroupActionSelectionBehavior "at-most-one" = RGASBAtMostOne
fromRequestGroupActionSelectionBehavior "one-or-more" = RGASBOneOrMore


data RequestGroupActionRequiredBehavior
    = RGARBMust
    | RGARBCould
    | RGARBMustUnlessDocumented
  deriving (Eq, Show)

instance ToJSON RequestGroupActionRequiredBehavior where
    toJSON RGARBMust = String "must"
    toJSON RGARBCould = String "could"
    toJSON RGARBMustUnlessDocumented = String "must-unless-documented"
instance FromJSON RequestGroupActionRequiredBehavior where
    parseJSON "must" = return RGARBMust
    parseJSON "could" = return RGARBCould
    parseJSON "must-unless-documented" = return RGARBMustUnlessDocumented

toRequestGroupActionRequiredBehavior RGARBMust = "must"
toRequestGroupActionRequiredBehavior RGARBCould = "could"
toRequestGroupActionRequiredBehavior RGARBMustUnlessDocumented = "must-unless-documented"
fromRequestGroupActionRequiredBehavior "must" = RGARBMust
fromRequestGroupActionRequiredBehavior "could" = RGARBCould
fromRequestGroupActionRequiredBehavior "must-unless-documented" = RGARBMustUnlessDocumented


data RequestGroupActionPrecheckBehavior
    = RGAPBYes
    | RGAPBNo
  deriving (Eq, Show)

instance ToJSON RequestGroupActionPrecheckBehavior where
    toJSON RGAPBYes = String "yes"
    toJSON RGAPBNo = String "no"
instance FromJSON RequestGroupActionPrecheckBehavior where
    parseJSON "yes" = return RGAPBYes
    parseJSON "no" = return RGAPBNo

toRequestGroupActionPrecheckBehavior RGAPBYes = "yes"
toRequestGroupActionPrecheckBehavior RGAPBNo = "no"
fromRequestGroupActionPrecheckBehavior "yes" = RGAPBYes
fromRequestGroupActionPrecheckBehavior "no" = RGAPBNo


data RequestGroupActionCardinalityBehavior
    = RGACBSingle
    | RGACBMultiple
  deriving (Eq, Show)

instance ToJSON RequestGroupActionCardinalityBehavior where
    toJSON RGACBSingle = String "single"
    toJSON RGACBMultiple = String "multiple"
instance FromJSON RequestGroupActionCardinalityBehavior where
    parseJSON "single" = return RGACBSingle
    parseJSON "multiple" = return RGACBMultiple

toRequestGroupActionCardinalityBehavior RGACBSingle = "single"
toRequestGroupActionCardinalityBehavior RGACBMultiple = "multiple"
fromRequestGroupActionCardinalityBehavior "single" = RGACBSingle
fromRequestGroupActionCardinalityBehavior "multiple" = RGACBMultiple


data RequestGroupActionTiming
    = RequestGroupActionTimingDateTime DateTime
    | RequestGroupActionTimingAge Age
    | RequestGroupActionTimingPeriod Period
    | RequestGroupActionTimingDuration Duration
    | RequestGroupActionTimingRange Range
    | RequestGroupActionTimingTiming Timing
    deriving (Eq, Show)

data RequestGroupAction = RequestGroupAction {
    requestGroupActionAttrId :: Maybe Text
  , requestGroupActionExtension :: [Extension]
  , requestGroupActionModifierExtension :: [Extension]
  , requestGroupActionPrefix :: Maybe Text
  , requestGroupActionTitle :: Maybe Text
  , requestGroupActionDescription :: Maybe Text
  , requestGroupActionTextEquivalent :: Maybe Text
  , requestGroupActionPriority :: Maybe RequestGroupActionPriority
  , requestGroupActionCode :: [CodeableConcept]
  , requestGroupActionDocumentation :: [RelatedArtifact]
  , requestGroupActionCondition :: [RequestGroupCondition]
  , requestGroupActionRelatedAction :: [RequestGroupRelatedAction]
  , requestGroupActionTimingDateTime :: Maybe DateTime
  , requestGroupActionTimingAge :: Maybe Age
  , requestGroupActionTimingPeriod :: Maybe Period
  , requestGroupActionTimingDuration :: Maybe Duration
  , requestGroupActionTimingRange :: Maybe Range
  , requestGroupActionTimingTiming :: Maybe Timing
  , requestGroupActionParticipant :: [Reference]
  , requestGroupActionType :: Maybe CodeableConcept
  , requestGroupActionGroupingBehavior :: Maybe RequestGroupActionGroupingBehavior
  , requestGroupActionSelectionBehavior :: Maybe RequestGroupActionSelectionBehavior
  , requestGroupActionRequiredBehavior :: Maybe RequestGroupActionRequiredBehavior
  , requestGroupActionPrecheckBehavior :: Maybe RequestGroupActionPrecheckBehavior
  , requestGroupActionCardinalityBehavior :: Maybe RequestGroupActionCardinalityBehavior
  , requestGroupActionResource :: Maybe Reference
  , requestGroupActionAction :: [RequestGroupAction]
  }
--

instance ToJSON RequestGroupAction where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (requestGroupActionAttrId p)
    ,  "extension" .= toJSON (requestGroupActionExtension p)
    ,  "modifierExtension" .= toJSON (requestGroupActionModifierExtension p)
    ,  "prefix" .= toJSON (requestGroupActionPrefix p)
    ,  "title" .= toJSON (requestGroupActionTitle p)
    ,  "description" .= toJSON (requestGroupActionDescription p)
    ,  "textEquivalent" .= toJSON (requestGroupActionTextEquivalent p)
    ,  "priority" .= toJSON (requestGroupActionPriority p)
    ,  "code" .= toJSON (requestGroupActionCode p)
    ,  "documentation" .= toJSON (requestGroupActionDocumentation p)
    ,  "condition" .= toJSON (requestGroupActionCondition p)
    ,  "relatedAction" .= toJSON (requestGroupActionRelatedAction p)
    ,  "timingDateTime" .= toJSON (requestGroupActionTimingDateTime p)
    ,  "timingAge" .= toJSON (requestGroupActionTimingAge p)
    ,  "timingPeriod" .= toJSON (requestGroupActionTimingPeriod p)
    ,  "timingDuration" .= toJSON (requestGroupActionTimingDuration p)
    ,  "timingRange" .= toJSON (requestGroupActionTimingRange p)
    ,  "timingTiming" .= toJSON (requestGroupActionTimingTiming p)
    ,  "participant" .= toJSON (requestGroupActionParticipant p)
    ,  "type" .= toJSON (requestGroupActionType p)
    ,  "groupingBehavior" .= toJSON (requestGroupActionGroupingBehavior p)
    ,  "selectionBehavior" .= toJSON (requestGroupActionSelectionBehavior p)
    ,  "requiredBehavior" .= toJSON (requestGroupActionRequiredBehavior p)
    ,  "precheckBehavior" .= toJSON (requestGroupActionPrecheckBehavior p)
    ,  "cardinalityBehavior" .= toJSON (requestGroupActionCardinalityBehavior p)
    ,  "resource" .= toJSON (requestGroupActionResource p)
    ,  "action" .= toJSON (requestGroupActionAction p)
    ]
instance FromJSON RequestGroupAction where
  parseJSON = withObject "RequestGroupAction" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        prefix <- o .:? "prefix"
        title <- o .:? "title"
        description <- o .:? "description"
        textEquivalent <- o .:? "textEquivalent"
        priority <- o .:? "priority"
        code <- o .:? "code" .!= []
        documentation <- o .:? "documentation" .!= []
        condition <- o .:? "condition" .!= []
        relatedAction <- o .:? "relatedAction" .!= []
        timingDateTime <- o .:? "timingDateTime"
        timingAge <- o .:? "timingAge"
        timingPeriod <- o .:? "timingPeriod"
        timingDuration <- o .:? "timingDuration"
        timingRange <- o .:? "timingRange"
        timingTiming <- o .:? "timingTiming"
        participant <- o .:? "participant" .!= []
        ty <- o .:? "type"
        groupingBehavior <- o .:? "groupingBehavior"
        selectionBehavior <- o .:? "selectionBehavior"
        requiredBehavior <- o .:? "requiredBehavior"
        precheckBehavior <- o .:? "precheckBehavior"
        cardinalityBehavior <- o .:? "cardinalityBehavior"
        resource <- o .:? "resource"
        action <- o .:? "action" .!= []
        return RequestGroupAction{
            requestGroupActionAttrId = id
          , requestGroupActionExtension = extension
          , requestGroupActionModifierExtension = modifierExtension
          , requestGroupActionPrefix = prefix
          , requestGroupActionTitle = title
          , requestGroupActionDescription = description
          , requestGroupActionTextEquivalent = textEquivalent
          , requestGroupActionPriority = priority
          , requestGroupActionCode = code
          , requestGroupActionDocumentation = documentation
          , requestGroupActionCondition = condition
          , requestGroupActionRelatedAction = relatedAction
          , requestGroupActionTimingDateTime = timingDateTime
          , requestGroupActionTimingAge = timingAge
          , requestGroupActionTimingPeriod = timingPeriod
          , requestGroupActionTimingDuration = timingDuration
          , requestGroupActionTimingRange = timingRange
          , requestGroupActionTimingTiming = timingTiming
          , requestGroupActionParticipant = participant
          , requestGroupActionType = ty
          , requestGroupActionGroupingBehavior = groupingBehavior
          , requestGroupActionSelectionBehavior = selectionBehavior
          , requestGroupActionRequiredBehavior = requiredBehavior
          , requestGroupActionPrecheckBehavior = precheckBehavior
          , requestGroupActionCardinalityBehavior = cardinalityBehavior
          , requestGroupActionResource = resource
          , requestGroupActionAction = action
          }
instance Xmlbf.ToXml RequestGroupAction where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (requestGroupActionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (requestGroupActionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (requestGroupActionModifierExtension p))
             , OptVal   "prefix" (fmap toString (requestGroupActionPrefix p))
             , OptVal   "title" (fmap toString (requestGroupActionTitle p))
             , OptVal   "description" (fmap toString (requestGroupActionDescription p))
             , OptVal   "textEquivalent" (fmap toString (requestGroupActionTextEquivalent p))
             , OptVal   "priority" (fmap toRequestGroupActionPriority (requestGroupActionPriority p))
             , PropList "code" (fmap Xmlbf.toXml (requestGroupActionCode p))
             , PropList "documentation" (fmap Xmlbf.toXml (requestGroupActionDocumentation p))
             , PropList "condition" (fmap Xmlbf.toXml (requestGroupActionCondition p))
             , PropList "relatedAction" (fmap Xmlbf.toXml (requestGroupActionRelatedAction p))
             , OptVal   "timingDateTime" (fmap toDateTime (requestGroupActionTimingDateTime p))
             , OptProp  "timingAge" (fmap Xmlbf.toXml (requestGroupActionTimingAge p))
             , OptProp  "timingPeriod" (fmap Xmlbf.toXml (requestGroupActionTimingPeriod p))
             , OptProp  "timingDuration" (fmap Xmlbf.toXml (requestGroupActionTimingDuration p))
             , OptProp  "timingRange" (fmap Xmlbf.toXml (requestGroupActionTimingRange p))
             , OptProp  "timingTiming" (fmap Xmlbf.toXml (requestGroupActionTimingTiming p))
             , PropList "participant" (fmap Xmlbf.toXml (requestGroupActionParticipant p))
             , OptProp  "type" (fmap Xmlbf.toXml (requestGroupActionType p))
             , OptVal   "groupingBehavior" (fmap toRequestGroupActionGroupingBehavior (requestGroupActionGroupingBehavior p))
             , OptVal   "selectionBehavior" (fmap toRequestGroupActionSelectionBehavior (requestGroupActionSelectionBehavior p))
             , OptVal   "requiredBehavior" (fmap toRequestGroupActionRequiredBehavior (requestGroupActionRequiredBehavior p))
             , OptVal   "precheckBehavior" (fmap toRequestGroupActionPrecheckBehavior (requestGroupActionPrecheckBehavior p))
             , OptVal   "cardinalityBehavior" (fmap toRequestGroupActionCardinalityBehavior (requestGroupActionCardinalityBehavior p))
             , OptProp  "resource" (fmap Xmlbf.toXml (requestGroupActionResource p))
             , PropList "action" (fmap Xmlbf.toXml (requestGroupActionAction p))
             ]
instance Xmlbf.FromXml RequestGroupAction where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    prefix <- optional $ Xmlbf.pElement "prefix" (Xmlbf.pAttr "value")
    title <- optional $ Xmlbf.pElement "title" (Xmlbf.pAttr "value")
    description <- optional $ Xmlbf.pElement "description" (Xmlbf.pAttr "value")
    textEquivalent <- optional $ Xmlbf.pElement "textEquivalent" (Xmlbf.pAttr "value")
    priority <- optional $ Xmlbf.pElement "priority" (Xmlbf.pAttr "value")
    code <- many     $ Xmlbf.pElement "code" Xmlbf.fromXml
    documentation <- many     $ Xmlbf.pElement "documentation" Xmlbf.fromXml
    condition <- many     $ Xmlbf.pElement "condition" Xmlbf.fromXml
    relatedAction <- many     $ Xmlbf.pElement "relatedAction" Xmlbf.fromXml
    timingDateTime <- optional $ Xmlbf.pElement "timingDateTime" (Xmlbf.pAttr "value")
    timingAge <- optional $ Xmlbf.pElement "timingAge" Xmlbf.fromXml
    timingPeriod <- optional $ Xmlbf.pElement "timingPeriod" Xmlbf.fromXml
    timingDuration <- optional $ Xmlbf.pElement "timingDuration" Xmlbf.fromXml
    timingRange <- optional $ Xmlbf.pElement "timingRange" Xmlbf.fromXml
    timingTiming <- optional $ Xmlbf.pElement "timingTiming" Xmlbf.fromXml
    participant <- many     $ Xmlbf.pElement "participant" Xmlbf.fromXml
    ty <- optional $ Xmlbf.pElement "type" Xmlbf.fromXml
    groupingBehavior <- optional $ Xmlbf.pElement "groupingBehavior" (Xmlbf.pAttr "value")
    selectionBehavior <- optional $ Xmlbf.pElement "selectionBehavior" (Xmlbf.pAttr "value")
    requiredBehavior <- optional $ Xmlbf.pElement "requiredBehavior" (Xmlbf.pAttr "value")
    precheckBehavior <- optional $ Xmlbf.pElement "precheckBehavior" (Xmlbf.pAttr "value")
    cardinalityBehavior <- optional $ Xmlbf.pElement "cardinalityBehavior" (Xmlbf.pAttr "value")
    resource <- optional $ Xmlbf.pElement "resource" Xmlbf.fromXml
    action <- many     $ Xmlbf.pElement "action" Xmlbf.fromXml
    return RequestGroupAction {
            requestGroupActionAttrId = id
          , requestGroupActionExtension = extension
          , requestGroupActionModifierExtension = modifierExtension
          , requestGroupActionPrefix = fmap fromString prefix
          , requestGroupActionTitle = fmap fromString title
          , requestGroupActionDescription = fmap fromString description
          , requestGroupActionTextEquivalent = fmap fromString textEquivalent
          , requestGroupActionPriority = fmap fromRequestGroupActionPriority priority
          , requestGroupActionCode = code
          , requestGroupActionDocumentation = documentation
          , requestGroupActionCondition = condition
          , requestGroupActionRelatedAction = relatedAction
          , requestGroupActionTimingDateTime = fmap fromDateTime timingDateTime
          , requestGroupActionTimingAge = timingAge
          , requestGroupActionTimingPeriod = timingPeriod
          , requestGroupActionTimingDuration = timingDuration
          , requestGroupActionTimingRange = timingRange
          , requestGroupActionTimingTiming = timingTiming
          , requestGroupActionParticipant = participant
          , requestGroupActionType = ty
          , requestGroupActionGroupingBehavior = fmap fromRequestGroupActionGroupingBehavior groupingBehavior
          , requestGroupActionSelectionBehavior = fmap fromRequestGroupActionSelectionBehavior selectionBehavior
          , requestGroupActionRequiredBehavior = fmap fromRequestGroupActionRequiredBehavior requiredBehavior
          , requestGroupActionPrecheckBehavior = fmap fromRequestGroupActionPrecheckBehavior precheckBehavior
          , requestGroupActionCardinalityBehavior = fmap fromRequestGroupActionCardinalityBehavior cardinalityBehavior
          , requestGroupActionResource = resource
          , requestGroupActionAction = action
          }



data RequestGroupRelatedActionRelationship
    = RGRARBeforeStart
    | RGRARBefore
    | RGRARBeforeEnd
    | RGRARConcurrentWithStart
    | RGRARConcurrent
    | RGRARConcurrentWithEnd
    | RGRARAfterStart
    | RGRARAfter
    | RGRARAfterEnd
  deriving (Eq, Show)

instance ToJSON RequestGroupRelatedActionRelationship where
    toJSON RGRARBeforeStart = String "before-start"
    toJSON RGRARBefore = String "before"
    toJSON RGRARBeforeEnd = String "before-end"
    toJSON RGRARConcurrentWithStart = String "concurrent-with-start"
    toJSON RGRARConcurrent = String "concurrent"
    toJSON RGRARConcurrentWithEnd = String "concurrent-with-end"
    toJSON RGRARAfterStart = String "after-start"
    toJSON RGRARAfter = String "after"
    toJSON RGRARAfterEnd = String "after-end"
instance FromJSON RequestGroupRelatedActionRelationship where
    parseJSON "before-start" = return RGRARBeforeStart
    parseJSON "before" = return RGRARBefore
    parseJSON "before-end" = return RGRARBeforeEnd
    parseJSON "concurrent-with-start" = return RGRARConcurrentWithStart
    parseJSON "concurrent" = return RGRARConcurrent
    parseJSON "concurrent-with-end" = return RGRARConcurrentWithEnd
    parseJSON "after-start" = return RGRARAfterStart
    parseJSON "after" = return RGRARAfter
    parseJSON "after-end" = return RGRARAfterEnd

toRequestGroupRelatedActionRelationship RGRARBeforeStart = "before-start"
toRequestGroupRelatedActionRelationship RGRARBefore = "before"
toRequestGroupRelatedActionRelationship RGRARBeforeEnd = "before-end"
toRequestGroupRelatedActionRelationship RGRARConcurrentWithStart = "concurrent-with-start"
toRequestGroupRelatedActionRelationship RGRARConcurrent = "concurrent"
toRequestGroupRelatedActionRelationship RGRARConcurrentWithEnd = "concurrent-with-end"
toRequestGroupRelatedActionRelationship RGRARAfterStart = "after-start"
toRequestGroupRelatedActionRelationship RGRARAfter = "after"
toRequestGroupRelatedActionRelationship RGRARAfterEnd = "after-end"
fromRequestGroupRelatedActionRelationship "before-start" = RGRARBeforeStart
fromRequestGroupRelatedActionRelationship "before" = RGRARBefore
fromRequestGroupRelatedActionRelationship "before-end" = RGRARBeforeEnd
fromRequestGroupRelatedActionRelationship "concurrent-with-start" = RGRARConcurrentWithStart
fromRequestGroupRelatedActionRelationship "concurrent" = RGRARConcurrent
fromRequestGroupRelatedActionRelationship "concurrent-with-end" = RGRARConcurrentWithEnd
fromRequestGroupRelatedActionRelationship "after-start" = RGRARAfterStart
fromRequestGroupRelatedActionRelationship "after" = RGRARAfter
fromRequestGroupRelatedActionRelationship "after-end" = RGRARAfterEnd


data RequestGroupRelatedActionOffset
    = RequestGroupRelatedActionOffsetDuration Duration
    | RequestGroupRelatedActionOffsetRange Range
    deriving (Eq, Show)

data RequestGroupRelatedAction = RequestGroupRelatedAction {
    requestGroupRelatedActionAttrId :: Maybe Text
  , requestGroupRelatedActionExtension :: [Extension]
  , requestGroupRelatedActionModifierExtension :: [Extension]
  , requestGroupRelatedActionActionId :: Id
  , requestGroupRelatedActionRelationship :: RequestGroupRelatedActionRelationship
  , requestGroupRelatedActionOffsetDuration :: Maybe Duration
  , requestGroupRelatedActionOffsetRange :: Maybe Range
  }
--

instance ToJSON RequestGroupRelatedAction where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (requestGroupRelatedActionAttrId p)
    ,  "extension" .= toJSON (requestGroupRelatedActionExtension p)
    ,  "modifierExtension" .= toJSON (requestGroupRelatedActionModifierExtension p)
    ,  "actionId" .= toJSON (requestGroupRelatedActionActionId p)
    ,  "relationship" .= toJSON (requestGroupRelatedActionRelationship p)
    ,  "offsetDuration" .= toJSON (requestGroupRelatedActionOffsetDuration p)
    ,  "offsetRange" .= toJSON (requestGroupRelatedActionOffsetRange p)
    ]
instance FromJSON RequestGroupRelatedAction where
  parseJSON = withObject "RequestGroupRelatedAction" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        actionId <- o .:  "actionId"
        relationship <- o .:  "relationship"
        offsetDuration <- o .:? "offsetDuration"
        offsetRange <- o .:? "offsetRange"
        return RequestGroupRelatedAction{
            requestGroupRelatedActionAttrId = id
          , requestGroupRelatedActionExtension = extension
          , requestGroupRelatedActionModifierExtension = modifierExtension
          , requestGroupRelatedActionActionId = actionId
          , requestGroupRelatedActionRelationship = relationship
          , requestGroupRelatedActionOffsetDuration = offsetDuration
          , requestGroupRelatedActionOffsetRange = offsetRange
          }
instance Xmlbf.ToXml RequestGroupRelatedAction where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (requestGroupRelatedActionAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (requestGroupRelatedActionExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (requestGroupRelatedActionModifierExtension p))
             , Val      "actionId" (     toId (requestGroupRelatedActionActionId p))
             , Val      "relationship" (     toRequestGroupRelatedActionRelationship (requestGroupRelatedActionRelationship p))
             , OptProp  "offsetDuration" (fmap Xmlbf.toXml (requestGroupRelatedActionOffsetDuration p))
             , OptProp  "offsetRange" (fmap Xmlbf.toXml (requestGroupRelatedActionOffsetRange p))
             ]
instance Xmlbf.FromXml RequestGroupRelatedAction where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    actionId <-            Xmlbf.pElement "actionId" (Xmlbf.pAttr "value")
    relationship <-            Xmlbf.pElement "relationship" (Xmlbf.pAttr "value")
    offsetDuration <- optional $ Xmlbf.pElement "offsetDuration" Xmlbf.fromXml
    offsetRange <- optional $ Xmlbf.pElement "offsetRange" Xmlbf.fromXml
    return RequestGroupRelatedAction {
            requestGroupRelatedActionAttrId = id
          , requestGroupRelatedActionExtension = extension
          , requestGroupRelatedActionModifierExtension = modifierExtension
          , requestGroupRelatedActionActionId =      fromId actionId
          , requestGroupRelatedActionRelationship =      fromRequestGroupRelatedActionRelationship relationship
          , requestGroupRelatedActionOffsetDuration = offsetDuration
          , requestGroupRelatedActionOffsetRange = offsetRange
          }




