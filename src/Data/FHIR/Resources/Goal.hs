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
-- FHIR 4.0.0 Goal
--

module Data.FHIR.Resources.Goal where

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

data GoalLifecycleStatus
    = GLSProposed
    | GLSPlanned
    | GLSAccepted
    | GLSActive
    | GLSOnHold
    | GLSCompleted
    | GLSCancelled
    | GLSEnteredInError
    | GLSRejected
  deriving (Eq, Show)

instance ToJSON GoalLifecycleStatus where
    toJSON GLSProposed = String "proposed"
    toJSON GLSPlanned = String "planned"
    toJSON GLSAccepted = String "accepted"
    toJSON GLSActive = String "active"
    toJSON GLSOnHold = String "on-hold"
    toJSON GLSCompleted = String "completed"
    toJSON GLSCancelled = String "cancelled"
    toJSON GLSEnteredInError = String "entered-in-error"
    toJSON GLSRejected = String "rejected"
instance FromJSON GoalLifecycleStatus where
    parseJSON "proposed" = return GLSProposed
    parseJSON "planned" = return GLSPlanned
    parseJSON "accepted" = return GLSAccepted
    parseJSON "active" = return GLSActive
    parseJSON "on-hold" = return GLSOnHold
    parseJSON "completed" = return GLSCompleted
    parseJSON "cancelled" = return GLSCancelled
    parseJSON "entered-in-error" = return GLSEnteredInError
    parseJSON "rejected" = return GLSRejected

toGoalLifecycleStatus GLSProposed = "proposed"
toGoalLifecycleStatus GLSPlanned = "planned"
toGoalLifecycleStatus GLSAccepted = "accepted"
toGoalLifecycleStatus GLSActive = "active"
toGoalLifecycleStatus GLSOnHold = "on-hold"
toGoalLifecycleStatus GLSCompleted = "completed"
toGoalLifecycleStatus GLSCancelled = "cancelled"
toGoalLifecycleStatus GLSEnteredInError = "entered-in-error"
toGoalLifecycleStatus GLSRejected = "rejected"
fromGoalLifecycleStatus "proposed" = GLSProposed
fromGoalLifecycleStatus "planned" = GLSPlanned
fromGoalLifecycleStatus "accepted" = GLSAccepted
fromGoalLifecycleStatus "active" = GLSActive
fromGoalLifecycleStatus "on-hold" = GLSOnHold
fromGoalLifecycleStatus "completed" = GLSCompleted
fromGoalLifecycleStatus "cancelled" = GLSCancelled
fromGoalLifecycleStatus "entered-in-error" = GLSEnteredInError
fromGoalLifecycleStatus "rejected" = GLSRejected


data GoalStart
    = GoalStartDate Date
    | GoalStartCodeableConcept CodeableConcept
    deriving (Eq, Show)

data Goal = Goal {
    goalId :: Maybe Id
  , goalMeta :: Maybe Meta
  , goalImplicitRules :: Maybe Uri
  , goalLanguage :: Maybe Language
  , goalText :: Maybe Narrative
--    goalContained :: [ResourceContainer]
  , goalExtension :: [Extension]
  , goalModifierExtension :: [Extension]
  , goalIdentifier :: [Identifier]
  , goalLifecycleStatus :: GoalLifecycleStatus
  , goalAchievementStatus :: Maybe CodeableConcept
  , goalCategory :: [CodeableConcept]
  , goalPriority :: Maybe CodeableConcept
  , goalDescription :: CodeableConcept
  , goalSubject :: Reference
  , goalStart :: Maybe GoalStart
  , goalTarget :: [GoalTarget]
  , goalStatusDate :: Maybe Date
  , goalStatusReason :: Maybe Text
  , goalExpressedBy :: Maybe Reference
  , goalAddresses :: [Reference]
  , goalNote :: [Annotation]
  , goalOutcomeCode :: [CodeableConcept]
  , goalOutcomeReference :: [Reference]
  }
--

instance ToJSON Goal where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Goal")
    ,  "id" .= toJSON (goalId p)
    ,  "meta" .= toJSON (goalMeta p)
    ,  "implicitRules" .= toJSON (goalImplicitRules p)
    ,  "language" .= toJSON (goalLanguage p)
    ,  "text" .= toJSON (goalText p)
--    , "contained" .= toJSON (goalContained p)
    ,  "extension" .= toJSON (goalExtension p)
    ,  "modifierExtension" .= toJSON (goalModifierExtension p)
    ,  "identifier" .= toJSON (goalIdentifier p)
    ,  "lifecycleStatus" .= toJSON (goalLifecycleStatus p)
    ,  "achievementStatus" .= toJSON (goalAchievementStatus p)
    ,  "category" .= toJSON (goalCategory p)
    ,  "priority" .= toJSON (goalPriority p)
    ,  "description" .= toJSON (goalDescription p)
    ,  "subject" .= toJSON (goalSubject p)
    , toStartJSON (goalStart p)
    ,  "target" .= toJSON (goalTarget p)
    ,  "statusDate" .= toJSON (goalStatusDate p)
    ,  "statusReason" .= toJSON (goalStatusReason p)
    ,  "expressedBy" .= toJSON (goalExpressedBy p)
    ,  "addresses" .= toJSON (goalAddresses p)
    ,  "note" .= toJSON (goalNote p)
    ,  "outcomeCode" .= toJSON (goalOutcomeCode p)
    ,  "outcomeReference" .= toJSON (goalOutcomeReference p)
    ]
    where 
      toStartJSON (     Nothing   ) = ("start", Null)
      toStartJSON (Just (GoalStartDate c)) = ("start", toJSON c)
      toStartJSON (Just (GoalStartCodeableConcept c)) = ("start", toJSON c)
instance FromJSON Goal where
  parseJSON = withObject "Goal" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Goal" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        lifecycleStatus <- o .:  "lifecycleStatus"
        achievementStatus <- o .:? "achievementStatus"
        category <- o .:? "category" .!= []
        priority <- o .:? "priority"
        description <- o .:  "description"
        subject <- o .:  "subject"
        start <- parseStart o
        target <- o .:? "target" .!= []
        statusDate <- o .:? "statusDate"
        statusReason <- o .:? "statusReason"
        expressedBy <- o .:? "expressedBy"
        addresses <- o .:? "addresses" .!= []
        note <- o .:? "note" .!= []
        outcomeCode <- o .:? "outcomeCode" .!= []
        outcomeReference <- o .:? "outcomeReference" .!= []
        return Goal{
            goalId = id
          , goalMeta = meta
          , goalImplicitRules = implicitRules
          , goalLanguage = language
          , goalText = text
--          , goalContained = contained
          , goalExtension = extension
          , goalModifierExtension = modifierExtension
          , goalIdentifier = identifier
          , goalLifecycleStatus = lifecycleStatus
          , goalAchievementStatus = achievementStatus
          , goalCategory = category
          , goalPriority = priority
          , goalDescription = description
          , goalSubject = subject
          , goalStart = start
          , goalTarget = target
          , goalStatusDate = statusDate
          , goalStatusReason = statusReason
          , goalExpressedBy = expressedBy
          , goalAddresses = addresses
          , goalNote = note
          , goalOutcomeCode = outcomeCode
          , goalOutcomeReference = outcomeReference
          }
      _ -> fail "not a Goal"
    where 
      parseStart o = parseStartDate o <|> parseStartCodeableConcept o
      parseStartDate o = do
                has <- o .: "startDate"
                return $ Just (GoalStartDate has)
      parseStartCodeableConcept o = do
                has <- o .: "startCodeableConcept"
                return $ Just (GoalStartCodeableConcept has)
instance Xmlbf.ToXml Goal where
  toXml p = Xmlbf.element "Goal" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (goalId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (goalMeta p))
             , OptVal   "implicitRules" (fmap toUri (goalImplicitRules p))
             , OptVal   "language" (fmap toLanguage (goalLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (goalText p))
--             , PropList "contained" (fmap Xmlbf.toXml (goalContained p))
             , PropList "extension" (fmap Xmlbf.toXml (goalExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (goalModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (goalIdentifier p))
             , Val      "lifecycleStatus" (     toGoalLifecycleStatus (goalLifecycleStatus p))
             , OptProp  "achievementStatus" (fmap Xmlbf.toXml (goalAchievementStatus p))
             , PropList "category" (fmap Xmlbf.toXml (goalCategory p))
             , OptProp  "priority" (fmap Xmlbf.toXml (goalPriority p))
             , Prop     "description" (HM.empty, Xmlbf.toXml (goalDescription p))
             , Prop     "subject" (HM.empty, Xmlbf.toXml (goalSubject p))
             , toStartXml (goalStart p)
             , PropList "target" (fmap Xmlbf.toXml (goalTarget p))
             , OptVal   "statusDate" (fmap toDate (goalStatusDate p))
             , OptVal   "statusReason" (fmap toString (goalStatusReason p))
             , OptProp  "expressedBy" (fmap Xmlbf.toXml (goalExpressedBy p))
             , PropList "addresses" (fmap Xmlbf.toXml (goalAddresses p))
             , PropList "note" (fmap Xmlbf.toXml (goalNote p))
             , PropList "outcomeCode" (fmap Xmlbf.toXml (goalOutcomeCode p))
             , PropList "outcomeReference" (fmap Xmlbf.toXml (goalOutcomeReference p))
             ]
          toStartXml ( Nothing   ) = (OptVal "start" Nothing)
          toStartXml (Just (GoalStartDate p)) = Val   "startDate" (toDate p)
          toStartXml (Just (GoalStartCodeableConcept p)) = Prop  "startCodeableConcept" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml Goal where
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
    lifecycleStatus <-            Xmlbf.pElement "lifecycleStatus" (Xmlbf.pAttr "value")
    achievementStatus <- optional $ Xmlbf.pElement "achievementStatus" Xmlbf.fromXml
    category <- many     $ Xmlbf.pElement "category" Xmlbf.fromXml
    priority <- optional $ Xmlbf.pElement "priority" Xmlbf.fromXml
    description <-            Xmlbf.pElement "description" Xmlbf.fromXml
    subject <-            Xmlbf.pElement "subject" Xmlbf.fromXml
    start <- fromStartXml
    target <- many     $ Xmlbf.pElement "target" Xmlbf.fromXml
    statusDate <- optional $ Xmlbf.pElement "statusDate" (Xmlbf.pAttr "value")
    statusReason <- optional $ Xmlbf.pElement "statusReason" (Xmlbf.pAttr "value")
    expressedBy <- optional $ Xmlbf.pElement "expressedBy" Xmlbf.fromXml
    addresses <- many     $ Xmlbf.pElement "addresses" Xmlbf.fromXml
    note <- many     $ Xmlbf.pElement "note" Xmlbf.fromXml
    outcomeCode <- many     $ Xmlbf.pElement "outcomeCode" Xmlbf.fromXml
    outcomeReference <- many     $ Xmlbf.pElement "outcomeReference" Xmlbf.fromXml
    return Goal {
            goalId = fmap fromId id
          , goalMeta = meta
          , goalImplicitRules = fmap fromUri implicitRules
          , goalLanguage = fmap fromLanguage language
          , goalText = text
--          , goalContained = contained
          , goalExtension = extension
          , goalModifierExtension = modifierExtension
          , goalIdentifier = identifier
          , goalLifecycleStatus =      fromGoalLifecycleStatus lifecycleStatus
          , goalAchievementStatus = achievementStatus
          , goalCategory = category
          , goalPriority = priority
          , goalDescription = description
          , goalSubject = subject
          , goalStart = start
          , goalTarget = target
          , goalStatusDate = fmap fromDate statusDate
          , goalStatusReason = fmap fromString statusReason
          , goalExpressedBy = expressedBy
          , goalAddresses = addresses
          , goalNote = note
          , goalOutcomeCode = outcomeCode
          , goalOutcomeReference = outcomeReference
          }

    where 
      fromStartXml = parseStartDate <|> parseStartCodeableConcept <|> pure Nothing
      parseStartDate = do
                has <- Xmlbf.pElement "startDate" (Xmlbf.pAttr "value")
                return $ Just (GoalStartDate (     toDate has))
      parseStartCodeableConcept = do
                has <- Xmlbf.pElement "startCodeableConcept" Xmlbf.fromXml
                return $ Just (GoalStartCodeableConcept (                      has))


data GoalTargetDetail
    = GoalTargetDetailQuantity Quantity
    | GoalTargetDetailRange Range
    | GoalTargetDetailCodeableConcept CodeableConcept
    | GoalTargetDetailString Text
    | GoalTargetDetailBoolean Boolean
    | GoalTargetDetailInteger Integer
    | GoalTargetDetailRatio Ratio
    deriving (Eq, Show)

data GoalTargetDue
    = GoalTargetDueDate Date
    | GoalTargetDueDuration Duration
    deriving (Eq, Show)

data GoalTarget = GoalTarget {
    goalTargetAttrId :: Maybe Text
  , goalTargetExtension :: [Extension]
  , goalTargetModifierExtension :: [Extension]
  , goalTargetMeasure :: Maybe CodeableConcept
  , goalTargetDetail :: Maybe GoalTargetDetail
  , goalTargetDue :: Maybe GoalTargetDue
  }
--

instance ToJSON GoalTarget where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (goalTargetAttrId p)
    ,  "extension" .= toJSON (goalTargetExtension p)
    ,  "modifierExtension" .= toJSON (goalTargetModifierExtension p)
    ,  "measure" .= toJSON (goalTargetMeasure p)
    , toDetailJSON (goalTargetDetail p)
    , toDueJSON (goalTargetDue p)
    ]
    where 
      toDetailJSON (     Nothing   ) = ("detail", Null)
      toDetailJSON (Just (GoalTargetDetailQuantity c)) = ("detailQuantity", toJSON c)
      toDetailJSON (Just (GoalTargetDetailRange c)) = ("detailRange", toJSON c)
      toDetailJSON (Just (GoalTargetDetailCodeableConcept c)) = ("detailCodeableConcept", toJSON c)
      toDetailJSON (Just (GoalTargetDetailString c)) = ("detailString", toJSON c)
      toDetailJSON (Just (GoalTargetDetailBoolean c)) = ("detailBoolean", toJSON c)
      toDetailJSON (Just (GoalTargetDetailInteger c)) = ("detailInteger", toJSON c)
      toDetailJSON (Just (GoalTargetDetailRatio c)) = ("detailRatio", toJSON c)
      toDueJSON (     Nothing   ) = ("due", Null)
      toDueJSON (Just (GoalTargetDueDate c)) = ("dueDate", toJSON c)
      toDueJSON (Just (GoalTargetDueDuration c)) = ("dueDuration", toJSON c)
instance FromJSON GoalTarget where
  parseJSON = withObject "GoalTarget" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        measure <- o .:? "measure"
        detail <- parseDetail o
        due <- parseDue o
        return GoalTarget{
            goalTargetAttrId = id
          , goalTargetExtension = extension
          , goalTargetModifierExtension = modifierExtension
          , goalTargetMeasure = measure
          , goalTargetDetail = detail
          , goalTargetDue = due
          }
    where 
      parseDetail o = parseDetailQuantity o <|> parseDetailRange o <|> parseDetailCodeableConcept o <|> parseDetailString o <|> parseDetailBoolean o <|> parseDetailInteger o <|> parseDetailRatio o
      parseDetailQuantity o = do
                has <- o .: "detailQuantity"
                return $ Just (GoalTargetDetailQuantity has)
      parseDetailRange o = do
                has <- o .: "detailRange"
                return $ Just (GoalTargetDetailRange has)
      parseDetailCodeableConcept o = do
                has <- o .: "detailCodeableConcept"
                return $ Just (GoalTargetDetailCodeableConcept has)
      parseDetailString o = do
                has <- o .: "detailString"
                return $ Just (GoalTargetDetailString has)
      parseDetailBoolean o = do
                has <- o .: "detailBoolean"
                return $ Just (GoalTargetDetailBoolean has)
      parseDetailInteger o = do
                has <- o .: "detailInteger"
                return $ Just (GoalTargetDetailInteger has)
      parseDetailRatio o = do
                has <- o .: "detailRatio"
                return $ Just (GoalTargetDetailRatio has)
      parseDue o = parseDueDate o <|> parseDueDuration o
      parseDueDate o = do
                has <- o .: "dueDate"
                return $ Just (GoalTargetDueDate has)
      parseDueDuration o = do
                has <- o .: "dueDuration"
                return $ Just (GoalTargetDueDuration has)
instance Xmlbf.ToXml GoalTarget where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (goalTargetAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (goalTargetExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (goalTargetModifierExtension p))
             , OptProp  "measure" (fmap Xmlbf.toXml (goalTargetMeasure p))
             , toDetailXml (goalTargetDetail p)
             , toDueXml (goalTargetDue p)
             ]
       where 
          toDetailXml ( Nothing   ) = (OptVal "detail" Nothing)
          toDetailXml (Just (GoalTargetDetailQuantity p)) = Prop  "detailQuantity" (HM.empty, Xmlbf.toXml p)
          toDetailXml (Just (GoalTargetDetailRange p)) = Prop  "detailRange" (HM.empty, Xmlbf.toXml p)
          toDetailXml (Just (GoalTargetDetailCodeableConcept p)) = Prop  "detailCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toDetailXml (Just (GoalTargetDetailString p)) = Val   "detailString" (toString p)
          toDetailXml (Just (GoalTargetDetailBoolean p)) = Val   "detailBoolean" (toBoolean p)
          toDetailXml (Just (GoalTargetDetailInteger p)) = Val   "detailInteger" (toInt p)
          toDetailXml (Just (GoalTargetDetailRatio p)) = Prop  "detailRatio" (HM.empty, Xmlbf.toXml p)
          toDueXml ( Nothing   ) = (OptVal "due" Nothing)
          toDueXml (Just (GoalTargetDueDate p)) = Val   "dueDate" (toDate p)
          toDueXml (Just (GoalTargetDueDuration p)) = Prop  "dueDuration" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml GoalTarget where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    measure <- optional $ Xmlbf.pElement "measure" Xmlbf.fromXml
    detail <- fromDetailXml
    due <- fromDueXml
    return GoalTarget {
            goalTargetAttrId = id
          , goalTargetExtension = extension
          , goalTargetModifierExtension = modifierExtension
          , goalTargetMeasure = measure
          , goalTargetDetail = detail
          , goalTargetDue = due
          }

    where 
      fromDetailXml = parseDetailQuantity <|> parseDetailRange <|> parseDetailCodeableConcept <|> parseDetailString <|> parseDetailBoolean <|> parseDetailInteger <|> parseDetailRatio <|> pure Nothing
      parseDetailQuantity = do
                has <- Xmlbf.pElement "detailQuantity" Xmlbf.fromXml
                return $ Just (GoalTargetDetailQuantity (                      has))
      parseDetailRange = do
                has <- Xmlbf.pElement "detailRange" Xmlbf.fromXml
                return $ Just (GoalTargetDetailRange (                      has))
      parseDetailCodeableConcept = do
                has <- Xmlbf.pElement "detailCodeableConcept" Xmlbf.fromXml
                return $ Just (GoalTargetDetailCodeableConcept (                      has))
      parseDetailString = do
                has <- Xmlbf.pElement "detailString" (Xmlbf.pAttr "value")
                return $ Just (GoalTargetDetailString (     fromString has))
      parseDetailBoolean = do
                has <- Xmlbf.pElement "detailBoolean" (Xmlbf.pAttr "value")
                return $ Just (GoalTargetDetailBoolean (     fromBoolean has))
      parseDetailInteger = do
                has <- Xmlbf.pElement "detailInteger" (Xmlbf.pAttr "value")
                return $ Just (GoalTargetDetailInteger (     fromInt has))
      parseDetailRatio = do
                has <- Xmlbf.pElement "detailRatio" Xmlbf.fromXml
                return $ Just (GoalTargetDetailRatio (                      has))
      fromDueXml = parseDueDate <|> parseDueDuration <|> pure Nothing
      parseDueDate = do
                has <- Xmlbf.pElement "dueDate" (Xmlbf.pAttr "value")
                return $ Just (GoalTargetDueDate (     fromDate has))
      parseDueDuration = do
                has <- Xmlbf.pElement "dueDuration" Xmlbf.fromXml
                return $ Just (GoalTargetDueDuration (                      has))



