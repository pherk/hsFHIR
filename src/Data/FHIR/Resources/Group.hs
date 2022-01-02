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
-- FHIR 4.0.0 Group
--

module Data.FHIR.Resources.Group where

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

data GroupType
    = GTPerson
    | GTAnimal
    | GTPractitioner
    | GTDevice
    | GTMedication
    | GTSubstance
  deriving (Eq, Show)

instance ToJSON GroupType where
    toJSON GTPerson = String "person"
    toJSON GTAnimal = String "animal"
    toJSON GTPractitioner = String "practitioner"
    toJSON GTDevice = String "device"
    toJSON GTMedication = String "medication"
    toJSON GTSubstance = String "substance"
instance FromJSON GroupType where
    parseJSON "person" = return GTPerson
    parseJSON "animal" = return GTAnimal
    parseJSON "practitioner" = return GTPractitioner
    parseJSON "device" = return GTDevice
    parseJSON "medication" = return GTMedication
    parseJSON "substance" = return GTSubstance

toGroupType GTPerson = "person"
toGroupType GTAnimal = "animal"
toGroupType GTPractitioner = "practitioner"
toGroupType GTDevice = "device"
toGroupType GTMedication = "medication"
toGroupType GTSubstance = "substance"
fromGroupType "person" = GTPerson
fromGroupType "animal" = GTAnimal
fromGroupType "practitioner" = GTPractitioner
fromGroupType "device" = GTDevice
fromGroupType "medication" = GTMedication
fromGroupType "substance" = GTSubstance


data Group = Group {
    groupId :: Maybe Id
  , groupMeta :: Maybe Meta
  , groupImplicitRules :: Maybe Uri
  , groupLanguage :: Maybe Language
  , groupText :: Maybe Narrative
--    groupContained :: [ResourceContainer]
  , groupExtension :: [Extension]
  , groupModifierExtension :: [Extension]
  , groupIdentifier :: [Identifier]
  , groupActive :: Maybe Boolean
  , groupType :: GroupType
  , groupActual :: Boolean
  , groupCode :: Maybe CodeableConcept
  , groupName :: Maybe Text
  , groupQuantity :: Maybe UnsignedInt
  , groupManagingEntity :: Maybe Reference
  , groupCharacteristic :: [GroupCharacteristic]
  , groupMember :: [GroupMember]
  }
--

instance ToJSON Group where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
     ("resourceType" , String "Group")
    ,  "id" .= toJSON (groupId p)
    ,  "meta" .= toJSON (groupMeta p)
    ,  "implicitRules" .= toJSON (groupImplicitRules p)
    ,  "language" .= toJSON (groupLanguage p)
    ,  "text" .= toJSON (groupText p)
--    , "contained" .= toJSON (groupContained p)
    ,  "extension" .= toJSON (groupExtension p)
    ,  "modifierExtension" .= toJSON (groupModifierExtension p)
    ,  "identifier" .= toJSON (groupIdentifier p)
    ,  "active" .= toJSON (groupActive p)
    ,  "type" .= toJSON (groupType p)
    ,  "actual" .= toJSON (groupActual p)
    ,  "code" .= toJSON (groupCode p)
    ,  "name" .= toJSON (groupName p)
    ,  "quantity" .= toJSON (groupQuantity p)
    ,  "managingEntity" .= toJSON (groupManagingEntity p)
    ,  "characteristic" .= toJSON (groupCharacteristic p)
    ,  "member" .= toJSON (groupMember p)
    ]
instance FromJSON Group where
  parseJSON = withObject "Group" $ \o -> do
    rt     <- o .:  "resourceType"  :: Parser Text
    case rt of
      "Group" -> do
        id <- o .:? "id"
        meta <- o .:? "meta"
        implicitRules <- o .:? "implicitRules"
        language <- o .:? "language"
        text <- o .:? "text"
--        contained <- o .:? "contained" .!= []
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        identifier <- o .:? "identifier" .!= []
        active <- o .:? "active"
        ty <- o .:  "type"
        actual <- o .:  "actual"
        code <- o .:? "code"
        name <- o .:? "name"
        quantity <- o .:? "quantity"
        managingEntity <- o .:? "managingEntity"
        characteristic <- o .:? "characteristic" .!= []
        member <- o .:? "member" .!= []
        return Group{
            groupId = id
          , groupMeta = meta
          , groupImplicitRules = implicitRules
          , groupLanguage = language
          , groupText = text
--          , groupContained = contained
          , groupExtension = extension
          , groupModifierExtension = modifierExtension
          , groupIdentifier = identifier
          , groupActive = active
          , groupType = ty
          , groupActual = actual
          , groupCode = code
          , groupName = name
          , groupQuantity = quantity
          , groupManagingEntity = managingEntity
          , groupCharacteristic = characteristic
          , groupMember = member
          }
      _ -> fail "not a Group"
instance Xmlbf.ToXml Group where
  toXml p = Xmlbf.element "Group" as cs
    where as = HM.fromList $ catMaybes $
                 fmap toAttr [
                     Val "xmlns" "http://hl7.org/fhir"
                  -- OptVal "xml:id" (domainResourceAttribs ps)
                   ]
          cs = concatMap toElement $
             [
               OptVal   "id" (fmap toId (groupId p))
             , OptProp  "meta" (fmap Xmlbf.toXml (groupMeta p))
             , OptVal   "implicitRules" (fmap toUri (groupImplicitRules p))
             , OptVal   "language" (fmap toLanguage (groupLanguage p))
             , OptProp  "text" (fmap Xmlbf.toXml (groupText p))
--             , PropList "contained" (fmap Xmlbf.toXml (groupContained p))
             , PropList "extension" (fmap Xmlbf.toXml (groupExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (groupModifierExtension p))
             , PropList "identifier" (fmap Xmlbf.toXml (groupIdentifier p))
             , OptVal   "active" (fmap toBoolean (groupActive p))
             , Val      "type" (     toGroupType (groupType p))
             , Val      "actual" (     toBoolean (groupActual p))
             , OptProp  "code" (fmap Xmlbf.toXml (groupCode p))
             , OptVal   "name" (fmap toString (groupName p))
             , OptVal   "quantity" (fmap toUnsignedInt (groupQuantity p))
             , OptProp  "managingEntity" (fmap Xmlbf.toXml (groupManagingEntity p))
             , PropList "characteristic" (fmap Xmlbf.toXml (groupCharacteristic p))
             , PropList "member" (fmap Xmlbf.toXml (groupMember p))
             ]
instance Xmlbf.FromXml Group where
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
    active <- optional $ Xmlbf.pElement "active" (Xmlbf.pAttr "value")
    ty <-            Xmlbf.pElement "type" (Xmlbf.pAttr "value")
    actual <-            Xmlbf.pElement "actual" (Xmlbf.pAttr "value")
    code <- optional $ Xmlbf.pElement "code" Xmlbf.fromXml
    name <- optional $ Xmlbf.pElement "name" (Xmlbf.pAttr "value")
    quantity <- optional $ Xmlbf.pElement "quantity" (Xmlbf.pAttr "value")
    managingEntity <- optional $ Xmlbf.pElement "managingEntity" Xmlbf.fromXml
    characteristic <- many     $ Xmlbf.pElement "characteristic" Xmlbf.fromXml
    member <- many     $ Xmlbf.pElement "member" Xmlbf.fromXml
    return Group {
            groupId = fmap fromId id
          , groupMeta = meta
          , groupImplicitRules = fmap fromUri implicitRules
          , groupLanguage = fmap fromLanguage language
          , groupText = text
--          , groupContained = contained
          , groupExtension = extension
          , groupModifierExtension = modifierExtension
          , groupIdentifier = identifier
          , groupActive = fmap fromBoolean active
          , groupType =      fromGroupType ty
          , groupActual =      fromBoolean actual
          , groupCode = code
          , groupName = fmap fromString name
          , groupQuantity = fmap fromUnsignedInt quantity
          , groupManagingEntity = managingEntity
          , groupCharacteristic = characteristic
          , groupMember = member
          }



data GroupCharacteristicValue
    = GroupCharacteristicValueCodeableConcept CodeableConcept
    | GroupCharacteristicValueBoolean Boolean
    | GroupCharacteristicValueQuantity Quantity
    | GroupCharacteristicValueRange Range
    | GroupCharacteristicValueReference Reference
    deriving (Eq, Show)

data GroupCharacteristic = GroupCharacteristic {
    groupCharacteristicAttrId :: Maybe Text
  , groupCharacteristicExtension :: [Extension]
  , groupCharacteristicModifierExtension :: [Extension]
  , groupCharacteristicCode :: CodeableConcept
  , groupCharacteristicValue :: GroupCharacteristicValue
  , groupCharacteristicExclude :: Boolean
  , groupCharacteristicPeriod :: Maybe Period
  }
--

instance ToJSON GroupCharacteristic where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (groupCharacteristicAttrId p)
    ,  "extension" .= toJSON (groupCharacteristicExtension p)
    ,  "modifierExtension" .= toJSON (groupCharacteristicModifierExtension p)
    ,  "code" .= toJSON (groupCharacteristicCode p)
    , toValueJSON (groupCharacteristicValue p)
    ,  "exclude" .= toJSON (groupCharacteristicExclude p)
    ,  "period" .= toJSON (groupCharacteristicPeriod p)
    ]
    where 
      toValueJSON (     (GroupCharacteristicValueCodeableConcept c)) = ("value", toJSON c)
      toValueJSON (     (GroupCharacteristicValueBoolean c)) = ("value", toJSON c)
      toValueJSON (     (GroupCharacteristicValueQuantity c)) = ("value", toJSON c)
      toValueJSON (     (GroupCharacteristicValueRange c)) = ("value", toJSON c)
      toValueJSON (     (GroupCharacteristicValueReference c)) = ("value", toJSON c)
instance FromJSON GroupCharacteristic where
  parseJSON = withObject "GroupCharacteristic" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        code <- o .:  "code"
        value <- parseValue o
        exclude <- o .:  "exclude"
        period <- o .:? "period"
        return GroupCharacteristic{
            groupCharacteristicAttrId = id
          , groupCharacteristicExtension = extension
          , groupCharacteristicModifierExtension = modifierExtension
          , groupCharacteristicCode = code
          , groupCharacteristicValue = value
          , groupCharacteristicExclude = exclude
          , groupCharacteristicPeriod = period
          }
    where 
      parseValue o = parseValueCodeableConcept o <|> parseValueBoolean o <|> parseValueQuantity o <|> parseValueRange o <|> parseValueReference o
      parseValueCodeableConcept o = do
                has <- o .: "valueCodeableConcept"
                return $ GroupCharacteristicValueCodeableConcept has
      parseValueBoolean o = do
                has <- o .: "valueBoolean"
                return $ GroupCharacteristicValueBoolean has
      parseValueQuantity o = do
                has <- o .: "valueQuantity"
                return $ GroupCharacteristicValueQuantity has
      parseValueRange o = do
                has <- o .: "valueRange"
                return $ GroupCharacteristicValueRange has
      parseValueReference o = do
                has <- o .: "valueReference"
                return $ GroupCharacteristicValueReference has
instance Xmlbf.ToXml GroupCharacteristic where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (groupCharacteristicAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (groupCharacteristicExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (groupCharacteristicModifierExtension p))
             , Prop     "code" (HM.empty, Xmlbf.toXml (groupCharacteristicCode p))
             , toValueXml (groupCharacteristicValue p)
             , Val      "exclude" (     toBoolean (groupCharacteristicExclude p))
             , OptProp  "period" (fmap Xmlbf.toXml (groupCharacteristicPeriod p))
             ]
       where 
          toValueXml (     (GroupCharacteristicValueCodeableConcept p)) = Prop     "valueCodeableConcept" (HM.empty, Xmlbf.toXml p)
          toValueXml (     (GroupCharacteristicValueBoolean p)) = Val      "valueBoolean" (     toBoolean p)
          toValueXml (     (GroupCharacteristicValueQuantity p)) = Prop     "valueQuantity" (HM.empty, Xmlbf.toXml p)
          toValueXml (     (GroupCharacteristicValueRange p)) = Prop     "valueRange" (HM.empty, Xmlbf.toXml p)
          toValueXml (     (GroupCharacteristicValueReference p)) = Prop     "valueReference" (HM.empty, Xmlbf.toXml p)
instance Xmlbf.FromXml GroupCharacteristic where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    code <-            Xmlbf.pElement "code" Xmlbf.fromXml
    value <- fromValueXml
    exclude <-            Xmlbf.pElement "exclude" (Xmlbf.pAttr "value")
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    return GroupCharacteristic {
            groupCharacteristicAttrId = id
          , groupCharacteristicExtension = extension
          , groupCharacteristicModifierExtension = modifierExtension
          , groupCharacteristicCode = code
          , groupCharacteristicValue = value
          , groupCharacteristicExclude =      fromBoolean exclude
          , groupCharacteristicPeriod = period
          }

    where 
      fromValueXml = parseValueCodeableConcept <|> parseValueBoolean <|> parseValueQuantity <|> parseValueRange <|> parseValueReference
      parseValueCodeableConcept = do
                has <- Xmlbf.pElement "valueCodeableConcept" Xmlbf.fromXml
                return $ GroupCharacteristicValueCodeableConcept (                      has)
      parseValueBoolean = do
                has <- Xmlbf.pElement "valueBoolean" (Xmlbf.pAttr "value")
                return $ GroupCharacteristicValueBoolean (     fromBoolean has)
      parseValueQuantity = do
                has <- Xmlbf.pElement "valueQuantity" Xmlbf.fromXml
                return $ GroupCharacteristicValueQuantity (                      has)
      parseValueRange = do
                has <- Xmlbf.pElement "valueRange" Xmlbf.fromXml
                return $ GroupCharacteristicValueRange (                      has)
      parseValueReference = do
                has <- Xmlbf.pElement "valueReference" Xmlbf.fromXml
                return $ GroupCharacteristicValueReference (                      has)


data GroupMember = GroupMember {
    groupMemberAttrId :: Maybe Text
  , groupMemberExtension :: [Extension]
  , groupMemberModifierExtension :: [Extension]
  , groupMemberEntity :: Reference
  , groupMemberPeriod :: Maybe Period
  , groupMemberInactive :: Maybe Boolean
  }
--

instance ToJSON GroupMember where
  toJSON p = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      "id" .= toJSON (groupMemberAttrId p)
    ,  "extension" .= toJSON (groupMemberExtension p)
    ,  "modifierExtension" .= toJSON (groupMemberModifierExtension p)
    ,  "entity" .= toJSON (groupMemberEntity p)
    ,  "period" .= toJSON (groupMemberPeriod p)
    ,  "inactive" .= toJSON (groupMemberInactive p)
    ]
instance FromJSON GroupMember where
  parseJSON = withObject "GroupMember" $ \o -> do
        id <- o .:? "id"
        extension <- o .:? "extension" .!= []
        modifierExtension <- o .:? "modifierExtension" .!= []
        entity <- o .:  "entity"
        period <- o .:? "period"
        inactive <- o .:? "inactive"
        return GroupMember{
            groupMemberAttrId = id
          , groupMemberExtension = extension
          , groupMemberModifierExtension = modifierExtension
          , groupMemberEntity = entity
          , groupMemberPeriod = period
          , groupMemberInactive = inactive
          }
instance Xmlbf.ToXml GroupMember where
  toXml p = concatMap toElement $
             [
               OptVal "id"   (groupMemberAttrId p)
             , PropList "extension" (fmap Xmlbf.toXml (groupMemberExtension p))
             , PropList "modifierExtension" (fmap Xmlbf.toXml (groupMemberModifierExtension p))
             , Prop     "entity" (HM.empty, Xmlbf.toXml (groupMemberEntity p))
             , OptProp  "period" (fmap Xmlbf.toXml (groupMemberPeriod p))
             , OptVal   "inactive" (fmap toBoolean (groupMemberInactive p))
             ]
instance Xmlbf.FromXml GroupMember where
  fromXml = do
    id <- optional $ Xmlbf.pAttr "id"
    extension <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
    modifierExtension <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
    entity <-            Xmlbf.pElement "entity" Xmlbf.fromXml
    period <- optional $ Xmlbf.pElement "period" Xmlbf.fromXml
    inactive <- optional $ Xmlbf.pElement "inactive" (Xmlbf.pAttr "value")
    return GroupMember {
            groupMemberAttrId = id
          , groupMemberExtension = extension
          , groupMemberModifierExtension = modifierExtension
          , groupMemberEntity = entity
          , groupMemberPeriod = period
          , groupMemberInactive = fmap fromBoolean inactive
          }




