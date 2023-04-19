{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators     #-}

{-
 - FHIR Datatypes v4.0.1
 - deviations
 -   primitive datatype cannot have extensions
 -   date and dateTime
 -   super element id and extension folded in
 -   super backBoneElement is not folded
 - Todos
 -   real toJSON and parseJSON instances
 -   toXMl, parseXML instances 
 -   Extension props when TH elminated
-}
module Data.FHIR.Datatypes.Internal where

import Data.Aeson  
import Data.Aeson.Types hiding (parseJSON)
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key                as AK
import qualified Data.Aeson.KeyMap             as AKM
#endif

import Data.Int(Int64)
import Data.Scientific (Scientific)
--import Data.Time.ISO8601.Duration
--import Errors
import Data.Time.Calendar(Day)
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime(ZonedTime)
import RIO
import qualified RIO.List.Partial as RL
import qualified RIO.Partial as RP
import qualified RIO.HashMap as HM
import qualified RIO.Text  as T
import qualified RIO.Vector as V
import           Data.FHIR.Datatypes.XhtmlDiv


{-
 - Base datatypes
-}
-- type XMLString = Text
type Base64Binary = Text
-- type Base64BinaryPrimitive = Text
type Boolean = Bool
-- type BooleanPrimitive = Bool
type Canonical = Text
-- type CanonicalPrimitive = Text
type Code = Text
-- type CodePrimitive = Text
type Date = Text
type DateTime = Text
-- type DatePrimitive = Text
-- type DateTimePrimitive = Text
type Decimal = Double
-- type DecimalPrimitive = Double
type Id = Text
-- type IdPrimitive = Text
type Instant = Text
-- type InstantPrimitive = Text
-- type IntegerPrimitive = Integer
type Markdown = Text
-- type MarkdownPrimitive = Text
type NText = Text
type Oid = Text
-- type OidPrimitive = Text
type PositiveInt = Int 
-- type PositiveIntPrimitive = Integer
type SampledDataDataType = Text
-- type SampledDataDataTypePrimitive = Text
-- type StringPrimitive = Text
type Time = Text
-- type TimePrimitive = Text
type UnsignedInt = Int
-- type UnsignedIntPrimitive = Integer
-- type UriPrimitive = Text
-- type UrlPrimitive = Text
-- type UuidPrimitive = Text
type Ucum = Text
type Uri  = Text
type Url  = Text
type Uuid = Text

toInt :: Integer -> Text
toInt = T.pack . show
fromInt :: Text -> Integer
fromInt = RP.read . T.unpack 
toInstant :: Instant -> Text
toInstant = id
fromInstant :: Text -> Instant
fromInstant = id
toPositiveInt :: PositiveInt -> Text
toPositiveInt = T.pack . show
fromPositiveInt :: Text -> PositiveInt
fromPositiveInt = RP.read . T.unpack 
toUnsignedInt :: UnsignedInt -> Text
toUnsignedInt = T.pack . show
fromUnsignedInt :: Text -> UnsignedInt
fromUnsignedInt = RP.read . T.unpack 
toDecimal :: Decimal -> Text
toDecimal = T.pack . show
fromDecimal :: Text -> Decimal
fromDecimal = RP.read . T.unpack 
toBase64Binary :: Base64Binary -> Text
toBase64Binary = id
fromBase64Binary :: Text -> Base64Binary
fromBase64Binary = id
toBoolean True  = "true"
toBoolean False = "false"
fromBoolean "true" = True
fromBoolean "True" = True
fromBoolean _ = False
toCanonical :: Canonical -> Text
toCanonical = id
fromCanonical :: Text -> Canonical
fromCanonical = id
toCode :: Code -> Text
toCode = id
fromCode :: Text -> Code
fromCode = id
toDate :: Date -> Text
toDate = id
fromDate :: Text -> Date
fromDate = id
toDateTime :: DateTime -> Text
toDateTime = id
fromDateTime :: Text -> DateTime
fromDateTime = id
toId :: Id -> Text
toId = id
fromId :: Text -> Id
fromId = id
toMarkdown  :: Markdown  -> Text
toMarkdown  = id
fromMarkdown  :: Text -> Markdown 
fromMarkdown  = id
toOid :: Oid -> Text
toOid = id
fromOid :: Text -> Oid
fromOid = id
-- toString :: String -> Text
toString = id
-- fromString :: Text -> String
fromString = id
toTime :: Time -> Text
toTime = id
fromTime :: Text -> Time
fromTime = id
toUri :: Uri -> Text
toUri = id
fromUri :: Text -> Uri
fromUri = id
toUrl :: Url -> Text
toUrl = id
fromUrl :: Text -> Url
fromUrl = id
toUuid :: Uuid -> Text
toUuid = id
fromUuid :: Text -> Uuid
fromUuid = id

data AnyAttrib = AnyAttrib (Text,Text)
    deriving (Eq, Show)

{-
data Element
    = Element { elementId :: Maybe Text
              , elementExtension :: [Extension]
              }
    deriving (Eq, Show)

instance ToJSON Element where
  toJSON (Element{elementId=Nothing})  = object []
  toJSON (Element{elementId=Just t})  = object ["id" .= toJSON t]

instance FromJSON Element where
  parseJSON = withObject "Element" $ \obj -> do
    id <- obj .:? "id"
    return (Element {elementId = id })
-}

--  <birthDate id="314159" value="1970-03-30" >
--    <extension url="http://example.org/fhir/StructureDefinition/text">
--      <valueString value="Easter 1970"/>
--    </extension>
--  </birthDate>
-- 
-- is represented in JSON as:
-- 
--  "birthDate": "1970-03-30",
--  "_birthDate": {
--    "id": "314159",
--    "extension" : [ {
--       "url" : "http://example.org/fhir/StructureDefinition/text",
--       "valueString" : "Easter 1970"
--    }]
--  }
-- 


data BackboneElement
    = BackboneElement {
              backboneElementAttribs :: [AnyAttrib]
            , backboneElementId :: Maybe Id
            , backboneElementExtension :: [Extension]
            , backboneElementModifierExtension :: [Extension]
            }
    deriving (Eq, Show)

toBackboneElementJSON c = 
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
          "id"                .= toJSON (backboneElementId c)
        , "extension"         .= toJSON (backboneElementExtension c)
        , "modifierExtension" .= toJSON (backboneElementModifierExtension c)
        ]
parseBackboneElementJSON o = do 
        i <- o .:? "id"
        e <- o .:? "extension" .!= []
        m <- o .:? "modifierExtension" .!= []
        return $ BackboneElement{
                     backboneElementAttribs=[]
                   , backboneElementId=i
                   , backboneElementExtension=e
                   , backboneElementModifierExtension=m
                   }

instance ToJSON BackboneElement where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
          "id"  .= toJSON (backboneElementId c)
        , "extension"         .= toJSON (backboneElementExtension c)
        , "modifierExtension" .= toJSON (backboneElementModifierExtension c)
        ]
instance FromJSON BackboneElement where
    parseJSON = withObject "BackboneElement" $ \o -> do
        i <- o .:? "id"
        e <- o .:? "extension" .!= []
        m <- o .:? "modifierExtension" .!= []
        return $ BackboneElement{
                     backboneElementAttribs=[]
                   , backboneElementId=i
                   , backboneElementExtension=e
                   , backboneElementModifierExtension=m
                   }

toAddressType AtPostal   = "postal"
toAddressType AtPhysical = "physical"
toAddressType AtBoth     = "both"
fromAddressType "postal"   = AtPostal
fromAddressType "physical" = AtPhysical
fromAddressType "both"     = AtBoth

toAddressUse AuBilling = "billing"
toAddressUse AuHome    = "home"
toAddressUse AuOld     = "old"
toAddressUse AuTemp    = "temp"
toAddressUse AuWork    = "work"
fromAddressUse "billing"   = AuBilling
fromAddressUse "home" = AuHome
fromAddressUse "old"  = AuOld
fromAddressUse "temp" = AuTemp
fromAddressUse "work" = AuWork

toAdministrativeGender AgFemale   = "female"
toAdministrativeGender AgMale     = "male"
toAdministrativeGender AgOther    = "other"
toAdministrativeGender AgUnknown  = "unknown"
fromAdministrativeGender "female"  = AgFemale
fromAdministrativeGender "male"    = AgMale
fromAdministrativeGender "other"   = AgOther
fromAdministrativeGender "unknown" = AgUnknown

fromContactPointSystem "email" = CpsEmail
fromContactPointSystem "fax"   = CpsFax
fromContactPointSystem "other" = CpsOther 
fromContactPointSystem "pager" = CpsPager 
fromContactPointSystem "phone" = CpsPhone 
fromContactPointSystem "sms"   = CpsSms 
fromContactPointSystem "url"   = CpsUrl
toContactPointSystem CpsEmail = "email"
toContactPointSystem CpsFax   = "fax"
toContactPointSystem CpsOther = "other"
toContactPointSystem CpsPager = "pager"
toContactPointSystem CpsPhone = "phone"
toContactPointSystem CpsSms   = "sms"
toContactPointSystem CpsUrl   = "url"

fromContactPointUse "home" = CpuHome
fromContactPointUse "mobile"=CpuMobile
fromContactPointUse "old"  = CpuOld
fromContactPointUse "temp" = CpuTemp
fromContactPointUse "work" = CpuWork
toContactPointUse CpuHome = "home"
toContactPointUse CpuMobile = "mobile"
toContactPointUse CpuOld = "old"
toContactPointUse CpuTemp = "temp"
toContactPointUse CpuWork = "work"

toFHIRVersion Fv_001 = "0.01"
toFHIRVersion Fv_005 = "0.05"
toFHIRVersion Fv_006 = "0.06"
toFHIRVersion Fv_011 = "0.11"
toFHIRVersion Fv_0080= "0.0.80"
toFHIRVersion Fv_0081= "0.0.81"
toFHIRVersion Fv_0082= "0.0.82"
toFHIRVersion Fv_040 = "0.4.0"
toFHIRVersion Fv_050 = "0.5.0"
toFHIRVersion Fv_100 = "1.0.0"
toFHIRVersion Fv_101 = "1.0.1"
toFHIRVersion Fv_102 = "1.0.2"
toFHIRVersion Fv_110 = "1.1.0"
toFHIRVersion Fv_140 = "1.4.0"
toFHIRVersion Fv_160 = "1.6.0"
toFHIRVersion Fv_180 = "1.8.0"
toFHIRVersion Fv_300 = "3.0.0"
toFHIRVersion Fv_301 = "3.0.1"
toFHIRVersion Fv_330 = "3.3.0"
toFHIRVersion Fv_350 = "3.5.0"
toFHIRVersion Fv_400 = "4.0.0"
toFHIRVersion Fv_401 = "4.0.1"
fromFHIRVersion "0.01" = Fv_001
fromFHIRVersion "0.05" = Fv_005
fromFHIRVersion "0.06" = Fv_006
fromFHIRVersion "0.11" = Fv_011
fromFHIRVersion "0.0.80" = Fv_0080
fromFHIRVersion "0.0.81" = Fv_0081
fromFHIRVersion "0.0.82" = Fv_0082
fromFHIRVersion "0.4.0" = Fv_040
fromFHIRVersion "0.5.0" = Fv_050
fromFHIRVersion "1.0.0" = Fv_100
fromFHIRVersion "1.0.1" = Fv_101
fromFHIRVersion "1.0.2" = Fv_102
fromFHIRVersion "1.1.0" = Fv_110
fromFHIRVersion "1.4.0" = Fv_140
fromFHIRVersion "1.6.0" = Fv_160
fromFHIRVersion "1.8.0" = Fv_180
fromFHIRVersion "3.0.0" = Fv_300
fromFHIRVersion "3.0.1" = Fv_301
fromFHIRVersion "3.3.0" = Fv_330
fromFHIRVersion "3.5.0" = Fv_350
fromFHIRVersion "4.0.0" = Fv_400
fromFHIRVersion "4.0.1" = Fv_401

fromIdentifierUse "official" = IuOfficial 
fromIdentifierUse "old"      = IuOld
fromIdentifierUse "secondary"= IuSecondary
fromIdentifierUse "temp"     = IuTemp
fromIdentifierUse "usual"    = IuUsual
toIdentifier IuOfficial = "official"  
toIdentifier IuOld      = "old"       
toIdentifier IuSecondary= "secondary" 
toIdentifier IuTemp     = "temp"      
toIdentifier IuUsual    = "usual"     

fromLinkType "refer"       = LtRefer
fromLinkType "replaced-by" = LtReplacedBy
fromLinkType "replaces"    = LtReplaces
fromLinkType "seealso"     = LtSeeAlso
toLinkType LtRefer      = "refer"
toLinkType LtReplacedBy = "replaced-by"
toLinkType LtReplaces   = "replaces"
toLinkType LtSeeAlso    = "seealso"

data Language 
  = LangDE
  | LangUS
  deriving (Eq, Show)

instance ToJSON Language where
  toJSON LangDE = String "de"
  toJSON LangUS = String "us"
instance FromJSON Language where
  parseJSON "de"  = return LangDE
  parseJSON "deu" = return LangDE
  parseJSON "us"  = return LangUS

toLanguage LangDE  = "de"
toLanguage LangUS  = "us"
fromLanguage "de"  = LangDE
fromLanguage "deu" = LangDE
fromLanguage "us"  = LangUS

instance FromJSON MimeType where
  parseJSON "xml"  = return MtXml
  parseJSON "json" = return MtJson
  parseJSON "ttl"  = return MtTtl

toMimeType MtXml = "xml"
toMimeType MtJson = "json"
toMimeType MtTtl  = "ttl"
fromMimeType "xml"  = MtXml
fromMimeType "json" = MtJson
fromMimeType "ttl"  = MtTtl

fromNameUse "anonymous" = NuAnonymous
fromNameUse "nickname"  = NuNickname
fromNameUse "official"  = NuOfficial
fromNameUse "old"       = NuOld
fromNameUse "temp"      = NuTemp      
fromNameUse "usual"     = NuUsual     
toNameUse NuAnonymous = "anonymous"
toNameUse NuNickname = "nickname"
toNameUse NuOfficial = "official"
toNameUse NuOld      = "old"
toNameUse NuTemp     = "temp"
toNameUse NuUsual    = "usual"

toNarrativeStatus NsAdditional = "additional"
toNarrativeStatus NsEmpty      = "empty"
toNarrativeStatus NsExtensions = "extensions"
toNarrativeStatus NsGenerated  = "generated"
fromNarrativeStatus "additional" = NsAdditional
fromNarrativeStatus "empty"      = NsEmpty   
fromNarrativeStatus "extensions" = NsExtensions
fromNarrativeStatus "generated"  = NsGenerated

toPublicationStatus (PsActive)  = "active"
toPublicationStatus (PsDraft)   = "draft"
toPublicationStatus (PsRetired) = "retired"
toPublicationStatus (PsUnknown) = "unknown"
fromPublicationStatus "active"  = PsActive
fromPublicationStatus "draft"   = PsDraft
fromPublicationStatus "retired" = PsRetired
fromPublicationStatus "unknown" = PsUnknown

toQuantityComparator (QcGt) = "gt"
toQuantityComparator (QcGe) = "ge"
toQuantityComparator (QcLt) = "lt"
toQuantityComparator (QcLe) = "le"
fromQuantityComparator "gt"  = QcGt
fromQuantityComparator "ge"  = QcGe
fromQuantityComparator "lt"  = QcLt
fromQuantityComparator "le"  = QcLe

data Reference
    = Reference {
          referenceAttribs :: HM.HashMap Text Text
        , referenceExtension :: [Extension]
        , referenceReference  :: Maybe Text
        , referenceType       :: Maybe Uri
        , referenceIdentifier :: Maybe Identifier
        , referenceDisplay    :: Maybe Text}
    deriving (Eq, Show)

instance ToJSON Reference where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        toAttribsJSON (referenceAttribs c)
        ++
        [
          "extension"  .= toJSON (referenceExtension c)
        , "reference"  .= toJSON (referenceReference c)
        , "type"       .= toJSON (referenceType c)
        , "identifier" .= toJSON (referenceIdentifier c)
        , "display"    .= toJSON (referenceDisplay c)
        ]
instance FromJSON Reference where
    parseJSON = withObject "Reference" $ \o -> do
        i  <- o .:? "id"
        ext<- o .:? "extension" .!= []
        r  <- o .:? "reference"
        t  <- o .:? "type"
        id <- o .:? "identifier"
        d  <- o .:? "display"
        return $ Reference{
                     referenceAttribs= case i of
                                         Just i' -> HM.fromList [("id",i')]
                                         Nothing -> HM.empty
                   , referenceExtension=ext
                   , referenceReference=r
                   , referenceType=t
                   , referenceIdentifier=id
                   , referenceDisplay=d
                   }

#if MIN_VERSION_aeson(2,0,0)
toAttribsJSON :: HM.HashMap Text Text -> [(Key,Value)]
toAttribsJSON c = fmap toAttribJSON as
    where as = fromObject c
          fromObject = HM.toList

toAttribJSON :: (Text,Text) -> (Key,Value)
toAttribJSON (k,v) = (AK.fromText k) .= toJSON v
#else
toAttribsJSON :: HM.HashMap Text Text -> [(Text,Value)]
toAttribsJSON c = fmap toAttribJSON as
    where as = fromObject c
          fromObject = HM.toList

toAttribJSON :: (Text, Text) -> (Text,Value)
toAttribJSON (k,v) = k .= toJSON v
#endif


data Period
    = Period {
          periodAttribs :: [AnyAttrib]
        , periodId :: Maybe Id
        , periodExtension :: [Extension]
        , periodStart :: Maybe DateTime
        , periodEnd :: Maybe DateTime
        }
        deriving (Eq, Show)

instance ToJSON Period where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
          "id"  .= toJSON (periodId c)
        , "extension"  .= toJSON (periodExtension c)
        , "start"  .= toJSON (periodStart c)
        , "end"  .= toJSON (periodEnd c)
        ]
instance FromJSON Period where
    parseJSON = withObject "Period" $ \o -> do
        i <- o .:? "id"
        ext<- o .:? "extension" .!= []
        s <- o .:? "start"
        e <- o .:? "end"
        return $ Period{
                     periodAttribs=[]
                   , periodExtension=ext
                   , periodId=i
                   , periodStart=s
                   , periodEnd=e
                   }


data QuantityComparator
    = QcGt | QcGe | QcLt | QcLe deriving (Eq, Show)

instance ToJSON QuantityComparator where
    toJSON (QcGt) = String "gt"
    toJSON (QcGe) = String "ge"
    toJSON (QcLt) = String "lt"
    toJSON (QcLe) = String "le"
instance FromJSON QuantityComparator where
    parseJSON (String s) = case T.unpack s of
      "gt"  -> return QcGt
      "ge"  -> return QcGe
      "lt"  -> return QcLt
      "le"  -> return QcLe


data Quantity = Quantity {
          quantityAttribs :: [AnyAttrib]
        , quantityId :: Maybe Id
        , quantityExtension :: [Extension]
        , quantityValue :: Maybe Decimal
        , quantityComparator :: Maybe QuantityComparator
        , quantityUnit :: Maybe Ucum  
        , quantitySystem :: Maybe Uri
        , quantityCode :: Maybe Code}
    deriving (Eq, Show)

instance ToJSON Quantity where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
          "id"  .= toJSON (quantityId c)
        , "extension"  .= toJSON (quantityExtension c)
        , "value"  .= toJSON (quantityValue c)
        , "comparator"  .= toJSON (quantityComparator c)
        , "unit"  .= toJSON (quantityUnit c)
        , "system"  .= toJSON (quantitySystem c)
        , "code"  .= toJSON (quantityCode c)
        ]
instance FromJSON Quantity where
    parseJSON = withObject "Quantity" $ \o -> do
        i <- o .:? "id"
        ext<- o .:? "extension" .!= []
        v <- o .:? "value"
        c <- o .:? "comparator"
        u <- o .:? "unit"
        s <- o .:? "system"
        co <- o .:? "code"
        return $ Quantity{
                     quantityAttribs=[]
                   , quantityId=i
                   , quantityExtension=ext
                   , quantityValue=v
                   , quantityComparator=c
                   , quantityUnit=u
                   , quantitySystem=s
                   , quantityCode=co
                   }

data AddressType
    = AtPostal | AtPhysical | AtBoth
    deriving (Eq, Show)

instance ToJSON AddressType where
    toJSON (AtPostal  ) = String "postal"
    toJSON (AtPhysical) = String "physical"
    toJSON (AtBoth    ) = String "both"
instance FromJSON AddressType where
    parseJSON (String s) = case T.unpack s of
      "postal"   -> return AtPostal
      "physical" -> return AtPhysical
      "both"     -> return AtBoth


data AddressUse
    = AuBilling | AuHome | AuOld | AuTemp | AuWork
    deriving (Eq, Show)

instance ToJSON AddressUse where
    toJSON (AuBilling) = String "billing"
    toJSON (AuHome)    = String "home"
    toJSON (AuOld)     = String "old"
    toJSON (AuTemp)    = String "temp"
    toJSON (AuWork)    = String "work"
instance FromJSON AddressUse where
    parseJSON (String s) = case T.unpack s of
      "billing"   -> return AuBilling
      "home" -> return AuHome
      "old"  -> return AuOld
      "temp" -> return AuTemp
      "work" -> return AuWork

data Address = Address {
          addressId :: Maybe Id
        , addressExtension :: [Extension]
        , addressUse :: Maybe AddressUse
        , addressType :: Maybe AddressType
        , addressText :: Maybe Text
        , addressLine :: [Text]
        , addressCity :: Maybe Text
        , addressDistrict :: Maybe Text
        , addressState :: Maybe Text
        , addressPostalCode :: Maybe Text
        , addressCountry :: Maybe Text
        , addressPeriod :: Maybe Period}
    deriving (Eq, Show)

instance ToJSON Address where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
          "id"         .= toJSON (addressId c)
        , "extension"  .= toJSON (addressExtension c)
        , "use"  .= toJSON (addressUse c)
        , "type"  .= toJSON (addressType c)
        , "text"  .= toJSON (addressText c)
        , "line"  .= toJSON (addressLine c)
        , "city"  .= toJSON (addressCity c)
        , "district"  .= toJSON (addressDistrict c)
        , "state"  .= toJSON (addressState c)
        , "postalCode"  .= toJSON (addressPostalCode c)
        , "country"  .= toJSON (addressCountry c)
        , "period"  .= toJSON (addressPeriod c)
        ]
instance FromJSON Address where
    parseJSON = withObject "Address" $ \o -> do
        id <- o .:? "id"
        ext<- o .:? "extension" .!= []
        u  <- o .:? "use"
        ty <- o .:? "type"
        te <- o .:? "text"
        l  <- o .:? "line" .!= []
        ci <- o .:? "city"
        s  <- o .:? "state"
        d  <- o .:? "district"
        po <- o .:? "postalCode"
        co <- o .:? "country"
        pe <- o .:? "period"
        return $ Address{
                     addressId=id
                   , addressExtension=ext
                   , addressUse=u
                   , addressType=ty
                   , addressText=te
                   , addressLine=l
                   , addressCity=ci
                   , addressState=s
                   , addressDistrict=d
                   , addressPostalCode=po
                   , addressCountry=co
                   , addressPeriod=pe
                   }

data AdministrativeGender
    = AgFemale | AgMale | AgOther | AgUnknown
    deriving (Eq, Show)

instance ToJSON AdministrativeGender where
    toJSON (AgFemale)  = String "female"
    toJSON (AgMale)    = String "male"
    toJSON (AgOther)   = String "other"
    toJSON (AgUnknown) = String "unknown"
instance FromJSON AdministrativeGender where
    parseJSON (String s) = case T.unpack s of
      "female"  -> return AgFemale
      "male"    -> return AgMale
      "other"   -> return AgOther
      "unknown" -> return AgUnknown


data Age = Age { 
          ageAttribs :: [AnyAttrib]
        , ageId :: Maybe Id
        , ageExtension :: [Extension]
        , ageValue :: Maybe Decimal
        , ageComparator :: Maybe QuantityComparator
        , ageUnit :: Maybe Ucum  
        , ageSystem :: Maybe Uri
        , ageCode :: Maybe Code}
    deriving (Eq, Show)

instance ToJSON Age where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
          "id"  .= toJSON (ageId c)
        , "extension"  .= toJSON (ageExtension c)
        , "value"  .= toJSON (ageValue c)
        , "comparator"  .= toJSON (ageComparator c)
        , "unit"  .= toJSON (ageUnit c)
        , "system"  .= toJSON (ageSystem c)
        , "code"  .= toJSON (ageCode c)
        ]
instance FromJSON Age where
    parseJSON = withObject "Quantity" $ \o -> do
        i <- o .:? "id"
        ext<- o .:? "extension" .!= []
        v <- o .:? "value"
        c <- o .:? "comparator"
        u <- o .:? "unit"
        s <- o .:? "system"
        co <- o .:? "code"
        return $ Age{
                     ageAttribs=[]
                   , ageId=i
                   , ageExtension=ext
                   , ageValue=v
                   , ageComparator=c
                   , ageUnit=u
                   , ageSystem=s
                   , ageCode=co
                   }


data AggregationMode
    = AmBundled | AmContained | AmReferenced
    deriving (Eq, Show)

instance ToJSON AggregationMode where
    toJSON (AmBundled)    = String "bundled"
    toJSON (AmContained)  = String "contained"
    toJSON (AmReferenced) = String "referenced"
instance FromJSON AggregationMode where
    parseJSON (String s) = case T.unpack s of
      "bundled"    -> return AmBundled
      "contained"  -> return AmContained
      "referenced" -> return AmReferenced


data AnnotationAuthor
    = AnnotationAuthorReference Reference
    | AnnotationAuthorString Text
    deriving (Eq, Show)

toAnnotationAuthorJSON (AnnotationAuthorReference r) = ("authorReference", toJSON r)
toAnnotationAuthorJSON (AnnotationAuthorString r) = ("authorString", toJSON r)

data Annotation
    = Annotation {
          annotationAttribs :: [AnyAttrib]
        , annotationId :: Maybe Id
        , annotationExtension :: [Extension]
        , annotationAuthor :: Maybe AnnotationAuthor
        , annotationTime :: Maybe DateTime
        , annotationText :: Markdown
        }
    deriving (Eq, Show)

instance ToJSON Annotation where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
          "id"  .= toJSON (annotationId c)
        , "extension"  .= toJSON (annotationExtension c)
        , toAuthorJSON c
        , "time"  .= toJSON (annotationTime c)
        , "text"  .= toJSON (annotationText c)
        ]
        where toAuthorJSON (Annotation{annotationAuthor= Just (AnnotationAuthorReference r)}) = ("authorReference", toJSON r)
              toAuthorJSON (Annotation{annotationAuthor= Just (AnnotationAuthorString    t)}) = ("authorString", toJSON t)
              toAuthorJSON (Annotation{annotationAuthor= Nothing                          }) = ("author", Null)
instance FromJSON Annotation where
    parseJSON = withObject "Annotation" $ \o -> do
        i <- o .:? "id"
        ext<- o .:? "extension" .!= []
        a  <- parseAuthorR o <|> parseAuthorS o <|> parseF
        ti <- o .:? "time"
        te <- o .: "text"
        return $ Annotation{
                     annotationAttribs=[]
                   , annotationId=i
                   , annotationExtension=ext
                   , annotationAuthor=a
                   , annotationTime=ti
                   , annotationText=te
                   }
        where parseAuthorR o = do
                hasRef <- o .: "authorReference" 
                return $ Just (AnnotationAuthorReference hasRef)
              parseAuthorS o = do
                hasT <- o .: "authorString"
                return $ Just (AnnotationAuthorString hasT)
              parseF = do pure Nothing


data Attachment = Attachment {
          attachmentAttribs :: [AnyAttrib]
        , attachmentId :: Maybe Id
        , attachmentExtension :: [Extension]
        , attachmentContentType :: Maybe Code
        , attachmentLanguage :: Maybe Code
        , attachmentData :: Maybe Base64Binary
        , attachmentUrl :: Maybe Url
        , attachmentSize :: Maybe UnsignedInt
        , attachmentHash :: Maybe Base64Binary
        , attachmentTitle :: Maybe Text
        , attachmentCreation :: Maybe DateTime}
    deriving (Eq, Show)

instance ToJSON Attachment where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
          "id"  .= toJSON (attachmentId c)
        , "extension"  .= toJSON (attachmentExtension c)
        , "contentType"  .= toJSON (attachmentContentType c)
        , "language"  .= toJSON (attachmentLanguage c)
        , "data"  .= toJSON (attachmentData c)
        , "url"  .= toJSON (attachmentUrl c)
        , "size"  .= toJSON (attachmentSize c)
        , "hash"  .= toJSON (attachmentHash c)
        , "title"  .= toJSON (attachmentTitle c)
        , "creation"  .= toJSON (attachmentCreation c)
        ]
instance FromJSON Attachment where
    parseJSON = withObject "Attachment" $ \o -> do
        i <- o .:? "id"
        ext<- o .:? "extension" .!= []
        ct <- o .:? "contentType"
        l  <- o .:? "language"
        d  <- o .:? "data"
        u  <- o .:? "url"
        s  <- o .:? "size"
        h  <- o .:? "hash"
        t  <- o .:? "title"
        cr <- o .:? "creation"
        return $ Attachment{
                     attachmentAttribs=[]
                   , attachmentId=i
                   , attachmentExtension=ext
                   , attachmentContentType=ct
                   , attachmentLanguage=l
                   , attachmentData=d
                   , attachmentUrl=u
                   , attachmentSize=s
                   , attachmentHash=h
                   , attachmentTitle=t
                   , attachmentCreation=cr
                   }


data Coding
    = Coding {
          codingAttribs :: [AnyAttrib]
        , codingId :: Maybe Id
        , codingExtension :: [Extension]
        , codingSystem :: Maybe Uri
        , codingVersion :: Maybe Text
        , codingCode :: Maybe Code
        , codingDisplay :: Maybe Text
        , codingUserSelected :: Maybe Boolean}
    deriving (Eq, Show)

instance ToJSON Coding where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
          "id"        .= toJSON (codingId c)
        , "extension" .= toJSON (codingExtension c)
        , "system"    .= toJSON (codingSystem c)
        , "version"   .= toJSON (codingVersion c)
        , "code"      .= toJSON (codingCode c)
        , "display"   .= toJSON (codingDisplay c)
        , "userSelected" .= toJSON (codingUserSelected c)
        ]
instance FromJSON Coding where
    parseJSON = withObject "Coding" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        s <- o .:? "system"
        v <- o .:? "version"
        val <- o .:? "code"  
        dis <- o .:? "display"
        us  <- o .:? "userSelected"
        return $ Coding{codingAttribs=[]
                   , codingId=id
                   , codingExtension=ext
                   , codingSystem=s
                   , codingVersion=v
                   , codingCode=val
                   , codingDisplay=dis
                   , codingUserSelected=us}


data CodeableConcept
    = CodeableConcept {
         codeableConceptAttribs :: [AnyAttrib]
       , codeableConceptId :: Maybe Id
       , codeableConceptExtension :: [Extension]
       , codeableConceptCoding :: [Coding]
       , codeableConceptText :: Maybe Text}
    deriving (Eq, Show)

instance ToJSON CodeableConcept where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
          "id"        .= toJSON (codeableConceptId c)
        , "extension" .= toJSON (codeableConceptExtension c)
        , "coding" .= toJSON (codeableConceptCoding c)
        , "text"   .= toJSON (codeableConceptText c)
        ]
instance FromJSON CodeableConcept where
    parseJSON = withObject "CodeableConcept" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        c <- o .:? "coding" .!= []
        t <- o .:? "text"
        return $ CodeableConcept{
                  codeableConceptAttribs=[]
                , codeableConceptId=id
                , codeableConceptExtension=ext
                , codeableConceptCoding=c 
                , codeableConceptText=t}

data ConceptMapEquivalence
    = CmeDisjoint
    | CmeEqual
    | CmeEquivalent
    | CmeInexact
    | CmeNarrower
    | CmeRelatedTo
    | CmeSpecializes
    | CmeSubsumes
    | CmeUnmatched
    | CmeWider
    deriving (Eq, Show)


instance ToJSON ConceptMapEquivalence where
    toJSON (CmeDisjoint)  = String "disjoint"
    toJSON (CmeEqual)     = String "equal"
    toJSON (CmeEquivalent) = String "equivalent"
    toJSON (CmeInexact)   = String "inexact"
    toJSON (CmeNarrower)  = String "narrower"
    toJSON (CmeRelatedTo) = String "relatedto"
    toJSON (CmeSpecializes) = String "specializes"
    toJSON (CmeSubsumes)  = String "subsumes"
    toJSON (CmeUnmatched) = String "unmatched"
    toJSON (CmeWider)     = String "wider"
instance FromJSON ConceptMapEquivalence where
    parseJSON (String s) = case T.unpack s of
      "disjoint" -> return CmeDisjoint
      "equal" -> return CmeEqual
      "equivalent" -> return CmeEquivalent
      "inexact" -> return CmeInexact
      "narrower" -> return CmeNarrower
      "relatedto" -> return CmeRelatedTo
      "specializes" -> return CmeSpecializes
      "subsumes" -> return CmeSubsumes
      "unmatched" -> return CmeUnmatched
      "wider" -> return CmeWider


data ConstraintSeverity = CsError | CsWarning 
    deriving (Eq, Show)

instance ToJSON ConstraintSeverity where
    toJSON (CsError)   = String "error"
    toJSON (CsWarning) = String "warning"
instance FromJSON ConstraintSeverity where
    parseJSON (String s) = case T.unpack s of
      "error" -> return CsError
      "warning" -> return CsWarning

{-
  "telecom": [
    {
      "system": "phone",
      "value": "555-334-3277",
      "use": "home"
    }
  ]
-}
data ContactPointSystem
    = CpsEmail | CpsFax | CpsOther | CpsPager | CpsPhone | CpsSms | CpsUrl
    deriving (Eq, Show)

instance ToJSON ContactPointSystem where
    toJSON (CpsEmail) = String "email"
    toJSON (CpsFax) = String "fax"
    toJSON (CpsOther) = String "other"
    toJSON (CpsPager) = String "pager"
    toJSON (CpsPhone) = String "phone"
    toJSON (CpsSms) = String "sms"
    toJSON (CpsUrl) = String "url"
instance FromJSON ContactPointSystem where
    parseJSON (String s) = case T.unpack s of
      "email" -> return CpsEmail
      "fax" -> return CpsFax
      "other" -> return CpsOther
      "pager" -> return CpsPager
      "phone" -> return CpsPhone
      "sms" -> return CpsSms
      "url" -> return CpsUrl

data ContactPointUse
    = CpuHome | CpuMobile | CpuOld | CpuTemp | CpuWork
    deriving (Eq, Show)

instance ToJSON ContactPointUse where
    toJSON (CpuHome) = String "home"
    toJSON (CpuMobile) = String "mobile"
    toJSON (CpuOld) = String "old"
    toJSON (CpuTemp) = String "temp"
    toJSON (CpuWork) = String "work"
instance FromJSON ContactPointUse where
    parseJSON (String s) = case T.unpack s of
      "home" -> return CpuHome
      "mobile" -> return CpuMobile
      "old" -> return CpuOld
      "temp" -> return CpuTemp
      "work" -> return CpuWork

data ContactPoint
    = ContactPoint{
          contactPointAttribs :: [AnyAttrib]
        , contactPointId :: Maybe Id
        , contactPointExtension :: [Extension]
        , contactPointSystem :: Maybe ContactPointSystem
        , contactPointValue :: Maybe Text
        , contactPointUse :: Maybe ContactPointUse
        , contactPointRank :: Maybe PositiveInt
        , contactPointPeriod :: Maybe Period}
    deriving (Eq, Show)

instance ToJSON ContactPoint where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
          "id" .= toJSON (contactPointId c)
        , "extension" .= toJSON (contactPointExtension c)
        , "system" .= toJSON (contactPointSystem c)
        , "value"  .= toJSON (contactPointValue c)
        , "use"    .= toJSON (contactPointUse c)
        , "rank"   .= toJSON (contactPointRank c)
        , "period" .= toJSON (contactPointPeriod c)
        ]
instance FromJSON ContactPoint where
    parseJSON = withObject "ContactPoint" $ \o -> do
        id <- o .:? "id"
        ext <- o .:? "extension" .!= []
        s <- o .:? "system"
        v <- o .:? "value"
        u <- o .:? "use" 
        r <- o .:? "rank" 
        p <- o .:? "period"
        return $ ContactPoint{
                       contactPointAttribs=[]
                     , contactPointId=id
                     , contactPointExtension=ext
                     , contactPointSystem=s, contactPointValue=v
                     , contactPointUse=u, contactPointRank=r
                     , contactPointPeriod=p}


data Count = Count { 
          countAttribs :: [AnyAttrib]
        , countId :: Maybe Id
        , countExtension :: [Extension]
        , countValue :: Maybe Decimal
        , countComparator :: Maybe QuantityComparator
        , countUnit :: Maybe Ucum  
        , countSystem :: Maybe Uri
        , countCode :: Maybe Code}
    deriving (Eq, Show)

instance ToJSON Count where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
          "id"  .= toJSON (countId c)
        , "extension"  .= toJSON (countExtension c)
        , "value"  .= toJSON (countValue c)
        , "comparator"  .= toJSON (countComparator c)
        , "unit"  .= toJSON (countUnit c)
        , "system"  .= toJSON (countSystem c)
        , "code"  .= toJSON (countCode c)
        ]
instance FromJSON Count where
    parseJSON = withObject "Count" $ \o -> do
        i <- o .:? "id"
        ext<- o .:? "extension" .!= []
        v <- o .:? "value"
        c <- o .:? "comparator"
        u <- o .:? "unit"
        s <- o .:? "system"
        co <- o .:? "code"
        return $ Count{
                     countAttribs=[]
                   , countId=i
                   , countExtension=ext
                   , countValue=v
                   , countComparator=c
                   , countUnit=u
                   , countSystem=s
                   , countCode=co
                   }


data DiscriminatorType
    = DtExists | DtPattern | DtProfile | DtType | DtValue
    deriving (Eq, Show)

instance ToJSON DiscriminatorType where
    toJSON (DtExists)  = String "exists"
    toJSON (DtPattern) = String "pattern"
    toJSON (DtProfile) = String "profile"
    toJSON (DtType)    = String "type"
    toJSON (DtValue)   = String "value"
instance FromJSON DiscriminatorType where
    parseJSON (String s) = case T.unpack s of
      "exists"  -> return DtExists
      "pattern" -> return DtPattern
      "profile" -> return DtProfile
      "type"    -> return DtType
      "value"   -> return DtValue


data Distance = Distance { 
          distanceAttribs :: [AnyAttrib]
        , distanceId :: Maybe Id
        , distanceExtension :: [Extension]
        , distanceValue :: Maybe Decimal
        , distanceComparator :: Maybe QuantityComparator
        , distanceUnit :: Maybe Ucum  
        , distanceSystem :: Maybe Uri
        , distanceCode :: Maybe Code}
    deriving (Eq, Show)

instance ToJSON Distance where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
          "id"  .= toJSON (distanceId c)
        , "extension"  .= toJSON (distanceExtension c)
        , "value"  .= toJSON (distanceValue c)
        , "comparator"  .= toJSON (distanceComparator c)
        , "unit"  .= toJSON (distanceUnit c)
        , "system"  .= toJSON (distanceSystem c)
        , "code"  .= toJSON (distanceCode c)
        ]
instance FromJSON Distance where
    parseJSON = withObject "Distance" $ \o -> do
        i <- o .:? "id"
        ext<- o .:? "extension" .!= []
        v <- o .:? "value"
        c <- o .:? "comparator"
        u <- o .:? "unit"
        s <- o .:? "system"
        co <- o .:? "code"
        return $ Distance{
                     distanceAttribs=[]
                   , distanceId=i
                   , distanceExtension=ext
                   , distanceValue=v
                   , distanceComparator=c
                   , distanceUnit=u
                   , distanceSystem=s
                   , distanceCode=co
                   }


data DocumentReferenceStatus
    = DrsCurrent | DrsEnteredInError | DrsSuperseeded
    deriving (Eq, Show)

instance ToJSON DocumentReferenceStatus where
    toJSON (DrsCurrent)        = String "current"
    toJSON (DrsEnteredInError) = String "entered-in-error"
    toJSON (DrsSuperseeded)    = String "superseeded"
instance FromJSON DocumentReferenceStatus where
    parseJSON (String s) = case T.unpack s of
      "current" -> return DrsCurrent
      "entered-in-error" -> return DrsEnteredInError
      "superseeded" -> return DrsSuperseeded


data Duration = Duration { 
          durationAttribs :: [AnyAttrib]
        , durationId :: Maybe Id
        , durationExtension :: [Extension]
        , durationValue :: Maybe Decimal
        , durationComparator :: Maybe QuantityComparator
        , durationUnit :: Maybe Ucum  
        , durationSystem :: Maybe Uri
        , durationCode :: Maybe Code}
    deriving (Eq, Show)

instance ToJSON Duration where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
          "id"  .= toJSON (durationId c)
        , "extension"  .= toJSON (durationExtension c)
        , "value"  .= toJSON (durationValue c)
        , "comparator"  .= toJSON (durationComparator c)
        , "unit"  .= toJSON (durationUnit c)
        , "system"  .= toJSON (durationSystem c)
        , "code"  .= toJSON (durationCode c)
        ]
instance FromJSON Duration where
    parseJSON = withObject "Duration" $ \o -> do
        i <- o .:? "id"
        ext<- o .:? "extension" .!= []
        v <- o .:? "value"
        c <- o .:? "comparator"
        u <- o .:? "unit"
        s <- o .:? "system"
        co <- o .:? "code"
        return $ Duration{
                     durationAttribs=[]
                   , durationId=i
                   , durationExtension=ext
                   , durationValue=v
                   , durationComparator=c
                   , durationUnit=u
                   , durationSystem=s
                   , durationCode=co
                   }


data EventTiming
    = EtAC
    | EtACD
    | EtACM
    | EtACV
    | EtAFT
    | EtAFTEarly
    | EtAFTLate
    | EtC
    | EtCD
    | EtCM
    | EtCV
    | EtEVE
    | EtEVEEarly
    | EtEVELate
    | EtHS
    | EtMORN
    | EtMORNEarly
    | EtMORNLate
    | EtNIGHT
    | EtNOON
    | EtPC
    | EtPCD
    | EtPCM
    | EtPCV
    | EtPHS
    | EtWAKE
    deriving (Eq, Show)

instance ToJSON EventTiming where
    toJSON (EtAC)       = String "AC"
    toJSON (EtACD)      = String "ACD"
    toJSON (EtACM)      = String "ACM"
    toJSON (EtACV)      = String "ACV"
    toJSON (EtAFT)      = String "AFT"
    toJSON (EtAFTEarly) = String "AFTEarly"
    toJSON (EtAFTLate)  = String "AFTLate"
    toJSON (EtC)       = String "C"
    toJSON (EtCD)      = String "CD"
    toJSON (EtCM)      = String "CM"
    toJSON (EtCV)      = String "CV"
    toJSON (EtEVE)      = String "EVE"
    toJSON (EtEVEEarly) = String "EVEEarly"
    toJSON (EtEVELate)  = String "EVELate"
    toJSON (EtHS)      = String "HS"
    toJSON (EtMORN)    = String "MORN"
    toJSON (EtMORNEarly) = String "MORNEarly"
    toJSON (EtMORNLate)  = String "MORNLate"
    toJSON (EtNIGHT)     = String "NIGHT"
    toJSON (EtNOON)     = String "NOON"
    toJSON (EtPC)       = String "PC"
    toJSON (EtPCD)      = String "PCD"
    toJSON (EtPCM)      = String "PCM"
    toJSON (EtPCV)      = String "PCV"
    toJSON (EtPHS)      = String "PHS"
    toJSON (EtWAKE)     = String "WAKE"
instance FromJSON EventTiming where
    parseJSON (String s) = case T.unpack s of
      "AC"    -> return EtAC
      "ACD"    -> return EtACD
      "ACM"    -> return EtACM
      "ACV"    -> return EtACV
      "AFT"    -> return EtAFT
      "AFTEarly"    -> return EtAFTEarly
      "AFTLate"    -> return EtAFTLate
      "C"    -> return EtC
      "CD"    -> return EtCD
      "CM"    -> return EtCM
      "CV"    -> return EtCV
      "EVE"    -> return EtEVE
      "EVEEarly"    -> return EtEVEEarly
      "EVELate"    -> return EtEVELate
      "HS"    -> return EtHS
      "MORN"    -> return EtMORN
      "MORNEarly"    -> return EtMORNEarly
      "MORNLate"    -> return EtMORNLate
      "NIGHT"    -> return EtNIGHT
      "NOON"    -> return EtNOON
      "PC"    -> return EtPC
      "PCD"    -> return EtPCD
      "PCM"    -> return EtPCM
      "PCV"    -> return EtPCV
      "PHS"    -> return EtPHS
      "WAKE"    -> return EtWAKE



data FHIRVersion
    = Fv_0080
    | Fv_0081
    | Fv_0082
    | Fv_001
    | Fv_005
    | Fv_006
    | Fv_011
    | Fv_040
    | Fv_050
    | Fv_100
    | Fv_101
    | Fv_102
    | Fv_110
    | Fv_140
    | Fv_160
    | Fv_180
    | Fv_300
    | Fv_301
    | Fv_330
    | Fv_350
    | Fv_400
    | Fv_401
    deriving (Eq, Show)

instance ToJSON FHIRVersion where
  toJSON Fv_001 = String "0.01"
  toJSON Fv_005 = String "0.05"
  toJSON Fv_006 = String "0.06"
  toJSON Fv_011 = String "0.11"
  toJSON Fv_0080 = String "0.0.80"
  toJSON Fv_0081 = String "0.0.81"
  toJSON Fv_0082 = String "0.0.82"
  toJSON Fv_040 = String "0.4.0"
  toJSON Fv_050 = String "0.5.0"
  toJSON Fv_100 = String "1.0.0"
  toJSON Fv_101 = String "1.0.1"
  toJSON Fv_102 = String "1.0.2"
  toJSON Fv_110 = String "1.1.0"
  toJSON Fv_140 = String "1.4.0"
  toJSON Fv_160 = String "1.6.0"
  toJSON Fv_180 = String "1.8.0"
  toJSON Fv_300 = String "3.0.0"
  toJSON Fv_301 = String "3.0.1"
  toJSON Fv_330 = String "3.3.0"
  toJSON Fv_350 = String "3.5.0"
  toJSON Fv_400 = String "4.0.0"
  toJSON Fv_401 = String "4.0.1"
instance FromJSON FHIRVersion where
    parseJSON (String s) = case T.unpack s of
      "0.01" -> return Fv_001
      "0.05" -> return Fv_005
      "0.06" -> return Fv_006
      "0.11" -> return Fv_011
      "0.0.80" -> return Fv_0080
      "0.0.81" -> return Fv_0081
      "0.0.82" -> return Fv_0082
      "0.4.0" -> return Fv_040
      "0.5.0" -> return Fv_050
      "1.0.0" -> return Fv_100
      "1.0.1" -> return Fv_101
      "1.0.2" -> return Fv_102
      "1.1.0" -> return Fv_110
      "1.4.0" -> return Fv_140
      "1.6.0" -> return Fv_160
      "1.8.0" -> return Fv_180
      "3.0.0" -> return Fv_300
      "3.0.1" -> return Fv_301
      "3.3.0" -> return Fv_330
      "3.5.0" -> return Fv_350
      "4.0.0" -> return Fv_400
      "4.0.1" -> return Fv_401


data NameUse
    = NuAnonymous | NuMaiden | NuNickname | NuOfficial | NuOld | NuTemp | NuUsual
    deriving (Eq, Show)

instance ToJSON NameUse where
    toJSON (NuAnonymous) = String "anonymous"
    toJSON (NuMaiden)    = String "maiden"
    toJSON (NuNickname)  = String "nickname"
    toJSON (NuOfficial)  = String "official"
    toJSON (NuOld)       = String "old"
    toJSON (NuTemp)      = String "temp"
    toJSON (NuUsual)     = String "usual"
instance FromJSON NameUse where
    parseJSON (String s) = case T.unpack s of
      "anonymous" -> return NuAnonymous
      "maiden"    -> return NuMaiden
      "nickname"  -> return NuNickname
      "official"  -> return NuOfficial
      "old"       -> return NuOld
      "temp"      -> return NuTemp
      "usual"     -> return NuUsual


data HumanName
    = HumanName {
          humanNameAttribs :: [AnyAttrib]
        , humanNameId :: Maybe Id
        , humanNameExtension :: [Extension]
        , humanNameUse :: Maybe NameUse
        , humanNameText :: Maybe Text
        , humanNameFamily :: Maybe Text
        , humanNameGiven :: [Text]
        , humanNamePrefix :: [Text]
        , humanNameSuffix :: [Text]
        , humanNamePeriod :: Maybe Period}
    deriving (Eq, Show)

instance ToJSON HumanName where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
          "id"     .= toJSON (humanNameId c)
        , "extension" .= toJSON (humanNameExtension c)
        , "use"    .= toJSON (humanNameUse c)
        , "text"   .= toJSON (humanNameText c)
        , "family" .= toJSON (humanNameFamily c)
        , "given"  .= toJSON (humanNameGiven c)
        , "prefix" .= toJSON (humanNamePrefix c)
        , "suffix" .= toJSON (humanNameSuffix c)
        , "period" .= toJSON (humanNamePeriod c)
        ]
instance FromJSON HumanName where
    parseJSON = withObject "HumanName" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        u <- o .:? "use"
        t <- o .:? "text"
        f <- o .:? "family"
        g <- o .:? "given" .!= []
        p <- o .:? "prefix" .!= []
        s <- o .:? "suffix" .!= []
        pd <- o .:? "period"
        return $ HumanName{
             humanNameAttribs=[]
           , humanNameId=id
           , humanNameExtension=ext
           , humanNameUse=u, humanNameText=t
           , humanNameFamily=f, humanNameGiven=g
           , humanNamePrefix=p, humanNameSuffix=s
           , humanNamePeriod=pd}
{-
  "name": [
    {
      "use": "official",
      "family": "Pouros728",
      "given": [
        "Freddie621"
      ],
      "prefix": [
        "Ms."
      ]
    }
  ],
-}

data IdentifierUse
    = IuOfficial | IuOld | IuSecondary | IuTemp | IuUsual
    deriving (Eq, Show)

instance ToJSON IdentifierUse where
    toJSON (IuOfficial) = String "official"
    toJSON (IuOld) = String "iold"
    toJSON (IuSecondary) = String "secondary"
    toJSON (IuTemp) = String "itemp"
    toJSON (IuUsual) = String "usual"
instance FromJSON IdentifierUse where
    parseJSON (String s) = case T.unpack s of
      "official" -> return IuOfficial
      "iold" -> return IuOld
      "secondary" -> return IuSecondary
      "itemp" -> return IuTemp
      "usual" -> return IuUsual

data Identifier
    = Identifier {
              identifierAttribs :: [AnyAttrib]
            , identifierId :: Maybe Id
            , identifierExtension :: [Extension]
            , identifierUse :: Maybe IdentifierUse
            , identifierType :: Maybe CodeableConcept
            , identifierSystem :: Maybe Uri
            , identifierValue :: Maybe Text
            , identifierPeriod :: Maybe Period
            , identifierAssigner :: Maybe Reference}
    deriving (Eq, Show)

instance ToJSON Identifier where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (identifierId c)
        ,  "extension"   .= toJSON (identifierExtension c)
        ,  "use" .= toJSON (identifierUse c)
        ,  "type"   .= toJSON (identifierType c)
        ,  "system"   .= toJSON (identifierSystem c)
        ,  "value"   .= toJSON (identifierValue c)
        ,  "period"   .= toJSON (identifierPeriod c) 
        ,  "assigner"   .= toJSON (identifierAssigner c)
        ]
instance FromJSON Identifier where
    parseJSON = withObject "Identifier" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        u <- o .:? "use"
        t <- o .:? "type"
        s <- o .:? "system"
        v <- o .:? "value"
        p <- o .:? "period"
        a <- o .:? "assigner"
        return $ Identifier{
                    identifierAttribs=[]
                  , identifierId=id
                  , identifierExtension=ext
                  , identifierUse=u
                  , identifierType=t
                  , identifierSystem=s
                  , identifierValue=v
                  , identifierPeriod=p
                  , identifierAssigner=a}

data LinkType
    = LtRefer | LtReplacedBy | LtReplaces | LtSeeAlso
    deriving (Eq, Show)

instance ToJSON LinkType where 
    toJSON (LtRefer) = String "refer"
    toJSON (LtReplacedBy) = String "replaced-by"
    toJSON (LtReplaces) = String "replaces"
    toJSON (LtSeeAlso) = String "seealso"
instance FromJSON LinkType where
    parseJSON (String s) = case T.unpack s of
      "refer" -> return LtRefer
      "replaced-by" -> return LtReplacedBy
      "replaces" -> return LtReplaces
      "seealso" -> return LtSeeAlso


data MarketingStatus
    = MarketingStatus {
          marketingStatusAttribs :: [AnyAttrib]
        , marketingStatusId :: Maybe Id
        , marketingStatusExtension :: [Extension]
        , marketingStatusCountry :: CodeableConcept
        , marketingStatusJurisdiction :: Maybe CodeableConcept
        , marketingStatusStatus :: CodeableConcept
        , marketingStatusDateRange :: Period
        , marketingStatusRestoreDate :: Maybe DateTime}
    deriving (Eq, Show)

instance ToJSON MarketingStatus where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (marketingStatusId c)
        ,  "extension"   .= toJSON (marketingStatusExtension c)
        ,  "country" .= toJSON (marketingStatusCountry c)
        ,  "jurisdiction"   .= toJSON (marketingStatusJurisdiction c)
        ,  "status"   .= toJSON (marketingStatusStatus c)
        ,  "dateRange"   .= toJSON (marketingStatusDateRange c)
        ,  "restoreDate"   .= toJSON (marketingStatusRestoreDate c) 
        ]
instance FromJSON MarketingStatus where
    parseJSON = withObject "MarketingStatus" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        c <- o .:  "country"
        j <- o .:? "jurisdiction"
        s <- o .:  "status"
        d <- o .:  "dateRange"
        r <- o .:? "restoreDate"
        return $ MarketingStatus{
                    marketingStatusAttribs=[]
                  , marketingStatusId=id
                  , marketingStatusExtension=ext
                  , marketingStatusCountry=c
                  , marketingStatusJurisdiction=j
                  , marketingStatusStatus=s
                  , marketingStatusDateRange=d
                  , marketingStatusRestoreDate=r}

data Money
    = Money { 
          moneyAttribs :: [AnyAttrib]
        , moneyId :: Maybe Id
        , moneyExtension :: [Extension]
        , moneyValue :: Maybe Decimal
        , moneyCurrency :: Maybe Code}
    deriving (Eq, Show)

instance ToJSON Money where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (moneyId c)
        ,  "extension"   .= toJSON (moneyExtension c)
        ,  "value" .= toJSON (moneyValue c)
        ,  "currency"   .= toJSON (moneyCurrency c)
        ]
instance FromJSON Money where
    parseJSON = withObject "Money" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        v <- o .:? "value"
        c <- o .:? "currency"
        return $ Money{
                    moneyAttribs=[]
                  , moneyId=id
                  , moneyExtension=ext
                  , moneyValue=v
                  , moneyCurrency=c}

data NoteType = NtDisplay | NtPrint | NtPrintoper deriving (Eq, Show)

instance ToJSON NoteType where 
    toJSON (NtDisplay) = String "display"
    toJSON (NtPrint) = String "print"
    toJSON (NtPrintoper) = String "printoper"
instance FromJSON NoteType where
    parseJSON (String s) = case T.unpack s of
      "display" -> return NtDisplay
      "print" -> return NtPrint
      "printoper" -> return NtPrintoper


data Range
    = Range { 
          rangeAttribs :: [AnyAttrib]
        , rangeId :: Maybe Id
        , rangeExtension :: [Extension]
        , rangeLow :: Maybe Quantity
        , rangeHigh :: Maybe Quantity}
    deriving (Eq, Show)

instance ToJSON Range where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (rangeId c)
        ,  "extension"   .= toJSON (rangeExtension c)
        ,  "low"  .= toJSON (rangeLow c)
        ,  "high" .= toJSON (rangeHigh c)
        ]
instance FromJSON Range where
    parseJSON = withObject "Range" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        l <- o .:? "low"
        h <- o .:? "high"
        return $ Range{
                    rangeAttribs=[]
                  , rangeId=id
                  , rangeExtension=ext
                  , rangeLow=l
                  , rangeHigh=h}

data Ratio
    = Ratio {
          ratioAttribs :: [AnyAttrib]
        , ratioId :: Maybe Id
        , ratioExtension :: [Extension]
        , ratioNumerator :: Maybe Quantity
        , ratioDenominator :: Maybe Quantity}
    deriving (Eq, Show)

instance ToJSON Ratio where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (ratioId c)
        ,  "extension"   .= toJSON (ratioExtension c)
        ,  "numerator"  .= toJSON (ratioNumerator c)
        ,  "denominator" .= toJSON (ratioDenominator c)
        ]
instance FromJSON Ratio where
    parseJSON = withObject "Ratio" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        l <- o .:? "numerator"
        h <- o .:? "denominator"
        return $ Ratio{
                    ratioAttribs=[]
                  , ratioId=id
                  , ratioExtension=ext
                  , ratioNumerator=l
                  , ratioDenominator=h}

data PopulationAge
    = PopulationAgeRange Range
    | PopulationAgeCodeableConcept CodeableConcept
    deriving (Eq, Show)

toPopulationAgeJSON (PopulationAgeRange           r) = ("ageRange", toJSON r)
toPopulationAgeJSON (PopulationAgeCodeableConcept r) = ("ageCodeableConcept", toJSON r)

data Population
    = Population {
          populationAttribs :: [AnyAttrib]
        , populationId :: Maybe Id
        , populationExtension :: [Extension]
        , populationAge :: PopulationAge
        , populationGender :: Maybe CodeableConcept
        , populationRace :: Maybe CodeableConcept
        , populationPhysiologicalCondition :: Maybe CodeableConcept}
    deriving (Eq, Show)

instance ToJSON Population where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (populationId c)
        ,  "extension"   .= toJSON (populationExtension c)
        , toPopulationAgeJSON (populationAge c)
        , "gender"   .= toJSON (populationGender c)
        , "race" .= toJSON (populationRace c)
        , "physiologicalCondition"   .= toJSON (populationPhysiologicalCondition c)
        ]
instance FromJSON Population where
    parseJSON = withObject "Population" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        a <- parseAgeR o <|> parseAgeCC o 
        g <- o .:? "gender"
        r <- o .:? "race"
        p <- o .:? "physiologicalCondition"
        return $ Population{
                    populationAttribs=[]
                  , populationId=id
                  , populationExtension=ext
                  , populationAge=a
                  , populationGender=g
                  , populationRace=r
                  , populationPhysiologicalCondition=p}
        where parseAgeR o = do
                hasR <- o .: "ageRange" 
                return $ (PopulationAgeRange hasR)
              parseAgeCC o = do
                hasCC <- o .: "ageCodeableConcept"
                return $ (PopulationAgeCodeableConcept hasCC)

data ProductShelfLife
    = ProductShelfLife { 
          productShelfLifeAttribs :: [AnyAttrib]
        , productShelfLifeId :: Maybe Id
        , productShelfLifeExtension :: [Extension]
        , productShelfLifeIdentifier :: Maybe Identifier
        , productShelfLifeType :: CodeableConcept
        , productShelfLifePeriod :: Quantity
        , productShelfLifeSpecialPrecautionsForStorage :: [CodeableConcept]}
    deriving (Eq, Show)

instance ToJSON ProductShelfLife where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (productShelfLifeId c)
        ,  "extension"  .= toJSON (productShelfLifeExtension c)
        ,  "identifier" .= toJSON (productShelfLifeIdentifier c)
        ,  "type"       .= toJSON (productShelfLifeType c)
        ,  "period"     .= toJSON (productShelfLifePeriod c)
        ,  "specialPrecautionsForStorage"   .= toJSON (productShelfLifeSpecialPrecautionsForStorage c)
        ]
instance FromJSON ProductShelfLife where
    parseJSON = withObject "ProductShelfLife" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        i <- o .:? "identifier"
        t <- o .:  "type"
        p <- o .:  "period"
        s <- o .:  "specialPrecautionsForStorage" .!= []
        return $ ProductShelfLife{
                    productShelfLifeAttribs=[]
                  , productShelfLifeId=id
                  , productShelfLifeExtension=ext
                  , productShelfLifeIdentifier=i
                  , productShelfLifeType=t
                  , productShelfLifePeriod=p
                  , productShelfLifeSpecialPrecautionsForStorage=s}

data ProdCharacteristic
    = ProdCharacteristic {
          prodCharacteristicAttribs :: [AnyAttrib]
        , prodCharacteristicId :: Maybe Id
        , prodCharacteristicExtension :: [Extension]
        , prodCharacteristicHeight :: Maybe Quantity
        , prodCharacteristicWidth :: Maybe Quantity
        , prodCharacteristicDepth :: Maybe Quantity
        , prodCharacteristicWeight :: Maybe Quantity
        , prodCharacteristicNominalVolume :: Maybe Quantity
        , prodCharacteristicExternalDiameter :: Maybe Quantity
        , prodCharacteristicShape :: Maybe Text
        , prodCharacteristicColor :: [Text]
        , prodCharacteristicImprint :: [Text]
        , prodCharacteristicImage :: [Attachment]
        , prodCharacteristicScoring :: Maybe CodeableConcept}
    deriving (Eq, Show)

instance ToJSON ProdCharacteristic where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (prodCharacteristicId c)
        ,  "extension"  .= toJSON (prodCharacteristicExtension c)
        ,  "height" .= toJSON (prodCharacteristicHeight c)
        ,  "width"  .= toJSON (prodCharacteristicWidth c)
        ,  "depth"  .= toJSON (prodCharacteristicDepth c)
        ,  "weight" .= toJSON (prodCharacteristicWeight c)
        ,  "nominalVolume"    .= toJSON (prodCharacteristicNominalVolume c)
        ,  "externalDiameter" .= toJSON (prodCharacteristicExternalDiameter c)
        ,  "shape"   .= toJSON (prodCharacteristicShape c)
        ,  "color"   .= toJSON (prodCharacteristicColor c)
        ,  "imprint" .= toJSON (prodCharacteristicImprint c)
        ,  "image"   .= toJSON (prodCharacteristicImage c)
        ,  "scoring" .= toJSON (prodCharacteristicScoring c)
        ]
instance FromJSON ProdCharacteristic where
    parseJSON = withObject "ProdCharacteristic" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        h  <- o .:? "height"
        w  <- o .:? "width"
        d  <- o .:? "depth"
        we <- o .:? "weight"
        n  <- o .:? "nominalVolume"
        e  <- o .:? "externalDiameter"
        sh <- o .:? "shape"
        co <- o .:  "color"   .!= []
        ip <- o .:  "imprint" .!= []
        im <- o .:  "image"   .!= []
        sc <- o .:? "scoring"
        return $ ProdCharacteristic{
                    prodCharacteristicAttribs=[]
                  , prodCharacteristicId=id
                  , prodCharacteristicExtension=ext
                  , prodCharacteristicHeight=h
                  , prodCharacteristicWidth=w
                  , prodCharacteristicDepth=d
                  , prodCharacteristicWeight=we
                  , prodCharacteristicNominalVolume=n
                  , prodCharacteristicExternalDiameter=e
                  , prodCharacteristicShape=sh
                  , prodCharacteristicColor=co
                  , prodCharacteristicImprint=ip
                  , prodCharacteristicImage=im
                  , prodCharacteristicScoring=sc}

data PropertyRepresentation 
    = PrcdaText | PrtypeAttr | Prxhtml | PrxmlAttr | PrxmlText
    deriving (Eq, Show)

instance ToJSON PropertyRepresentation where
    toJSON (PrcdaText)  = String "cdaText"
    toJSON (PrtypeAttr) = String "typeAttr"
    toJSON (Prxhtml)    = String "xhtml"
    toJSON (PrxmlAttr)  = String "xmlAttr"
    toJSON (xmlText)    = String "xmlText"
instance FromJSON PropertyRepresentation where
    parseJSON (String s) = case T.unpack s of
      "cdaText"  -> return PrcdaText
      "typeAttr" -> return PrtypeAttr
      "xhtml"    -> return Prxhtml
      "xmlAttr"  -> return PrxmlAttr
      "xmlText"  -> return PrxmlText

data PublicationStatus
    = PsActive | PsDraft | PsRetired | PsUnknown
    deriving (Eq, Show)

instance ToJSON PublicationStatus where
    toJSON (PsActive)  = String "active"
    toJSON (PsDraft)   = String "draft"
    toJSON (PsRetired) = String "retired"
    toJSON (PsUnknown) = String "unknown"
instance FromJSON PublicationStatus where
    parseJSON (String s) = case T.unpack s of
      "active"  -> return PsActive
      "draft"   -> return PsDraft
      "retired" -> return PsRetired
      "unknown" -> return PsUnknown


data RemittanceOutcome
    = RoComplete | RoError | RoPartial | RoQueued
    deriving (Eq, Show)

instance ToJSON RemittanceOutcome where
    toJSON (RoComplete) = String "complete"
    toJSON (RoError)    = String "error"
    toJSON (RoPartial)  = String "partial"
    toJSON (RoQueued)   = String "queued"
instance FromJSON RemittanceOutcome where
    parseJSON (String s) = case T.unpack s of
      "complete"-> return RoComplete
      "error"   -> return RoError
      "partial" -> return RoPartial
      "queued"  -> return RoQueued


data SampledData
    = SampledData {
          sampledDataAttribs :: [AnyAttrib]
        , sampledDataId :: Maybe Id
        , sampledDataExtension :: [Extension]
        , sampledDataOrigin :: Quantity
        , sampledDataPeriod :: Decimal
        , sampledDataFactor :: Maybe Decimal
        , sampledDataLowerLimit :: Maybe Decimal
        , sampledDataUpperLimit :: Maybe Decimal
        , sampledDataDimensions :: PositiveInt
        , sampledDataData :: Maybe SampledDataDataType}
    deriving (Eq, Show)

instance ToJSON SampledData where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (sampledDataId c)
        ,  "extension"  .= toJSON (sampledDataExtension c)
        ,  "origin" .= toJSON (sampledDataOrigin c)
        ,  "period" .= toJSON (sampledDataPeriod c)
        ,  "factor" .= toJSON (sampledDataFactor c)
        ,  "lowerLimit" .= toJSON (sampledDataLowerLimit c)
        ,  "upperLimit" .= toJSON (sampledDataUpperLimit c)
        ,  "dimensions" .= toJSON (sampledDataDimensions c)
        ,  "data"   .= toJSON (sampledDataData c)
        ]
instance FromJSON SampledData where
    parseJSON = withObject "SampledData" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        or <- o .:  "origin"
        p  <- o .:  "period"
        f  <- o .:? "factor"
        l  <- o .:? "lowerLimit"
        u  <- o .:? "upperLimit"
        di <- o .:  "dimensions"
        da <- o .:? "data"
        return $ SampledData{
                    sampledDataAttribs=[]
                  , sampledDataId=id
                  , sampledDataExtension=ext
                  , sampledDataOrigin=or
                  , sampledDataPeriod=p
                  , sampledDataFactor=f
                  , sampledDataLowerLimit=l
                  , sampledDataUpperLimit=u
                  , sampledDataDimensions=di
                  , sampledDataData=da}


data SearchParamType
    = SptComposite
    | SptDate
    | SptNumber
    | SptQuantity
    | SptReference
    | SptSpecial
    | SptString
    | SptToken
    | SptUri
    deriving (Eq, Show)

instance ToJSON SearchParamType where
    toJSON (SptComposite)= String "composite"
    toJSON (SptDate)     = String "date"
    toJSON (SptNumber)   = String "number"
    toJSON (SptQuantity) = String "quantity"
    toJSON (SptReference)= String "reference"
    toJSON (SptSpecial)  = String "special"
    toJSON (SptString)   = String "string"
    toJSON (SptToken)    = String "token"
    toJSON (SptUri)      = String "uri"
instance FromJSON SearchParamType where
    parseJSON (String s) = case T.unpack s of
      "composite" -> return SptComposite
      "date"      -> return SptDate
      "number"    -> return SptNumber
      "quantity"  -> return SptQuantity
      "reference" -> return SptReference
      "special"   -> return SptSpecial
      "string"    -> return SptString
      "token"     -> return SptToken
      "uri"       -> return SptUri


data Signature = Signature {
          signatureAttribs :: [AnyAttrib]
        , signatureId :: Maybe Id
        , signatureExtension :: [Extension]
        , signatureType :: [Coding]
        , signatureWhen :: Instant
        , signatureWho :: Reference
        , signatureOnBehalfOf :: Maybe Reference
        , signatureTargetFormat :: Maybe Code
        , signatureSigFormat :: Maybe Code
        , signatureData :: Maybe Base64Binary}
    deriving (Eq, Show)

instance ToJSON Signature where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (signatureId c)
        ,  "extension"  .= toJSON (signatureExtension c)
        ,  "type" .= toJSON (signatureType c)
        ,  "when" .= toJSON (signatureWhen c)
        ,  "who"  .= toJSON (signatureWho c)
        ,  "onBehalfOf"   .= toJSON (signatureOnBehalfOf c)
        ,  "targetFormat" .= toJSON (signatureTargetFormat c)
        ,  "sigFormat"    .= toJSON (signatureSigFormat c)
        ,  "data" .= toJSON (signatureData c)
        ]
instance FromJSON Signature where
    parseJSON = withObject "Signature" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        t  <- o .:  "type" .!= []
        we <- o .:  "when"
        wo <- o .:  "who"
        ob <- o .:? "onBehalfOf"
        tf <- o .:? "targetFormat"
        s  <- o .:? "sigFormat"
        d  <- o .:? "data"
        return $ Signature{
                    signatureAttribs=[]
                  , signatureId=id
                  , signatureExtension=ext
                  , signatureType=t
                  , signatureWhen=we
                  , signatureWho=wo
                  , signatureOnBehalfOf=ob
                  , signatureTargetFormat=tf
                  , signatureSigFormat=s
                  , signatureData=d
                  }  

data SlicingRules
    = SrClosed
    | SrOpen
    | SrOpenAtEnd
    deriving (Eq, Show)

instance ToJSON SlicingRules where
    toJSON (SrClosed)    = String "closed"
    toJSON (SrOpen)      = String "open"
    toJSON (SrOpenAtEnd) = String "open-at-end"
instance FromJSON SlicingRules where
    parseJSON (String s) = case T.unpack s of
      "closed"      -> return SrClosed
      "open"        -> return SrOpen
      "open-at-end" -> return SrOpenAtEnd


data SortDirection
    = SdAscending
    | SdDescending
    deriving (Eq, Show)

instance ToJSON SortDirection where
    toJSON (SdAscending)  = String "ascending"
    toJSON (SdDescending) = String "descending"
instance FromJSON SortDirection where
    parseJSON (String s) = case T.unpack s of
      "ascending"  -> return SdAscending
      "descending" -> return SdDescending


data SubstanceAmountReferenceRange
    = SubstanceAmountReferenceRange {
          substanceAmountReferenceRangeAttribs :: [AnyAttrib]
        , substanceAmountReferenceRangeId :: Maybe Id
        , substanceAmountReferenceRangeExtension :: [Extension]
        , substanceAmountReferenceRangeLowLimit :: Maybe Quantity
        , substanceAmountReferenceRangeHighLimit :: Maybe Quantity}
    deriving (Eq, Show)

instance ToJSON SubstanceAmountReferenceRange where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id"        .= toJSON (substanceAmountReferenceRangeId c)
        ,  "extension" .= toJSON (substanceAmountReferenceRangeExtension c)
        ,  "lowLimit"  .= toJSON (substanceAmountReferenceRangeLowLimit c)
        ,  "highLimit" .= toJSON (substanceAmountReferenceRangeHighLimit c)
        ]
instance FromJSON SubstanceAmountReferenceRange where
    parseJSON = withObject "SubstanceAmountReferenceRange" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        l <- o .:? "lowLimit"
        h <- o .:? "highLimit"
        return $ SubstanceAmountReferenceRange{
                    substanceAmountReferenceRangeAttribs=[]
                  , substanceAmountReferenceRangeId=id
                  , substanceAmountReferenceRangeExtension=ext
                  , substanceAmountReferenceRangeLowLimit=l
                  , substanceAmountReferenceRangeHighLimit=h}

data SubstanceAmountAmount
    = SubstanceAmountAmountQuantity Quantity
    | SubstanceAmountAmountRange Range
    | SubstanceAmountAmountString Text
    deriving (Eq, Show)

toSubstanceAmountAmountJSON (SubstanceAmountAmountQuantity q) = ("amountQuantity", toJSON q)
toSubstanceAmountAmountJSON (SubstanceAmountAmountRange q)    = ("amountRange", toJSON q)
toSubstanceAmountAmountJSON (SubstanceAmountAmountString q)   = ("amountString", toJSON q)

data SubstanceAmount
    = SubstanceAmount {
          substanceAmountSuper :: BackboneElement
        , substanceAmountAmount :: SubstanceAmountAmount
        , substanceAmountAmountType :: Maybe CodeableConcept
        , substanceAmountAmountText :: Maybe Text
        , substanceAmountReferenceRange :: Maybe SubstanceAmountReferenceRange}
    deriving (Eq, Show)

instance ToJSON SubstanceAmount where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        toBackboneElementJSON (substanceAmountSuper c)
        ++
        [
           toSubstanceAmountAmountJSON (substanceAmountAmount c)
        ,  "amountType"     .= toJSON (substanceAmountAmountType c)
        ,  "amountText"     .= toJSON (substanceAmountAmountText c)
        ,  "referenceRange" .= toJSON (substanceAmountReferenceRange c)
        ]
instance FromJSON SubstanceAmount where
    parseJSON = withObject "SubstanceAmount" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        mxt <- o .:? "modifierExtension" .!= []
        a  <- parseAmountQ o <|> parseAmountR o <|> parseAmountS o
        ty <- o .:? "amountType"
        te <- o .:? "amountText"
        r  <- o .:? "referenceRange"
        return $ SubstanceAmount{
                    substanceAmountSuper = mkBackboneElement id ext mxt
                  , substanceAmountAmount=a
                  , substanceAmountAmountType=ty
                  , substanceAmountAmountText=te
                  , substanceAmountReferenceRange=r}
        where parseAmountQ o = do
                hasQ <- o .: "amountQuantity" 
                return $ (SubstanceAmountAmountQuantity hasQ)
              parseAmountR o = do
                hasR <- o .: "amountRange"
                return $ (SubstanceAmountAmountRange hasR)
              parseAmountS o = do
                hasS <- o .: "amountString"
                return $ (SubstanceAmountAmountString hasS)


data UnitsOfTime
    = UotA 
    | UotD 
    | UotH
    | UotMin
    | UotMo
    | UotS
    | UotWk
    deriving (Eq, Show)

instance ToJSON UnitsOfTime where
    toJSON (UotA) = String "a"
    toJSON (UotD) = String "d"
    toJSON (UotH) = String "h"
    toJSON (UotMin) = String "min"
    toJSON (UotMo)  = String "mo"
    toJSON (UotS)   = String "s"
    toJSON (UotWk)  = String "wk"
instance FromJSON UnitsOfTime where
    parseJSON (String s) = case T.unpack s of
      "a" -> return UotA
      "d" -> return UotD
      "h" -> return UotH
      "min" -> return UotMin
      "mo"  -> return UotMo
      "s"   -> return UotS
      "wk"  -> return UotWk


data TimingRepeatBounds
    = TimingRepeatBoundsDuration Duration
    | TimingRepeatBoundsRange Range
    | TimingRepeatBoundsPeriod Period
    deriving (Eq, Show)

toTimingRepeatBoundsJSON (TimingRepeatBoundsDuration b) = ("boundDuration", toJSON b)
toTimingRepeatBoundsJSON (TimingRepeatBoundsRange    b) = ("boundRange", toJSON b)
toTimingRepeatBoundsJSON (TimingRepeatBoundsPeriod   b) = ("boundsPeriod", toJSON b)

data TimingRepeat
    = TimingRepeat {
          timingRepeatAttribs :: [AnyAttrib]
        , timingRepeatId :: Maybe Id
        , timingRepeatExtension :: [Extension]
        , timingRepeatBounds :: TimingRepeatBounds
        , timingRepeatCount :: Maybe PositiveInt
        , timingRepeatCountMax :: Maybe PositiveInt
        , timingRepeatDuration :: Maybe Decimal
        , timingRepeatDurationMax :: Maybe Decimal
        , timingRepeatDurationUnit :: Maybe UnitsOfTime
        , timingRepeatFrequency :: Maybe PositiveInt
        , timingRepeatFrequencyMax :: Maybe PositiveInt
        , timingRepeatPeriod :: Maybe Decimal
        , timingRepeatPeriodMax :: Maybe Decimal
        , timingRepeatPeriodUnit :: Maybe UnitsOfTime
        , timingRepeatDayOfWeek :: [Code]
        , timingRepeatTimeOfDay :: [Time]
        , timingRepeatWhen :: [EventTiming]
        , timingRepeatOffset :: Maybe UnsignedInt}
    deriving (Eq, Show)

instance ToJSON TimingRepeat where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (timingRepeatId c)
        ,  "extension" .= toJSON (timingRepeatExtension c)
        ,  toTimingRepeatBoundsJSON (timingRepeatBounds c)
        ,  "count"        .= toJSON (timingRepeatCount c)
        ,  "countMax"     .= toJSON (timingRepeatCountMax c)
        ,  "duration"     .= toJSON (timingRepeatDuration c)
        ,  "durationMax"  .= toJSON (timingRepeatDurationMax c)
        ,  "durationUnit" .= toJSON (timingRepeatDurationUnit c)
        ,  "frequency"    .= toJSON (timingRepeatFrequency c)
        ,  "frequencyMax" .= toJSON (timingRepeatFrequencyMax c)
        ,  "period"       .= toJSON (timingRepeatPeriod c)
        ,  "periodMax"    .= toJSON (timingRepeatPeriodMax c)
        ,  "periodUnit"   .= toJSON (timingRepeatPeriodUnit c)
        ,  "dayOfWeek"    .= toJSON (timingRepeatDayOfWeek c)
        ,  "timeOfDay"    .= toJSON (timingRepeatTimeOfDay c)
        ,  "when"         .= toJSON (timingRepeatWhen c)
        ,  "offset"       .= toJSON (timingRepeatOffset c)
        ]
instance FromJSON TimingRepeat where
    parseJSON = withObject "TimingRepeat" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        bo <- parseBoundsDuration o <|> parseBoundsRange o <|> parseBoundsPeriod o
        co <- o .:? "count"
        cm <- o .:? "countMax"
        d  <- o .:? "duration"
        dm <- o .:? "durationMax"
        du <- o .:? "durationUnit"
        f  <- o .:? "frequency"
        fm <- o .:? "frequencyMax"
        p  <- o .:? "period"
        pm <- o .:? "periodMax"
        pu <- o .:? "periodUnit"
        dow <- o .: "dayOfWeek" .!= []
        tod <- o .: "timeOfDay" .!= []
        w   <- o .:? "when" .!= []
        o   <- o .:? "offset"
        return $ TimingRepeat{
                    timingRepeatAttribs=[]
                  , timingRepeatId=id
                  , timingRepeatExtension=ext
                  , timingRepeatBounds=bo
                  , timingRepeatCount=co
                  , timingRepeatCountMax=cm
                  , timingRepeatDuration=d
                  , timingRepeatDurationMax=dm
                  , timingRepeatDurationUnit=du
                  , timingRepeatFrequency=f
                  , timingRepeatFrequencyMax=fm
                  , timingRepeatPeriod=p
                  , timingRepeatPeriodMax=pm
                  , timingRepeatPeriodUnit=pu
                  , timingRepeatDayOfWeek=dow
                  , timingRepeatTimeOfDay=tod
                  , timingRepeatWhen=w
                  , timingRepeatOffset=o}
        where parseBoundsDuration o = do
                hasD <- o .: "boundsDuration" 
                return $ (TimingRepeatBoundsDuration hasD)
              parseBoundsRange o = do
                hasR <- o .: "boundsRange"
                return $ (TimingRepeatBoundsRange hasR)
              parseBoundsPeriod o = do
                hasP <- o .: "boundsPeriod"
                return $ (TimingRepeatBoundsPeriod hasP)



data Timing
    = Timing {
          timingSuper :: BackboneElement
        , timingEvent :: [DateTime]
        , timingRepeat :: Maybe TimingRepeat
        , timingCode :: Maybe CodeableConcept}
    deriving (Eq, Show)

instance ToJSON Timing where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        toBackboneElementJSON (timingSuper c)
        ++
        [
           "event"  .= toJSON (timingEvent c)
        ,  "repeat" .= toJSON (timingRepeat c)
        ,  "code"   .= toJSON (timingCode c)
        ]
instance FromJSON Timing where
    parseJSON = withObject "Timing" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        mxt <- o .:? "modifierExtension" .!= []
        e <- o .:  "event" .!= []
        r <- o .:? "repeat"
        c <- o .:? "code"
        return $ Timing{
                    timingSuper = mkBackboneElement id ext mxt
                  , timingEvent=e
                  , timingRepeat=r
                  , timingCode=c}

{-
 - Metadata
-}

data ContactDetail
    = ContactDetail {
          contactDetailAttribs :: [AnyAttrib]
        , contactDetailId :: Maybe Id
        , contactDetailExtension :: [Extension]
        , contactDetailName :: Maybe Text
        , contactDetailTelecom :: [ContactPoint]}
    deriving (Eq, Show)

instance ToJSON ContactDetail where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (contactDetailId c)
        ,  "extension" .= toJSON (contactDetailExtension c)
        ,  "name" .= toJSON (contactDetailName c)
        ,  "telecom" .= toJSON (contactDetailTelecom c)
        ]
instance FromJSON ContactDetail where
    parseJSON = withObject "ContactDetail" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        n <- o .:? "name"
        t <- o .:  "telecom" .!= []
        return $ ContactDetail{
                    contactDetailAttribs=[]
                  , contactDetailId=id
                  , contactDetailExtension=ext
                  , contactDetailName=n
                  , contactDetailTelecom=t}

data ContributorType
    = CtAuthor | CtEditor | CtEndorser | CtReviewer
    deriving (Eq, Show)

instance ToJSON ContributorType where
    toJSON (CtAuthor) = String "author"
    toJSON (CtEditor) = String "editor"
    toJSON (CtEndorser) = String "endorser"
    toJSON (CtReviewer) = String "reviewer"
instance FromJSON ContributorType where
    parseJSON (String s) = case T.unpack s of
      "author" -> return CtAuthor
      "editor" -> return CtEditor
      "endorser" -> return CtEndorser
      "reviewer" -> return CtReviewer

data Contributor
    = Contributor {
          contributorAttribs :: [AnyAttrib]
        , contributorId :: Maybe Id
        , contributorExtension :: [Extension]
        , contributorType :: ContributorType
        , contributorName :: Text
        , contributorContact :: [ContactDetail]}
    deriving (Eq, Show)

instance ToJSON Contributor where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (contributorId c)
        ,  "extension" .= toJSON (contributorExtension c)
        ,  "type"   .= toJSON (contributorType c)
        ,  "name" .= toJSON (contributorName c)
        ,  "contact" .= toJSON (contributorContact c)
        ]
instance FromJSON Contributor where
    parseJSON = withObject "Contributor" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        t <- o .:  "type"
        n <- o .:  "name"
        c <- o .:  "contact" .!= []
        return $ Contributor{
                    contributorAttribs=[]
                  , contributorId=id
                  , contributorExtension=ext
                  , contributorName=n
                  , contributorType=t
                  , contributorContact=c}

data DataRequirementCodeFilter
    = DataRequirementCodeFilter {
          dataRequirementCodeFilterAttribs :: [AnyAttrib]
        , dataRequirementCodeFilterId :: Maybe Id
        , dataRequirementCodeFilterExtension :: [Extension]
        , dataRequirementCodeFilterPath :: Maybe Text
        , dataRequirementCodeFilterSearchParam :: Maybe Text
        , dataRequirementCodeFilterValueSet :: Maybe Canonical
        , dataRequirementCodeFilterCode :: [Coding]}
    deriving (Eq, Show)

instance ToJSON DataRequirementCodeFilter where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (dataRequirementCodeFilterId c)
        ,  "extension" .= toJSON (dataRequirementCodeFilterExtension c)
        ,  "path" .= toJSON (dataRequirementCodeFilterPath c)
        ,  "searchParam" .= toJSON (dataRequirementCodeFilterSearchParam c)
        ,  "valueSet"    .= toJSON (dataRequirementCodeFilterValueSet c)
        ,  "code"        .= toJSON (dataRequirementCodeFilterCode c)
        ]
instance FromJSON DataRequirementCodeFilter where
    parseJSON = withObject "DataRequirementCodeFilter" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        p <- o .:? "path"
        s <- o .:? "searchParam"
        v <- o .:? "valueSet"
        c <- o .: "code" .!= []
        return $ DataRequirementCodeFilter{
                    dataRequirementCodeFilterAttribs=[]
                  , dataRequirementCodeFilterId=id
                  , dataRequirementCodeFilterExtension=ext
                  , dataRequirementCodeFilterPath=p
                  , dataRequirementCodeFilterSearchParam=s
                  , dataRequirementCodeFilterValueSet=v
                  , dataRequirementCodeFilterCode=c}

data DataRequirementDateFilterValue
    = DataRequirementDateFilterValueDateTime DateTime
    | DataRequirementDateFilterValuePeriod Period
    | DataRequirementDateFilterValueDuration Duration
    deriving (Eq, Show)

toDataRequirementDateFilterValueJSON (DataRequirementDateFilterValueDateTime c) = ("valueDateTime", toJSON c)
toDataRequirementDateFilterValueJSON (DataRequirementDateFilterValuePeriod   c) = ("valuePeriod", toJSON c)
toDataRequirementDateFilterValueJSON (DataRequirementDateFilterValueDuration c) = ("valueDuration", toJSON c)

data DataRequirementDateFilter
    = DataRequirementDateFilter {
          dataRequirementDateFilterAttribs :: [AnyAttrib]
        , dataRequirementDateFilterId :: Maybe Id
        , dataRequirementDateFilterExtension :: [Extension]
        , dataRequirementDateFilterPath :: Maybe Text
        , dataRequirementDateFilterSearchParam :: Maybe Text
        , dataRequirementDateFilterValue :: DataRequirementDateFilterValue}
    deriving (Eq, Show)

instance ToJSON DataRequirementDateFilter where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (dataRequirementDateFilterId c)
        ,  "extension" .= toJSON (dataRequirementDateFilterExtension c)
        ,  "path"        .= toJSON (dataRequirementDateFilterPath c)
        ,  "searchParam" .= toJSON (dataRequirementDateFilterSearchParam c)
        ,  toDataRequirementDateFilterValueJSON (dataRequirementDateFilterValue c)
        ]
instance FromJSON DataRequirementDateFilter where
    parseJSON = withObject "DataRequirementDateFilter" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        p <- o .:? "country"
        s <- o .:? "jurisdiction"
        v <- parseValueDT o <|> parseValueP o <|> parseValueD o
        return $ DataRequirementDateFilter{
                    dataRequirementDateFilterAttribs=[]
                  , dataRequirementDateFilterId=id
                  , dataRequirementDateFilterExtension=ext
                  , dataRequirementDateFilterPath=p
                  , dataRequirementDateFilterSearchParam=s
                  , dataRequirementDateFilterValue=v}
        where parseValueDT o = do
                hasDT <- o .: "valueDateTime" 
                return $ (DataRequirementDateFilterValueDateTime hasDT)
              parseValueP o = do
                hasP <- o .: "valuePeriod"
                return $ (DataRequirementDateFilterValuePeriod hasP)
              parseValueD o = do
                hasD <- o .: "valueDuration"
                return $ (DataRequirementDateFilterValueDuration hasD)


data DataRequirementSort
    = DataRequirementSort {
          dataRequirementSortAttribs :: [AnyAttrib]
        , dataRequirementSortId :: Maybe Id
        , dataRequirementSortExtension :: [Extension]
        , dataRequirementSortPath :: Text
        , dataRequirementSortDirection :: SortDirection}
    deriving (Eq, Show)

instance ToJSON DataRequirementSort where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (dataRequirementSortId c)
        ,  "extension" .= toJSON (dataRequirementSortExtension c)
        ,  "path"      .= toJSON (dataRequirementSortPath c)
        ,  "direction" .= toJSON (dataRequirementSortDirection c)
        ]
instance FromJSON DataRequirementSort where
    parseJSON = withObject "DataRequirementSort" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        p <- o .:  "path"
        d <- o .:  "direction"
        return $ DataRequirementSort{
                    dataRequirementSortAttribs=[]
                  , dataRequirementSortId=id
                  , dataRequirementSortExtension=ext
                  , dataRequirementSortPath=p
                  , dataRequirementSortDirection=d}

data DataRequirementSubject
    = DataRequirementSubjectCodeableConcept CodeableConcept
    | DataRequirementSubjectReference Reference
    deriving (Eq, Show)

toDataRequirementSubjectJSON (DataRequirementSubjectCodeableConcept c) = ("subjectCodeableConcept", toJSON c)
toDataRequirementSubjectJSON (DataRequirementSubjectReference c) = ("subjectReference", toJSON c)

data DataRequirement
    = DataRequirement {
          dataRequirementAttribs :: [AnyAttrib]
        , dataRequirementId :: Maybe Id
        , dataRequirementExtension :: [Extension]
        , dataRequirementType :: Code
        , dataRequirementProfile :: [Canonical]
        , dataRequirementSubject :: DataRequirementSubject
        , dataRequirementMustSupport :: [Text]
        , dataRequirementCodeFilter :: [DataRequirementCodeFilter]
        , dataRequirementDateFilter :: [DataRequirementDateFilter]
        , dataRequirementLimit :: Maybe PositiveInt
        , dataRequirementSort :: [DataRequirementSort]}
    deriving (Eq, Show)

instance ToJSON DataRequirement where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (dataRequirementId c)
        ,  "extension" .= toJSON (dataRequirementExtension c)
        ,  "type"    .= toJSON (dataRequirementType c)
        ,  "profile" .= toJSON (dataRequirementProfile c)
        ,  toDataRequirementSubjectJSON (dataRequirementSubject c)
        ,  "mustSupport" .= toJSON (dataRequirementMustSupport c)
        ,  "codeFilter"  .= toJSON (dataRequirementCodeFilter c)
        ,  "dateFilter"  .= toJSON (dataRequirementDateFilter c)
        ,  "limit"       .= toJSON (dataRequirementLimit c)
        ,  "sort"        .= toJSON (dataRequirementSort c)
        ]
instance FromJSON DataRequirement where
    parseJSON = withObject "DataRequirement" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        t <- o .:  "type"
        p <- o .:  "profile" .!= []
        s <- parseSubjectCC o <|> parseSubjectR o
        m <- o .:  "mustSupport" .!= []
        c <- o .:  "codeFilter" .!= []
        d <- o .:  "dateFilter" .!= []
        l <- o .:? "limit"
        so <- o .:  "sort" .!= []
        return $ DataRequirement{
                    dataRequirementAttribs=[]
                  , dataRequirementId=id
                  , dataRequirementExtension=ext
                  , dataRequirementType=t
                  , dataRequirementProfile=p
                  , dataRequirementSubject=s
                  , dataRequirementMustSupport=m
                  , dataRequirementCodeFilter=c
                  , dataRequirementDateFilter=d
                  , dataRequirementLimit=l
                  , dataRequirementSort=so}
        where parseSubjectCC o = do
                hasCC <- o .: "subjectCodeableConcept" 
                return $ (DataRequirementSubjectCodeableConcept hasCC)
              parseSubjectR o = do
                hasR <- o .: "subjectReference"
                return $ (DataRequirementSubjectReference hasR)


data ExpressionLanguage
    = ElApplicationXFhirQuery
    | ElTextCql
    | ElTextFhirpath
    deriving (Eq, Show)

instance ToJSON ExpressionLanguage where
    toJSON (ElApplicationXFhirQuery) = String "application/x-fhir-query"
    toJSON (ElTextCql) = String "text/cql"
    toJSON (ElTextFhirpath) = String "text/fhirpath"
instance FromJSON ExpressionLanguage where
    parseJSON (String s) = case T.unpack s of
      "application/x-fhir-query" -> return ElApplicationXFhirQuery
      "text/cql"                 -> return ElTextCql
      "text/fhirpath"            -> return ElTextFhirpath


data Expression
    = Expression {
          expressionAttribs :: [AnyAttrib]
        , expressionId :: Maybe Id
        , expressionExtension :: [Extension]
        , expressionDescription :: Maybe Text
        , expressionName :: Maybe Id
        , expressionLanguage :: ExpressionLanguage
        , expressionExpression :: Maybe Text
        , expressionReference :: Maybe Uri}
    deriving (Eq, Show)

instance ToJSON Expression where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (expressionId c)
        ,  "extension" .= toJSON (expressionExtension c)
        ,  "description" .= toJSON (expressionDescription c)
        ,  "name"        .= toJSON (expressionName c)
        ,  "language"    .= toJSON (expressionLanguage c)
        ,  "expression"  .= toJSON (expressionExpression c)
        ,  "reference"   .= toJSON (expressionReference c)
        ]
instance FromJSON Expression where
    parseJSON = withObject "Expression" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        d <- o .:? "description"
        n <- o .:? "name"
        l <- o .:  "language"
        e <- o .:? "expression"
        r <- o .:? "reference"
        return $ Expression{
                    expressionAttribs=[]
                  , expressionId=id
                  , expressionExtension=ext
                  , expressionDescription=d
                  , expressionName=n
                  , expressionLanguage=l
                  , expressionExpression=e
                  , expressionReference=r}

data ParameterDefinition
    = ParameterDefinition {
          parameterDefinitionAttribs :: [AnyAttrib]
        , parameterDefinitionId :: Maybe Id
        , parameterDefinitionExtension :: [Extension]
        , parameterDefinitionName :: Code
        , parameterDefinitionUse :: Code
        , parameterDefinitionMin :: Maybe Integer
        , parameterDefinitionMax :: Maybe Text
        , parameterDefinitionDocumentation :: Maybe Text
        , parameterDefinitionType :: Code
        , parameterDefinitionProfile :: Maybe Canonical}
    deriving (Eq, Show)

instance ToJSON ParameterDefinition where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (parameterDefinitionId c)
        ,  "extension" .= toJSON (parameterDefinitionExtension c)
        ,  "name" .= toJSON (parameterDefinitionName c)
        ,  "use"  .= toJSON (parameterDefinitionUse c)
        ,  "min"  .= toJSON (parameterDefinitionMin c)
        ,  "max"  .= toJSON (parameterDefinitionMax c)
        ,  "documentation" .= toJSON (parameterDefinitionDocumentation c)
        ,  "type"          .= toJSON (parameterDefinitionType c)
        ,  "profile"       .= toJSON (parameterDefinitionProfile c)
        ]
instance FromJSON ParameterDefinition where
    parseJSON = withObject "ParameterDefinition" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        n  <- o .:  "name"
        u  <- o .:  "use"
        mi <- o .:? "min"
        ma <- o .:? "max"
        d  <- o .:? "documentation"
        t  <- o .:  "type"
        p  <- o .:? "profile"
        return $ ParameterDefinition{
                    parameterDefinitionAttribs=[]
                  , parameterDefinitionId=id
                  , parameterDefinitionExtension=ext
                  , parameterDefinitionName=n
                  , parameterDefinitionUse=u
                  , parameterDefinitionMin=mi
                  , parameterDefinitionMax=ma
                  , parameterDefinitionDocumentation=d
                  , parameterDefinitionType=t
                  , parameterDefinitionProfile=p}

data RelatedArtifactType 
    = RatCitation
    | RatComposedOf
    | RatDependsOn
    | RatDerivedFrom
    | RatDocumentation
    | RatJustification
    | RatPredecessor
    | RatSuccessor
    deriving (Eq, Show)

instance ToJSON RelatedArtifactType where
    toJSON (RatCitation)     = String "citation"
    toJSON (RatComposedOf)   = String "composed-of"
    toJSON (RatDependsOn)    = String "depends-on"
    toJSON (RatDerivedFrom)  = String "derived-from"
    toJSON (RatDocumentation) = String "documentation"
    toJSON (RatJustification) = String "justification"
    toJSON (RatPredecessor)   = String "predecessor"
    toJSON (RatSuccessor)     = String "successor"
instance FromJSON RelatedArtifactType where
    parseJSON (String s) = case T.unpack s of
      "citation"     -> return RatCitation
      "composed-of"  -> return RatComposedOf
      "depends-on"   -> return RatDependsOn
      "derived-from" -> return RatDerivedFrom
      "documentation" -> return RatDocumentation
      "justification" -> return RatJustification
      "predecessor"   -> return RatPredecessor
      "succesoor"     -> return RatSuccessor


data RelatedArtifact
    = RelatedArtifact {
          relatedArtifactAttribs :: [AnyAttrib]
        , relatedArtifactId :: Maybe Id
        , relatedArtifactExtension :: [Extension]
        , relatedArtifactType :: RelatedArtifactType
        , relatedArtifactLabel :: Maybe Text
        , relatedArtifactDisplay :: Maybe Text
        , relatedArtifactCitation :: Maybe Markdown
        , relatedArtifactUrl :: Maybe Url
        , relatedArtifactDocument :: Maybe Attachment
        , relatedArtifactResource :: Maybe Canonical}
    deriving (Eq, Show)

instance ToJSON RelatedArtifact where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id"        .= toJSON (relatedArtifactId c)
        ,  "extension" .= toJSON (relatedArtifactExtension c)
        ,  "type"     .= toJSON (relatedArtifactType c)
        ,  "label"    .= toJSON (relatedArtifactLabel c)
        ,  "display"  .= toJSON (relatedArtifactDisplay c)
        ,  "citation" .= toJSON (relatedArtifactCitation c)
        ,  "url"      .= toJSON (relatedArtifactUrl c)
        ,  "document" .= toJSON (relatedArtifactDocument c)
        ,  "resource" .= toJSON (relatedArtifactResource c)
        ]
instance FromJSON RelatedArtifact where
    parseJSON = withObject "RelatedArtifact" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        t <- o .:  "type"
        l <- o .:? "label"
        di<- o .:? "display"
        c <- o .:? "citation"
        u <- o .:? "url"
        d <- o .:? "document"
        r <- o .:? "resource"
        return $ RelatedArtifact{
                    relatedArtifactAttribs=[]
                  , relatedArtifactId=id
                  , relatedArtifactExtension=ext
                  , relatedArtifactType=t
                  , relatedArtifactLabel=l
                  , relatedArtifactDisplay=di
                  , relatedArtifactCitation=c
                  , relatedArtifactUrl=u
                  , relatedArtifactDocument=d
                  , relatedArtifactResource=r}

data TriggerDefinitionTiming
    = TriggerDefinitionTimingTiming Timing
    | TriggerDefinitionTimingReference Reference
    | TriggerDefinitionTimingDate Date
    | TriggerDefinitionTimingDateTime DateTime
    deriving (Eq, Show)

toTriggerDefinitionTimingJSON (TriggerDefinitionTimingTiming    c) = ("timingTiming", toJSON c)
toTriggerDefinitionTimingJSON (TriggerDefinitionTimingReference c) = ("timingReference", toJSON c)
toTriggerDefinitionTimingJSON (TriggerDefinitionTimingDate      c) = ("timingDate", toJSON c)
toTriggerDefinitionTimingJSON (TriggerDefinitionTimingDateTime  c) = ("timingDateTime", toJSON c)

data TriggerType
    = TtDataAccessEnded
    | TtDataAccessed
    | TtDataAdded
    | TtDataChanged
    | TtDataModified
    | TtDataRemoved
    | TtNamedEvent
    | TtPeriodic
    deriving (Eq, Show)

instance ToJSON TriggerType where
    toJSON (TtDataAccessEnded) = String "data-access-ended"
    toJSON (TtDataAccessed) = String "data-accessed"
    toJSON (TtDataAdded)    = String "data-added"
    toJSON (TtDataChanged)  = String "data-changed"
    toJSON (TtDataModified) = String "data-modified"
    toJSON (TtDataRemoved)  = String "data-removed"
    toJSON (TtNamedEvent)   = String "named-event"
    toJSON (TtPeriodic)     = String "periodic"
instance FromJSON TriggerType where
    parseJSON (String s) = case T.unpack s of
      "data-access-ended" -> return TtDataAccessEnded
      "data-accessed" -> return TtDataAccessed
      "data-added"    -> return TtDataAdded
      "data-changed"  -> return TtDataChanged
      "data-modified" -> return TtDataModified
      "data-removed"  -> return TtDataRemoved
      "named-event"   -> return TtNamedEvent
      "periodic"      -> return TtPeriodic


data TriggerDefinition
    = TriggerDefinition {
          triggerDefinitionAttribs :: [AnyAttrib]
        , triggerDefinitionId :: Maybe Id
        , triggerDefinitionExtension :: [Extension]
        , triggerDefinitionType :: TriggerType
        , triggerDefinitionName :: Maybe Text
        , triggerDefinitionTiming :: TriggerDefinitionTiming
        , triggerDefinitionData :: [DataRequirement]
        , triggerDefinitionCondition :: Maybe Expression}
    deriving (Eq, Show)

instance ToJSON TriggerDefinition where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (triggerDefinitionId c)
        ,  "extension" .= toJSON (triggerDefinitionExtension c)
        ,  "type" .= toJSON (triggerDefinitionType c)
        ,  "name" .= toJSON (triggerDefinitionName c)
        ,  toTriggerDefinitionTimingJSON (triggerDefinitionTiming c)
        ,  "data"      .= toJSON (triggerDefinitionData c)
        ,  "condition" .= toJSON (triggerDefinitionCondition c)
        ]
instance FromJSON TriggerDefinition where
    parseJSON = withObject "TriggerDefinition" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        t <- o .:  "type"
        n <- o .:? "name"
        ti<- parseTimingT o <|> parseTimingR o <|> parseTimingD o <|> parseTimingDT o
        d <- o .:  "data" .!= []
        c <- o .:? "condition"
        return $ TriggerDefinition{
                    triggerDefinitionAttribs=[]
                  , triggerDefinitionId=id
                  , triggerDefinitionExtension=ext
                  , triggerDefinitionType=t
                  , triggerDefinitionName=n
                  , triggerDefinitionTiming=ti
                  , triggerDefinitionData=d
                  , triggerDefinitionCondition=c}
        where parseTimingT o = do
                hasT <- o .: "timingTiming" 
                return $ (TriggerDefinitionTimingTiming hasT)
              parseTimingR o = do
                hasR <- o .: "timingReference"
                return $ (TriggerDefinitionTimingReference hasR)
              parseTimingD o = do
                hasD <- o .: "timingDate" 
                return $ (TriggerDefinitionTimingDate hasD)
              parseTimingDT o = do
                hasDT  <- o .: "timingDateTime"
                return $ (TriggerDefinitionTimingDateTime hasDT)



data UsageContextValue
    = UcvCC CodeableConcept
    | UcvQuantity Quantity
    | UcvRange Range
    | UcvReference Reference
    deriving (Eq, Show)

toUsageContextValueJSON (UcvCC        cc) = ("valueCodeableConcept", toJSON cc)
toUsageContextValueJSON (UcvQuantity   q) = ("valueQuantity", toJSON q)
toUsageContextValueJSON (UcvRange      r) = ("valueRange", toJSON r)
toUsageContextValueJSON (UcvReference  r) = ("valueReference", toJSON r)

data UsageContext
    = UsageContext {
          usageContextAttribs :: [AnyAttrib]
        , usageContextId :: Maybe Id
        , usageContextExtension :: [Extension]
        , usageContextCode :: Coding
        , usageContextValue :: UsageContextValue}
    deriving (Eq, Show)

instance ToJSON UsageContext where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (usageContextId c)
        ,  "extension" .= toJSON (usageContextExtension c)
        ,  "code"  .= toJSON (usageContextCode c)
        ,  toUsageContextValueJSON (usageContextValue c)
        ]
instance FromJSON UsageContext where
    parseJSON = withObject "UsageContext" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        c <- o .: "code"
        v <- parseValueCC o <|> parseValueQ o <|> parseValueR o <|> parseValueRef o
        return $ UsageContext{
                    usageContextAttribs=[]
                  , usageContextId=id
                  , usageContextExtension=ext
                  , usageContextCode=c
                  , usageContextValue=v}
        where parseValueCC o = do
                hasCC <- o .: "valueCodeableConcept" 
                return $ (UcvCC hasCC)
              parseValueQ o = do
                hasQ <- o .: "valueQuantity"
                return $ (UcvQuantity hasQ)
              parseValueR o = do
                hasR <- o .: "valueRange" 
                return $ (UcvRange hasR)
              parseValueRef o = do
                hasRef <- o .: "valueReference"
                return $ (UcvReference hasRef)


{-
 - SpecialPurpose
-}

data DosageAsNeeded
    = DosageAsNeededBoolean Boolean
    | DosageAsNeededCodeableConcept CodeableConcept
    deriving (Eq, Show)

toDosageAsNeededJSON (DosageAsNeededBoolean          b) = ("asNeededBoolean", toJSON b)
toDosageAsNeededJSON (DosageAsNeededCodeableConcept cc) = ("asNeededCodeableConcept", toJSON cc)

data DosageDoseAndRateDose
    = DosageDoseAndRateDoseRange Range
    | DosageDoseAndRateDoseQuantity Quantity
    deriving (Eq, Show)

toDosageDoseAndRateDoseJSON (DosageDoseAndRateDoseRange    r) = ("doseAndRateRange", toJSON r)
toDosageDoseAndRateDoseJSON (DosageDoseAndRateDoseQuantity q) = ("doseAndRateQuantity", toJSON q)

data DosageDoseAndRateRate
    = DosageDoseAndRateRateRatio Ratio
    | DosageDoseAndRateRateRange Range
    | DosageDoseAndRateRateQuantity Quantity
    deriving (Eq, Show)

toDosageDoseAndRateRateJSON (DosageDoseAndRateRateRatio    r) = ("doseAndRateRatio", toJSON r)
toDosageDoseAndRateRateJSON (DosageDoseAndRateRateRange    r) = ("doseAndRateRange", toJSON r)
toDosageDoseAndRateRateJSON (DosageDoseAndRateRateQuantity q) = ("doseAndRateQuantity", toJSON q)

data DosageDoseAndRate
    = DosageDoseAndRate {
          dosageDoseAndRateAttribs :: [AnyAttrib]
        , dosageDoseAndRateId :: Maybe Id
        , dosageDoseAndRateExtension :: [Extension]
        , dosageDoseAndRateType :: Maybe CodeableConcept
        , dosageDoseAndRateDose :: DosageDoseAndRateDose
        , dosageDoseAndRateRate :: DosageDoseAndRateRate}
    deriving (Eq, Show)

instance ToJSON DosageDoseAndRate where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        [
           "id" .= toJSON (dosageDoseAndRateId c)
        ,  "extension" .= toJSON (dosageDoseAndRateExtension c)
        ,  "type" .= toJSON (dosageDoseAndRateType c)
        ,  toDosageDoseAndRateDoseJSON (dosageDoseAndRateDose c)
        ,  toDosageDoseAndRateRateJSON (dosageDoseAndRateRate c)
        ]
instance FromJSON DosageDoseAndRate where
    parseJSON = withObject "DosageDoseAndRate" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        t <- o .:? "type"
        d <- parseDoseRg o <|> parseDoseQu o
        r <- parseRateRg o <|> parseRateRa o <|> parseRateQu o
        return $ DosageDoseAndRate{
                    dosageDoseAndRateAttribs=[]
                  , dosageDoseAndRateId=id
                  , dosageDoseAndRateExtension=ext
                  , dosageDoseAndRateType=t
                  , dosageDoseAndRateDose=d
                  , dosageDoseAndRateRate=r
                  }
        where parseDoseRg o = do
                hasR <- o .: "doseRange"
                return $ (DosageDoseAndRateDoseRange hasR)
              parseDoseQu o = do
                hasQ <- o .: "doseQuantity"
                return $ (DosageDoseAndRateDoseQuantity hasQ)
              parseRateRa o = do
                hasR <- o .: "rateRatio" 
                return $ (DosageDoseAndRateRateQuantity hasR)
              parseRateRg o = do
                hasR <- o .: "rateRange" 
                return $ (DosageDoseAndRateRateQuantity hasR)
              parseRateQu o = do
                hasQ <- o .: "rateQuantity"
                return $ (DosageDoseAndRateRateQuantity hasQ)

data Dosage
    = Dosage {
          dosageSuper :: BackboneElement
        , dosageSequence :: Maybe Integer
        , dosageText :: Maybe Text
        , dosageAdditionalInstruction :: [CodeableConcept]
        , dosagePatientInstruction :: Maybe Text
        , dosageTiming :: Maybe Timing
        , dosageAsNeeded :: DosageAsNeeded
        , dosageSite :: Maybe CodeableConcept
        , dosageRoute :: Maybe CodeableConcept
        , dosageMethod :: Maybe CodeableConcept
        , dosageDoseAndRate :: [DosageDoseAndRate]
        , dosageMaxDosePerPeriod :: Maybe Ratio
        , dosageMaxDosePerAdministration :: Maybe Quantity
        , dosageMaxDosePerLifetime :: Maybe Quantity}
    deriving (Eq, Show)

instance ToJSON Dosage where
    toJSON c = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
        toBackboneElementJSON (dosageSuper c)
        ++
        [
           "sequence" .= toJSON (dosageSequence c)
        ,  "text" .= toJSON (dosageText c)
        ,  "additionalInstruction" .= toJSON (dosageAdditionalInstruction c)
        ,  "patientInstruction" .= toJSON (dosagePatientInstruction c)
        ,  "timing" .= toJSON (dosageTiming c)
        ,  toDosageAsNeededJSON  (dosageAsNeeded c)
        ,  "site" .= toJSON (dosageSite c)
        ,  "route" .= toJSON (dosageRoute c)
        ,  "method" .= toJSON (dosageMethod c)
        ,  "doseAndRate" .= toJSON (dosageDoseAndRate c)
        ,  "maxDosePerPeriod" .= toJSON (dosageMaxDosePerPeriod c)
        ,  "maxDosePerAdministration" .= toJSON (dosageMaxDosePerAdministration c)
        ,  "maxDosePerLifetime" .= toJSON (dosageMaxDosePerLifetime c)
        ]
instance FromJSON Dosage where
    parseJSON = withObject "Dosage" $ \o -> do
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        mxt <- o .:? "modifierExtension" .!= []
        se <- o .:? "sequence"
        te <- o .:? "text"
        ad <- o .:? "additionalInstruction" .!= []
        pa <- o .:? "patientInstruction"
        ti <- o .:? "timing"
        as <- parseAsNeededB o <|> parseAsNeededCC o
        si <- o .:? "site"
        ro <- o .:? "route"
        me <- o .:? "method"
        dr <- o .:? "doseAndRate" .!= []
        dp <- o .:? "maxDosePerPeriod"
        da <- o .:? "maxDosePerAdministration"
        dl <- o .:? "maxDosePerLifetime"
        return $ Dosage{
                    dosageSuper = mkBackboneElement id ext mxt
                  , dosageSequence=se
                  , dosageText=te
                  , dosageAdditionalInstruction=ad
                  , dosagePatientInstruction=pa
                  , dosageTiming=ti
                  , dosageAsNeeded=as
                  , dosageSite=si
                  , dosageRoute=ro
                  , dosageMethod=me
                  , dosageDoseAndRate=dr
                  , dosageMaxDosePerPeriod=dp
                  , dosageMaxDosePerAdministration=da
                  , dosageMaxDosePerLifetime=dl}
        where parseAsNeededB o = do
                hasB <- o .: "asNeededBoolean"
                return $ (DosageAsNeededBoolean hasB)
              parseAsNeededCC o = do
                hasCC <- o .: "asNeededCodeableConcept"
                return $ (DosageAsNeededCodeableConcept hasCC)


data Meta
    = Meta {
          metaAttribs :: [AnyAttrib]
        , metaId :: Maybe Id
        , metaExtension :: [Extension]
        , metaVersionId :: Maybe Id
        , metaLastUpdated :: Maybe Instant
        , metaSource :: Maybe Uri
        , metaProfile :: [Canonical]
        , metaSecurity :: [Coding]
        , metaTag :: [Coding]}
    deriving (Eq, Show)

instance ToJSON Meta where
    toJSON m = object $
       filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
       [
         "id"        .= toJSON (metaId m)
       , "extension" .= toJSON (metaExtension m)
       , "versionId" .= toJSON (metaVersionId m)
       , "lastUpdated" .= toJSON (metaLastUpdated m)
       , "source" .= toJSON (metaSource m)
       , "profile" .= toJSON (metaProfile m)
       , "security" .= toJSON (metaSecurity m)
       , "tag" .= toJSON (metaTag m)
       ]
instance FromJSON Meta where
    parseJSON = withObject "Meta" $ \m -> do
       i  <- m .:? "id"
       e  <- m .:? "extension" .!= []
       vi <- m .:? "versionId"
       l  <- m .:? "lastUpdated"
       so <- m .:? "source"
       p  <- m .:? "profile" .!= []
       se <- m .:? "security" .!= []
       t  <- m .:? "tag" .!= []
       return Meta{
             metaAttribs = []
           , metaId = i
           , metaExtension = e
           , metaVersionId = vi
           , metaLastUpdated = l
           , metaSource = so
           , metaProfile = p
           , metaSecurity = se
           , metaTag = t
           }

data MimeType
    = MtXml | MtJson | MtTtl
    deriving (Eq, Show)

instance ToJSON MimeType where
    toJSON MtXml = String "xml"
    toJSON MtJson= String "json"
    toJSON MtTtl = String "ttl"

data NarrativeStatus
    = NsAdditional | NsEmpty | NsExtensions | NsGenerated
    deriving (Eq, Show)

instance FromJSON NarrativeStatus where
    parseJSON (String s) = case T.unpack s of
        "additional" -> pure NsAdditional
        "empty"      -> pure NsEmpty   
        "extensions" -> pure NsExtensions
        "generated"  -> pure NsGenerated

data Narrative
    = Narrative {
          narrativeAttribs :: [AnyAttrib]
        , narrativeId :: Maybe Id
        , narrativeExtension :: [Extension]
        , narrativeStatus :: NarrativeStatus
        , narrativeXhtmlDiv :: XhtmlDiv}
    deriving (Eq, Show)

instance ToJSON Narrative where
    toJSON e = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
      [
        "id"        .= toJSON (narrativeId e)
      , "extension" .= toJSON (narrativeExtension e)
      , ("status", toNarrativeStatus (narrativeStatus e))
      , "div"       .=  toJSON (narrativeXhtmlDiv e)
      ]
instance FromJSON Narrative where
    parseJSON = withObject "Narrative" $ \o -> do
       i  <- o .:? "id"
       e  <- o .:? "extension" .!= []
       s  <- o .:  "status"
       d  <- o .:  "div"
       return Narrative{
                narrativeAttribs=[]
              , narrativeId=i
              , narrativeExtension=e
              , narrativeStatus= s
              , narrativeXhtmlDiv=d}


data ExtensionValue
-- Primitive
    = ExtensionValueBase64Binary Base64Binary
    | ExtensionValueBoolean Boolean
    | ExtensionValueCanonical Canonical
    | ExtensionValueCode Code
    | ExtensionValueDate Date
    | ExtensionValueDateTime DateTime
    | ExtensionValueDecimal Decimal
    | ExtensionValueId Id
    | ExtensionValueInstant Instant
    | ExtensionValueInteger Integer
    | ExtensionValueMarkdown Markdown
    | ExtensionValueOid Oid
    | ExtensionValuePositiveInt PositiveInt
    | ExtensionValueString Text
    | ExtensionValueTime Time
    | ExtensionValueUnsignedInt UnsignedInt
    | ExtensionValueUri Uri
    | ExtensionValueUrl Url
    | ExtensionValueUuid Uuid
-- GeneralPurpose
    | ExtensionValueAddress Address
    | ExtensionValueAge Age
    | ExtensionValueAnnotation Annotation
    | ExtensionValueAttachment Attachment
    | ExtensionValueCodeableConcept CodeableConcept
    | ExtensionValueCoding Coding
    | ExtensionValueContactPoint ContactPoint
    | ExtensionValueCount Count
    | ExtensionValueDistance Distance
    | ExtensionValueDuration Duration
    | ExtensionValueHumanName HumanName
    | ExtensionValueIdentifier Identifier
    | ExtensionValueMoney Money
    | ExtensionValuePeriod Period
    | ExtensionValueQuantity Quantity
    | ExtensionValueRange Range
    | ExtensionValueRatio Ratio
    | ExtensionValueSampledData SampledData
    | ExtensionValueSignature Signature
    | ExtensionValueTiming Timing
-- Metadata
    | ExtensionValueContactDetail ContactDetail
    | ExtensionValueContributor Contributor
    | ExtensionValueDataRequirement DataRequirement
    | ExtensionValueExpression Expression
    | ExtensionValueParameterDefinition ParameterDefinition
    | ExtensionValueRelatedArtifact RelatedArtifact
    | ExtensionValueTriggerDefinition TriggerDefinition
    | ExtensionValueUsageContext UsageContext
-- SpecialPurpose
    | ExtensionValueReference Reference
    | ExtensionValueDosage Dosage
    | ExtensionValueMeta Meta
    deriving (Eq, Show)

instance ToJSON ExtensionValue where
    toJSON (ExtensionValueMeta m) = object [ ("valueMeta", toJSON m) ]
    toJSON _ = error "extension error"
instance FromJSON ExtensionValue where
    parseJSON = withObject "ExtensionValue" $ \o -> do
      m <- o .: "valueMeta"
      return $ ExtensionValueMeta m
{-
        ["valueBoolean"]  -> ExtensionValueBoolean <$> parseJSON $ getVal en
        _         -> fail "not supported extension"
      where en         = filter (T.isPrefixOf "value") $ HM.keys o
            getVal [k] = maybe emptyObject $ HM.lookup k o
            getVal []  = emptyObject
-}

toExtensionValueJSON (Just (ExtensionValueBase64Binary e)) = ("valueBase64Binary", toJSON e)
toExtensionValueJSON (Just (ExtensionValueBoolean e)) = ("valueBoolean", toJSON e)
toExtensionValueJSON (Just (ExtensionValueCanonical e)) = ("valueCanonical", toJSON e)
toExtensionValueJSON (Just (ExtensionValueCode e)) = ("valueCode", toJSON e)
toExtensionValueJSON (Just (ExtensionValueDate e)) = ("valueDate", toJSON e)
toExtensionValueJSON (Just (ExtensionValueDateTime e)) = ("valueDateTime", toJSON e)
toExtensionValueJSON (Just (ExtensionValueDecimal e)) = ("valueDecimal", toJSON e)
toExtensionValueJSON (Just (ExtensionValueId e)) = ("valueId", toJSON e)
toExtensionValueJSON (Just (ExtensionValueInstant e)) = ("valueInstant", toJSON e)
toExtensionValueJSON (Just (ExtensionValueInteger e)) = ("valueInteger", toJSON e)
toExtensionValueJSON (Just (ExtensionValueMarkdown e)) = ("valueMarkdown", toJSON e)
toExtensionValueJSON (Just (ExtensionValueOid e)) = ("valueOid", toJSON e)
toExtensionValueJSON (Just (ExtensionValuePositiveInt e)) = ("valuePositiveInt", toJSON e)
toExtensionValueJSON (Just (ExtensionValueString e)) = ("valueString", toJSON e)
toExtensionValueJSON (Just (ExtensionValueTime e)) = ("valueTime", toJSON e)
toExtensionValueJSON (Just (ExtensionValueUnsignedInt e)) = ("valueUnsignedInt", toJSON e)
toExtensionValueJSON (Just (ExtensionValueUri e)) = ("valueUri", toJSON e)
toExtensionValueJSON (Just (ExtensionValueUrl e)) = ("valueUrl", toJSON e)
toExtensionValueJSON (Just (ExtensionValueUuid e)) = ("valueUuid", toJSON e)
-- GeneralPurpose
toExtensionValueJSON (Just (ExtensionValueAddress e)) = ("valueAddress", toJSON e)
toExtensionValueJSON (Just (ExtensionValueAge e)) = ("valueAge", toJSON e)
toExtensionValueJSON (Just (ExtensionValueAnnotation e)) = ("valueAnnotation", toJSON e)
toExtensionValueJSON (Just (ExtensionValueAttachment e)) = ("valueAttachment", toJSON e)
toExtensionValueJSON (Just (ExtensionValueCodeableConcept e)) = ("valueCodeableConcept", toJSON e)
toExtensionValueJSON (Just (ExtensionValueCoding e)) = ("valueCoding", toJSON e)
toExtensionValueJSON (Just (ExtensionValueContactPoint e)) = ("valueContactPoint", toJSON e)
toExtensionValueJSON (Just (ExtensionValueCount e)) = ("valueCount", toJSON e)
toExtensionValueJSON (Just (ExtensionValueDistance e)) = ("valueDistance", toJSON e)
toExtensionValueJSON (Just (ExtensionValueDuration e)) = ("valueDuration", toJSON e)
toExtensionValueJSON (Just (ExtensionValueHumanName e)) = ("valueHumanName", toJSON e)
toExtensionValueJSON (Just (ExtensionValueIdentifier e)) = ("valueIdentifier", toJSON e)
toExtensionValueJSON (Just (ExtensionValueMoney e)) = ("valueMoney", toJSON e)
toExtensionValueJSON (Just (ExtensionValuePeriod e)) = ("valuePeriod", toJSON e)
toExtensionValueJSON (Just (ExtensionValueQuantity e)) = ("valueQuantity", toJSON e)
toExtensionValueJSON (Just (ExtensionValueRange e)) = ("valueRange", toJSON e)
toExtensionValueJSON (Just (ExtensionValueRatio e)) = ("valueRatio", toJSON e)
toExtensionValueJSON (Just (ExtensionValueSampledData e)) = ("valueSampledData", toJSON e)
toExtensionValueJSON (Just (ExtensionValueSignature e)) = ("valueSignature", toJSON e)
toExtensionValueJSON (Just (ExtensionValueTiming e)) = ("valueTiming", toJSON e)
-- Metadata
toExtensionValueJSON (Just (ExtensionValueContactDetail e)) = ("valueContactDetail", toJSON e)
toExtensionValueJSON (Just (ExtensionValueContributor e)) = ("valueContributor", toJSON e)
toExtensionValueJSON (Just (ExtensionValueDataRequirement e)) = ("valueDataRequirement", toJSON e)
toExtensionValueJSON (Just (ExtensionValueExpression e)) = ("valueExpression", toJSON e)
toExtensionValueJSON (Just (ExtensionValueParameterDefinition e)) = ("valueParameterDefinition", toJSON e)
toExtensionValueJSON (Just (ExtensionValueRelatedArtifact e)) = ("valueRelatedArtifact", toJSON e)
toExtensionValueJSON (Just (ExtensionValueTriggerDefinition e)) = ("valueTriggerDefinition", toJSON e)
toExtensionValueJSON (Just (ExtensionValueUsageContext e)) = ("valueUsageContext", toJSON e)
-- SpecialPurpose
toExtensionValueJSON (Just (ExtensionValueReference e)) = ("valueReference", toJSON e)
toExtensionValueJSON (Just (ExtensionValueDosage e)) = ("valueDosage", toJSON e)
toExtensionValueJSON (Just (ExtensionValueMeta e)) = ("valueMeta", toJSON e)
toExtensionValueJSON Nothing                       = ("value", Null)

data Extension 
    = Extension {
          extensionUrl :: Uri
        , extensionId :: Maybe Id
        , extensionExtension :: [Extension]
        , extensionValue :: Maybe ExtensionValue}
    deriving (Eq, Show)

instance ToJSON Extension where
    toJSON e = object $
      filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
      [
        ("url",        toJSON (extensionUrl e))
      , ("id",         toJSON (extensionId e))
      , ("extension",  toJSON (extensionExtension e))
      , toExtensionValueJSON (extensionValue e)
      ]
instance FromJSON Extension where
    parseJSON = withObject "Extension" $ \o -> do
        url <- o .:  "url"
        id  <- o .:? "id"
        ext <- o .:? "extension" .!= []
        va  <- parseMaybeExtensionValue o
        return Extension{
              extensionUrl=url
            , extensionId=id
            , extensionExtension=ext
            , extensionValue=va 
            }
parseMaybeExtensionValue :: Object -> Parser (Maybe ExtensionValue)
#if MIN_VERSION_aeson(2,0,0)
parseMaybeExtensionValue o  = parseExtensionValue n o
    where n = fmap AK.toText $ filter (T.isPrefixOf "value" . AK.toText) $ AKM.keys o
#else
parseMaybeExtensionValue o  = parseExtensionValue n o
    where n = filter (T.isPrefixOf "value") $ HM.keys o
#endif

parseExtensionValue :: [Text] -> Object -> Parser (Maybe ExtensionValue)
parseExtensionValue [] o  = o .:? "empty" 
parseExtensionValue ["valueBase64Binary"] o  = do
        c <- o .: "valueBase64Binary"
        return $ Just $ ExtensionValueBase64Binary c
parseExtensionValue ["valueBoolean"] o  = do
        c <- o .: "valueBoolean"
        return $ Just $ ExtensionValueBoolean c
parseExtensionValue ["valueCanonical"] o  = do
        c <- o .: "valueCanonical"
        return $ Just $ ExtensionValueCanonical c
parseExtensionValue ["valueCode"] o  = do
        c <- o .: "valueCode"
        return $ Just $ ExtensionValueCode c
parseExtensionValue ["valueDecimal"] o  = do
        c <- o .: "valueDecimal"
        return $ Just $ ExtensionValueDecimal c
parseExtensionValue ["valueDate"] o  = do
        c <- o .: "valueDate"
        return $ Just $ ExtensionValueDate c
parseExtensionValue ["valueDateTime"] o  = do
        c <- o .: "valueDateTime"
        return $ Just $ ExtensionValueDateTime c
parseExtensionValue ["valueId"] o  = do
        c <- o .: "valueId"
        return $ Just $ ExtensionValueId c
parseExtensionValue ["valueInstant"] o  = do
        c <- o .: "valueInstant"
        return $ Just $ ExtensionValueInstant c
parseExtensionValue ["valueInteger"] o  = do
        c <- o .: "valueInteger"
        return $ Just $ ExtensionValueInteger c
parseExtensionValue ["valueMarkdown"] o  = do
        c <- o .: "valueMarkdown"
        return $ Just $ ExtensionValueMarkdown c
parseExtensionValue ["valueOid"] o  = do
        c <- o .: "valueOid"
        return $ Just $ ExtensionValueOid c
parseExtensionValue ["valuePositiveInt"] o  = do
        c <- o .: "valuePositiveInt"
        return $ Just $ ExtensionValuePositiveInt c
parseExtensionValue ["valueString"] o  = do
        c <- o .: "valueString"
        return $ Just $ ExtensionValueString c
parseExtensionValue ["valueTime"] o  = do
        c <- o .: "valueTime"
        return $ Just $ ExtensionValueTime c
parseExtensionValue ["valueUnsignedInt"] o  = do
        c <- o .: "valueUnsignedInt"
        return $ Just $ ExtensionValueUnsignedInt c
parseExtensionValue ["valueUri"] o  = do
        c <- o .: "valueUri"
        return $ Just $ ExtensionValueUri c
parseExtensionValue ["valueUrl"] o  = do
        c <- o .: "valueUrl"
        return $ Just $ ExtensionValueUrl c
parseExtensionValue ["valueUuid"] o  = do
        c <- o .: "valueUuid"
        return $ Just $ ExtensionValueUuid c
-- Complex
parseExtensionValue ["valueAddress"] o  = do
        c <- o .: "valueAddress"
        return $ Just $ ExtensionValueAddress c
parseExtensionValue ["valueAge"] o  = do
        c <- o .: "valueAge"
        return $ Just $ ExtensionValueAge c
parseExtensionValue ["valueAnnotation"] o  = do
        c <- o .: "valueAnnotation"
        return $ Just $ ExtensionValueAnnotation c
parseExtensionValue ["valueAttachment"] o  = do
        c <- o .: "valueAttachment"
        return $ Just $ ExtensionValueAttachment c
parseExtensionValue ["valueCodeableConcept"] o  = do
        c <- o .: "valueCodeableConcept"
        return $ Just $ ExtensionValueCodeableConcept c
parseExtensionValue ["valueCoding"] o  = do
        c <- o .: "valueCoding"
        return $ Just $ ExtensionValueCoding c
parseExtensionValue ["valueContactPoint"] o  = do
        c <- o .: "valueContactPoint"
        return $ Just $ ExtensionValueContactPoint c
parseExtensionValue ["valueCount"] o  = do
        c <- o .: "valueCount"
        return $ Just $ ExtensionValueCount c
parseExtensionValue ["valueDistance"] o  = do
        c <- o .: "valueDistance"
        return $ Just $ ExtensionValueDistance c
parseExtensionValue ["valueDuration"] o  = do
        c <- o .: "valueDuration"
        return $ Just $ ExtensionValueDuration c
parseExtensionValue ["valueHumanName"] o  = do
        c <- o .: "valueHumanName"
        return $ Just $ ExtensionValueHumanName c
parseExtensionValue ["valueIdentifier"] o  = do
        c <- o .: "valueIdentifier"
        return $ Just $ ExtensionValueIdentifier c
parseExtensionValue ["valueMoney"] o  = do
        c <- o .: "valueMoney"
        return $ Just $ ExtensionValueMoney c
parseExtensionValue ["valuePeriod"] o  = do
        c <- o .: "valuePeriod"
        return $ Just $ ExtensionValuePeriod c
parseExtensionValue ["valueQuantity"] o  = do
        c <- o .: "valueQuantity"
        return $ Just $ ExtensionValueQuantity c
parseExtensionValue ["valueRange"] o  = do
        c <- o .: "valueRange"
        return $ Just $ ExtensionValueRange c
parseExtensionValue ["valueRatio"] o  = do
        c <- o .: "valueRatio"
        return $ Just $ ExtensionValueRatio c
parseExtensionValue ["valueSampledData"] o  = do
        c <- o .: "valueSampledData"
        return $ Just $ ExtensionValueSampledData c
parseExtensionValue ["valueSignature"] o  = do
        c <- o .: "valueSignature"
        return $ Just $ ExtensionValueSignature c
parseExtensionValue ["valueTiming"] o  = do
        c <- o .: "valueTiming"
        return $ Just $ ExtensionValueTiming c
-- Metadata
parseExtensionValue ["valueContactDetail"] o  = do
        c <- o .: "valueContactDetail"
        return $ Just $ ExtensionValueContactDetail c
parseExtensionValue ["valueContributor"] o  = do
        c <- o .: "valueContributor"
        return $ Just $ ExtensionValueContributor c
parseExtensionValue ["valueDataRequirement"] o  = do
        c <- o .: "valueDataRequirement"
        return $ Just $ ExtensionValueDataRequirement c
parseExtensionValue ["valueExpression"] o  = do
        c <- o .: "valueExpression"
        return $ Just $ ExtensionValueExpression c
parseExtensionValue ["valueParameterDefinition"] o  = do
        c <- o .: "valueParameterDefinition"
        return $ Just $ ExtensionValueParameterDefinition c
parseExtensionValue ["valueRelatedArtifact"] o  = do
        c <- o .: "valueRelatedArtifact"
        return $ Just $ ExtensionValueRelatedArtifact c
parseExtensionValue ["valueTriggerDefinition"] o  = do
        c <- o .: "valueTriggerDefinition"
        return $ Just $ ExtensionValueTriggerDefinition c
parseExtensionValue ["valueUsageContext"] o  = do
        c <- o .: "valueUsageContext"
        return $ Just $ ExtensionValueUsageContext c
-- SpecialPurpose
parseExtensionValue ["valueReference"] o  = do
        c <- o .: "valueReference"
        return $ Just $ ExtensionValueReference c
parseExtensionValue ["valueDosage"] o  = do
        c <- o .: "valueDosage"
        return $ Just $ ExtensionValueDosage c
parseExtensionValue ["valueMeta"] o  = do
        c <- o .: "valueMeta"
        return $ Just $ ExtensionValueMeta c
parseExtensionValue _ o  = o .:? "empty" 


mkAddress = Address {
          addressId = Nothing
        , addressExtension= []
        , addressUse = Just AuHome
        , addressType = Just AtPostal
        , addressText = Nothing
        , addressLine = ["Kerpenerstr. 62"]
        , addressCity = Just "Köln"
        , addressDistrict = Nothing
        , addressState = Just "NRW"
        , addressPostalCode = Just "50729"
        , addressCountry = Just "DEU"
--    ISO 3166 3 letter code  
        , addressPeriod = Nothing
        }
mkBackboneElement i e m = BackboneElement{
          backboneElementAttribs=[]
        , backboneElementId = i
        , backboneElementExtension= e
        , backboneElementModifierExtension= m
        }
mkCodeableConcept c = CodeableConcept {
          codeableConceptAttribs= []
        , codeableConceptId= Nothing
        , codeableConceptExtension= []
        , codeableConceptCoding = [mkCoding c]
        , codeableConceptText = Just c
        }
mkCoding "lang" = Coding{
          codingAttribs = []
        , codingId = Nothing
        , codingExtension= []
        , codingSystem = Just "urn:ietf:bcp:47"
        , codingVersion = Just "v7"
        , codingCode = Just "nl-NL" 
        , codingDisplay = Just "Dutch" 
        , codingUserSelected = Nothing
        }
mkCoding "eclass" = Coding{
          codingAttribs = []
        , codingId = Nothing
        , codingExtension= []
        , codingSystem = Just "http://terminology.hl7.org/ValueSet/v3-ActEncounterCode"
        , codingVersion = Just "2014-03-26"
        , codingCode = Just "AMB" 
        , codingDisplay = Just "amblant"
        , codingUserSelected = Nothing
        }
mkCoding "pid" = Coding{
          codingAttribs = []
        , codingId = Nothing
        , codingExtension= []
        , codingSystem = Just "orbis-pid"
        , codingVersion = Nothing
        , codingCode = Just "ORBISPID" 
        , codingDisplay = Nothing
        , codingUserSelected = Nothing
        }
mkContactPoint = ContactPoint{
          contactPointAttribs = []
        , contactPointId = Nothing
        , contactPointExtension= []
        , contactPointSystem = Just CpsPhone
        , contactPointValue = Just "0221-478-42160"
        , contactPointUse = Just CpuWork
        , contactPointRank = Just 1
        , contactPointPeriod = Nothing
        }
mkHumanName = HumanName {
          humanNameAttribs= []
        , humanNameId= Nothing
        , humanNameExtension= []
        , humanNameUse = Just NuOfficial
        , humanNameText = Just "Dummy, Detlef, *2000-01-01"
        , humanNameFamily = Just "Dummy"
        , humanNameGiven = ["Detlef","Ralph"]
        , humanNamePrefix = ["Dr.med."]
        , humanNameSuffix = ["Jr."]
        , humanNamePeriod = Just $ mkPeriod
        }
mkIdentifier = Identifier{
          identifierAttribs= []
        , identifierId= Nothing
        , identifierExtension= []
        , identifierUse = Just IuUsual
        , identifierType = Just $ mkCodeableConcept "pid"
        , identifierSystem = Just "http://uk-koeln.de/ORBISPID"
        , identifierValue  = Just "6000001"
        , identifierPeriod  = Nothing
        , identifierAssigner = Nothing
        }
mkMeta = Meta{
          metaAttribs=[]
        , metaId= Nothing
        , metaExtension= []
        , metaVersionId=Just "0"
        , metaLastUpdated = Nothing
        , metaSource = Nothing
        , metaProfile = []
        , metaSecurity = []
        , metaTag = []
        }
mkNarrative= Narrative{
          narrativeAttribs = []
        , narrativeId = Nothing
        , narrativeExtension= []
        , narrativeStatus = NsGenerated
        , narrativeXhtmlDiv = mkXhtmlDiv "this is text"
        }
mkPeriod = Period{
          periodAttribs = []
        , periodId = Nothing
        , periodExtension= []
        , periodStart = Just "2021-04-01T09:00:00" 
        , periodEnd = Nothing
        }
mkReference "org" = Reference{
          referenceAttribs = HM.fromList [("id","123")]
        , referenceExtension= []
        , referenceReference  = Just "nabu/organizations/kikl"
        , referenceType       = Nothing
        , referenceIdentifier = Nothing
        , referenceDisplay    = Just "UKK Kinderklinik"
        }
mkReference "gp" = Reference{
          referenceAttribs = HM.fromList [("id","123")]
        , referenceExtension= []
        , referenceReference  = Just "nabu/practitioners/p-12345"
        , referenceType       = Nothing
        , referenceIdentifier = Nothing
        , referenceDisplay    = Just "Bönte, Anselm"
        }
mkReference "pmh" = Reference{
          referenceAttribs = HM.fromList [("id","123")]
        , referenceExtension= []
        , referenceReference  = Just "nabu/practitioners/u-pmh"
        , referenceType       = Nothing
        , referenceIdentifier = Nothing
        , referenceDisplay    = Just "Herkenrath, Peter"
        }
