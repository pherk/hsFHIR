{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

{-
 - FHIR Datatypes v4.0.1
- deviations
-}

module Data.FHIR.Datatypes.XML where

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key as AK
import Data.Aeson.KeyMap as AKM
#endif
import           Data.FHIR.Datatypes.Internal
import           Data.FHIR.Datatypes.XhtmlDiv
import           Data.FHIR.Datatypes.XmlUtils
import           RIO
import qualified RIO.HashMap as HM
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import           Xmlbf

--      where as = HM.fromList $ catMaybes $ fmap toAttr [ OptVal "id" (signatureId c) ]

instance ToXml Address where
--          OptVal "id"    (addressId c))
    toXml c = concatMap toElement $
        [
          PropList "extension"   (fmap toXml (addressExtension c))
        , OptVal "use"   (fmap toAddressUse (addressUse c))
        , OptVal "type"  (fmap toAddressType (addressType c))
        , OptVal "text"  (addressText c)
        , ValList "line" (addressLine c)
        , OptVal "city"  (addressCity c)
        , OptVal "district"   (addressDistrict c)
        , OptVal "state"      (addressState c)
        , OptVal "postalCode" (addressPostalCode c)
        , OptVal "country"    (addressCountry c)
        , OptProp "period"    (fmap toXml $ addressPeriod c)
        ]
instance FromXml Address where
    fromXml = do
        id <- optional $ (pAttr "id")
        ext<- many     $ pElement "extension" fromXml
        u  <- optional $ pElement "use" (pAttr "value")
        ty <- optional $ pElement "type" (pAttr "value")
        te <- optional $ pElement "text" (pAttr "value")
        l  <- many     $ pElement "line" (pAttr "value")
        ci <- optional $ pElement "city" (pAttr "value")
        s  <- optional $ pElement "state" (pAttr "value")
        d  <- optional $ pElement "district" (pAttr "value")
        po <- optional $ pElement "postalCode" (pAttr "value")
        co <- optional $ pElement "country" (pAttr "value")
        pe <- optional $ pElement "period" fromXml
        return $ Address{
                     addressId=id
                   , addressExtension=ext
                   , addressUse= fmap fromAddressUse u
                   , addressType= fmap fromAddressType ty
                   , addressText=te
                   , addressLine=l
                   , addressCity=ci
                   , addressState=s
                   , addressDistrict=d
                   , addressPostalCode=po
                   , addressCountry=co
                   , addressPeriod=pe
                   }


instance ToXml Age where
    toXml c = concatMap toElement $
        [
--          "id"  (fmap toXml (ageId c))
          PropList "extension"  (fmap toXml (ageExtension c))
        , OptVal   "value"  (fmap toDecimal (ageValue c))
        , OptVal   "comparator"  (fmap toQuantityComparator (ageComparator c))
        , OptVal   "unit"   (            (ageUnit c))
        , OptVal   "system" (           (ageSystem c))
        , OptVal   "code"   (           (ageCode c))
        ]
instance FromXml Age where
    fromXml = do
        i <- optional $ pAttr "id"
        ext<- many    $ pElement "extension"  fromXml
        v <- optional $ pElement "value" (pAttr "value")
        c <- optional $ pElement "comparator" (pAttr "value")
        u <- optional $ pElement "unit" (pAttr "value")
        s <- optional $ pElement "system" (pAttr "value")
        co <- optional $ pElement "code" (pAttr "value")
        return $ Age{
                     ageAttribs=[]
                   , ageId=i
                   , ageExtension=ext
                   , ageValue=fmap fromDecimal v
                   , ageComparator=fmap fromQuantityComparator c
                   , ageUnit=u
                   , ageSystem=s
                   , ageCode=co
                   }

instance ToXml Annotation where
    toXml c = concatMap toElement $
        [
--          "id"  (fmap toXml (annotationId c))
          PropList "extension"  (fmap toXml (annotationExtension c))
        , toAuthorXml c
        , OptVal   "time"  (fmap toDateTime (annotationTime c))
        , Val      "text"  (                (annotationText c))
        ]
        where toAuthorXml (Annotation{annotationAuthor= Just (AnnotationAuthorReference r)}) = OptProp "authorReference" (Just (toXml r))
              toAuthorXml (Annotation{annotationAuthor= Just (AnnotationAuthorString    t)}) = OptVal  "authorString" (Just (t))
              toAuthorXml (Annotation{annotationAuthor= Nothing                           }) = OptProp "author" Nothing
instance FromXml Annotation where
    fromXml = do
        i  <- optional $ pAttr "id"
        ext<- many     $ pElement "extension"  fromXml
        a  <- parseAuthorR <|> parseAuthorS <|> parseF
        ti <- optional $ pElement "time" (pAttr "value")
        te <-            pElement "text" (pAttr "value")
        return $ Annotation{
                     annotationAttribs=[]
                   , annotationId=i
                   , annotationExtension=ext
                   , annotationAuthor=a
                   , annotationTime=ti
                   , annotationText=te
                   }
        where parseAuthorR = do
                hasRef <- pElement "authorReference" fromXml
                return $ Just (AnnotationAuthorReference hasRef)
              parseAuthorS = do
                hasT <-   pElement "authorString" (pAttr "value")
                return $ Just (AnnotationAuthorString hasT)
              parseF = do pure Nothing


instance ToXml Attachment where
--          "id"  (fmap toXml (attachmentId c))
    toXml c = concatMap toElement $
        [
          PropList "extension" (fmap toXml (attachmentExtension c))
        , OptVal "contentType"                     (attachmentContentType c)
        , OptVal "language"                        (attachmentLanguage c) 
        , OptVal "data"                            (attachmentData c) 
        , OptVal "url"                             (attachmentUrl c) 
        , OptVal "size"        (fmap toUnsignedInt (attachmentSize c))
        , OptVal "hash"                            (attachmentHash c)
        , OptVal "title"                           (attachmentTitle c)
        , OptVal "creation"                        (attachmentCreation c)
        ]
instance FromXml Attachment where
    fromXml = do
        i <- optional $ pAttr "id"
        e  <- many     $ pElement "extension" fromXml
        ct <- optional $ pElement "contentType" (pAttr "value")
        l  <- optional $ pElement "language" (pAttr "value")
        d  <- optional $ pElement "data" (pAttr "value")
        u  <- optional $ pElement "url" (pAttr "value")
        s  <- optional $ pElement "size" (pAttr "value")
        h  <- optional $ pElement "hash" (pAttr "value")
        t  <- optional $ pElement "title" (pAttr "value")
        cr <- optional $ pElement "creation" (pAttr "value")
        return $ Attachment{
                     attachmentAttribs=[]
                   , attachmentId=i
                   , attachmentExtension=e
                   , attachmentContentType=ct
                   , attachmentLanguage=l
                   , attachmentData=d
                   , attachmentUrl=u
                   , attachmentSize= fmap fromUnsignedInt s
                   , attachmentHash=h
                   , attachmentTitle=t
                   , attachmentCreation=cr
                   }


instance ToXml BackboneElement where
    toXml b = concatMap toElement $
        [
--              AttrList backboneElementAttribs b
          OptVal   "id" (backboneElementId b)
        , PropList "extension"         (fmap toXml (backboneElementExtension b))
        , PropList "modifierExtension" (fmap toXml (backboneElementModifierExtension b))
        ]
instance FromXml BackboneElement where
    fromXml = do
        i  <- optional $ pElement "id" (pAttr "value")
        e  <- many     $ pElement "extension" fromXml
        m  <- many     $ pElement "modifierExtension" fromXml
        pure BackboneElement{
              backboneElementAttribs = []
            , backboneElementId = i
            , backboneElementExtension = e
            , backboneElementModifierExtension = m
            }
toBackboneElementXml b =
    [
--    AttrList    (backboneElementAttribs b)
      OptVal "id" (backboneElementId b)
    , PropList "extension"         (fmap toXml (backboneElementExtension b))
    , PropList "modifierExtension" (fmap toXml (backboneElementModifierExtension b))
    ]

instance ToXml CodeableConcept where
    toXml m = concatMap toElement $
        [
--          AttrList    (backboneElementAttribs m)
--          OptAttr  "id"      (codeableConceptId m)
          PropList "extension" (fmap toXml (codeableConceptExtension m))
        , PropList "coding"    (fmap toXml (codeableConceptCoding m))
        , OptVal   "text"      (codeableConceptText m)
        ]
instance FromXml CodeableConcept where
    fromXml = do
        i  <- optional $ pAttr "id"
        e  <- many     $ pElement "extension" fromXml
        c  <- many     $ pElement "coding" fromXml
        t  <- optional $ pElement "text"   (pAttr "value")
        pure CodeableConcept{
                  codeableConceptAttribs = []
                , codeableConceptId = i
                , codeableConceptExtension = e
                , codeableConceptCoding  = c
                , codeableConceptText    = t
                }


instance ToXml Coding where
    toXml m = concatMap toElement $
        [
--          OptAttr  "id"      (codingId m)
          PropList "extension" (fmap toXml (codingExtension m))
        , OptVal "system"  (codingSystem m)
        , OptVal "version" (codingVersion m)
        , OptVal "code"    (codingCode m) 
        , OptVal "display" (codingDisplay m)
        , OptVal "userSelected" (fmap toBoolean $ codingUserSelected m)
        ]
instance FromXml Coding where
    fromXml = do
       i  <- optional (pAttr "id") 
       e  <- many     $ pElement "extension" fromXml
       sy <- optional $ pElement "system" (pAttr "value")
       ve <- optional $ pElement "version" (pAttr "value")
       co <- optional $ pElement "code" (pAttr "value")
       di <- optional $ pElement "display" (pAttr "value")
       us <- optional $ pElement "userselected" (pAttr "value")
       return Coding{
             codingAttribs = []
           , codingId = i
           , codingExtension = e
           , codingSystem = sy
           , codingVersion = ve
           , codingCode = co
           , codingDisplay = di
           , codingUserSelected = fmap fromBoolean us
           }

instance ToXml ContactDetail where
    toXml c = concatMap toElement $
       [
         PropList "extension" (fmap toXml (contactDetailExtension c))
       , OptVal "name" (contactDetailName c)
       , PropList "telecom" (fmap Xmlbf.toXml (contactDetailTelecom c))
       ]
instance FromXml ContactDetail where
    fromXml = do
       i  <- optional (pAttr "id") 
       e  <- many     $ pElement "extension" fromXml
       n <- optional $ pElement "name" (pAttr "value")
       t <- many     $ pElement "telecom" fromXml
       return $ ContactDetail{
                    contactDetailAttribs=[]
                  , contactDetailId=i
                  , contactDetailExtension=e
                  , contactDetailName=n
                  , contactDetailTelecom=t}


instance ToXml ContactPoint where
    toXml m = concatMap toElement $
        [
          PropList "extension" (fmap toXml (contactPointExtension m))
        , OptVal   "system"    (fmap toContactPointSystem $ contactPointSystem m)
        , OptVal   "value"     (contactPointValue m)
        , OptVal   "use"       (fmap toContactPointUse $ contactPointUse m) 
        , OptVal   "rank"      (fmap toPositiveInt $ contactPointRank m) 
        , OptProp  "period"    (fmap toXml $ contactPointPeriod m)
        ]
instance FromXml ContactPoint where
    fromXml = do
       i  <- optional (pAttr "id") 
       e  <- many     $ pElement "extension" fromXml
       sy <- optional $ pElement "system" (pAttr "value") 
       va <- optional $ pElement "value"  (pAttr "value")
       us <- optional $ pElement "use"    (pAttr "value") 
       ra <- optional $ pElement "rank"   (pAttr "value") 
       pe <- optional $ pElement "period" fromXml
       return ContactPoint{
             contactPointAttribs = []
           , contactPointId     = i
           , contactPointExtension=e
           , contactPointSystem = fmap fromContactPointSystem sy
           , contactPointValue  = va
           , contactPointUse    = fmap fromContactPointUse us
           , contactPointRank   = fmap fromPositiveInt ra
           , contactPointPeriod = pe
           }

instance ToXml Count where
    toXml c = concatMap toElement $
        [
--          "id"  (fmap toXml (countId c))
          PropList "extension"  (fmap toXml (countExtension c))
        , OptVal "value"       (fmap toDecimal (countValue c))
        , OptVal "comparator"  (fmap toQuantityComparator (countComparator c))
        , OptVal "unit"        (           (countUnit c))
        , OptVal "system"      (           (countSystem c))
        , OptVal "code"        (           (countCode c))
        ]
instance FromXml Count where
    fromXml = do
        i  <- optional $ pAttr "id"
        ext<- many     $ pElement "extension"  fromXml
        v  <- optional $ pElement "value" (pAttr "value")
        c  <- optional $ pElement "comparator" (pAttr "value")
        u  <- optional $ pElement "unit" (pAttr "value")
        s  <- optional $ pElement "system" (pAttr "value")
        co <- optional $ pElement "code" (pAttr "value")
        return $ Count{
                     countAttribs=[]
                   , countId=i
                   , countExtension=ext
                   , countValue=fmap fromDecimal v
                   , countComparator=fmap fromQuantityComparator c
                   , countUnit=u
                   , countSystem=s
                   , countCode=co
                   }


instance ToXml Distance where
    toXml c = concatMap toElement $
        [
--          "id"  (fmap toXml (distanceId c))
          PropList "extension"  (fmap toXml (distanceExtension c))
        , OptVal   "value"      (fmap toDecimal (distanceValue c))
        , OptVal   "comparator" (fmap toQuantityComparator (distanceComparator c))
        , OptVal   "unit"       (           (distanceUnit c))
        , OptVal   "system"     (           (distanceSystem c))
        , OptVal   "code"       (           (distanceCode c))
        ]
instance FromXml Distance where
    fromXml = do
        i  <- optional $ pAttr "id"
        ext<- many     $ pElement "extension"  fromXml
        v  <- optional $ pElement "value" (pAttr "value")
        c  <- optional $ pElement "comparator" (pAttr "value")
        u  <- optional $ pElement "unit" (pAttr "value")
        s  <- optional $ pElement "system" (pAttr "value")
        co <- optional $ pElement "code" (pAttr "value")
        return $ Distance{
                     distanceAttribs=[]
                   , distanceId=i
                   , distanceExtension=ext
                   , distanceValue=fmap fromDecimal v
                   , distanceComparator=fmap fromQuantityComparator c
                   , distanceUnit=u
                   , distanceSystem=s
                   , distanceCode=co
                   }


toDosageAsNeededXml  (DosageAsNeededBoolean c)         = Val "dosageAsNeededBoolean" (toBoolean c)
toDosageAsNeededXml  (DosageAsNeededCodeableConcept c) = Prop "dosageAsNeededCodeableConcept" (HM.empty, toXml c) 
toDosageDoseAndRateDoseXml (DosageDoseAndRateDoseRange    r) = Prop "doseAndRateRange" (HM.empty, toXml r)
toDosageDoseAndRateDoseXml (DosageDoseAndRateDoseQuantity q) = Prop "doseAndRateQuantity"(HM.empty, toXml q)
toDosageDoseAndRateRateXml (DosageDoseAndRateRateRatio    r) = Prop "doseAndRateRatio" (HM.empty, toXml r)
toDosageDoseAndRateRateXml (DosageDoseAndRateRateRange    r) = Prop "doseAndRateRange" (HM.empty, toXml r)
toDosageDoseAndRateRateXml (DosageDoseAndRateRateQuantity q) = Prop "doseAndRateQuantity" (HM.empty, toXml q)

instance ToXml DosageDoseAndRate where
    toXml c = concatMap toElement $
        [
--           "id"  (fmap toXml (dosageDoseAndRateId c))
          PropList "extension"  (fmap toXml (dosageDoseAndRateExtension c))
        , OptProp  "type"  (fmap toXml (dosageDoseAndRateType c))
        ,  toDosageDoseAndRateDoseXml (dosageDoseAndRateDose c)
        ,  toDosageDoseAndRateRateXml (dosageDoseAndRateRate c)
        ]
instance FromXml DosageDoseAndRate where
    fromXml = do
        id  <- optional $ pAttr "id"
        ext <- many     $ pElement "extension" fromXml
        t <- optional $ pElement "type" fromXml
        d <- parseDoseRg <|> parseDoseQu
        r <- parseRateRg <|> parseRateRa <|> parseRateQu
        return $ DosageDoseAndRate{
                    dosageDoseAndRateAttribs=[]
                  , dosageDoseAndRateId=id
                  , dosageDoseAndRateExtension=ext
                  , dosageDoseAndRateType=t
                  , dosageDoseAndRateDose=d
                  , dosageDoseAndRateRate=r
                  }
        where parseDoseRg = do
                hasR <- pElement "doseRange" fromXml
                return $ (DosageDoseAndRateDoseRange hasR)
              parseDoseQu = do
                hasQ <- pElement "doseQuantity" fromXml
                return $ (DosageDoseAndRateDoseQuantity hasQ)
              parseRateRa = do
                hasR <- pElement "rateRatio"  fromXml
                return $ (DosageDoseAndRateRateQuantity hasR)
              parseRateRg = do
                hasR <- pElement "rateRange"  fromXml
                return $ (DosageDoseAndRateRateQuantity hasR)
              parseRateQu = do
                hasQ <- pElement "rateQuantity" fromXml
                return $ (DosageDoseAndRateRateQuantity hasQ)

instance ToXml Dosage where
    toXml c = concatMap toElement $
        toBackboneElementXml (dosageSuper c)
        ++
        [
          OptVal   "sequence"  (fmap toInt (dosageSequence c))
        , OptVal   "text"      (           (dosageText c))
        , PropList "additionalInstruction"  (fmap toXml (dosageAdditionalInstruction c))
        , OptVal   "patientInstruction"  (           (dosagePatientInstruction c))
        , OptProp "timing"     (fmap toXml (dosageTiming c))
        , toDosageAsNeededXml  (dosageAsNeeded c)
        , OptProp  "site"      (fmap toXml (dosageSite c))
        , OptProp  "route"     (fmap toXml (dosageRoute c))
        , OptProp  "method"    (fmap toXml (dosageMethod c))
        , PropList "doseAndRate"         (fmap toXml (dosageDoseAndRate c))
        , OptProp  "maxDosePerPeriod"    (fmap toXml (dosageMaxDosePerPeriod c))
        , OptProp  "maxDosePerAdministration"  (fmap toXml (dosageMaxDosePerAdministration c))
        , OptProp  "maxDosePerLifetime"  (fmap toXml (dosageMaxDosePerLifetime c))
        ]

instance FromXml Dosage where
    fromXml = do
        id  <- optional $ pElement "id" (pAttr "value")
        ext <- many     $ pElement "extension" fromXml
        mxt <- many     $ pElement "modifierExtension" fromXml
        se <- optional $ pElement "sequence" (pAttr "value")
        te <- optional $ pElement "text"     (pAttr "value")
        ad <- many     $ pElement "additionalInstruction" fromXml
        pa <- optional $ pElement "patientInstruction" (pAttr "value")
        ti <- optional $ pElement "timing" fromXml
        as <- parseAsNeededB <|> parseAsNeededCC
        si <- optional $ pElement "site" fromXml
        ro <- optional $ pElement "route" fromXml
        me <- optional $ pElement "method" fromXml
        dr <- many     $ pElement "doseAndRate" fromXml
        dp <- optional $ pElement "maxDosePerPeriod" fromXml
        da <- optional $ pElement "maxDosePerAdministration" fromXml
        dl <- optional $ pElement "maxDosePerLifetime" fromXml
        return $ Dosage{
                    dosageSuper = mkBackboneElement id ext mxt
                  , dosageSequence= fmap fromInt se
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
        where parseAsNeededB = do
                hasB <- pElement "asNeededBoolean" (pAttr "value")
                return $ DosageAsNeededBoolean (fromBoolean hasB)
              parseAsNeededCC = do
                hasCC <- pElement "asNeededCodeableConcept" fromXml
                return $ DosageAsNeededCodeableConcept hasCC

instance ToXml Duration where
    toXml m = concatMap toElement $
        [
          PropList "extension" (fmap toXml (durationExtension m))
        , OptVal   "value" (fmap toDecimal $ durationValue m)
        , OptVal   "comparator"  (fmap toQuantityComparator $ durationComparator m)
        , OptVal   "unit"  (durationUnit m)
        , OptVal   "uri"   (durationSystem m)
        , OptVal   "code"  (durationCode m)
        ]
instance FromXml Duration where
    fromXml = do
       i  <- optional (pAttr "id") 
       e  <- many     $ pElement "extension" fromXml
       va <- optional $ pElement "value"      (pAttr "value") 
       cp <- optional $ pElement "comparator" (pAttr "value") 
       un <- optional $ pElement "unit"       (pAttr "value")
       sy <- optional $ pElement "system"     (pAttr "value")
       co <- optional $ pElement "code"       (pAttr "value")
       return Duration{
             durationAttribs = []
           , durationId     = i
           , durationExtension  = e
           , durationValue      = fmap fromDecimal va
           , durationComparator = fmap fromQuantityComparator cp
           , durationUnit       = un
           , durationSystem     = sy
           , durationCode       = co
           }


instance ToXml HumanName where
    toXml m = concatMap toElement $
        [
          PropList "extension" (fmap toXml (humanNameExtension m))
        , OptVal   "use"       (fmap toNameUse $ humanNameUse m)
        , OptVal   "text"      (humanNameText m)
        , OptVal   "family"    (humanNameFamily m) 
        , ValList  "given"     (humanNameGiven m)
        , ValList  "prefix"    (humanNamePrefix m)
        , ValList  "suffix"    (humanNameSuffix m)
        , OptProp  "period"    (fmap toXml $ humanNamePeriod m)
        ]
instance FromXml HumanName where
    fromXml = do
       i  <- optional (pAttr "id") 
       e  <- many     $ pElement "extension" fromXml
       us <- optional $ pElement "use"    (pAttr "value") 
       te <- optional $ pElement "text"   (pAttr "value") 
       fa <- optional $ pElement "family" (pAttr "value")
       gi <- many     $ pElement "given"  (pAttr "value")
       pr <- many     $ pElement "prefix" (pAttr "value")
       su <- many     $ pElement "suffix" (pAttr "value")
       pe <- optional $ pElement "period" fromXml
       return HumanName{
             humanNameAttribs = []
           , humanNameId     = i
           , humanNameExtension=e
           , humanNameUse    = fmap fromNameUse us
           , humanNameText   = te
           , humanNameFamily = fa
           , humanNameGiven  = gi
           , humanNamePrefix = pr
           , humanNameSuffix = su
           , humanNamePeriod = pe
           }


instance ToXml Identifier where
    toXml m = concatMap toElement $
        [
          PropList "extension" (fmap toXml (identifierExtension m))
        , OptVal "use"       (fmap toIdentifier $ identifierUse m)
        , OptProp "type"     (fmap toXml $ identifierType m)
        , OptVal "system"    (identifierSystem m) 
        , OptVal "value"     (identifierValue m)
        , OptProp "period"   (fmap toXml $ identifierPeriod m)
        , OptProp "assigner" (fmap toXml $ identifierAssigner m)
        ]
instance FromXml Identifier where
    fromXml = do
       i  <- optional (pAttr "id") 
       e  <- many     $ pElement "extension" fromXml
       us <- optional $ pElement "use" (pAttr "value")
       ty <- optional $ pElement "type" fromXml
       sy <- optional $ pElement "system" (pAttr "value")
       va <- optional $ pElement "value" (pAttr "value")
       pe <- optional $ pElement "period" fromXml
       as <- optional $ pElement "assigner" fromXml
       return Identifier{
             identifierAttribs = []
           , identifierId     = i
           , identifierExtension=e
           , identifierUse    = fmap fromIdentifierUse us
           , identifierType   = ty
           , identifierSystem = sy
           , identifierValue  = va
           , identifierPeriod = pe
           , identifierAssigner = as
           }



instance ToXml Meta where
    toXml m = concatMap toElement $
        [
          PropList "extension"   (fmap toXml (metaExtension m))
        , OptVal   "versionId"   (metaVersionId m)
        , OptVal   "lastUpdated" (metaLastUpdated m)
        , OptVal   "source"      (metaSource m)
        , ValList  "profile"     (metaProfile m)
        , PropList "security" (fmap toXml (metaSecurity m))
        , PropList "tag"      (fmap toXml (metaTag m))
        ]
instance FromXml Meta where
    fromXml = do
       i  <- optional (pAttr "id") 
       e  <- many     $ pElement "extension" fromXml
       vi <- optional $ pElement "versionId" (pAttr "value")
       l  <- optional $ pElement "lastUpdated" (pAttr "value")
       so <- optional $ pElement "source" (pAttr "value")
       p  <- many     $ pElement "profile" (pAttr "value")
       se <- many     $ pElement "security" $ fromXml
       t  <- many     $ pElement "tag" $ fromXml
       return Meta{
             metaAttribs = []
           , metaId = i
           , metaExtension=e
           , metaVersionId = vi
           , metaLastUpdated = l
           , metaSource = so
           , metaProfile = p
           , metaSecurity = se
           , metaTag = t
           }

instance ToXml Money where
    toXml c = concatMap toElement $
        [
--           "id" (fmap toXml (moneyId c))
          PropList "extension" (fmap toXml (moneyExtension c))
        , OptVal   "value"     (fmap toDecimal (moneyValue c))
        , OptVal   "currency"  (           (moneyCurrency c))
        ]
instance FromXml Money where
    fromXml = do
        id  <- optional $ pAttr "id"
        ext <- many     $ pElement "extension"  fromXml
        v   <- optional $ pElement "value" (pAttr "value")
        c   <- optional $ pElement "currency" (pAttr "value")
        return $ Money{
                    moneyAttribs=[]
                  , moneyId=id
                  , moneyExtension=ext
                  , moneyValue=fmap fromDecimal v
                  , moneyCurrency=c}

instance ToXml Narrative where
    toXml n = concatMap toElement $
        [
          PropList "extension"   (fmap toXml (narrativeExtension n))
        , Val  "status"          (toNarrativeStatus (narrativeStatus n))
        , Prop "div"             (toXhtmlDivXml (narrativeXhtmlDiv n))
        ]
instance FromXml Narrative where
    fromXml = do
       id <- optional $ pAttr "id"
       e  <- many     $ pElement "extension" fromXml
       s  <-            pElement "status" (pAttr "value")
       d  <-            pElement "div" fromXml
       return Narrative{
                narrativeAttribs=[]
              , narrativeId=id
              , narrativeExtension=e
              , narrativeStatus= fromNarrativeStatus s
              , narrativeXhtmlDiv=d}

instance ToXml Period where
    toXml m = concatMap toElement $
        [
          PropList "extension"   (fmap toXml (periodExtension m))
        , OptVal "start"  (periodStart m)
        , OptVal "end"    (periodEnd m)
        ]
instance FromXml Period where
    fromXml = do
        i  <- optional (pAttr "id") 
        e  <- many     $ pElement "extension" fromXml
        st <- optional $ pElement "start" (pAttr "value")
        en <- optional $ pElement "end" (pAttr "value")
        return Period{
             periodAttribs = []
           , periodId = i
           , periodExtension=e
           , periodStart = st
           , periodEnd = en
           }


instance ToXml ProductShelfLife where
    toXml c = concatMap toElement $
        [
          PropList "extension"   (fmap toXml (productShelfLifeExtension c))
        , OptProp  "identifier"  (fmap toXml (productShelfLifeIdentifier c))
        , Prop     "type"        (HM.empty, toXml (productShelfLifeType c))
        , Prop     "period"      (HM.empty, toXml (productShelfLifePeriod c))
        , PropList "specialPrecautionsForStorage" (fmap toXml (productShelfLifeSpecialPrecautionsForStorage c))
        ]
instance FromXml ProductShelfLife where
    fromXml = do
        id  <- optional $ pAttr "id"
        ext <- many     $ pElement "extension"  fromXml
        i <-   optional $ pElement "identifier" fromXml
        t <-              pElement "type"       fromXml
        p <-              pElement "period"     fromXml
        s <-   many     $ pElement "specialPrecautionsForStorage" fromXml
        return $ ProductShelfLife{
                    productShelfLifeAttribs=[]
                  , productShelfLifeId=id
                  , productShelfLifeExtension=ext
                  , productShelfLifeIdentifier=i
                  , productShelfLifeType=t
                  , productShelfLifePeriod=p
                  , productShelfLifeSpecialPrecautionsForStorage=s}


instance ToXml ProdCharacteristic where
    toXml c = concatMap toElement $
        [
          OptVal   "id" (prodCharacteristicId c)
        , PropList "extension"  (fmap toXml (prodCharacteristicExtension c))
        , OptProp  "height"   (fmap toXml (prodCharacteristicHeight c))
        , OptProp  "width"    (fmap toXml (prodCharacteristicWidth c))
        , OptProp  "depth"    (fmap toXml (prodCharacteristicDepth c))
        , OptProp  "weight"   (fmap toXml (prodCharacteristicWeight c))
        , OptProp  "nominalVolume"      (fmap toXml (prodCharacteristicNominalVolume c))
        , OptProp  "externalDiameter"   (fmap toXml (prodCharacteristicExternalDiameter c))
        , OptVal   "shape"     (           (prodCharacteristicShape c))
        , ValList  "color"     (           (prodCharacteristicColor c))
        , ValList  "imprint"   (           (prodCharacteristicImprint c))
        , PropList "image"     (fmap toXml (prodCharacteristicImage c))
        , OptProp  "scoring"   (fmap toXml (prodCharacteristicScoring c))
        ]
instance FromXml ProdCharacteristic where
    fromXml = do
        id  <- optional $ pAttr "id"
        ext <- many     $ pElement "extension" fromXml
        h  <- optional $ pElement "height" fromXml
        w  <- optional $ pElement "width"  fromXml
        d  <- optional $ pElement "depth"  fromXml
        we <- optional $ pElement "weight" fromXml
        n  <- optional $ pElement "nominalVolume" fromXml
        e  <- optional $ pElement "externalDiameter" fromXml
        sh <- optional $ pElement "shape" (pAttr "value")
        co <- many     $ pElement "color"   (pAttr "value")
        ip <- many     $ pElement "imprint" (pAttr "value")
        im <- many     $ pElement "image"   fromXml
        sc <- optional $ pElement "scoring" fromXml
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


instance ToXml Quantity where
    toXml c = concatMap toElement $
        [
          PropList "extension"   (fmap toXml (quantityExtension c))
        , OptVal "value"  (fmap toDecimal (quantityValue c))
        , OptVal "comparator" (fmap toQuantityComparator (quantityComparator c))
        , OptVal "unit"   (quantityUnit c)
        , OptVal "system" (quantitySystem c)
        , OptVal "code"   (quantityCode c)
        ]
instance FromXml Quantity where
    fromXml = do
        i <- optional $ pElement  "id" (pAttr "value")
        e <- many     $ pElement "extension" fromXml
        v <- optional $ pElement  "value" (pAttr "value")
        c <- optional $ pElement  "comparator" (pAttr "value")
        u <- optional $ pElement  "unit" (pAttr "value")
        s <- optional $ pElement  "system" (pAttr "value")
        co <- optional $ pElement "code" (pAttr "value")
        return $ Quantity{
                     quantityAttribs=[]
                   , quantityId=i
                   , quantityExtension=e
                   , quantityValue= fmap fromDecimal v
                   , quantityComparator= fmap fromQuantityComparator c
                   , quantityUnit=u
                   , quantitySystem=s
                   , quantityCode=co
                   }

instance ToXml Range where
    toXml c = concatMap toElement $
        [
          PropList "extension"   (fmap toXml (rangeExtension c))
        , OptProp "low"  (fmap Xmlbf.toXml (rangeLow c))
        , OptProp "high" (fmap Xmlbf.toXml (rangeHigh c))
        ]
instance FromXml Range where
    fromXml = do
        i  <- optional (pAttr "id") 
        e  <- many     $ pElement "extension" fromXml
        l <- optional $ pElement "low" fromXml
        h <- optional $ pElement "high" fromXml
        return $ Range{
              rangeAttribs=[]
            , rangeId=i
            , rangeExtension=e
            , rangeLow=l
            , rangeHigh=h}

instance ToXml Ratio where
    toXml c = concatMap toElement $
        [
          PropList "extension"   (fmap toXml (ratioExtension c))
        , OptProp "numerator"  (fmap Xmlbf.toXml (ratioNumerator c))
        , OptProp "denominator" (fmap Xmlbf.toXml (ratioDenominator c))
        ]
instance FromXml Ratio where
    fromXml = do
        i  <- optional (pAttr "id") 
        e  <- many     $ pElement "extension" fromXml
        l <- optional $ pElement "numerator" fromXml
        h <- optional $ pElement "denominator" fromXml
        return $ Ratio{
              ratioAttribs=[]
            , ratioId=i
            , ratioExtension=e
            , ratioNumerator=l
            , ratioDenominator=h}


instance ToXml Reference where
    toXml m = concatMap toElement $
        [
          PropList "extension"   (fmap toXml (referenceExtension m))
        , OptVal  "reference"  (referenceReference m)
        , OptVal  "type"       (referenceType m)
        , OptProp "identifier" (fmap toXml (referenceIdentifier m))
        , OptVal  "display"    (referenceDisplay m)
        ]
instance FromXml Reference where
    fromXml = do
       as <- optional $ pAttrs 
       e  <- many     $ pElement "extension"  fromXml
       re <- optional $ pElement "reference"  (pAttr "value")
       ty <- optional $ pElement "type"       (pAttr "value")
       id <- optional $ pElement "identifier" fromXml
       di <- optional $ pElement "display"    (pAttr "value")
       return Reference{
             referenceAttribs = fromMaybe HM.empty as
           , referenceExtension=e
           , referenceReference = re
           , referenceType = ty
           , referenceIdentifier = id
           , referenceDisplay = di
           }

instance ToXml SampledData where
    toXml c = concatMap toElement $
        [
--           "id" (fmap toXml (sampledDataId c))
          PropList "extension"  (fmap toXml (sampledDataExtension c))
        , Prop     "origin"     (HM.empty, toXml (sampledDataOrigin c))
        , Val      "period"     (     toDecimal  (sampledDataPeriod c))
        , OptVal   "factor"     (fmap toDecimal  (sampledDataFactor c))
        , OptVal   "lowerLimit" (fmap toDecimal  (sampledDataLowerLimit c))
        , OptVal   "upperLimit" (fmap toDecimal  (sampledDataUpperLimit c))
        , Val      "dimensions" (     toPositiveInt (sampledDataDimensions c))
        , OptVal   "data"       (                   (sampledDataData c))
        ]
instance FromXml SampledData where
    fromXml = do
        id  <- optional $ pAttr "id"
        ext <- many     $ pElement "extension"  fromXml
        or  <-            pElement "origin" fromXml
        p   <-            pElement "period" (pAttr "value")
        f   <- optional $ pElement "factor" (pAttr "value")
        l   <- optional $ pElement "lowerLimit" (pAttr "value")
        u   <- optional $ pElement "upperLimit" (pAttr "value")
        di  <-            pElement "dimensions" (pAttr "value")
        da  <- optional $ pElement "data" (pAttr "value")
        return $ SampledData{
                    sampledDataAttribs=[]
                  , sampledDataId=id
                  , sampledDataExtension=ext
                  , sampledDataOrigin=or
                  , sampledDataPeriod=fromDecimal p
                  , sampledDataFactor=fmap fromDecimal f
                  , sampledDataLowerLimit=fmap fromDecimal l
                  , sampledDataUpperLimit=fmap fromDecimal u
                  , sampledDataDimensions=fromPositiveInt di
                  , sampledDataData=da}


toSearchParamTypeXml (SptComposite)= "composite"
toSearchParamTypeXml (SptDate)     = "date"
toSearchParamTypeXml (SptNumber)   = "number"
toSearchParamTypeXml (SptQuantity) = "quantity"
toSearchParamTypeXml (SptReference)= "reference"
toSearchParamTypeXml (SptSpecial)  = "special"
toSearchParamTypeXml (SptString)   = "string"
toSearchParamTypeXml (SptToken)    = "token"
toSearchParamTypeXml (SptUri)      = "uri"

instance ToXml Signature where
    toXml c = concatMap toElement $
        [
          PropList "extension"    (fmap toXml (signatureExtension c))
        , PropList "type"         (fmap toXml $ signatureType c)
        , Val      "when"                      (signatureWhen c)
        , Prop     "who"          (HM.empty,   toXml $ signatureWho c)
        , OptProp  "onBehalfOf"   (fmap toXml $ signatureOnBehalfOf c)
        , OptVal   "targetFormat" (signatureTargetFormat c)
        , OptVal   "sigFormat"    (signatureSigFormat c)
        , OptVal   "data"         (signatureData c)
        ]
instance FromXml Signature where
    fromXml = do
        i  <- optional (pAttr "id") 
        e  <- many     $ pElement "extension" fromXml
        t  <- many     $ pElement "type" fromXml
        we <-            pElement "when" (pAttr "value")
        wo <-            pElement "who"   fromXml
        ob <- optional $ pElement "onBehalfOf" fromXml
        tf <- optional $ pElement "targetFormat" (pAttr "value")
        s  <- optional $ pElement "sigFormat" (pAttr "value")
        d  <- optional $ pElement  "data" (pAttr "value")
        return $ Signature{
                    signatureAttribs=[]
                  , signatureId=i
                  , signatureExtension=e
                  , signatureType=t
                  , signatureWhen=we
                  , signatureWho=wo
                  , signatureOnBehalfOf=ob
                  , signatureTargetFormat=tf
                  , signatureSigFormat=d
                  , signatureData=d
                  }

toSortDirectionXml (SdAscending)  = "ascending"
toSortDirectionXml (SdDescending) = "descending"
fromSortDirectionXml "ascending"  = SdAscending
fromSortDirectionXml "descending" = SdDescending
toTimingRepeatBoundsXml (TimingRepeatBoundsDuration b) = Prop "boundDuration" (HM.empty, toXml b)
toTimingRepeatBoundsXml (TimingRepeatBoundsRange    b) = Prop "boundRange" (HM.empty,  toXml b)
toTimingRepeatBoundsXml (TimingRepeatBoundsPeriod   b) = Prop "boundsPeriod" (HM.empty, toXml b)
toEventTimingXml (EtAC)       =  "AC"
toEventTimingXml (EtACD)      =  "ACD"
toEventTimingXml (EtACM)      =  "ACM"
toEventTimingXml (EtACV)      =  "ACV"
toEventTimingXml (EtAFT)      =  "AFT"
toEventTimingXml (EtAFTEarly) =  "AFTEarly"
toEventTimingXml (EtAFTLate)  =  "AFTLate"
toEventTimingXml (EtC)       =  "C"
toEventTimingXml (EtCD)      =  "CD"
toEventTimingXml (EtCM)      =  "CM"
toEventTimingXml (EtCV)      =  "CV"
toEventTimingXml (EtEVE)      =  "EVE"
toEventTimingXml (EtEVEEarly) =  "EVEEarly"
toEventTimingXml (EtEVELate)  =  "EVELate"
toEventTimingXml (EtHS)      =  "HS"
toEventTimingXml (EtMORN)    =  "MORN"
toEventTimingXml (EtMORNEarly) =  "MORNEarly"
toEventTimingXml (EtMORNLate)  =  "MORNLate"
toEventTimingXml (EtNIGHT)     =  "NIGHT"
toEventTimingXml (EtNOON)     =  "NOON"
toEventTimingXml (EtPC)       =  "PC"
toEventTimingXml (EtPCD)      =  "PCD"
toEventTimingXml (EtPCM)      =  "PCM"
toEventTimingXml (EtPCV)      =  "PCV"
toEventTimingXml (EtPHS)      =  "PHS"
toEventTimingXml (EtWAKE)     =  "WAKE"
fromEventTimingXml "AC"    = EtAC
fromEventTimingXml "ACD"    = EtACD
fromEventTimingXml "ACM"    = EtACM
fromEventTimingXml "ACV"    = EtACV
fromEventTimingXml "AFT"    = EtAFT
fromEventTimingXml "AFTEarly"    = EtAFTEarly
fromEventTimingXml "AFTLate"    = EtAFTLate
fromEventTimingXml "C"    = EtC
fromEventTimingXml "CD"    = EtCD
fromEventTimingXml "CM"    = EtCM
fromEventTimingXml "CV"    = EtCV
fromEventTimingXml "EVE"    = EtEVE
fromEventTimingXml "EVEEarly"    = EtEVEEarly
fromEventTimingXml "EVELate"    = EtEVELate
fromEventTimingXml "HS"    = EtHS
fromEventTimingXml "MORN"    = EtMORN
fromEventTimingXml "MORNEarly"    = EtMORNEarly
fromEventTimingXml "MORNLate"    = EtMORNLate
fromEventTimingXml "NIGHT"    = EtNIGHT
fromEventTimingXml "NOON"    = EtNOON
fromEventTimingXml "PC"    = EtPC
fromEventTimingXml "PCD"    = EtPCD
fromEventTimingXml "PCM"    = EtPCM
fromEventTimingXml "PCV"    = EtPCV
fromEventTimingXml "PHS"    = EtPHS
fromEventTimingXml "WAKE"    = EtWAKE
toUnitsOfTimeXml (UotA) =  "a"
toUnitsOfTimeXml (UotD) =  "d"
toUnitsOfTimeXml (UotH) =  "h"
toUnitsOfTimeXml (UotMin) =  "min"
toUnitsOfTimeXml (UotMo)  =  "mo"
toUnitsOfTimeXml (UotS)   =  "s"
toUnitsOfTimeXml (UotWk)  =  "wk"
fromUnitsOfTimeXml "a" = UotA
fromUnitsOfTimeXml "d" = UotD
fromUnitsOfTimeXml "h" = UotH
fromUnitsOfTimeXml "min" = UotMin
fromUnitsOfTimeXml "mo"  = UotMo
fromUnitsOfTimeXml "s"   = UotS
fromUnitsOfTimeXml "wk"  = UotWk


instance ToXml TimingRepeat where
    toXml c = concatMap toElement $
        [
--           "id"  (fmap toXml (timingRepeatId c))
          PropList "extension"  (fmap toXml (timingRepeatExtension c))
        , toTimingRepeatBoundsXml (timingRepeatBounds c)
        , OptVal "count"         (fmap toPositiveInt (timingRepeatCount c))
        , OptVal "countMax"      (fmap toPositiveInt (timingRepeatCountMax c))
        , OptVal "duration"      (fmap toDecimal (timingRepeatDuration c))
        , OptVal "durationMax"   (fmap toDecimal (timingRepeatDurationMax c))
        , OptVal "durationUnit"  (fmap toUnitsOfTimeXml   (timingRepeatDurationUnit c))
        , OptVal "frequency"     (fmap toPositiveInt (timingRepeatFrequency c))
        , OptVal "frequencyMax"  (fmap toPositiveInt (timingRepeatFrequencyMax c))
        , OptVal "period"        (fmap toDecimal (timingRepeatPeriod c))
        , OptVal "periodMax"     (fmap toDecimal (timingRepeatPeriodMax c))
        , OptVal  "periodUnit"   (fmap toUnitsOfTimeXml (timingRepeatPeriodUnit c))
        , ValList "dayOfWeek"    (           (timingRepeatDayOfWeek c))
        , ValList "timeOfDay"    (           (timingRepeatTimeOfDay c))
        , ValList "when"        (fmap toEventTimingXml (timingRepeatWhen c))
        , OptVal "offset"        (fmap toUnsignedInt (timingRepeatOffset c))
        ]
instance FromXml TimingRepeat where
    fromXml = do
        id  <- optional $ pAttr "id"
        ext <- many     $ pElement "extension" fromXml
        bo <- parseBoundsDuration <|> parseBoundsRange <|> parseBoundsPeriod
        co <- optional $ pElement "count"    (pAttr "value")
        cm <- optional $ pElement "countMax" (pAttr "value")
        d  <- optional $ pElement "duration" (pAttr "value")
        dm <- optional $ pElement "durationMax"  (pAttr "value")
        du <- optional $ pElement "durationUnit" (pAttr "value")
        f  <- optional $ pElement "frequency"    (pAttr "value")
        fm <- optional $ pElement "frequencyMax" (pAttr "value")
        p  <- optional $ pElement "period"       (pAttr "value")
        pm <- optional $ pElement "periodMax"    (pAttr "value")
        pu <- optional $ pElement "periodUnit"   (pAttr "value")
        dow <- many    $ pElement "dayOfWeek" (pAttr "value")
        tod <- many    $ pElement "timeOfDay" (pAttr "value")
        w  <- many     $ pElement "when"      (pAttr "value")
        o  <- optional $ pElement "offset" (pAttr "value")
        return $ TimingRepeat{
                    timingRepeatAttribs=[]
                  , timingRepeatId=id
                  , timingRepeatExtension=ext
                  , timingRepeatBounds=bo
                  , timingRepeatCount= fmap fromPositiveInt co
                  , timingRepeatCountMax=fmap fromPositiveInt cm
                  , timingRepeatDuration=fmap fromDecimal d
                  , timingRepeatDurationMax=fmap fromDecimal dm
                  , timingRepeatDurationUnit=fmap fromUnitsOfTimeXml du
                  , timingRepeatFrequency=fmap fromPositiveInt f
                  , timingRepeatFrequencyMax=fmap fromPositiveInt fm
                  , timingRepeatPeriod=fmap fromDecimal p
                  , timingRepeatPeriodMax=fmap fromDecimal pm
                  , timingRepeatPeriodUnit=fmap fromUnitsOfTimeXml pu
                  , timingRepeatDayOfWeek=dow
                  , timingRepeatTimeOfDay=tod
                  , timingRepeatWhen=fmap fromEventTimingXml w
                  , timingRepeatOffset=fmap fromUnsignedInt o}
        where parseBoundsDuration = do
                hasD <- pElement "boundsDuration" fromXml
                return $ (TimingRepeatBoundsDuration hasD)
              parseBoundsRange = do
                hasR <- pElement "boundsRange" fromXml
                return $ (TimingRepeatBoundsRange hasR)
              parseBoundsPeriod = do
                hasP <- pElement "boundsPeriod" fromXml
                return $ (TimingRepeatBoundsPeriod hasP)


instance ToXml Timing where
    toXml c = concatMap toElement $
        toBackboneElementXml (timingSuper c)
        ++
        [
          ValList "event"   (           (timingEvent c))
        , OptProp "repeat"  (fmap toXml (timingRepeat c))
        , OptProp "code"    (fmap toXml (timingCode c))
        ]
instance FromXml Timing where
    fromXml = do
        id  <- optional $ pElement "id" (pAttr "value")
        ext <- many     $ pElement "extension" fromXml
        mxt <- many     $ pElement "modifierExtension" fromXml
        e   <- many     $ pElement "event" (pAttr "value")
        r   <- optional $ pElement "repeat" fromXml
        c   <- optional $ pElement "code"   fromXml
        return $ Timing{
                    timingSuper = mkBackboneElement id ext mxt
                  , timingEvent=e
                  , timingRepeat=r
                  , timingCode=c}

toContributorTypeXml CtAuthor = "author"
toContributorTypeXml CtEditor = "editor"
toContributorTypeXml CtEndorser = "endorser"
toContributorTypeXml CtReviewer = "reviewer"
fromContributorTypeXml   "author" = CtAuthor
fromContributorTypeXml   "editor" = CtEditor
fromContributorTypeXml   "endorser" = CtEndorser
fromContributorTypeXml   "reviewer" = CtReviewer


instance ToXml Contributor where
    toXml c = concatMap toElement $
        [
--           "id" (fmap toXml (contributorId c))
          PropList "extension" (fmap toXml (contributorExtension c))
        , Val      "type"    (toContributorTypeXml (contributorType c))
        , Val      "name"    (           (contributorName c))
        , PropList "contact" (fmap toXml (contributorContact c))
        ]
instance FromXml Contributor where
    fromXml = do
        id  <- optional $ pAttr "id"
        ext <- many     $ pElement "extension"  fromXml
        t   <-            pElement  "type" (pAttr "value")
        n   <-            pElement  "name" (pAttr "value")
        c   <- many     $ pElement  "contact"  fromXml
        return $ Contributor{
                    contributorAttribs=[]
                  , contributorId=id
                  , contributorExtension=ext
                  , contributorType=fromContributorTypeXml t
                  , contributorName=n
                  , contributorContact=c}

instance ToXml DataRequirementCodeFilter where
    toXml c = concatMap toElement $
        [
--           "id" (fmap toXml (dataRequirementCodeFilterId c))
          PropList "extension"   (fmap toXml (dataRequirementCodeFilterExtension c))
        , OptVal   "path"        (           (dataRequirementCodeFilterPath c))
        , OptVal   "searchParam" (           (dataRequirementCodeFilterSearchParam c))
        , OptVal   "valueSet"    (           (dataRequirementCodeFilterValueSet c))
        , PropList "code"        (fmap toXml (dataRequirementCodeFilterCode c))
        ]
instance FromXml DataRequirementCodeFilter where
    fromXml = do
        id  <- optional $ pAttr "id"
        ext <- many     $ pElement "extension"  fromXml
        p <- optional $ pElement "path" (pAttr "value")
        s <- optional $ pElement "searchParam" (pAttr "value")
        v <- optional $ pElement "valueSet" (pAttr "value")
        c <- many      $ pElement "code"  fromXml
        return $ DataRequirementCodeFilter{
                    dataRequirementCodeFilterAttribs=[]
                  , dataRequirementCodeFilterId=id
                  , dataRequirementCodeFilterExtension=ext
                  , dataRequirementCodeFilterPath=p
                  , dataRequirementCodeFilterSearchParam=s
                  , dataRequirementCodeFilterValueSet=v
                  , dataRequirementCodeFilterCode=c}

toDataRequirementDateFilterValueXml (DataRequirementDateFilterValueDateTime c) = Val  "valueDateTime" (                c)
toDataRequirementDateFilterValueXml (DataRequirementDateFilterValuePeriod   c) = Prop "valuePeriod"   (HM.empty, toXml c)
toDataRequirementDateFilterValueXml (DataRequirementDateFilterValueDuration c) = Prop "valueDuration" (HM.empty, toXml c)


instance ToXml DataRequirementDateFilter where
    toXml c = concatMap toElement $
        [
--           "id" (fmap toXml (dataRequirementDateFilterId c))
          PropList "extension"   (fmap toXml (dataRequirementDateFilterExtension c))
        , OptVal   "path"        (           (dataRequirementDateFilterPath c))
        , OptVal   "searchParam" (           (dataRequirementDateFilterSearchParam c))
        , toDataRequirementDateFilterValueXml (dataRequirementDateFilterValue c)
        ]
instance FromXml DataRequirementDateFilter where
    fromXml = do
        id  <- optional $ pAttr "id"
        ext <- many     $ pElement "extension"  fromXml
        p   <- optional $ pElement "country" (pAttr "value")
        s   <- optional $ pElement "jurisdiction" (pAttr "value")
        v   <- parseValueDT <|> parseValueP <|> parseValueD
        return $ DataRequirementDateFilter{
                    dataRequirementDateFilterAttribs=[]
                  , dataRequirementDateFilterId=id
                  , dataRequirementDateFilterExtension=ext
                  , dataRequirementDateFilterPath=p
                  , dataRequirementDateFilterSearchParam=s
                  , dataRequirementDateFilterValue=v}
        where parseValueDT = do
                hasDT <- pElement "valueDateTime" (pAttr "value")
                return $ (DataRequirementDateFilterValueDateTime hasDT)
              parseValueP = do
                hasP <- pElement "valuePeriod" fromXml
                return $ (DataRequirementDateFilterValuePeriod hasP)
              parseValueD = do
                hasD <- pElement "valueDuration" fromXml
                return $ (DataRequirementDateFilterValueDuration hasD)


instance ToXml DataRequirementSort where
    toXml c = concatMap toElement $
        [
--           "id" (fmap toXml (dataRequirementSortId c))
           PropList "extension" (fmap toXml (dataRequirementSortExtension c))
        ,  Val      "path"      (           (dataRequirementSortPath c))
        ,  Val      "direction" (toSortDirectionXml (dataRequirementSortDirection c))
        ]
instance FromXml DataRequirementSort where
    fromXml = do
        id  <- optional $ pAttr "id"
        ext <- many     $ pElement "extension"  fromXml
        p   <-            pElement  "path" (pAttr "value")
        d   <-            pElement  "direction" (pAttr "value")
        return $ DataRequirementSort{
                    dataRequirementSortAttribs=[]
                  , dataRequirementSortId=id
                  , dataRequirementSortExtension=ext
                  , dataRequirementSortPath=p
                  , dataRequirementSortDirection=fromSortDirectionXml d}

toDataRequirementSubjectXml (DataRequirementSubjectCodeableConcept c) = Prop "subjectCodeableConcept" (HM.empty, toXml c)
toDataRequirementSubjectXml (DataRequirementSubjectReference c)       = Prop "subjectReference"       (HM.empty, toXml c)

instance ToXml DataRequirement where
    toXml c = concatMap toElement $
        [
--           "id" (fmap toXml (dataRequirementId c))
          PropList "extension"   (fmap toXml (dataRequirementExtension c))
        , Val      "type"        (           (dataRequirementType c))
        , ValList  "profile"     (           (dataRequirementProfile c))
        , toDataRequirementSubjectXml (dataRequirementSubject c)
        , ValList  "mustSupport" (           (dataRequirementMustSupport c))
        , PropList "codeFilter"  (fmap toXml (dataRequirementCodeFilter c))
        , PropList "dateFilter"  (fmap toXml (dataRequirementDateFilter c))
        , OptVal   "limit"       (fmap toPositiveInt (dataRequirementLimit c))
        , PropList "sort"        (fmap toXml (dataRequirementSort c))
        ]
instance FromXml DataRequirement where
    fromXml = do
        id  <- optional $ pAttr "id"
        ext <- many     $ pElement "extension"  fromXml
        t   <-            pElement "type" (pAttr "value")
        p   <- many     $ pElement "profile"  (pAttr "value")
        s   <- parseSubjectCC <|> parseSubjectR
        m   <- many     $ pElement "mustSupport" (pAttr "value")
        c   <- many     $ pElement "codeFilter"  fromXml
        d   <- many     $ pElement "dateFilter"  fromXml
        l   <- optional $ pElement "limit" (pAttr "value")
        so  <- many     $ pElement "sort"  fromXml
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
                  , dataRequirementLimit=fmap fromPositiveInt l
                  , dataRequirementSort=so}
        where parseSubjectCC = do
                hasCC <- pElement "subjectCodeableConcept" fromXml
                return $ (DataRequirementSubjectCodeableConcept hasCC)
              parseSubjectR = do
                hasR <- pElement "subjectReference" fromXml
                return $ (DataRequirementSubjectReference hasR)


toExpressionLanguageXml (ElApplicationXFhirQuery) = "application/x-fhir-query"
toExpressionLanguageXml (ElTextCql)               = "text/cql"
toExpressionLanguageXml (ElTextFhirpath)          = "text/fhirpath"
fromExpressionLanguageXml "application/x-fhir-query" = ElApplicationXFhirQuery
fromExpressionLanguageXml "text/cql"                 = ElTextCql
fromExpressionLanguageXml "text/fhirpath"            = ElTextFhirpath


instance ToXml Expression where
    toXml c = concatMap toElement $
        [
--           "id" (fmap toXml (expressionId c))
          PropList "extension" (fmap toXml (expressionExtension c))
        , OptVal   "description" (           (expressionDescription c))
        , OptVal   "name"        (           (expressionName c))
        , Val      "language"    (           toExpressionLanguageXml (expressionLanguage c))
        , OptVal   "expression"  (           (expressionExpression c))
        , OptVal   "reference"   (           (expressionReference c))
        ]
instance FromXml Expression where
    fromXml = do
        id  <- optional $ pAttr "id"
        ext <- many     $ pElement "extension"  fromXml
        d   <- optional $ pElement "description" (pAttr "value")
        n   <- optional $ pElement "name" (pAttr "value")
        l   <-            pElement "language" (pAttr "value")
        e   <- optional $ pElement "expression" (pAttr "value")
        r   <- optional $ pElement "reference" (pAttr "value")
        return $ Expression{
                    expressionAttribs=[]
                  , expressionId=id
                  , expressionExtension=ext
                  , expressionDescription=d
                  , expressionName=n
                  , expressionLanguage=fromExpressionLanguageXml l
                  , expressionExpression=e
                  , expressionReference=r}


instance ToXml ParameterDefinition where
    toXml c = concatMap toElement $
        [
--           "id" (fmap toXml (parameterDefinitionId c))
          PropList "extension" (fmap toXml (parameterDefinitionExtension c))
        , Val      "name" (           (parameterDefinitionName c))
        , Val      "use"  (           (parameterDefinitionUse c))
        , OptVal   "min"  (fmap toInt (parameterDefinitionMin c))
        , OptVal   "max"  (           (parameterDefinitionMax c))
        , OptVal   "documentation" (           (parameterDefinitionDocumentation c))
        , Val      "type"          (           (parameterDefinitionType c))
        , OptVal   "profile"       (           (parameterDefinitionProfile c))
        ]
instance FromXml ParameterDefinition where
    fromXml = do
        id  <- optional $ pAttr "id"
        ext <- many     $ pElement "extension"  fromXml
        n   <-            pElement "name"    (pAttr "value")
        u   <-            pElement "use"     (pAttr "value")
        mi  <- optional $ pElement "min"     (pAttr "value")
        ma  <- optional $ pElement "max"     (pAttr "value")
        d   <- optional $ pElement "documentation" (pAttr "value")
        t   <-            pElement "type"    (pAttr "value")
        p   <- optional $ pElement "profile" (pAttr "value")
        return $ ParameterDefinition{
                    parameterDefinitionAttribs=[]
                  , parameterDefinitionId=id
                  , parameterDefinitionExtension=ext
                  , parameterDefinitionName=n
                  , parameterDefinitionUse=u
                  , parameterDefinitionMin=fmap fromInt mi
                  , parameterDefinitionMax=ma
                  , parameterDefinitionDocumentation=d
                  , parameterDefinitionType=t
                  , parameterDefinitionProfile=p}


toRelatedArtifactTypeXml (RatCitation)     = "citation"
toRelatedArtifactTypeXml (RatComposedOf)   = "composed-of"
toRelatedArtifactTypeXml (RatDependsOn)    = "depends-on"
toRelatedArtifactTypeXml (RatDerivedFrom)  = "derived-from"
toRelatedArtifactTypeXml (RatDocumentation) = "documentation"
toRelatedArtifactTypeXml (RatJustification) = "justification"
toRelatedArtifactTypeXml (RatPredecessor)   = "predecessor"
toRelatedArtifactTypeXml (RatSuccessor)     = "successor"
fromRelatedArtifactTypeXml       "citation"     = RatCitation
fromRelatedArtifactTypeXml       "composed-of"  = RatComposedOf
fromRelatedArtifactTypeXml       "depends-on"   = RatDependsOn
fromRelatedArtifactTypeXml       "derived-from" = RatDerivedFrom
fromRelatedArtifactTypeXml       "documentation" = RatDocumentation
fromRelatedArtifactTypeXml       "justification" = RatJustification
fromRelatedArtifactTypeXml       "predecessor"   = RatPredecessor
fromRelatedArtifactTypeXml       "succesoor"     = RatSuccessor


instance ToXml RelatedArtifact where
    toXml c = concatMap toElement $
        [
--           "id"        (fmap toXml (relatedArtifactId c))
          PropList "extension" (fmap toXml (relatedArtifactExtension c))
        , Val      "type"     (     toRelatedArtifactTypeXml (relatedArtifactType c))
        , OptVal   "label"    (           (relatedArtifactLabel c))
        , OptVal   "display"  (           (relatedArtifactDisplay c))
        , OptVal   "citation" (           (relatedArtifactCitation c))
        , OptVal   "url"      (           (relatedArtifactUrl c))
        , OptProp  "document" (fmap toXml (relatedArtifactDocument c))
        , OptVal   "resource" (           (relatedArtifactResource c))
        ]
instance FromXml RelatedArtifact where
    fromXml = do
        id  <- optional $ pAttr "id"
        ext <- many     $ pElement "extension"  fromXml
        t   <-            pElement "type" (pAttr "value")
        l   <- optional $ pElement "label" (pAttr "value")
        di  <- optional $ pElement "display" (pAttr "value")
        c   <- optional $ pElement "citation" (pAttr "value")
        u   <- optional $ pElement "url" (pAttr "value")
        d   <- optional $ pElement "document" fromXml
        r   <- optional $ pElement "resource" (pAttr "value")
        return $ RelatedArtifact{
                    relatedArtifactAttribs=[]
                  , relatedArtifactId=id
                  , relatedArtifactExtension=ext
                  , relatedArtifactType=fromRelatedArtifactTypeXml t
                  , relatedArtifactLabel=l
                  , relatedArtifactDisplay=di
                  , relatedArtifactCitation=c
                  , relatedArtifactUrl=u
                  , relatedArtifactDocument=d
                  , relatedArtifactResource=r}


toTriggerDefinitionTimingXml (TriggerDefinitionTimingTiming    c) = Prop "timingTiming" (HM.empty, toXml c)
toTriggerDefinitionTimingXml (TriggerDefinitionTimingReference c) = Prop "timingReference" (HM.empty, toXml c)
toTriggerDefinitionTimingXml (TriggerDefinitionTimingDate      c) = Val  "timingDate" c
toTriggerDefinitionTimingXml (TriggerDefinitionTimingDateTime  c) = Val  "timingDateTime" c

toTriggerTypeXml (TtDataAccessEnded) = "data-access-ended"
toTriggerTypeXml (TtDataAccessed) = "data-accessed"
toTriggerTypeXml (TtDataAdded)    = "data-added"
toTriggerTypeXml (TtDataChanged)  = "data-changed"
toTriggerTypeXml (TtDataModified) = "data-modified"
toTriggerTypeXml (TtDataRemoved)  = "data-removed"
toTriggerTypeXml (TtNamedEvent)   = "named-event"
toTriggerTypeXml (TtPeriodic)     = "periodic"
fromTriggerTypeXml "data-access-ended" = TtDataAccessEnded
fromTriggerTypeXml "data-accessed" = TtDataAccessed
fromTriggerTypeXml "data-added"    = TtDataAdded
fromTriggerTypeXml "data-changed"  = TtDataChanged
fromTriggerTypeXml "data-modified" = TtDataModified
fromTriggerTypeXml "data-removed"  = TtDataRemoved
fromTriggerTypeXml "named-event"   = TtNamedEvent
fromTriggerTypeXml "periodic"      = TtPeriodic


instance ToXml TriggerDefinition where
    toXml c = concatMap toElement $
        [
--           "id" (fmap toXml (triggerDefinitionId c))
          PropList "extension" (fmap toXml (triggerDefinitionExtension c))
        , Val      "type" (toTriggerTypeXml (triggerDefinitionType c))
        , OptVal   "name" (           (triggerDefinitionName c))
        , toTriggerDefinitionTimingXml (triggerDefinitionTiming c)
        , PropList "data"      (fmap toXml (triggerDefinitionData c))
        , OptProp  "condition" (fmap toXml (triggerDefinitionCondition c))
        ]
instance FromXml TriggerDefinition where
    fromXml = do
        id  <- optional $ pAttr "id"
        ext <- many     $ pElement "extension"  fromXml
        t   <-            pElement  "type" (pAttr "value")
        n   <- optional $ pElement "name" (pAttr "value")
        ti  <- parseTimingT <|> parseTimingR <|> parseTimingD <|> parseTimingDT
        d   <- many     $ pElement  "data"  fromXml
        c   <- optional $ pElement "condition" fromXml
        return $ TriggerDefinition{
                    triggerDefinitionAttribs=[]
                  , triggerDefinitionId=id
                  , triggerDefinitionExtension=ext
                  , triggerDefinitionType=fromTriggerTypeXml t
                  , triggerDefinitionName=n
                  , triggerDefinitionTiming=ti
                  , triggerDefinitionData=d
                  , triggerDefinitionCondition=c}
        where parseTimingT = do
                hasT <- pElement "timingTiming" fromXml
                return $ (TriggerDefinitionTimingTiming hasT)
              parseTimingR = do
                hasR <- pElement "timingReference" fromXml
                return $ (TriggerDefinitionTimingReference hasR)
              parseTimingD = do
                hasD <- pElement "timingDate"  (pAttr "value")
                return $ (TriggerDefinitionTimingDate hasD)
              parseTimingDT = do
                hasDT  <- pElement "timingDateTime" (pAttr "value")
                return $ (TriggerDefinitionTimingDateTime hasDT)


toUsageContextValue (UcvCC       cc) = Prop "valueCodeableConcept" (HM.empty, toXml cc)
toUsageContextValue (UcvQuantity  q) = Prop "valueQuantity"  (HM.empty, toXml q)
toUsageContextValue (UcvRange     r) = Prop "valueRange"     (HM.empty, toXml r)
toUsageContextValue (UcvReference r) = Prop "valueReference" (HM.empty, toXml r)

instance ToXml UsageContext where
    toXml c = concatMap toElement $
        [
          PropList "extension"   (fmap toXml (usageContextExtension c))
        , Prop "code"  (HM.empty, Xmlbf.toXml (usageContextCode c))
        , toUsageContextValue (usageContextValue c)
        ]
instance FromXml UsageContext where
    fromXml = do
        i  <- optional (pAttr "id") 
        e  <- many     $ pElement "extension" fromXml
        c <- pElement "code" fromXml
        v <- parseValueCC <|> parseValueQ <|> parseValueR <|> parseValueRef
        return $ UsageContext{
              usageContextAttribs=[]
            , usageContextId=i
            , usageContextExtension=e
            , usageContextCode=c
            , usageContextValue=v}
        where parseValueCC = do
                hasCC <- pElement "valueCodeableConcept" fromXml
                return $ (UcvCC hasCC)
              parseValueQ = do
                hasQ <- pElement "valueQuantity" fromXml
                return $ (UcvQuantity hasQ)
              parseValueR = do
                hasR <- pElement "valueRange" fromXml
                return $ (UcvRange hasR)
              parseValueRef = do
                hasRef <- pElement "valueReference" fromXml
                return $ (UcvReference hasRef)

-- Right [Element "Patient" [] [Element "extension" [("url","eurl"),("id","12345")] [Element "valueString" [("value","extext")] []]]]

toExtensionValueXml (Just (ExtensionValueBase64Binary e))= Val  "valueBase64Binary" e
toExtensionValueXml (Just (ExtensionValueBoolean e))     = Val  "valueBoolean" (toBoolean e)
toExtensionValueXml (Just (ExtensionValueCanonical e))   = Val  "valueCanonical" e
toExtensionValueXml (Just (ExtensionValueCode e))        = Val  "valueCode" e
toExtensionValueXml (Just (ExtensionValueDate e))        = Val  "valueDate" e
toExtensionValueXml (Just (ExtensionValueDateTime e))    = Val  "valueDateTime" e
toExtensionValueXml (Just (ExtensionValueDecimal e))     = Val  "valueDecimal" (toDecimal e)
toExtensionValueXml (Just (ExtensionValueId e))          = Val  "valueId" e
toExtensionValueXml (Just (ExtensionValueInstant e))     = Val  "valueInstant" e
toExtensionValueXml (Just (ExtensionValueInteger e))     = Val  "valueInteger" (toInt e)
toExtensionValueXml (Just (ExtensionValueMarkdown e))    = Val  "valueMarkdown" e
toExtensionValueXml (Just (ExtensionValueOid e))         = Val  "valueOid" e
toExtensionValueXml (Just (ExtensionValuePositiveInt e)) = Val  "valuePositiveInt" (toPositiveInt e)
toExtensionValueXml (Just (ExtensionValueString e))      = Val  "valueString" e
toExtensionValueXml (Just (ExtensionValueTime e))        = Val  "valueTime" e
toExtensionValueXml (Just (ExtensionValueUnsignedInt e)) = Val  "valueUnsignedInt" (toUnsignedInt e)
toExtensionValueXml (Just (ExtensionValueUri e))         = Val  "valueUri" e
toExtensionValueXml (Just (ExtensionValueUrl e))         = Val  "valueUrl" e
toExtensionValueXml (Just (ExtensionValueUuid e))        = Val  "valueUuid" e
-- GeneralPurpose
toExtensionValueXml (Just (ExtensionValueAddress e))     = Prop "valueAddress" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueAge e))         = Prop "valueAge" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueAnnotation e))  = Prop "valueAnnotation" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueAttachment e))  = Prop "valueAttachment" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueCodeableConcept e)) = Prop "valueCodeableConcept" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueCoding e))          = Prop "valueCoding" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueContactPoint e))    = Prop "valueContactPoint" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueCount e))       = Prop "valueCount" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueDistance e))    = Prop "valueDistance" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueDuration e))    = Prop "valueDuration" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueHumanName e))   = Prop "valueHumanName" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueIdentifier e))  = Prop "valueIdentifier" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueMoney e))       = Prop "valueMoney" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValuePeriod e))      = Prop "valuePeriod" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueQuantity e))    = Prop "valueQuantity" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueRange e))       = Prop "valueRange" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueRatio e))       = Prop "valueRatio" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueSampledData e)) = Prop "valueSampledData" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueSignature e))   = Prop "valueSignature" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueTiming e))      = Prop "valueTiming" (HM.empty, toXml e)
-- Metadata
toExtensionValueXml (Just (ExtensionValueContactDetail e))       = Prop "valueContactDetail" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueContributor e))         = Prop "valueContributor" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueDataRequirement e))     = Prop "valueDataRequirement" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueExpression e))          = Prop "valueExpression" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueParameterDefinition e)) = Prop "valueParameterDefinition" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueRelatedArtifact e))     = Prop "valueRelatedArtifact" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueTriggerDefinition e))   = Prop "valueTriggerDefinition" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueUsageContext e))        = Prop "valueUsageContext" (HM.empty, toXml e)
-- SpecialPurpose
toExtensionValueXml (Just (ExtensionValueReference e)) = Prop "valueReference" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueDosage e))    = Prop "valueDosage" (HM.empty, toXml e)
toExtensionValueXml (Just (ExtensionValueMeta e))      = Prop "valueMeta" (HM.empty, toXml e)
toExtensionValueXml Nothing                            = Prop "value"     (HM.empty, [])

instance ToXml Extension where
--        ("url"  (extensionUrl e))
    toXml e = concatMap toElement [
        toExtensionValueXml (extensionValue e)
      ]
instance FromXml Extension where
    fromXml = do
        id  <- optional $ pAttr "id"
-- for testing
        url <- optional $ pAttr "url"
        ext <- many     $ pElement "extension" fromXml
        any <- optional $ pAnyElement fromAny
        return Extension{
                  extensionId=id
                , extensionUrl= fromMaybe "" url
                , extensionExtension=ext
                , extensionValue= any
                }

fromAny :: Parser ExtensionValue
fromAny = do
  n <- pName
  case n of
    "valueBase64Binary" -> ExtensionValueBase64Binary <$> (pAttr "value")
    "valueBoolean"      -> do
                           b <- (pAttr "value")
                           pure $ ExtensionValueBoolean (fromBoolean b)
    "valueCanonical"    -> ExtensionValueCanonical <$> (pAttr "value")
    "valueCode"         -> ExtensionValueCode      <$> (pAttr "value")
    "valueDate"         -> ExtensionValueDate      <$> (pAttr "value")
    "valueDateTime"     -> ExtensionValueDateTime  <$> (pAttr "value")
    "valueDecimal" -> do
                      d <- (pAttr "value")
                      pure $ ExtensionValueDecimal (fromDecimal d)
    "valueId" -> ExtensionValueId <$> (pAttr "value")
    "valueInstant" -> ExtensionValueInstant <$> (pAttr "value")
    "valueInteger" -> do 
                      i <- (pAttr "value")
                      pure $ ExtensionValueInteger (fromInt i)
    "valueMarkdown"    -> ExtensionValueMarkdown <$> (pAttr "value")
    "valueOid"         -> ExtensionValueOid <$> (pAttr "value")
    "valuePositiveInt" -> do
                          p <- (pAttr "value")
                          pure $ ExtensionValuePositiveInt (fromPositiveInt p)
    "valueString"      -> ExtensionValueString <$> (pAttr "value")
    "valueTime"        -> ExtensionValueTime <$> (pAttr "value")
    "valueUnsignedInt" -> do
                          u <- (pAttr "value")
                          pure $ ExtensionValueUnsignedInt (fromUnsignedInt u)
    "valueUri" -> ExtensionValueUri <$> (pAttr "value")
    "valueUrl" -> ExtensionValueUrl <$> (pAttr "value")
    "valueUuid" -> ExtensionValueUuid <$> (pAttr "value")
-- GeneralPurpose
    "valueAddress" -> ExtensionValueAddress <$> fromXml
    "valueAge" -> ExtensionValueAge <$> fromXml
    "valueAnnotation" -> ExtensionValueAnnotation <$> fromXml
    "valueAttachment" -> ExtensionValueAttachment <$> fromXml
    "valueCodeableConcept" -> ExtensionValueCodeableConcept <$> fromXml
    "valueCoding" -> ExtensionValueCoding <$> fromXml
    "valueContactPoint" -> ExtensionValueContactPoint <$> fromXml
    "valueCount" -> ExtensionValueCount <$> fromXml
    "valueDistance" -> ExtensionValueDistance <$> fromXml
    "valueDuration" -> ExtensionValueDuration <$> fromXml
    "valueHumanName" -> ExtensionValueHumanName <$> fromXml
    "valueIdentifier" -> ExtensionValueIdentifier <$> fromXml
    "valueMoney" -> ExtensionValueMoney <$> fromXml
    "valuePeriod" -> ExtensionValuePeriod <$> fromXml
    "valueQuantity" -> ExtensionValueQuantity <$> fromXml
    "valueRange" -> ExtensionValueRange <$> fromXml
    "valueRatio" -> ExtensionValueRatio <$> fromXml
    "valueSampledData" -> ExtensionValueSampledData <$> fromXml
    "valueSignature" -> ExtensionValueSignature <$> fromXml
    "valueTiming" -> ExtensionValueTiming <$> fromXml
-- Metadata
    "valueContactDetail" -> ExtensionValueContactDetail <$> fromXml
    "valueContributor" -> ExtensionValueContributor <$> fromXml
    "valueDataRequirement" -> ExtensionValueDataRequirement <$> fromXml
    "valueExpression" -> ExtensionValueExpression <$> fromXml
    "valueParameterDefinition" -> ExtensionValueParameterDefinition <$> fromXml
    "valueRelatedArtifact" -> ExtensionValueRelatedArtifact <$> fromXml
    "valueTriggerDefinition" -> ExtensionValueTriggerDefinition <$> fromXml
    "valueUsageContext" -> ExtensionValueUsageContext <$> fromXml
-- SpecialPurpose
    "valueReference" -> ExtensionValueReference <$> fromXml
    "valueDosage" -> ExtensionValueDosage <$> fromXml
    "valueMeta" -> ExtensionValueMeta <$> fromXml
    _           -> ExtensionValueString <$> (pAttr "value")

