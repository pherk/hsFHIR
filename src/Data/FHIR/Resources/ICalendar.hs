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

module Data.FHIR.Resources.ICalendar where

import Data.Aeson  
import Data.Aeson.Types hiding (parseJSON)

import qualified Data.HashMap.Strict as HM
--import GHC.Generics
import GHC.TypeLits

import RIO
import qualified RIO.Vector as V
-- import Xmlbf
import           Data.FHIR.Datatypes
import           Data.FHIR.Datatypes.XML
import           Data.FHIR.Datatypes.XmlUtils
import           Data.FHIR.Resources.DomainResource
import qualified Xmlbf  as Xmlbf

-- observation.erl       questionnaire.erl
-- questionnaireresponse.erl
-- organization.erl      requestgroup.erl
-- icalendar.erl          parameters.erl        resource.erl
-- imagingstudy.erl       patient.erl           riskassessment.erl
-- familymemberhistory.erl immunization.erl       plandefinition.erl    searchparameter.erl
-- leave.erl              servicerequest.erl
-- library.erl            practitioner.erl
-- location.erl           practitionerrole.erl  task.erl
-- medicationrequest.erl  procedure.erl         
-- provenance.erl
 

data PatientCommunication
    = PatientCommunication { 
        patientCommunicationSuper :: BackboneElement,
        patientCommunicationLanguage :: CodeableConcept,
        patientCommunicationPreferred :: Maybe Boolean}
    deriving (Eq, Show)

instance ToJSON PatientCommunication where
   toJSON p = object $
       filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
       toBackboneElementJSON (patientCommunicationSuper p)
       ++
       [
         "language"  .= toJSON (patientCommunicationLanguage p)
       , "preferred" .= toJSON (patientCommunicationPreferred p)
       ]
instance FromJSON PatientCommunication where
  parseJSON = withObject "PatientCommunication" $ \o -> do
        id  <- o .:? "id" 
        ext <- o .:? "extension" .!= []
        mxt <- o .:? "modifierExtension" .!= []
        l   <- o .:  "language"
        p   <- o .:? "preferred"
        return PatientCommunication{
                    patientCommunicationSuper= mkBackboneElement id ext mxt
                  , patientCommunicationLanguage= l
                  , patientCommunicationPreferred= p
                  }

instance Xmlbf.ToXml PatientCommunication where
  toXml p = cs
     where -- as = HM.fromList $ catMaybes $ fmap toAttr []--[ OptVal "xml:id" (domainResourceAttribs ps) ]
           cs = concatMap toElement $
             [
               Prop   "language"  (HM.empty, Xmlbf.toXml (patientCommunicationLanguage p))
             , OptVal "preferred" (fmap toBoolean (patientCommunicationPreferred p))
             ]
instance Xmlbf.FromXml PatientCommunication where
  fromXml = do
        id <- optional $ Xmlbf.pAttr "id"
        e  <- many     $ Xmlbf.pElement "extension" Xmlbf.fromXml
        m  <- many     $ Xmlbf.pElement "modifierExtension" Xmlbf.fromXml
        la <- Xmlbf.pElement "language" Xmlbf.fromXml
        pr <- optional $ Xmlbf.pElement "preferred" (Xmlbf.pAttr "value")
        return PatientCommunication{
                    patientCommunicationSuper= mkBackboneElement id e m
                  , patientCommunicationLanguage = la
                  , patientCommunicationPreferred = fmap fromBoolean pr
                  }

