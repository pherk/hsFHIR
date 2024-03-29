{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

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
{-
 - needed for FHIR test data
 - CarePlan
 - Condition
 - DiagnosticReport
 - Encounter
 - Goal
 - MedicationRequest
 - Observation
 - Patient
 - Procedure
 -}
module Data.FHIR.Resources.ResourceContainer (
    DomainResourceC(..)
--  module Data.FHIR.Resources.Account
  , module Data.FHIR.Resources.ActivityDefinition
--  module Data.FHIR.Resources.AdverseEvent
--  module Data.FHIR.Resources.AllergyIntolerance
--  module Data.FHIR.Resources.Appointment
--  module Data.FHIR.Resources.AppointmentResponse
--  module Data.FHIR.Resources.AuditEvent
--  module Data.FHIR.Resources.Basic
  , module Data.FHIR.Resources.Binary
--  module Data.FHIR.Resources.BiologicallyDerivedProduct
--  module Data.FHIR.Resources.BodyStructure
  , module Data.FHIR.Resources.CapabilityStatement
  , module Data.FHIR.Resources.CarePlan
  , module Data.FHIR.Resources.CareTeam
--  module Data.FHIR.Resources.CatalogEntry
--  module Data.FHIR.Resources.ChargeItem
--  module Data.FHIR.Resources.ChargeItemDefinition
--  module Data.FHIR.Resources.Claim
--  module Data.FHIR.Resources.ClaimResponse
--  module Data.FHIR.Resources.ClinicalImpression
--  module Data.FHIR.Resources.CodeSystem
  , module Data.FHIR.Resources.Communication
--  module Data.FHIR.Resources.CommunicationRequest
--  module Data.FHIR.Resources.CompartmentDefinition
  , module Data.FHIR.Resources.Composition
--  module Data.FHIR.Resources.ConceptMap
  , module Data.FHIR.Resources.Condition
  , module Data.FHIR.Resources.Consent
--  module Data.FHIR.Resources.Contract
--  module Data.FHIR.Resources.Coverage
--  module Data.FHIR.Resources.CoverageEligibilityRequest
--  module Data.FHIR.Resources.CoverageEligibilityResponse
--  module Data.FHIR.Resources.DetectedIssue
  , module Data.FHIR.Resources.Device
--  module Data.FHIR.Resources.DeviceDefinition
--  module Data.FHIR.Resources.DeviceMetric
--  module Data.FHIR.Resources.DeviceRequest
--  module Data.FHIR.Resources.DeviceUseStatement
  , module Data.FHIR.Resources.DiagnosticReport
--  module Data.FHIR.Resources.DocumentManifest
--  module Data.FHIR.Resources.DocumentReference
--  module Data.FHIR.Resources.EffectEvidenceSynthesis
  , module Data.FHIR.Resources.Encounter
--  module Data.FHIR.Resources.Endpoint
--  module Data.FHIR.Resources.EnrollmentRequest
--  module Data.FHIR.Resources.EnrollmentResponse
  , module Data.FHIR.Resources.EpisodeOfCare
--  module Data.FHIR.Resources.EventDefinition
--  module Data.FHIR.Resources.Evidence
--  module Data.FHIR.Resources.EvidenceVariable
--  module Data.FHIR.Resources.ExampleScenario
--  module Data.FHIR.Resources.ExplanationOfBenefit
--  module Data.FHIR.Resources.FamilyMemberHistory
--  module Data.FHIR.Resources.Flag
  , module Data.FHIR.Resources.Goal
--  module Data.FHIR.Resources.GraphDefinition
--  module Data.FHIR.Resources.Group
--  module Data.FHIR.Resources.GuidanceResponse
  , module Data.FHIR.Resources.HealthcareService
--  module Data.FHIR.Resources.ImagingStudy
--  module Data.FHIR.Resources.Immunization
--  module Data.FHIR.Resources.ImmunizationEvaluation
--  module Data.FHIR.Resources.ImmunizationRecommendation
--  module Data.FHIR.Resources.ImplementationGuide
--  module Data.FHIR.Resources.InsurancePlan
--  module Data.FHIR.Resources.Invoice
  , module Data.FHIR.Resources.Library
--  module Data.FHIR.Resources.Linkage
--  module Data.FHIR.Resources.List
  , module Data.FHIR.Resources.Location
--  module Data.FHIR.Resources.Measure
--  module Data.FHIR.Resources.MeasureReport
--  module Data.FHIR.Resources.Media
--  module Data.FHIR.Resources.Medication
--  module Data.FHIR.Resources.MedicationAdministration
--  module Data.FHIR.Resources.MedicationDispense
--  module Data.FHIR.Resources.MedicationKnowledge
  , module Data.FHIR.Resources.MedicationRequest
--  module Data.FHIR.Resources.MedicationStatement
--  module Data.FHIR.Resources.MedicinalProduct
--  module Data.FHIR.Resources.MedicinalProductAuthorization
--  module Data.FHIR.Resources.MedicinalProductContraindication
--  module Data.FHIR.Resources.MedicinalProductIndication
--  module Data.FHIR.Resources.MedicinalProductIngredient
--  module Data.FHIR.Resources.MedicinalProductInteraction
--  module Data.FHIR.Resources.MedicinalProductManufactured
--  module Data.FHIR.Resources.MedicinalProductPackaged
--  module Data.FHIR.Resources.MedicinalProductPharmaceutical
--  module Data.FHIR.Resources.MedicinalProductUndesirableEffect
--  module Data.FHIR.Resources.MessageDefinition
--  module Data.FHIR.Resources.MessageHeader
--  module Data.FHIR.Resources.MolecularSequence
--  module Data.FHIR.Resources.NamingSystem
--  module Data.FHIR.Resources.NutritionOrder
  , module Data.FHIR.Resources.Observation
--  module Data.FHIR.Resources.ObservationDefinition
--  module Data.FHIR.Resources.OperationDefinition
  , module Data.FHIR.Resources.OperationOutcome
  , module Data.FHIR.Resources.Organization
--  module Data.FHIR.Resources.OrganizationAffiliation
  , module Data.FHIR.Resources.Patient
--  module Data.FHIR.Resources.PaymentNotice
--  module Data.FHIR.Resources.PaymentReconciliation
--  module Data.FHIR.Resources.Person
  , module Data.FHIR.Resources.PlanDefinition
  , module Data.FHIR.Resources.Practitioner
  , module Data.FHIR.Resources.PractitionerRole
  , module Data.FHIR.Resources.Procedure
  , module Data.FHIR.Resources.Provenance
  , module Data.FHIR.Resources.Questionnaire
  , module Data.FHIR.Resources.QuestionnaireResponse
--  module Data.FHIR.Resources.RelatedPerson
  , module Data.FHIR.Resources.RequestGroup
--  module Data.FHIR.Resources.ResearchDefinition
--  module Data.FHIR.Resources.ResearchElementDefinition
--  module Data.FHIR.Resources.ResearchStudy
--  module Data.FHIR.Resources.ResearchSubject
--  module Data.FHIR.Resources.RiskAssessment
--  module Data.FHIR.Resources.RiskEvidenceSynthesis
--  module Data.FHIR.Resources.Schedule
--  module Data.FHIR.Resources.SearchParameter
--  module Data.FHIR.Resources.ServiceRequest
--  module Data.FHIR.Resources.Slot
--  module Data.FHIR.Resources.Specimen
--  module Data.FHIR.Resources.SpecimenDefinition
--  module Data.FHIR.Resources.StructureDefinition
--  module Data.FHIR.Resources.StructureMap
--  module Data.FHIR.Resources.Subscription
--  module Data.FHIR.Resources.Substance
--  module Data.FHIR.Resources.SubstancePolymer
--  module Data.FHIR.Resources.SubstanceProtein
--  module Data.FHIR.Resources.SubstanceReferenceInformation
--  module Data.FHIR.Resources.SubstanceSpecification
--  module Data.FHIR.Resources.SubstanceSourceMaterial
--  module Data.FHIR.Resources.SupplyDelivery
--  module Data.FHIR.Resources.SupplyRequest
  , module Data.FHIR.Resources.Task
--  module Data.FHIR.Resources.TerminologyCapabilities
--  module Data.FHIR.Resources.TestReport
--  module Data.FHIR.Resources.TestScript
--  module Data.FHIR.Resources.ValueSet
--  module Data.FHIR.Resources.VerificationResult
  , module Data.FHIR.Resources.UserConfig
  , module Data.FHIR.Resources.Leave
  , module Data.FHIR.Resources.ICalendar
  ) where

import Data.Aeson  
import Data.Aeson.Types hiding (parseJSON)
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key as AK
import Data.Aeson.KeyMap as AKM
#endif

import GHC.TypeLits

import RIO
import qualified RIO.Text as T
import qualified RIO.Vector as V
import qualified RIO.HashMap as HM
import qualified Xmlbf as Xmlbf

import Data.FHIR.Datatypes

--import Data.FHIR.Resources.Account
import Data.FHIR.Resources.ActivityDefinition
--import Data.FHIR.Resources.AdverseEvent
--import Data.FHIR.Resources.AllergyIntolerance
--import Data.FHIR.Resources.Appointment
--import Data.FHIR.Resources.AppointmentResponse
--import Data.FHIR.Resources.AuditEvent
--import Data.FHIR.Resources.Basic
import Data.FHIR.Resources.Binary
--import Data.FHIR.Resources.BiologicallyDerivedProduct
--import Data.FHIR.Resources.BodyStructure
import Data.FHIR.Resources.CapabilityStatement
import Data.FHIR.Resources.CarePlan
import Data.FHIR.Resources.CareTeam
--import Data.FHIR.Resources.CatalogEntry
--import Data.FHIR.Resources.ChargeItem
--import Data.FHIR.Resources.ChargeItemDefinition
--import Data.FHIR.Resources.Claim
--import Data.FHIR.Resources.ClaimResponse
--import Data.FHIR.Resources.ClinicalImpression
--import Data.FHIR.Resources.CodeSystem
import Data.FHIR.Resources.Communication
--import Data.FHIR.Resources.CommunicationRequest
--import Data.FHIR.Resources.CompartmentDefinition
import Data.FHIR.Resources.Composition
--import Data.FHIR.Resources.ConceptMap
import Data.FHIR.Resources.Condition
import Data.FHIR.Resources.Consent
--import Data.FHIR.Resources.Contract
--import Data.FHIR.Resources.Coverage
--import Data.FHIR.Resources.CoverageEligibilityRequest
--import Data.FHIR.Resources.CoverageEligibilityResponse
--import Data.FHIR.Resources.DetectedIssue
import Data.FHIR.Resources.Device
--import Data.FHIR.Resources.DeviceDefinition
--import Data.FHIR.Resources.DeviceMetric
--import Data.FHIR.Resources.DeviceRequest
--import Data.FHIR.Resources.DeviceUseStatement
import Data.FHIR.Resources.DiagnosticReport
--import Data.FHIR.Resources.DocumentManifest
--import Data.FHIR.Resources.DocumentReference
--import Data.FHIR.Resources.EffectEvidenceSynthesis
import Data.FHIR.Resources.Encounter
--import Data.FHIR.Resources.Endpoint
--import Data.FHIR.Resources.EnrollmentRequest
--import Data.FHIR.Resources.EnrollmentResponse
import Data.FHIR.Resources.EpisodeOfCare
--import Data.FHIR.Resources.EventDefinition
--import Data.FHIR.Resources.Evidence
--import Data.FHIR.Resources.EvidenceVariable
--import Data.FHIR.Resources.ExampleScenario
--import Data.FHIR.Resources.ExplanationOfBenefit
--import Data.FHIR.Resources.FamilyMemberHistory
--import Data.FHIR.Resources.Flag
import Data.FHIR.Resources.Goal
--import Data.FHIR.Resources.GraphDefinition
--import Data.FHIR.Resources.Group
--import Data.FHIR.Resources.GuidanceResponse
import Data.FHIR.Resources.HealthcareService
--import Data.FHIR.Resources.ImagingStudy
--import Data.FHIR.Resources.Immunization
--import Data.FHIR.Resources.ImmunizationEvaluation
--import Data.FHIR.Resources.ImmunizationRecommendation
--import Data.FHIR.Resources.ImplementationGuide
--import Data.FHIR.Resources.InsurancePlan
--import Data.FHIR.Resources.Invoice
import Data.FHIR.Resources.Library
--import Data.FHIR.Resources.Linkage
--import Data.FHIR.Resources.List
import Data.FHIR.Resources.Location
--import Data.FHIR.Resources.Measure
--import Data.FHIR.Resources.MeasureReport
--import Data.FHIR.Resources.Media
--import Data.FHIR.Resources.Medication
--import Data.FHIR.Resources.MedicationAdministration
--import Data.FHIR.Resources.MedicationDispense
--import Data.FHIR.Resources.MedicationKnowledge
import Data.FHIR.Resources.MedicationRequest
--import Data.FHIR.Resources.MedicationStatement
--import Data.FHIR.Resources.MedicinalProduct
--import Data.FHIR.Resources.MedicinalProductAuthorization
--import Data.FHIR.Resources.MedicinalProductContraindication
--import Data.FHIR.Resources.MedicinalProductIndication
--import Data.FHIR.Resources.MedicinalProductIngredient
--import Data.FHIR.Resources.MedicinalProductInteraction
--import Data.FHIR.Resources.MedicinalProductManufactured
--import Data.FHIR.Resources.MedicinalProductPackaged
--import Data.FHIR.Resources.MedicinalProductPharmaceutical
--import Data.FHIR.Resources.MedicinalProductUndesirableEffect
--import Data.FHIR.Resources.MessageDefinition
--import Data.FHIR.Resources.MessageHeader
--import Data.FHIR.Resources.MolecularSequence
--import Data.FHIR.Resources.NamingSystem
--import Data.FHIR.Resources.NutritionOrder
import Data.FHIR.Resources.Observation
--import Data.FHIR.Resources.ObservationDefinition
--import Data.FHIR.Resources.OperationDefinition
import Data.FHIR.Resources.OperationOutcome
import Data.FHIR.Resources.Organization
--import Data.FHIR.Resources.OrganizationAffiliation
import Data.FHIR.Resources.Patient
--import Data.FHIR.Resources.PaymentNotice
--import Data.FHIR.Resources.PaymentReconciliation
--import Data.FHIR.Resources.Person
import Data.FHIR.Resources.PlanDefinition
import Data.FHIR.Resources.Practitioner
import Data.FHIR.Resources.PractitionerRole
import Data.FHIR.Resources.Procedure
import Data.FHIR.Resources.Provenance
import Data.FHIR.Resources.Questionnaire
import Data.FHIR.Resources.QuestionnaireResponse
--import Data.FHIR.Resources.RelatedPerson
import Data.FHIR.Resources.RequestGroup
--import Data.FHIR.Resources.ResearchDefinition
--import Data.FHIR.Resources.ResearchElementDefinition
--import Data.FHIR.Resources.ResearchStudy
--import Data.FHIR.Resources.ResearchSubject
--import Data.FHIR.Resources.RiskAssessment
--import Data.FHIR.Resources.RiskEvidenceSynthesis
--import Data.FHIR.Resources.Schedule
--import Data.FHIR.Resources.SearchParameter
--import Data.FHIR.Resources.ServiceRequest
--import Data.FHIR.Resources.Slot
--import Data.FHIR.Resources.Specimen
--import Data.FHIR.Resources.SpecimenDefinition
--import Data.FHIR.Resources.StructureDefinition
--import Data.FHIR.Resources.StructureMap
--import Data.FHIR.Resources.Subscription
--import Data.FHIR.Resources.Substance
--import Data.FHIR.Resources.SubstancePolymer
--import Data.FHIR.Resources.SubstanceProtein
--import Data.FHIR.Resources.SubstanceReferenceInformation
--import Data.FHIR.Resources.SubstanceSpecification
--import Data.FHIR.Resources.SubstanceSourceMaterial
--import Data.FHIR.Resources.SupplyDelivery
--import Data.FHIR.Resources.SupplyRequest
import Data.FHIR.Resources.Task
--import Data.FHIR.Resources.TerminologyCapabilities
--import Data.FHIR.Resources.TestReport
--import Data.FHIR.Resources.TestScript
--import Data.FHIR.Resources.ValueSet
--import Data.FHIR.Resources.VerificationResult
import Data.FHIR.Resources.UserConfig
import Data.FHIR.Resources.Leave
import Data.FHIR.Resources.ICalendar

type ResourceContainer = DomainResourceC

-- | DomainResourceC
--   solving import cycles for extensions
--   TODO
--   not very elegant, is there a better solution on type level?

data DomainResourceC
--  = AccountR Account
  = ActivityDefinitionDR ActivityDefinition
--  | AdverseEventDR AdverseEvent
--  | AllergyIntoleranceDR AllergyIntolerance
--  | AppointmentDR Appointment
--  | AppointmentResponseDR AppointmentResponse
--  | AuditEventDR AuditEvent
--  | BasicDR Basic
  | BinaryDR Binary
--  | BiologicallyDerivedProductDR BiologicallyDerivedProduct
--  | BodyStructureDR BodyStructure
  | CapabilityStatementDR CapabilityStatement
  | CarePlanDR CarePlan
  | CareTeamDR CareTeam
--  | CatalogEntryDR CatalogEntry
--  | ChargeItemDR ChargeItem
--  | ChargeItemDefinitionDR ChargeItemDefinition
--  | ClaimDR Claim
--  | ClaimResponseDR ClaimResponse
--  | ClinicalImpressionDR ClinicalImpression
--  | CodeSystemDR CodeSystem
  | CommunicationDR Communication
--  | CommunicationRequestDR CommunicationRequest
--  | CompartmentDefinitionDR CompartmentDefinition
  | CompositionDR Composition
--  | ConceptMapDR ConceptMap
  | ConditionDR Condition
  | ConsentDR Consent
--  | ContractDR Contract
--  | CoverageDR Coverage
--  | CoverageEligibilityRequestDR CoverageEligibilityRequest
--  | CoverageEligibilityResponseDR CoverageEligibilityResponse
--  | DetectedIssueDR DetectedIssue
  | DeviceDR Device
--  | DeviceDefinitionDR DeviceDefinition
--  | DeviceMetricDR DeviceMetric
--  | DeviceRequestDR DeviceRequest
--  | DeviceUseStatementDR DeviceUseStatement
  | DiagnosticReportDR DiagnosticReport
--  | DocumentManifestDR DocumentManifest
--  | DocumentReferenceDR DocumentReference
--  | EffectEvidenceSynthesisDR EffectEvidenceSynthesis
  | EncounterDR Encounter
--  | EndpointDR Endpoint
--  | EnrollmentRequestDR EnrollmentRequest
--  | EnrollmentResponseDR EnrollmentResponse
  | EpisodeOfCareDR EpisodeOfCare
--  | EventDefinitionDR EventDefinition
--  | EvidenceDR Evidence
--  | EvidenceVariableDR EvidenceVariable
--  | ExampleScenarioDR ExampleScenario
--  | ExplanationOfBenefitDR ExplanationOfBenefit
--  | FamilyMemberHistoryDR FamilyMemberHistory
--  | FlagDR Flag
  | GoalDR Goal
--  | GraphDefinitionDR GraphDefinition
--  | GroupDR Group
--  | GuidanceResponseDR GuidanceResponse
  | HealthcareServiceDR HealthcareService
--  | ImagingStudyDR ImagingStudy
--  | ImmunizationDR Immunization
--  | ImmunizationEvaluationDR ImmunizationEvaluation
--  | ImmunizationRecommendationDR ImmunizationRecommendation
--  | ImplementationGuideDR ImplementationGuide
--  | InsurancePlanDR InsurancePlan
--  | InvoiceDR Invoice
  | LibraryDR Library
--  | LinkageDR Linkage
--  | ListDR List
  | LocationDR Location
--  | MeasureDR Measure
--  | MeasureReportDR MeasureReport
--  | MediaDR Media
--  | MedicationDR Medication
--  | MedicationAdministrationDR MedicationAdministration
--  | MedicationDispenseDR MedicationDispense
--  | MedicationKnowledgeDR MedicationKnowledge
  | MedicationRequestDR MedicationRequest
--  | MedicationStatementDR MedicationStatement
--  | MedicinalProductDR MedicinalProduct
--  | MedicinalProductAuthorizationDR MedicinalProductAuthorization
--  | MedicinalProductContraindicationDR MedicinalProductContraindication
--  | MedicinalProductIndicationDR MedicinalProductIndication
--  | MedicinalProductIngredientDR MedicinalProductIngredient
--  | MedicinalProductInteractionDR MedicinalProductInteraction
--  | MedicinalProductManufacturedDR MedicinalProductManufactured
--  | MedicinalProductPackagedDR MedicinalProductPackaged
--  | MedicinalProductPharmaceuticalDR MedicinalProductPharmaceutical
--  | MedicinalProductUndesirableEffectDR MedicinalProductUndesirableEffect
--  | MessageDefinitionDR MessageDefinition
--  | MessageHeaderDR MessageHeader
--  | MolecularSequenceDR MolecularSequence
--  | NamingSystemDR NamingSystem
--  | NutritionOrderDR NutritionOrder
  | ObservationDR Observation
--  | ObservationDefinitionDR ObservationDefinition
--  | OperationDefinitionDR OperationDefinition
  | OperationOutcomeDR OperationOutcome
  | OrganizationDR Organization
--  | OrganizationAffiliationDR OrganizationAffiliation
--  | ParametersDR Parameters
  | PatientDR Patient
--  | PaymentNoticeDR PaymentNotice
--  | PaymentReconciliationDR PaymentReconciliation
--  | PersonDR Person
  | PlanDefinitionDR PlanDefinition
  | PractitionerDR Practitioner
  | PractitionerRoleDR PractitionerRole
  | ProcedureDR Procedure
  | ProvenanceDR Provenance
  | QuestionnaireDR Questionnaire
  | QuestionnaireResponseDR QuestionnaireResponse
--  | RelatedPersonDR RelatedPerson
  | RequestGroupDR RequestGroup
--  | ResearchDefinitionDR ResearchDefinition
--  | ResearchElementDefinitionDR ResearchElementDefinition
--  | ResearchStudyDR ResearchStudy
--  | ResearchSubjectDR ResearchSubject
--  | RiskAssessmentDR RiskAssessment
--  | RiskEvidenceSynthesisDR RiskEvidenceSynthesis
--  | ScheduleDR Schedule
--  | SearchParameterDR SearchParameter
--  | ServiceRequestDR ServiceRequest
--  | SlotDR Slot
--  | SpecimenDR Specimen
--  | SpecimenDefinitionDR SpecimenDefinition
--  | StructureDefinitionDR StructureDefinition
--  | StructureMapDR StructureMap
--  | SubscriptionDR Subscription
--  | SubstanceDR Substance
--  | SubstancePolymerDR SubstancePolymer
--  | SubstanceProteinDR SubstanceProtein
--  | SubstanceReferenceInformationDR SubstanceReferenceInformation
--  | SubstanceSpecificationDR SubstanceSpecification
--  | SubstanceSourceMaterialDR SubstanceSourceMaterial
--  | SupplyDeliveryDR SupplyDelivery
--  | SupplyRequestDR SupplyRequest
  | TaskDR Task
--  | TerminologyCapabilitiesDR TerminologyCapabilities
--  | TestReportDR TestReport
--  | TestScriptDR TestScript
--  | ValueSetDR ValueSet
--  | VerificationResultDR VerificationResult
-- TODO not needed!??
  | UserConfigDR UserConfig
  | LeaveDR Leave
  | ICalendarDR ICalendar
  deriving (Eq, Show)

instance ToJSON DomainResourceC where
--  toJSON (AccountDR e) = toJSON e
  toJSON (ActivityDefinitionDR e) = toJSON e
--  toJSON (AdverseEventDR e) = toJSON e
--  toJSON (AllergyIntoleranceDR e) = toJSON e
--  toJSON (AppointmentDR e) = toJSON e
--  toJSON (AppointmentResponseDR e) = toJSON e
--  toJSON (AuditEventDR e) = toJSON e
--  toJSON (BasicDR e) = toJSON e
  toJSON (BinaryDR e) = toJSON e
--  toJSON (BiologicallyDerivedProductDR e) = toJSON e
--  toJSON (BodyStructureDR e) = toJSON e
--  toJSON (CapabilityStatementDR e) = toJSON e
  toJSON (CarePlanDR e) = toJSON e
  toJSON (CareTeamDR e) = toJSON e
--  toJSON (CatalogEntryDR e) = toJSON e
--  toJSON (ChargeItemDR e) = toJSON e
--  toJSON (ChargeItemDefinitionDR e) = toJSON e
--  toJSON (ClaimDR e) = toJSON e
--  toJSON (ClaimResponseDR e) = toJSON e
--  toJSON (ClinicalImpressionDR e) = toJSON e
--  toJSON (CodeSystemDR e) = toJSON e
  toJSON (CommunicationDR e) = toJSON e
--  toJSON (CommunicationRequestDR e) = toJSON e
--  toJSON (CompartmentDefinitionDR e) = toJSON e
  toJSON (CompositionDR e) = toJSON e
--  toJSON (ConceptMapDR e) = toJSON e
  toJSON (ConditionDR e) = toJSON e
  toJSON (ConsentDR e) = toJSON e
--  toJSON (ContractDR e) = toJSON e
--  toJSON (CoverageDR e) = toJSON e
--  toJSON (CoverageEligibilityRequestDR e) = toJSON e
--  toJSON (CoverageEligibilityResponseDR e) = toJSON e
--  toJSON (DetectedIssueDR e) = toJSON e
  toJSON (DeviceDR e) = toJSON e
--  toJSON (DeviceDefinitionDR e) = toJSON e
--  toJSON (DeviceMetricDR e) = toJSON e
--  toJSON (DeviceRequestDR e) = toJSON e
--  toJSON (DeviceUseStatementDR e) = toJSON e
  toJSON (DiagnosticReportDR e) = toJSON e
--  toJSON (DocumentManifestDR e) = toJSON e
--  toJSON (DocumentReferenceDR e) = toJSON e
--  toJSON (EffectEvidenceSynthesisDR e) = toJSON e
  toJSON (EncounterDR e) = toJSON e
--  toJSON (EndpointDR e) = toJSON e
--  toJSON (EnrollmentRequestDR e) = toJSON e
--  toJSON (EnrollmentResponseDR e) = toJSON e
  toJSON (EpisodeOfCareDR e) = toJSON e
--  toJSON (EventDefinitionDR e) = toJSON e
--  toJSON (EvidenceDR e) = toJSON e
--  toJSON (EvidenceVariableDR e) = toJSON e
--  toJSON (ExampleScenarioDR e) = toJSON e
--  toJSON (ExplanationOfBenefitDR e) = toJSON e
--  toJSON (FamilyMemberHistoryDR e) = toJSON e
--  toJSON (FlagDR e) = toJSON e
  toJSON (GoalDR e) = toJSON e
--  toJSON (GraphDefinitionDR e) = toJSON e
--  toJSON (GroupDR e) = toJSON e
--  toJSON (GuidanceResponseDR e) = toJSON e
  toJSON (HealthcareServiceDR e) = toJSON e
--  toJSON (ImagingStudyDR e) = toJSON e
--  toJSON (ImmunizationDR e) = toJSON e
--  toJSON (ImmunizationEvaluationDR e) = toJSON e
--  toJSON (ImmunizationRecommendationDR e) = toJSON e
--  toJSON (ImplementationGuideDR e) = toJSON e
--  toJSON (InsurancePlanDR e) = toJSON e
--  toJSON (InvoiceDR e) = toJSON e
  toJSON (LibraryDR e) = toJSON e
--  toJSON (LinkageDR e) = toJSON e
--  toJSON (ListDR e) = toJSON e
  toJSON (LocationDR e) = toJSON e
--  toJSON (MeasureDR e) = toJSON e
--  toJSON (MeasureReportDR e) = toJSON e
--  toJSON (MediaDR e) = toJSON e
--  toJSON (MedicationDR e) = toJSON e
--  toJSON (MedicationAdministrationDR e) = toJSON e
--  toJSON (MedicationDispenseDR e) = toJSON e
--  toJSON (MedicationKnowledgeDR e) = toJSON e
  toJSON (MedicationRequestDR e) = toJSON e
--  toJSON (MedicationStatementDR e) = toJSON e
--  toJSON (MedicinalProductDR e) = toJSON e
--  toJSON (MedicinalProductAuthorizationDR e) = toJSON e
--  toJSON (MedicinalProductContraindicationDR e) = toJSON e
--  toJSON (MedicinalProductIndicationDR e) = toJSON e
--  toJSON (MedicinalProductIngredientDR e) = toJSON e
--  toJSON (MedicinalProductInteractionDR e) = toJSON e
--  toJSON (MedicinalProductManufacturedDR e) = toJSON e
--  toJSON (MedicinalProductPackagedDR e) = toJSON e
--  toJSON (MedicinalProductPharmaceuticalDR e) = toJSON e
--  toJSON (MedicinalProductUndesirableEffectDR e) = toJSON e
--  toJSON (MessageDefinitionDR e) = toJSON e
--  toJSON (MessageHeaderDR e) = toJSON e
--  toJSON (MolecularSequenceDR e) = toJSON e
--  toJSON (NamingSystemDR e) = toJSON e
--  toJSON (NutritionOrderDR e) = toJSON e
  toJSON (ObservationDR e) = toJSON e
--  toJSON (ObservationDefinitionDR e) = toJSON e
--  toJSON (OperationDefinitionDR e) = toJSON e
  toJSON (OperationOutcomeDR e) = toJSON e
  toJSON (OrganizationDR e) = toJSON e
--  toJSON (OrganizationAffiliationDR e) = toJSON e
  toJSON (PatientDR e) = toJSON e
--  toJSON (PaymentNoticeDR e) = toJSON e
--  toJSON (PaymentReconciliationDR e) = toJSON e
--  toJSON (PersonDR e) = toJSON e
  toJSON (PlanDefinitionDR e) = toJSON e
  toJSON (PractitionerDR e) = toJSON e
  toJSON (PractitionerRoleDR e) = toJSON e
  toJSON (ProcedureDR e) = toJSON e
  toJSON (ProvenanceDR e) = toJSON e
  toJSON (QuestionnaireDR e) = toJSON e
  toJSON (QuestionnaireResponseDR e) = toJSON e
--  toJSON (RelatedPersonDR e) = toJSON e
  toJSON (RequestGroupDR e) = toJSON e
--  toJSON (ResearchDefinitionDR e) = toJSON e
--  toJSON (ResearchElementDefinitionDR e) = toJSON e
--  toJSON (ResearchStudyDR e) = toJSON e
--  toJSON (ResearchSubjectDR e) = toJSON e
--  toJSON (RiskAssessmentDR e) = toJSON e
--  toJSON (RiskEvidenceSynthesisDR e) = toJSON e
--  toJSON (ScheduleDR e) = toJSON e
--  toJSON (SearchParameterDR e) = toJSON e
--  toJSON (ServiceRequestDR e) = toJSON e
--  toJSON (SlotDR e) = toJSON e
--  toJSON (SpecimenDR e) = toJSON e
--  toJSON (SpecimenDefinitionDR e) = toJSON e
--  toJSON (StructureDefinitionDR e) = toJSON e
--  toJSON (StructureMapDR e) = toJSON e
--  toJSON (SubscriptionDR e) = toJSON e
--  toJSON (SubstanceDR e) = toJSON e
--  toJSON (SubstancePolymerDR e) = toJSON e
--  toJSON (SubstanceProteinDR e) = toJSON e
--  toJSON (SubstanceReferenceInformationDR e) = toJSON e
--  toJSON (SubstanceSpecificationDR e) = toJSON e
--  toJSON (SubstanceSourceMaterialDR e) = toJSON e
--  toJSON (SupplyDeliveryDR e) = toJSON e
--  toJSON (SupplyRequestDR e) = toJSON e
  toJSON (TaskDR e) = toJSON e
--  toJSON (TerminologyCapabilitiesDR e) = toJSON e
--  toJSON (TestReportDR e) = toJSON e
--  toJSON (TestScriptDR e) = toJSON e
--  toJSON (ValueSetDR e) = toJSON e
--  toJSON (VerificationResultDR e) = toJSON e
  toJSON (UserConfigDR e) = toJSON e
  toJSON (LeaveDR e) = toJSON e
  toJSON (ICalendarDR e) = toJSON e
instance FromJSON DomainResourceC where
   parseJSON (Object v) = do
       case rt of
--         Just "Account" -> AccountDR <$> parseJSON (Object v)
         Just "ActivityDefinition" -> ActivityDefinitionDR <$> parseJSON (Object v)
--         Just "AdverseEvent" -> AdverseEventDR <$> parseJSON (Object v)
--         Just "AllergyIntolerance" -> AllergyIntoleranceDR <$> parseJSON (Object v)
--         Just "Appointment" -> AppointmentDR <$> parseJSON (Object v)
--         Just "AppointmentResponse" -> AppointmentResponseDR <$> parseJSON (Object v)
--         Just "AuditEvent" -> AuditEventDR <$> parseJSON (Object v)
--         Just "Basic" -> BasicDR <$> parseJSON (Object v)
         Just "Binary" -> BinaryDR <$> parseJSON (Object v)
--         Just "BiologicallyDerivedProduct" -> BiologicallyDerivedProductDR <$> parseJSON (Object v)
--         Just "BodyStructure" -> BodyStructureDR <$> parseJSON (Object v)
--         Just "CapabilityStatement" -> CapabilityStatementDR <$> parseJSON (Object v)
         Just "CarePlan" -> CarePlanDR <$> parseJSON (Object v)
         Just "CareTeam" -> CareTeamDR <$> parseJSON (Object v)
--         Just "CatalogEntry" -> CatalogEntryDR <$> parseJSON (Object v)
--         Just "ChargeItem" -> ChargeItemDR <$> parseJSON (Object v)
--         Just "ChargeItemDefinition" -> ChargeItemDefinitionDR <$> parseJSON (Object v)
--         Just "Claim" -> ClaimDR <$> parseJSON (Object v)
--         Just "ClaimResponse" -> ClaimResponseDR <$> parseJSON (Object v)
--         Just "ClinicalImpression" -> ClinicalImpressionDR <$> parseJSON (Object v)
--         Just "CodeSystem" -> CodeSystemDR <$> parseJSON (Object v)
         Just "Communication" -> CommunicationDR <$> parseJSON (Object v)
--         Just "CommunicationRequest" -> CommunicationRequestDR <$> parseJSON (Object v)
--         Just "CompartmentDefinition" -> CompartmentDefinitionDR <$> parseJSON (Object v)
         Just "Composition" -> CompositionDR <$> parseJSON (Object v)
--         Just "ConceptMap" -> ConceptMapDR <$> parseJSON (Object v)
         Just "Condition" -> ConditionDR <$> parseJSON (Object v)
         Just "Consent" -> ConsentDR <$> parseJSON (Object v)
--         Just "Contract" -> ContractDR <$> parseJSON (Object v)
--         Just "Coverage" -> CoverageDR <$> parseJSON (Object v)
--         Just "CoverageEligibilityRequest" -> CoverageEligibilityRequestDR <$> parseJSON (Object v)
--         Just "CoverageEligibilityResponse" -> CoverageEligibilityResponseDR <$> parseJSON (Object v)
--         Just "DetectedIssue" -> DetectedIssueDR <$> parseJSON (Object v)
         Just "Device" -> DeviceDR <$> parseJSON (Object v)
--         Just "DeviceDefinition" -> DeviceDefinitionDR <$> parseJSON (Object v)
--         Just "DeviceMetric" -> DeviceMetricDR <$> parseJSON (Object v)
--         Just "DeviceRequest" -> DeviceRequestDR <$> parseJSON (Object v)
--         Just "DeviceUseStatement" -> DeviceUseStatementDR <$> parseJSON (Object v)
         Just "DiagnosticReport" -> DiagnosticReportDR <$> parseJSON (Object v)
--         Just "DocumentManifest" -> DocumentManifestDR <$> parseJSON (Object v)
--         Just "DocumentReference" -> DocumentReferenceDR <$> parseJSON (Object v)
--         Just "EffectEvidenceSynthesis" -> EffectEvidenceSynthesisDR <$> parseJSON (Object v)
         Just "Encounter" -> EncounterDR <$> parseJSON (Object v)
--         Just "Endpoint" -> EndpointDR <$> parseJSON (Object v)
--         Just "EnrollmentRequest" -> EnrollmentRequestDR <$> parseJSON (Object v)
--         Just "EnrollmentResponse" -> EnrollmentResponseDR <$> parseJSON (Object v)
         Just "EpisodeOfCare" -> EpisodeOfCareDR <$> parseJSON (Object v)
--         Just "EventDefinition" -> EventDefinitionDR <$> parseJSON (Object v)
--         Just "Evidence" -> EvidenceDR <$> parseJSON (Object v)
--         Just "EvidenceVariable" -> EvidenceVariableDR <$> parseJSON (Object v)
--         Just "ExampleScenario" -> ExampleScenarioDR <$> parseJSON (Object v)
--         Just "ExplanationOfBenefit" -> ExplanationOfBenefitDR <$> parseJSON (Object v)
--         Just "FamilyMemberHistory" -> FamilyMemberHistoryDR <$> parseJSON (Object v)
--         Just "Flag" -> FlagDR <$> parseJSON (Object v)
         Just "Goal" -> GoalDR <$> parseJSON (Object v)
--         Just "GraphDefinition" -> GraphDefinitionDR <$> parseJSON (Object v)
--         Just "Group" -> GroupDR <$> parseJSON (Object v)
--         Just "GuidanceResponse" -> GuidanceResponseDR <$> parseJSON (Object v)
         Just "HealthcareService" -> HealthcareServiceDR <$> parseJSON (Object v)
--         Just "ImagingStudy" -> ImagingStudyDR <$> parseJSON (Object v)
--         Just "Immunization" -> ImmunizationDR <$> parseJSON (Object v)
--         Just "ImmunizationEvaluation" -> ImmunizationEvaluationDR <$> parseJSON (Object v)
--         Just "ImmunizationRecommendation" -> ImmunizationRecommendationDR <$> parseJSON (Object v)
--         Just "ImplementationGuide" -> ImplementationGuideDR <$> parseJSON (Object v)
--         Just "InsurancePlan" -> InsurancePlanDR <$> parseJSON (Object v)
--         Just "Invoice" -> InvoiceDR <$> parseJSON (Object v)
         Just "Library" -> LibraryDR <$> parseJSON (Object v)
--         Just "Linkage" -> LinkageDR <$> parseJSON (Object v)
--         Just "List" -> ListDR <$> parseJSON (Object v)
         Just "Location" -> LocationDR <$> parseJSON (Object v)
--         Just "Measure" -> MeasureDR <$> parseJSON (Object v)
--         Just "MeasureReport" -> MeasureReportDR <$> parseJSON (Object v)
--         Just "Media" -> MediaDR <$> parseJSON (Object v)
--         Just "Medication" -> MedicationDR <$> parseJSON (Object v)
--         Just "MedicationAdministration" -> MedicationAdministrationDR <$> parseJSON (Object v)
--         Just "MedicationDispense" -> MedicationDispenseDR <$> parseJSON (Object v)
--         Just "MedicationKnowledge" -> MedicationKnowledgeDR <$> parseJSON (Object v)
         Just "MedicationRequest" -> MedicationRequestDR <$> parseJSON (Object v)
--         Just "MedicationStatement" -> MedicationStatementDR <$> parseJSON (Object v)
--         Just "MedicinalProduct" -> MedicinalProductDR <$> parseJSON (Object v)
--         Just "MedicinalProductAuthorization" -> MedicinalProductAuthorizationDR <$> parseJSON (Object v)
--         Just "MedicinalProductContraindication" -> MedicinalProductContraindicationDR <$> parseJSON (Object v)
--         Just "MedicinalProductIndication" -> MedicinalProductIndicationDR <$> parseJSON (Object v)
--         Just "MedicinalProductIngredient" -> MedicinalProductIngredientDR <$> parseJSON (Object v)
--         Just "MedicinalProductInteraction" -> MedicinalProductInteractionDR <$> parseJSON (Object v)
--         Just "MedicinalProductManufactured" -> MedicinalProductManufacturedDR <$> parseJSON (Object v)
--         Just "MedicinalProductPackaged" -> MedicinalProductPackagedDR <$> parseJSON (Object v)
--         Just "MedicinalProductPharmaceutical" -> MedicinalProductPharmaceuticalDR <$> parseJSON (Object v)
--         Just "MedicinalProductUndesirableEffect" -> MedicinalProductUndesirableEffectDR <$> parseJSON (Object v)
--         Just "MessageDefinition" -> MessageDefinitionDR <$> parseJSON (Object v)
--         Just "MessageHeader" -> MessageHeaderDR <$> parseJSON (Object v)
--         Just "MolecularSequence" -> MolecularSequenceDR <$> parseJSON (Object v)
--         Just "NamingSystem" -> NamingSystemDR <$> parseJSON (Object v)
--         Just "NutritionOrder" -> NutritionOrderDR <$> parseJSON (Object v)
         Just "Observation" -> ObservationDR <$> parseJSON (Object v)
--         Just "ObservationDefinition" -> ObservationDefinitionDR <$> parseJSON (Object v)
--         Just "OperationDefinition" -> OperationDefinitionDR <$> parseJSON (Object v)
         Just "OperationOutcome" -> OperationOutcomeDR <$> parseJSON (Object v)
         Just "Organization" -> OrganizationDR <$> parseJSON (Object v)
--         Just "OrganizationAffiliation" -> OrganizationAffiliationDR <$> parseJSON (Object v)
         Just "Patient" -> PatientDR <$> parseJSON (Object v)
--         Just "PaymentNotice" -> PaymentNoticeDR <$> parseJSON (Object v)
--         Just "PaymentReconciliation" -> PaymentReconciliationDR <$> parseJSON (Object v)
--         Just "Person" -> PersonDR <$> parseJSON (Object v)
         Just "PlanDefinition" -> PlanDefinitionDR <$> parseJSON (Object v)
         Just "Practitioner" -> PractitionerDR <$> parseJSON (Object v)
         Just "PractitionerRole" -> PractitionerRoleDR <$> parseJSON (Object v)
         Just "Procedure" -> ProcedureDR <$> parseJSON (Object v)
         Just "Provenance" -> ProvenanceDR <$> parseJSON (Object v)
         Just "Questionnaire" -> QuestionnaireDR <$> parseJSON (Object v)
         Just "QuestionnaireResponse" -> QuestionnaireResponseDR <$> parseJSON (Object v)
--         Just "RelatedPerson" -> RelatedPersonDR <$> parseJSON (Object v)
         Just "RequestGroup" -> RequestGroupDR <$> parseJSON (Object v)
--         Just "ResearchDefinition" -> ResearchDefinitionDR <$> parseJSON (Object v)
--         Just "ResearchElementDefinition" -> ResearchElementDefinitionDR <$> parseJSON (Object v)
--         Just "ResearchStudy" -> ResearchStudyDR <$> parseJSON (Object v)
--         Just "ResearchSubject" -> ResearchSubjectDR <$> parseJSON (Object v)
--         Just "RiskAssessment" -> RiskAssessmentDR <$> parseJSON (Object v)
--         Just "RiskEvidenceSynthesis" -> RiskEvidenceSynthesisDR <$> parseJSON (Object v)
--         Just "Schedule" -> ScheduleDR <$> parseJSON (Object v)
--         Just "SearchParameter" -> SearchParameterDR <$> parseJSON (Object v)
--         Just "ServiceRequest" -> ServiceRequestDR <$> parseJSON (Object v)
--         Just "Slot" -> SlotDR <$> parseJSON (Object v)
--         Just "Specimen" -> SpecimenDR <$> parseJSON (Object v)
--         Just "SpecimenDefinition" -> SpecimenDefinitionDR <$> parseJSON (Object v)
--         Just "StructureDefinition" -> StructureDefinitionDR <$> parseJSON (Object v)
--         Just "StructureMap" -> StructureMapDR <$> parseJSON (Object v)
--         Just "Subscription" -> SubscriptionDR <$> parseJSON (Object v)
--         Just "Substance" -> SubstanceDR <$> parseJSON (Object v)
--         Just "SubstancePolymer" -> SubstancePolymerDR <$> parseJSON (Object v)
--         Just "SubstanceProtein" -> SubstanceProteinDR <$> parseJSON (Object v)
--         Just "SubstanceReferenceInformation" -> SubstanceReferenceInformationDR <$> parseJSON (Object v)
--         Just "SubstanceSpecification" -> SubstanceSpecificationDR <$> parseJSON (Object v)
--         Just "SubstanceSourceMaterial" -> SubstanceSourceMaterialDR <$> parseJSON (Object v)
--         Just "SupplyDelivery" -> SupplyDeliveryDR <$> parseJSON (Object v)
--         Just "SupplyRequest" -> SupplyRequestDR <$> parseJSON (Object v)
         Just "Task" -> TaskDR <$> parseJSON (Object v)
--         Just "TerminologyCapabilities" -> TerminologyCapabilitiesDR <$> parseJSON (Object v)
--         Just "TestReport" -> TestReportDR <$> parseJSON (Object v)
--         Just "TestScript" -> TestScriptDR <$> parseJSON (Object v)
--         Just "ValueSet" -> ValueSetDR <$> parseJSON (Object v)
--         Just "VerificationResult" -> VerificationResultDR <$> parseJSON (Object v)
         Just "UserConfig" -> UserConfigDR <$> parseJSON (Object v)
         Just "Leave" -> LeaveDR <$> parseJSON (Object v)
         Just "ICalendar" -> ICalendarDR <$> parseJSON (Object v)
#if MIN_VERSION_aeson(2,0,0)
       where rt = AKM.lookup "resourceType" v
#else
       where rt = HM.lookup "resourceType" v
#endif

instance Xmlbf.ToXml DomainResourceC where
--  toXml (AccountDR e) = Xmlbf.toXml e
  toXml (ActivityDefinitionDR e) = Xmlbf.toXml e
--  toXml (AdverseEventDR e) = Xmlbf.toXml e
--  toXml (AllergyIntoleranceDR e) = Xmlbf.toXml e
--  toXml (AppointmentDR e) = Xmlbf.toXml e
--  toXml (AppointmentResponseDR e) = Xmlbf.toXml e
--  toXml (AuditEventDR e) = Xmlbf.toXml e
--  toXml (BasicDR e) = Xmlbf.toXml e
  toXml (BinaryDR e) = Xmlbf.toXml e
--  toXml (BiologicallyDerivedProductDR e) = Xmlbf.toXml e
--  toXml (BodyStructureDR e) = Xmlbf.toXml e
--  toXml (CapabilityStatementDR e) = Xmlbf.toXml e
  toXml (CarePlanDR e) = Xmlbf.toXml e
  toXml (CareTeamDR e) = Xmlbf.toXml e
--  toXml (CatalogEntryDR e) = Xmlbf.toXml e
--  toXml (ChargeItemDR e) = Xmlbf.toXml e
--  toXml (ChargeItemDefinitionDR e) = Xmlbf.toXml e
--  toXml (ClaimDR e) = Xmlbf.toXml e
--  toXml (ClaimResponseDR e) = Xmlbf.toXml e
--  toXml (ClinicalImpressionDR e) = Xmlbf.toXml e
--  toXml (CodeSystemDR e) = Xmlbf.toXml e
  toXml (CommunicationDR e) = Xmlbf.toXml e
--  toXml (CommunicationRequestDR e) = Xmlbf.toXml e
--  toXml (CompartmentDefinitionDR e) = Xmlbf.toXml e
  toXml (CompositionDR e) = Xmlbf.toXml e
--  toXml (ConceptMapDR e) = Xmlbf.toXml e
  toXml (ConditionDR e) = Xmlbf.toXml e
  toXml (ConsentDR e) = Xmlbf.toXml e
--  toXml (ContractDR e) = Xmlbf.toXml e
--  toXml (CoverageDR e) = Xmlbf.toXml e
--  toXml (CoverageEligibilityRequestDR e) = Xmlbf.toXml e
--  toXml (CoverageEligibilityResponseDR e) = Xmlbf.toXml e
--  toXml (DetectedIssueDR e) = Xmlbf.toXml e
  toXml (DeviceDR e) = Xmlbf.toXml e
--  toXml (DeviceDefinitionDR e) = Xmlbf.toXml e
--  toXml (DeviceMetricDR e) = Xmlbf.toXml e
--  toXml (DeviceRequestDR e) = Xmlbf.toXml e
--  toXml (DeviceUseStatementDR e) = Xmlbf.toXml e
  toXml (DiagnosticReportDR e) = Xmlbf.toXml e
--  toXml (DocumentManifestDR e) = Xmlbf.toXml e
--  toXml (DocumentReferenceDR e) = Xmlbf.toXml e
--  toXml (EffectEvidenceSynthesisDR e) = Xmlbf.toXml e
  toXml (EncounterDR e) = Xmlbf.toXml e
--  toXml (EndpointDR e) = Xmlbf.toXml e
--  toXml (EnrollmentRequestDR e) = Xmlbf.toXml e
--  toXml (EnrollmentResponseDR e) = Xmlbf.toXml e
  toXml (EpisodeOfCareDR e) = Xmlbf.toXml e
--  toXml (EventDefinitionDR e) = Xmlbf.toXml e
--  toXml (EvidenceDR e) = Xmlbf.toXml e
--  toXml (EvidenceVariableDR e) = Xmlbf.toXml e
--  toXml (ExampleScenarioDR e) = Xmlbf.toXml e
--  toXml (ExplanationOfBenefitDR e) = Xmlbf.toXml e
--  toXml (FamilyMemberHistoryDR e) = Xmlbf.toXml e
--  toXml (FlagDR e) = Xmlbf.toXml e
  toXml (GoalDR e) = Xmlbf.toXml e
--  toXml (GraphDefinitionDR e) = Xmlbf.toXml e
--  toXml (GroupDR e) = Xmlbf.toXml e
--  toXml (GuidanceResponseDR e) = Xmlbf.toXml e
  toXml (HealthcareServiceDR e) = Xmlbf.toXml e
--  toXml (ImagingStudyDR e) = Xmlbf.toXml e
--  toXml (ImmunizationDR e) = Xmlbf.toXml e
--  toXml (ImmunizationEvaluationDR e) = Xmlbf.toXml e
--  toXml (ImmunizationRecommendationDR e) = Xmlbf.toXml e
--  toXml (ImplementationGuideDR e) = Xmlbf.toXml e
--  toXml (InsurancePlanDR e) = Xmlbf.toXml e
--  toXml (InvoiceDR e) = Xmlbf.toXml e
  toXml (LibraryDR e) = Xmlbf.toXml e
--  toXml (LinkageDR e) = Xmlbf.toXml e
--  toXml (ListDR e) = Xmlbf.toXml e
  toXml (LocationDR e) = Xmlbf.toXml e
--  toXml (MeasureDR e) = Xmlbf.toXml e
--  toXml (MeasureReportDR e) = Xmlbf.toXml e
--  toXml (MediaDR e) = Xmlbf.toXml e
--  toXml (MedicationDR e) = Xmlbf.toXml e
--  toXml (MedicationAdministrationDR e) = Xmlbf.toXml e
--  toXml (MedicationDispenseDR e) = Xmlbf.toXml e
--  toXml (MedicationKnowledgeDR e) = Xmlbf.toXml e
  toXml (MedicationRequestDR e) = Xmlbf.toXml e
--  toXml (MedicationStatementDR e) = Xmlbf.toXml e
--  toXml (MedicinalProductDR e) = Xmlbf.toXml e
--  toXml (MedicinalProductAuthorizationDR e) = Xmlbf.toXml e
--  toXml (MedicinalProductContraindicationDR e) = Xmlbf.toXml e
--  toXml (MedicinalProductIndicationDR e) = Xmlbf.toXml e
--  toXml (MedicinalProductIngredientDR e) = Xmlbf.toXml e
--  toXml (MedicinalProductInteractionDR e) = Xmlbf.toXml e
--  toXml (MedicinalProductManufacturedDR e) = Xmlbf.toXml e
--  toXml (MedicinalProductPackagedDR e) = Xmlbf.toXml e
--  toXml (MedicinalProductPharmaceuticalDR e) = Xmlbf.toXml e
--  toXml (MedicinalProductUndesirableEffectDR e) = Xmlbf.toXml e
--  toXml (MessageDefinitionDR e) = Xmlbf.toXml e
--  toXml (MessageHeaderDR e) = Xmlbf.toXml e
--  toXml (MolecularSequenceDR e) = Xmlbf.toXml e
--  toXml (NamingSystemDR e) = Xmlbf.toXml e
--  toXml (NutritionOrderDR e) = Xmlbf.toXml e
  toXml (ObservationDR e) = Xmlbf.toXml e
--  toXml (ObservationDefinitionDR e) = Xmlbf.toXml e
--  toXml (OperationDefinitionDR e) = Xmlbf.toXml e
  toXml (OperationOutcomeDR e) = Xmlbf.toXml e
  toXml (OrganizationDR e) = Xmlbf.toXml e
--  toXml (OrganizationAffiliationDR e) = Xmlbf.toXml e
  toXml (PatientDR e) = Xmlbf.toXml e
--  toXml (PaymentNoticeDR e) = Xmlbf.toXml e
--  toXml (PaymentReconciliationDR e) = Xmlbf.toXml e
--  toXml (PersonDR e) = Xmlbf.toXml e
  toXml (PlanDefinitionDR e) = Xmlbf.toXml e
  toXml (PractitionerDR e) = Xmlbf.toXml e
  toXml (PractitionerRoleDR e) = Xmlbf.toXml e
  toXml (ProcedureDR e) = Xmlbf.toXml e
  toXml (ProvenanceDR e) = Xmlbf.toXml e
  toXml (QuestionnaireDR e) = Xmlbf.toXml e
  toXml (QuestionnaireResponseDR e) = Xmlbf.toXml e
--  toXml (RelatedPersonDR e) = Xmlbf.toXml e
  toXml (RequestGroupDR e) = Xmlbf.toXml e
--  toXml (ResearchDefinitionDR e) = Xmlbf.toXml e
--  toXml (ResearchElementDefinitionDR e) = Xmlbf.toXml e
--  toXml (ResearchStudyDR e) = Xmlbf.toXml e
--  toXml (ResearchSubjectDR e) = Xmlbf.toXml e
--  toXml (RiskAssessmentDR e) = Xmlbf.toXml e
--  toXml (RiskEvidenceSynthesisDR e) = Xmlbf.toXml e
--  toXml (ScheduleDR e) = Xmlbf.toXml e
--  toXml (SearchParameterDR e) = Xmlbf.toXml e
--  toXml (ServiceRequestDR e) = Xmlbf.toXml e
--  toXml (SlotDR e) = Xmlbf.toXml e
--  toXml (SpecimenDR e) = Xmlbf.toXml e
--  toXml (SpecimenDefinitionDR e) = Xmlbf.toXml e
--  toXml (StructureDefinitionDR e) = Xmlbf.toXml e
--  toXml (StructureMapDR e) = Xmlbf.toXml e
--  toXml (SubscriptionDR e) = Xmlbf.toXml e
--  toXml (SubstanceDR e) = Xmlbf.toXml e
--  toXml (SubstancePolymerDR e) = Xmlbf.toXml e
--  toXml (SubstanceProteinDR e) = Xmlbf.toXml e
--  toXml (SubstanceReferenceInformationDR e) = Xmlbf.toXml e
--  toXml (SubstanceSpecificationDR e) = Xmlbf.toXml e
--  toXml (SubstanceSourceMaterialDR e) = Xmlbf.toXml e
--  toXml (SupplyDeliveryDR e) = Xmlbf.toXml e
--  toXml (SupplyRequestDR e) = Xmlbf.toXml e
  toXml (TaskDR e) = Xmlbf.toXml e
--  toXml (TerminologyCapabilitiesDR e) = Xmlbf.toXml e
--  toXml (TestReportDR e) = Xmlbf.toXml e
--  toXml (TestScriptDR e) = Xmlbf.toXml e
--  toXml (ValueSetDR e) = Xmlbf.toXml e
--  toXml (VerificationResultDR e) = Xmlbf.toXml e
  toXml (UserConfigDR e) = Xmlbf.toXml e
  toXml (LeaveDR e) = Xmlbf.toXml e
  toXml (ICalendarDR e) = Xmlbf.toXml e
instance Xmlbf.FromXml DomainResourceC where
  fromXml = do
    r <- Xmlbf.pAnyElement fromAny 
    return r
    where fromAny = do
            n <- Xmlbf.pName
            case n of
--            "Account" -> AccountDR <$> Xmlbf.fromXml
              "ActivityDefinition" -> ActivityDefinitionDR <$> Xmlbf.fromXml
--            "AdverseEvent" -> AdverseEventDR <$> Xmlbf.fromXml
--            "AllergyIntolerance" -> AllergyIntoleranceDR <$> Xmlbf.fromXml
--            "Appointment" -> AppointmentDR <$> Xmlbf.fromXml
--            "AppointmentResponse" -> AppointmentResponseDR <$> Xmlbf.fromXml
--            "AuditEvent" -> AuditEventDR <$> Xmlbf.fromXml
--            "Basic" -> BasicDR <$> Xmlbf.fromXml
              "Binary" -> BinaryDR <$> Xmlbf.fromXml
--            "BiologicallyDerivedProduct" -> BiologicallyDerivedProductDR <$> Xmlbf.fromXml
--            "BodyStructure" -> BodyStructureDR <$> Xmlbf.fromXml
--            "CapabilityStatement" -> CapabilityStatementDR <$> Xmlbf.fromXml
              "CarePlan" -> CarePlanDR <$> Xmlbf.fromXml
              "CareTeam" -> CareTeamDR <$> Xmlbf.fromXml
--            "CatalogEntry" -> CatalogEntryDR <$> Xmlbf.fromXml
--            "ChargeItem" -> ChargeItemDR <$> Xmlbf.fromXml
--            "ChargeItemDefinition" -> ChargeItemDefinitionDR <$> Xmlbf.fromXml
--            "Claim" -> ClaimDR <$> Xmlbf.fromXml
--            "ClaimResponse" -> ClaimResponseDR <$> Xmlbf.fromXml
--            "ClinicalImpression" -> ClinicalImpressionDR <$> Xmlbf.fromXml
--            "CodeSystem" -> CodeSystemDR <$> Xmlbf.fromXml
              "Communication" -> CommunicationDR <$> Xmlbf.fromXml
--            "CommunicationRequest" -> CommunicationRequestDR <$> Xmlbf.fromXml
--            "CompartmentDefinition" -> CompartmentDefinitionDR <$> Xmlbf.fromXml
              "Composition" -> CompositionDR <$> Xmlbf.fromXml
--            "ConceptMap" -> ConceptMapDR <$> Xmlbf.fromXml
              "Condition" -> ConditionDR <$> Xmlbf.fromXml
              "Consent" -> ConsentDR <$> Xmlbf.fromXml
--            "Contract" -> ContractDR <$> Xmlbf.fromXml
--            "Coverage" -> CoverageDR <$> Xmlbf.fromXml
--            "CoverageEligibilityRequest" -> CoverageEligibilityRequestDR <$> Xmlbf.fromXml
--            "CoverageEligibilityResponse" -> CoverageEligibilityResponseDR <$> Xmlbf.fromXml
--            "DetectedIssue" -> DetectedIssueDR <$> Xmlbf.fromXml
              "Device" -> DeviceDR <$> Xmlbf.fromXml
--            "DeviceDefinition" -> DeviceDefinitionDR <$> Xmlbf.fromXml
--            "DeviceMetric" -> DeviceMetricDR <$> Xmlbf.fromXml
--            "DeviceRequest" -> DeviceRequestDR <$> Xmlbf.fromXml
--            "DeviceUseStatement" -> DeviceUseStatementDR <$> Xmlbf.fromXml
              "DiagnosticReport" -> DiagnosticReportDR <$> Xmlbf.fromXml
--            "DocumentManifest" -> DocumentManifestDR <$> Xmlbf.fromXml
--            "DocumentReference" -> DocumentReferenceDR <$> Xmlbf.fromXml
--            "EffectEvidenceSynthesis" -> EffectEvidenceSynthesisDR <$> Xmlbf.fromXml
              "Encounter" -> EncounterDR <$> Xmlbf.fromXml
--            "Endpoint" -> EndpointDR <$> Xmlbf.fromXml
--            "EnrollmentRequest" -> EnrollmentRequestDR <$> Xmlbf.fromXml
--            "EnrollmentResponse" -> EnrollmentResponseDR <$> Xmlbf.fromXml
              "EpisodeOfCare" -> EpisodeOfCareDR <$> Xmlbf.fromXml
--            "EventDefinition" -> EventDefinitionDR <$> Xmlbf.fromXml
--            "Evidence" -> EvidenceDR <$> Xmlbf.fromXml
--            "EvidenceVariable" -> EvidenceVariableDR <$> Xmlbf.fromXml
--            "ExampleScenario" -> ExampleScenarioDR <$> Xmlbf.fromXml
--            "ExplanationOfBenefit" -> ExplanationOfBenefitDR <$> Xmlbf.fromXml
--            "FamilyMemberHistory" -> FamilyMemberHistoryDR <$> Xmlbf.fromXml
--            "Flag" -> FlagDR <$> Xmlbf.fromXml
              "Goal" -> GoalDR <$> Xmlbf.fromXml
--            "GraphDefinition" -> GraphDefinitionDR <$> Xmlbf.fromXml
--            "Group" -> GroupDR <$> Xmlbf.fromXml
--            "GuidanceResponse" -> GuidanceResponseDR <$> Xmlbf.fromXml
              "HealthcareService" -> HealthcareServiceDR <$> Xmlbf.fromXml
--            "ImagingStudy" -> ImagingStudyDR <$> Xmlbf.fromXml
--            "Immunization" -> ImmunizationDR <$> Xmlbf.fromXml
--            "ImmunizationEvaluation" -> ImmunizationEvaluationDR <$> Xmlbf.fromXml
--            "ImmunizationRecommendation" -> ImmunizationRecommendationDR <$> Xmlbf.fromXml
--            "ImplementationGuide" -> ImplementationGuideDR <$> Xmlbf.fromXml
--            "InsurancePlan" -> InsurancePlanDR <$> Xmlbf.fromXml
--            "Invoice" -> InvoiceDR <$> Xmlbf.fromXml
              "Library" -> LibraryDR <$> Xmlbf.fromXml
--            "Linkage" -> LinkageDR <$> Xmlbf.fromXml
--            "List" -> ListDR <$> Xmlbf.fromXml
              "Location" -> LocationDR <$> Xmlbf.fromXml
--            "Measure" -> MeasureDR <$> Xmlbf.fromXml
--            "MeasureReport" -> MeasureReportDR <$> Xmlbf.fromXml
--            "Media" -> MediaDR <$> Xmlbf.fromXml
--            "Medication" -> MedicationDR <$> Xmlbf.fromXml
--            "MedicationAdministration" -> MedicationAdministrationDR <$> Xmlbf.fromXml
--            "MedicationDispense" -> MedicationDispenseDR <$> Xmlbf.fromXml
--            "MedicationKnowledge" -> MedicationKnowledgeDR <$> Xmlbf.fromXml
              "MedicationRequest" -> MedicationRequestDR <$> Xmlbf.fromXml
--            "MedicationStatement" -> MedicationStatementDR <$> Xmlbf.fromXml
--            "MedicinalProduct" -> MedicinalProductDR <$> Xmlbf.fromXml
--            "MedicinalProductAuthorization" -> MedicinalProductAuthorizationDR <$> Xmlbf.fromXml
--            "MedicinalProductContraindication" -> MedicinalProductContraindicationDR <$> Xmlbf.fromXml
--            "MedicinalProductIndication" -> MedicinalProductIndicationDR <$> Xmlbf.fromXml
--            "MedicinalProductIngredient" -> MedicinalProductIngredientDR <$> Xmlbf.fromXml
--            "MedicinalProductInteraction" -> MedicinalProductInteractionDR <$> Xmlbf.fromXml
--            "MedicinalProductManufactured" -> MedicinalProductManufacturedDR <$> Xmlbf.fromXml
--            "MedicinalProductPackaged" -> MedicinalProductPackagedDR <$> Xmlbf.fromXml
--            "MedicinalProductPharmaceutical" -> MedicinalProductPharmaceuticalDR <$> Xmlbf.fromXml
--            "MedicinalProductUndesirableEffect" -> MedicinalProductUndesirableEffectDR <$> Xmlbf.fromXml
--            "MessageDefinition" -> MessageDefinitionDR <$> Xmlbf.fromXml
--            "MessageHeader" -> MessageHeaderDR <$> Xmlbf.fromXml
--            "MolecularSequence" -> MolecularSequenceDR <$> Xmlbf.fromXml
--            "NamingSystem" -> NamingSystemDR <$> Xmlbf.fromXml
--            "NutritionOrder" -> NutritionOrderDR <$> Xmlbf.fromXml
              "Observation" -> ObservationDR <$> Xmlbf.fromXml
--            "ObservationDefinition" -> ObservationDefinitionDR <$> Xmlbf.fromXml
--            "OperationDefinition" -> OperationDefinitionDR <$> Xmlbf.fromXml
--            "OperationOutcome" -> OperationOutcomeDR <$> Xmlbf.fromXml
              "Organization" -> OrganizationDR <$> Xmlbf.fromXml
--            "OrganizationAffiliation" -> OrganizationAffiliationDR <$> Xmlbf.fromXml
              "Patient" -> PatientDR <$> Xmlbf.fromXml
--            "PaymentNotice" -> PaymentNoticeDR <$> Xmlbf.fromXml
--            "PaymentReconciliation" -> PaymentReconciliationDR <$> Xmlbf.fromXml
--            "Person" -> PersonDR <$> Xmlbf.fromXml
              "PlanDefinition" -> PlanDefinitionDR <$> Xmlbf.fromXml
              "Practitioner" -> PractitionerDR <$> Xmlbf.fromXml
              "PractitionerRole" -> PractitionerRoleDR <$> Xmlbf.fromXml
              "Procedure" -> ProcedureDR <$> Xmlbf.fromXml
              "Provenance" -> ProvenanceDR <$> Xmlbf.fromXml
              "Questionnaire" -> QuestionnaireDR <$> Xmlbf.fromXml
              "QuestionnaireResponse" -> QuestionnaireResponseDR <$> Xmlbf.fromXml
--            "RelatedPerson" -> RelatedPersonDR <$> Xmlbf.fromXml
              "RequestGroup" -> RequestGroupDR <$> Xmlbf.fromXml
--            "ResearchDefinition" -> ResearchDefinitionDR <$> Xmlbf.fromXml
--            "ResearchElementDefinition" -> ResearchElementDefinitionDR <$> Xmlbf.fromXml
--            "ResearchStudy" -> ResearchStudyDR <$> Xmlbf.fromXml
--            "ResearchSubject" -> ResearchSubjectDR <$> Xmlbf.fromXml
--            "RiskAssessment" -> RiskAssessmentDR <$> Xmlbf.fromXml
--            "RiskEvidenceSynthesis" -> RiskEvidenceSynthesisDR <$> Xmlbf.fromXml
--            "Schedule" -> ScheduleDR <$> Xmlbf.fromXml
--            "SearchParameter" -> SearchParameterDR <$> Xmlbf.fromXml
--            "ServiceRequest" -> ServiceRequestDR <$> Xmlbf.fromXml
--            "Slot" -> SlotDR <$> Xmlbf.fromXml
--            "Specimen" -> SpecimenDR <$> Xmlbf.fromXml
--            "SpecimenDefinition" -> SpecimenDefinitionDR <$> Xmlbf.fromXml
--            "StructureDefinition" -> StructureDefinitionDR <$> Xmlbf.fromXml
--            "StructureMap" -> StructureMapDR <$> Xmlbf.fromXml
--            "Subscription" -> SubscriptionDR <$> Xmlbf.fromXml
--            "Substance" -> SubstanceDR <$> Xmlbf.fromXml
--            "SubstancePolymer" -> SubstancePolymerDR <$> Xmlbf.fromXml
--            "SubstanceProtein" -> SubstanceProteinDR <$> Xmlbf.fromXml
--            "SubstanceReferenceInformation" -> SubstanceReferenceInformationDR <$> Xmlbf.fromXml
--            "SubstanceSpecification" -> SubstanceSpecificationDR <$> Xmlbf.fromXml
--            "SubstanceSourceMaterial" -> SubstanceSourceMaterialDR <$> Xmlbf.fromXml
--            "SupplyDelivery" -> SupplyDeliveryDR <$> Xmlbf.fromXml
--            "SupplyRequest" -> SupplyRequestDR <$> Xmlbf.fromXml
              "Task" -> TaskDR <$> Xmlbf.fromXml
--            "TerminologyCapabilities" -> TerminologyCapabilitiesDR <$> Xmlbf.fromXml
--            "TestReport" -> TestReportDR <$> Xmlbf.fromXml
--            "TestScript" -> TestScriptDR <$> Xmlbf.fromXml
--            "ValueSet" -> ValueSetDR <$> Xmlbf.fromXml
--            "VerificationResult" -> VerificationResultDR <$> Xmlbf.fromXml
              "UserConfig" -> UserConfigDR <$> Xmlbf.fromXml
              "Leave" -> LeaveDR <$> Xmlbf.fromXml
              "ICalendar" -> ICalendarDR <$> Xmlbf.fromXml

-- Bundle, Parameters are not DomainResources

--  | AccountDR Account
--  | ActivityDefinitionDR ActivityDefinition
--  | AdverseEventDR AdverseEvent
--  | AllergyIntoleranceDR AllergyIntolerance
--  | AppointmentDR Appointment
--  | AppointmentResponseDR AppointmentResponse
--  | AuditEventDR AuditEvent
--  | BasicDR Basic
--  | BinaryDR Binary
--  | BiologicallyDerivedProductDR BiologicallyDerivedProduct
--  | BodyStructureDR BodyStructure
--  | CapabilityStatementDR CapabilityStatement
--  | CarePlanDR CarePlan
--  | CareTeamDR CareTeam
--  | CatalogEntryDR CatalogEntry
--  | ChargeItemDR ChargeItem
--  | ChargeItemDefinitionDR ChargeItemDefinition
--  | ClaimDR Claim
--  | ClaimResponseDR ClaimResponse
--  | ClinicalImpressionDR ClinicalImpression
--  | CodeSystemDR CodeSystem
--  | CommunicationDR Communication
--  | CommunicationRequestDR CommunicationRequest
--  | CompartmentDefinitionDR CompartmentDefinition
--  | CompositionDR Composition
--  | ConceptMapDR ConceptMap
--  | ConditionDR Condition
--  | ConsentDR Consent
--  | ContractDR Contract
--  | CoverageDR Coverage
--  | CoverageEligibilityRequestDR CoverageEligibilityRequest
--  | CoverageEligibilityResponseDR CoverageEligibilityResponse
--  | DetectedIssueDR DetectedIssue
--  | DeviceDR Device
--  | DeviceDefinitionDR DeviceDefinition
--  | DeviceMetricDR DeviceMetric
--  | DeviceRequestDR DeviceRequest
--  | DeviceUseStatementDR DeviceUseStatement
--  | DiagnosticReportDR DiagnosticReport
--  | DocumentManifestDR DocumentManifest
--  | DocumentReferenceDR DocumentReference
--  | EffectEvidenceSynthesisDR EffectEvidenceSynthesis
--  | EncounterDR Encounter
--  | EndpointDR Endpoint
--  | EnrollmentRequestDR EnrollmentRequest
--  | EnrollmentResponseDR EnrollmentResponse
--  | EpisodeOfCareDR EpisodeOfCare
--  | EventDefinitionDR EventDefinition
--  | EvidenceDR Evidence
--  | EvidenceVariableDR EvidenceVariable
--  | ExampleScenarioDR ExampleScenario
--  | ExplanationOfBenefitDR ExplanationOfBenefit
--  | FamilyMemberHistoryDR FamilyMemberHistory
--  | FlagDR Flag
--  | GoalDR Goal
--  | GraphDefinitionDR GraphDefinition
--  | GroupDR Group
--  | GuidanceResponseDR GuidanceResponse
--  | HealthcareServiceDR HealthcareService
--  | ImagingStudyDR ImagingStudy
--  | ImmunizationDR Immunization
--  | ImmunizationEvaluationDR ImmunizationEvaluation
--  | ImmunizationRecommendationDR ImmunizationRecommendation
--  | ImplementationGuideDR ImplementationGuide
--  | InsurancePlanDR InsurancePlan
--  | InvoiceDR Invoice
--  | LibraryDR Library
--  | LinkageDR Linkage
--  | ListDR List
--  | LocationDR Location
--  | MeasureDR Measure
--  | MeasureReportDR MeasureReport
--  | MediaDR Media
--  | MedicationDR Medication
--  | MedicationAdministrationDR MedicationAdministration
--  | MedicationDispenseDR MedicationDispense
--  | MedicationKnowledgeDR MedicationKnowledge
--  | MedicationRequestDR MedicationRequest
--  | MedicationStatementDR MedicationStatement
--  | MedicinalProductDR MedicinalProduct
--  | MedicinalProductAuthorizationDR MedicinalProductAuthorization
--  | MedicinalProductContraindicationDR MedicinalProductContraindication
--  | MedicinalProductIndicationDR MedicinalProductIndication
--  | MedicinalProductIngredientDR MedicinalProductIngredient
--  | MedicinalProductInteractionDR MedicinalProductInteraction
--  | MedicinalProductManufacturedDR MedicinalProductManufactured
--  | MedicinalProductPackagedDR MedicinalProductPackaged
--  | MedicinalProductPharmaceuticalDR MedicinalProductPharmaceutical
--  | MedicinalProductUndesirableEffectDR MedicinalProductUndesirableEffect
--  | MessageDefinitionDR MessageDefinition
--  | MessageHeaderDR MessageHeader
--  | MolecularSequenceDR MolecularSequence
--  | NamingSystemDR NamingSystem
--  | NutritionOrderDR NutritionOrder
--  | ObservationDR Observation
--  | ObservationDefinitionDR ObservationDefinition
--  | OperationDefinitionDR OperationDefinition
--  | OperationOutcomeDR OperationOutcome
--  | OrganizationDR Organization
--  | OrganizationAffiliationDR OrganizationAffiliation
--  | PatientDR Patient
--  | PaymentNoticeDR PaymentNotice
--  | PaymentReconciliationDR PaymentReconciliation
--  | PersonDR Person
--  | PlanDefinitionDR PlanDefinition
--  | PractitionerDR Practitioner
--  | PractitionerRoleDR PractitionerRole
--  | ProcedureDR Procedure
--  | ProvenanceDR Provenance
--  | QuestionnaireDR Questionnaire
--  | QuestionnaireResponseDR QuestionnaireResponse
--  | RelatedPersonDR RelatedPerson
--  | RequestGroupDR RequestGroup
--  | ResearchDefinitionDR ResearchDefinition
--  | ResearchElementDefinitionDR ResearchElementDefinition
--  | ResearchStudyDR ResearchStudy
--  | ResearchSubjectDR ResearchSubject
--  | RiskAssessmentDR RiskAssessment
--  | RiskEvidenceSynthesisDR RiskEvidenceSynthesis
--  | ScheduleDR Schedule
--  | SearchParameterDR SearchParameter
--  | ServiceRequestDR ServiceRequest
--  | SlotDR Slot
--  | SpecimenDR Specimen
--  | SpecimenDefinitionDR SpecimenDefinition
--  | StructureDefinitionDR StructureDefinition
--  | StructureMapDR StructureMap
--  | SubscriptionDR Subscription
--  | SubstanceDR Substance
--  | SubstancePolymerDR SubstancePolymer
--  | SubstanceProteinDR SubstanceProtein
--  | SubstanceReferenceInformationDR SubstanceReferenceInformation
--  | SubstanceSpecificationDR SubstanceSpecification
--  | SubstanceSourceMaterialDR SubstanceSourceMaterial
--  | SupplyDeliveryDR SupplyDelivery
--  | SupplyRequestDR SupplyRequest
--  | TaskDR Task
--  | TerminologyCapabilitiesDR TerminologyCapabilities
--  | TestReportDR TestReport
--  | TestScriptDR TestScript
--  | ValueSetDR ValueSet
--  | VerificationResultDR VerificationResult
--  | UserConfigDR UserConfig
--  | LeaveDR 
--  | ICalendarDR ICalendar
