{-# LANGUAGE NoImplicitPrelude #-}

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

module Data.FHIR.Resources (
    Resource(..)
  , mkBundle
  , mkCapabilityStatement
  , mkEncounter
  , mkOperationOutcome
  , mkPatient
  , mkPatientLink
  , mkPatientContact
  , mkPatientComm
  , module Data.FHIR.Resources.Bundle
  , module Data.FHIR.Resources.ResourceContainer
--  , module Data.FHIR.Resources.Resource -- empty
  ) where

import Data.Aeson  
import Data.Aeson.Types hiding (parseJSON)

--import GHC.Generics
import GHC.TypeLits

import RIO
import qualified RIO.HashMap as HM
import qualified RIO.Vector as V

import           Data.FHIR.Datatypes
import           Data.FHIR.Datatypes.XML
import           Data.FHIR.Datatypes.XmlUtils
import Data.FHIR.Resources.Bundle
--import Data.FHIR.Resources.Parameters
import Data.FHIR.Resources.ResourceContainer

import qualified Xmlbf  as Xmlbf

data ResourceN = ResourceN (Maybe Id) (Maybe Meta) (Maybe Uri) (Maybe Language) Resource
    deriving (Eq, Show)

instance ToJSON ResourceN where
  toJSON (ResourceN i m ir l r) = object $
    filter (\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $
    [
      rt
    , "id" .= toJSON i 
    , "meta" .= toJSON m 
    , "implicitRules" .= toJSON ir 
    , "language" .= toJSON l
    ]
    ++ rest
    where (Object v) = toJSON r
          rt:rest    = HM.toList v
instance FromJSON ResourceN where
  parseJSON = withObject "ResourceN" $ \o -> do
      i  <- o .:? "id"
      m  <- o .:? "meta"
      ir <- o .:? "implicitRules"
      l  <- o .:? "language"
      r  <- parseJSON (Object o)
      return $ ResourceN i m ir l r



data Resource
--  = AccountR Account
  = ActivityDefinitionR ActivityDefinition
--  | AdverseEventR AdverseEvent
--  | AllergyIntoleranceR AllergyIntolerance
--  | AppointmentR Appointment
--  | AppointmentResponseR AppointmentResponse
--  | AuditEventR AuditEvent
--  | BasicR Basic
  | BinaryR Binary
--  | BiologicallyDerivedProductR BiologicallyDerivedProduct
--  | BodyStructureR BodyStructure
  | BundleR Bundle
  | CapabilityStatementR CapabilityStatement
--  | CarePlanR CarePlan
--  | CareTeamR CareTeam
--  | CatalogEntryR CatalogEntry
--  | ChargeItemR ChargeItem
--  | ChargeItemDefinitionR ChargeItemDefinition
--  | ClaimR Claim
--  | ClaimResponseR ClaimResponse
--  | ClinicalImpressionR ClinicalImpression
--  | CodeSystemR CodeSystem
--  | CommunicationR Communication
--  | CommunicationRequestR CommunicationRequest
--  | CompartmentDefinitionR CompartmentDefinition
--  | CompositionR Composition
--  | ConceptMapR ConceptMap
--  | ConditionR Condition
--  | ConsentR Consent
--  | ContractR Contract
--  | CoverageR Coverage
--  | CoverageEligibilityRequestR CoverageEligibilityRequest
--  | CoverageEligibilityResponseR CoverageEligibilityResponse
--  | DetectedIssueR DetectedIssue
--  | DeviceR Device
--  | DeviceDefinitionR DeviceDefinition
--  | DeviceMetricR DeviceMetric
--  | DeviceRequestR DeviceRequest
--  | DeviceUseStatementR DeviceUseStatement
--  | DiagnosticReportR DiagnosticReport
--  | DocumentManifestR DocumentManifest
--  | DocumentReferenceR DocumentReference
--  | EffectEvidenceSynthesisR EffectEvidenceSynthesis
  | EncounterR Encounter
--  | EndpointR Endpoint
--  | EnrollmentRequestR EnrollmentRequest
--  | EnrollmentResponseR EnrollmentResponse
--  | EpisodeOfCareR EpisodeOfCare
--  | EventDefinitionR EventDefinition
--  | EvidenceR Evidence
--  | EvidenceVariableR EvidenceVariable
--  | ExampleScenarioR ExampleScenario
--  | ExplanationOfBenefitR ExplanationOfBenefit
--  | FamilyMemberHistoryR FamilyMemberHistory
--  | FlagR Flag
--  | GoalR Goal
--  | GraphDefinitionR GraphDefinition
--  | GroupR Group
--  | GuidanceResponseR GuidanceResponse
--  | HealthcareServiceR HealthcareService
--  | ImagingStudyR ImagingStudy
--  | ImmunizationR Immunization
--  | ImmunizationEvaluationR ImmunizationEvaluation
--  | ImmunizationRecommendationR ImmunizationRecommendation
--  | ImplementationGuideR ImplementationGuide
--  | InsurancePlanR InsurancePlan
--  | InvoiceR Invoice
--  | LibraryR Library
--  | LinkageR Linkage
--  | ListR List
--  | LocationR Location
--  | MeasureR Measure
--  | MeasureReportR MeasureReport
--  | MediaR Media
--  | MedicationR Medication
--  | MedicationAdministrationR MedicationAdministration
--  | MedicationDispenseR MedicationDispense
--  | MedicationKnowledgeR MedicationKnowledge
--  | MedicationRequestR MedicationRequest
--  | MedicationStatementR MedicationStatement
--  | MedicinalProductR MedicinalProduct
--  | MedicinalProductAuthorizationR MedicinalProductAuthorization
--  | MedicinalProductContraindicationR MedicinalProductContraindication
--  | MedicinalProductIndicationR MedicinalProductIndication
--  | MedicinalProductIngredientR MedicinalProductIngredient
--  | MedicinalProductInteractionR MedicinalProductInteraction
--  | MedicinalProductManufacturedR MedicinalProductManufactured
--  | MedicinalProductPackagedR MedicinalProductPackaged
--  | MedicinalProductPharmaceuticalR MedicinalProductPharmaceutical
--  | MedicinalProductUndesirableEffectR MedicinalProductUndesirableEffect
--  | MessageDefinitionR MessageDefinition
--  | MessageHeaderR MessageHeader
--  | MolecularSequenceR MolecularSequence
--  | NamingSystemR NamingSystem
--  | NutritionOrderR NutritionOrder
--  | ObservationR Observation
--  | ObservationDefinitionR ObservationDefinition
--  | OperationDefinitionR OperationDefinition
  | OperationOutcomeR OperationOutcome
--  | OrganizationR Organization
--  | OrganizationAffiliationR OrganizationAffiliation
--  | ParametersR Parameters
  | PatientR Patient
--  | PaymentNoticeR PaymentNotice
--  | PaymentReconciliationR PaymentReconciliation
--  | PersonR Person
--  | PlanDefinitionR PlanDefinition
--  | PractitionerR Practitioner
--  | PractitionerRoleR PractitionerRole
--  | ProcedureR Procedure
--  | ProvenanceR Provenance
--  | QuestionnaireR Questionnaire
--  | QuestionnaireResponseR QuestionnaireResponse
--  | RelatedPersonR RelatedPerson
--  | RequestGroupR RequestGroup
--  | ResearchDefinitionR ResearchDefinition
--  | ResearchElementDefinitionR ResearchElementDefinition
--  | ResearchStudyR ResearchStudy
--  | ResearchSubjectR ResearchSubject
--  | RiskAssessmentR RiskAssessment
--  | RiskEvidenceSynthesisR RiskEvidenceSynthesis
--  | ScheduleR Schedule
--  | SearchParameterR SearchParameter
--  | ServiceRequestR ServiceRequest
--  | SlotR Slot
--  | SpecimenR Specimen
--  | SpecimenDefinitionR SpecimenDefinition
--  | StructureDefinitionR StructureDefinition
--  | StructureMapR StructureMap
--  | SubscriptionR Subscription
--  | SubstanceR Substance
--  | SubstancePolymerR SubstancePolymer
--  | SubstanceProteinR SubstanceProtein
--  | SubstanceReferenceInformationR SubstanceReferenceInformation
--  | SubstanceSpecificationR SubstanceSpecification
--  | SubstanceSourceMaterialR SubstanceSourceMaterial
--  | SupplyDeliveryR SupplyDelivery
--  | SupplyRequestR SupplyRequest
--  | TaskR Task
--  | TerminologyCapabilitiesR TerminologyCapabilities
--  | TestReportR TestReport
--  | TestScriptR TestScript
--  | ValueSetR ValueSet
--  | VerificationResultR VerificationResult
  deriving (Eq, Show)
 
instance ToJSON Resource where
  toJSON (BundleR b) = toJSON b
  toJSON (BinaryR b) = toJSON b
-- DomainResources
--  toJSON (AccountR e) = toJSON e
  toJSON (ActivityDefinitionR e) = toJSON e
--  toJSON (AdverseEventR e) = toJSON e
--  toJSON (AllergyIntoleranceR e) = toJSON e
--  toJSON (AppointmentR e) = toJSON e
--  toJSON (AppointmentResponseR e) = toJSON e
--  toJSON (AuditEventR e) = toJSON e
--  toJSON (BasicR e) = toJSON e
--  toJSON (BiologicallyDerivedProductR e) = toJSON e
--  toJSON (BodyStructureR e) = toJSON e
  toJSON (CapabilityStatementR e) = toJSON e
--  toJSON (CarePlanR e) = toJSON e
--  toJSON (CareTeamR e) = toJSON e
--  toJSON (CatalogEntryR e) = toJSON e
--  toJSON (ChargeItemR e) = toJSON e
--  toJSON (ChargeItemDefinitionR e) = toJSON e
--  toJSON (ClaimR e) = toJSON e
--  toJSON (ClaimResponseR e) = toJSON e
--  toJSON (ClinicalImpressionR e) = toJSON e
--  toJSON (CodeSystemR e) = toJSON e
--  toJSON (CommunicationR e) = toJSON e
--  toJSON (CommunicationRequestR e) = toJSON e
--  toJSON (CompartmentDefinitionR e) = toJSON e
--  toJSON (CompositionR e) = toJSON e
--  toJSON (ConceptMapR e) = toJSON e
--  toJSON (ConditionR e) = toJSON e
--  toJSON (ConsentR e) = toJSON e
--  toJSON (ContractR e) = toJSON e
--  toJSON (CoverageR e) = toJSON e
--  toJSON (CoverageEligibilityRequestR e) = toJSON e
--  toJSON (CoverageEligibilityResponseR e) = toJSON e
--  toJSON (DetectedIssueR e) = toJSON e
--  toJSON (DeviceR e) = toJSON e
--  toJSON (DeviceDefinitionR e) = toJSON e
--  toJSON (DeviceMetricR e) = toJSON e
--  toJSON (DeviceRequestR e) = toJSON e
--  toJSON (DeviceUseStatementR e) = toJSON e
--  toJSON (DiagnosticReportR e) = toJSON e
--  toJSON (DocumentManifestR e) = toJSON e
--  toJSON (DocumentReferenceR e) = toJSON e
--  toJSON (EffectEvidenceSynthesisR e) = toJSON e
  toJSON (EncounterR e) = toJSON e
--  toJSON (EndpointR e) = toJSON e
--  toJSON (EnrollmentRequestR e) = toJSON e
--  toJSON (EnrollmentResponseR e) = toJSON e
--  toJSON (EpisodeOfCareR e) = toJSON e
--  toJSON (EventDefinitionR e) = toJSON e
--  toJSON (EvidenceR e) = toJSON e
--  toJSON (EvidenceVariableR e) = toJSON e
--  toJSON (ExampleScenarioR e) = toJSON e
--  toJSON (ExplanationOfBenefitR e) = toJSON e
--  toJSON (FamilyMemberHistoryR e) = toJSON e
--  toJSON (FlagR e) = toJSON e
--  toJSON (GoalR e) = toJSON e
--  toJSON (GraphDefinitionR e) = toJSON e
--  toJSON (GroupR e) = toJSON e
--  toJSON (GuidanceResponseR e) = toJSON e
--  toJSON (HealthcareServiceR e) = toJSON e
--  toJSON (ImagingStudyR e) = toJSON e
--  toJSON (ImmunizationR e) = toJSON e
--  toJSON (ImmunizationEvaluationR e) = toJSON e
--  toJSON (ImmunizationRecommendationR e) = toJSON e
--  toJSON (ImplementationGuideR e) = toJSON e
--  toJSON (InsurancePlanR e) = toJSON e
--  toJSON (InvoiceR e) = toJSON e
--  toJSON (LibraryR e) = toJSON e
--  toJSON (LinkageR e) = toJSON e
--  toJSON (ListR e) = toJSON e
--  toJSON (LocationR e) = toJSON e
--  toJSON (MeasureR e) = toJSON e
--  toJSON (MeasureReportR e) = toJSON e
--  toJSON (MediaR e) = toJSON e
--  toJSON (MedicationR e) = toJSON e
--  toJSON (MedicationAdministrationR e) = toJSON e
--  toJSON (MedicationDispenseR e) = toJSON e
--  toJSON (MedicationKnowledgeR e) = toJSON e
--  toJSON (MedicationRequestR e) = toJSON e
--  toJSON (MedicationStatementR e) = toJSON e
--  toJSON (MedicinalProductR e) = toJSON e
--  toJSON (MedicinalProductAuthorizationR e) = toJSON e
--  toJSON (MedicinalProductContraindicationR e) = toJSON e
--  toJSON (MedicinalProductIndicationR e) = toJSON e
--  toJSON (MedicinalProductIngredientR e) = toJSON e
--  toJSON (MedicinalProductInteractionR e) = toJSON e
--  toJSON (MedicinalProductManufacturedR e) = toJSON e
--  toJSON (MedicinalProductPackagedR e) = toJSON e
--  toJSON (MedicinalProductPharmaceuticalR e) = toJSON e
--  toJSON (MedicinalProductUndesirableEffectR e) = toJSON e
--  toJSON (MessageDefinitionR e) = toJSON e
--  toJSON (MessageHeaderR e) = toJSON e
--  toJSON (MolecularSequenceR e) = toJSON e
--  toJSON (NamingSystemR e) = toJSON e
--  toJSON (NutritionOrderR e) = toJSON e
--  toJSON (ObservationR e) = toJSON e
--  toJSON (ObservationDefinitionR e) = toJSON e
--  toJSON (OperationDefinitionR e) = toJSON e
  toJSON (OperationOutcomeR e) = toJSON e
--  toJSON (OrganizationR e) = toJSON e
--  toJSON (OrganizationAffiliationR e) = toJSON e
  toJSON (PatientR e) = toJSON e
--  toJSON (PaymentNoticeR e) = toJSON e
--  toJSON (PaymentReconciliationR e) = toJSON e
--  toJSON (PersonR e) = toJSON e
--  toJSON (PlanDefinitionR e) = toJSON e
--  toJSON (PractitionerR e) = toJSON e
--  toJSON (PractitionerRoleR e) = toJSON e
--  toJSON (ProcedureR e) = toJSON e
--  toJSON (ProvenanceR e) = toJSON e
--  toJSON (QuestionnaireR e) = toJSON e
--  toJSON (QuestionnaireResponseR e) = toJSON e
--  toJSON (RelatedPersonR e) = toJSON e
--  toJSON (RequestGroupR e) = toJSON e
--  toJSON (ResearchDefinitionR e) = toJSON e
--  toJSON (ResearchElementDefinitionR e) = toJSON e
--  toJSON (ResearchStudyR e) = toJSON e
--  toJSON (ResearchSubjectR e) = toJSON e
--  toJSON (RiskAssessmentR e) = toJSON e
--  toJSON (RiskEvidenceSynthesisR e) = toJSON e
--  toJSON (ScheduleR e) = toJSON e
--  toJSON (SearchParameterR e) = toJSON e
--  toJSON (ServiceRequestR e) = toJSON e
--  toJSON (SlotR e) = toJSON e
--  toJSON (SpecimenR e) = toJSON e
--  toJSON (SpecimenDefinitionR e) = toJSON e
--  toJSON (StructureDefinitionR e) = toJSON e
--  toJSON (StructureMapR e) = toJSON e
--  toJSON (SubscriptionR e) = toJSON e
--  toJSON (SubstanceR e) = toJSON e
--  toJSON (SubstancePolymerR e) = toJSON e
--  toJSON (SubstanceProteinR e) = toJSON e
--  toJSON (SubstanceReferenceInformationR e) = toJSON e
--  toJSON (SubstanceSpecificationR e) = toJSON e
--  toJSON (SubstanceSourceMaterialR e) = toJSON e
--  toJSON (SupplyDeliveryR e) = toJSON e
--  toJSON (SupplyRequestR e) = toJSON e
--  toJSON (TaskR e) = toJSON e
--  toJSON (TerminologyCapabilitiesR e) = toJSON e
--  toJSON (TestReportR e) = toJSON e
--  toJSON (TestScriptR e) = toJSON e
--  toJSON (ValueSetR e) = toJSON e
--  toJSON (VerificationResultR e) = toJSON e
instance FromJSON Resource where
  parseJSON (Object v) = do
      case rt of
         Just "Binary" -> BinaryR <$> parseJSON (Object v)
         Just "Bundle"  ->    BundleR    <$> parseJSON (Object v)
--DomainResources
--         Just "Account" -> AccountR <$> parseJSON (Object v)
         Just "ActivityDefinition" -> ActivityDefinitionR <$> parseJSON (Object v)
--         Just "AdverseEvent" -> AdverseEventR <$> parseJSON (Object v)
--         Just "AllergyIntolerance" -> AllergyIntoleranceR <$> parseJSON (Object v)
--         Just "Appointment" -> AppointmentR <$> parseJSON (Object v)
--         Just "AppointmentResponse" -> AppointmentResponseR <$> parseJSON (Object v)
--         Just "AuditEvent" -> AuditEventR <$> parseJSON (Object v)
--         Just "Basic" -> BasicR <$> parseJSON (Object v)
--         Just "BiologicallyDerivedProduct" -> BiologicallyDerivedProductR <$> parseJSON (Object v)
--         Just "BodyStructure" -> BodyStructureR <$> parseJSON (Object v)
--         Just "CapabilityStatement" -> CapabilityStatementR <$> parseJSON (Object v)
--         Just "CarePlan" -> CarePlanR <$> parseJSON (Object v)
--         Just "CareTeam" -> CareTeamR <$> parseJSON (Object v)
--         Just "CatalogEntry" -> CatalogEntryR <$> parseJSON (Object v)
--         Just "ChargeItem" -> ChargeItemR <$> parseJSON (Object v)
--         Just "ChargeItemDefinition" -> ChargeItemDefinitionR <$> parseJSON (Object v)
--         Just "Claim" -> ClaimR <$> parseJSON (Object v)
--         Just "ClaimResponse" -> ClaimResponseR <$> parseJSON (Object v)
--         Just "ClinicalImpression" -> ClinicalImpressionR <$> parseJSON (Object v)
--         Just "CodeSystem" -> CodeSystemR <$> parseJSON (Object v)
--         Just "Communication" -> CommunicationR <$> parseJSON (Object v)
--         Just "CommunicationRequest" -> CommunicationRequestR <$> parseJSON (Object v)
--         Just "CompartmentDefinition" -> CompartmentDefinitionR <$> parseJSON (Object v)
--         Just "Composition" -> CompositionR <$> parseJSON (Object v)
--         Just "ConceptMap" -> ConceptMapR <$> parseJSON (Object v)
--         Just "Condition" -> ConditionR <$> parseJSON (Object v)
--         Just "Consent" -> ConsentR <$> parseJSON (Object v)
--         Just "Contract" -> ContractR <$> parseJSON (Object v)
--         Just "Coverage" -> CoverageR <$> parseJSON (Object v)
--         Just "CoverageEligibilityRequest" -> CoverageEligibilityRequestR <$> parseJSON (Object v)
--         Just "CoverageEligibilityResponse" -> CoverageEligibilityResponseR <$> parseJSON (Object v)
--         Just "DetectedIssue" -> DetectedIssueR <$> parseJSON (Object v)
--         Just "Device" -> DeviceR <$> parseJSON (Object v)
--         Just "DeviceDefinition" -> DeviceDefinitionR <$> parseJSON (Object v)
--         Just "DeviceMetric" -> DeviceMetricR <$> parseJSON (Object v)
--         Just "DeviceRequest" -> DeviceRequestR <$> parseJSON (Object v)
--         Just "DeviceUseStatement" -> DeviceUseStatementR <$> parseJSON (Object v)
--         Just "DiagnosticReport" -> DiagnosticReportR <$> parseJSON (Object v)
--         Just "DocumentManifest" -> DocumentManifestR <$> parseJSON (Object v)
--         Just "DocumentReference" -> DocumentReferenceR <$> parseJSON (Object v)
--         Just "EffectEvidenceSynthesis" -> EffectEvidenceSynthesisR <$> parseJSON (Object v)
         Just "Encounter" -> EncounterR <$> parseJSON (Object v)
--         Just "Endpoint" -> EndpointR <$> parseJSON (Object v)
--         Just "EnrollmentRequest" -> EnrollmentRequestR <$> parseJSON (Object v)
--         Just "EnrollmentResponse" -> EnrollmentResponseR <$> parseJSON (Object v)
--         Just "EpisodeOfCare" -> EpisodeOfCareR <$> parseJSON (Object v)
--         Just "EventDefinition" -> EventDefinitionR <$> parseJSON (Object v)
--         Just "Evidence" -> EvidenceR <$> parseJSON (Object v)
--         Just "EvidenceVariable" -> EvidenceVariableR <$> parseJSON (Object v)
--         Just "ExampleScenario" -> ExampleScenarioR <$> parseJSON (Object v)
--         Just "ExplanationOfBenefit" -> ExplanationOfBenefitR <$> parseJSON (Object v)
--         Just "FamilyMemberHistory" -> FamilyMemberHistoryR <$> parseJSON (Object v)
--         Just "Flag" -> FlagR <$> parseJSON (Object v)
--         Just "Goal" -> GoalR <$> parseJSON (Object v)
--         Just "GraphDefinition" -> GraphDefinitionR <$> parseJSON (Object v)
--         Just "Group" -> GroupR <$> parseJSON (Object v)
--         Just "GuidanceResponse" -> GuidanceResponseR <$> parseJSON (Object v)
--         Just "HealthcareService" -> HealthcareServiceR <$> parseJSON (Object v)
--         Just "ImagingStudy" -> ImagingStudyR <$> parseJSON (Object v)
--         Just "Immunization" -> ImmunizationR <$> parseJSON (Object v)
--         Just "ImmunizationEvaluation" -> ImmunizationEvaluationR <$> parseJSON (Object v)
--         Just "ImmunizationRecommendation" -> ImmunizationRecommendationR <$> parseJSON (Object v)
--         Just "ImplementationGuide" -> ImplementationGuideR <$> parseJSON (Object v)
--         Just "InsurancePlan" -> InsurancePlanR <$> parseJSON (Object v)
--         Just "Invoice" -> InvoiceR <$> parseJSON (Object v)
--         Just "Library" -> LibraryR <$> parseJSON (Object v)
--         Just "Linkage" -> LinkageR <$> parseJSON (Object v)
--         Just "List" -> ListR <$> parseJSON (Object v)
--         Just "Location" -> LocationR <$> parseJSON (Object v)
--         Just "Measure" -> MeasureR <$> parseJSON (Object v)
--         Just "MeasureReport" -> MeasureReportR <$> parseJSON (Object v)
--         Just "Media" -> MediaR <$> parseJSON (Object v)
--         Just "Medication" -> MedicationR <$> parseJSON (Object v)
--         Just "MedicationAdministration" -> MedicationAdministrationR <$> parseJSON (Object v)
--         Just "MedicationDispense" -> MedicationDispenseR <$> parseJSON (Object v)
--         Just "MedicationKnowledge" -> MedicationKnowledgeR <$> parseJSON (Object v)
--         Just "MedicationRequest" -> MedicationRequestR <$> parseJSON (Object v)
--         Just "MedicationStatement" -> MedicationStatementR <$> parseJSON (Object v)
--         Just "MedicinalProduct" -> MedicinalProductR <$> parseJSON (Object v)
--         Just "MedicinalProductAuthorization" -> MedicinalProductAuthorizationR <$> parseJSON (Object v)
--         Just "MedicinalProductContraindication" -> MedicinalProductContraindicationR <$> parseJSON (Object v)
--         Just "MedicinalProductIndication" -> MedicinalProductIndicationR <$> parseJSON (Object v)
--         Just "MedicinalProductIngredient" -> MedicinalProductIngredientR <$> parseJSON (Object v)
--         Just "MedicinalProductInteraction" -> MedicinalProductInteractionR <$> parseJSON (Object v)
--         Just "MedicinalProductManufactured" -> MedicinalProductManufacturedR <$> parseJSON (Object v)
--         Just "MedicinalProductPackaged" -> MedicinalProductPackagedR <$> parseJSON (Object v)
--         Just "MedicinalProductPharmaceutical" -> MedicinalProductPharmaceuticalR <$> parseJSON (Object v)
--         Just "MedicinalProductUndesirableEffect" -> MedicinalProductUndesirableEffectR <$> parseJSON (Object v)
--         Just "MessageDefinition" -> MessageDefinitionR <$> parseJSON (Object v)
--         Just "MessageHeader" -> MessageHeaderR <$> parseJSON (Object v)
--         Just "MolecularSequence" -> MolecularSequenceR <$> parseJSON (Object v)
--         Just "NamingSystem" -> NamingSystemR <$> parseJSON (Object v)
--         Just "NutritionOrder" -> NutritionOrderR <$> parseJSON (Object v)
--         Just "Observation" -> ObservationR <$> parseJSON (Object v)
--         Just "ObservationDefinition" -> ObservationDefinitionR <$> parseJSON (Object v)
--         Just "OperationDefinition" -> OperationDefinitionR <$> parseJSON (Object v)
         Just "OperationOutcome" -> OperationOutcomeR <$> parseJSON (Object v)
--         Just "Organization" -> OrganizationR <$> parseJSON (Object v)
--         Just "OrganizationAffiliation" -> OrganizationAffiliationR <$> parseJSON (Object v)
         Just "Patient" -> PatientR <$> parseJSON (Object v)
--         Just "PaymentNotice" -> PaymentNoticeR <$> parseJSON (Object v)
--         Just "PaymentReconciliation" -> PaymentReconciliationR <$> parseJSON (Object v)
--         Just "Person" -> PersonR <$> parseJSON (Object v)
--         Just "PlanDefinition" -> PlanDefinitionR <$> parseJSON (Object v)
--         Just "Practitioner" -> PractitionerR <$> parseJSON (Object v)
--         Just "PractitionerRole" -> PractitionerRoleR <$> parseJSON (Object v)
--         Just "Procedure" -> ProcedureR <$> parseJSON (Object v)
--         Just "Provenance" -> ProvenanceR <$> parseJSON (Object v)
--         Just "Questionnaire" -> QuestionnaireR <$> parseJSON (Object v)
--         Just "QuestionnaireResponse" -> QuestionnaireResponseR <$> parseJSON (Object v)
--         Just "RelatedPerson" -> RelatedPersonR <$> parseJSON (Object v)
--         Just "RequestGroup" -> RequestGroupR <$> parseJSON (Object v)
--         Just "ResearchDefinition" -> ResearchDefinitionR <$> parseJSON (Object v)
--         Just "ResearchElementDefinition" -> ResearchElementDefinitionR <$> parseJSON (Object v)
--         Just "ResearchStudy" -> ResearchStudyR <$> parseJSON (Object v)
--         Just "ResearchSubject" -> ResearchSubjectR <$> parseJSON (Object v)
--         Just "RiskAssessment" -> RiskAssessmentR <$> parseJSON (Object v)
--         Just "RiskEvidenceSynthesis" -> RiskEvidenceSynthesisR <$> parseJSON (Object v)
--         Just "Schedule" -> ScheduleR <$> parseJSON (Object v)
--         Just "SearchParameter" -> SearchParameterR <$> parseJSON (Object v)
--         Just "ServiceRequest" -> ServiceRequestR <$> parseJSON (Object v)
--         Just "Slot" -> SlotR <$> parseJSON (Object v)
--         Just "Specimen" -> SpecimenR <$> parseJSON (Object v)
--         Just "SpecimenDefinition" -> SpecimenDefinitionR <$> parseJSON (Object v)
--         Just "StructureDefinition" -> StructureDefinitionR <$> parseJSON (Object v)
--         Just "StructureMap" -> StructureMapR <$> parseJSON (Object v)
--         Just "Subscription" -> SubscriptionR <$> parseJSON (Object v)
--         Just "Substance" -> SubstanceR <$> parseJSON (Object v)
--         Just "SubstancePolymer" -> SubstancePolymerR <$> parseJSON (Object v)
--         Just "SubstanceProtein" -> SubstanceProteinR <$> parseJSON (Object v)
--         Just "SubstanceReferenceInformation" -> SubstanceReferenceInformationR <$> parseJSON (Object v)
--         Just "SubstanceSpecification" -> SubstanceSpecificationR <$> parseJSON (Object v)
--         Just "SubstanceSourceMaterial" -> SubstanceSourceMaterialR <$> parseJSON (Object v)
--         Just "SupplyDelivery" -> SupplyDeliveryR <$> parseJSON (Object v)
--         Just "SupplyRequest" -> SupplyRequestR <$> parseJSON (Object v)
--         Just "Task" -> TaskR <$> parseJSON (Object v)
--         Just "TerminologyCapabilities" -> TerminologyCapabilitiesR <$> parseJSON (Object v)
--         Just "TestReport" -> TestReportR <$> parseJSON (Object v)
--         Just "TestScript" -> TestScriptR <$> parseJSON (Object v)
--         Just "ValueSet" -> ValueSetR <$> parseJSON (Object v)
--         Just "VerificationResult" -> VerificationResultR <$> parseJSON (Object v)
         _         -> fail "not supported resource"
      where rt = HM.lookup "resourceType" v

instance Xmlbf.ToXml Resource where
  toXml (BinaryR e) = Xmlbf.toXml e
  toXml (BundleR b)              = Xmlbf.toXml b
-- DomainResources
--  toXml (AccountR e) = Xmlbf.toXml e
  toXml (ActivityDefinitionR e) = Xmlbf.toXml e
--  toXml (AdverseEventR e) = Xmlbf.toXml e
--  toXml (AllergyIntoleranceR e) = Xmlbf.toXml e
--  toXml (AppointmentR e) = Xmlbf.toXml e
--  toXml (AppointmentResponseR e) = Xmlbf.toXml e
--  toXml (AuditEventR e) = Xmlbf.toXml e
--  toXml (BasicR e) = Xmlbf.toXml e
--  toXml (BiologicallyDerivedProductR e) = Xmlbf.toXml e
--  toXml (BodyStructureR e) = Xmlbf.toXml e
  toXml (CapabilityStatementR e) = Xmlbf.toXml e
--  toXml (CarePlanR e) = Xmlbf.toXml e
--  toXml (CareTeamR e) = Xmlbf.toXml e
--  toXml (CatalogEntryR e) = Xmlbf.toXml e
--  toXml (ChargeItemR e) = Xmlbf.toXml e
--  toXml (ChargeItemDefinitionR e) = Xmlbf.toXml e
--  toXml (ClaimR e) = Xmlbf.toXml e
--  toXml (ClaimResponseR e) = Xmlbf.toXml e
--  toXml (ClinicalImpressionR e) = Xmlbf.toXml e
--  toXml (CodeSystemR e) = Xmlbf.toXml e
--  toXml (CommunicationR e) = Xmlbf.toXml e
--  toXml (CommunicationRequestR e) = Xmlbf.toXml e
--  toXml (CompartmentDefinitionR e) = Xmlbf.toXml e
--  toXml (CompositionR e) = Xmlbf.toXml e
--  toXml (ConceptMapR e) = Xmlbf.toXml e
--  toXml (ConditionR e) = Xmlbf.toXml e
--  toXml (ConsentR e) = Xmlbf.toXml e
--  toXml (ContractR e) = Xmlbf.toXml e
--  toXml (CoverageR e) = Xmlbf.toXml e
--  toXml (CoverageEligibilityRequestR e) = Xmlbf.toXml e
--  toXml (CoverageEligibilityResponseR e) = Xmlbf.toXml e
--  toXml (DetectedIssueR e) = Xmlbf.toXml e
--  toXml (DeviceR e) = Xmlbf.toXml e
--  toXml (DeviceDefinitionR e) = Xmlbf.toXml e
--  toXml (DeviceMetricR e) = Xmlbf.toXml e
--  toXml (DeviceRequestR e) = Xmlbf.toXml e
--  toXml (DeviceUseStatementR e) = Xmlbf.toXml e
--  toXml (DiagnosticReportR e) = Xmlbf.toXml e
--  toXml (DocumentManifestR e) = Xmlbf.toXml e
--  toXml (DocumentReferenceR e) = Xmlbf.toXml e
--  toXml (EffectEvidenceSynthesisR e) = Xmlbf.toXml e
  toXml (EncounterR e) = Xmlbf.toXml e
--  toXml (EndpointR e) = Xmlbf.toXml e
--  toXml (EnrollmentRequestR e) = Xmlbf.toXml e
--  toXml (EnrollmentResponseR e) = Xmlbf.toXml e
--  toXml (EpisodeOfCareR e) = Xmlbf.toXml e
--  toXml (EventDefinitionR e) = Xmlbf.toXml e
--  toXml (EvidenceR e) = Xmlbf.toXml e
--  toXml (EvidenceVariableR e) = Xmlbf.toXml e
--  toXml (ExampleScenarioR e) = Xmlbf.toXml e
--  toXml (ExplanationOfBenefitR e) = Xmlbf.toXml e
--  toXml (FamilyMemberHistoryR e) = Xmlbf.toXml e
--  toXml (FlagR e) = Xmlbf.toXml e
--  toXml (GoalR e) = Xmlbf.toXml e
--  toXml (GraphDefinitionR e) = Xmlbf.toXml e
--  toXml (GroupR e) = Xmlbf.toXml e
--  toXml (GuidanceResponseR e) = Xmlbf.toXml e
--  toXml (HealthcareServiceR e) = Xmlbf.toXml e
--  toXml (ImagingStudyR e) = Xmlbf.toXml e
--  toXml (ImmunizationR e) = Xmlbf.toXml e
--  toXml (ImmunizationEvaluationR e) = Xmlbf.toXml e
--  toXml (ImmunizationRecommendationR e) = Xmlbf.toXml e
--  toXml (ImplementationGuideR e) = Xmlbf.toXml e
--  toXml (InsurancePlanR e) = Xmlbf.toXml e
--  toXml (InvoiceR e) = Xmlbf.toXml e
--  toXml (LibraryR e) = Xmlbf.toXml e
--  toXml (LinkageR e) = Xmlbf.toXml e
--  toXml (ListR e) = Xmlbf.toXml e
--  toXml (LocationR e) = Xmlbf.toXml e
--  toXml (MeasureR e) = Xmlbf.toXml e
--  toXml (MeasureReportR e) = Xmlbf.toXml e
--  toXml (MediaR e) = Xmlbf.toXml e
--  toXml (MedicationR e) = Xmlbf.toXml e
--  toXml (MedicationAdministrationR e) = Xmlbf.toXml e
--  toXml (MedicationDispenseR e) = Xmlbf.toXml e
--  toXml (MedicationKnowledgeR e) = Xmlbf.toXml e
--  toXml (MedicationRequestR e) = Xmlbf.toXml e
--  toXml (MedicationStatementR e) = Xmlbf.toXml e
--  toXml (MedicinalProductR e) = Xmlbf.toXml e
--  toXml (MedicinalProductAuthorizationR e) = Xmlbf.toXml e
--  toXml (MedicinalProductContraindicationR e) = Xmlbf.toXml e
--  toXml (MedicinalProductIndicationR e) = Xmlbf.toXml e
--  toXml (MedicinalProductIngredientR e) = Xmlbf.toXml e
--  toXml (MedicinalProductInteractionR e) = Xmlbf.toXml e
--  toXml (MedicinalProductManufacturedR e) = Xmlbf.toXml e
--  toXml (MedicinalProductPackagedR e) = Xmlbf.toXml e
--  toXml (MedicinalProductPharmaceuticalR e) = Xmlbf.toXml e
--  toXml (MedicinalProductUndesirableEffectR e) = Xmlbf.toXml e
--  toXml (MessageDefinitionR e) = Xmlbf.toXml e
--  toXml (MessageHeaderR e) = Xmlbf.toXml e
--  toXml (MolecularSequenceR e) = Xmlbf.toXml e
--  toXml (NamingSystemR e) = Xmlbf.toXml e
--  toXml (NutritionOrderR e) = Xmlbf.toXml e
--  toXml (ObservationR e) = Xmlbf.toXml e
--  toXml (ObservationDefinitionR e) = Xmlbf.toXml e
--  toXml (OperationDefinitionR e) = Xmlbf.toXml e
  toXml (OperationOutcomeR e) = Xmlbf.toXml e
--  toXml (OrganizationR e) = Xmlbf.toXml e
--  toXml (OrganizationAffiliationR e) = Xmlbf.toXml e
  toXml (PatientR e) = Xmlbf.toXml e
--  toXml (PaymentNoticeR e) = Xmlbf.toXml e
--  toXml (PaymentReconciliationR e) = Xmlbf.toXml e
--  toXml (PersonR e) = Xmlbf.toXml e
--  toXml (PlanDefinitionR e) = Xmlbf.toXml e
--  toXml (PractitionerR e) = Xmlbf.toXml e
--  toXml (PractitionerRoleR e) = Xmlbf.toXml e
--  toXml (ProcedureR e) = Xmlbf.toXml e
--  toXml (ProvenanceR e) = Xmlbf.toXml e
--  toXml (QuestionnaireR e) = Xmlbf.toXml e
--  toXml (QuestionnaireResponseR e) = Xmlbf.toXml e
--  toXml (RelatedPersonR e) = Xmlbf.toXml e
--  toXml (RequestGroupR e) = Xmlbf.toXml e
--  toXml (ResearchDefinitionR e) = Xmlbf.toXml e
--  toXml (ResearchElementDefinitionR e) = Xmlbf.toXml e
--  toXml (ResearchStudyR e) = Xmlbf.toXml e
--  toXml (ResearchSubjectR e) = Xmlbf.toXml e
--  toXml (RiskAssessmentR e) = Xmlbf.toXml e
--  toXml (RiskEvidenceSynthesisR e) = Xmlbf.toXml e
--  toXml (ScheduleR e) = Xmlbf.toXml e
--  toXml (SearchParameterR e) = Xmlbf.toXml e
--  toXml (ServiceRequestR e) = Xmlbf.toXml e
--  toXml (SlotR e) = Xmlbf.toXml e
--  toXml (SpecimenR e) = Xmlbf.toXml e
--  toXml (SpecimenDefinitionR e) = Xmlbf.toXml e
--  toXml (StructureDefinitionR e) = Xmlbf.toXml e
--  toXml (StructureMapR e) = Xmlbf.toXml e
--  toXml (SubscriptionR e) = Xmlbf.toXml e
--  toXml (SubstanceR e) = Xmlbf.toXml e
--  toXml (SubstancePolymerR e) = Xmlbf.toXml e
--  toXml (SubstanceProteinR e) = Xmlbf.toXml e
--  toXml (SubstanceReferenceInformationR e) = Xmlbf.toXml e
--  toXml (SubstanceSpecificationR e) = Xmlbf.toXml e
--  toXml (SubstanceSourceMaterialR e) = Xmlbf.toXml e
--  toXml (SupplyDeliveryR e) = Xmlbf.toXml e
--  toXml (SupplyRequestR e) = Xmlbf.toXml e
--  toXml (TaskR e) = Xmlbf.toXml e
--  toXml (TerminologyCapabilitiesR e) = Xmlbf.toXml e
--  toXml (TestReportR e) = Xmlbf.toXml e
--  toXml (TestScriptR e) = Xmlbf.toXml e
--  toXml (ValueSetR e) = Xmlbf.toXml e
--  toXml (VerificationResultR e) = Xmlbf.toXml e
instance Xmlbf.FromXml Resource where
  fromXml = do
    r <- Xmlbf.pAnyElement fromAny 
    return r
    where fromAny = do
            n <- Xmlbf.pName
            case n of
              "Binary" -> BinaryR <$> Xmlbf.fromXml
              "Bundle" -> BundleR <$> Xmlbf.fromXml
-- DomainResources
--            "Account" -> AccountR <$> Xmlbf.fromXml
              "ActivityDefinition" -> ActivityDefinitionR <$> Xmlbf.fromXml
--            "AdverseEvent" -> AdverseEventR <$> Xmlbf.fromXml
--            "AllergyIntolerance" -> AllergyIntoleranceR <$> Xmlbf.fromXml
--            "Appointment" -> AppointmentR <$> Xmlbf.fromXml
--            "AppointmentResponse" -> AppointmentResponseR <$> Xmlbf.fromXml
--            "AuditEvent" -> AuditEventR <$> Xmlbf.fromXml
--            "Basic" -> BasicR <$> Xmlbf.fromXml
--            "BiologicallyDerivedProduct" -> BiologicallyDerivedProductR <$> Xmlbf.fromXml
--            "BodyStructure" -> BodyStructureR <$> Xmlbf.fromXml
--              "CapabilityStatement" -> CapabilityStatementR <$> Xmlbf.fromXml
--            "CarePlan" -> CarePlanR <$> Xmlbf.fromXml
--            "CareTeam" -> CareTeamR <$> Xmlbf.fromXml
--            "CatalogEntry" -> CatalogEntryR <$> Xmlbf.fromXml
--            "ChargeItem" -> ChargeItemR <$> Xmlbf.fromXml
--            "ChargeItemDefinition" -> ChargeItemDefinitionR <$> Xmlbf.fromXml
--            "Claim" -> ClaimR <$> Xmlbf.fromXml
--            "ClaimResponse" -> ClaimResponseR <$> Xmlbf.fromXml
--            "ClinicalImpression" -> ClinicalImpressionR <$> Xmlbf.fromXml
--            "CodeSystem" -> CodeSystemR <$> Xmlbf.fromXml
--            "Communication" -> CommunicationR <$> Xmlbf.fromXml
--            "CommunicationRequest" -> CommunicationRequestR <$> Xmlbf.fromXml
--            "CompartmentDefinition" -> CompartmentDefinitionR <$> Xmlbf.fromXml
--            "Composition" -> CompositionR <$> Xmlbf.fromXml
--            "ConceptMap" -> ConceptMapR <$> Xmlbf.fromXml
--            "Condition" -> ConditionR <$> Xmlbf.fromXml
--            "Consent" -> ConsentR <$> Xmlbf.fromXml
--            "Contract" -> ContractR <$> Xmlbf.fromXml
--            "Coverage" -> CoverageR <$> Xmlbf.fromXml
--            "CoverageEligibilityRequest" -> CoverageEligibilityRequestR <$> Xmlbf.fromXml
--            "CoverageEligibilityResponse" -> CoverageEligibilityResponseR <$> Xmlbf.fromXml
--            "DetectedIssue" -> DetectedIssueR <$> Xmlbf.fromXml
--            "Device" -> DeviceR <$> Xmlbf.fromXml
--            "DeviceDefinition" -> DeviceDefinitionR <$> Xmlbf.fromXml
--            "DeviceMetric" -> DeviceMetricR <$> Xmlbf.fromXml
--            "DeviceRequest" -> DeviceRequestR <$> Xmlbf.fromXml
--            "DeviceUseStatement" -> DeviceUseStatementR <$> Xmlbf.fromXml
--            "DiagnosticReport" -> DiagnosticReportR <$> Xmlbf.fromXml
--            "DocumentManifest" -> DocumentManifestR <$> Xmlbf.fromXml
--            "DocumentReference" -> DocumentReferenceR <$> Xmlbf.fromXml
--            "EffectEvidenceSynthesis" -> EffectEvidenceSynthesisR <$> Xmlbf.fromXml
              "Encounter" -> EncounterR <$> Xmlbf.fromXml
--            "Endpoint" -> EndpointR <$> Xmlbf.fromXml
--            "EnrollmentRequest" -> EnrollmentRequestR <$> Xmlbf.fromXml
--            "EnrollmentResponse" -> EnrollmentResponseR <$> Xmlbf.fromXml
--            "EpisodeOfCare" -> EpisodeOfCareR <$> Xmlbf.fromXml
--            "EventDefinition" -> EventDefinitionR <$> Xmlbf.fromXml
--            "Evidence" -> EvidenceR <$> Xmlbf.fromXml
--            "EvidenceVariable" -> EvidenceVariableR <$> Xmlbf.fromXml
--            "ExampleScenario" -> ExampleScenarioR <$> Xmlbf.fromXml
--            "ExplanationOfBenefit" -> ExplanationOfBenefitR <$> Xmlbf.fromXml
--            "FamilyMemberHistory" -> FamilyMemberHistoryR <$> Xmlbf.fromXml
--            "Flag" -> FlagR <$> Xmlbf.fromXml
--            "Goal" -> GoalR <$> Xmlbf.fromXml
--            "GraphDefinition" -> GraphDefinitionR <$> Xmlbf.fromXml
--            "Group" -> GroupR <$> Xmlbf.fromXml
--            "GuidanceResponse" -> GuidanceResponseR <$> Xmlbf.fromXml
--            "HealthcareService" -> HealthcareServiceR <$> Xmlbf.fromXml
--            "ImagingStudy" -> ImagingStudyR <$> Xmlbf.fromXml
--            "Immunization" -> ImmunizationR <$> Xmlbf.fromXml
--            "ImmunizationEvaluation" -> ImmunizationEvaluationR <$> Xmlbf.fromXml
--            "ImmunizationRecommendation" -> ImmunizationRecommendationR <$> Xmlbf.fromXml
--            "ImplementationGuide" -> ImplementationGuideR <$> Xmlbf.fromXml
--            "InsurancePlan" -> InsurancePlanR <$> Xmlbf.fromXml
--            "Invoice" -> InvoiceR <$> Xmlbf.fromXml
--            "Library" -> LibraryR <$> Xmlbf.fromXml
--            "Linkage" -> LinkageR <$> Xmlbf.fromXml
--            "List" -> ListR <$> Xmlbf.fromXml
--            "Location" -> LocationR <$> Xmlbf.fromXml
--            "Measure" -> MeasureR <$> Xmlbf.fromXml
--            "MeasureReport" -> MeasureReportR <$> Xmlbf.fromXml
--            "Media" -> MediaR <$> Xmlbf.fromXml
--            "Medication" -> MedicationR <$> Xmlbf.fromXml
--            "MedicationAdministration" -> MedicationAdministrationR <$> Xmlbf.fromXml
--            "MedicationDispense" -> MedicationDispenseR <$> Xmlbf.fromXml
--            "MedicationKnowledge" -> MedicationKnowledgeR <$> Xmlbf.fromXml
--            "MedicationRequest" -> MedicationRequestR <$> Xmlbf.fromXml
--            "MedicationStatement" -> MedicationStatementR <$> Xmlbf.fromXml
--            "MedicinalProduct" -> MedicinalProductR <$> Xmlbf.fromXml
--            "MedicinalProductAuthorization" -> MedicinalProductAuthorizationR <$> Xmlbf.fromXml
--            "MedicinalProductContraindication" -> MedicinalProductContraindicationR <$> Xmlbf.fromXml
--            "MedicinalProductIndication" -> MedicinalProductIndicationR <$> Xmlbf.fromXml
--            "MedicinalProductIngredient" -> MedicinalProductIngredientR <$> Xmlbf.fromXml
--            "MedicinalProductInteraction" -> MedicinalProductInteractionR <$> Xmlbf.fromXml
--            "MedicinalProductManufactured" -> MedicinalProductManufacturedR <$> Xmlbf.fromXml
--            "MedicinalProductPackaged" -> MedicinalProductPackagedR <$> Xmlbf.fromXml
--            "MedicinalProductPharmaceutical" -> MedicinalProductPharmaceuticalR <$> Xmlbf.fromXml
--            "MedicinalProductUndesirableEffect" -> MedicinalProductUndesirableEffectR <$> Xmlbf.fromXml
--            "MessageDefinition" -> MessageDefinitionR <$> Xmlbf.fromXml
--            "MessageHeader" -> MessageHeaderR <$> Xmlbf.fromXml
--            "MolecularSequence" -> MolecularSequenceR <$> Xmlbf.fromXml
--            "NamingSystem" -> NamingSystemR <$> Xmlbf.fromXml
--            "NutritionOrder" -> NutritionOrderR <$> Xmlbf.fromXml
--            "Observation" -> ObservationR <$> Xmlbf.fromXml
--            "ObservationDefinition" -> ObservationDefinitionR <$> Xmlbf.fromXml
--            "OperationDefinition" -> OperationDefinitionR <$> Xmlbf.fromXml
              "OperationOutcome" -> OperationOutcomeR <$> Xmlbf.fromXml
--            "Organization" -> OrganizationR <$> Xmlbf.fromXml
--            "OrganizationAffiliation" -> OrganizationAffiliationR <$> Xmlbf.fromXml
              "Patient" -> PatientR <$> Xmlbf.fromXml
--            "PaymentNotice" -> PaymentNoticeR <$> Xmlbf.fromXml
--            "PaymentReconciliation" -> PaymentReconciliationR <$> Xmlbf.fromXml
--            "Person" -> PersonR <$> Xmlbf.fromXml
--            "PlanDefinition" -> PlanDefinitionR <$> Xmlbf.fromXml
--            "Practitioner" -> PractitionerR <$> Xmlbf.fromXml
--            "PractitionerRole" -> PractitionerRoleR <$> Xmlbf.fromXml
--            "Procedure" -> ProcedureR <$> Xmlbf.fromXml
--            "Provenance" -> ProvenanceR <$> Xmlbf.fromXml
--            "Questionnaire" -> QuestionnaireR <$> Xmlbf.fromXml
--            "QuestionnaireResponse" -> QuestionnaireResponseR <$> Xmlbf.fromXml
--            "RelatedPerson" -> RelatedPersonR <$> Xmlbf.fromXml
--            "RequestGroup" -> RequestGroupR <$> Xmlbf.fromXml
--            "ResearchDefinition" -> ResearchDefinitionR <$> Xmlbf.fromXml
--            "ResearchElementDefinition" -> ResearchElementDefinitionR <$> Xmlbf.fromXml
--            "ResearchStudy" -> ResearchStudyR <$> Xmlbf.fromXml
--            "ResearchSubject" -> ResearchSubjectR <$> Xmlbf.fromXml
--            "RiskAssessment" -> RiskAssessmentR <$> Xmlbf.fromXml
--            "RiskEvidenceSynthesis" -> RiskEvidenceSynthesisR <$> Xmlbf.fromXml
--            "Schedule" -> ScheduleR <$> Xmlbf.fromXml
--            "SearchParameter" -> SearchParameterR <$> Xmlbf.fromXml
--            "ServiceRequest" -> ServiceRequestR <$> Xmlbf.fromXml
--            "Slot" -> SlotR <$> Xmlbf.fromXml
--            "Specimen" -> SpecimenR <$> Xmlbf.fromXml
--            "SpecimenDefinition" -> SpecimenDefinitionR <$> Xmlbf.fromXml
--            "StructureDefinition" -> StructureDefinitionR <$> Xmlbf.fromXml
--            "StructureMap" -> StructureMapR <$> Xmlbf.fromXml
--            "Subscription" -> SubscriptionR <$> Xmlbf.fromXml
--            "Substance" -> SubstanceR <$> Xmlbf.fromXml
--            "SubstancePolymer" -> SubstancePolymerR <$> Xmlbf.fromXml
--            "SubstanceProtein" -> SubstanceProteinR <$> Xmlbf.fromXml
--            "SubstanceReferenceInformation" -> SubstanceReferenceInformationR <$> Xmlbf.fromXml
--            "SubstanceSpecification" -> SubstanceSpecificationR <$> Xmlbf.fromXml
--            "SubstanceSourceMaterial" -> SubstanceSourceMaterialR <$> Xmlbf.fromXml
--            "SupplyDelivery" -> SupplyDeliveryR <$> Xmlbf.fromXml
--            "SupplyRequest" -> SupplyRequestR <$> Xmlbf.fromXml
--            "Task" -> TaskR <$> Xmlbf.fromXml
--            "TerminologyCapabilities" -> TerminologyCapabilitiesR <$> Xmlbf.fromXml
--            "TestReport" -> TestReportR <$> Xmlbf.fromXml
--            "TestScript" -> TestScriptR <$> Xmlbf.fromXml
--            "ValueSet" -> ValueSetR <$> Xmlbf.fromXml
--            "VerificationResult" -> VerificationResultR <$> Xmlbf.fromXml

{-
mkDomainResource = DomainResource{
          domainResourceFilter = []
        , domainResourceAttribs= HM.fromList [("xmlns","http://hl7.org/fhir"),("xml:id","123456")]
        , domainResourceId=Just "123456"
        , domainResourceMeta=Just mkMeta
        , domainResourceImplicitRules=Nothing
        , domainResourceLanguage=Nothing
        , domainResourceText= Just $ mkNarrative
        , domainResourceExtension=[]
        , domainResourceModifierExtension=[]}

<Bundle>
  <type value="search-set"></type>
  <timestamp value="2021-06-03"></timestamp>
  <total value="1"></total>
  <entry>
    <fullUrl value="test"></fullUrl>
    <resource>
      <Patient xmlns="http://hl7.org/fhir"><id value="00016996-0d92-4880-8161-7845c3b4c4be"></id><name><use value="official"></use><family value="Pouros728"></family><given value="Freddie621"></given><prefix value="Ms."></prefix></name><telecom><system value="phone"></system><value value="555-334-3277"></value><use value="home"></use></telecom><gender value="female"></gender><birthDate value="1995-01-26"></birthDate>
      </Patient>
    </resource>
  </entry>
</Bundle>
-}
mkBundle btype drs = Bundle{
         bundleId = Nothing
       , bundleMeta = Nothing
       , bundleImplicitRules = Nothing
       , bundleLanguage = Nothing
       , bundleIdentifier=Nothing
       , bundleType=btype
       , bundleTimestamp=Just "2021-06-03"
       , bundleTotal=Just $ length drs
       , bundleLink=[]
       , bundleEntry=fmap mkEntry drs
       , bundleSignature=Nothing
       }
mkEntry (Just dr) = BundleEntry {
    bundleEntryAttrId    = Nothing
  , bundleEntryExtension = []
  , bundleEntryModifierExtension = []
  , bundleEntryLink      = []
  , bundleEntryFullUrl   = Just "test"
  , bundleEntryResource  = Just dr
  , bundleEntrySearch    = Nothing
  , bundleEntryRequest   = Nothing
  , bundleEntryResponse  = Nothing
  }
mkEntry Nothing = BundleEntry {
    bundleEntryAttrId    = Nothing
  , bundleEntryExtension = []
  , bundleEntryModifierExtension = []
  , bundleEntryLink      = []
  , bundleEntryFullUrl   = Just "decoding json failed" -- should be stated in response
  , bundleEntryResource  = Nothing
  , bundleEntrySearch    = Nothing
  , bundleEntryRequest   = Nothing
  , bundleEntryResponse  = Nothing
  }

mkCapabilityStatement = CapabilityStatement{
    capabilityStatementId = Nothing
  , capabilityStatementMeta = Nothing
  , capabilityStatementImplicitRules = Nothing
  , capabilityStatementLanguage  = Nothing
  , capabilityStatementText      = Nothing
  , capabilityStatementExtension = []
  , capabilityStatementModifierExtension = []
  , capabilityStatementUrl = Nothing
  , capabilityStatementVersion = Nothing
  , capabilityStatementName = Just "Ntwo Haskell"
  , capabilityStatementTitle = Just "Ntwo Haskell"
  , capabilityStatementStatus = PsDraft
  , capabilityStatementExperimental = Just True
  , capabilityStatementDate = "2021-06-22T08:00:00"
  , capabilityStatementPublisher = Just "eNahar.org"
  , capabilityStatementContact = [mkContact]
  , capabilityStatementDescription = Nothing
  , capabilityStatementUseContext = []
  , capabilityStatementJurisdiction = [mkJurisdiction]
  , capabilityStatementPurpose = Just "FHIR server for nSPZ UKK"
  , capabilityStatementCopyright = Just "MAAT"
  , capabilityStatementKind = CSKInstance
  , capabilityStatementInstantiates = []
  , capabilityStatementImports = []
  , capabilityStatementSoftware = Just mkCapabilityStatementSoftware
  , capabilityStatementImplementation = Just mkCapabilityStatementImplementation
  , capabilityStatementFhirVersion = Fv_401
  , capabilityStatementFormat = []
  , capabilityStatementPatchFormat = []
  , capabilityStatementImplementationGuide = []
  , capabilityStatementRest = [mkRest]
  , capabilityStatementMessaging = []
  , capabilityStatementDocument = []
  }

mkCapabilityStatementSoftware
  = CapabilityStatementSoftware{
      capabilityStatementSoftwareAttrId = Nothing
    , capabilityStatementSoftwareExtension = []
    , capabilityStatementSoftwareModifierExtension = []
    , capabilityStatementSoftwareName = "EHR"
    , capabilityStatementSoftwareVersion = Just "0.00.020.2134"
    , capabilityStatementSoftwareReleaseDate = Just "2012-01-04"
    }
mkCapabilityStatementImplementation
  = CapabilityStatementImplementation{
      capabilityStatementImplementationAttrId = Nothing
    , capabilityStatementImplementationExtension = []
    , capabilityStatementImplementationModifierExtension = []
    , capabilityStatementImplementationDescription = "Main EHR at SPZ UKK"
    , capabilityStatementImplementationUrl = Just "http://10.2.3.4/fhir"
    , capabilityStatementImplementationCustodian = Just $ mkReference "pmh"
    }
mkContact = ContactDetail{
      contactDetailAttribs = []
    , contactDetailId = Nothing
    , contactDetailExtension = []
    , contactDetailName = Just "System Administrator" 
    , contactDetailTelecom = [
          ContactPoint{ 
                contactPointAttribs = []
              , contactPointId = Nothing
              , contactPointExtension = []
              , contactPointSystem = Just CpsEmail 
              , contactPointValue  = Just "peter.herkenrath@uk-koeln.de" 
              , contactPointUse    = Nothing
              , contactPointRank   = Nothing
              , contactPointPeriod = Nothing
              }]
    }
mkUseContext = UsageContext{
      usageContextAttribs = []
    , usageContextId = Nothing
    , usageContextExtension = []
    , usageContextCode = Coding{ 
             codingAttribs = []
           , codingId = Nothing
           , codingExtension = []
           , codingSystem = Just "http://terminology.hl7.org/CodeSystem/usage-context-type"
           , codingCode = Just "focus" 
           , codingVersion = Nothing
           , codingDisplay = Nothing
           , codingUserSelected = Nothing
           }
    , usageContextValue = UcvCC CodeableConcept{
             codeableConceptAttribs = []
           , codeableConceptId = Nothing
           , codeableConceptExtension = []
           , codeableConceptCoding = [
               Coding{ 
                   codingAttribs = []
                 , codingId = Nothing
                 , codingExtension = []
                 , codingSystem = Just "http://terminology.hl7.org/CodeSystem/variant-state"
                 , codingCode = Just "positive"
                 , codingVersion = Nothing
                 , codingDisplay = Nothing
                 , codingUserSelected = Nothing
                 }]
           , codeableConceptText = Nothing
           }
    }
mkJurisdiction =
    CodeableConcept{
        codeableConceptAttribs = []
      , codeableConceptId = Nothing
      , codeableConceptExtension = []
      , codeableConceptCoding =[
          Coding{ 
                codingAttribs = []
              , codingId = Nothing
              , codingExtension = []
              , codingSystem = Just "urn:iso:std:iso:3166" 
              , codingCode = Just "DEU" 
              , codingVersion = Nothing
              , codingDisplay = Just "Deutschland"
              , codingUserSelected = Nothing
              }]
      , codeableConceptText = Nothing
    }
mkRest = CapabilityStatementRest{
      capabilityStatementRestAttrId = Nothing
    , capabilityStatementRestExtension = []
    , capabilityStatementRestModifierExtension = []
    , capabilityStatementRestMode = CSRMServer
    , capabilityStatementRestDocumentation = Just "Main endpoint for Nabu2"
    , capabilityStatementRestSecurity = Nothing
    , capabilityStatementRestResource= mkResources
    , capabilityStatementRestInteraction = []
    , capabilityStatementRestSearchParam = []  -- for all resources
    , capabilityStatementRestOperation   = []
    , capabilityStatementRestCompartment = ["http://enahar.org/nabu/CompartmentDefinition/patient"]
    }
mkInteraction = CapabilityStatementInteraction{
      capabilityStatementInteractionAttrId = Nothing
    , capabilityStatementInteractionExtension = []
    , capabilityStatementInteractionModifierExtension = []
    , capabilityStatementInteractionCode = CSICSearchType
    , capabilityStatementInteractionDocumentation = Nothing
    }
mkResources = []
{-
    <security> 
      <!--   cors support is highly recommended - mandatory if using SMART on FHIR  -->
      <cors value="true"/> 
      <service> 
        <coding> 
          <codingSystem value="http://terminology.hl7.org/CodeSystem/restful-security-service"/> 
          <codingCode value="SMART-on-FHIR"/> 
        </coding> 
      </service> 
      <description value="See Smart on FHIR documentation"/> 
    </security> 
-}
{-
    <resource>
      <type value="Patient"/>
      <!--    let's assume that HL7 has stood up a profile registry at http://registry.fhir.org
        - it's likely to have a registry, though this is not decided, nor is a URL decided.

        This application simply uses a profile registered directly with HL7. For the simplest

        case of a FHIR REST Server, just delete this profile reference. Profile references
       do
        not need to be a UUID, though a profile registry could insist that they are
        -->
      <profile value="http://registry.fhir.org/r4/StructureDefinition/7896271d-57f6-4231-89dc-dcc91eab2416"/>
      <!--    this system supports a specific profile for animals   -->
      <supportedProfile value="http://registry.fhir.org/r4/StructureDefinition/00ab9e7a-06c7-4f77-9234-4154ca1e3347"/>
      <documentation value="This server does not let the clients create identities."/>
      <interaction>
        <codingCode value="read"/>
      </interaction>
      <interaction>
        <codingCode value="vread"/>
        <documentation value="Only supported for patient records since 12-Dec 2012"/>
      </interaction>
      <interaction>
        <codingCode value="update"/>
      </interaction>
      <interaction>
        <codingCode value="history-instance"/>
      </interaction>
      <interaction>
        <codingCode value="create"/>
      </interaction>
      <interaction>
        <codingCode value="history-type"/>
      </interaction>
      <versioning value="versioned-update"/>
      <readHistory value="true"/>
      <!--   this server doesn't let the clients create identities   -->
      <updateCreate value="false"/>
      <!--   it's good to support conditional create on patients; this solves a common middleware problem   -->
      <conditionalCreate value="true"/>
      <conditionalRead value="full-support"/>
      <conditionalUpdate value="false"/>
      <!--   0..1 If allows/uses conditional update   -->
      <conditionalDelete value="not-supported"/>
      <searchInclude value="Organization"/>
      <searchRevInclude value="Person"/>
      <searchParam>
        <name value="identifier"/>
        <definition value="http://hl7.org/fhir/SearchParameter/Patient-identifier"/>
        <type value="token"/>
        <documentation value="Only supports search by institution MRN"/>
      </searchParam>
      <searchParam>
        <name value="general-practitioner"/>
        <definition value="http://hl7.org/fhir/SearchParameter/Patient-general-practitioner"/>
        <type value="reference"/>
      </searchParam>
    </resource>
-}
-- mkMessaging=
{-
  <!--    a messaging capability statement. Applications are not required to make a capability
   
    statement with regard to messaging, though there is active argument that they should.
       -->
  <messaging> 
    <endpoint> 
      <protocol> 
        <codingSystem value="http://terminology.hl7.org/CodeSystem/message-transport"/> 
        <codingCode value="mllp"/> 
      </protocol> 
      <!--   LLP server at 10.1.1.10 on port 9234   -->
      <address value="mllp:10.1.1.10:9234"/> 
    </endpoint> 
    <reliableCache value="30"/> 
    <documentation value="ADT A08 equivalent for external system notifications"/> 
    <supportedMessage> 
      <mode value="receiver"/> 
      <definition value="MessageDefinition/example"/> 
    </supportedMessage> 
  </messaging> 
-}
mkDocument 
  = CapabilityStatementDocument{
      capabilityStatementDocumentAttrId = Nothing
    , capabilityStatementDocumentExtension = []
    , capabilityStatementDocumentModifierExtension = []
    , capabilityStatementDocumentMode = CSDMConsumer 
    , capabilityStatementDocumentDocumentation = Just "Basic rules for all documents in the EHR system"
    --    this is the important Xmlbf.element: a reference to a published document profile note that this is a version specific reference.
    , capabilityStatementDocumentProfile ="http://fhir.hl7.org/base/Profilebc054d23-75e1-4dc6-aca5-838b6b1ac81d/_history/b5fdd9fc-b021-4ea1-911a-721a60663796"
    }

mkEncounter = Encounter{
    encounterId = Nothing
  , encounterMeta = Nothing
  , encounterImplicitRules = Nothing
  , encounterLanguage  = Nothing
  , encounterText      = Nothing
  , encounterExtension = []
  , encounterModifierExtension = []
  , encounterIdentifier      = [mkIdentifier] 
  , encounterStatus          = ESFinished
  , encounterStatusHistory   = []
  , encounterClass           = mkCoding "eclass"
  , encounterClassHistory    = []
  , encounterType            = []
  , encounterServiceType     = Nothing
  , encounterPriority        = Nothing
  , encounterSubject         = Just $ mkReference "gp"
  , encounterEpisodeOfCare   = []
  , encounterBasedOn         = []
  , encounterParticipant     = []
  , encounterAppointment     = []
  , encounterPeriod          = Nothing
  , encounterLength          = Nothing
  , encounterReasonCode      = []
  , encounterReasonReference = []
  , encounterDiagnosis       = []
  , encounterAccount         = []
  , encounterHospitalization = Nothing
  , encounterLocation        = []
  , encounterServiceProvider = Nothing
  , encounterPartOf          = Nothing
  }

mkOperationOutcomeIssue 
    = OperationOutcomeIssue{
        operationOutcomeIssueAttrId = Nothing
      , operationOutcomeIssueExtension = []
      , operationOutcomeIssueModifierExtension = []
      , operationOutcomeIssueSeverity = OOISInformation
      , operationOutcomeIssueCode = OOICInformational
      , operationOutcomeIssueDetails = Nothing
      , operationOutcomeIssueDiagnostics = Just "Test"
      , operationOutcomeIssueLocation    = []
      , operationOutcomeIssueExpression = []
      }
mkOperationOutcome 
    = OperationOutcome{
          operationOutcomeId = Nothing
        , operationOutcomeMeta = Nothing
        , operationOutcomeImplicitRules = Nothing
        , operationOutcomeLanguage  = Nothing
        , operationOutcomeText      = Nothing
        , operationOutcomeExtension = []
        , operationOutcomeModifierExtension = []
      , operationOutcomeIssue=[mkOperationOutcomeIssue]
      }


mkPatient i = Patient{
          patientId = Nothing
        , patientMeta = Nothing
        , patientImplicitRules = Nothing
        , patientLanguage  = Nothing
        , patientText      = Nothing
        , patientExtension = []
        , patientModifierExtension = []
        , patientIdentifier = [mkIdentifier,mkIdentifier]
        , patientActive= Just True
        , patientName = [mkHumanName]
        , patientTelecom = [mkContactPoint]
        , patientGender = Just AgFemale
        , patientBirthDate = Just i
        , patientMultipleBirth = Nothing
        , patientAddress = [mkAddress]
        , patientMaritalStatus = Nothing
        , patientDeceased = Just (PatientDeceasedBoolean False)
        , patientPhoto = []
        , patientContact = [mkPatientContact]
        , patientCommunication = [mkPatientComm]
        , patientGeneralPractitioner = [mkReference "gp"]
        , patientManagingOrganization = Just $ mkReference "org"
        , patientLink = []
        }
mkPatientLink = PatientLink{
          patientLinkAttrId = Nothing
        , patientLinkExtension = []
        , patientLinkModifierExtension = []
        , patientLinkOther = mkReference "lang"
        , patientLinkType  = LtReplaces
        }
mkPatientComm = PatientCommunication{
          patientCommunicationAttrId = Nothing
        , patientCommunicationExtension = []
        , patientCommunicationModifierExtension = []
        , patientCommunicationLanguage = mkCodeableConcept "lang"
        , patientCommunicationPreferred = Just True
        }
mkPatientContact = PatientContact{
          patientContactAttrId = Nothing
        , patientContactExtension = []
        , patientContactModifierExtension = []
        , patientContactRelationship = []
        , patientContactName = Just mkHumanName
        , patientContactTelecom = []
        , patientContactAddress = Just mkAddress
        , patientContactGender = Just AgMale
        , patientContactOrganization= Just $ mkReference "org"
        , patientContactPeriod = Nothing
        }
