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

module Data.FHIR.Interface (
    RID(..)
  , FHIR_Type(..)
  , Compartment(..)
  , FhirStatus(..)
  , Bundle(..)
  , BundleType(..)
  , BundleEntry(..)
  , BundleRequestMethod(..)
  , CapabilityStatement
  , Resource(..)
  , DomainResourceC(..)
  , resourceToBundle
  , mkCapabilityStatement
  , mkOperationOutcome
  , mkResource
  , mkBatchResponseBundle
  , mkHistoryBundle
  , mkSearchsetBundle
  , module Data.FHIR.Datatypes
  , module Data.FHIR.Datatypes.ResourceTypes 
  , module Data.FHIR.Resources.OperationOutcome
  ) where

import Data.Aeson  
import Data.Aeson.Types hiding (parseJSON)
import RIO
import qualified RIO.Text as T
import qualified RIO.HashMap as HM

import Data.FHIR.Datatypes
import Data.FHIR.Datatypes.ResourceTypes
import Data.FHIR.Resources
import Data.FHIR.Resources.OperationOutcome
import Data.FHIR.Resources.Resource
import Data.FHIR.Resources.ResourceContainer
import Xmlbf

-- TODO remove stubs
--
mkResource :: FHIR_Type -> T.Text -> Resource
mkResource FHIR_Patient i          = PatientR          $ mkPatient i
mkResource FHIR_Encounter _        = EncounterR        $ mkEncounter
mkResource FHIR_OperationOutcome _ = OperationOutcomeR $ mkOperationOutcome
mkResource _ _                     = error "mkResource: type nyi" 


-- mkBundleR:: BundleType -> [Maybe DomainResourceC] -> Resource
mkBatchResponseBundle rs = BundleR $ mkBundle BTBatchResponse (fmap toDomainResourceC rs)
mkHistoryBundle drs = BundleR $ mkBundle BTHistory drs
mkSearchsetBundle drs = BundleR $ mkBundle BTSearchset drs

resourceToBundle :: Resource -> Bundle
resourceToBundle (BundleR b) = b
resourceToBundle _ = error "nyi"

-- | toDomainResourceC
--
--   maps from ResourceR to ResourceDR
--   needed in Bundle construction
--
toDomainResourceC :: Maybe Resource -> Maybe DomainResourceC
toDomainResourceC Nothing = Nothing
--  toDomainResourceC (Just (AccountR o)) = Just ( AccountDR o)
toDomainResourceC (Just (ActivityDefinitionR o)) = Just ( ActivityDefinitionDR o)
--  toDomainResourceC (Just (AdverseEventR o)) = Just ( AdverseEventDR o)
--  toDomainResourceC (Just (AllergyIntoleranceR o)) = Just ( AllergyIntoleranceDR o)
--  toDomainResourceC (Just (AppointmentR o)) = Just ( AppointmentDR o)
--  toDomainResourceC (Just (AppointmentResponseR o)) = Just ( AppointmentResponseDR o)
--  toDomainResourceC (Just (AuditEventR o)) = Just ( AuditEventDR o)
--  toDomainResourceC (Just (BasicR o)) = Just ( BasicDR o)
--  toDomainResourceC (Just (BinaryR o)) = Just ( BinaryDR o)
--  toDomainResourceC (Just (BiologicallyDerivedProductR o)) = Just ( BiologicallyDerivedProductDR o)
--  toDomainResourceC (Just (BodyStructureR o)) = Just ( BodyStructureDR o)
toDomainResourceC (Just (CapabilityStatementR o)) = Just ( CapabilityStatementDR o)
--  toDomainResourceC (Just (CarePlanR o)) = Just ( CarePlanDR o)
--  toDomainResourceC (Just (CareTeamR o)) = Just ( CareTeamDR o)
--  toDomainResourceC (Just (CatalogEntryR o)) = Just ( CatalogEntryDR o)
--  toDomainResourceC (Just (ChargeItemR o)) = Just ( ChargeItemDR o)
--  toDomainResourceC (Just (ChargeItemDefinitionR o)) = Just ( ChargeItemDefinitionDR o)
--  toDomainResourceC (Just (ClaimR o)) = Just ( ClaimDR o)
--  toDomainResourceC (Just (ClaimResponseR o)) = Just ( ClaimResponseDR o)
--  toDomainResourceC (Just (ClinicalImpressionR o)) = Just ( ClinicalImpressionDR o)
--  toDomainResourceC (Just (CodeSystemR o)) = Just ( CodeSystemDR o)
--  toDomainResourceC (Just (CommunicationR o)) = Just ( CommunicationDR o)
--  toDomainResourceC (Just (CommunicationRequestR o)) = Just ( CommunicationRequestDR o)
--  toDomainResourceC (Just (CompartmentDefinitionR o)) = Just ( CompartmentDefinitionDR o)
--  toDomainResourceC (Just (CompositionR o)) = Just ( CompositionDR o)
--  toDomainResourceC (Just (ConceptMapR o)) = Just ( ConceptMapDR o)
--  toDomainResourceC (Just (ConditionR o)) = Just ( ConditionDR o)
--  toDomainResourceC (Just (ConsentR o)) = Just ( ConsentDR o)
--  toDomainResourceC (Just (ContractR o)) = Just ( ContractDR o)
--  toDomainResourceC (Just (CoverageR o)) = Just ( CoverageDR o)
--  toDomainResourceC (Just (CoverageEligibilityRequestR o)) = Just ( CoverageEligibilityRequestDR o)
--  toDomainResourceC (Just (CoverageEligibilityResponseR o)) = Just ( CoverageEligibilityResponseDR o)
--  toDomainResourceC (Just (DetectedIssueR o)) = Just ( DetectedIssueDR o)
--  toDomainResourceC (Just (DeviceR o)) = Just ( DeviceDR o)
--  toDomainResourceC (Just (DeviceDefinitionR o)) = Just ( DeviceDefinitionDR o)
--  toDomainResourceC (Just (DeviceMetricR o)) = Just ( DeviceMetricDR o)
--  toDomainResourceC (Just (DeviceRequestR o)) = Just ( DeviceRequestDR o)
--  toDomainResourceC (Just (DeviceUseStatementR o)) = Just ( DeviceUseStatementDR o)
--  toDomainResourceC (Just (DiagnosticReportR o)) = Just ( DiagnosticReportDR o)
--  toDomainResourceC (Just (DocumentManifestR o)) = Just ( DocumentManifestDR o)
--  toDomainResourceC (Just (DocumentReferenceR o)) = Just ( DocumentReferenceDR o)
--  toDomainResourceC (Just (EffectEvidenceSynthesisR o)) = Just ( EffectEvidenceSynthesisDR o)
toDomainResourceC (Just (EncounterR o)) = Just ( EncounterDR o)
--  toDomainResourceC (Just (EndpointR o)) = Just ( EndpointDR o)
--  toDomainResourceC (Just (EnrollmentRequestR o)) = Just ( EnrollmentRequestDR o)
--  toDomainResourceC (Just (EnrollmentResponseR o)) = Just ( EnrollmentResponseDR o)
--  toDomainResourceC (Just (EpisodeOfCareR o)) = Just ( EpisodeOfCareDR o)
--  toDomainResourceC (Just (EventDefinitionR o)) = Just ( EventDefinitionDR o)
--  toDomainResourceC (Just (EvidenceR o)) = Just ( EvidenceDR o)
--  toDomainResourceC (Just (EvidenceVariableR o)) = Just ( EvidenceVariableDR o)
--  toDomainResourceC (Just (ExampleScenarioR o)) = Just ( ExampleScenarioDR o)
--  toDomainResourceC (Just (ExplanationOfBenefitR o)) = Just ( ExplanationOfBenefitDR o)
--  toDomainResourceC (Just (FamilyMemberHistoryR o)) = Just ( FamilyMemberHistoryDR o)
--  toDomainResourceC (Just (FlagR o)) = Just ( FlagDR o)
--  toDomainResourceC (Just (GoalR o)) = Just ( GoalDR o)
--  toDomainResourceC (Just (GraphDefinitionR o)) = Just ( GraphDefinitionDR o)
--  toDomainResourceC (Just (GroupR o)) = Just ( GroupDR o)
--  toDomainResourceC (Just (GuidanceResponseR o)) = Just ( GuidanceResponseDR o)
--  toDomainResourceC (Just (HealthcareServiceR o)) = Just ( HealthcareServiceDR o)
--  toDomainResourceC (Just (ImagingStudyR o)) = Just ( ImagingStudyDR o)
--  toDomainResourceC (Just (ImmunizationR o)) = Just ( ImmunizationDR o)
--  toDomainResourceC (Just (ImmunizationEvaluationR o)) = Just ( ImmunizationEvaluationDR o)
--  toDomainResourceC (Just (ImmunizationRecommendationR o)) = Just ( ImmunizationRecommendationDR o)
--  toDomainResourceC (Just (ImplementationGuideR o)) = Just ( ImplementationGuideDR o)
--  toDomainResourceC (Just (InsurancePlanR o)) = Just ( InsurancePlanDR o)
--  toDomainResourceC (Just (InvoiceR o)) = Just ( InvoiceDR o)
--  toDomainResourceC (Just (LibraryR o)) = Just ( LibraryDR o)
--  toDomainResourceC (Just (LinkageR o)) = Just ( LinkageDR o)
--  toDomainResourceC (Just (ListR o)) = Just ( ListDR o)
--  toDomainResourceC (Just (LocationR o)) = Just ( LocationDR o)
--  toDomainResourceC (Just (MeasureR o)) = Just ( MeasureDR o)
--  toDomainResourceC (Just (MeasureReportR o)) = Just ( MeasureReportDR o)
--  toDomainResourceC (Just (MediaR o)) = Just ( MediaDR o)
--  toDomainResourceC (Just (MedicationR o)) = Just ( MedicationDR o)
--  toDomainResourceC (Just (MedicationAdministrationR o)) = Just ( MedicationAdministrationDR o)
--  toDomainResourceC (Just (MedicationDispenseR o)) = Just ( MedicationDispenseDR o)
--  toDomainResourceC (Just (MedicationKnowledgeR o)) = Just ( MedicationKnowledgeDR o)
--  toDomainResourceC (Just (MedicationRequestR o)) = Just ( MedicationRequestDR o)
--  toDomainResourceC (Just (MedicationStatementR o)) = Just ( MedicationStatementDR o)
--  toDomainResourceC (Just (MedicinalProductR o)) = Just ( MedicinalProductDR o)
--  toDomainResourceC (Just (MedicinalProductAuthorizationR o)) = Just ( MedicinalProductAuthorizationDR o)
--  toDomainResourceC (Just (MedicinalProductContraindicationR o)) = Just ( MedicinalProductContraindicationDR o)
--  toDomainResourceC (Just (MedicinalProductIndicationR o)) = Just ( MedicinalProductIndicationDR o)
--  toDomainResourceC (Just (MedicinalProductIngredientR o)) = Just ( MedicinalProductIngredientDR o)
--  toDomainResourceC (Just (MedicinalProductInteractionR o)) = Just ( MedicinalProductInteractionDR o)
--  toDomainResourceC (Just (MedicinalProductManufacturedR o)) = Just ( MedicinalProductManufacturedDR o)
--  toDomainResourceC (Just (MedicinalProductPackagedR o)) = Just ( MedicinalProductPackagedDR o)
--  toDomainResourceC (Just (MedicinalProductPharmaceuticalR o)) = Just ( MedicinalProductPharmaceuticalDR o)
--  toDomainResourceC (Just (MedicinalProductUndesirableEffectR o)) = Just ( MedicinalProductUndesirableEffectDR o)
--  toDomainResourceC (Just (MessageDefinitionR o)) = Just ( MessageDefinitionDR o)
--  toDomainResourceC (Just (MessageHeaderR o)) = Just ( MessageHeaderDR o)
--  toDomainResourceC (Just (MolecularSequenceR o)) = Just ( MolecularSequenceDR o)
--  toDomainResourceC (Just (NamingSystemR o)) = Just ( NamingSystemDR o)
--  toDomainResourceC (Just (NutritionOrderR o)) = Just ( NutritionOrderDR o)
--  toDomainResourceC (Just (ObservationR o)) = Just ( ObservationDR o)
--  toDomainResourceC (Just (ObservationDefinitionR o)) = Just ( ObservationDefinitionDR o)
--  toDomainResourceC (Just (OperationDefinitionR o)) = Just ( OperationDefinitionDR o)
toDomainResourceC (Just (OperationOutcomeR o)) = Just ( OperationOutcomeDR o)
--  toDomainResourceC (Just (OrganizationR o)) = Just ( OrganizationDR o)
--  toDomainResourceC (Just (OrganizationAffiliationR o)) = Just ( OrganizationAffiliationDR o)
--  toDomainResourceC (Just (ParametersR o)) = Just ( ParametersDR o)
toDomainResourceC (Just (PatientR o)) = Just ( PatientDR o)
--  toDomainResourceC (Just (PaymentNoticeR o)) = Just ( PaymentNoticeDR o)
--  toDomainResourceC (Just (PaymentReconciliationR o)) = Just ( PaymentReconciliationDR o)
--  toDomainResourceC (Just (PersonR o)) = Just ( PersonDR o)
--  toDomainResourceC (Just (PlanDefinitionR o)) = Just ( PlanDefinitionDR o)
--  toDomainResourceC (Just (PractitionerR o)) = Just ( PractitionerDR o)
--  toDomainResourceC (Just (PractitionerRoleR o)) = Just ( PractitionerRoleDR o)
--  toDomainResourceC (Just (ProcedureR o)) = Just ( ProcedureDR o)
--  toDomainResourceC (Just (ProvenanceR o)) = Just ( ProvenanceDR o)
--  toDomainResourceC (Just (QuestionnaireR o)) = Just ( QuestionnaireDR o)
--  toDomainResourceC (Just (QuestionnaireResponseR o)) = Just ( QuestionnaireResponseDR o)
--  toDomainResourceC (Just (RelatedPersonR o)) = Just ( RelatedPersonDR o)
--  toDomainResourceC (Just (RequestGroupR o)) = Just ( RequestGroupDR o)
--  toDomainResourceC (Just (ResearchDefinitionR o)) = Just ( ResearchDefinitionDR o)
--  toDomainResourceC (Just (ResearchElementDefinitionR o)) = Just ( ResearchElementDefinitionDR o)
--  toDomainResourceC (Just (ResearchStudyR o)) = Just ( ResearchStudyDR o)
--  toDomainResourceC (Just (ResearchSubjectR o)) = Just ( ResearchSubjectDR o)
--  toDomainResourceC (Just (RiskAssessmentR o)) = Just ( RiskAssessmentDR o)
--  toDomainResourceC (Just (RiskEvidenceSynthesisR o)) = Just ( RiskEvidenceSynthesisDR o)
--  toDomainResourceC (Just (ScheduleR o)) = Just ( ScheduleDR o)
--  toDomainResourceC (Just (SearchParameterR o)) = Just ( SearchParameterDR o)
--  toDomainResourceC (Just (ServiceRequestR o)) = Just ( ServiceRequestDR o)
--  toDomainResourceC (Just (SlotR o)) = Just ( SlotDR o)
--  toDomainResourceC (Just (SpecimenR o)) = Just ( SpecimenDR o)
--  toDomainResourceC (Just (SpecimenDefinitionR o)) = Just ( SpecimenDefinitionDR o)
--  toDomainResourceC (Just (StructureDefinitionR o)) = Just ( StructureDefinitionDR o)
--  toDomainResourceC (Just (StructureMapR o)) = Just ( StructureMapDR o)
--  toDomainResourceC (Just (SubscriptionR o)) = Just ( SubscriptionDR o)
--  toDomainResourceC (Just (SubstanceR o)) = Just ( SubstanceDR o)
--  toDomainResourceC (Just (SubstancePolymerR o)) = Just ( SubstancePolymerDR o)
--  toDomainResourceC (Just (SubstanceProteinR o)) = Just ( SubstanceProteinDR o)
--  toDomainResourceC (Just (SubstanceReferenceInformationR o)) = Just ( SubstanceReferenceInformationDR o)
--  toDomainResourceC (Just (SubstanceSpecificationR o)) = Just ( SubstanceSpecificationDR o)
--  toDomainResourceC (Just (SubstanceSourceMaterialR o)) = Just ( SubstanceSourceMaterialDR o)
--  toDomainResourceC (Just (SupplyDeliveryR o)) = Just ( SupplyDeliveryDR o)
--  toDomainResourceC (Just (SupplyRequestR o)) = Just ( SupplyRequestDR o)
--  toDomainResourceC (Just (TaskR o)) = Just ( TaskDR o)
--  toDomainResourceC (Just (TerminologyCapabilitiesR o)) = Just ( TerminologyCapabilitiesDR o)
--  toDomainResourceC (Just (TestReportR o)) = Just ( TestReportDR o)
--  toDomainResourceC (Just (TestScriptR o)) = Just ( TestScriptDR o)
--  toDomainResourceC (Just (ValueSetR o)) = Just ( ValueSetDR o)
--  toDomainResourceC (Just (VerificationResultR o)) = Just ( VerificationResultDR o)
--  toDomainResourceC (Just (VisionPrescriptionR o)) = Just ( VisionPrescriptionDR o)
toDomainResourceC (Just (UserConfigR o)) = Just ( UserConfigDR o)
toDomainResourceC (Just (LeaveR o)) = Just ( LeaveDR o)
toDomainResourceC (Just (ICalendarR o)) = Just ( ICalendarDR o)
toDomainResourceC _ = error "toDomainResourceC: type nyi"
