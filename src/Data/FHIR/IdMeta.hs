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

module Data.FHIR.IdMeta
  (
    getId
  , setId
  , getMeta
  , setMeta
  , getVersionId
  , setVersionId
  , getLastUpdated
  , setLastUpdated
  , getTag
  , setTag
  ) where

import RIO
--import qualified RIO.Text as T

import Data.FHIR.Datatypes
import Data.FHIR.Datatypes.ResourceTypes
import Data.FHIR.Resources
import Data.FHIR.Resources.OperationOutcome
import Data.FHIR.Resources.Resource

getId :: Resource -> Maybe Text
getId r = case r of
--    AccountR o -> accountId o
    ActivityDefinitionR o -> activityDefinitionId o
--    AdverseEventR o -> adverseEventId o
--    AllergyIntoleranceR o -> allergyIntoleranceId o
--    AppointmentR o -> appointmentId o
--    AppointmentResponseR o -> appointmentResponseId o
--    AuditEventR o -> auditEventId o
--    BasicR o -> basicId o
    BinaryR o -> binaryId o
--    BiologicallyDerivedProductR o -> biologicallyDerivedProductId o
--    BodyStructureR o -> bodyStructureId o
    BundleR o -> bundleId o
    CapabilityStatementR o -> capabilityStatementId o
--    CarePlanR o -> carePlanId o
--    CareTeamR o -> careTeamId o
--    CatalogEntryR o -> catalogEntryId o
--    ChargeItemR o -> chargeItemId o
--    ChargeItemDefinitionR o -> chargeItemDefinitionId o
--    ClaimR o -> claimId o
----    ClaimResponseR o -> claimResponseId o
--    ClinicalImpressionR o -> clinicalImpressionId o
--    CodeSystemR o -> codeSystemId o
--    CommunicationR o -> communicationId o
--    CommunicationRequestR o -> communicationRequestId o
--    CompartmentDefinitionR o -> compartmentDefinitionId o
--    CompositionR o -> compositionId o
--    ConceptMapR o -> conceptMapId o
--    ConditionR o -> conditionId o
--    ConsentR o -> consentId o
--    ContractR o -> contractId o
--    CoverageR o -> coverageId o
--    CoverageEligibilityRequestR o -> coverageEligibilityRequestId o
--    CoverageEligibilityResponseR o -> coverageEligibilityResponseId o
--    DetectedIssueR o -> detectedIssueId o
--    DeviceR o -> deviceId o
--    DeviceDefinitionR o -> deviceDefinitionId o
--    DeviceMetricR o -> deviceMetricId o
--    DeviceRequestR o -> deviceRequestId o
--    DeviceUseStatementR o -> deviceUseStatementId o
--    DiagnosticReportR o -> diagnosticReportId o
--    DocumentManifestR o -> documentManifestId o
--    DocumentReferenceR o -> documentReferenceId o
--    EffectEvidenceSynthesisR o -> effectEvidenceSynthesisId o
    EncounterR o -> encounterId o
--    EndpointR o -> endpointId o
--    EnrollmentRequestR o -> enrollmentRequestId o
--    EnrollmentResponseR o -> enrollmentResponseId o
--    EpisodeOfCareR o -> episodeOfCareId o
--    EventDefinitionR o -> eventDefinitionId o
--    EvidenceR o -> evidenceId o
--    EvidenceVariableR o -> evidenceVariableId o
--    ExampleScenarioR o -> exampleScenarioId o
--    ExplanationOfBenefitR o -> explanationOfBenefitId o
--    FamilyMemberHistoryR o -> familyMemberHistoryId o
--    FlagR o -> flagId o
--    GoalR o -> goalId o
--    GraphDefinitionR o -> graphDefinitionId o
--    GroupR o -> groupId o
--    GuidanceResponseR o -> guidanceResponseId o
--    HealthcareServiceR o -> healthcareServiceId o
--    ImagingStudyR o -> imagingStudyId o
--    ImmunizationR o -> immunizationId o
--    ImmunizationEvaluationR o -> immunizationEvaluationId o
--    ImmunizationRecommendationR o -> immunizationRecommendationId o
--    ImplementationGuideR o -> implementationGuideId o
--    InsurancePlanR o -> insurancePlanId o
--    InvoiceR o -> invoiceId o
--    LibraryR o -> libraryId o
--    LinkageR o -> linkageId o
--    ListR o -> listId o
--    LocationR o -> locationId o
--    MeasureR o -> MeasureId o
--    MeasureReportR o -> measureReportId o
--    MediaR o -> mediaId o
--    MedicationR o -> medicationId o
--    MedicationAdministrationR o -> medicationAdministrationId o
--    MedicationDispenseR o -> medicationDispenseId o
--    MedicationKnowledgeR o -> medicationKnowledgeId o
--    MedicationRequestR o -> medicationRequestId o
--    MedicationStatementR o -> medicationStatementId o
--    MedicinalProductR o -> medicinalProductId o
--    MedicinalProductAuthorizationR o -> medicinalProductAuthorizationId o
--    MedicinalProductContraindicationR o -> medicinalProductContraindicationId o
--    MedicinalProductIndicationR o -> medicinalProductIndicationId o
--    MedicinalProductIngredientR o -> medicinalProductIngredientId o
--    MedicinalProductInteractionR o -> medicinalProductInteractionId o
--    MedicinalProductManufacturedR o -> medicinalProductManufacturedId o
--    MedicinalProductPackagedR o -> medicinalProductPackagedId o
--    MedicinalProductPharmaceuticalR o -> medicinalProductPharmaceuticalId o
--    MedicinalProductUndesirableEffectR o -> medicinalProductUndesirableEffectId o
--    MessageDefinitionR o -> messageDefinitionId o
--    MessageHeaderR o -> messageHeaderId o
--    MolecularSequenceR o -> molecularSequenceId o
--    NamingSystemR o -> namingSystemId o
--    NutritionOrderR o -> NutritionOrderId o
--    ObservationR o -> observationId o
--    ObservationDefinitionR o -> observationDefinitionId o
--    OperationDefinitionR o -> operationDefinitionId o
    OperationOutcomeR o -> operationOutcomeId o
--    OrganizationR o -> organizationId o
--    OrganizationAffiliationR o -> organizationAffiliationId o
--    ParametersR o -> parametersId o
    PatientR o -> patientId o
--    PaymentNoticeR o -> paymentNoticeId o
--    PaymentReconciliationR o -> paymentReconciliationId o
--    PersonR o -> personId o
--    PlanDefinitionR o -> planDefinitionId o
--    PractitionerR o -> practitionerId o
--    PractitionerRoleR o -> practitionerRoleId o
--    ProcedureR o -> procedureId o
--    ProvenanceR o -> provenanceId o
--    QuestionnaireR o -> questionnaireId o
--    QuestionnaireResponseR o -> questionnaireResponseId o
--    RelatedPersonR o -> relatedPersonId o
--    RequestGroupR o -> requestGroupId o
--    ResearchDefinitionR o -> researchDefinitionId o
--    ResearchElementDefinitionR o -> researchElementDefinitionId o
--    ResearchStudyR o -> researchStudyId o
--    ResearchSubjectR o -> researchSubjectId o
--    RiskAssessmentR o -> riskAssessmentId o
--    RiskEvidenceSynthesisR o -> riskEvidenceSynthesisId o
--    ScheduleR o -> scheduleId o
--    SearchParameterR o -> searchParameterId o
--    ServiceRequestR o -> serviceRequestId o
--    SlotR o -> slotId o
--    SpecimenR o -> specimenId o
--    SpecimenDefinitionR o -> specimenDefinitionId o
--    StructureDefinitionR o -> structureDefinitionId o
--    StructureMapR o -> structureMapId o
--    SubscriptionR o -> subscriptionId o
--    SubstanceR o -> substanceId o
--    SubstancePolymerR o -> substancePolymerId o
--    SubstanceProteinR o -> substanceProteinId o
--    SubstanceReferenceInformationR o -> substanceReferenceInformationId o
--    SubstanceSpecificationR o -> substanceSpecificationId o
--    SubstanceSourceMaterialR o -> substanceSourceMaterialId o
--    SupplyDeliveryR o -> supplyDeliveryId o
--    SupplyRequestR o -> supplyRequestId o
--    TaskR o -> taskId o
--    TerminologyCapabilitiesR o -> terminologyCapabilitiesId o
--    TestReportR o -> testReportId o
--    TestScriptR o -> testScriptId o
--    ValueSetR o -> valueSetId o
--    VerificationResultR o -> verificationResultId o
--    VisionPrescriptionR o -> visionPrescriptionId o
    UserConfigR o -> userConfigId o
    LeaveR o -> leaveId o
    ICalendarR o -> iCalendarId o
    _ -> error "getId: type nyi"

setId :: Maybe Id -> Resource -> Resource
setId id r = case r of
--    AccountR o -> AccountR $ o{accountId= id}
    ActivityDefinitionR o -> ActivityDefinitionR $ o{activityDefinitionId= id}
--    AdverseEventR o -> AdverseEventR $ o{adverseEventId= id}
--    AllergyIntoleranceR o -> AllergyIntoleranceR $ o{allergyIntoleranceId= id}
--    AppointmentR o -> AppointmentR $ o{appointmentId= id}
--    AppointmentResponseR o -> AppointmentResponseR $ o{appointmentResponseId= id}
--    AuditEventR o -> AuditEventR $ o{auditEventId= id}
--    BasicR o -> BasicR $ o{basicId= id}
    BinaryR o -> BinaryR $ o{binaryId= id}
--    BiologicallyDerivedProductR o -> BiologicallyDerivedProductR $ o{biologicallyDerivedProductId= id}
--    BodyStructureR o -> BodyStructureR $ o{bodyStructureId= id}
    BundleR o -> BundleR $ o{bundleId= id}
    CapabilityStatementR o -> CapabilityStatementR $ o{capabilityStatementId= id}
--    CarePlanR o -> CarePlanR $ o{carePlanId= id}
--    CareTeamR o -> CareTeamR $ o{careTeamId= id}
--    CatalogEntryR o -> CatalogEntryR $ o{catalogEntryId= id}
--    ChargeItemR o -> ChargeItemR $ o{chargeItemId= id}
--    ChargeItemDefinitionR o -> ChargeItemDefinitionR $ o{chargeItemDefinitionId= id}
--    ClaimR o -> ClaimR $ o{claimId= id}
--    ClaimResponseR o -> ClaimResponseR $ o{claimResponseId= id}
--    ClinicalImpressionR o -> ClinicalImpressionR $ o{clinicalImpressionId= id}
--    CodeSystemR o -> CodeSystemR $ o{codeSystemId= id}
--    CommunicationR o -> CommunicationR $ o{communicationId= id}
--    CommunicationRequestR o -> CommunicationRequestR $ o{communicationRequestId= id}
--    CompartmentDefinitionR o -> CompartmentDefinitionR $ o{compartmentDefinitionId= id}
--    CompositionR o -> CompositionR $ o{compositionId= id}
--    ConceptMapR o -> ConceptMapR $ o{conceptMapId= id}
--    ConditionR o -> ConditionR $ o{conditionId= id}
--    ConsentR o -> ConsentR $ o{consentId= id}
--    ContractR o -> ContractR $ o{contractId= id}
--    CoverageR o -> CoverageR $ o{coverageId= id}
--    CoverageEligibilityRequestR o -> CoverageEligibilityRequestR $ o{coverageEligibilityRequestId= id}
--    CoverageEligibilityResponseR o -> CoverageEligibilityResponseR $ o{coverageEligibilityResponseId= id}
--    DetectedIssueR o -> DetectedIssueR $ o{detectedIssueId= id}
--    DeviceR o -> DeviceR $ o{deviceId= id}
--    DeviceDefinitionR o -> DeviceDefinitionR $ o{deviceDefinitionId= id}
--    DeviceMetricR o -> DeviceMetricR $ o{deviceMetricId= id}
--    DeviceRequestR o -> DeviceRequestR $ o{deviceRequestId= id}
--    DeviceUseStatementR o -> DeviceUseStatementR $ o{deviceUseStatementId= id}
--    DiagnosticReportR o -> DiagnosticReportR $ o{diagnosticReportId= id}
--    DocumentManifestR o -> DocumentManifestR $ o{documentManifestId= id}
--    DocumentReferenceR o -> DocumentReferenceR $ o{documentReferenceId= id}
--    EffectEvidenceSynthesisR o -> EffectEvidenceSynthesisR $ o{effectEvidenceSynthesisId= id}
    EncounterR o -> EncounterR $ o{encounterId= id}
--    EndpointR o -> EndpointR $ o{endpointId= id}
--    EnrollmentRequestR o -> EnrollmentRequestR $ o{enrollmentRequestId= id}
--    EnrollmentResponseR o -> EnrollmentResponseR $ o{enrollmentResponseId= id}
--    EpisodeOfCareR o -> EpisodeOfCareR $ o{episodeOfCareId= id}
--    EventDefinitionR o -> EventDefinitionR $ o{eventDefinitionId= id}
--    EvidenceR o -> EvidenceR $ o{evidenceId= id}
--    EvidenceVariableR o -> EvidenceVariableR $ o{evidenceVariableId= id}
--    ExampleScenarioR o -> ExampleScenarioR $ o{exampleScenarioId= id}
--    ExplanationOfBenefitR o -> ExplanationOfBenefitR $ o{explanationOfBenefitId= id}
--    FamilyMemberHistoryR o -> FamilyMemberHistoryR $ o{familyMemberHistoryId= id}
--    FlagR o -> FlagR $ o{flagId= id}
--    GoalR o -> GoalR $ o{goalId= id}
--    GraphDefinitionR o -> GraphDefinitionR $ o{graphDefinitionId= id}
--    GroupR o -> GroupR $ o{groupId= id}
--    GuidanceResponseR o -> GuidanceResponseR $ o{guidanceResponseId= id}
--    HealthcareServiceR o -> HealthcareServiceR $ o{healthcareServiceId= id}
--    ImagingStudyR o -> ImagingStudyR $ o{imagingStudyId= id}
--    ImmunizationR o -> ImmunizationR $ o{immunizationId= id}
--    ImmunizationEvaluationR o -> ImmunizationEvaluationR $ o{immunizationEvaluationId= id}
--    ImmunizationRecommendationR o -> ImmunizationRecommendationR $ o{immunizationRecommendationId= id}
--    ImplementationGuideR o -> ImplementationGuideR $ o{implementationGuideId= id}
--    InsurancePlanR o -> InsurancePlanR $ o{insurancePlanId= id}
--    InvoiceR o -> InvoiceR $ o{invoiceId= id}
--    LibraryR o -> LibraryR $ o{libraryId= id}
--    LinkageR o -> LinkageR $ o{linkageId= id}
--    ListR o -> ListR $ o{listId= id}
--    LocationR o -> LocationR $ o{locationId= id}
--    MeasureR o -> MeasureR $ o{measureId= id}
--    MeasureReportR o -> MeasureReportR $ o{measureReportId= id}
--    MediaR o -> MediaR $ o{mediaId= id}
--    MedicationR o -> MedicationR $ o{medicationId= id}
--    MedicationAdministrationR o -> MedicationAdministrationR $ o{medicationAdministrationId= id}
--    MedicationDispenseR o -> MedicationDispenseR $ o{medicationDispenseId= id}
--    MedicationKnowledgeR o -> MedicationKnowledgeR $ o{medicationKnowledgeId= id}
--    MedicationRequestR o -> MedicationRequestR $ o{medicationRequestId= id}
--    MedicationStatementR o -> MedicationStatementR $ o{medicationStatementId= id}
--    MedicinalProductR o -> MedicinalProductR $ o{medicinalProductId= id}
--    MedicinalProductAuthorizationR o -> MedicinalProductAuthorizationR $ o{medicinalProductAuthorizationId= id}
--    MedicinalProductContraindicationR o -> MedicinalProductContraindicationR $ o{medicinalProductContraindicationId= id}
--    MedicinalProductIndicationR o -> MedicinalProductIndicationR $ o{medicinalProductIndicationId= id}
--    MedicinalProductIngredientR o -> MedicinalProductIngredientR $ o{medicinalProductIngredientId= id}
--    MedicinalProductInteractionR o -> MedicinalProductInteractionR $ o{medicinalProductInteractionId= id}
--    MedicinalProductManufacturedR o -> MedicinalProductManufacturedR $ o{medicinalProductManufacturedId= id}
--    MedicinalProductPackagedR o -> MedicinalProductPackagedR $ o{medicinalProductPackagedId= id}
--    MedicinalProductPharmaceuticalR o -> MedicinalProductPharmaceuticalR $ o{medicinalProductPharmaceuticalId= id}
--    MedicinalProductUndesirableEffectR o -> MedicinalProductUndesirableEffectR $ o{medicinalProductUndesirableEffectId= id}
--    MessageDefinitionR o -> MessageDefinitionR $ o{messageDefinitionId= id}
--    MessageHeaderR o -> MessageHeaderR $ o{messageHeaderId= id}
--    MolecularSequenceR o -> MolecularSequenceR $ o{molecularSequenceId= id}
--    NamingSystemR o -> NamingSystemR $ o{namingSystemId= id}
--    NutritionOrderR o -> NutritionOrderR $ o{nutritionOrderId= id}
--    ObservationR o -> ObservationR $ o{observationId= id}
--    ObservationDefinitionR o -> ObservationDefinitionR $ o{observationDefinitionId= id}
--    OperationDefinitionR o -> OperationDefinitionR $ o{operationDefinitionId= id}
    OperationOutcomeR o -> OperationOutcomeR $ o{operationOutcomeId= id}
--    OrganizationR o -> OrganizationR $ o{organizationId= id}
--    OrganizationAffiliationR o -> OrganizationAffiliationR $ o{organizationAffiliationId= id}
--    ParametersR o -> ParametersR $ o{parametersId= id}
    PatientR o -> PatientR $ o{patientId= id}
--    PaymentNoticeR o -> PaymentNoticeR $ o{paymentNoticeId= id}
--    PaymentReconciliationR o -> PaymentReconciliationR $ o{paymentReconciliationId= id}
--    PersonR o -> PersonR $ o{personId= id}
--    PlanDefinitionR o -> PlanDefinitionR $ o{planDefinitionId= id}
--    PractitionerR o -> PractitionerR $ o{practitionerId= id}
--    PractitionerRoleR o -> PractitionerRoleR $ o{practitionerRoleId= id}
--    ProcedureR o -> ProcedureR $ o{procedureId= id}
--    ProvenanceR o -> ProvenanceR $ o{provenanceId= id}
--    QuestionnaireR o -> QuestionnaireR $ o{questionnaireId= id}
--    QuestionnaireResponseR o -> QuestionnaireResponseR $ o{questionnaireResponseId= id}
--    RelatedPersonR o -> RelatedPersonR $ o{relatedPersonId= id}
--    RequestGroupR o -> RequestGroupR $ o{requestGroupId= id}
--    ResearchDefinitionR o -> ResearchDefinitionR $ o{researchDefinitionId= id}
--    ResearchElementDefinitionR o -> ResearchElementDefinitionR $ o{researchElementDefinitionId= id}
--    ResearchStudyR o -> ResearchStudyR $ o{researchStudyId= id}
--    ResearchSubjectR o -> ResearchSubjectR $ o{researchSubjectId= id}
--    RiskAssessmentR o -> RiskAssessmentR $ o{riskAssessmentId= id}
--    RiskEvidenceSynthesisR o -> RiskEvidenceSynthesisR $ o{riskEvidenceSynthesisId= id}
--    ScheduleR o -> ScheduleR $ o{scheduleId= id}
--    SearchParameterR o -> SearchParameterR $ o{searchParameterId= id}
--    ServiceRequestR o -> ServiceRequestR $ o{serviceRequestId= id}
--    SlotR o -> SlotR $ o{slotId= id}
--    SpecimenR o -> SpecimenR $ o{specimenId= id}
--    SpecimenDefinitionR o -> SpecimenDefinitionR $ o{specimenDefinitionId= id}
--    StructureDefinitionR o -> StructureDefinitionR $ o{structureDefinitionId= id}
--    StructureMapR o -> StructureMapR $ o{structureMapId= id}
--    SubscriptionR o -> SubscriptionR $ o{subscriptionId= id}
--    SubstanceR o -> SubstanceR $ o{substanceId= id}
--    SubstancePolymerR o -> SubstancePolymerR $ o{substancePolymerId= id}
--    SubstanceProteinR o -> SubstanceProteinR $ o{substanceProteinId= id}
--    SubstanceReferenceInformationR o -> SubstanceReferenceInformationR $ o{substanceReferenceInformationId= id}
--    SubstanceSpecificationR o -> SubstanceSpecificationR $ o{substanceSpecificationId= id}
--    SubstanceSourceMaterialR o -> SubstanceSourceMaterialR $ o{substanceSourceMaterialId= id}
--    SupplyDeliveryR o -> SupplyDeliveryR $ o{supplyDeliveryId= id}
--    SupplyRequestR o -> SupplyRequestR $ o{supplyRequestId= id}
--    TaskR o -> TaskR $ o{taskId= id}
--    TerminologyCapabilitiesR o -> TerminologyCapabilitiesR $ o{terminologyCapabilitiesId= id}
--    TestReportR o -> TestReportR $ o{testReportId= id}
--    TestScriptR o -> TestScriptR $ o{testScriptId= id}
--    ValueSetR o -> ValueSetR $ o{valueSetId= id}
--    VerificationResultR o -> VerificationResultR $ o{verificationResultId= id}
--    VisionPrescriptionR o -> VisionPrescriptionR $ o{visionPrescriptionId= id}
    UserConfigR o -> UserConfigR $ o{userConfigId= id}
    LeaveR o -> LeaveR $ o{leaveId= id}
    ICalendarR o -> ICalendarR $ o{iCalendarId= id}
    _ -> error "setId: type nyi"

getMeta :: Resource -> Maybe Meta
getMeta r = case r of
--    AccountR o -> accountMeta o
    ActivityDefinitionR o -> activityDefinitionMeta o
--    AdverseEventR o -> adverseEventMeta o
--    AllergyIntoleranceR o -> allergyIntoleranceMeta o
--    AppointmentR o -> appointmentMeta o
--    AppointmentResponseR o -> appointmentResponseMeta o
--    AuditEventR o -> auditEventMeta o
--    BasicR o -> basicMeta o
    BinaryR o -> binaryMeta o
--    BiologicallyDerivedProductR o -> biologicallyDerivedProductMeta o
--    BodyStructureR o -> bodyStructureMeta o
    BundleR o -> bundleMeta o
    CapabilityStatementR o -> capabilityStatementMeta o
--    CarePlanR o -> carePlanMeta o
--    CareTeamR o -> careTeamMeta o
--    CatalogEntryR o -> catalogEntryMeta o
--    ChargeItemR o -> chargeItemMeta o
--    ChargeItemDefinitionR o -> chargeItemDefinitionMeta o
--    ClaimR o -> claimMeta o
--    ClaimResponseR o -> claimResponseMeta o
--    ClinicalImpressionR o -> clinicalImpressionMeta o
--    CodeSystemR o -> codeSystemMeta o
--    CommunicationR o -> communicationMeta o
--    CommunicationRequestR o -> communicationRequestMeta o
--    CompartmentDefinitionR o -> compartmentDefinitionMeta o
--    CompositionR o -> compositionMeta o
--    ConceptMapR o -> conceptMapMeta o
--    ConditionR o -> conditionMeta o
--    ConsentR o -> consentMeta o
--    ContractR o -> contractMeta o
--    CoverageR o -> coverageMeta o
--    CoverageEligibilityRequestR o -> coverageEligibilityRequestMeta o
--    CoverageEligibilityResponseR o -> coverageEligibilityResponseMeta o
--    DetectedIssueR o -> detectedIssueMeta o
--    DeviceR o -> deviceMeta o
--    DeviceDefinitionR o -> deviceDefinitionMeta o
--    DeviceMetricR o -> deviceMetricMeta o
--    DeviceRequestR o -> deviceRequestMeta o
--    DeviceUseStatementR o -> deviceUseStatementMeta o
--    DiagnosticReportR o -> diagnosticReportMeta o
--    DocumentManifestR o -> documentManifestMeta o
--    DocumentReferenceR o -> documentReferenceMeta o
--    EffectEvidenceSynthesisR o -> effectEvidenceSynthesisMeta o
    EncounterR o -> encounterMeta o
--    EndpointR o -> endpointMeta o
--    EnrollmentRequestR o -> enrollmentRequestMeta o
--    EnrollmentResponseR o -> enrollmentResponseMeta o
--    EpisodeOfCareR o -> episodeOfCareMeta o
--    EventDefinitionR o -> eventDefinitionMeta o
--    EvidenceR o -> evidenceMeta o
--    EvidenceVariableR o -> evidenceVariableMeta o
--    ExampleScenarioR o -> exampleScenarioMeta o
--    ExplanationOfBenefitR o -> explanationOfBenefitMeta o
--    FamilyMemberHistoryR o -> familyMemberHistoryMeta o
--    FlagR o -> flagMeta o
--    GoalR o -> goalMeta o
--    GraphDefinitionR o -> graphDefinitionMeta o
--    GroupR o -> groupMeta o
--    GuidanceResponseR o -> guidanceResponseMeta o
--    HealthcareServiceR o -> healthcareServiceMeta o
--    ImagingStudyR o -> imagingStudyMeta o
--    ImmunizationR o -> immunizationMeta o
--    ImmunizationEvaluationR o -> immunizationEvaluationMeta o
--    ImmunizationRecommendationR o -> immunizationRecommendationMeta o
--    ImplementationGuideR o -> implementationGuideMeta o
--    InsurancePlanR o -> insurancePlanMeta o
--    InvoiceR o -> invoiceMeta o
--    LibraryR o -> libraryMeta o
--    LinkageR o -> linkageMeta o
--    ListR o -> listMeta o
--    LocationR o -> locationMeta o
--    MeasureR o -> MeasureMeta o
--    MeasureReportR o -> measureReportMeta o
--    MediaR o -> mediaMeta o
--    MedicationR o -> medicationMeta o
--    MedicationAdministrationR o -> medicationAdministrationMeta o
--    MedicationDispenseR o -> medicationDispenseMeta o
--    MedicationKnowledgeR o -> medicationKnowledgeMeta o
--    MedicationRequestR o -> medicationRequestMeta o
--    MedicationStatementR o -> medicationStatementMeta o
--    MedicinalProductR o -> medicinalProductMeta o
--    MedicinalProductAuthorizationR o -> medicinalProductAuthorizationMeta o
--    MedicinalProductContraindicationR o -> medicinalProductContraindicationMeta o
--    MedicinalProductIndicationR o -> medicinalProductIndicationMeta o
--    MedicinalProductIngredientR o -> medicinalProductIngredientMeta o
--    MedicinalProductInteractionR o -> medicinalProductInteractionMeta o
--    MedicinalProductManufacturedR o -> medicinalProductManufacturedMeta o
--    MedicinalProductPackagedR o -> medicinalProductPackagedMeta o
--    MedicinalProductPharmaceuticalR o -> medicinalProductPharmaceuticalMeta o
--    MedicinalProductUndesirableEffectR o -> medicinalProductUndesirableEffectMeta o
--    MessageDefinitionR o -> messageDefinitionMeta o
--    MessageHeaderR o -> messageHeaderMeta o
--    MolecularSequenceR o -> molecularSequenceMeta o
--    NamingSystemR o -> namingSystemMeta o
--    NutritionOrderR o -> NutritionOrderMeta o
--    ObservationR o -> observationMeta o
--    ObservationDefinitionR o -> observationDefinitionMeta o
--    OperationDefinitionR o -> operationDefinitionMeta o
    OperationOutcomeR o -> operationOutcomeMeta o
--    OrganizationR o -> organizationMeta o
--    OrganizationAffiliationR o -> organizationAffiliationMeta o
--    ParametersR o -> parametersMeta o
    PatientR o -> patientMeta o
--    PaymentNoticeR o -> paymentNoticeMeta o
--    PaymentReconciliationR o -> paymentReconciliationMeta o
--    PersonR o -> personMeta o
--    PlanDefinitionR o -> planDefinitionMeta o
--    PractitionerR o -> practitionerMeta o
--    PractitionerRoleR o -> practitionerRoleMeta o
--    ProcedureR o -> procedureMeta o
--    ProvenanceR o -> provenanceMeta o
--    QuestionnaireR o -> questionnaireMeta o
--    QuestionnaireResponseR o -> questionnaireResponseMeta o
--    RelatedPersonR o -> relatedPersonMeta o
--    RequestGroupR o -> requestGroupMeta o
--    ResearchDefinitionR o -> researchDefinitionMeta o
--    ResearchElementDefinitionR o -> researchElementDefinitionMeta o
--    ResearchStudyR o -> researchStudyMeta o
--    ResearchSubjectR o -> researchSubjectMeta o
--    RiskAssessmentR o -> riskAssessmentMeta o
--    RiskEvidenceSynthesisR o -> riskEvidenceSynthesisMeta o
--    ScheduleR o -> scheduleMeta o
--    SearchParameterR o -> searchParameterMeta o
--    ServiceRequestR o -> serviceRequestMeta o
--    SlotR o -> slotMeta o
--    SpecimenR o -> specimenMeta o
--    SpecimenDefinitionR o -> specimenDefinitionMeta o
--    StructureDefinitionR o -> structureDefinitionMeta o
--    StructureMapR o -> structureMapMeta o
--    SubscriptionR o -> subscriptionMeta o
--    SubstanceR o -> substanceMeta o
--    SubstancePolymerR o -> substancePolymerMeta o
--    SubstanceProteinR o -> substanceProteinMeta o
--    SubstanceReferenceInformationR o -> substanceReferenceInformationMeta o
--    SubstanceSpecificationR o -> substanceSpecificationMeta o
--    SubstanceSourceMaterialR o -> substanceSourceMaterialMeta o
--    SupplyDeliveryR o -> supplyDeliveryMeta o
--    SupplyRequestR o -> supplyRequestMeta o
--    TaskR o -> taskMeta o
--    TerminologyCapabilitiesR o -> terminologyCapabilitiesMeta o
--    TestReportR o -> testReportMeta o
--    TestScriptR o -> testScriptMeta o
--    ValueSetR o -> valueSetMeta o
--    VerificationResultR o -> verificationResultMeta o
--    VisionPrescriptionR o -> visionPrescriptionMeta o
--    UserConfigR o -> userConfigMeta o
--    LeaveR o -> leaveMeta o
    ICalendarR o -> iCalendarMeta o
    _ -> error "getMeta: type nyi"

setMeta :: Maybe Meta -> Resource -> Resource
setMeta meta r = case r of
--    AccountR o -> AccountR $ o{accountMeta= meta}
    ActivityDefinitionR o -> ActivityDefinitionR $ o{activityDefinitionMeta= meta}
--    AdverseEventR o -> AdverseEventR $ o{adverseEventMeta= meta}
--    AllergyIntoleranceR o -> AllergyIntoleranceR $ o{allergyIntoleranceMeta= meta}
--    AppointmentR o -> AppointmentR $ o{appointmentMeta= meta}
--    AppointmentResponseR o -> AppointmentResponseR $ o{appointmentResponseMeta= meta}
--    AuditEventR o -> AuditEventR $ o{auditEventMeta= meta}
--    BasicR o -> BasicR $ o{basicMeta= meta}
    BinaryR o -> BinaryR $ o{binaryMeta= meta}
--    BiologicallyDerivedProductR o -> BiologicallyDerivedProductR $ o{biologicallyDerivedProductMeta= meta}
--    BodyStructureR o -> BodyStructureR $ o{bodyStructureMeta= meta}
    BundleR o -> BundleR $ o{bundleMeta= meta}
    CapabilityStatementR o -> CapabilityStatementR $ o{capabilityStatementMeta= meta}
--    CarePlanR o -> CarePlanR $ o{carePlanMeta= meta}
--    CareTeamR o -> CareTeamR $ o{careTeamMeta= meta}
--    CatalogEntryR o -> CatalogEntryR $ o{catalogEntryMeta= meta}
--    ChargeItemR o -> ChargeItemR $ o{chargeItemMeta= meta}
--    ChargeItemDefinitionR o -> ChargeItemDefinitionR $ o{chargeItemDefinitionMeta= meta}
--    ClaimR o -> ClaimR $ o{claimMeta= meta}
--    ClaimResponseR o -> ClaimResponseR $ o{claimResponseMeta= meta}
--    ClinicalImpressionR o -> ClinicalImpressionR $ o{clinicalImpressionMeta= meta}
--    CodeSystemR o -> CodeSystemR $ o{codeSystemMeta= meta}
--    CommunicationR o -> CommunicationR $ o{communicationMeta= meta}
--    CommunicationRequestR o -> CommunicationRequestR $ o{communicationRequestMeta= meta}
--    CompartmentDefinitionR o -> CompartmentDefinitionR $ o{compartmentDefinitionMeta= meta}
--    CompositionR o -> CompositionR $ o{compositionMeta= meta}
--    ConceptMapR o -> ConceptMapR $ o{conceptMapMeta= meta}
--    ConditionR o -> ConditionR $ o{conditionMeta= meta}
--    ConsentR o -> ConsentR $ o{consentMeta= meta}
--    ContractR o -> ContractR $ o{contractMeta= meta}
--    CoverageR o -> CoverageR $ o{coverageMeta= meta}
--    CoverageEligibilityRequestR o -> CoverageEligibilityRequestR $ o{coverageEligibilityRequestMeta= meta}
--    CoverageEligibilityResponseR o -> CoverageEligibilityResponseR $ o{coverageEligibilityResponseMeta= meta}
--    DetectedIssueR o -> DetectedIssueR $ o{detectedIssueMeta= meta}
--    DeviceR o -> DeviceR $ o{deviceMeta= meta}
--    DeviceDefinitionR o -> DeviceDefinitionR $ o{deviceDefinitionMeta= meta}
--    DeviceMetricR o -> DeviceMetricR $ o{deviceMetricMeta= meta}
--    DeviceRequestR o -> DeviceRequestR $ o{deviceRequestMeta= meta}
--    DeviceUseStatementR o -> DeviceUseStatementR $ o{deviceUseStatementMeta= meta}
--    DiagnosticReportR o -> DiagnosticReportR $ o{diagnosticReportMeta= meta}
--    DocumentManifestR o -> DocumentManifestR $ o{documentManifestMeta= meta}
--    DocumentReferenceR o -> DocumentReferenceR $ o{documentReferenceMeta= meta}
--    EffectEvmetaenceSynthesisR o -> EffectEvidenceSynthesisR $ o{effectEvidenceSynthesisMeta= id}
    EncounterR o -> EncounterR $ o{encounterMeta= meta}
--    EndpointR o -> EndpointR $ o{endpointMeta= meta}
--    EnrollmentRequestR o -> EnrollmentRequestR $ o{enrollmentRequestMeta= meta}
--    EnrollmentResponseR o -> EnrollmentResponseR $ o{enrollmentResponseMeta= meta}
--    EpisodeOfCareR o -> EpisodeOfCareR $ o{episodeOfCareMeta= meta}
--    EventDefinitionR o -> EventDefinitionR $ o{eventDefinitionMeta= meta}
--    EvmetaenceR o -> EvidenceR $ o{evidenceMeta= id}
--    EvmetaenceVariableR o -> EvidenceVariableR $ o{evidenceVariableMeta= id}
--    ExampleScenarioR o -> ExampleScenarioR $ o{exampleScenarioMeta= meta}
--    ExplanationOfBenefitR o -> ExplanationOfBenefitR $ o{explanationOfBenefitMeta= meta}
--    FamilyMemberHistoryR o -> FamilyMemberHistoryR $ o{familyMemberHistoryMeta= meta}
--    FlagR o -> FlagR $ o{flagMeta= meta}
--    GoalR o -> GoalR $ o{goalMeta= meta}
--    GraphDefinitionR o -> GraphDefinitionR $ o{graphDefinitionMeta= meta}
--    GroupR o -> GroupR $ o{groupMeta= meta}
--    GumetaanceResponseR o -> GuidanceResponseR $ o{guidanceResponseMeta= id}
--    HealthcareServiceR o -> HealthcareServiceR $ o{healthcareServiceMeta= meta}
--    ImagingStudyR o -> ImagingStudyR $ o{imagingStudyMeta= meta}
--    ImmunizationR o -> ImmunizationR $ o{immunizationMeta= meta}
--    ImmunizationEvaluationR o -> ImmunizationEvaluationR $ o{immunizationEvaluationMeta= meta}
--    ImmunizationRecommendationR o -> ImmunizationRecommendationR $ o{immunizationRecommendationMeta= meta}
--    ImplementationGumetaeR o -> ImplementationGuideR $ o{implementationGuideMeta= id}
--    InsurancePlanR o -> InsurancePlanR $ o{insurancePlanMeta= meta}
--    InvoiceR o -> InvoiceR $ o{invoiceMeta= meta}
--    LibraryR o -> LibraryR $ o{libraryMeta= meta}
--    LinkageR o -> LinkageR $ o{linkageMeta= meta}
--    ListR o -> ListR $ o{listMeta= meta}
--    LocationR o -> LocationR $ o{locationMeta= meta}
--    MeasureR o -> MeasureR $ o{measureMeta= meta}
--    MeasureReportR o -> MeasureReportR $ o{measureReportMeta= meta}
--    MediaR o -> MediaR $ o{mediaMeta= meta}
--    MedicationR o -> MedicationR $ o{medicationMeta= meta}
--    MedicationAdministrationR o -> MedicationAdministrationR $ o{medicationAdministrationMeta= meta}
--    MedicationDispenseR o -> MedicationDispenseR $ o{medicationDispenseMeta= meta}
--    MedicationKnowledgeR o -> MedicationKnowledgeR $ o{medicationKnowledgeMeta= meta}
--    MedicationRequestR o -> MedicationRequestR $ o{medicationRequestMeta= meta}
--    MedicationStatementR o -> MedicationStatementR $ o{medicationStatementMeta= meta}
--    MedicinalProductR o -> MedicinalProductR $ o{medicinalProductMeta= meta}
--    MedicinalProductAuthorizationR o -> MedicinalProductAuthorizationR $ o{medicinalProductAuthorizationMeta= meta}
--    MedicinalProductContraindicationR o -> MedicinalProductContraindicationR $ o{medicinalProductContraindicationMeta= meta}
--    MedicinalProductIndicationR o -> MedicinalProductIndicationR $ o{medicinalProductIndicationMeta= meta}
--    MedicinalProductIngredientR o -> MedicinalProductIngredientR $ o{medicinalProductIngredientMeta= meta}
--    MedicinalProductInteractionR o -> MedicinalProductInteractionR $ o{medicinalProductInteractionMeta= meta}
--    MedicinalProductManufacturedR o -> MedicinalProductManufacturedR $ o{medicinalProductManufacturedMeta= meta}
--    MedicinalProductPackagedR o -> MedicinalProductPackagedR $ o{medicinalProductPackagedMeta= meta}
--    MedicinalProductPharmaceuticalR o -> MedicinalProductPharmaceuticalR $ o{medicinalProductPharmaceuticalMeta= meta}
--    MedicinalProductUndesirableEffectR o -> MedicinalProductUndesirableEffectR $ o{medicinalProductUndesirableEffectMeta= meta}
--    MessageDefinitionR o -> MessageDefinitionR $ o{messageDefinitionMeta= meta}
--    MessageHeaderR o -> MessageHeaderR $ o{messageHeaderMeta= meta}
--    MolecularSequenceR o -> MolecularSequenceR $ o{molecularSequenceMeta= meta}
--    NamingSystemR o -> NamingSystemR $ o{namingSystemMeta= meta}
--    NutritionOrderR o -> NutritionOrderR $ o{nutritionOrderMeta= meta}
--    ObservationR o -> ObservationR $ o{observationMeta= meta}
--    ObservationDefinitionR o -> ObservationDefinitionR $ o{observationDefinitionMeta= meta}
--    OperationDefinitionR o -> OperationDefinitionR $ o{operationDefinitionMeta= meta}
    OperationOutcomeR o -> OperationOutcomeR $ o{operationOutcomeMeta= meta}
--    OrganizationR o -> OrganizationR $ o{organizationMeta= meta}
--    OrganizationAffiliationR o -> OrganizationAffiliationR $ o{organizationAffiliationMeta= meta}
--    ParametersR o -> ParametersR $ o{parametersMeta= meta}
    PatientR o -> PatientR $ o{patientMeta= meta}
--    PaymentNoticeR o -> PaymentNoticeR $ o{paymentNoticeMeta= meta}
--    PaymentReconciliationR o -> PaymentReconciliationR $ o{paymentReconciliationMeta= meta}
--    PersonR o -> PersonR $ o{personMeta= meta}
--    PlanDefinitionR o -> PlanDefinitionR $ o{planDefinitionMeta= meta}
--    PractitionerR o -> PractitionerR $ o{practitionerMeta= meta}
--    PractitionerRoleR o -> PractitionerRoleR $ o{practitionerRoleMeta= meta}
--    ProcedureR o -> ProcedureR $ o{procedureMeta= meta}
--    ProvenanceR o -> ProvenanceR $ o{provenanceMeta= meta}
--    QuestionnaireR o -> QuestionnaireR $ o{questionnaireMeta= meta}
--    QuestionnaireResponseR o -> QuestionnaireResponseR $ o{questionnaireResponseMeta= meta}
--    RelatedPersonR o -> RelatedPersonR $ o{relatedPersonMeta= meta}
--    RequestGroupR o -> RequestGroupR $ o{requestGroupMeta= meta}
--    ResearchDefinitionR o -> ResearchDefinitionR $ o{researchDefinitionMeta= meta}
--    ResearchElementDefinitionR o -> ResearchElementDefinitionR $ o{researchElementDefinitionMeta= meta}
--    ResearchStudyR o -> ResearchStudyR $ o{researchStudyMeta= meta}
--    ResearchSubjectR o -> ResearchSubjectR $ o{researchSubjectMeta= meta}
--    RiskAssessmentR o -> RiskAssessmentR $ o{riskAssessmentMeta= meta}
--    RiskEvmetaenceSynthesisR o -> RiskEvidenceSynthesisR $ o{riskEvidenceSynthesisMeta= id}
--    ScheduleR o -> ScheduleR $ o{scheduleMeta= meta}
--    SearchParameterR o -> SearchParameterR $ o{searchParameterMeta= meta}
--    ServiceRequestR o -> ServiceRequestR $ o{serviceRequestMeta= meta}
--    SlotR o -> SlotR $ o{slotMeta= meta}
--    SpecimenR o -> SpecimenR $ o{specimenMeta= meta}
--    SpecimenDefinitionR o -> SpecimenDefinitionR $ o{specimenDefinitionMeta= meta}
--    StructureDefinitionR o -> StructureDefinitionR $ o{structureDefinitionMeta= meta}
--    StructureMapR o -> StructureMapR $ o{structureMapMeta= meta}
--    SubscriptionR o -> SubscriptionR $ o{subscriptionMeta= meta}
--    SubstanceR o -> SubstanceR $ o{substanceMeta= meta}
--    SubstancePolymerR o -> SubstancePolymerR $ o{substancePolymerMeta= meta}
--    SubstanceProteinR o -> SubstanceProteinR $ o{substanceProteinMeta= meta}
--    SubstanceReferenceInformationR o -> SubstanceReferenceInformationR $ o{substanceReferenceInformationMeta= meta}
--    SubstanceSpecificationR o -> SubstanceSpecificationR $ o{substanceSpecificationMeta= meta}
--    SubstanceSourceMaterialR o -> SubstanceSourceMaterialR $ o{substanceSourceMaterialMeta= meta}
--    SupplyDeliveryR o -> SupplyDeliveryR $ o{supplyDeliveryMeta= meta}
--    SupplyRequestR o -> SupplyRequestR $ o{supplyRequestMeta= meta}
--    TaskR o -> TaskR $ o{taskMeta= meta}
--    TerminologyCapabilitiesR o -> TerminologyCapabilitiesR $ o{terminologyCapabilitiesMeta= meta}
--    TestReportR o -> TestReportR $ o{testReportMeta= meta}
--    TestScriptR o -> TestScriptR $ o{testScriptMeta= meta}
--    ValueSetR o -> ValueSetR $ o{valueSetMeta= meta}
--    VerificationResultR o -> VerificationResultR $ o{verificationResultMeta= meta}
--    VisionPrescriptionR o -> VisionPrescriptionR $ o{visionPrescriptionMeta= meta}
    UserConfigR o -> UserConfigR $ o{userConfigMeta= meta}
    LeaveR o -> LeaveR $ o{leaveMeta= meta}
    ICalendarR o -> ICalendarR o{iCalendarMeta= meta}
    _ -> error "setMeta: type nyi"

getVersionId :: Meta -> Maybe Id
getVersionId m = metaVersionId m
setVersionId :: Maybe Id -> Meta -> Meta
setVersionId id m = m{metaVersionId= id}
getLastUpdated :: Meta -> Maybe Instant
getLastUpdated m = metaLastUpdated m
setLastUpdated :: Maybe Instant -> Meta -> Meta
setLastUpdated lu m = m{metaLastUpdated= lu}
getTag :: Meta -> [Coding]
getTag m = metaTag m
setTag :: [Coding] -> Meta -> Meta
setTag ts m = m{metaTag= ts}
