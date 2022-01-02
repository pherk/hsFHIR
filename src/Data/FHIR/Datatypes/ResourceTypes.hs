{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators     #-}

module Data.FHIR.Datatypes.ResourceTypes where

import Data.Aeson  
import Data.Aeson.Types hiding (parseJSON)
import Text.Read (readMaybe)

import qualified Data.Text as T
import qualified Data.UUID as UUID
--import GHC.Generics
import GHC.TypeLits
import Prelude (enumFrom, toEnum)
import RIO
import qualified RIO.Text as T
import qualified RIO.HashMap as HM

--import Data.FHIR.Datatypes
--import Data.FHIR.Resources.OperationOutcome
--import Data.FHIR.Resources.Resource
import qualified Xmlbf as Xmlbf
 
newtype RID = RID UUID.UUID
  deriving (Eq)

instance Show RID where
  show (RID u) = show u

--
--TODO collapse FHIR_Type and ResourceType
--
data FHIR_Type = 
    FHIR_Bundle
  | FHIR_Any
  | FHIR_ActivityDefinition
  | FHIR_Binary
  | FHIR_CapabilityStatement
  | FHIR_DomainResource
  | FHIR_Encounter 
  | FHIR_OperationOutcome
  | FHIR_Patient
  | FHIR_Practitioner
  | FHIR_PractitionerRole
  | FHIR_Resource
  | FHIR_Task
  deriving (Generic, Eq, Hashable, Ord, Read)

instance Show FHIR_Type where
  show FHIR_Bundle = "Bundle"
  show FHIR_Any    = "*"
  show FHIR_ActivityDefinition = "ActivityDefinition"
  show FHIR_Binary           = "Binary"
  show FHIR_CapabilityStatement = "CapabilityStatement"
  show FHIR_DomainResource   = "DomainResource"
  show FHIR_Encounter        = "Encounter"
  show FHIR_OperationOutcome = "OperationOutcome"
  show FHIR_Patient          = "Patient"
  show FHIR_Practitioner     = "Practitioner"
  show FHIR_PractitionerRole = "PractitionerRole"
  show FHIR_Resource         = "Resource"
  show FHIR_Task             = "Task"

instance ToJSON FHIR_Type where
  toJSON FHIR_Any = String "*"
  toJSON FHIR_Bundle = String "Bundle"
  toJSON FHIR_ActivityDefinition = String "ActivityDefinition"
  toJSON FHIR_Binary = String "Binary"
  toJSON FHIR_CapabilityStatement = String "CapabilityStatement"
  toJSON FHIR_DomainResource = String "DomainResource"
  toJSON FHIR_Encounter = String "Encounter" 
  toJSON FHIR_OperationOutcome = String "OperationOutcome"
  toJSON FHIR_Patient = String "Patient"
  toJSON FHIR_Practitioner = String "Practitioner"
  toJSON FHIR_PractitionerRole = String "PractitionerRole"
  toJSON FHIR_Resource = String "Resource"
  toJSON FHIR_Task     = String "Task"
instance FromJSON FHIR_Type where
  parseJSON "*"                = return FHIR_Any
  parseJSON "Any"              = return FHIR_Any
  parseJSON "Bundle"           = return FHIR_Bundle
  parseJSON "ActivityDefinition"  = return FHIR_ActivityDefinition
  parseJSON "Binary"              = return FHIR_Binary
  parseJSON "CapabilityStatement" = return FHIR_CapabilityStatement
  parseJSON "DomainResource"      = return FHIR_DomainResource
  parseJSON "Encounter"        = return FHIR_Encounter 
  parseJSON "OperationOutcome" = return FHIR_OperationOutcome
  parseJSON "Patient"          = return FHIR_Patient
  parseJSON "Practitioner"     = return FHIR_Practitioner
  parseJSON "PractitionerRole" = return FHIR_PractitionerRole
  parseJSON "Resource"         = return FHIR_Resource
  parseJSON "Task"             = return FHIR_Task

maybeFromFHIRType "*"                = Just FHIR_Any
maybeFromFHIRType "Any"              = Just FHIR_Any
maybeFromFHIRType "Bundle"           = Just FHIR_Bundle
maybeFromFHIRType "ActivityDefinition"  = Just FHIR_ActivityDefinition
maybeFromFHIRType "Binary"              = Just FHIR_Binary
maybeFromFHIRType "CapabilityStatement" = Just FHIR_CapabilityStatement
maybeFromFHIRType "DomainResource"      = Just FHIR_DomainResource
maybeFromFHIRType "Encounter"        = Just FHIR_Encounter 
maybeFromFHIRType "OperationOutcome" = Just FHIR_OperationOutcome
maybeFromFHIRType "Patient"          = Just FHIR_Patient
maybeFromFHIRType "Practitioner"     = Just FHIR_Practitioner
maybeFromFHIRType "PractitionerRole" = Just FHIR_PractitionerRole
maybeFromFHIRType "Resource"         = Just FHIR_Resource
maybeFromFHIRType "Task"             = Just FHIR_Task
maybeFromFHIRType _                  = Nothing

instance ToJSONKey FHIR_Type where
  toJSONKey = toJSONKeyText (T.pack . drop 5 .show)
instance FromJSONKey FHIR_Type where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe ("FHIR_" ++ T.unpack t) of
    Just k -> pure k
    Nothing -> fail ("Invalid key: " ++ show t)

-- tag type is used in to/ from JSON to reduce the use of magic strings
data ResourceType
  = RtAny
  | RtAccount
  | RtActivityDefinition
  | RtAdverseEvent
  | RtAllergyIntolerance
  | RtAppointment
  | RtAppointmentResponse
  | RtAuditEvent
  | RtBasic
  | RtBinary
  | RtBiologicallyDerivedProduct
  | RtBodyStructure
  | RtBundle
  | RtCapabilityStatement
  | RtCarePlan
  | RtCareTeam
  | RtCatalogEntry
  | RtChargeItem
  | RtChargeItemDefinition
  | RtClaim
  | RtClaimResponse
  | RtClinicalImpression
  | RtCodeSystem
  | RtCommunication
  | RtCommunicationRequest
  | RtCompartmentDefinition
  | RtComposition
  | RtConceptMap
  | RtCondition
  | RtConsent
  | RtContract
  | RtCoverage
  | RtCoverageEligibilityRequest
  | RtCoverageEligibilityResponse
  | RtDetectedIssue
  | RtDevice
  | RtDeviceDefinition
  | RtDeviceMetric
  | RtDeviceRequest
  | RtDeviceUseStatement
  | RtDiagnosticReport
  | RtDocumentManifest
  | RtDocumentReference
  | RtEffectEvidenceSynthesis
  | RtEncounter
  | RtEndpoint
  | RtEnrollmentRequest
  | RtEnrollmentResponse
  | RtEpisodeOfCare
  | RtEventDefinition
  | RtEvidence
  | RtEvidenceVariable
  | RtExampleScenario
  | RtExplanationOfBenefit
  | RtFamilyMemberHistory
  | RtFlag
  | RtGoal
  | RtGraphDefinition
  | RtGroup
  | RtGuidanceResponse
  | RtHealthcareService
  | RtImagingStudy
  | RtImmunization
  | RtImmunizationEvaluation
  | RtImmunizationRecommendation
  | RtImplementationGuide
  | RtInsurancePlan
  | RtInvoice
  | RtLibrary
  | RtLinkage
  | RtList
  | RtLocation
  | RtMeasure
  | RtMeasureReport
  | RtMedia
  | RtMedication
  | RtMedicationAdministration
  | RtMedicationDispense
  | RtMedicationKnowledge
  | RtMedicationRequest
  | RtMedicationStatement
  | RtMedicinalProduct
  | RtMedicinalProductAuthorization
  | RtMedicinalProductContraindication
  | RtMedicinalProductIndication
  | RtMedicinalProductIngredient
  | RtMedicinalProductInteraction
  | RtMedicinalProductManufactured
  | RtMedicinalProductPackaged
  | RtMedicinalProductPharmaceutical
  | RtMedicinalProductUndesirableEffect
  | RtMessageDefinition
  | RtMessageHeader
  | RtMolecularSequence
  | RtNamingSystem
  | RtNutritionOrder
  | RtObservation
  | RtObservationDefinition
  | RtOperationDefinition
  | RtOperationOutcome
  | RtOrganization
  | RtOrganizationAffiliation
  | RtParameters
  | RtPatient
  | RtPaymentNotice
  | RtPaymentReconciliation
  | RtPerson
  | RtPlanDefinition
  | RtPractitioner
  | RtPractitionerRole
  | RtProcedure
  | RtProvenance
  | RtQuestionnaire
  | RtQuestionnaireResponse
  | RtRelatedPerson
  | RtRequestGroup
  | RtResearchDefinition
  | RtResearchElementDefinition
  | RtResearchStudy
  | RtResearchSubject
  | RtRiskAssessment
  | RtRiskEvidenceSynthesis
  | RtSchedule
  | RtSearchParameter
  | RtServiceRequest
  | RtSlot
  | RtSpecimen
  | RtSpecimenDefinition
  | RtStructureDefinition
  | RtStructureMap
  | RtSubscription
  | RtSubstance
  | RtSubstancePolymer
  | RtSubstanceProtein
  | RtSubstanceReferenceInformation
  | RtSubstanceSpecification
  | RtSubstanceSourceMaterial
  | RtSupplyDelivery
  | RtSupplyRequest
  | RtTask
  | RtTerminologyCapabilities
  | RtTestReport
  | RtTestScript
  | RtValueSet
  | RtVerificationResult
  | RtVisionPrescription
  deriving (Generic, Eq, Enum, Ord, Show)

instance ToJSON ResourceType where
  toJSON RtAccount     = String "Account"
  toJSON RtActivityDefinition  = String "ActivityDefinition"
  toJSON RtAdverseEvent     = String "AdverseEvent"
  toJSON RtAllergyIntolerance     = String "AllergyIntolerance"
  toJSON RtAppointment     = String "Appointment"
  toJSON RtAppointmentResponse     = String "AppointmentResponse"
  toJSON RtAuditEvent     = String "AuditEvent"
  toJSON RtBasic     = String "Basic"
  toJSON RtBinary     = String "Binary"
  toJSON RtBiologicallyDerivedProduct     = String "BiologicallyDerivedProduct"
  toJSON RtBodyStructure     = String "BodyStructure"
  toJSON RtBundle              = String "Bundle"
  toJSON RtCapabilityStatement = String "CapabilityStatement"
  toJSON RtCarePlan     = String "CarePlan"
  toJSON RtCareTeam     = String "CareTeam"
  toJSON RtCatalogEntry     = String "CatalogEntry"
  toJSON RtChargeItem     = String "ChargeItem"
  toJSON RtChargeItemDefinition     = String "ChargeItemDefinition"
  toJSON RtClaim     = String "Claim"
  toJSON RtClaimResponse     = String "ClaimResponse"
  toJSON RtClinicalImpression     = String "ClinicalImpression"
  toJSON RtCodeSystem     = String "CodeSystem"
  toJSON RtCommunication     = String "Communication"
  toJSON RtCommunicationRequest     = String "CommunicationRequest"
  toJSON RtCompartmentDefinition     = String "CompartmentDefinition"
  toJSON RtComposition     = String "Composition"
  toJSON RtConceptMap     = String "ConceptMap"
  toJSON RtCondition     = String "Condition"
  toJSON RtConsent     = String "Consent"
  toJSON RtContract     = String "Contract"
  toJSON RtCoverage     = String "Coverage"
  toJSON RtCoverageEligibilityRequest     = String "CoverageEligibilityRequest"
  toJSON RtCoverageEligibilityResponse     = String "CoverageEligibilityResponse"
  toJSON RtDetectedIssue     = String "DetectedIssue"
  toJSON RtDevice     = String "Device"
  toJSON RtDeviceDefinition     = String "DeviceDefinition"
  toJSON RtDeviceMetric     = String "DeviceMetric"
  toJSON RtDeviceRequest     = String "DeviceRequest"
  toJSON RtDeviceUseStatement     = String "DeviceUseStatement"
  toJSON RtDiagnosticReport     = String "DiagnosticReport"
  toJSON RtDocumentManifest     = String "DocumentManifest"
  toJSON RtDocumentReference     = String "DocumentReference"
  toJSON RtEffectEvidenceSynthesis     = String "EffectEvidenceSynthesis"
  toJSON RtEncounter           = String "Encounter"
  toJSON RtEndpoint     = String "Endpoint"
  toJSON RtEnrollmentRequest     = String "EnrollmentRequest"
  toJSON RtEnrollmentResponse     = String "EnrollmentResponse"
  toJSON RtEpisodeOfCare     = String "EpisodeOfCare"
  toJSON RtEventDefinition     = String "EventDefinition"
  toJSON RtEvidence     = String "Evidence"
  toJSON RtEvidenceVariable     = String "EvidenceVariable"
  toJSON RtExampleScenario     = String "ExampleScenario"
  toJSON RtExplanationOfBenefit     = String "ExplanationOfBenefit"
  toJSON RtFamilyMemberHistory     = String "FamilyMemberHistory"
  toJSON RtFlag     = String "Flag"
  toJSON RtGoal     = String "Goal"
  toJSON RtGraphDefinition     = String "GraphDefinition"
  toJSON RtGroup     = String "Group"
  toJSON RtGuidanceResponse     = String "GuidanceResponse"
  toJSON RtHealthcareService     = String "HealthcareService"
  toJSON RtImagingStudy     = String "ImagingStudy"
  toJSON RtImmunization     = String "Immunization"
  toJSON RtImmunizationEvaluation     = String "ImmunizationEvaluation"
  toJSON RtImmunizationRecommendation     = String "ImmunizationRecommendation"
  toJSON RtImplementationGuide     = String "ImplementationGuide"
  toJSON RtInsurancePlan     = String "InsurancePlan"
  toJSON RtInvoice     = String "Invoice"
  toJSON RtLibrary     = String "Library"
  toJSON RtLinkage     = String "Linkage"
  toJSON RtList     = String "List"
  toJSON RtLocation     = String "Location"
  toJSON RtMeasure     = String "Measure"
  toJSON RtMeasureReport     = String "MeasureReport"
  toJSON RtMedia     = String "Media"
  toJSON RtMedication     = String "Medication"
  toJSON RtMedicationAdministration     = String "MedicationAdministration"
  toJSON RtMedicationDispense     = String "MedicationDispense"
  toJSON RtMedicationKnowledge     = String "MedicationKnowledge"
  toJSON RtMedicationRequest     = String "MedicationRequest"
  toJSON RtMedicationStatement     = String "MedicationStatement"
  toJSON RtMedicinalProduct     = String "MedicinalProduct"
  toJSON RtMedicinalProductAuthorization     = String "MedicinalProductAuthorization"
  toJSON RtMedicinalProductContraindication     = String "MedicinalProductContraindication"
  toJSON RtMedicinalProductIndication     = String "MedicinalProductIndication"
  toJSON RtMedicinalProductIngredient     = String "MedicinalProductIngredient"
  toJSON RtMedicinalProductInteraction     = String "MedicinalProductInteraction"
  toJSON RtMedicinalProductManufactured     = String "MedicinalProductManufactured"
  toJSON RtMedicinalProductPackaged     = String "MedicinalProductPackaged"
  toJSON RtMedicinalProductPharmaceutical     = String "MedicinalProductPharmaceutical"
  toJSON RtMedicinalProductUndesirableEffect     = String "MedicinalProductUndesirableEffect"
  toJSON RtMessageDefinition     = String "MessageDefinition"
  toJSON RtMessageHeader     = String "MessageHeader"
  toJSON RtMolecularSequence     = String "MolecularSequence"
  toJSON RtNamingSystem     = String "NamingSystem"
  toJSON RtNutritionOrder     = String "NutritionOrder"
  toJSON RtObservation     = String "Observation"
  toJSON RtObservationDefinition     = String "ObservationDefinition"
  toJSON RtOperationDefinition     = String "OperationDefinition"
  toJSON RtOperationOutcome     = String "OperationOutcome"
  toJSON RtOrganization     = String "Organization"
  toJSON RtOrganizationAffiliation     = String "OrganizationAffiliation"
  toJSON RtParameters     = String "Parameters"
  toJSON RtPatient             = String "Patient"
  toJSON RtPaymentNotice     = String "PaymentNotice"
  toJSON RtPaymentReconciliation     = String "PaymentReconciliation"
  toJSON RtPerson     = String "Person"
  toJSON RtPlanDefinition     = String "PlanDefinition"
  toJSON RtPractitioner     = String "Practitioner"
  toJSON RtPractitionerRole     = String "PractitionerRole"
  toJSON RtProcedure     = String "Procedure"
  toJSON RtProvenance     = String "Provenance"
  toJSON RtQuestionnaire     = String "Questionnaire"
  toJSON RtQuestionnaireResponse     = String "QuestionnaireResponse"
  toJSON RtRelatedPerson     = String "RelatedPerson"
  toJSON RtRequestGroup     = String "RequestGroup"
  toJSON RtResearchDefinition     = String "ResearchDefinition"
  toJSON RtResearchElementDefinition     = String "ResearchElementDefinition"
  toJSON RtResearchStudy     = String "ResearchStudy"
  toJSON RtResearchSubject     = String "ResearchSubject"
  toJSON RtRiskAssessment     = String "RiskAssessment"
  toJSON RtRiskEvidenceSynthesis     = String "RiskEvidenceSynthesis"
  toJSON RtSchedule     = String "Schedule"
  toJSON RtSearchParameter     = String "SearchParameter"
  toJSON RtServiceRequest     = String "ServiceRequest"
  toJSON RtSlot     = String "Slot"
  toJSON RtSpecimen     = String "Specimen"
  toJSON RtSpecimenDefinition     = String "SpecimenDefinition"
  toJSON RtStructureDefinition     = String "StructureDefinition"
  toJSON RtStructureMap     = String "StructureMap"
  toJSON RtSubscription     = String "Subscription"
  toJSON RtSubstance     = String "Substance"
  toJSON RtSubstancePolymer     = String "SubstancePolymer"
  toJSON RtSubstanceProtein     = String "SubstanceProtein"
  toJSON RtSubstanceReferenceInformation     = String "SubstanceReferenceInformation"
  toJSON RtSubstanceSpecification     = String "SubstanceSpecification"
  toJSON RtSubstanceSourceMaterial     = String "SubstanceSourceMaterial"
  toJSON RtSupplyDelivery     = String "SupplyDelivery"
  toJSON RtSupplyRequest     = String "SupplyRequest"
  toJSON RtTask     = String "Task"
  toJSON RtTerminologyCapabilities     = String "TerminologyCapabilities"
  toJSON RtTestReport     = String "TestReport"
  toJSON RtTestScript     = String "TestScript"
  toJSON RtValueSet     = String "ValueSet"
  toJSON RtVerificationResult     = String "VerificationResult"
  toJSON RtVisionPrescription     = String "VisionPrescription"
instance FromJSON ResourceType where
  parseJSON "*" = return RtAny
  parseJSON "Account" = return RtAccount
  parseJSON "ActivityDefinition" = return RtActivityDefinition
  parseJSON "AdverseEvent" = return RtAdverseEvent
  parseJSON "AllergyIntolerance" = return RtAllergyIntolerance
  parseJSON "Appointment" = return RtAppointment
  parseJSON "AppointmentResponse" = return RtAppointmentResponse
  parseJSON "AuditEvent" = return RtAuditEvent
  parseJSON "Basic" = return RtBasic
  parseJSON "Binary" = return RtBinary
  parseJSON "BiologicallyDerivedProduct" = return RtBiologicallyDerivedProduct
  parseJSON "BodyStructure" = return RtBodyStructure
  parseJSON "Bundle" = return RtBundle
  parseJSON "CapabilityStatement" = return RtCapabilityStatement
  parseJSON "CarePlan" = return RtCarePlan
  parseJSON "CareTeam" = return RtCareTeam
  parseJSON "CatalogEntry" = return RtCatalogEntry
  parseJSON "ChargeItem" = return RtChargeItem
  parseJSON "ChargeItemDefinition" = return RtChargeItemDefinition
  parseJSON "Claim" = return RtClaim
  parseJSON "ClaimResponse" = return RtClaimResponse
  parseJSON "ClinicalImpression" = return RtClinicalImpression
  parseJSON "CodeSystem" = return RtCodeSystem
  parseJSON "Communication" = return RtCommunication
  parseJSON "CommunicationRequest" = return RtCommunicationRequest
  parseJSON "CompartmentDefinition" = return RtCompartmentDefinition
  parseJSON "Composition" = return RtComposition
  parseJSON "ConceptMap" = return RtConceptMap
  parseJSON "Condition" = return RtCondition
  parseJSON "Consent" = return RtConsent
  parseJSON "Contract" = return RtContract
  parseJSON "Coverage" = return RtCoverage
  parseJSON "CoverageEligibilityRequest" = return RtCoverageEligibilityRequest
  parseJSON "CoverageEligibilityResponse" = return RtCoverageEligibilityResponse
  parseJSON "DetectedIssue" = return RtDetectedIssue
  parseJSON "Device" = return RtDevice
  parseJSON "DeviceDefinition" = return RtDeviceDefinition
  parseJSON "DeviceMetric" = return RtDeviceMetric
  parseJSON "DeviceRequest" = return RtDeviceRequest
  parseJSON "DeviceUseStatement" = return RtDeviceUseStatement
  parseJSON "DiagnosticReport" = return RtDiagnosticReport
  parseJSON "DocumentManifest" = return RtDocumentManifest
  parseJSON "DocumentReference" = return RtDocumentReference
  parseJSON "EffectEvidenceSynthesis" = return RtEffectEvidenceSynthesis
  parseJSON "Encounter" = return RtEncounter
  parseJSON "Endpoint" = return RtEndpoint
  parseJSON "EnrollmentRequest" = return RtEnrollmentRequest
  parseJSON "EnrollmentResponse" = return RtEnrollmentResponse
  parseJSON "EpisodeOfCare" = return RtEpisodeOfCare
  parseJSON "EventDefinition" = return RtEventDefinition
  parseJSON "Evidence" = return RtEvidence
  parseJSON "EvidenceVariable" = return RtEvidenceVariable
  parseJSON "ExampleScenario" = return RtExampleScenario
  parseJSON "ExplanationOfBenefit" = return RtExplanationOfBenefit
  parseJSON "FamilyMemberHistory" = return RtFamilyMemberHistory
  parseJSON "Flag" = return RtFlag
  parseJSON "Goal" = return RtGoal
  parseJSON "GraphDefinition" = return RtGraphDefinition
  parseJSON "Group" = return RtGroup
  parseJSON "GuidanceResponse" = return RtGuidanceResponse
  parseJSON "HealthcareService" = return RtHealthcareService
  parseJSON "ImagingStudy" = return RtImagingStudy
  parseJSON "Immunization" = return RtImmunization
  parseJSON "ImmunizationEvaluation" = return RtImmunizationEvaluation
  parseJSON "ImmunizationRecommendation" = return RtImmunizationRecommendation
  parseJSON "ImplementationGuide" = return RtImplementationGuide
  parseJSON "InsurancePlan" = return RtInsurancePlan
  parseJSON "Invoice" = return RtInvoice
  parseJSON "Library" = return RtLibrary
  parseJSON "Linkage" = return RtLinkage
  parseJSON "List" = return RtList
  parseJSON "Location" = return RtLocation
  parseJSON "Measure" = return RtMeasure
  parseJSON "MeasureReport" = return RtMeasureReport
  parseJSON "Media" = return RtMedia
  parseJSON "Medication" = return RtMedication
  parseJSON "MedicationAdministration" = return RtMedicationAdministration
  parseJSON "MedicationDispense" = return RtMedicationDispense
  parseJSON "MedicationKnowledge" = return RtMedicationKnowledge
  parseJSON "MedicationRequest" = return RtMedicationRequest
  parseJSON "MedicationStatement" = return RtMedicationStatement
  parseJSON "MedicinalProduct" = return RtMedicinalProduct
  parseJSON "MedicinalProductAuthorization" = return RtMedicinalProductAuthorization
  parseJSON "MedicinalProductContraindication" = return RtMedicinalProductContraindication
  parseJSON "MedicinalProductIndication" = return RtMedicinalProductIndication
  parseJSON "MedicinalProductIngredient" = return RtMedicinalProductIngredient
  parseJSON "MedicinalProductInteraction" = return RtMedicinalProductInteraction
  parseJSON "MedicinalProductManufactured" = return RtMedicinalProductManufactured
  parseJSON "MedicinalProductPackaged" = return RtMedicinalProductPackaged
  parseJSON "MedicinalProductPharmaceutical" = return RtMedicinalProductPharmaceutical
  parseJSON "MedicinalProductUndesirableEffect" = return RtMedicinalProductUndesirableEffect
  parseJSON "MessageDefinition" = return RtMessageDefinition
  parseJSON "MessageHeader" = return RtMessageHeader
  parseJSON "MolecularSequence" = return RtMolecularSequence
  parseJSON "NamingSystem" = return RtNamingSystem
  parseJSON "NutritionOrder" = return RtNutritionOrder
  parseJSON "Observation" = return RtObservation
  parseJSON "ObservationDefinition" = return RtObservationDefinition
  parseJSON "OperationDefinition" = return RtOperationDefinition
  parseJSON "OperationOutcome" = return RtOperationOutcome
  parseJSON "Organization" = return RtOrganization
  parseJSON "OrganizationAffiliation" = return RtOrganizationAffiliation
  parseJSON "Parameters" = return RtParameters
  parseJSON "Patient" = return RtPatient
  parseJSON "PaymentNotice" = return RtPaymentNotice
  parseJSON "PaymentReconciliation" = return RtPaymentReconciliation
  parseJSON "Person" = return RtPerson
  parseJSON "PlanDefinition" = return RtPlanDefinition
  parseJSON "Practitioner" = return RtPractitioner
  parseJSON "PractitionerRole" = return RtPractitionerRole
  parseJSON "Procedure" = return RtProcedure
  parseJSON "Provenance" = return RtProvenance
  parseJSON "Questionnaire" = return RtQuestionnaire
  parseJSON "QuestionnaireResponse" = return RtQuestionnaireResponse
  parseJSON "RelatedPerson" = return RtRelatedPerson
  parseJSON "RequestGroup" = return RtRequestGroup
  parseJSON "ResearchDefinition" = return RtResearchDefinition
  parseJSON "ResearchElementDefinition" = return RtResearchElementDefinition
  parseJSON "ResearchStudy" = return RtResearchStudy
  parseJSON "ResearchSubject" = return RtResearchSubject
  parseJSON "RiskAssessment" = return RtRiskAssessment
  parseJSON "RiskEvidenceSynthesis" = return RtRiskEvidenceSynthesis
  parseJSON "Schedule" = return RtSchedule
  parseJSON "SearchParameter" = return RtSearchParameter
  parseJSON "ServiceRequest" = return RtServiceRequest
  parseJSON "Slot" = return RtSlot
  parseJSON "Specimen" = return RtSpecimen
  parseJSON "SpecimenDefinition" = return RtSpecimenDefinition
  parseJSON "StructureDefinition" = return RtStructureDefinition
  parseJSON "StructureMap" = return RtStructureMap
  parseJSON "Subscription" = return RtSubscription
  parseJSON "Substance" = return RtSubstance
  parseJSON "SubstancePolymer" = return RtSubstancePolymer
  parseJSON "SubstanceProtein" = return RtSubstanceProtein
  parseJSON "SubstanceReferenceInformation" = return RtSubstanceReferenceInformation
  parseJSON "SubstanceSpecification" = return RtSubstanceSpecification
  parseJSON "SubstanceSourceMaterial" = return RtSubstanceSourceMaterial
  parseJSON "SupplyDelivery" = return RtSupplyDelivery
  parseJSON "SupplyRequest" = return RtSupplyRequest
  parseJSON "Task" = return RtTask
  parseJSON "TerminologyCapabilities" = return RtTerminologyCapabilities
  parseJSON "TestReport" = return RtTestReport
  parseJSON "TestScript" = return RtTestScript
  parseJSON "ValueSet" = return RtValueSet
  parseJSON "VerificationResult" = return RtVerificationResult
  parseJSON "VisionPrescription" = return RtVisionPrescription

toResourceType RtAccount     = "Account"
toResourceType RtActivityDefinition     = "ActivityDefinition"
toResourceType RtAdverseEvent     = "AdverseEvent"
toResourceType RtAllergyIntolerance     = "AllergyIntolerance"
toResourceType RtAppointment     = "Appointment"
toResourceType RtAppointmentResponse     = "AppointmentResponse"
toResourceType RtAuditEvent     = "AuditEvent"
toResourceType RtBasic     = "Basic"
toResourceType RtBinary     = "Binary"
toResourceType RtBiologicallyDerivedProduct     = "BiologicallyDerivedProduct"
toResourceType RtBodyStructure     = "BodyStructure"
toResourceType RtBundle              = "Bundle"
toResourceType RtCapabilityStatement = "CapabilityStatement"
toResourceType RtCarePlan     = "CarePlan"
toResourceType RtCareTeam     = "CareTeam"
toResourceType RtCatalogEntry     = "CatalogEntry"
toResourceType RtChargeItem     = "ChargeItem"
toResourceType RtChargeItemDefinition     = "ChargeItemDefinition"
toResourceType RtClaim     = "Claim"
toResourceType RtClaimResponse     = "ClaimResponse"
toResourceType RtClinicalImpression     = "ClinicalImpression"
toResourceType RtCodeSystem     = "CodeSystem"
toResourceType RtCommunication     = "Communication"
toResourceType RtCommunicationRequest     = "CommunicationRequest"
toResourceType RtCompartmentDefinition     = "CompartmentDefinition"
toResourceType RtComposition     = "Composition"
toResourceType RtConceptMap     = "ConceptMap"
toResourceType RtCondition     = "Condition"
toResourceType RtConsent     = "Consent"
toResourceType RtContract     = "Contract"
toResourceType RtCoverage     = "Coverage"
toResourceType RtCoverageEligibilityRequest     = "CoverageEligibilityRequest"
toResourceType RtCoverageEligibilityResponse     = "CoverageEligibilityResponse"
toResourceType RtDetectedIssue     = "DetectedIssue"
toResourceType RtDevice     = "Device"
toResourceType RtDeviceDefinition     = "DeviceDefinition"
toResourceType RtDeviceMetric     = "DeviceMetric"
toResourceType RtDeviceRequest     = "DeviceRequest"
toResourceType RtDeviceUseStatement     = "DeviceUseStatement"
toResourceType RtDiagnosticReport     = "DiagnosticReport"
toResourceType RtDocumentManifest     = "DocumentManifest"
toResourceType RtDocumentReference     = "DocumentReference"
toResourceType RtEffectEvidenceSynthesis     = "EffectEvidenceSynthesis"
toResourceType RtEncounter           = "Encounter"
toResourceType RtEndpoint     = "Endpoint"
toResourceType RtEnrollmentRequest     = "EnrollmentRequest"
toResourceType RtEnrollmentResponse     = "EnrollmentResponse"
toResourceType RtEpisodeOfCare     = "EpisodeOfCare"
toResourceType RtEventDefinition     = "EventDefinition"
toResourceType RtEvidence     = "Evidence"
toResourceType RtEvidenceVariable     = "EvidenceVariable"
toResourceType RtExampleScenario     = "ExampleScenario"
toResourceType RtExplanationOfBenefit     = "ExplanationOfBenefit"
toResourceType RtFamilyMemberHistory     = "FamilyMemberHistory"
toResourceType RtFlag     = "Flag"
toResourceType RtGoal     = "Goal"
toResourceType RtGraphDefinition     = "GraphDefinition"
toResourceType RtGroup     = "Group"
toResourceType RtGuidanceResponse     = "GuidanceResponse"
toResourceType RtHealthcareService     = "HealthcareService"
toResourceType RtImagingStudy     = "ImagingStudy"
toResourceType RtImmunization     = "Immunization"
toResourceType RtImmunizationEvaluation     = "ImmunizationEvaluation"
toResourceType RtImmunizationRecommendation     = "ImmunizationRecommendation"
toResourceType RtImplementationGuide     = "ImplementationGuide"
toResourceType RtInsurancePlan = "InsurancePlan"
toResourceType RtInvoice       = "Invoice"
toResourceType RtLibrary       = "Library"
toResourceType RtLinkage       = "Linkage"
toResourceType RtList          = "List"
toResourceType RtLocation      = "Location"
toResourceType RtMeasure       = "Measure"
toResourceType RtMeasureReport = "MeasureReport"
toResourceType RtMedia         = "Media"
toResourceType RtMedication    = "Medication"
toResourceType RtMedicationAdministration = "MedicationAdministration"
toResourceType RtMedicationDispense       = "MedicationDispense"
toResourceType RtMedicationKnowledge      = "MedicationKnowledge"
toResourceType RtMedicationRequest        = "MedicationRequest"
toResourceType RtMedicationStatement      = "MedicationStatement"
toResourceType RtMedicinalProduct         = "MedicinalProduct"
toResourceType RtMedicinalProductAuthorization     = "MedicinalProductAuthorization"
toResourceType RtMedicinalProductContraindication  = "MedicinalProductContraindication"
toResourceType RtMedicinalProductIndication        = "MedicinalProductIndication"
toResourceType RtMedicinalProductIngredient        = "MedicinalProductIngredient"
toResourceType RtMedicinalProductInteraction       = "MedicinalProductInteraction"
toResourceType RtMedicinalProductManufactured      = "MedicinalProductManufactured"
toResourceType RtMedicinalProductPackaged          = "MedicinalProductPackaged"
toResourceType RtMedicinalProductPharmaceutical    = "MedicinalProductPharmaceutical"
toResourceType RtMedicinalProductUndesirableEffect = "MedicinalProductUndesirableEffect"
toResourceType RtMessageDefinition    = "MessageDefinition"
toResourceType RtMessageHeader        = "MessageHeader"
toResourceType RtMolecularSequence    = "MolecularSequence"
toResourceType RtNamingSystem         = "NamingSystem"
toResourceType RtNutritionOrder       = "NutritionOrder"
toResourceType RtObservation          = "Observation"
toResourceType RtObservationDefinition= "ObservationDefinition"
toResourceType RtOperationDefinition  = "OperationDefinition"
toResourceType RtOperationOutcome     = "OperationOutcome"
toResourceType RtOrganization         = "Organization"
toResourceType RtOrganizationAffiliation     = "OrganizationAffiliation"
toResourceType RtParameters           = "Parameters"
toResourceType RtPatient              = "Patient"
toResourceType RtPaymentNotice        = "PaymentNotice"
toResourceType RtPaymentReconciliation= "PaymentReconciliation"
toResourceType RtPerson               = "Person"
toResourceType RtPlanDefinition       = "PlanDefinition"
toResourceType RtPractitioner         = "Practitioner"
toResourceType RtPractitionerRole     = "PractitionerRole"
toResourceType RtProcedure            = "Procedure"
toResourceType RtProvenance           = "Provenance"
toResourceType RtQuestionnaire        = "Questionnaire"
toResourceType RtQuestionnaireResponse= "QuestionnaireResponse"
toResourceType RtRelatedPerson        = "RelatedPerson"
toResourceType RtRequestGroup         = "RequestGroup"
toResourceType RtResearchDefinition   = "ResearchDefinition"
toResourceType RtResearchElementDefinition     = "ResearchElementDefinition"
toResourceType RtResearchStudy        = "ResearchStudy"
toResourceType RtResearchSubject      = "ResearchSubject"
toResourceType RtRiskAssessment       = "RiskAssessment"
toResourceType RtRiskEvidenceSynthesis= "RiskEvidenceSynthesis"
toResourceType RtSchedule             = "Schedule"
toResourceType RtSearchParameter      = "SearchParameter"
toResourceType RtServiceRequest       = "ServiceRequest"
toResourceType RtSlot                 = "Slot"
toResourceType RtSpecimen             = "Specimen"
toResourceType RtSpecimenDefinition   = "SpecimenDefinition"
toResourceType RtStructureDefinition  = "StructureDefinition"
toResourceType RtStructureMap         = "StructureMap"
toResourceType RtSubscription         = "Subscription"
toResourceType RtSubstance            = "Substance"
toResourceType RtSubstancePolymer     = "SubstancePolymer"
toResourceType RtSubstanceProtein     = "SubstanceProtein"
toResourceType RtSubstanceReferenceInformation     = "SubstanceReferenceInformation"
toResourceType RtSubstanceSpecification  = "SubstanceSpecification"
toResourceType RtSubstanceSourceMaterial = "SubstanceSourceMaterial"
toResourceType RtSupplyDelivery          = "SupplyDelivery"
toResourceType RtSupplyRequest           = "SupplyRequest"
toResourceType RtTask                    = "Task"
toResourceType RtTerminologyCapabilities = "TerminologyCapabilities"
toResourceType RtTestReport              = "TestReport"
toResourceType RtTestScript              = "TestScript"
toResourceType RtValueSet                = "ValueSet"
toResourceType RtVerificationResult      = "VerificationResult"
toResourceType RtVisionPrescription      = "VisionPrescription"

fromResourceType "Account"     = RtAccount
fromResourceType "ActivityDefinition"     = RtActivityDefinition
fromResourceType "AdverseEvent"     = RtAdverseEvent
fromResourceType "AllergyIntolerance"     = RtAllergyIntolerance
fromResourceType "Appointment"     = RtAppointment
fromResourceType "AppointmentResponse"     = RtAppointmentResponse
fromResourceType "AuditEvent"     = RtAuditEvent
fromResourceType "Basic"     = RtBasic
fromResourceType "Binary"     = RtBinary
fromResourceType "BiologicallyDerivedProduct"     = RtBiologicallyDerivedProduct
fromResourceType "BodyStructure"     = RtBodyStructure
fromResourceType "Bundle"              = RtBundle
fromResourceType "CapabilityStatement" = RtCapabilityStatement
fromResourceType "CarePlan"     = RtCarePlan
fromResourceType "CareTeam"     = RtCareTeam
fromResourceType "CatalogEntry"     = RtCatalogEntry
fromResourceType "ChargeItem"     = RtChargeItem
fromResourceType "ChargeItemDefinition"     = RtChargeItemDefinition
fromResourceType "Claim"     = RtClaim
fromResourceType "ClaimResponse"     = RtClaimResponse
fromResourceType "ClinicalImpression"     = RtClinicalImpression
fromResourceType "CodeSystem"     = RtCodeSystem
fromResourceType "Communication"     = RtCommunication
fromResourceType "CommunicationRequest"     = RtCommunicationRequest
fromResourceType "CompartmentDefinition"     = RtCompartmentDefinition
fromResourceType "Composition"     = RtComposition
fromResourceType "ConceptMap"     = RtConceptMap
fromResourceType "Condition"     = RtCondition
fromResourceType "Consent"     = RtConsent
fromResourceType "Contract"     = RtContract
fromResourceType "Coverage"     = RtCoverage
fromResourceType "CoverageEligibilityRequest"     = RtCoverageEligibilityRequest
fromResourceType "CoverageEligibilityResponse"     = RtCoverageEligibilityResponse
fromResourceType "DetectedIssue"     = RtDetectedIssue
fromResourceType "Device"     = RtDevice
fromResourceType "DeviceDefinition"     = RtDeviceDefinition
fromResourceType "DeviceMetric"     = RtDeviceMetric
fromResourceType "DeviceRequest"     = RtDeviceRequest
fromResourceType "DeviceUseStatement"     = RtDeviceUseStatement
fromResourceType "DiagnosticReport"     = RtDiagnosticReport
fromResourceType "DocumentManifest"     = RtDocumentManifest
fromResourceType "DocumentReference"     = RtDocumentReference
fromResourceType "EffectEvidenceSynthesis"     = RtEffectEvidenceSynthesis
fromResourceType "Encounter"           = RtEncounter
fromResourceType "Endpoint"     = RtEndpoint
fromResourceType "EnrollmentRequest"     = RtEnrollmentRequest
fromResourceType "EnrollmentResponse"     = RtEnrollmentResponse
fromResourceType "EpisodeOfCare"     = RtEpisodeOfCare
fromResourceType "EventDefinition"     = RtEventDefinition
fromResourceType "Evidence"     = RtEvidence
fromResourceType "EvidenceVariable"     = RtEvidenceVariable
fromResourceType "ExampleScenario"     = RtExampleScenario
fromResourceType "ExplanationOfBenefit"     = RtExplanationOfBenefit
fromResourceType "FamilyMemberHistory"     = RtFamilyMemberHistory
fromResourceType "Flag"     = RtFlag
fromResourceType "Goal"     = RtGoal
fromResourceType "GraphDefinition"     = RtGraphDefinition
fromResourceType "Group"     = RtGroup
fromResourceType "GuidanceResponse"     = RtGuidanceResponse
fromResourceType "HealthcareService"     = RtHealthcareService
fromResourceType "ImagingStudy"     = RtImagingStudy
fromResourceType "Immunization"     = RtImmunization
fromResourceType "ImmunizationEvaluation"     = RtImmunizationEvaluation
fromResourceType "ImmunizationRecommendation"     = RtImmunizationRecommendation
fromResourceType "ImplementationGuide"     = RtImplementationGuide
fromResourceType "InsurancePlan"     = RtInsurancePlan
fromResourceType "Invoice"     = RtInvoice
fromResourceType "Library"     = RtLibrary
fromResourceType "Linkage"     = RtLinkage
fromResourceType "List"     = RtList
fromResourceType "Location"     = RtLocation
fromResourceType "Measure"     = RtMeasure
fromResourceType "MeasureReport"     = RtMeasureReport
fromResourceType "Media"     = RtMedia
fromResourceType "Medication"     = RtMedication
fromResourceType "MedicationAdministration"     = RtMedicationAdministration
fromResourceType "MedicationDispense"     = RtMedicationDispense
fromResourceType "MedicationKnowledge"     = RtMedicationKnowledge
fromResourceType "MedicationRequest"     = RtMedicationRequest
fromResourceType "MedicationStatement"     = RtMedicationStatement
fromResourceType "MedicinalProduct"     = RtMedicinalProduct
fromResourceType "MedicinalProductAuthorization"     = RtMedicinalProductAuthorization
fromResourceType "MedicinalProductContraindication"     = RtMedicinalProductContraindication
fromResourceType "MedicinalProductIndication"     = RtMedicinalProductIndication
fromResourceType "MedicinalProductIngredient"     = RtMedicinalProductIngredient
fromResourceType "MedicinalProductInteraction"     = RtMedicinalProductInteraction
fromResourceType "MedicinalProductManufactured"     = RtMedicinalProductManufactured
fromResourceType "MedicinalProductPackaged"     = RtMedicinalProductPackaged
fromResourceType "MedicinalProductPharmaceutical"     = RtMedicinalProductPharmaceutical
fromResourceType "MedicinalProductUndesirableEffect"     = RtMedicinalProductUndesirableEffect
fromResourceType "MessageDefinition"     = RtMessageDefinition
fromResourceType "MessageHeader"     = RtMessageHeader
fromResourceType "MolecularSequence"     = RtMolecularSequence
fromResourceType "NamingSystem"     = RtNamingSystem
fromResourceType "NutritionOrder"     = RtNutritionOrder
fromResourceType "Observation"     = RtObservation
fromResourceType "ObservationDefinition"     = RtObservationDefinition
fromResourceType "OperationDefinition"     = RtOperationDefinition
fromResourceType "OperationOutcome"     = RtOperationOutcome
fromResourceType "Organization"     = RtOrganization
fromResourceType "OrganizationAffiliation"     = RtOrganizationAffiliation
fromResourceType "Parameters"     = RtParameters
fromResourceType "Patient"             = RtPatient 
fromResourceType "PaymentNotice"     = RtPaymentNotice
fromResourceType "PaymentReconciliation"     = RtPaymentReconciliation
fromResourceType "Person"     = RtPerson
fromResourceType "PlanDefinition"     = RtPlanDefinition
fromResourceType "Practitioner"     = RtPractitioner
fromResourceType "PractitionerRole"     = RtPractitionerRole
fromResourceType "Procedure"     = RtProcedure
fromResourceType "Provenance"     = RtProvenance
fromResourceType "Questionnaire"     = RtQuestionnaire
fromResourceType "QuestionnaireResponse"     = RtQuestionnaireResponse
fromResourceType "RelatedPerson"     = RtRelatedPerson
fromResourceType "RequestGroup"     = RtRequestGroup
fromResourceType "ResearchDefinition"     = RtResearchDefinition
fromResourceType "ResearchElementDefinition"     = RtResearchElementDefinition
fromResourceType "ResearchStudy"     = RtResearchStudy
fromResourceType "ResearchSubject"     = RtResearchSubject
fromResourceType "RiskAssessment"     = RtRiskAssessment
fromResourceType "RiskEvidenceSynthesis"     = RtRiskEvidenceSynthesis
fromResourceType "Schedule"     = RtSchedule
fromResourceType "SearchParameter"     = RtSearchParameter
fromResourceType "ServiceRequest"     = RtServiceRequest
fromResourceType "Slot"     = RtSlot
fromResourceType "Specimen"     = RtSpecimen
fromResourceType "SpecimenDefinition"     = RtSpecimenDefinition
fromResourceType "StructureDefinition"     = RtStructureDefinition
fromResourceType "StructureMap"     = RtStructureMap
fromResourceType "Subscription"     = RtSubscription
fromResourceType "Substance"     = RtSubstance
fromResourceType "SubstancePolymer"     = RtSubstancePolymer
fromResourceType "SubstanceProtein"     = RtSubstanceProtein
fromResourceType "SubstanceReferenceInformation"     = RtSubstanceReferenceInformation
fromResourceType "SubstanceSpecification"     = RtSubstanceSpecification
fromResourceType "SubstanceSourceMaterial"     = RtSubstanceSourceMaterial
fromResourceType "SupplyDelivery"     = RtSupplyDelivery
fromResourceType "SupplyRequest"     = RtSupplyRequest
fromResourceType "Task"     = RtTask
fromResourceType "TerminologyCapabilities"     = RtTerminologyCapabilities
fromResourceType "TestReport"     = RtTestReport
fromResourceType "TestScript"     = RtTestScript
fromResourceType "ValueSet"     = RtValueSet
fromResourceType "VerificationResult"     = RtVerificationResult
fromResourceType "VisionPrescription"     = RtVisionPrescription


data Compartment
  = CompartmentPatient 
  | CompartmentEncounter 
  | CompartmentRelatedPerson
  | CompartmentPractitioner
  | CompartmentDevice
  deriving (Generic, Eq, Show)

data FhirStatus =
    FsNormative
  | FsTrial
  deriving (Generic, Eq, Show)



