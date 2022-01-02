-ifndef(codes).
%%%
%%%
%%% FHIR XSD schema converted to erlang map and types
%%% @version : 4.0.0
%%% do not edit or know what you are doing ;-)
%%% generated with convert-xsd-erlang.xql script
%%% comments, known deficits:
%%% - attributes exists only in Element (id) and Extension (url)
%%% - xs:choice are all converted as simple or (no restriction on parallel use)
%%% - Narrative has to be manually fixed (xhtml:div ref, no type)
%%% - simple elements are defined in XSD as a type which refers to a type with value attribute plus code-list simple type
%%%   these are converted to a union type, that means these type are not in the big map
%%%

%%%
%%% simple types
%%%%% date-primitive
 -type date_primitive() ::
             'xs:gYear'
           | 'xs:gYearMonth'
           | 'xs:date'.
 %% dateTime-primitive
 -type dateTime_primitive() ::
             'xs:gYear'
           | 'xs:gYearMonth'
           | 'xs:date'
           | 'xs:dateTime'.
 %% decimal-primitive
 -type decimal_primitive() ::
             'xs:decimal'
           | 'xs:double'.

-define(fhir_codes, 
        #{
%% NarrativeStatus-list
           <<"narrativestatus">> => [
             <<"generated">>
           , <<"extensions">>
           , <<"additional">>
           , <<"empty">>]
 %% AddressUse-list
         , <<"addressuse">> => [
             <<"home">>
           , <<"work">>
           , <<"temp">>
           , <<"old">>
           , <<"billing">>]
 %% AddressType-list
         , <<"addresstype">> => [
             <<"postal">>
           , <<"physical">>
           , <<"both">>]
 %% ContributorType-list
         , <<"contributortype">> => [
             <<"author">>
           , <<"editor">>
           , <<"reviewer">>
           , <<"endorser">>]
 %% SortDirection-list
         , <<"sortdirection">> => [
             <<"ascending">>
           , <<"descending">>]
 %% NameUse-list
         , <<"nameuse">> => [
             <<"usual">>
           , <<"official">>
           , <<"temp">>
           , <<"nickname">>
           , <<"anonymous">>
           , <<"old">>
           , <<"maiden">>]
 %% ContactPointSystem-list
         , <<"contactpointsystem">> => [
             <<"phone">>
           , <<"fax">>
           , <<"email">>
           , <<"pager">>
           , <<"url">>
           , <<"sms">>
           , <<"other">>]
 %% ContactPointUse-list
         , <<"contactpointuse">> => [
             <<"home">>
           , <<"work">>
           , <<"temp">>
           , <<"old">>
           , <<"mobile">>]
 %% IdentifierUse-list
         , <<"identifieruse">> => [
             <<"usual">>
           , <<"official">>
           , <<"temp">>
           , <<"secondary">>
           , <<"old">>]
 %% TriggerType-list
         , <<"triggertype">> => [
             <<"named-event">>
           , <<"periodic">>
           , <<"data-changed">>
           , <<"data-added">>
           , <<"data-modified">>
           , <<"data-removed">>
           , <<"data-accessed">>
           , <<"data-access-ended">>]
 %% QuantityComparator-list
         , <<"quantitycomparator">> => [
             <<"<">>
           , <<"<=">>
           , <<">=">>
           , <<">">>]
 %% RelatedArtifactType-list
         , <<"relatedartifacttype">> => [
             <<"documentation">>
           , <<"justification">>
           , <<"citation">>
           , <<"predecessor">>
           , <<"successor">>
           , <<"derived-from">>
           , <<"depends-on">>
           , <<"composed-of">>]
 %% ExpressionLanguage-list
         , <<"expressionlanguage">> => [
             <<"text/cql">>
           , <<"text/fhirpath">>
           , <<"application/x-fhir-query">>]
 %% UnitsOfTime-list
         , <<"unitsoftime">> => [
             <<"s">>
           , <<"min">>
           , <<"h">>
           , <<"d">>
           , <<"wk">>
           , <<"mo">>
           , <<"a">>]
 %% EventTiming-list
         , <<"eventtiming">> => [
             <<"MORN">>
           , <<"MORN.early">>
           , <<"MORN.late">>
           , <<"NOON">>
           , <<"AFT">>
           , <<"AFT.early">>
           , <<"AFT.late">>
           , <<"EVE">>
           , <<"EVE.early">>
           , <<"EVE.late">>
           , <<"NIGHT">>
           , <<"PHS">>
           , <<"HS">>
           , <<"WAKE">>
           , <<"C">>
           , <<"CM">>
           , <<"CD">>
           , <<"CV">>
           , <<"AC">>
           , <<"ACM">>
           , <<"ACD">>
           , <<"ACV">>
           , <<"PC">>
           , <<"PCM">>
           , <<"PCD">>
           , <<"PCV">>]
 %% PropertyRepresentation-list
         , <<"propertyrepresentation">> => [
             <<"xmlAttr">>
           , <<"xmlText">>
           , <<"typeAttr">>
           , <<"cdaText">>
           , <<"xhtml">>]
 %% ConstraintSeverity-list
         , <<"constraintseverity">> => [
             <<"error">>
           , <<"warning">>]
 %% AggregationMode-list
         , <<"aggregationmode">> => [
             <<"contained">>
           , <<"referenced">>
           , <<"bundled">>]
 %% ReferenceVersionRules-list
         , <<"referenceversionrules">> => [
             <<"either">>
           , <<"independent">>
           , <<"specific">>]
 %% SlicingRules-list
         , <<"slicingrules">> => [
             <<"closed">>
           , <<"open">>
           , <<"openAtEnd">>]
 %% BindingStrength-list
         , <<"bindingstrength">> => [
             <<"required">>
           , <<"extensible">>
           , <<"preferred">>
           , <<"example">>]
 %% DiscriminatorType-list
         , <<"discriminatortype">> => [
             <<"value">>
           , <<"exists">>
           , <<"pattern">>
           , <<"type">>
           , <<"profile">>]
 %% PublicationStatus-list
         , <<"publicationstatus">> => [
             <<"draft">>
           , <<"active">>
           , <<"retired">>
           , <<"unknown">>]
 %% SearchParamType-list
         , <<"searchparamtype">> => [
             <<"number">>
           , <<"date">>
           , <<"string">>
           , <<"token">>
           , <<"reference">>
           , <<"composite">>
           , <<"quantity">>
           , <<"uri">>
           , <<"special">>]
 %% AdministrativeGender-list
         , <<"administrativegender">> => [
             <<"male">>
           , <<"female">>
           , <<"other">>
           , <<"unknown">>]
 %% FHIRVersion-list
         , <<"fhirversion">> => [
             <<"0.01">>
           , <<"0.05">>
           , <<"0.06">>
           , <<"0.11">>
           , <<"0.0.80">>
           , <<"0.0.81">>
           , <<"0.0.82">>
           , <<"0.4.0">>
           , <<"0.5.0">>
           , <<"1.0.0">>
           , <<"1.0.1">>
           , <<"1.0.2">>
           , <<"1.1.0">>
           , <<"1.4.0">>
           , <<"1.6.0">>
           , <<"1.8.0">>
           , <<"3.0.0">>
           , <<"3.0.1">>
           , <<"3.3.0">>
           , <<"3.5.0">>
           , <<"4.0.0">>]
 %% NoteType-list
         , <<"notetype">> => [
             <<"display">>
           , <<"print">>
           , <<"printoper">>]
 %% RemittanceOutcome-list
         , <<"remittanceoutcome">> => [
             <<"queued">>
           , <<"complete">>
           , <<"error">>
           , <<"partial">>]
 %% ConceptMapEquivalence-list
         , <<"conceptmapequivalence">> => [
             <<"relatedto">>
           , <<"equivalent">>
           , <<"equal">>
           , <<"wider">>
           , <<"subsumes">>
           , <<"narrower">>
           , <<"specializes">>
           , <<"inexact">>
           , <<"unmatched">>
           , <<"disjoint">>]
 %% DocumentReferenceStatus-list
         , <<"documentreferencestatus">> => [
             <<"current">>
           , <<"superseded">>
           , <<"entered-in-error">>]
 %% AccountStatus-list
         , <<"accountstatus">> => [
             <<"active">>
           , <<"inactive">>
           , <<"entered-in-error">>
           , <<"on-hold">>
           , <<"unknown">>]
 %% ActionParticipantType-list
         , <<"actionparticipanttype">> => [
             <<"patient">>
           , <<"practitioner">>
           , <<"related-person">>
           , <<"device">>]
 %% RequestIntent-list
         , <<"requestintent">> => [
             <<"proposal">>
           , <<"plan">>
           , <<"directive">>
           , <<"order">>
           , <<"original-order">>
           , <<"reflex-order">>
           , <<"filler-order">>
           , <<"instance-order">>
           , <<"option">>]
 %% RequestResourceType-list
         , <<"requestresourcetype">> => [
             <<"Appointment">>
           , <<"AppointmentResponse">>
           , <<"CarePlan">>
           , <<"Claim">>
           , <<"CommunicationRequest">>
           , <<"Contract">>
           , <<"DeviceRequest">>
           , <<"EnrollmentRequest">>
           , <<"ImmunizationRecommendation">>
           , <<"MedicationRequest">>
           , <<"NutritionOrder">>
           , <<"ServiceRequest">>
           , <<"SupplyRequest">>
           , <<"Task">>
           , <<"VisionPrescription">>]
 %% RequestPriority-list
         , <<"requestpriority">> => [
             <<"routine">>
           , <<"urgent">>
           , <<"asap">>
           , <<"stat">>]
 %% AdverseEventActuality-list
         , <<"adverseeventactuality">> => [
             <<"actual">>
           , <<"potential">>]
 %% AllergyIntoleranceCriticality-list
         , <<"allergyintolerancecriticality">> => [
             <<"low">>
           , <<"high">>
           , <<"unable-to-assess">>]
 %% AllergyIntoleranceType-list
         , <<"allergyintolerancetype">> => [
             <<"allergy">>
           , <<"intolerance">>]
 %% AllergyIntoleranceCategory-list
         , <<"allergyintolerancecategory">> => [
             <<"food">>
           , <<"medication">>
           , <<"environment">>
           , <<"biologic">>]
 %% AllergyIntoleranceSeverity-list
         , <<"allergyintoleranceseverity">> => [
             <<"mild">>
           , <<"moderate">>
           , <<"severe">>]
 %% ParticipantRequired-list
         , <<"participantrequired">> => [
             <<"required">>
           , <<"optional">>
           , <<"information-only">>]
 %% AppointmentStatus-list
         , <<"appointmentstatus">> => [
             <<"proposed">>
           , <<"pending">>
           , <<"booked">>
           , <<"arrived">>
           , <<"fulfilled">>
           , <<"cancelled">>
           , <<"noshow">>
           , <<"entered-in-error">>
           , <<"checked-in">>
           , <<"waitlist">>]
 %% ParticipationStatus-list
         , <<"participationstatus">> => [
             <<"accepted">>
           , <<"declined">>
           , <<"tentative">>
           , <<"needs-action">>]
 %% AuditEventOutcome-list
         , <<"auditeventoutcome">> => [
             <<"0">>
           , <<"4">>
           , <<"8">>
           , <<"12">>]
 %% AuditEventAction-list
         , <<"auditeventaction">> => [
             <<"C">>
           , <<"R">>
           , <<"U">>
           , <<"D">>
           , <<"E">>]
 %% AuditEventAgentNetworkType-list
         , <<"auditeventagentnetworktype">> => [
             <<"1">>
           , <<"2">>
           , <<"3">>
           , <<"4">>
           , <<"5">>]
 %% BiologicallyDerivedProductCategory-list
         , <<"biologicallyderivedproductcategory">> => [
             <<"organ">>
           , <<"tissue">>
           , <<"fluid">>
           , <<"cells">>
           , <<"biologicalAgent">>]
 %% BiologicallyDerivedProductStatus-list
         , <<"biologicallyderivedproductstatus">> => [
             <<"available">>
           , <<"unavailable">>]
 %% BiologicallyDerivedProductStorageScale-list
         , <<"biologicallyderivedproductstoragescale">> => [
             <<"farenheit">>
           , <<"celsius">>
           , <<"kelvin">>]
 %% HTTPVerb-list
         , <<"httpverb">> => [
             <<"GET">>
           , <<"HEAD">>
           , <<"POST">>
           , <<"PUT">>
           , <<"DELETE">>
           , <<"PATCH">>]
 %% BundleType-list
         , <<"bundletype">> => [
             <<"document">>
           , <<"message">>
           , <<"transaction">>
           , <<"transaction-response">>
           , <<"batch">>
           , <<"batch-response">>
           , <<"history">>
           , <<"searchset">>
           , <<"collection">>]
 %% SearchEntryMode-list
         , <<"searchentrymode">> => [
             <<"match">>
           , <<"include">>
           , <<"outcome">>]
 %% CapabilityStatementKind-list
         , <<"capabilitystatementkind">> => [
             <<"instance">>
           , <<"capability">>
           , <<"requirements">>]
 %% EventCapabilityMode-list
         , <<"eventcapabilitymode">> => [
             <<"sender">>
           , <<"receiver">>]
 %% ResourceVersionPolicy-list
         , <<"resourceversionpolicy">> => [
             <<"no-version">>
           , <<"versioned">>
           , <<"versioned-update">>]
 %% DocumentMode-list
         , <<"documentmode">> => [
             <<"producer">>
           , <<"consumer">>]
 %% RestfulCapabilityMode-list
         , <<"restfulcapabilitymode">> => [
             <<"client">>
           , <<"server">>]
 %% TypeRestfulInteraction-list
         , <<"typerestfulinteraction">> => [
             <<"read">>
           , <<"vread">>
           , <<"update">>
           , <<"patch">>
           , <<"delete">>
           , <<"history-instance">>
           , <<"history-type">>
           , <<"create">>
           , <<"search-type">>]
 %% SystemRestfulInteraction-list
         , <<"systemrestfulinteraction">> => [
             <<"transaction">>
           , <<"batch">>
           , <<"search-system">>
           , <<"history-system">>]
 %% ConditionalReadStatus-list
         , <<"conditionalreadstatus">> => [
             <<"not-supported">>
           , <<"modified-since">>
           , <<"not-match">>
           , <<"full-support">>]
 %% ReferenceHandlingPolicy-list
         , <<"referencehandlingpolicy">> => [
             <<"literal">>
           , <<"logical">>
           , <<"resolves">>
           , <<"enforced">>
           , <<"local">>]
 %% ConditionalDeleteStatus-list
         , <<"conditionaldeletestatus">> => [
             <<"not-supported">>
           , <<"single">>
           , <<"multiple">>]
 %% CarePlanActivityKind-list
         , <<"careplanactivitykind">> => [
             <<"Appointment">>
           , <<"CommunicationRequest">>
           , <<"DeviceRequest">>
           , <<"MedicationRequest">>
           , <<"NutritionOrder">>
           , <<"Task">>
           , <<"ServiceRequest">>
           , <<"VisionPrescription">>]
 %% CarePlanActivityStatus-list
         , <<"careplanactivitystatus">> => [
             <<"not-started">>
           , <<"scheduled">>
           , <<"in-progress">>
           , <<"on-hold">>
           , <<"completed">>
           , <<"cancelled">>
           , <<"stopped">>
           , <<"unknown">>
           , <<"entered-in-error">>]
 %% CarePlanIntent-list
         , <<"careplanintent">> => [
             <<"proposal">>
           , <<"plan">>
           , <<"order">>
           , <<"option">>]
 %% RequestStatus-list
         , <<"requeststatus">> => [
             <<"draft">>
           , <<"active">>
           , <<"on-hold">>
           , <<"revoked">>
           , <<"completed">>
           , <<"entered-in-error">>
           , <<"unknown">>]
 %% CareTeamStatus-list
         , <<"careteamstatus">> => [
             <<"proposed">>
           , <<"active">>
           , <<"suspended">>
           , <<"inactive">>
           , <<"entered-in-error">>]
 %% CatalogEntryRelationType-list
         , <<"catalogentryrelationtype">> => [
             <<"triggers">>
           , <<"is-replaced-by">>]
 %% ChargeItemStatus-list
         , <<"chargeitemstatus">> => [
             <<"planned">>
           , <<"billable">>
           , <<"not-billable">>
           , <<"aborted">>
           , <<"billed">>
           , <<"entered-in-error">>
           , <<"unknown">>]
 %% InvoicePriceComponentType-list
         , <<"invoicepricecomponenttype">> => [
             <<"base">>
           , <<"surcharge">>
           , <<"deduction">>
           , <<"discount">>
           , <<"tax">>
           , <<"informational">>]
 %% Use-list
         , <<"use">> => [
             <<"claim">>
           , <<"preauthorization">>
           , <<"predetermination">>]
 %% FinancialResourceStatusCodes-list
         , <<"financialresourcestatuscodes">> => [
             <<"active">>
           , <<"cancelled">>
           , <<"draft">>
           , <<"entered-in-error">>]
 %% ClaimProcessingCodes-list
         , <<"claimprocessingcodes">> => [
             <<"queued">>
           , <<"complete">>
           , <<"error">>
           , <<"partial">>]
 %% ClinicalImpressionStatus-list
         , <<"clinicalimpressionstatus">> => [
             <<"in-progress">>
           , <<"completed">>
           , <<"entered-in-error">>]
 %% FilterOperator-list
         , <<"filteroperator">> => [
             <<"=">>
           , <<"is-a">>
           , <<"descendent-of">>
           , <<"is-not-a">>
           , <<"regex">>
           , <<"in">>
           , <<"not-in">>
           , <<"generalizes">>
           , <<"exists">>]
 %% PropertyType-list
         , <<"propertytype">> => [
             <<"code">>
           , <<"Coding">>
           , <<"string">>
           , <<"integer">>
           , <<"boolean">>
           , <<"dateTime">>
           , <<"decimal">>]
 %% CodeSystemHierarchyMeaning-list
         , <<"codesystemhierarchymeaning">> => [
             <<"grouped-by">>
           , <<"is-a">>
           , <<"part-of">>
           , <<"classified-with">>]
 %% CodeSystemContentMode-list
         , <<"codesystemcontentmode">> => [
             <<"not-present">>
           , <<"example">>
           , <<"fragment">>
           , <<"complete">>
           , <<"supplement">>]
 %% EventStatus-list
         , <<"eventstatus">> => [
             <<"preparation">>
           , <<"in-progress">>
           , <<"not-done">>
           , <<"on-hold">>
           , <<"stopped">>
           , <<"completed">>
           , <<"entered-in-error">>
           , <<"unknown">>]
 %% CompartmentType-list
         , <<"compartmenttype">> => [
             <<"Patient">>
           , <<"Encounter">>
           , <<"RelatedPerson">>
           , <<"Practitioner">>
           , <<"Device">>]
 %% vConfidentialityClassification-list
         , <<"vconfidentialityclassification">> => [
             <<"U">>
           , <<"L">>
           , <<"M">>
           , <<"N">>
           , <<"R">>
           , <<"V">>]
 %% CompositionStatus-list
         , <<"compositionstatus">> => [
             <<"preliminary">>
           , <<"final">>
           , <<"amended">>
           , <<"entered-in-error">>]
 %% DocumentRelationshipType-list
         , <<"documentrelationshiptype">> => [
             <<"replaces">>
           , <<"transforms">>
           , <<"signs">>
           , <<"appends">>]
 %% CompositionAttestationMode-list
         , <<"compositionattestationmode">> => [
             <<"personal">>
           , <<"professional">>
           , <<"legal">>
           , <<"official">>]
 %% ListMode-list
         , <<"listmode">> => [
             <<"working">>
           , <<"snapshot">>
           , <<"changes">>]
 %% ConceptMapGroupUnmappedMode-list
         , <<"conceptmapgroupunmappedmode">> => [
             <<"provided">>
           , <<"fixed">>
           , <<"other-map">>]
 %% ConsentProvisionType-list
         , <<"consentprovisiontype">> => [
             <<"deny">>
           , <<"permit">>]
 %% ConsentDataMeaning-list
         , <<"consentdatameaning">> => [
             <<"instance">>
           , <<"related">>
           , <<"dependents">>
           , <<"authoredby">>]
 %% ConsentState-list
         , <<"consentstate">> => [
             <<"draft">>
           , <<"proposed">>
           , <<"active">>
           , <<"rejected">>
           , <<"inactive">>
           , <<"entered-in-error">>]
 %% ContractResourceStatusCodes-list
         , <<"contractresourcestatuscodes">> => [
             <<"amended">>
           , <<"appended">>
           , <<"cancelled">>
           , <<"disputed">>
           , <<"entered-in-error">>
           , <<"executable">>
           , <<"executed">>
           , <<"negotiable">>
           , <<"offered">>
           , <<"policy">>
           , <<"rejected">>
           , <<"renewed">>
           , <<"revoked">>
           , <<"resolved">>
           , <<"terminated">>]
 %% ContractResourcePublicationStatusCodes-list
         , <<"contractresourcepublicationstatuscodes">> => [
             <<"amended">>
           , <<"appended">>
           , <<"cancelled">>
           , <<"disputed">>
           , <<"entered-in-error">>
           , <<"executable">>
           , <<"executed">>
           , <<"negotiable">>
           , <<"offered">>
           , <<"policy">>
           , <<"rejected">>
           , <<"renewed">>
           , <<"revoked">>
           , <<"resolved">>
           , <<"terminated">>]
 %% EligibilityRequestPurpose-list
         , <<"eligibilityrequestpurpose">> => [
             <<"auth-requirements">>
           , <<"benefits">>
           , <<"discovery">>
           , <<"validation">>]
 %% EligibilityResponsePurpose-list
         , <<"eligibilityresponsepurpose">> => [
             <<"auth-requirements">>
           , <<"benefits">>
           , <<"discovery">>
           , <<"validation">>]
 %% ObservationStatus-list
         , <<"observationstatus">> => [
             <<"registered">>
           , <<"preliminary">>
           , <<"final">>
           , <<"amended">>
           , <<"corrected">>
           , <<"cancelled">>
           , <<"entered-in-error">>
           , <<"unknown">>]
 %% DetectedIssueSeverity-list
         , <<"detectedissueseverity">> => [
             <<"high">>
           , <<"moderate">>
           , <<"low">>]
 %% FHIRDeviceStatus-list
         , <<"fhirdevicestatus">> => [
             <<"active">>
           , <<"inactive">>
           , <<"entered-in-error">>
           , <<"unknown">>]
 %% DeviceNameType-list
         , <<"devicenametype">> => [
             <<"udi-label-name">>
           , <<"user-friendly-name">>
           , <<"patient-reported-name">>
           , <<"manufacturer-name">>
           , <<"model-name">>
           , <<"other">>]
 %% UDIEntryType-list
         , <<"udientrytype">> => [
             <<"barcode">>
           , <<"rfid">>
           , <<"manual">>
           , <<"card">>
           , <<"self-reported">>
           , <<"unknown">>]
 %% DeviceMetricCalibrationType-list
         , <<"devicemetriccalibrationtype">> => [
             <<"unspecified">>
           , <<"offset">>
           , <<"gain">>
           , <<"two-point">>]
 %% DeviceMetricColor-list
         , <<"devicemetriccolor">> => [
             <<"black">>
           , <<"red">>
           , <<"green">>
           , <<"yellow">>
           , <<"blue">>
           , <<"magenta">>
           , <<"cyan">>
           , <<"white">>]
 %% DeviceMetricCalibrationState-list
         , <<"devicemetriccalibrationstate">> => [
             <<"not-calibrated">>
           , <<"calibration-required">>
           , <<"calibrated">>
           , <<"unspecified">>]
 %% DeviceMetricOperationalStatus-list
         , <<"devicemetricoperationalstatus">> => [
             <<"on">>
           , <<"off">>
           , <<"standby">>
           , <<"entered-in-error">>]
 %% DeviceMetricCategory-list
         , <<"devicemetriccategory">> => [
             <<"measurement">>
           , <<"setting">>
           , <<"calculation">>
           , <<"unspecified">>]
 %% DeviceUseStatementStatus-list
         , <<"deviceusestatementstatus">> => [
             <<"active">>
           , <<"completed">>
           , <<"entered-in-error">>
           , <<"intended">>
           , <<"stopped">>
           , <<"on-hold">>]
 %% DiagnosticReportStatus-list
         , <<"diagnosticreportstatus">> => [
             <<"registered">>
           , <<"partial">>
           , <<"preliminary">>
           , <<"final">>
           , <<"amended">>
           , <<"corrected">>
           , <<"appended">>
           , <<"cancelled">>
           , <<"entered-in-error">>
           , <<"unknown">>]
 %% ExposureState-list
         , <<"exposurestate">> => [
             <<"exposure">>
           , <<"exposure-alternative">>]
 %% EncounterLocationStatus-list
         , <<"encounterlocationstatus">> => [
             <<"planned">>
           , <<"active">>
           , <<"reserved">>
           , <<"completed">>]
 %% EncounterStatus-list
         , <<"encounterstatus">> => [
             <<"planned">>
           , <<"arrived">>
           , <<"triaged">>
           , <<"in-progress">>
           , <<"onleave">>
           , <<"finished">>
           , <<"cancelled">>
           , <<"entered-in-error">>
           , <<"unknown">>]
 %% EndpointStatus-list
         , <<"endpointstatus">> => [
             <<"active">>
           , <<"suspended">>
           , <<"error">>
           , <<"off">>
           , <<"entered-in-error">>
           , <<"test">>]
 %% EpisodeOfCareStatus-list
         , <<"episodeofcarestatus">> => [
             <<"planned">>
           , <<"waitlist">>
           , <<"active">>
           , <<"onhold">>
           , <<"finished">>
           , <<"cancelled">>
           , <<"entered-in-error">>]
 %% EvidenceVariableType-list
         , <<"evidencevariabletype">> => [
             <<"dichotomous">>
           , <<"continuous">>
           , <<"descriptive">>]
 %% GroupMeasure-list
         , <<"groupmeasure">> => [
             <<"mean">>
           , <<"median">>
           , <<"mean-of-mean">>
           , <<"mean-of-median">>
           , <<"median-of-mean">>
           , <<"median-of-median">>]
 %% ExampleScenarioActorType-list
         , <<"examplescenarioactortype">> => [
             <<"person">>
           , <<"entity">>]
 %% ResourceType-list
         , <<"resourcetype">> => [
             <<"Account">>
           , <<"ActivityDefinition">>
           , <<"AdverseEvent">>
           , <<"AllergyIntolerance">>
           , <<"Appointment">>
           , <<"AppointmentResponse">>
           , <<"AuditEvent">>
           , <<"Basic">>
           , <<"Binary">>
           , <<"BiologicallyDerivedProduct">>
           , <<"BodyStructure">>
           , <<"Bundle">>
           , <<"CapabilityStatement">>
           , <<"CarePlan">>
           , <<"CareTeam">>
           , <<"CatalogEntry">>
           , <<"ChargeItem">>
           , <<"ChargeItemDefinition">>
           , <<"Claim">>
           , <<"ClaimResponse">>
           , <<"ClinicalImpression">>
           , <<"CodeSystem">>
           , <<"Communication">>
           , <<"CommunicationRequest">>
           , <<"CompartmentDefinition">>
           , <<"Composition">>
           , <<"ConceptMap">>
           , <<"Condition">>
           , <<"Consent">>
           , <<"Contract">>
           , <<"Coverage">>
           , <<"CoverageEligibilityRequest">>
           , <<"CoverageEligibilityResponse">>
           , <<"DetectedIssue">>
           , <<"Device">>
           , <<"DeviceDefinition">>
           , <<"DeviceMetric">>
           , <<"DeviceRequest">>
           , <<"DeviceUseStatement">>
           , <<"DiagnosticReport">>
           , <<"DocumentManifest">>
           , <<"DocumentReference">>
           , <<"DomainResource">>
           , <<"EffectEvidenceSynthesis">>
           , <<"Encounter">>
           , <<"Endpoint">>
           , <<"EnrollmentRequest">>
           , <<"EnrollmentResponse">>
           , <<"EpisodeOfCare">>
           , <<"EventDefinition">>
           , <<"Evidence">>
           , <<"EvidenceVariable">>
           , <<"ExampleScenario">>
           , <<"ExplanationOfBenefit">>
           , <<"FamilyMemberHistory">>
           , <<"Flag">>
           , <<"Goal">>
           , <<"GraphDefinition">>
           , <<"Group">>
           , <<"GuidanceResponse">>
           , <<"HealthcareService">>
           , <<"ImagingStudy">>
           , <<"Immunization">>
           , <<"ImmunizationEvaluation">>
           , <<"ImmunizationRecommendation">>
           , <<"ImplementationGuide">>
           , <<"InsurancePlan">>
           , <<"Invoice">>
           , <<"Library">>
           , <<"Linkage">>
           , <<"List">>
           , <<"Location">>
           , <<"Measure">>
           , <<"MeasureReport">>
           , <<"Media">>
           , <<"Medication">>
           , <<"MedicationAdministration">>
           , <<"MedicationDispense">>
           , <<"MedicationKnowledge">>
           , <<"MedicationRequest">>
           , <<"MedicationStatement">>
           , <<"MedicinalProduct">>
           , <<"MedicinalProductAuthorization">>
           , <<"MedicinalProductContraindication">>
           , <<"MedicinalProductIndication">>
           , <<"MedicinalProductIngredient">>
           , <<"MedicinalProductInteraction">>
           , <<"MedicinalProductManufactured">>
           , <<"MedicinalProductPackaged">>
           , <<"MedicinalProductPharmaceutical">>
           , <<"MedicinalProductUndesirableEffect">>
           , <<"MessageDefinition">>
           , <<"MessageHeader">>
           , <<"MolecularSequence">>
           , <<"NamingSystem">>
           , <<"NutritionOrder">>
           , <<"Observation">>
           , <<"ObservationDefinition">>
           , <<"OperationDefinition">>
           , <<"OperationOutcome">>
           , <<"Organization">>
           , <<"OrganizationAffiliation">>
           , <<"Parameters">>
           , <<"Patient">>
           , <<"PaymentNotice">>
           , <<"PaymentReconciliation">>
           , <<"Person">>
           , <<"PlanDefinition">>
           , <<"Practitioner">>
           , <<"PractitionerRole">>
           , <<"Procedure">>
           , <<"Provenance">>
           , <<"Questionnaire">>
           , <<"QuestionnaireResponse">>
           , <<"RelatedPerson">>
           , <<"RequestGroup">>
           , <<"ResearchDefinition">>
           , <<"ResearchElementDefinition">>
           , <<"ResearchStudy">>
           , <<"ResearchSubject">>
           , <<"Resource">>
           , <<"RiskAssessment">>
           , <<"RiskEvidenceSynthesis">>
           , <<"Schedule">>
           , <<"SearchParameter">>
           , <<"ServiceRequest">>
           , <<"Slot">>
           , <<"Specimen">>
           , <<"SpecimenDefinition">>
           , <<"StructureDefinition">>
           , <<"StructureMap">>
           , <<"Subscription">>
           , <<"Substance">>
           , <<"SubstanceNucleicAcid">>
           , <<"SubstancePolymer">>
           , <<"SubstanceProtein">>
           , <<"SubstanceReferenceInformation">>
           , <<"SubstanceSourceMaterial">>
           , <<"SubstanceSpecification">>
           , <<"SupplyDelivery">>
           , <<"SupplyRequest">>
           , <<"Task">>
           , <<"TerminologyCapabilities">>
           , <<"TestReport">>
           , <<"TestScript">>
           , <<"ValueSet">>
           , <<"VerificationResult">>
           , <<"VisionPrescription">>]
 %% ExplanationOfBenefitStatus-list
         , <<"explanationofbenefitstatus">> => [
             <<"active">>
           , <<"cancelled">>
           , <<"draft">>
           , <<"entered-in-error">>]
 %% FamilyHistoryStatus-list
         , <<"familyhistorystatus">> => [
             <<"partial">>
           , <<"completed">>
           , <<"entered-in-error">>
           , <<"health-unknown">>]
 %% FlagStatus-list
         , <<"flagstatus">> => [
             <<"active">>
           , <<"inactive">>
           , <<"entered-in-error">>]
 %% GoalLifecycleStatus-list
         , <<"goallifecyclestatus">> => [
             <<"proposed">>
           , <<"planned">>
           , <<"accepted">>
           , <<"active">>
           , <<"on-hold">>
           , <<"completed">>
           , <<"cancelled">>
           , <<"entered-in-error">>
           , <<"rejected">>]
 %% GraphCompartmentUse-list
         , <<"graphcompartmentuse">> => [
             <<"condition">>
           , <<"requirement">>]
 %% GraphCompartmentRule-list
         , <<"graphcompartmentrule">> => [
             <<"identical">>
           , <<"matching">>
           , <<"different">>
           , <<"custom">>]
 %% GroupType-list
         , <<"grouptype">> => [
             <<"person">>
           , <<"animal">>
           , <<"practitioner">>
           , <<"device">>
           , <<"medication">>
           , <<"substance">>]
 %% GuidanceResponseStatus-list
         , <<"guidanceresponsestatus">> => [
             <<"success">>
           , <<"data-requested">>
           , <<"data-required">>
           , <<"in-progress">>
           , <<"failure">>
           , <<"entered-in-error">>]
 %% DaysOfWeek-list
         , <<"daysofweek">> => [
             <<"mon">>
           , <<"tue">>
           , <<"wed">>
           , <<"thu">>
           , <<"fri">>
           , <<"sat">>
           , <<"sun">>]
 %% ImagingStudyStatus-list
         , <<"imagingstudystatus">> => [
             <<"registered">>
           , <<"available">>
           , <<"cancelled">>
           , <<"entered-in-error">>
           , <<"unknown">>]
 %% ImmunizationStatusCodes-list
         , <<"immunizationstatuscodes">> => [
             <<"completed">>
           , <<"entered-in-error">>
           , <<"not-done">>]
 %% ImmunizationEvaluationStatusCodes-list
         , <<"immunizationevaluationstatuscodes">> => [
             <<"completed">>
           , <<"entered-in-error">>]
 %% GuideParameterCode-list
         , <<"guideparametercode">> => [
             <<"apply">>
           , <<"path-resource">>
           , <<"path-pages">>
           , <<"path-tx-cache">>
           , <<"expansion-parameter">>
           , <<"rule-broken-links">>
           , <<"generate-xml">>
           , <<"generate-json">>
           , <<"generate-turtle">>
           , <<"html-template">>]
 %% SPDXLicense-list
         , <<"spdxlicense">> => [
             <<"not-open-source">>
           , <<"0BSD">>
           , <<"AAL">>
           , <<"Abstyles">>
           , <<"Adobe-2006">>
           , <<"Adobe-Glyph">>
           , <<"ADSL">>
           , <<"AFL-1.1">>
           , <<"AFL-1.2">>
           , <<"AFL-2.0">>
           , <<"AFL-2.1">>
           , <<"AFL-3.0">>
           , <<"Afmparse">>
           , <<"AGPL-1.0-only">>
           , <<"AGPL-1.0-or-later">>
           , <<"AGPL-3.0-only">>
           , <<"AGPL-3.0-or-later">>
           , <<"Aladdin">>
           , <<"AMDPLPA">>
           , <<"AML">>
           , <<"AMPAS">>
           , <<"ANTLR-PD">>
           , <<"Apache-1.0">>
           , <<"Apache-1.1">>
           , <<"Apache-2.0">>
           , <<"APAFML">>
           , <<"APL-1.0">>
           , <<"APSL-1.0">>
           , <<"APSL-1.1">>
           , <<"APSL-1.2">>
           , <<"APSL-2.0">>
           , <<"Artistic-1.0-cl8">>
           , <<"Artistic-1.0-Perl">>
           , <<"Artistic-1.0">>
           , <<"Artistic-2.0">>
           , <<"Bahyph">>
           , <<"Barr">>
           , <<"Beerware">>
           , <<"BitTorrent-1.0">>
           , <<"BitTorrent-1.1">>
           , <<"Borceux">>
           , <<"BSD-1-Clause">>
           , <<"BSD-2-Clause-FreeBSD">>
           , <<"BSD-2-Clause-NetBSD">>
           , <<"BSD-2-Clause-Patent">>
           , <<"BSD-2-Clause">>
           , <<"BSD-3-Clause-Attribution">>
           , <<"BSD-3-Clause-Clear">>
           , <<"BSD-3-Clause-LBNL">>
           , <<"BSD-3-Clause-No-Nuclear-License-2014">>
           , <<"BSD-3-Clause-No-Nuclear-License">>
           , <<"BSD-3-Clause-No-Nuclear-Warranty">>
           , <<"BSD-3-Clause">>
           , <<"BSD-4-Clause-UC">>
           , <<"BSD-4-Clause">>
           , <<"BSD-Protection">>
           , <<"BSD-Source-Code">>
           , <<"BSL-1.0">>
           , <<"bzip2-1.0.5">>
           , <<"bzip2-1.0.6">>
           , <<"Caldera">>
           , <<"CATOSL-1.1">>
           , <<"CC-BY-1.0">>
           , <<"CC-BY-2.0">>
           , <<"CC-BY-2.5">>
           , <<"CC-BY-3.0">>
           , <<"CC-BY-4.0">>
           , <<"CC-BY-NC-1.0">>
           , <<"CC-BY-NC-2.0">>
           , <<"CC-BY-NC-2.5">>
           , <<"CC-BY-NC-3.0">>
           , <<"CC-BY-NC-4.0">>
           , <<"CC-BY-NC-ND-1.0">>
           , <<"CC-BY-NC-ND-2.0">>
           , <<"CC-BY-NC-ND-2.5">>
           , <<"CC-BY-NC-ND-3.0">>
           , <<"CC-BY-NC-ND-4.0">>
           , <<"CC-BY-NC-SA-1.0">>
           , <<"CC-BY-NC-SA-2.0">>
           , <<"CC-BY-NC-SA-2.5">>
           , <<"CC-BY-NC-SA-3.0">>
           , <<"CC-BY-NC-SA-4.0">>
           , <<"CC-BY-ND-1.0">>
           , <<"CC-BY-ND-2.0">>
           , <<"CC-BY-ND-2.5">>
           , <<"CC-BY-ND-3.0">>
           , <<"CC-BY-ND-4.0">>
           , <<"CC-BY-SA-1.0">>
           , <<"CC-BY-SA-2.0">>
           , <<"CC-BY-SA-2.5">>
           , <<"CC-BY-SA-3.0">>
           , <<"CC-BY-SA-4.0">>
           , <<"CC0-1.0">>
           , <<"CDDL-1.0">>
           , <<"CDDL-1.1">>
           , <<"CDLA-Permissive-1.0">>
           , <<"CDLA-Sharing-1.0">>
           , <<"CECILL-1.0">>
           , <<"CECILL-1.1">>
           , <<"CECILL-2.0">>
           , <<"CECILL-2.1">>
           , <<"CECILL-B">>
           , <<"CECILL-C">>
           , <<"ClArtistic">>
           , <<"CNRI-Jython">>
           , <<"CNRI-Python-GPL-Compatible">>
           , <<"CNRI-Python">>
           , <<"Condor-1.1">>
           , <<"CPAL-1.0">>
           , <<"CPL-1.0">>
           , <<"CPOL-1.02">>
           , <<"Crossword">>
           , <<"CrystalStacker">>
           , <<"CUA-OPL-1.0">>
           , <<"Cube">>
           , <<"curl">>
           , <<"D-FSL-1.0">>
           , <<"diffmark">>
           , <<"DOC">>
           , <<"Dotseqn">>
           , <<"DSDP">>
           , <<"dvipdfm">>
           , <<"ECL-1.0">>
           , <<"ECL-2.0">>
           , <<"EFL-1.0">>
           , <<"EFL-2.0">>
           , <<"eGenix">>
           , <<"Entessa">>
           , <<"EPL-1.0">>
           , <<"EPL-2.0">>
           , <<"ErlPL-1.1">>
           , <<"EUDatagrid">>
           , <<"EUPL-1.0">>
           , <<"EUPL-1.1">>
           , <<"EUPL-1.2">>
           , <<"Eurosym">>
           , <<"Fair">>
           , <<"Frameworx-1.0">>
           , <<"FreeImage">>
           , <<"FSFAP">>
           , <<"FSFUL">>
           , <<"FSFULLR">>
           , <<"FTL">>
           , <<"GFDL-1.1-only">>
           , <<"GFDL-1.1-or-later">>
           , <<"GFDL-1.2-only">>
           , <<"GFDL-1.2-or-later">>
           , <<"GFDL-1.3-only">>
           , <<"GFDL-1.3-or-later">>
           , <<"Giftware">>
           , <<"GL2PS">>
           , <<"Glide">>
           , <<"Glulxe">>
           , <<"gnuplot">>
           , <<"GPL-1.0-only">>
           , <<"GPL-1.0-or-later">>
           , <<"GPL-2.0-only">>
           , <<"GPL-2.0-or-later">>
           , <<"GPL-3.0-only">>
           , <<"GPL-3.0-or-later">>
           , <<"gSOAP-1.3b">>
           , <<"HaskellReport">>
           , <<"HPND">>
           , <<"IBM-pibs">>
           , <<"ICU">>
           , <<"IJG">>
           , <<"ImageMagick">>
           , <<"iMatix">>
           , <<"Imlib2">>
           , <<"Info-ZIP">>
           , <<"Intel-ACPI">>
           , <<"Intel">>
           , <<"Interbase-1.0">>
           , <<"IPA">>
           , <<"IPL-1.0">>
           , <<"ISC">>
           , <<"JasPer-2.0">>
           , <<"JSON">>
           , <<"LAL-1.2">>
           , <<"LAL-1.3">>
           , <<"Latex2e">>
           , <<"Leptonica">>
           , <<"LGPL-2.0-only">>
           , <<"LGPL-2.0-or-later">>
           , <<"LGPL-2.1-only">>
           , <<"LGPL-2.1-or-later">>
           , <<"LGPL-3.0-only">>
           , <<"LGPL-3.0-or-later">>
           , <<"LGPLLR">>
           , <<"Libpng">>
           , <<"libtiff">>
           , <<"LiLiQ-P-1.1">>
           , <<"LiLiQ-R-1.1">>
           , <<"LiLiQ-Rplus-1.1">>
           , <<"Linux-OpenIB">>
           , <<"LPL-1.0">>
           , <<"LPL-1.02">>
           , <<"LPPL-1.0">>
           , <<"LPPL-1.1">>
           , <<"LPPL-1.2">>
           , <<"LPPL-1.3a">>
           , <<"LPPL-1.3c">>
           , <<"MakeIndex">>
           , <<"MirOS">>
           , <<"MIT-0">>
           , <<"MIT-advertising">>
           , <<"MIT-CMU">>
           , <<"MIT-enna">>
           , <<"MIT-feh">>
           , <<"MIT">>
           , <<"MITNFA">>
           , <<"Motosoto">>
           , <<"mpich2">>
           , <<"MPL-1.0">>
           , <<"MPL-1.1">>
           , <<"MPL-2.0-no-copyleft-exception">>
           , <<"MPL-2.0">>
           , <<"MS-PL">>
           , <<"MS-RL">>
           , <<"MTLL">>
           , <<"Multics">>
           , <<"Mup">>
           , <<"NASA-1.3">>
           , <<"Naumen">>
           , <<"NBPL-1.0">>
           , <<"NCSA">>
           , <<"Net-SNMP">>
           , <<"NetCDF">>
           , <<"Newsletr">>
           , <<"NGPL">>
           , <<"NLOD-1.0">>
           , <<"NLPL">>
           , <<"Nokia">>
           , <<"NOSL">>
           , <<"Noweb">>
           , <<"NPL-1.0">>
           , <<"NPL-1.1">>
           , <<"NPOSL-3.0">>
           , <<"NRL">>
           , <<"NTP">>
           , <<"OCCT-PL">>
           , <<"OCLC-2.0">>
           , <<"ODbL-1.0">>
           , <<"OFL-1.0">>
           , <<"OFL-1.1">>
           , <<"OGTSL">>
           , <<"OLDAP-1.1">>
           , <<"OLDAP-1.2">>
           , <<"OLDAP-1.3">>
           , <<"OLDAP-1.4">>
           , <<"OLDAP-2.0.1">>
           , <<"OLDAP-2.0">>
           , <<"OLDAP-2.1">>
           , <<"OLDAP-2.2.1">>
           , <<"OLDAP-2.2.2">>
           , <<"OLDAP-2.2">>
           , <<"OLDAP-2.3">>
           , <<"OLDAP-2.4">>
           , <<"OLDAP-2.5">>
           , <<"OLDAP-2.6">>
           , <<"OLDAP-2.7">>
           , <<"OLDAP-2.8">>
           , <<"OML">>
           , <<"OpenSSL">>
           , <<"OPL-1.0">>
           , <<"OSET-PL-2.1">>
           , <<"OSL-1.0">>
           , <<"OSL-1.1">>
           , <<"OSL-2.0">>
           , <<"OSL-2.1">>
           , <<"OSL-3.0">>
           , <<"PDDL-1.0">>
           , <<"PHP-3.0">>
           , <<"PHP-3.01">>
           , <<"Plexus">>
           , <<"PostgreSQL">>
           , <<"psfrag">>
           , <<"psutils">>
           , <<"Python-2.0">>
           , <<"Qhull">>
           , <<"QPL-1.0">>
           , <<"Rdisc">>
           , <<"RHeCos-1.1">>
           , <<"RPL-1.1">>
           , <<"RPL-1.5">>
           , <<"RPSL-1.0">>
           , <<"RSA-MD">>
           , <<"RSCPL">>
           , <<"Ruby">>
           , <<"SAX-PD">>
           , <<"Saxpath">>
           , <<"SCEA">>
           , <<"Sendmail">>
           , <<"SGI-B-1.0">>
           , <<"SGI-B-1.1">>
           , <<"SGI-B-2.0">>
           , <<"SimPL-2.0">>
           , <<"SISSL-1.2">>
           , <<"SISSL">>
           , <<"Sleepycat">>
           , <<"SMLNJ">>
           , <<"SMPPL">>
           , <<"SNIA">>
           , <<"Spencer-86">>
           , <<"Spencer-94">>
           , <<"Spencer-99">>
           , <<"SPL-1.0">>
           , <<"SugarCRM-1.1.3">>
           , <<"SWL">>
           , <<"TCL">>
           , <<"TCP-wrappers">>
           , <<"TMate">>
           , <<"TORQUE-1.1">>
           , <<"TOSL">>
           , <<"Unicode-DFS-2015">>
           , <<"Unicode-DFS-2016">>
           , <<"Unicode-TOU">>
           , <<"Unlicense">>
           , <<"UPL-1.0">>
           , <<"Vim">>
           , <<"VOSTROM">>
           , <<"VSL-1.0">>
           , <<"W3C-19980720">>
           , <<"W3C-20150513">>
           , <<"W3C">>
           , <<"Watcom-1.0">>
           , <<"Wsuipa">>
           , <<"WTFPL">>
           , <<"X11">>
           , <<"Xerox">>
           , <<"XFree86-1.1">>
           , <<"xinetd">>
           , <<"Xnet">>
           , <<"xpp">>
           , <<"XSkat">>
           , <<"YPL-1.0">>
           , <<"YPL-1.1">>
           , <<"Zed">>
           , <<"Zend-2.0">>
           , <<"Zimbra-1.3">>
           , <<"Zimbra-1.4">>
           , <<"zlib-acknowledgement">>
           , <<"Zlib">>
           , <<"ZPL-1.1">>
           , <<"ZPL-2.0">>
           , <<"ZPL-2.1">>]
 %% GuidePageGeneration-list
         , <<"guidepagegeneration">> => [
             <<"html">>
           , <<"markdown">>
           , <<"xml">>
           , <<"generated">>]
 %% InvoiceStatus-list
         , <<"invoicestatus">> => [
             <<"draft">>
           , <<"issued">>
           , <<"balanced">>
           , <<"cancelled">>
           , <<"entered-in-error">>]
 %% LinkageType-list
         , <<"linkagetype">> => [
             <<"source">>
           , <<"alternate">>
           , <<"historical">>]
 %% ListStatus-list
         , <<"liststatus">> => [
             <<"current">>
           , <<"retired">>
           , <<"entered-in-error">>]
 %% LocationMode-list
         , <<"locationmode">> => [
             <<"instance">>
           , <<"kind">>]
 %% LocationStatus-list
         , <<"locationstatus">> => [
             <<"active">>
           , <<"suspended">>
           , <<"inactive">>]
 %% MeasureReportStatus-list
         , <<"measurereportstatus">> => [
             <<"complete">>
           , <<"pending">>
           , <<"error">>]
 %% MeasureReportType-list
         , <<"measurereporttype">> => [
             <<"individual">>
           , <<"subject-list">>
           , <<"summary">>
           , <<"data-collection">>]
 %% MedicationStatusCodes-list
         , <<"medicationstatuscodes">> => [
             <<"active">>
           , <<"inactive">>
           , <<"entered-in-error">>]
 %% medicationrequestStatus-list
         , <<"medicationrequeststatus">> => [
             <<"active">>
           , <<"on-hold">>
           , <<"cancelled">>
           , <<"completed">>
           , <<"entered-in-error">>
           , <<"stopped">>
           , <<"draft">>
           , <<"unknown">>]
 %% medicationRequestIntent-list
         , <<"medicationrequestintent">> => [
             <<"proposal">>
           , <<"plan">>
           , <<"order">>
           , <<"original-order">>
           , <<"reflex-order">>
           , <<"filler-order">>
           , <<"instance-order">>
           , <<"option">>]
 %% MessageSignificanceCategory-list
         , <<"messagesignificancecategory">> => [
             <<"consequence">>
           , <<"currency">>
           , <<"notification">>]
 %% messageheaderResponseRequest-list
         , <<"messageheaderresponserequest">> => [
             <<"always">>
           , <<"on-error">>
           , <<"never">>
           , <<"on-success">>]
 %% ResponseType-list
         , <<"responsetype">> => [
             <<"ok">>
           , <<"transient-error">>
           , <<"fatal-error">>]
 %% strandType-list
         , <<"strandtype">> => [
             <<"watson">>
           , <<"crick">>]
 %% orientationType-list
         , <<"orientationtype">> => [
             <<"sense">>
           , <<"antisense">>]
 %% repositoryType-list
         , <<"repositorytype">> => [
             <<"directlink">>
           , <<"openapi">>
           , <<"login">>
           , <<"oauth">>
           , <<"other">>]
 %% qualityType-list
         , <<"qualitytype">> => [
             <<"indel">>
           , <<"snp">>
           , <<"unknown">>]
 %% sequenceType-list
         , <<"sequencetype">> => [
             <<"aa">>
           , <<"dna">>
           , <<"rna">>]
 %% NamingSystemIdentifierType-list
         , <<"namingsystemidentifiertype">> => [
             <<"oid">>
           , <<"uuid">>
           , <<"uri">>
           , <<"other">>]
 %% NamingSystemType-list
         , <<"namingsystemtype">> => [
             <<"codesystem">>
           , <<"identifier">>
           , <<"root">>]
 %% ObservationDataType-list
         , <<"observationdatatype">> => [
             <<"Quantity">>
           , <<"CodeableConcept">>
           , <<"string">>
           , <<"boolean">>
           , <<"integer">>
           , <<"Range">>
           , <<"Ratio">>
           , <<"SampledData">>
           , <<"time">>
           , <<"dateTime">>
           , <<"Period">>]
 %% ObservationRangeCategory-list
         , <<"observationrangecategory">> => [
             <<"reference">>
           , <<"critical">>
           , <<"absolute">>]
 %% OperationParameterUse-list
         , <<"operationparameteruse">> => [
             <<"in">>
           , <<"out">>]
 %% OperationKind-list
         , <<"operationkind">> => [
             <<"operation">>
           , <<"query">>]
 %% IssueType-list
         , <<"issuetype">> => [
             <<"invalid">>
           , <<"structure">>
           , <<"required">>
           , <<"value">>
           , <<"invariant">>
           , <<"security">>
           , <<"login">>
           , <<"unknown">>
           , <<"expired">>
           , <<"forbidden">>
           , <<"suppressed">>
           , <<"processing">>
           , <<"not-supported">>
           , <<"duplicate">>
           , <<"multiple-matches">>
           , <<"not-found">>
           , <<"deleted">>
           , <<"too-long">>
           , <<"code-invalid">>
           , <<"extension">>
           , <<"too-costly">>
           , <<"business-rule">>
           , <<"conflict">>
           , <<"transient">>
           , <<"lock-error">>
           , <<"no-store">>
           , <<"exception">>
           , <<"timeout">>
           , <<"incomplete">>
           , <<"throttled">>
           , <<"informational">>]
 %% IssueSeverity-list
         , <<"issueseverity">> => [
             <<"fatal">>
           , <<"error">>
           , <<"warning">>
           , <<"information">>]
 %% LinkType-list
         , <<"linktype">> => [
             <<"replaced-by">>
           , <<"replaces">>
           , <<"refer">>
           , <<"seealso">>]
 %% IdentityAssuranceLevel-list
         , <<"identityassurancelevel">> => [
             <<"level1">>
           , <<"level2">>
           , <<"level3">>
           , <<"level4">>]
 %% ActionRequiredBehavior-list
         , <<"actionrequiredbehavior">> => [
             <<"must">>
           , <<"could">>
           , <<"must-unless-documented">>]
 %% ActionRelationshipType-list
         , <<"actionrelationshiptype">> => [
             <<"before-start">>
           , <<"before">>
           , <<"before-end">>
           , <<"concurrent-with-start">>
           , <<"concurrent">>
           , <<"concurrent-with-end">>
           , <<"after-start">>
           , <<"after">>
           , <<"after-end">>]
 %% ActionGroupingBehavior-list
         , <<"actiongroupingbehavior">> => [
             <<"visual-group">>
           , <<"logical-group">>
           , <<"sentence-group">>]
 %% ActionSelectionBehavior-list
         , <<"actionselectionbehavior">> => [
             <<"any">>
           , <<"all">>
           , <<"all-or-none">>
           , <<"exactly-one">>
           , <<"at-most-one">>
           , <<"one-or-more">>]
 %% ActionCardinalityBehavior-list
         , <<"actioncardinalitybehavior">> => [
             <<"single">>
           , <<"multiple">>]
 %% ActionPrecheckBehavior-list
         , <<"actionprecheckbehavior">> => [
             <<"yes">>
           , <<"no">>]
 %% ActionConditionKind-list
         , <<"actionconditionkind">> => [
             <<"applicability">>
           , <<"start">>
           , <<"stop">>]
 %% ProvenanceEntityRole-list
         , <<"provenanceentityrole">> => [
             <<"derivation">>
           , <<"revision">>
           , <<"quotation">>
           , <<"source">>
           , <<"removal">>]
 %% QuestionnaireItemType-list
         , <<"questionnaireitemtype">> => [
             <<"group">>
           , <<"display">>
           , <<"boolean">>
           , <<"decimal">>
           , <<"integer">>
           , <<"date">>
           , <<"dateTime">>
           , <<"time">>
           , <<"string">>
           , <<"text">>
           , <<"url">>
           , <<"choice">>
           , <<"open-choice">>
           , <<"attachment">>
           , <<"reference">>
           , <<"quantity">>]
 %% EnableWhenBehavior-list
         , <<"enablewhenbehavior">> => [
             <<"all">>
           , <<"any">>]
 %% QuestionnaireItemOperator-list
         , <<"questionnaireitemoperator">> => [
             <<"exists">>
           , <<"=">>
           , <<"!=">>
           , <<">">>
           , <<"<">>
           , <<">=">>
           , <<"<=">>]
 %% QuestionnaireResponseStatus-list
         , <<"questionnaireresponsestatus">> => [
             <<"in-progress">>
           , <<"completed">>
           , <<"amended">>
           , <<"entered-in-error">>
           , <<"stopped">>]
 %% ResearchElementType-list
         , <<"researchelementtype">> => [
             <<"population">>
           , <<"exposure">>
           , <<"outcome">>]
 %% VariableType-list
         , <<"variabletype">> => [
             <<"dichotomous">>
           , <<"continuous">>
           , <<"descriptive">>]
 %% ResearchStudyStatus-list
         , <<"researchstudystatus">> => [
             <<"active">>
           , <<"administratively-completed">>
           , <<"approved">>
           , <<"closed-to-accrual">>
           , <<"closed-to-accrual-and-intervention">>
           , <<"completed">>
           , <<"disapproved">>
           , <<"in-review">>
           , <<"temporarily-closed-to-accrual">>
           , <<"temporarily-closed-to-accrual-and-intervention">>
           , <<"withdrawn">>]
 %% ResearchSubjectStatus-list
         , <<"researchsubjectstatus">> => [
             <<"candidate">>
           , <<"eligible">>
           , <<"follow-up">>
           , <<"ineligible">>
           , <<"not-registered">>
           , <<"off-study">>
           , <<"on-study">>
           , <<"on-study-intervention">>
           , <<"on-study-observation">>
           , <<"pending-on-study">>
           , <<"potential-candidate">>
           , <<"screening">>
           , <<"withdrawn">>]
 %% XPathUsageType-list
         , <<"xpathusagetype">> => [
             <<"normal">>
           , <<"phonetic">>
           , <<"nearby">>
           , <<"distance">>
           , <<"other">>]
 %% SearchModifierCode-list
         , <<"searchmodifiercode">> => [
             <<"missing">>
           , <<"exact">>
           , <<"contains">>
           , <<"not">>
           , <<"text">>
           , <<"in">>
           , <<"not-in">>
           , <<"below">>
           , <<"above">>
           , <<"type">>
           , <<"identifier">>
           , <<"ofType">>]
 %% SearchComparator-list
         , <<"searchcomparator">> => [
             <<"eq">>
           , <<"ne">>
           , <<"gt">>
           , <<"lt">>
           , <<"ge">>
           , <<"le">>
           , <<"sa">>
           , <<"eb">>
           , <<"ap">>]
 %% SlotStatus-list
         , <<"slotstatus">> => [
             <<"busy">>
           , <<"free">>
           , <<"busy-unavailable">>
           , <<"busy-tentative">>
           , <<"entered-in-error">>]
 %% SpecimenStatus-list
         , <<"specimenstatus">> => [
             <<"available">>
           , <<"unavailable">>
           , <<"unsatisfactory">>
           , <<"entered-in-error">>]
 %% SpecimenContainedPreference-list
         , <<"specimencontainedpreference">> => [
             <<"preferred">>
           , <<"alternate">>]
 %% StructureDefinitionKind-list
         , <<"structuredefinitionkind">> => [
             <<"primitive-type">>
           , <<"complex-type">>
           , <<"resource">>
           , <<"logical">>]
 %% TypeDerivationRule-list
         , <<"typederivationrule">> => [
             <<"specialization">>
           , <<"constraint">>]
 %% ExtensionContextType-list
         , <<"extensioncontexttype">> => [
             <<"fhirpath">>
           , <<"element">>
           , <<"extension">>]
 %% StructureMapInputMode-list
         , <<"structuremapinputmode">> => [
             <<"source">>
           , <<"target">>]
 %% StructureMapContextType-list
         , <<"structuremapcontexttype">> => [
             <<"type">>
           , <<"variable">>]
 %% StructureMapTargetListMode-list
         , <<"structuremaptargetlistmode">> => [
             <<"first">>
           , <<"share">>
           , <<"last">>
           , <<"collate">>]
 %% StructureMapTransform-list
         , <<"structuremaptransform">> => [
             <<"create">>
           , <<"copy">>
           , <<"truncate">>
           , <<"escape">>
           , <<"cast">>
           , <<"append">>
           , <<"translate">>
           , <<"reference">>
           , <<"dateOp">>
           , <<"uuid">>
           , <<"pointer">>
           , <<"evaluate">>
           , <<"cc">>
           , <<"c">>
           , <<"qty">>
           , <<"id">>
           , <<"cp">>]
 %% StructureMapSourceListMode-list
         , <<"structuremapsourcelistmode">> => [
             <<"first">>
           , <<"not_first">>
           , <<"last">>
           , <<"not_last">>
           , <<"only_one">>]
 %% StructureMapGroupTypeMode-list
         , <<"structuremapgrouptypemode">> => [
             <<"none">>
           , <<"types">>
           , <<"type-and-types">>]
 %% StructureMapModelMode-list
         , <<"structuremapmodelmode">> => [
             <<"source">>
           , <<"queried">>
           , <<"target">>
           , <<"produced">>]
 %% SubscriptionStatus-list
         , <<"subscriptionstatus">> => [
             <<"requested">>
           , <<"active">>
           , <<"error">>
           , <<"off">>]
 %% SubscriptionChannelType-list
         , <<"subscriptionchanneltype">> => [
             <<"rest-hook">>
           , <<"websocket">>
           , <<"email">>
           , <<"sms">>
           , <<"message">>]
 %% FHIRSubstanceStatus-list
         , <<"fhirsubstancestatus">> => [
             <<"active">>
           , <<"inactive">>
           , <<"entered-in-error">>]
 %% SupplyDeliveryStatus-list
         , <<"supplydeliverystatus">> => [
             <<"in-progress">>
           , <<"completed">>
           , <<"abandoned">>
           , <<"entered-in-error">>]
 %% SupplyRequestStatus-list
         , <<"supplyrequeststatus">> => [
             <<"draft">>
           , <<"active">>
           , <<"suspended">>
           , <<"cancelled">>
           , <<"completed">>
           , <<"entered-in-error">>
           , <<"unknown">>]
 %% TaskStatus-list
         , <<"taskstatus">> => [
             <<"draft">>
           , <<"requested">>
           , <<"received">>
           , <<"accepted">>
           , <<"rejected">>
           , <<"ready">>
           , <<"cancelled">>
           , <<"in-progress">>
           , <<"on-hold">>
           , <<"failed">>
           , <<"completed">>
           , <<"entered-in-error">>]
 %% TaskIntent-list
         , <<"taskintent">> => [
             <<"unknown">>
           , <<"proposal">>
           , <<"plan">>
           , <<"order">>
           , <<"original-order">>
           , <<"reflex-order">>
           , <<"filler-order">>
           , <<"instance-order">>
           , <<"option">>]
 %% CodeSearchSupport-list
         , <<"codesearchsupport">> => [
             <<"explicit">>
           , <<"all">>]
 %% TestReportStatus-list
         , <<"testreportstatus">> => [
             <<"completed">>
           , <<"in-progress">>
           , <<"waiting">>
           , <<"stopped">>
           , <<"entered-in-error">>]
 %% TestReportActionResult-list
         , <<"testreportactionresult">> => [
             <<"pass">>
           , <<"skip">>
           , <<"fail">>
           , <<"warning">>
           , <<"error">>]
 %% TestReportParticipantType-list
         , <<"testreportparticipanttype">> => [
             <<"test-engine">>
           , <<"client">>
           , <<"server">>]
 %% TestReportResult-list
         , <<"testreportresult">> => [
             <<"pass">>
           , <<"fail">>
           , <<"pending">>]
 %% AssertionDirectionType-list
         , <<"assertiondirectiontype">> => [
             <<"response">>
           , <<"request">>]
 %% AssertionOperatorType-list
         , <<"assertionoperatortype">> => [
             <<"equals">>
           , <<"notEquals">>
           , <<"in">>
           , <<"notIn">>
           , <<"greaterThan">>
           , <<"lessThan">>
           , <<"empty">>
           , <<"notEmpty">>
           , <<"contains">>
           , <<"notContains">>
           , <<"eval">>]
 %% AssertionResponseTypes-list
         , <<"assertionresponsetypes">> => [
             <<"okay">>
           , <<"created">>
           , <<"noContent">>
           , <<"notModified">>
           , <<"bad">>
           , <<"forbidden">>
           , <<"notFound">>
           , <<"methodNotAllowed">>
           , <<"conflict">>
           , <<"gone">>
           , <<"preconditionFailed">>
           , <<"unprocessable">>]
 %% TestScriptRequestMethodCode-list
         , <<"testscriptrequestmethodcode">> => [
             <<"delete">>
           , <<"get">>
           , <<"options">>
           , <<"patch">>
           , <<"post">>
           , <<"put">>
           , <<"head">>]
 %% status-list
         , <<"status">> => [
             <<"attested">>
           , <<"validated">>
           , <<"in-process">>
           , <<"req-revalid">>
           , <<"val-fail">>
           , <<"reval-fail">>]
 %% VisionBase-list
         , <<"visionbase">> => [
             <<"up">>
           , <<"down">>
           , <<"in">>
           , <<"out">>]
 %% VisionEyes-list
         , <<"visioneyes">> => [
             <<"right">>
           , <<"left">>]
        }).
%%
%% ResourceContainer
%%
-type resourcecontainer() :: 

            'Account'
           | 'ActivityDefinition'
           | 'AdverseEvent'
           | 'AllergyIntolerance'
           | 'Appointment'
           | 'AppointmentResponse'
           | 'AuditEvent'
           | 'Basic'
           | 'Binary'
           | 'BiologicallyDerivedProduct'
           | 'BodyStructure'
           | 'Bundle'
           | 'CapabilityStatement'
           | 'CarePlan'
           | 'CareTeam'
           | 'CatalogEntry'
           | 'ChargeItem'
           | 'ChargeItemDefinition'
           | 'Claim'
           | 'ClaimResponse'
           | 'ClinicalImpression'
           | 'CodeSystem'
           | 'Communication'
           | 'CommunicationRequest'
           | 'CompartmentDefinition'
           | 'Composition'
           | 'ConceptMap'
           | 'Condition'
           | 'Consent'
           | 'Contract'
           | 'Coverage'
           | 'CoverageEligibilityRequest'
           | 'CoverageEligibilityResponse'
           | 'DetectedIssue'
           | 'Device'
           | 'DeviceDefinition'
           | 'DeviceMetric'
           | 'DeviceRequest'
           | 'DeviceUseStatement'
           | 'DiagnosticReport'
           | 'DocumentManifest'
           | 'DocumentReference'
           | 'EffectEvidenceSynthesis'
           | 'Encounter'
           | 'Endpoint'
           | 'EnrollmentRequest'
           | 'EnrollmentResponse'
           | 'EpisodeOfCare'
           | 'EventDefinition'
           | 'Evidence'
           | 'EvidenceVariable'
           | 'ExampleScenario'
           | 'ExplanationOfBenefit'
           | 'FamilyMemberHistory'
           | 'Flag'
           | 'Goal'
           | 'GraphDefinition'
           | 'Group'
           | 'GuidanceResponse'
           | 'HealthcareService'
           | 'ImagingStudy'
           | 'Immunization'
           | 'ImmunizationEvaluation'
           | 'ImmunizationRecommendation'
           | 'ImplementationGuide'
           | 'InsurancePlan'
           | 'Invoice'
           | 'Library'
           | 'Linkage'
           | 'List'
           | 'Location'
           | 'Measure'
           | 'MeasureReport'
           | 'Media'
           | 'Medication'
           | 'MedicationAdministration'
           | 'MedicationDispense'
           | 'MedicationKnowledge'
           | 'MedicationRequest'
           | 'MedicationStatement'
           | 'MedicinalProduct'
           | 'MedicinalProductAuthorization'
           | 'MedicinalProductContraindication'
           | 'MedicinalProductIndication'
           | 'MedicinalProductIngredient'
           | 'MedicinalProductInteraction'
           | 'MedicinalProductManufactured'
           | 'MedicinalProductPackaged'
           | 'MedicinalProductPharmaceutical'
           | 'MedicinalProductUndesirableEffect'
           | 'MessageDefinition'
           | 'MessageHeader'
           | 'MolecularSequence'
           | 'NamingSystem'
           | 'NutritionOrder'
           | 'Observation'
           | 'ObservationDefinition'
           | 'OperationDefinition'
           | 'OperationOutcome'
           | 'Organization'
           | 'OrganizationAffiliation'
           | 'Patient'
           | 'PaymentNotice'
           | 'PaymentReconciliation'
           | 'Person'
           | 'PlanDefinition'
           | 'Practitioner'
           | 'PractitionerRole'
           | 'Procedure'
           | 'Provenance'
           | 'Questionnaire'
           | 'QuestionnaireResponse'
           | 'RelatedPerson'
           | 'RequestGroup'
           | 'ResearchDefinition'
           | 'ResearchElementDefinition'
           | 'ResearchStudy'
           | 'ResearchSubject'
           | 'RiskAssessment'
           | 'RiskEvidenceSynthesis'
           | 'Schedule'
           | 'SearchParameter'
           | 'ServiceRequest'
           | 'Slot'
           | 'Specimen'
           | 'SpecimenDefinition'
           | 'StructureDefinition'
           | 'StructureMap'
           | 'Subscription'
           | 'Substance'
           | 'SubstanceNucleicAcid'
           | 'SubstancePolymer'
           | 'SubstanceProtein'
           | 'SubstanceReferenceInformation'
           | 'SubstanceSourceMaterial'
           | 'SubstanceSpecification'
           | 'SupplyDelivery'
           | 'SupplyRequest'
           | 'Task'
           | 'TerminologyCapabilities'
           | 'TestReport'
           | 'TestScript'
           | 'ValueSet'
           | 'VerificationResult'
           | 'VisionPrescription'
           | 'Parameters'.

-endif.
