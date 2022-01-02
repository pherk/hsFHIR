-ifndef(fhir_xsd).
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
%%% complex types
%%%
-define(fhir_xsd, 
    #{
%%
%% Extension
%% Optional Extension Element - found in all resources.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
      <<"Extension">> => {<<"Element">>,
            [
            {<<"valueBase64Binary">>, {{primitive, <<"base64Binary">>}, optional}},
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"valueCanonical">>, {{primitive, <<"canonical">>}, optional}},
            {<<"valueCode">>, {{primitive, <<"code">>}, optional}},
            {<<"valueDate">>, {{primitive, <<"date">>}, optional}},
            {<<"valueDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"valueDecimal">>, {{primitive, <<"decimal">>}, optional}},
            {<<"valueId">>, {{primitive, <<"id">>}, optional}},
            {<<"valueInstant">>, {{primitive, <<"instant">>}, optional}},
            {<<"valueInteger">>, {{primitive, <<"integer">>}, optional}},
            {<<"valueMarkdown">>, {{primitive, <<"markdown">>}, optional}},
            {<<"valueOid">>, {{primitive, <<"oid">>}, optional}},
            {<<"valuePositiveInt">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"valueString">>, {{primitive, <<"string">>}, optional}},
            {<<"valueTime">>, {{primitive, <<"time">>}, optional}},
            {<<"valueUnsignedInt">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"valueUri">>, {{primitive, <<"uri">>}, optional}},
            {<<"valueUrl">>, {{primitive, <<"url">>}, optional}},
            {<<"valueUuid">>, {{primitive, <<"uuid">>}, optional}},
            {<<"valueAddress">>, {{complex, <<"Address">>}, optional}},
            {<<"valueAge">>, {{complex, <<"Age">>}, optional}},
            {<<"valueAnnotation">>, {{complex, <<"Annotation">>}, optional}},
            {<<"valueAttachment">>, {{complex, <<"Attachment">>}, optional}},
            {<<"valueCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"valueCoding">>, {{complex, <<"Coding">>}, optional}},
            {<<"valueContactPoint">>, {{complex, <<"ContactPoint">>}, optional}},
            {<<"valueCount">>, {{complex, <<"Count">>}, optional}},
            {<<"valueDistance">>, {{complex, <<"Distance">>}, optional}},
            {<<"valueDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"valueHumanName">>, {{complex, <<"HumanName">>}, optional}},
            {<<"valueIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"valueMoney">>, {{complex, <<"Money">>}, optional}},
            {<<"valuePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"valueRange">>, {{complex, <<"Range">>}, optional}},
            {<<"valueRatio">>, {{complex, <<"Ratio">>}, optional}},
            {<<"valueReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"valueSampledData">>, {{complex, <<"SampledData">>}, optional}},
            {<<"valueSignature">>, {{complex, <<"Signature">>}, optional}},
            {<<"valueTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"valueContactDetail">>, {{complex, <<"ContactDetail">>}, optional}},
            {<<"valueContributor">>, {{complex, <<"Contributor">>}, optional}},
            {<<"valueDataRequirement">>, {{complex, <<"DataRequirement">>}, optional}},
            {<<"valueExpression">>, {{complex, <<"Expression">>}, optional}},
            {<<"valueParameterDefinition">>, {{complex, <<"ParameterDefinition">>}, optional}},
            {<<"valueRelatedArtifact">>, {{complex, <<"RelatedArtifact">>}, optional}},
            {<<"valueTriggerDefinition">>, {{complex, <<"TriggerDefinition">>}, optional}},
            {<<"valueUsageContext">>, {{complex, <<"UsageContext">>}, optional}},
            {<<"valueDosage">>, {{bbelement, <<"Dosage">>}, optional}}
            ],
            [{<<"url">>, <<"uri">>}],
            [
            {<<"valueBase64Binary">>, <<"valueBoolean">>, <<"valueCanonical">>, <<"valueCode">>, <<"valueDate">>, <<"valueDateTime">>, <<"valueDecimal">>, <<"valueId">>, <<"valueInstant">>, <<"valueInteger">>, <<"valueMarkdown">>, <<"valueOid">>, <<"valuePositiveInt">>, <<"valueString">>, <<"valueTime">>, <<"valueUnsignedInt">>, <<"valueUri">>, <<"valueUrl">>, <<"valueUuid">>, <<"valueAddress">>, <<"valueAge">>, <<"valueAnnotation">>, <<"valueAttachment">>, <<"valueCodeableConcept">>, <<"valueCoding">>, <<"valueContactPoint">>, <<"valueCount">>, <<"valueDistance">>, <<"valueDuration">>, <<"valueHumanName">>, <<"valueIdentifier">>, <<"valueMoney">>, <<"valuePeriod">>, <<"valueQuantity">>, <<"valueRange">>, <<"valueRatio">>, <<"valueReference">>, <<"valueSampledData">>, <<"valueSignature">>, <<"valueTiming">>, <<"valueContactDetail">>, <<"valueContributor">>, <<"valueDataRequirement">>, <<"valueExpression">>, <<"valueParameterDefinition">>, <<"valueRelatedArtifact">>, <<"valueTriggerDefinition">>, <<"valueUsageContext">>, <<"valueDosage">>}
            ]
} 
%%
%% BackboneElement
%% Base definition for all elements that are defined inside a resource - but not those in a data type.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"BackboneElement">> => {<<"Element">>,
            [
            {<<"modifierExtension">>, {{complex, <<"Extension">>}, list}}
            ],
            [],
            []
} 
%%
%% Narrative
%% A human-readable summary of the resource conveying the essential clinical and business information for the resource.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Narrative">> => {<<"Element">>,
            [
            {<<"status">>, {{code, <<"narrativestatus_list">>}, required}},
            {<<"">>, {{complex, <<"">>}, required}}
            ],
            [],
            []
} 
%%
%% Element
%% Base definition for all elements in a resource.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Element">> => {<<"">>,
            [
            {<<"extension">>, {{complex, <<"Extension">>}, list}}
            ],
            [{<<"id">>, <<"string">>}],
            []
} 
%%
%% Meta
%% The metadata about a resource. This is content in the resource that is maintained by the infrastructure. Changes to the content might not always be associated with version changes to the resource.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Meta">> => {<<"Element">>,
            [
            {<<"versionId">>, {{primitive, <<"id">>}, optional}},
            {<<"lastUpdated">>, {{primitive, <<"instant">>}, optional}},
            {<<"source">>, {{primitive, <<"uri">>}, optional}},
            {<<"profile">>, {{primitive, <<"canonical">>}, list}},
            {<<"security">>, {{complex, <<"Coding">>}, list}},
            {<<"tag">>, {{complex, <<"Coding">>}, list}}
            ],
            [],
            []
} 
%%
%% Address
%% An address expressed using postal conventions (as opposed to GPS or other location definition formats).  This data type may be used to convey addresses for use in delivering mail as well as for visiting locations which might not be valid for mail delivery.  There are a variety of postal address formats defined around the world.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Address">> => {<<"Element">>,
            [
            {<<"use">>, {{code, <<"addressuse_list">>}, optional}},
            {<<"type">>, {{code, <<"addresstype_list">>}, optional}},
            {<<"text">>, {{primitive, <<"string">>}, optional}},
            {<<"line">>, {{primitive, <<"string">>}, list}},
            {<<"city">>, {{primitive, <<"string">>}, optional}},
            {<<"district">>, {{primitive, <<"string">>}, optional}},
            {<<"state">>, {{primitive, <<"string">>}, optional}},
            {<<"postalCode">>, {{primitive, <<"string">>}, optional}},
            {<<"country">>, {{primitive, <<"string">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            []
} 
%%
%% Contributor
%% A contributor to the content of a knowledge asset, including authors, editors, reviewers, and endorsers.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Contributor">> => {<<"Element">>,
            [
            {<<"type">>, {{code, <<"contributortype_list">>}, required}},
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}}
            ],
            [],
            []
} 
%%
%% Attachment
%% For referring to data content defined in other formats.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Attachment">> => {<<"Element">>,
            [
            {<<"contentType">>, {{primitive, <<"code">>}, optional}},
            {<<"language">>, {{primitive, <<"code">>}, optional}},
            {<<"data">>, {{primitive, <<"base64Binary">>}, optional}},
            {<<"url">>, {{primitive, <<"url">>}, optional}},
            {<<"size">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"hash">>, {{primitive, <<"base64Binary">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"creation">>, {{primitive, <<"dateTime">>}, optional}}
            ],
            [],
            []
} 
%%
%% Count
%% A measured amount (or an amount that can potentially be measured). Note that measured amounts include amounts that are not precisely quantified, including amounts involving arbitrary units and floating currencies.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Count">> => {<<"Quantity">>,
            [],
            [],
            []
} 
%%
%% DataRequirement
%% Describes a required data item for evaluation in terms of the type of data, and optional code or date-based filters of the data.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"DataRequirement">> => {<<"Element">>,
            [
            {<<"type">>, {{primitive, <<"code">>}, required}},
            {<<"profile">>, {{primitive, <<"canonical">>}, list}},
            {<<"subjectCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subjectReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"mustSupport">>, {{primitive, <<"string">>}, list}},
            {<<"codeFilter">>, {{complex, <<"DataRequirement.CodeFilter">>}, list}},
            {<<"dateFilter">>, {{complex, <<"DataRequirement.DateFilter">>}, list}},
            {<<"limit">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"sort">>, {{complex, <<"DataRequirement.Sort">>}, list}}
            ],
            [],
            [
            {<<"subjectCodeableConcept">>, <<"subjectReference">>}
            ]
} 
%%
%% DataRequirement.CodeFilter
%% Describes a required data item for evaluation in terms of the type of data, and optional code or date-based filters of the data.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"DataRequirement.CodeFilter">> => {<<"Element">>,
            [
            {<<"path">>, {{primitive, <<"string">>}, optional}},
            {<<"searchParam">>, {{primitive, <<"string">>}, optional}},
            {<<"valueSet">>, {{primitive, <<"canonical">>}, optional}},
            {<<"code">>, {{complex, <<"Coding">>}, list}}
            ],
            [],
            []
} 
%%
%% DataRequirement.DateFilter
%% Describes a required data item for evaluation in terms of the type of data, and optional code or date-based filters of the data.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"DataRequirement.DateFilter">> => {<<"Element">>,
            [
            {<<"path">>, {{primitive, <<"string">>}, optional}},
            {<<"searchParam">>, {{primitive, <<"string">>}, optional}},
            {<<"valueDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"valuePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"valueDuration">>, {{complex, <<"Duration">>}, optional}}
            ],
            [],
            [
            {<<"valueDateTime">>, <<"valuePeriod">>, <<"valueDuration">>}
            ]
} 
%%
%% DataRequirement.Sort
%% Describes a required data item for evaluation in terms of the type of data, and optional code or date-based filters of the data.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"DataRequirement.Sort">> => {<<"Element">>,
            [
            {<<"path">>, {{primitive, <<"string">>}, required}},
            {<<"direction">>, {{code, <<"sortdirection_list">>}, required}}
            ],
            [],
            []
} 
%%
%% Dosage
%% Indicates how the medication is/was taken or should be taken by the patient.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Dosage">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"integer">>}, optional}},
            {<<"text">>, {{primitive, <<"string">>}, optional}},
            {<<"additionalInstruction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"patientInstruction">>, {{primitive, <<"string">>}, optional}},
            {<<"timing">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"asNeededBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"asNeededCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"site">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"route">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"method">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"doseAndRate">>, {{bbelement, <<"Dosage.DoseAndRate">>}, list}},
            {<<"maxDosePerPeriod">>, {{complex, <<"Ratio">>}, optional}},
            {<<"maxDosePerAdministration">>, {{complex, <<"Quantity">>}, optional}},
            {<<"maxDosePerLifetime">>, {{complex, <<"Quantity">>}, optional}}
            ],
            [],
            [
            {<<"asNeededBoolean">>, <<"asNeededCodeableConcept">>}
            ]
} 
%%
%% Dosage.DoseAndRate
%% Indicates how the medication is/was taken or should be taken by the patient.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Dosage.DoseAndRate">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"doseRange">>, {{complex, <<"Range">>}, optional}},
            {<<"doseQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"rateRatio">>, {{complex, <<"Ratio">>}, optional}},
            {<<"rateRange">>, {{complex, <<"Range">>}, optional}},
            {<<"rateQuantity">>, {{complex, <<"Quantity">>}, optional}}
            ],
            [],
            [
            {<<"doseRange">>, <<"doseQuantity">>}, 
            {<<"rateRatio">>, <<"rateRange">>, <<"rateQuantity">>}
            ]
} 
%%
%% Money
%% An amount of economic utility in some recognized currency.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Money">> => {<<"Element">>,
            [
            {<<"value">>, {{primitive, <<"decimal">>}, optional}},
            {<<"currency">>, {{primitive, <<"code">>}, optional}}
            ],
            [],
            []
} 
%%
%% HumanName
%% A human's name with the ability to identify parts and usage.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"HumanName">> => {<<"Element">>,
            [
            {<<"use">>, {{code, <<"nameuse_list">>}, optional}},
            {<<"text">>, {{primitive, <<"string">>}, optional}},
            {<<"family">>, {{primitive, <<"string">>}, optional}},
            {<<"given">>, {{primitive, <<"string">>}, list}},
            {<<"prefix">>, {{primitive, <<"string">>}, list}},
            {<<"suffix">>, {{primitive, <<"string">>}, list}},
            {<<"period">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            []
} 
%%
%% ContactPoint
%% Details for all kinds of technology mediated contact points for a person or organization, including telephone, email, etc.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"ContactPoint">> => {<<"Element">>,
            [
            {<<"system">>, {{code, <<"contactpointsystem_list">>}, optional}},
            {<<"value">>, {{primitive, <<"string">>}, optional}},
            {<<"use">>, {{code, <<"contactpointuse_list">>}, optional}},
            {<<"rank">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            []
} 
%%
%% MarketingStatus
%% The marketing status describes the date when a medicinal product is actually put on the market or the date as of which it is no longer available.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"MarketingStatus">> => {<<"BackboneElement">>,
            [
            {<<"country">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"status">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"dateRange">>, {{complex, <<"Period">>}, required}},
            {<<"restoreDate">>, {{primitive, <<"dateTime">>}, optional}}
            ],
            [],
            []
} 
%%
%% Identifier
%% An identifier - identifies some entity uniquely and unambiguously. Typically this is used for business identifiers.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Identifier">> => {<<"Element">>,
            [
            {<<"use">>, {{code, <<"identifieruse_list">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"system">>, {{primitive, <<"uri">>}, optional}},
            {<<"value">>, {{primitive, <<"string">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"assigner">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstanceAmount
%% Chemical substances are a single substance type whose primary defining element is the molecular structure. Chemical substances shall be defined on the basis of their complete covalent molecular structure; the presence of a salt (counter-ion) and/or solvates (water, alcohols) is also captured. Purity, grade, physical form or particle size are not taken into account in the definition of a chemical substance or in the assignment of a Substance ID.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"SubstanceAmount">> => {<<"BackboneElement">>,
            [
            {<<"amountQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"amountRange">>, {{complex, <<"Range">>}, optional}},
            {<<"amountString">>, {{primitive, <<"string">>}, optional}},
            {<<"amountType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"amountText">>, {{primitive, <<"string">>}, optional}},
            {<<"referenceRange">>, {{bbelement, <<"SubstanceAmount.ReferenceRange">>}, optional}}
            ],
            [],
            [
            {<<"amountQuantity">>, <<"amountRange">>, <<"amountString">>}
            ]
} 
%%
%% SubstanceAmount.ReferenceRange
%% Chemical substances are a single substance type whose primary defining element is the molecular structure. Chemical substances shall be defined on the basis of their complete covalent molecular structure; the presence of a salt (counter-ion) and/or solvates (water, alcohols) is also captured. Purity, grade, physical form or particle size are not taken into account in the definition of a chemical substance or in the assignment of a Substance ID.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"SubstanceAmount.ReferenceRange">> => {<<"BackboneElement">>,
            [
            {<<"lowLimit">>, {{complex, <<"Quantity">>}, optional}},
            {<<"highLimit">>, {{complex, <<"Quantity">>}, optional}}
            ],
            [],
            []
} 
%%
%% Coding
%% A reference to a code defined by a terminology system.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Coding">> => {<<"Element">>,
            [
            {<<"system">>, {{primitive, <<"uri">>}, optional}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"code">>, {{primitive, <<"code">>}, optional}},
            {<<"display">>, {{primitive, <<"string">>}, optional}},
            {<<"userSelected">>, {{primitive, <<"boolean">>}, optional}}
            ],
            [],
            []
} 
%%
%% SampledData
%% A series of measurements taken by a device, with upper and lower limits. There may be more than one dimension in the data.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"SampledData">> => {<<"Element">>,
            [
            {<<"origin">>, {{complex, <<"Quantity">>}, required}},
            {<<"period">>, {{primitive, <<"decimal">>}, required}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"lowerLimit">>, {{primitive, <<"decimal">>}, optional}},
            {<<"upperLimit">>, {{primitive, <<"decimal">>}, optional}},
            {<<"dimensions">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"data">>, {{primitive, <<"SampledDataDataType">>}, optional}}
            ],
            [],
            []
} 
%%
%% Population
%% A populatioof people with some set of grouping criteria.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Population">> => {<<"BackboneElement">>,
            [
            {<<"ageRange">>, {{complex, <<"Range">>}, optional}},
            {<<"ageCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"gender">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"race">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"physiologicalCondition">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            [
            {<<"ageRange">>, <<"ageCodeableConcept">>}
            ]
} 
%%
%% Ratio
%% A relationship of two Quantity values - expressed as a numerator and a denominator.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Ratio">> => {<<"Element">>,
            [
            {<<"numerator">>, {{complex, <<"Quantity">>}, optional}},
            {<<"denominator">>, {{complex, <<"Quantity">>}, optional}}
            ],
            [],
            []
} 
%%
%% Distance
%% A length - a value with a unit that is a physical distance.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Distance">> => {<<"Quantity">>,
            [],
            [],
            []
} 
%%
%% Age
%% A duration of time during which an organism (or a process) has existed.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Age">> => {<<"Quantity">>,
            [],
            [],
            []
} 
%%
%% Reference
%% A reference from one resource to another.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Reference">> => {<<"Element">>,
            [
            {<<"reference">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"display">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% TriggerDefinition
%% A description of a triggering event. Triggering events can be named events, data events, or periodic, as determined by the type element.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"TriggerDefinition">> => {<<"Element">>,
            [
            {<<"type">>, {{code, <<"triggertype_list">>}, required}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"timingTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"timingReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"timingDate">>, {{primitive, <<"date">>}, optional}},
            {<<"timingDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"data">>, {{complex, <<"DataRequirement">>}, list}},
            {<<"condition">>, {{complex, <<"Expression">>}, optional}}
            ],
            [],
            [
            {<<"timingTiming">>, <<"timingReference">>, <<"timingDate">>, <<"timingDateTime">>}
            ]
} 
%%
%% Quantity
%% A measured amount (or an amount that can potentially be measured). Note that measured amounts include amounts that are not precisely quantified, including amounts involving arbitrary units and floating currencies.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Quantity">> => {<<"Element">>,
            [
            {<<"value">>, {{primitive, <<"decimal">>}, optional}},
            {<<"comparator">>, {{code, <<"quantitycomparator_list">>}, optional}},
            {<<"unit">>, {{primitive, <<"string">>}, optional}},
            {<<"system">>, {{primitive, <<"uri">>}, optional}},
            {<<"code">>, {{primitive, <<"code">>}, optional}}
            ],
            [],
            []
} 
%%
%% Period
%% A time period defined by a start and end date and optionally time.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Period">> => {<<"Element">>,
            [
            {<<"start">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"end">>, {{primitive, <<"dateTime">>}, optional}}
            ],
            [],
            []
} 
%%
%% Duration
%% A length of time.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Duration">> => {<<"Quantity">>,
            [],
            [],
            []
} 
%%
%% Range
%% A set of ordered Quantities defined by a low and high limit.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Range">> => {<<"Element">>,
            [
            {<<"low">>, {{complex, <<"Quantity">>}, optional}},
            {<<"high">>, {{complex, <<"Quantity">>}, optional}}
            ],
            [],
            []
} 
%%
%% RelatedArtifact
%% Related artifacts such as additional documentation, justification, or bibliographic references.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"RelatedArtifact">> => {<<"Element">>,
            [
            {<<"type">>, {{code, <<"relatedartifacttype_list">>}, required}},
            {<<"label">>, {{primitive, <<"string">>}, optional}},
            {<<"display">>, {{primitive, <<"string">>}, optional}},
            {<<"citation">>, {{primitive, <<"markdown">>}, optional}},
            {<<"url">>, {{primitive, <<"url">>}, optional}},
            {<<"document">>, {{complex, <<"Attachment">>}, optional}},
            {<<"resource">>, {{primitive, <<"canonical">>}, optional}}
            ],
            [],
            []
} 
%%
%% Annotation
%% A  text note which also  contains information about who made the statement and when.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Annotation">> => {<<"Element">>,
            [
            {<<"authorReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"authorString">>, {{primitive, <<"string">>}, optional}},
            {<<"time">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"text">>, {{primitive, <<"markdown">>}, required}}
            ],
            [],
            [
            {<<"authorReference">>, <<"authorString">>}
            ]
} 
%%
%% ProductShelfLife
%% The shelf-life and storage information for a medicinal product item or container can be described using this class.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"ProductShelfLife">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"period">>, {{complex, <<"Quantity">>}, required}},
            {<<"specialPrecautionsForStorage">>, {{complex, <<"CodeableConcept">>}, list}}
            ],
            [],
            []
} 
%%
%% ContactDetail
%% Specifies contact information for a person or organization.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"ContactDetail">> => {<<"Element">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"telecom">>, {{complex, <<"ContactPoint">>}, list}}
            ],
            [],
            []
} 
%%
%% UsageContext
%% Specifies clinical/business/etc. metadata that can be used to retrieve, index and/or categorize an artifact. This metadata can either be specific to the applicable population (e.g., age category, DRG) or the specific context of care (e.g., venue, care setting, provider of care).
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"UsageContext">> => {<<"Element">>,
            [
            {<<"code">>, {{complex, <<"Coding">>}, required}},
            {<<"valueCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, required}},
            {<<"valueRange">>, {{complex, <<"Range">>}, required}},
            {<<"valueReference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            [
            {<<"valueCodeableConcept">>, <<"valueQuantity">>, <<"valueRange">>, <<"valueReference">>}
            ]
} 
%%
%% Expression
%% A expression that is evaluated in a specified context and returns a value. The context of use of the expression must specify the context in which the expression is evaluated, and how the result of the expression is used.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Expression">> => {<<"Element">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"id">>}, optional}},
            {<<"language">>, {{code, <<"expressionlanguage_list">>}, required}},
            {<<"expression">>, {{primitive, <<"string">>}, optional}},
            {<<"reference">>, {{primitive, <<"uri">>}, optional}}
            ],
            [],
            []
} 
%%
%% Signature
%% A signature along with supporting context. The signature may be a digital signature that is cryptographic in nature, or some other signature acceptable to the domain. This other signature may be as simple as a graphical image representing a hand-written signature, or a signature ceremony Different signature approaches have different utilities.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Signature">> => {<<"Element">>,
            [
            {<<"type">>, {{complex, <<"Coding">>}, non_empty_list}},
            {<<"when">>, {{primitive, <<"instant">>}, required}},
            {<<"who">>, {{complex, <<"Reference">>}, required}},
            {<<"onBehalfOf">>, {{complex, <<"Reference">>}, optional}},
            {<<"targetFormat">>, {{primitive, <<"code">>}, optional}},
            {<<"sigFormat">>, {{primitive, <<"code">>}, optional}},
            {<<"data">>, {{primitive, <<"base64Binary">>}, optional}}
            ],
            [],
            []
} 
%%
%% Timing
%% Specifies an event that may occur multiple times. Timing schedules are used to record when things are planned, expected or requested to occur. The most common usage is in dosage instructions for medications. They are also used when planning care of various kinds, and may be used for reporting the schedule to which past regular activities were carried out.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Timing">> => {<<"BackboneElement">>,
            [
            {<<"event">>, {{primitive, <<"dateTime">>}, list}},
            {<<"repeat">>, {{bbelement, <<"Timing.Repeat">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% Timing.Repeat
%% Specifies an event that may occur multiple times. Timing schedules are used to record when things are planned, expected or requested to occur. The most common usage is in dosage instructions for medications. They are also used when planning care of various kinds, and may be used for reporting the schedule to which past regular activities were carried out.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"Timing.Repeat">> => {<<"BackboneElement">>,
            [
            {<<"boundsDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"boundsRange">>, {{complex, <<"Range">>}, optional}},
            {<<"boundsPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"count">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"countMax">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"duration">>, {{primitive, <<"decimal">>}, optional}},
            {<<"durationMax">>, {{primitive, <<"decimal">>}, optional}},
            {<<"durationUnit">>, {{code, <<"unitsoftime_list">>}, optional}},
            {<<"frequency">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"frequencyMax">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"period">>, {{primitive, <<"decimal">>}, optional}},
            {<<"periodMax">>, {{primitive, <<"decimal">>}, optional}},
            {<<"periodUnit">>, {{code, <<"unitsoftime_list">>}, optional}},
            {<<"dayOfWeek">>, {{primitive, <<"code">>}, list}},
            {<<"timeOfDay">>, {{primitive, <<"time">>}, list}},
            {<<"when">>, {{code, <<"eventtiming_list">>}, list}},
            {<<"offset">>, {{primitive, <<"unsignedInt">>}, optional}}
            ],
            [],
            [
            {<<"boundsDuration">>, <<"boundsRange">>, <<"boundsPeriod">>}
            ]
} 
%%
%% ProdCharacteristic
%% The marketing status describes the date when a medicinal product is actually put on the market or the date as of which it is no longer available.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"ProdCharacteristic">> => {<<"BackboneElement">>,
            [
            {<<"height">>, {{complex, <<"Quantity">>}, optional}},
            {<<"width">>, {{complex, <<"Quantity">>}, optional}},
            {<<"depth">>, {{complex, <<"Quantity">>}, optional}},
            {<<"weight">>, {{complex, <<"Quantity">>}, optional}},
            {<<"nominalVolume">>, {{complex, <<"Quantity">>}, optional}},
            {<<"externalDiameter">>, {{complex, <<"Quantity">>}, optional}},
            {<<"shape">>, {{primitive, <<"string">>}, optional}},
            {<<"color">>, {{primitive, <<"string">>}, list}},
            {<<"imprint">>, {{primitive, <<"string">>}, list}},
            {<<"image">>, {{complex, <<"Attachment">>}, list}},
            {<<"scoring">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% CodeableConcept
%% A concept that may be defined by a formal reference to a terminology or ontology or may be provided by text.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"CodeableConcept">> => {<<"Element">>,
            [
            {<<"coding">>, {{complex, <<"Coding">>}, list}},
            {<<"text">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% ParameterDefinition
%% The parameters to the module. This collection specifies both the input and output parameters. Input parameters are provided by the caller as part of the $evaluate operation. Output parameters are included in the GuidanceResponse.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"ParameterDefinition">> => {<<"Element">>,
            [
            {<<"name">>, {{primitive, <<"code">>}, optional}},
            {<<"use">>, {{primitive, <<"code">>}, required}},
            {<<"min">>, {{primitive, <<"integer">>}, optional}},
            {<<"max">>, {{primitive, <<"string">>}, optional}},
            {<<"documentation">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{primitive, <<"code">>}, required}},
            {<<"profile">>, {{primitive, <<"canonical">>}, optional}}
            ],
            [],
            []
} 
%%
%% ElementDefinition
%% Captures constraints on each element within the resource, profile, or extension.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"ElementDefinition">> => {<<"BackboneElement">>,
            [
            {<<"path">>, {{primitive, <<"string">>}, required}},
            {<<"representation">>, {{code, <<"propertyrepresentation_list">>}, list}},
            {<<"sliceName">>, {{primitive, <<"string">>}, optional}},
            {<<"sliceIsConstraining">>, {{primitive, <<"boolean">>}, optional}},
            {<<"label">>, {{primitive, <<"string">>}, optional}},
            {<<"code">>, {{complex, <<"Coding">>}, list}},
            {<<"slicing">>, {{bbelement, <<"ElementDefinition.Slicing">>}, optional}},
            {<<"short">>, {{primitive, <<"string">>}, optional}},
            {<<"definition">>, {{primitive, <<"markdown">>}, optional}},
            {<<"comment">>, {{primitive, <<"markdown">>}, optional}},
            {<<"requirements">>, {{primitive, <<"markdown">>}, optional}},
            {<<"alias">>, {{primitive, <<"string">>}, list}},
            {<<"min">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"max">>, {{primitive, <<"string">>}, optional}},
            {<<"base">>, {{bbelement, <<"ElementDefinition.Base">>}, optional}},
            {<<"contentReference">>, {{primitive, <<"uri">>}, optional}},
            {<<"type">>, {{bbelement, <<"ElementDefinition.Type">>}, list}},
            {<<"defaultValueBase64Binary">>, {{primitive, <<"base64Binary">>}, optional}},
            {<<"defaultValueBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"defaultValueCanonical">>, {{primitive, <<"canonical">>}, optional}},
            {<<"defaultValueCode">>, {{primitive, <<"code">>}, optional}},
            {<<"defaultValueDate">>, {{primitive, <<"date">>}, optional}},
            {<<"defaultValueDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"defaultValueDecimal">>, {{primitive, <<"decimal">>}, optional}},
            {<<"defaultValueId">>, {{primitive, <<"id">>}, optional}},
            {<<"defaultValueInstant">>, {{primitive, <<"instant">>}, optional}},
            {<<"defaultValueInteger">>, {{primitive, <<"integer">>}, optional}},
            {<<"defaultValueMarkdown">>, {{primitive, <<"markdown">>}, optional}},
            {<<"defaultValueOid">>, {{primitive, <<"oid">>}, optional}},
            {<<"defaultValuePositiveInt">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"defaultValueString">>, {{primitive, <<"string">>}, optional}},
            {<<"defaultValueTime">>, {{primitive, <<"time">>}, optional}},
            {<<"defaultValueUnsignedInt">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"defaultValueUri">>, {{primitive, <<"uri">>}, optional}},
            {<<"defaultValueUrl">>, {{primitive, <<"url">>}, optional}},
            {<<"defaultValueUuid">>, {{primitive, <<"uuid">>}, optional}},
            {<<"defaultValueAddress">>, {{complex, <<"Address">>}, optional}},
            {<<"defaultValueAge">>, {{complex, <<"Age">>}, optional}},
            {<<"defaultValueAnnotation">>, {{complex, <<"Annotation">>}, optional}},
            {<<"defaultValueAttachment">>, {{complex, <<"Attachment">>}, optional}},
            {<<"defaultValueCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"defaultValueCoding">>, {{complex, <<"Coding">>}, optional}},
            {<<"defaultValueContactPoint">>, {{complex, <<"ContactPoint">>}, optional}},
            {<<"defaultValueCount">>, {{complex, <<"Count">>}, optional}},
            {<<"defaultValueDistance">>, {{complex, <<"Distance">>}, optional}},
            {<<"defaultValueDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"defaultValueHumanName">>, {{complex, <<"HumanName">>}, optional}},
            {<<"defaultValueIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"defaultValueMoney">>, {{complex, <<"Money">>}, optional}},
            {<<"defaultValuePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"defaultValueQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"defaultValueRange">>, {{complex, <<"Range">>}, optional}},
            {<<"defaultValueRatio">>, {{complex, <<"Ratio">>}, optional}},
            {<<"defaultValueReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"defaultValueSampledData">>, {{complex, <<"SampledData">>}, optional}},
            {<<"defaultValueSignature">>, {{complex, <<"Signature">>}, optional}},
            {<<"defaultValueTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"defaultValueContactDetail">>, {{complex, <<"ContactDetail">>}, optional}},
            {<<"defaultValueContributor">>, {{complex, <<"Contributor">>}, optional}},
            {<<"defaultValueDataRequirement">>, {{complex, <<"DataRequirement">>}, optional}},
            {<<"defaultValueExpression">>, {{complex, <<"Expression">>}, optional}},
            {<<"defaultValueParameterDefinition">>, {{complex, <<"ParameterDefinition">>}, optional}},
            {<<"defaultValueRelatedArtifact">>, {{complex, <<"RelatedArtifact">>}, optional}},
            {<<"defaultValueTriggerDefinition">>, {{complex, <<"TriggerDefinition">>}, optional}},
            {<<"defaultValueUsageContext">>, {{complex, <<"UsageContext">>}, optional}},
            {<<"defaultValueDosage">>, {{bbelement, <<"Dosage">>}, optional}},
            {<<"meaningWhenMissing">>, {{primitive, <<"markdown">>}, optional}},
            {<<"orderMeaning">>, {{primitive, <<"string">>}, optional}},
            {<<"fixedBase64Binary">>, {{primitive, <<"base64Binary">>}, optional}},
            {<<"fixedBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"fixedCanonical">>, {{primitive, <<"canonical">>}, optional}},
            {<<"fixedCode">>, {{primitive, <<"code">>}, optional}},
            {<<"fixedDate">>, {{primitive, <<"date">>}, optional}},
            {<<"fixedDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"fixedDecimal">>, {{primitive, <<"decimal">>}, optional}},
            {<<"fixedId">>, {{primitive, <<"id">>}, optional}},
            {<<"fixedInstant">>, {{primitive, <<"instant">>}, optional}},
            {<<"fixedInteger">>, {{primitive, <<"integer">>}, optional}},
            {<<"fixedMarkdown">>, {{primitive, <<"markdown">>}, optional}},
            {<<"fixedOid">>, {{primitive, <<"oid">>}, optional}},
            {<<"fixedPositiveInt">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"fixedString">>, {{primitive, <<"string">>}, optional}},
            {<<"fixedTime">>, {{primitive, <<"time">>}, optional}},
            {<<"fixedUnsignedInt">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"fixedUri">>, {{primitive, <<"uri">>}, optional}},
            {<<"fixedUrl">>, {{primitive, <<"url">>}, optional}},
            {<<"fixedUuid">>, {{primitive, <<"uuid">>}, optional}},
            {<<"fixedAddress">>, {{complex, <<"Address">>}, optional}},
            {<<"fixedAge">>, {{complex, <<"Age">>}, optional}},
            {<<"fixedAnnotation">>, {{complex, <<"Annotation">>}, optional}},
            {<<"fixedAttachment">>, {{complex, <<"Attachment">>}, optional}},
            {<<"fixedCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"fixedCoding">>, {{complex, <<"Coding">>}, optional}},
            {<<"fixedContactPoint">>, {{complex, <<"ContactPoint">>}, optional}},
            {<<"fixedCount">>, {{complex, <<"Count">>}, optional}},
            {<<"fixedDistance">>, {{complex, <<"Distance">>}, optional}},
            {<<"fixedDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"fixedHumanName">>, {{complex, <<"HumanName">>}, optional}},
            {<<"fixedIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"fixedMoney">>, {{complex, <<"Money">>}, optional}},
            {<<"fixedPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"fixedQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"fixedRange">>, {{complex, <<"Range">>}, optional}},
            {<<"fixedRatio">>, {{complex, <<"Ratio">>}, optional}},
            {<<"fixedReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"fixedSampledData">>, {{complex, <<"SampledData">>}, optional}},
            {<<"fixedSignature">>, {{complex, <<"Signature">>}, optional}},
            {<<"fixedTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"fixedContactDetail">>, {{complex, <<"ContactDetail">>}, optional}},
            {<<"fixedContributor">>, {{complex, <<"Contributor">>}, optional}},
            {<<"fixedDataRequirement">>, {{complex, <<"DataRequirement">>}, optional}},
            {<<"fixedExpression">>, {{complex, <<"Expression">>}, optional}},
            {<<"fixedParameterDefinition">>, {{complex, <<"ParameterDefinition">>}, optional}},
            {<<"fixedRelatedArtifact">>, {{complex, <<"RelatedArtifact">>}, optional}},
            {<<"fixedTriggerDefinition">>, {{complex, <<"TriggerDefinition">>}, optional}},
            {<<"fixedUsageContext">>, {{complex, <<"UsageContext">>}, optional}},
            {<<"fixedDosage">>, {{bbelement, <<"Dosage">>}, optional}},
            {<<"patternBase64Binary">>, {{primitive, <<"base64Binary">>}, optional}},
            {<<"patternBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"patternCanonical">>, {{primitive, <<"canonical">>}, optional}},
            {<<"patternCode">>, {{primitive, <<"code">>}, optional}},
            {<<"patternDate">>, {{primitive, <<"date">>}, optional}},
            {<<"patternDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"patternDecimal">>, {{primitive, <<"decimal">>}, optional}},
            {<<"patternId">>, {{primitive, <<"id">>}, optional}},
            {<<"patternInstant">>, {{primitive, <<"instant">>}, optional}},
            {<<"patternInteger">>, {{primitive, <<"integer">>}, optional}},
            {<<"patternMarkdown">>, {{primitive, <<"markdown">>}, optional}},
            {<<"patternOid">>, {{primitive, <<"oid">>}, optional}},
            {<<"patternPositiveInt">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"patternString">>, {{primitive, <<"string">>}, optional}},
            {<<"patternTime">>, {{primitive, <<"time">>}, optional}},
            {<<"patternUnsignedInt">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"patternUri">>, {{primitive, <<"uri">>}, optional}},
            {<<"patternUrl">>, {{primitive, <<"url">>}, optional}},
            {<<"patternUuid">>, {{primitive, <<"uuid">>}, optional}},
            {<<"patternAddress">>, {{complex, <<"Address">>}, optional}},
            {<<"patternAge">>, {{complex, <<"Age">>}, optional}},
            {<<"patternAnnotation">>, {{complex, <<"Annotation">>}, optional}},
            {<<"patternAttachment">>, {{complex, <<"Attachment">>}, optional}},
            {<<"patternCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"patternCoding">>, {{complex, <<"Coding">>}, optional}},
            {<<"patternContactPoint">>, {{complex, <<"ContactPoint">>}, optional}},
            {<<"patternCount">>, {{complex, <<"Count">>}, optional}},
            {<<"patternDistance">>, {{complex, <<"Distance">>}, optional}},
            {<<"patternDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"patternHumanName">>, {{complex, <<"HumanName">>}, optional}},
            {<<"patternIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"patternMoney">>, {{complex, <<"Money">>}, optional}},
            {<<"patternPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"patternQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"patternRange">>, {{complex, <<"Range">>}, optional}},
            {<<"patternRatio">>, {{complex, <<"Ratio">>}, optional}},
            {<<"patternReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"patternSampledData">>, {{complex, <<"SampledData">>}, optional}},
            {<<"patternSignature">>, {{complex, <<"Signature">>}, optional}},
            {<<"patternTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"patternContactDetail">>, {{complex, <<"ContactDetail">>}, optional}},
            {<<"patternContributor">>, {{complex, <<"Contributor">>}, optional}},
            {<<"patternDataRequirement">>, {{complex, <<"DataRequirement">>}, optional}},
            {<<"patternExpression">>, {{complex, <<"Expression">>}, optional}},
            {<<"patternParameterDefinition">>, {{complex, <<"ParameterDefinition">>}, optional}},
            {<<"patternRelatedArtifact">>, {{complex, <<"RelatedArtifact">>}, optional}},
            {<<"patternTriggerDefinition">>, {{complex, <<"TriggerDefinition">>}, optional}},
            {<<"patternUsageContext">>, {{complex, <<"UsageContext">>}, optional}},
            {<<"patternDosage">>, {{bbelement, <<"Dosage">>}, optional}},
            {<<"example">>, {{bbelement, <<"ElementDefinition.Example">>}, list}},
            {<<"minValueDate">>, {{primitive, <<"date">>}, optional}},
            {<<"minValueDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"minValueInstant">>, {{primitive, <<"instant">>}, optional}},
            {<<"minValueTime">>, {{primitive, <<"time">>}, optional}},
            {<<"minValueDecimal">>, {{primitive, <<"decimal">>}, optional}},
            {<<"minValueInteger">>, {{primitive, <<"integer">>}, optional}},
            {<<"minValuePositiveInt">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"minValueUnsignedInt">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"minValueQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"maxValueDate">>, {{primitive, <<"date">>}, optional}},
            {<<"maxValueDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"maxValueInstant">>, {{primitive, <<"instant">>}, optional}},
            {<<"maxValueTime">>, {{primitive, <<"time">>}, optional}},
            {<<"maxValueDecimal">>, {{primitive, <<"decimal">>}, optional}},
            {<<"maxValueInteger">>, {{primitive, <<"integer">>}, optional}},
            {<<"maxValuePositiveInt">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"maxValueUnsignedInt">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"maxValueQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"maxLength">>, {{primitive, <<"integer">>}, optional}},
            {<<"condition">>, {{primitive, <<"id">>}, list}},
            {<<"constraint">>, {{bbelement, <<"ElementDefinition.Constraint">>}, list}},
            {<<"mustSupport">>, {{primitive, <<"boolean">>}, optional}},
            {<<"isModifier">>, {{primitive, <<"boolean">>}, optional}},
            {<<"isModifierReason">>, {{primitive, <<"string">>}, optional}},
            {<<"isSummary">>, {{primitive, <<"boolean">>}, optional}},
            {<<"binding">>, {{bbelement, <<"ElementDefinition.Binding">>}, optional}},
            {<<"mapping">>, {{bbelement, <<"ElementDefinition.Mapping">>}, list}}
            ],
            [],
            [
            {<<"defaultValueBase64Binary">>, <<"defaultValueBoolean">>, <<"defaultValueCanonical">>, <<"defaultValueCode">>, <<"defaultValueDate">>, <<"defaultValueDateTime">>, <<"defaultValueDecimal">>, <<"defaultValueId">>, <<"defaultValueInstant">>, <<"defaultValueInteger">>, <<"defaultValueMarkdown">>, <<"defaultValueOid">>, <<"defaultValuePositiveInt">>, <<"defaultValueString">>, <<"defaultValueTime">>, <<"defaultValueUnsignedInt">>, <<"defaultValueUri">>, <<"defaultValueUrl">>, <<"defaultValueUuid">>, <<"defaultValueAddress">>, <<"defaultValueAge">>, <<"defaultValueAnnotation">>, <<"defaultValueAttachment">>, <<"defaultValueCodeableConcept">>, <<"defaultValueCoding">>, <<"defaultValueContactPoint">>, <<"defaultValueCount">>, <<"defaultValueDistance">>, <<"defaultValueDuration">>, <<"defaultValueHumanName">>, <<"defaultValueIdentifier">>, <<"defaultValueMoney">>, <<"defaultValuePeriod">>, <<"defaultValueQuantity">>, <<"defaultValueRange">>, <<"defaultValueRatio">>, <<"defaultValueReference">>, <<"defaultValueSampledData">>, <<"defaultValueSignature">>, <<"defaultValueTiming">>, <<"defaultValueContactDetail">>, <<"defaultValueContributor">>, <<"defaultValueDataRequirement">>, <<"defaultValueExpression">>, <<"defaultValueParameterDefinition">>, <<"defaultValueRelatedArtifact">>, <<"defaultValueTriggerDefinition">>, <<"defaultValueUsageContext">>, <<"defaultValueDosage">>}, 
            {<<"fixedBase64Binary">>, <<"fixedBoolean">>, <<"fixedCanonical">>, <<"fixedCode">>, <<"fixedDate">>, <<"fixedDateTime">>, <<"fixedDecimal">>, <<"fixedId">>, <<"fixedInstant">>, <<"fixedInteger">>, <<"fixedMarkdown">>, <<"fixedOid">>, <<"fixedPositiveInt">>, <<"fixedString">>, <<"fixedTime">>, <<"fixedUnsignedInt">>, <<"fixedUri">>, <<"fixedUrl">>, <<"fixedUuid">>, <<"fixedAddress">>, <<"fixedAge">>, <<"fixedAnnotation">>, <<"fixedAttachment">>, <<"fixedCodeableConcept">>, <<"fixedCoding">>, <<"fixedContactPoint">>, <<"fixedCount">>, <<"fixedDistance">>, <<"fixedDuration">>, <<"fixedHumanName">>, <<"fixedIdentifier">>, <<"fixedMoney">>, <<"fixedPeriod">>, <<"fixedQuantity">>, <<"fixedRange">>, <<"fixedRatio">>, <<"fixedReference">>, <<"fixedSampledData">>, <<"fixedSignature">>, <<"fixedTiming">>, <<"fixedContactDetail">>, <<"fixedContributor">>, <<"fixedDataRequirement">>, <<"fixedExpression">>, <<"fixedParameterDefinition">>, <<"fixedRelatedArtifact">>, <<"fixedTriggerDefinition">>, <<"fixedUsageContext">>, <<"fixedDosage">>}, 
            {<<"patternBase64Binary">>, <<"patternBoolean">>, <<"patternCanonical">>, <<"patternCode">>, <<"patternDate">>, <<"patternDateTime">>, <<"patternDecimal">>, <<"patternId">>, <<"patternInstant">>, <<"patternInteger">>, <<"patternMarkdown">>, <<"patternOid">>, <<"patternPositiveInt">>, <<"patternString">>, <<"patternTime">>, <<"patternUnsignedInt">>, <<"patternUri">>, <<"patternUrl">>, <<"patternUuid">>, <<"patternAddress">>, <<"patternAge">>, <<"patternAnnotation">>, <<"patternAttachment">>, <<"patternCodeableConcept">>, <<"patternCoding">>, <<"patternContactPoint">>, <<"patternCount">>, <<"patternDistance">>, <<"patternDuration">>, <<"patternHumanName">>, <<"patternIdentifier">>, <<"patternMoney">>, <<"patternPeriod">>, <<"patternQuantity">>, <<"patternRange">>, <<"patternRatio">>, <<"patternReference">>, <<"patternSampledData">>, <<"patternSignature">>, <<"patternTiming">>, <<"patternContactDetail">>, <<"patternContributor">>, <<"patternDataRequirement">>, <<"patternExpression">>, <<"patternParameterDefinition">>, <<"patternRelatedArtifact">>, <<"patternTriggerDefinition">>, <<"patternUsageContext">>, <<"patternDosage">>}, 
            {<<"minValueDate">>, <<"minValueDateTime">>, <<"minValueInstant">>, <<"minValueTime">>, <<"minValueDecimal">>, <<"minValueInteger">>, <<"minValuePositiveInt">>, <<"minValueUnsignedInt">>, <<"minValueQuantity">>}, 
            {<<"maxValueDate">>, <<"maxValueDateTime">>, <<"maxValueInstant">>, <<"maxValueTime">>, <<"maxValueDecimal">>, <<"maxValueInteger">>, <<"maxValuePositiveInt">>, <<"maxValueUnsignedInt">>, <<"maxValueQuantity">>}
            ]
} 
%%
%% ElementDefinition.Constraint
%% Captures constraints on each element within the resource, profile, or extension.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"ElementDefinition.Constraint">> => {<<"BackboneElement">>,
            [
            {<<"key">>, {{primitive, <<"id">>}, required}},
            {<<"requirements">>, {{primitive, <<"string">>}, optional}},
            {<<"severity">>, {{code, <<"constraintseverity_list">>}, required}},
            {<<"human">>, {{primitive, <<"string">>}, required}},
            {<<"expression">>, {{primitive, <<"string">>}, optional}},
            {<<"xpath">>, {{primitive, <<"string">>}, optional}},
            {<<"source">>, {{primitive, <<"canonical">>}, optional}}
            ],
            [],
            []
} 
%%
%% ElementDefinition.Mapping
%% Captures constraints on each element within the resource, profile, or extension.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"ElementDefinition.Mapping">> => {<<"BackboneElement">>,
            [
            {<<"identity">>, {{primitive, <<"id">>}, required}},
            {<<"language">>, {{primitive, <<"code">>}, optional}},
            {<<"map">>, {{primitive, <<"string">>}, required}},
            {<<"comment">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% ElementDefinition.Base
%% Captures constraints on each element within the resource, profile, or extension.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"ElementDefinition.Base">> => {<<"BackboneElement">>,
            [
            {<<"path">>, {{primitive, <<"string">>}, required}},
            {<<"min">>, {{primitive, <<"unsignedInt">>}, required}},
            {<<"max">>, {{primitive, <<"string">>}, required}}
            ],
            [],
            []
} 
%%
%% ElementDefinition.Type
%% Captures constraints on each element within the resource, profile, or extension.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"ElementDefinition.Type">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{primitive, <<"uri">>}, required}},
            {<<"profile">>, {{primitive, <<"canonical">>}, list}},
            {<<"targetProfile">>, {{primitive, <<"canonical">>}, list}},
            {<<"aggregation">>, {{code, <<"aggregationmode_list">>}, list}},
            {<<"versioning">>, {{code, <<"referenceversionrules_list">>}, optional}}
            ],
            [],
            []
} 
%%
%% ElementDefinition.Example
%% Captures constraints on each element within the resource, profile, or extension.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"ElementDefinition.Example">> => {<<"BackboneElement">>,
            [
            {<<"label">>, {{primitive, <<"string">>}, required}},
            {<<"valueBase64Binary">>, {{primitive, <<"base64Binary">>}, required}},
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, required}},
            {<<"valueCanonical">>, {{primitive, <<"canonical">>}, required}},
            {<<"valueCode">>, {{primitive, <<"code">>}, required}},
            {<<"valueDate">>, {{primitive, <<"date">>}, required}},
            {<<"valueDateTime">>, {{primitive, <<"dateTime">>}, required}},
            {<<"valueDecimal">>, {{primitive, <<"decimal">>}, required}},
            {<<"valueId">>, {{primitive, <<"id">>}, required}},
            {<<"valueInstant">>, {{primitive, <<"instant">>}, required}},
            {<<"valueInteger">>, {{primitive, <<"integer">>}, required}},
            {<<"valueMarkdown">>, {{primitive, <<"markdown">>}, required}},
            {<<"valueOid">>, {{primitive, <<"oid">>}, required}},
            {<<"valuePositiveInt">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"valueString">>, {{primitive, <<"string">>}, required}},
            {<<"valueTime">>, {{primitive, <<"time">>}, required}},
            {<<"valueUnsignedInt">>, {{primitive, <<"unsignedInt">>}, required}},
            {<<"valueUri">>, {{primitive, <<"uri">>}, required}},
            {<<"valueUrl">>, {{primitive, <<"url">>}, required}},
            {<<"valueUuid">>, {{primitive, <<"uuid">>}, required}},
            {<<"valueAddress">>, {{complex, <<"Address">>}, required}},
            {<<"valueAge">>, {{complex, <<"Age">>}, required}},
            {<<"valueAnnotation">>, {{complex, <<"Annotation">>}, required}},
            {<<"valueAttachment">>, {{complex, <<"Attachment">>}, required}},
            {<<"valueCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"valueCoding">>, {{complex, <<"Coding">>}, required}},
            {<<"valueContactPoint">>, {{complex, <<"ContactPoint">>}, required}},
            {<<"valueCount">>, {{complex, <<"Count">>}, required}},
            {<<"valueDistance">>, {{complex, <<"Distance">>}, required}},
            {<<"valueDuration">>, {{complex, <<"Duration">>}, required}},
            {<<"valueHumanName">>, {{complex, <<"HumanName">>}, required}},
            {<<"valueIdentifier">>, {{complex, <<"Identifier">>}, required}},
            {<<"valueMoney">>, {{complex, <<"Money">>}, required}},
            {<<"valuePeriod">>, {{complex, <<"Period">>}, required}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, required}},
            {<<"valueRange">>, {{complex, <<"Range">>}, required}},
            {<<"valueRatio">>, {{complex, <<"Ratio">>}, required}},
            {<<"valueReference">>, {{complex, <<"Reference">>}, required}},
            {<<"valueSampledData">>, {{complex, <<"SampledData">>}, required}},
            {<<"valueSignature">>, {{complex, <<"Signature">>}, required}},
            {<<"valueTiming">>, {{bbelement, <<"Timing">>}, required}},
            {<<"valueContactDetail">>, {{complex, <<"ContactDetail">>}, required}},
            {<<"valueContributor">>, {{complex, <<"Contributor">>}, required}},
            {<<"valueDataRequirement">>, {{complex, <<"DataRequirement">>}, required}},
            {<<"valueExpression">>, {{complex, <<"Expression">>}, required}},
            {<<"valueParameterDefinition">>, {{complex, <<"ParameterDefinition">>}, required}},
            {<<"valueRelatedArtifact">>, {{complex, <<"RelatedArtifact">>}, required}},
            {<<"valueTriggerDefinition">>, {{complex, <<"TriggerDefinition">>}, required}},
            {<<"valueUsageContext">>, {{complex, <<"UsageContext">>}, required}},
            {<<"valueDosage">>, {{bbelement, <<"Dosage">>}, required}}
            ],
            [],
            [
            {<<"valueBase64Binary">>, <<"valueBoolean">>, <<"valueCanonical">>, <<"valueCode">>, <<"valueDate">>, <<"valueDateTime">>, <<"valueDecimal">>, <<"valueId">>, <<"valueInstant">>, <<"valueInteger">>, <<"valueMarkdown">>, <<"valueOid">>, <<"valuePositiveInt">>, <<"valueString">>, <<"valueTime">>, <<"valueUnsignedInt">>, <<"valueUri">>, <<"valueUrl">>, <<"valueUuid">>, <<"valueAddress">>, <<"valueAge">>, <<"valueAnnotation">>, <<"valueAttachment">>, <<"valueCodeableConcept">>, <<"valueCoding">>, <<"valueContactPoint">>, <<"valueCount">>, <<"valueDistance">>, <<"valueDuration">>, <<"valueHumanName">>, <<"valueIdentifier">>, <<"valueMoney">>, <<"valuePeriod">>, <<"valueQuantity">>, <<"valueRange">>, <<"valueRatio">>, <<"valueReference">>, <<"valueSampledData">>, <<"valueSignature">>, <<"valueTiming">>, <<"valueContactDetail">>, <<"valueContributor">>, <<"valueDataRequirement">>, <<"valueExpression">>, <<"valueParameterDefinition">>, <<"valueRelatedArtifact">>, <<"valueTriggerDefinition">>, <<"valueUsageContext">>, <<"valueDosage">>}
            ]
} 
%%
%% ElementDefinition.Slicing
%% Captures constraints on each element within the resource, profile, or extension.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"ElementDefinition.Slicing">> => {<<"BackboneElement">>,
            [
            {<<"discriminator">>, {{bbelement, <<"ElementDefinition.Discriminator">>}, list}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"ordered">>, {{primitive, <<"boolean">>}, optional}},
            {<<"rules">>, {{code, <<"slicingrules_list">>}, required}}
            ],
            [],
            []
} 
%%
%% ElementDefinition.Binding
%% Captures constraints on each element within the resource, profile, or extension.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"ElementDefinition.Binding">> => {<<"BackboneElement">>,
            [
            {<<"strength">>, {{code, <<"bindingstrength_list">>}, required}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"valueSet">>, {{primitive, <<"canonical">>}, optional}}
            ],
            [],
            []
} 
%%
%% ElementDefinition.Discriminator
%% Captures constraints on each element within the resource, profile, or extension.
%% If the element is present, it must have a value for at least one of the defined elements, an @id referenced from the Narrative, or extensions
%%
    , <<"ElementDefinition.Discriminator">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{code, <<"discriminatortype_list">>}, required}},
            {<<"path">>, {{primitive, <<"string">>}, required}}
            ],
            [],
            []
} 
%%
%% DomainResource
%% A resource that includes narrative, extensions, and contained resources.
%%
    , <<"DomainResource">> => {<<"Resource">>,
            [
            {<<"text">>, {{special, <<"Narrative">>}, optional}},
            {<<"contained">>, {{complex, <<"ResourceContainer">>}, list}},
            {<<"extension">>, {{complex, <<"Extension">>}, list}},
            {<<"modifierExtension">>, {{complex, <<"Extension">>}, list}}
            ],
            [],
            []
} 
%%
%% Resource
%% This is the base resource type for everything.
%%
    , <<"Resource">> => {<<"">>,
            [
            {<<"id">>, {{primitive, <<"id">>}, optional}},
            {<<"meta">>, {{complex, <<"Meta">>}, optional}},
            {<<"implicitRules">>, {{primitive, <<"uri">>}, optional}},
            {<<"language">>, {{primitive, <<"code">>}, optional}}
            ],
            [],
            []
} 
%%
%% Account
%% A financial tool for tracking value accrued for a particular purpose.  In the healthcare field, used to track charges for a patient, cost centers, etc.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Account">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"accountstatus_list">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, list}},
            {<<"servicePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"coverage">>, {{bbelement, <<"Account.Coverage">>}, list}},
            {<<"owner">>, {{complex, <<"Reference">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"guarantor">>, {{bbelement, <<"Account.Guarantor">>}, list}},
            {<<"partOf">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% Account.Coverage
%% A financial tool for tracking value accrued for a particular purpose.  In the healthcare field, used to track charges for a patient, cost centers, etc.
%%
    , <<"Account.Coverage">> => {<<"BackboneElement">>,
            [
            {<<"coverage">>, {{complex, <<"Reference">>}, required}},
            {<<"priority">>, {{primitive, <<"positiveInt">>}, optional}}
            ],
            [],
            []
} 
%%
%% Account.Guarantor
%% A financial tool for tracking value accrued for a particular purpose.  In the healthcare field, used to track charges for a patient, cost centers, etc.
%%
    , <<"Account.Guarantor">> => {<<"BackboneElement">>,
            [
            {<<"party">>, {{complex, <<"Reference">>}, required}},
            {<<"onHold">>, {{primitive, <<"boolean">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            []
} 
%%
%% ActivityDefinition
%% This resource allows for the definition of some activity to be performed, independent of a particular patient, practitioner, or other performance context.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ActivityDefinition">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"subtitle">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"subjectCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subjectReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"usage">>, {{primitive, <<"string">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"approvalDate">>, {{primitive, <<"date">>}, optional}},
            {<<"lastReviewDate">>, {{primitive, <<"date">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"topic">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"author">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"editor">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"reviewer">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"endorser">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"relatedArtifact">>, {{complex, <<"RelatedArtifact">>}, list}},
            {<<"library">>, {{primitive, <<"canonical">>}, list}},
            {<<"kind">>, {{code, <<"requestresourcetype_list">>}, optional}},
            {<<"profile">>, {{primitive, <<"canonical">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"intent">>, {{code, <<"requestintent_list">>}, optional}},
            {<<"priority">>, {{code, <<"requestpriority_list">>}, optional}},
            {<<"doNotPerform">>, {{primitive, <<"boolean">>}, optional}},
            {<<"timingTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"timingDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"timingAge">>, {{complex, <<"Age">>}, optional}},
            {<<"timingPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"timingRange">>, {{complex, <<"Range">>}, optional}},
            {<<"timingDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"location">>, {{complex, <<"Reference">>}, optional}},
            {<<"participant">>, {{bbelement, <<"ActivityDefinition.Participant">>}, list}},
            {<<"productReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"productCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"dosage">>, {{bbelement, <<"Dosage">>}, list}},
            {<<"bodySite">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"specimenRequirement">>, {{complex, <<"Reference">>}, list}},
            {<<"observationRequirement">>, {{complex, <<"Reference">>}, list}},
            {<<"observationResultRequirement">>, {{complex, <<"Reference">>}, list}},
            {<<"transform">>, {{primitive, <<"canonical">>}, optional}},
            {<<"dynamicValue">>, {{bbelement, <<"ActivityDefinition.DynamicValue">>}, list}}
            ],
            [],
            [
            {<<"subjectCodeableConcept">>, <<"subjectReference">>}, 
            {<<"timingTiming">>, <<"timingDateTime">>, <<"timingAge">>, <<"timingPeriod">>, <<"timingRange">>, <<"timingDuration">>}, 
            {<<"productReference">>, <<"productCodeableConcept">>}
            ]
} 
%%
%% ActivityDefinition.Participant
%% This resource allows for the definition of some activity to be performed, independent of a particular patient, practitioner, or other performance context.
%%
    , <<"ActivityDefinition.Participant">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{code, <<"actionparticipanttype_list">>}, required}},
            {<<"role">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% ActivityDefinition.DynamicValue
%% This resource allows for the definition of some activity to be performed, independent of a particular patient, practitioner, or other performance context.
%%
    , <<"ActivityDefinition.DynamicValue">> => {<<"BackboneElement">>,
            [
            {<<"path">>, {{primitive, <<"string">>}, required}},
            {<<"expression">>, {{complex, <<"Expression">>}, required}}
            ],
            [],
            []
} 
%%
%% AdverseEvent
%% Actual or  potential/avoided event causing unintended physical injury resulting from or contributed to by medical care, a research study or other healthcare setting factors that requires additional monitoring, treatment, or hospitalization, or that results in death.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"AdverseEvent">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"actuality">>, {{code, <<"adverseeventactuality_list">>}, required}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"event">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"detected">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"recordedDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"resultingCondition">>, {{complex, <<"Reference">>}, list}},
            {<<"location">>, {{complex, <<"Reference">>}, optional}},
            {<<"seriousness">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"severity">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"outcome">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"recorder">>, {{complex, <<"Reference">>}, optional}},
            {<<"contributor">>, {{complex, <<"Reference">>}, list}},
            {<<"suspectEntity">>, {{bbelement, <<"AdverseEvent.SuspectEntity">>}, list}},
            {<<"subjectMedicalHistory">>, {{complex, <<"Reference">>}, list}},
            {<<"referenceDocument">>, {{complex, <<"Reference">>}, list}},
            {<<"study">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% AdverseEvent.SuspectEntity
%% Actual or  potential/avoided event causing unintended physical injury resulting from or contributed to by medical care, a research study or other healthcare setting factors that requires additional monitoring, treatment, or hospitalization, or that results in death.
%%
    , <<"AdverseEvent.SuspectEntity">> => {<<"BackboneElement">>,
            [
            {<<"instance">>, {{complex, <<"Reference">>}, required}},
            {<<"causality">>, {{bbelement, <<"AdverseEvent.Causality">>}, list}}
            ],
            [],
            []
} 
%%
%% AdverseEvent.Causality
%% Actual or  potential/avoided event causing unintended physical injury resulting from or contributed to by medical care, a research study or other healthcare setting factors that requires additional monitoring, treatment, or hospitalization, or that results in death.
%%
    , <<"AdverseEvent.Causality">> => {<<"BackboneElement">>,
            [
            {<<"assessment">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"productRelatedness">>, {{primitive, <<"string">>}, optional}},
            {<<"author">>, {{complex, <<"Reference">>}, optional}},
            {<<"method">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% AllergyIntolerance
%% Risk of harmful or undesirable, physiological response which is unique to an individual and associated with exposure to a substance.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"AllergyIntolerance">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"clinicalStatus">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"verificationStatus">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"type">>, {{code, <<"allergyintolerancetype_list">>}, optional}},
            {<<"category">>, {{code, <<"allergyintolerancecategory_list">>}, list}},
            {<<"criticality">>, {{code, <<"allergyintolerancecriticality_list">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"patient">>, {{complex, <<"Reference">>}, required}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"onsetDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"onsetAge">>, {{complex, <<"Age">>}, optional}},
            {<<"onsetPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"onsetRange">>, {{complex, <<"Range">>}, optional}},
            {<<"onsetString">>, {{primitive, <<"string">>}, optional}},
            {<<"recordedDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"recorder">>, {{complex, <<"Reference">>}, optional}},
            {<<"asserter">>, {{complex, <<"Reference">>}, optional}},
            {<<"lastOccurrence">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"reaction">>, {{bbelement, <<"AllergyIntolerance.Reaction">>}, list}}
            ],
            [],
            [
            {<<"onsetDateTime">>, <<"onsetAge">>, <<"onsetPeriod">>, <<"onsetRange">>, <<"onsetString">>}
            ]
} 
%%
%% AllergyIntolerance.Reaction
%% Risk of harmful or undesirable, physiological response which is unique to an individual and associated with exposure to a substance.
%%
    , <<"AllergyIntolerance.Reaction">> => {<<"BackboneElement">>,
            [
            {<<"substance">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"manifestation">>, {{complex, <<"CodeableConcept">>}, non_empty_list}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"onset">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"severity">>, {{code, <<"allergyintoleranceseverity_list">>}, optional}},
            {<<"exposureRoute">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            []
} 
%%
%% Appointment
%% A booking of a healthcare event among patient(s), practitioner(s), related person(s) and/or device(s) for a specific date/time. This may result in one or more Encounter(s).
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Appointment">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"appointmentstatus_list">>}, required}},
            {<<"cancelationReason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"serviceCategory">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"serviceType">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"specialty">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"appointmentType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"priority">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"supportingInformation">>, {{complex, <<"Reference">>}, list}},
            {<<"start">>, {{primitive, <<"instant">>}, optional}},
            {<<"end">>, {{primitive, <<"instant">>}, optional}},
            {<<"minutesDuration">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"slot">>, {{complex, <<"Reference">>}, list}},
            {<<"created">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"comment">>, {{primitive, <<"string">>}, optional}},
            {<<"patientInstruction">>, {{primitive, <<"string">>}, optional}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"participant">>, {{bbelement, <<"Appointment.Participant">>}, non_empty_list}},
            {<<"requestedPeriod">>, {{complex, <<"Period">>}, list}}
            ],
            [],
            []
} 
%%
%% Appointment.Participant
%% A booking of a healthcare event among patient(s), practitioner(s), related person(s) and/or device(s) for a specific date/time. This may result in one or more Encounter(s).
%%
    , <<"Appointment.Participant">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"actor">>, {{complex, <<"Reference">>}, optional}},
            {<<"required">>, {{code, <<"participantrequired_list">>}, optional}},
            {<<"status">>, {{code, <<"participationstatus_list">>}, required}},
            {<<"period">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            []
} 
%%
%% AppointmentResponse
%% A reply to an appointment request for a patient and/or practitioner(s), such as a confirmation or rejection.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"AppointmentResponse">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"appointment">>, {{complex, <<"Reference">>}, required}},
            {<<"start">>, {{primitive, <<"instant">>}, optional}},
            {<<"end">>, {{primitive, <<"instant">>}, optional}},
            {<<"participantType">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"actor">>, {{complex, <<"Reference">>}, optional}},
            {<<"participantStatus">>, {{code, <<"participationstatus_list">>}, required}},
            {<<"comment">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% AuditEvent
%% A record of an event made for purposes of maintaining a security log. Typical uses include detection of intrusion attempts and monitoring for inappropriate usage.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"AuditEvent">> => {<<"DomainResource">>,
            [
            {<<"type">>, {{complex, <<"Coding">>}, required}},
            {<<"subtype">>, {{complex, <<"Coding">>}, list}},
            {<<"action">>, {{code, <<"auditeventaction_list">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"recorded">>, {{primitive, <<"instant">>}, required}},
            {<<"outcome">>, {{code, <<"auditeventoutcome_list">>}, optional}},
            {<<"outcomeDesc">>, {{primitive, <<"string">>}, optional}},
            {<<"purposeOfEvent">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"agent">>, {{bbelement, <<"AuditEvent.Agent">>}, non_empty_list}},
            {<<"source">>, {{bbelement, <<"AuditEvent.Source">>}, required}},
            {<<"entity">>, {{bbelement, <<"AuditEvent.Entity">>}, list}}
            ],
            [],
            []
} 
%%
%% AuditEvent.Agent
%% A record of an event made for purposes of maintaining a security log. Typical uses include detection of intrusion attempts and monitoring for inappropriate usage.
%%
    , <<"AuditEvent.Agent">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"role">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"who">>, {{complex, <<"Reference">>}, optional}},
            {<<"altId">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"requestor">>, {{primitive, <<"boolean">>}, required}},
            {<<"location">>, {{complex, <<"Reference">>}, optional}},
            {<<"policy">>, {{primitive, <<"uri">>}, list}},
            {<<"media">>, {{complex, <<"Coding">>}, optional}},
            {<<"network">>, {{bbelement, <<"AuditEvent.Network">>}, optional}},
            {<<"purposeOfUse">>, {{complex, <<"CodeableConcept">>}, list}}
            ],
            [],
            []
} 
%%
%% AuditEvent.Network
%% A record of an event made for purposes of maintaining a security log. Typical uses include detection of intrusion attempts and monitoring for inappropriate usage.
%%
    , <<"AuditEvent.Network">> => {<<"BackboneElement">>,
            [
            {<<"address">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{code, <<"auditeventagentnetworktype_list">>}, optional}}
            ],
            [],
            []
} 
%%
%% AuditEvent.Source
%% A record of an event made for purposes of maintaining a security log. Typical uses include detection of intrusion attempts and monitoring for inappropriate usage.
%%
    , <<"AuditEvent.Source">> => {<<"BackboneElement">>,
            [
            {<<"site">>, {{primitive, <<"string">>}, optional}},
            {<<"observer">>, {{complex, <<"Reference">>}, required}},
            {<<"type">>, {{complex, <<"Coding">>}, list}}
            ],
            [],
            []
} 
%%
%% AuditEvent.Entity
%% A record of an event made for purposes of maintaining a security log. Typical uses include detection of intrusion attempts and monitoring for inappropriate usage.
%%
    , <<"AuditEvent.Entity">> => {<<"BackboneElement">>,
            [
            {<<"what">>, {{complex, <<"Reference">>}, optional}},
            {<<"type">>, {{complex, <<"Coding">>}, optional}},
            {<<"role">>, {{complex, <<"Coding">>}, optional}},
            {<<"lifecycle">>, {{complex, <<"Coding">>}, optional}},
            {<<"securityLabel">>, {{complex, <<"Coding">>}, list}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"query">>, {{primitive, <<"base64Binary">>}, optional}},
            {<<"detail">>, {{bbelement, <<"AuditEvent.Detail">>}, list}}
            ],
            [],
            []
} 
%%
%% AuditEvent.Detail
%% A record of an event made for purposes of maintaining a security log. Typical uses include detection of intrusion attempts and monitoring for inappropriate usage.
%%
    , <<"AuditEvent.Detail">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{primitive, <<"string">>}, required}},
            {<<"valueString">>, {{primitive, <<"string">>}, required}},
            {<<"valueBase64Binary">>, {{primitive, <<"base64Binary">>}, required}}
            ],
            [],
            [
            {<<"valueString">>, <<"valueBase64Binary">>}
            ]
} 
%%
%% Basic
%% Basic is used for handling concepts not yet defined in FHIR, narrative-only resources that don't map to an existing resource, and custom resources not appropriate for inclusion in the FHIR specification.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Basic">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"created">>, {{primitive, <<"date">>}, optional}},
            {<<"author">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% Binary
%% A resource that represents the data of a single raw artifact as digital content accessible in its native format.  A Binary resource can contain any content, whether text, image, pdf, zip archive, etc.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Binary">> => {<<"Resource">>,
            [
            {<<"contentType">>, {{primitive, <<"code">>}, required}},
            {<<"securityContext">>, {{complex, <<"Reference">>}, optional}},
            {<<"data">>, {{primitive, <<"base64Binary">>}, optional}}
            ],
            [],
            []
} 
%%
%% BiologicallyDerivedProduct
%% A material substance originating from a biological entity intended to be transplanted or infused
%% into another (possibly the same) biological entity.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"BiologicallyDerivedProduct">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"productCategory">>, {{code, <<"biologicallyderivedproductcategory_list">>}, optional}},
            {<<"productCode">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"status">>, {{code, <<"biologicallyderivedproductstatus_list">>}, optional}},
            {<<"request">>, {{complex, <<"Reference">>}, list}},
            {<<"quantity">>, {{primitive, <<"integer">>}, optional}},
            {<<"parent">>, {{complex, <<"Reference">>}, list}},
            {<<"collection">>, {{bbelement, <<"BiologicallyDerivedProduct.Collection">>}, optional}},
            {<<"processing">>, {{bbelement, <<"BiologicallyDerivedProduct.Processing">>}, list}},
            {<<"manipulation">>, {{bbelement, <<"BiologicallyDerivedProduct.Manipulation">>}, optional}},
            {<<"storage">>, {{bbelement, <<"BiologicallyDerivedProduct.Storage">>}, list}}
            ],
            [],
            []
} 
%%
%% BiologicallyDerivedProduct.Collection
%% A material substance originating from a biological entity intended to be transplanted or infused
%% into another (possibly the same) biological entity.
%%
    , <<"BiologicallyDerivedProduct.Collection">> => {<<"BackboneElement">>,
            [
            {<<"collector">>, {{complex, <<"Reference">>}, optional}},
            {<<"source">>, {{complex, <<"Reference">>}, optional}},
            {<<"collectedDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"collectedPeriod">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            [
            {<<"collectedDateTime">>, <<"collectedPeriod">>}
            ]
} 
%%
%% BiologicallyDerivedProduct.Processing
%% A material substance originating from a biological entity intended to be transplanted or infused
%% into another (possibly the same) biological entity.
%%
    , <<"BiologicallyDerivedProduct.Processing">> => {<<"BackboneElement">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"procedure">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"additive">>, {{complex, <<"Reference">>}, optional}},
            {<<"timeDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"timePeriod">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            [
            {<<"timeDateTime">>, <<"timePeriod">>}
            ]
} 
%%
%% BiologicallyDerivedProduct.Manipulation
%% A material substance originating from a biological entity intended to be transplanted or infused
%% into another (possibly the same) biological entity.
%%
    , <<"BiologicallyDerivedProduct.Manipulation">> => {<<"BackboneElement">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"timeDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"timePeriod">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            [
            {<<"timeDateTime">>, <<"timePeriod">>}
            ]
} 
%%
%% BiologicallyDerivedProduct.Storage
%% A material substance originating from a biological entity intended to be transplanted or infused
%% into another (possibly the same) biological entity.
%%
    , <<"BiologicallyDerivedProduct.Storage">> => {<<"BackboneElement">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"temperature">>, {{primitive, <<"decimal">>}, optional}},
            {<<"scale">>, {{code, <<"biologicallyderivedproductstoragescale_list">>}, optional}},
            {<<"duration">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            []
} 
%%
%% BodyStructure
%% Record details about an anatomical structure.  This resource may be used when a coded concept does not provide the necessary detail needed for the use case.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"BodyStructure">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"active">>, {{primitive, <<"boolean">>}, optional}},
            {<<"morphology">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"location">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"locationQualifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"image">>, {{complex, <<"Attachment">>}, list}},
            {<<"patient">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% Bundle
%% A container for a collection of resources.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Bundle">> => {<<"Resource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"type">>, {{code, <<"bundletype_list">>}, required}},
            {<<"timestamp">>, {{primitive, <<"instant">>}, optional}},
            {<<"total">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"link">>, {{bbelement, <<"Bundle.Link">>}, list}},
            {<<"entry">>, {{bbelement, <<"Bundle.Entry">>}, list}},
            {<<"signature">>, {{complex, <<"Signature">>}, optional}}
            ],
            [],
            []
} 
%%
%% Bundle.Link
%% A container for a collection of resources.
%%
    , <<"Bundle.Link">> => {<<"BackboneElement">>,
            [
            {<<"relation">>, {{primitive, <<"string">>}, required}},
            {<<"url">>, {{primitive, <<"uri">>}, required}}
            ],
            [],
            []
} 
%%
%% Bundle.Entry
%% A container for a collection of resources.
%%
    , <<"Bundle.Entry">> => {<<"BackboneElement">>,
            [
            {<<"link">>, {{bbelement, <<"Bundle.Link">>}, list}},
            {<<"fullUrl">>, {{primitive, <<"uri">>}, optional}},
            {<<"resource">>, {{complex, <<"ResourceContainer">>}, optional}},
            {<<"search">>, {{bbelement, <<"Bundle.Search">>}, optional}},
            {<<"request">>, {{bbelement, <<"Bundle.Request">>}, optional}},
            {<<"response">>, {{bbelement, <<"Bundle.Response">>}, optional}}
            ],
            [],
            []
} 
%%
%% Bundle.Search
%% A container for a collection of resources.
%%
    , <<"Bundle.Search">> => {<<"BackboneElement">>,
            [
            {<<"mode">>, {{code, <<"searchentrymode_list">>}, optional}},
            {<<"score">>, {{primitive, <<"decimal">>}, optional}}
            ],
            [],
            []
} 
%%
%% Bundle.Request
%% A container for a collection of resources.
%%
    , <<"Bundle.Request">> => {<<"BackboneElement">>,
            [
            {<<"method">>, {{code, <<"httpverb_list">>}, required}},
            {<<"url">>, {{primitive, <<"uri">>}, required}},
            {<<"ifNoneMatch">>, {{primitive, <<"string">>}, optional}},
            {<<"ifModifiedSince">>, {{primitive, <<"instant">>}, optional}},
            {<<"ifMatch">>, {{primitive, <<"string">>}, optional}},
            {<<"ifNoneExist">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% Bundle.Response
%% A container for a collection of resources.
%%
    , <<"Bundle.Response">> => {<<"BackboneElement">>,
            [
            {<<"status">>, {{primitive, <<"string">>}, required}},
            {<<"location">>, {{primitive, <<"uri">>}, optional}},
            {<<"etag">>, {{primitive, <<"string">>}, optional}},
            {<<"lastModified">>, {{primitive, <<"instant">>}, optional}},
            {<<"outcome">>, {{complex, <<"ResourceContainer">>}, optional}}
            ],
            [],
            []
} 
%%
%% CapabilityStatement
%% A Capability Statement documents a set of capabilities (behaviors) of a FHIR Server for a particular version of FHIR that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"CapabilityStatement">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, required}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"kind">>, {{code, <<"capabilitystatementkind_list">>}, required}},
            {<<"instantiates">>, {{primitive, <<"canonical">>}, list}},
            {<<"imports">>, {{primitive, <<"canonical">>}, list}},
            {<<"software">>, {{bbelement, <<"CapabilityStatement.Software">>}, optional}},
            {<<"implementation">>, {{bbelement, <<"CapabilityStatement.Implementation">>}, optional}},
            {<<"fhirVersion">>, {{code, <<"fhirversion_list">>}, required}},
            {<<"format">>, {{primitive, <<"code">>}, non_empty_list}},
            {<<"patchFormat">>, {{primitive, <<"code">>}, list}},
            {<<"implementationGuide">>, {{primitive, <<"canonical">>}, list}},
            {<<"rest">>, {{bbelement, <<"CapabilityStatement.Rest">>}, list}},
            {<<"messaging">>, {{bbelement, <<"CapabilityStatement.Messaging">>}, list}},
            {<<"document">>, {{bbelement, <<"CapabilityStatement.Document">>}, list}}
            ],
            [],
            []
} 
%%
%% CapabilityStatement.Software
%% A Capability Statement documents a set of capabilities (behaviors) of a FHIR Server for a particular version of FHIR that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"CapabilityStatement.Software">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"releaseDate">>, {{primitive, <<"dateTime">>}, optional}}
            ],
            [],
            []
} 
%%
%% CapabilityStatement.Implementation
%% A Capability Statement documents a set of capabilities (behaviors) of a FHIR Server for a particular version of FHIR that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"CapabilityStatement.Implementation">> => {<<"BackboneElement">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, required}},
            {<<"url">>, {{primitive, <<"url">>}, optional}},
            {<<"custodian">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% CapabilityStatement.Rest
%% A Capability Statement documents a set of capabilities (behaviors) of a FHIR Server for a particular version of FHIR that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"CapabilityStatement.Rest">> => {<<"BackboneElement">>,
            [
            {<<"mode">>, {{code, <<"restfulcapabilitymode_list">>}, required}},
            {<<"documentation">>, {{primitive, <<"markdown">>}, optional}},
            {<<"security">>, {{bbelement, <<"CapabilityStatement.Security">>}, optional}},
            {<<"resource">>, {{bbelement, <<"CapabilityStatement.Resource">>}, list}},
            {<<"interaction">>, {{bbelement, <<"CapabilityStatement.Interaction1">>}, list}},
            {<<"searchParam">>, {{bbelement, <<"CapabilityStatement.SearchParam">>}, list}},
            {<<"operation">>, {{bbelement, <<"CapabilityStatement.Operation">>}, list}},
            {<<"compartment">>, {{primitive, <<"canonical">>}, list}}
            ],
            [],
            []
} 
%%
%% CapabilityStatement.Security
%% A Capability Statement documents a set of capabilities (behaviors) of a FHIR Server for a particular version of FHIR that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"CapabilityStatement.Security">> => {<<"BackboneElement">>,
            [
            {<<"cors">>, {{primitive, <<"boolean">>}, optional}},
            {<<"service">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}}
            ],
            [],
            []
} 
%%
%% CapabilityStatement.Resource
%% A Capability Statement documents a set of capabilities (behaviors) of a FHIR Server for a particular version of FHIR that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"CapabilityStatement.Resource">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{primitive, <<"code">>}, required}},
            {<<"profile">>, {{primitive, <<"canonical">>}, optional}},
            {<<"supportedProfile">>, {{primitive, <<"canonical">>}, list}},
            {<<"documentation">>, {{primitive, <<"markdown">>}, optional}},
            {<<"interaction">>, {{bbelement, <<"CapabilityStatement.Interaction">>}, list}},
            {<<"versioning">>, {{code, <<"resourceversionpolicy_list">>}, optional}},
            {<<"readHistory">>, {{primitive, <<"boolean">>}, optional}},
            {<<"updateCreate">>, {{primitive, <<"boolean">>}, optional}},
            {<<"conditionalCreate">>, {{primitive, <<"boolean">>}, optional}},
            {<<"conditionalRead">>, {{code, <<"conditionalreadstatus_list">>}, optional}},
            {<<"conditionalUpdate">>, {{primitive, <<"boolean">>}, optional}},
            {<<"conditionalDelete">>, {{code, <<"conditionaldeletestatus_list">>}, optional}},
            {<<"referencePolicy">>, {{code, <<"referencehandlingpolicy_list">>}, list}},
            {<<"searchInclude">>, {{primitive, <<"string">>}, list}},
            {<<"searchRevInclude">>, {{primitive, <<"string">>}, list}},
            {<<"searchParam">>, {{bbelement, <<"CapabilityStatement.SearchParam">>}, list}},
            {<<"operation">>, {{bbelement, <<"CapabilityStatement.Operation">>}, list}}
            ],
            [],
            []
} 
%%
%% CapabilityStatement.Interaction
%% A Capability Statement documents a set of capabilities (behaviors) of a FHIR Server for a particular version of FHIR that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"CapabilityStatement.Interaction">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{code, <<"typerestfulinteraction_list">>}, required}},
            {<<"documentation">>, {{primitive, <<"markdown">>}, optional}}
            ],
            [],
            []
} 
%%
%% CapabilityStatement.SearchParam
%% A Capability Statement documents a set of capabilities (behaviors) of a FHIR Server for a particular version of FHIR that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"CapabilityStatement.SearchParam">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"definition">>, {{primitive, <<"canonical">>}, optional}},
            {<<"type">>, {{code, <<"searchparamtype_list">>}, required}},
            {<<"documentation">>, {{primitive, <<"markdown">>}, optional}}
            ],
            [],
            []
} 
%%
%% CapabilityStatement.Operation
%% A Capability Statement documents a set of capabilities (behaviors) of a FHIR Server for a particular version of FHIR that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"CapabilityStatement.Operation">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"definition">>, {{primitive, <<"canonical">>}, required}},
            {<<"documentation">>, {{primitive, <<"markdown">>}, optional}}
            ],
            [],
            []
} 
%%
%% CapabilityStatement.Interaction1
%% A Capability Statement documents a set of capabilities (behaviors) of a FHIR Server for a particular version of FHIR that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"CapabilityStatement.Interaction1">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{code, <<"systemrestfulinteraction_list">>}, required}},
            {<<"documentation">>, {{primitive, <<"markdown">>}, optional}}
            ],
            [],
            []
} 
%%
%% CapabilityStatement.Messaging
%% A Capability Statement documents a set of capabilities (behaviors) of a FHIR Server for a particular version of FHIR that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"CapabilityStatement.Messaging">> => {<<"BackboneElement">>,
            [
            {<<"endpoint">>, {{bbelement, <<"CapabilityStatement.Endpoint">>}, list}},
            {<<"reliableCache">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"documentation">>, {{primitive, <<"markdown">>}, optional}},
            {<<"supportedMessage">>, {{bbelement, <<"CapabilityStatement.SupportedMessage">>}, list}}
            ],
            [],
            []
} 
%%
%% CapabilityStatement.Endpoint
%% A Capability Statement documents a set of capabilities (behaviors) of a FHIR Server for a particular version of FHIR that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"CapabilityStatement.Endpoint">> => {<<"BackboneElement">>,
            [
            {<<"protocol">>, {{complex, <<"Coding">>}, required}},
            {<<"address">>, {{primitive, <<"url">>}, required}}
            ],
            [],
            []
} 
%%
%% CapabilityStatement.SupportedMessage
%% A Capability Statement documents a set of capabilities (behaviors) of a FHIR Server for a particular version of FHIR that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"CapabilityStatement.SupportedMessage">> => {<<"BackboneElement">>,
            [
            {<<"mode">>, {{code, <<"eventcapabilitymode_list">>}, required}},
            {<<"definition">>, {{primitive, <<"canonical">>}, required}}
            ],
            [],
            []
} 
%%
%% CapabilityStatement.Document
%% A Capability Statement documents a set of capabilities (behaviors) of a FHIR Server for a particular version of FHIR that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"CapabilityStatement.Document">> => {<<"BackboneElement">>,
            [
            {<<"mode">>, {{code, <<"documentmode_list">>}, required}},
            {<<"documentation">>, {{primitive, <<"markdown">>}, optional}},
            {<<"profile">>, {{primitive, <<"canonical">>}, required}}
            ],
            [],
            []
} 
%%
%% CarePlan
%% Describes the intention of how one or more practitioners intend to deliver care for a particular patient, group or community for a period of time, possibly limited to care for a specific condition or set of conditions.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"CarePlan">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"instantiatesCanonical">>, {{primitive, <<"canonical">>}, list}},
            {<<"instantiatesUri">>, {{primitive, <<"uri">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"replaces">>, {{complex, <<"Reference">>}, list}},
            {<<"partOf">>, {{complex, <<"Reference">>}, list}},
            {<<"status">>, {{code, <<"requeststatus_list">>}, required}},
            {<<"intent">>, {{code, <<"careplanintent_list">>}, required}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"created">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"author">>, {{complex, <<"Reference">>}, optional}},
            {<<"contributor">>, {{complex, <<"Reference">>}, list}},
            {<<"careTeam">>, {{complex, <<"Reference">>}, list}},
            {<<"addresses">>, {{complex, <<"Reference">>}, list}},
            {<<"supportingInfo">>, {{complex, <<"Reference">>}, list}},
            {<<"goal">>, {{complex, <<"Reference">>}, list}},
            {<<"activity">>, {{bbelement, <<"CarePlan.Activity">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            []
} 
%%
%% CarePlan.Activity
%% Describes the intention of how one or more practitioners intend to deliver care for a particular patient, group or community for a period of time, possibly limited to care for a specific condition or set of conditions.
%%
    , <<"CarePlan.Activity">> => {<<"BackboneElement">>,
            [
            {<<"outcomeCodeableConcept">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"outcomeReference">>, {{complex, <<"Reference">>}, list}},
            {<<"progress">>, {{complex, <<"Annotation">>}, list}},
            {<<"reference">>, {{complex, <<"Reference">>}, optional}},
            {<<"detail">>, {{bbelement, <<"CarePlan.Detail">>}, optional}}
            ],
            [],
            []
} 
%%
%% CarePlan.Detail
%% Describes the intention of how one or more practitioners intend to deliver care for a particular patient, group or community for a period of time, possibly limited to care for a specific condition or set of conditions.
%%
    , <<"CarePlan.Detail">> => {<<"BackboneElement">>,
            [
            {<<"kind">>, {{code, <<"careplanactivitykind_list">>}, optional}},
            {<<"instantiatesCanonical">>, {{primitive, <<"canonical">>}, list}},
            {<<"instantiatesUri">>, {{primitive, <<"uri">>}, list}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"goal">>, {{complex, <<"Reference">>}, list}},
            {<<"status">>, {{code, <<"careplanactivitystatus_list">>}, required}},
            {<<"statusReason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"doNotPerform">>, {{primitive, <<"boolean">>}, optional}},
            {<<"scheduledTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"scheduledPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"scheduledString">>, {{primitive, <<"string">>}, optional}},
            {<<"location">>, {{complex, <<"Reference">>}, optional}},
            {<<"performer">>, {{complex, <<"Reference">>}, list}},
            {<<"productCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"productReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"dailyAmount">>, {{complex, <<"Quantity">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            [
            {<<"scheduledTiming">>, <<"scheduledPeriod">>, <<"scheduledString">>}, 
            {<<"productCodeableConcept">>, <<"productReference">>}
            ]
} 
%%
%% CareTeam
%% The Care Team includes all the people and organizations who plan to participate in the coordination and delivery of care for a patient.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"CareTeam">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"careteamstatus_list">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"participant">>, {{bbelement, <<"CareTeam.Participant">>}, list}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"managingOrganization">>, {{complex, <<"Reference">>}, list}},
            {<<"telecom">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            []
} 
%%
%% CareTeam.Participant
%% The Care Team includes all the people and organizations who plan to participate in the coordination and delivery of care for a patient.
%%
    , <<"CareTeam.Participant">> => {<<"BackboneElement">>,
            [
            {<<"role">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"member">>, {{complex, <<"Reference">>}, optional}},
            {<<"onBehalfOf">>, {{complex, <<"Reference">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            []
} 
%%
%% CatalogEntry
%% Catalog entries are wrappers that contextualize items included in a catalog.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"CatalogEntry">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"orderable">>, {{primitive, <<"boolean">>}, required}},
            {<<"referencedItem">>, {{complex, <<"Reference">>}, required}},
            {<<"additionalIdentifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"classification">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, optional}},
            {<<"validityPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"validTo">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"lastUpdated">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"additionalCharacteristic">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"additionalClassification">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"relatedEntry">>, {{bbelement, <<"CatalogEntry.RelatedEntry">>}, list}}
            ],
            [],
            []
} 
%%
%% CatalogEntry.RelatedEntry
%% Catalog entries are wrappers that contextualize items included in a catalog.
%%
    , <<"CatalogEntry.RelatedEntry">> => {<<"BackboneElement">>,
            [
            {<<"relationtype">>, {{code, <<"catalogentryrelationtype_list">>}, required}},
            {<<"item">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% ChargeItem
%% The resource ChargeItem describes the provision of healthcare provider products for a certain patient, therefore referring not only to the product, but containing in addition details of the provision, like date, time, amounts and participating organizations and persons. Main Usage of the ChargeItem is to enable the billing process and internal cost allocation.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ChargeItem">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"definitionUri">>, {{primitive, <<"uri">>}, list}},
            {<<"definitionCanonical">>, {{primitive, <<"canonical">>}, list}},
            {<<"status">>, {{code, <<"chargeitemstatus_list">>}, required}},
            {<<"partOf">>, {{complex, <<"Reference">>}, list}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"context">>, {{complex, <<"Reference">>}, optional}},
            {<<"occurrenceDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"occurrencePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"occurrenceTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"performer">>, {{bbelement, <<"ChargeItem.Performer">>}, list}},
            {<<"performingOrganization">>, {{complex, <<"Reference">>}, optional}},
            {<<"requestingOrganization">>, {{complex, <<"Reference">>}, optional}},
            {<<"costCenter">>, {{complex, <<"Reference">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"bodysite">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"factorOverride">>, {{primitive, <<"decimal">>}, optional}},
            {<<"priceOverride">>, {{complex, <<"Money">>}, optional}},
            {<<"overrideReason">>, {{primitive, <<"string">>}, optional}},
            {<<"enterer">>, {{complex, <<"Reference">>}, optional}},
            {<<"enteredDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"reason">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"service">>, {{complex, <<"Reference">>}, list}},
            {<<"productReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"productCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"account">>, {{complex, <<"Reference">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"supportingInformation">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            [
            {<<"occurrenceDateTime">>, <<"occurrencePeriod">>, <<"occurrenceTiming">>}, 
            {<<"productReference">>, <<"productCodeableConcept">>}
            ]
} 
%%
%% ChargeItem.Performer
%% The resource ChargeItem describes the provision of healthcare provider products for a certain patient, therefore referring not only to the product, but containing in addition details of the provision, like date, time, amounts and participating organizations and persons. Main Usage of the ChargeItem is to enable the billing process and internal cost allocation.
%%
    , <<"ChargeItem.Performer">> => {<<"BackboneElement">>,
            [
            {<<"function">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"actor">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% ChargeItemDefinition
%% The ChargeItemDefinition resource provides the properties that apply to the (billing) codes necessary to calculate costs and prices. The properties may differ largely depending on type and realm, therefore this resource gives only a rough structure and requires profiling for each type of billing code system.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ChargeItemDefinition">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, required}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"derivedFromUri">>, {{primitive, <<"uri">>}, list}},
            {<<"partOf">>, {{primitive, <<"canonical">>}, list}},
            {<<"replaces">>, {{primitive, <<"canonical">>}, list}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"approvalDate">>, {{primitive, <<"date">>}, optional}},
            {<<"lastReviewDate">>, {{primitive, <<"date">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"instance">>, {{complex, <<"Reference">>}, list}},
            {<<"applicability">>, {{bbelement, <<"ChargeItemDefinition.Applicability">>}, list}},
            {<<"propertyGroup">>, {{bbelement, <<"ChargeItemDefinition.PropertyGroup">>}, list}}
            ],
            [],
            []
} 
%%
%% ChargeItemDefinition.Applicability
%% The ChargeItemDefinition resource provides the properties that apply to the (billing) codes necessary to calculate costs and prices. The properties may differ largely depending on type and realm, therefore this resource gives only a rough structure and requires profiling for each type of billing code system.
%%
    , <<"ChargeItemDefinition.Applicability">> => {<<"BackboneElement">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"language">>, {{primitive, <<"string">>}, optional}},
            {<<"expression">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% ChargeItemDefinition.PropertyGroup
%% The ChargeItemDefinition resource provides the properties that apply to the (billing) codes necessary to calculate costs and prices. The properties may differ largely depending on type and realm, therefore this resource gives only a rough structure and requires profiling for each type of billing code system.
%%
    , <<"ChargeItemDefinition.PropertyGroup">> => {<<"BackboneElement">>,
            [
            {<<"applicability">>, {{bbelement, <<"ChargeItemDefinition.Applicability">>}, list}},
            {<<"priceComponent">>, {{bbelement, <<"ChargeItemDefinition.PriceComponent">>}, list}}
            ],
            [],
            []
} 
%%
%% ChargeItemDefinition.PriceComponent
%% The ChargeItemDefinition resource provides the properties that apply to the (billing) codes necessary to calculate costs and prices. The properties may differ largely depending on type and realm, therefore this resource gives only a rough structure and requires profiling for each type of billing code system.
%%
    , <<"ChargeItemDefinition.PriceComponent">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{code, <<"invoicepricecomponenttype_list">>}, required}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"amount">>, {{complex, <<"Money">>}, optional}}
            ],
            [],
            []
} 
%%
%% Claim
%% A provider issued list of professional services and products which have been provided, or are to be provided, to a patient which is sent to an insurer for reimbursement.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Claim">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"financialresourcestatuscodes_list">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"subType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"use">>, {{code, <<"use_list">>}, required}},
            {<<"patient">>, {{complex, <<"Reference">>}, required}},
            {<<"billablePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"created">>, {{primitive, <<"dateTime">>}, required}},
            {<<"enterer">>, {{complex, <<"Reference">>}, optional}},
            {<<"insurer">>, {{complex, <<"Reference">>}, optional}},
            {<<"provider">>, {{complex, <<"Reference">>}, required}},
            {<<"priority">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"fundsReserve">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"related">>, {{bbelement, <<"Claim.Related">>}, list}},
            {<<"prescription">>, {{complex, <<"Reference">>}, optional}},
            {<<"originalPrescription">>, {{complex, <<"Reference">>}, optional}},
            {<<"payee">>, {{bbelement, <<"Claim.Payee">>}, optional}},
            {<<"referral">>, {{complex, <<"Reference">>}, optional}},
            {<<"facility">>, {{complex, <<"Reference">>}, optional}},
            {<<"careTeam">>, {{bbelement, <<"Claim.CareTeam">>}, list}},
            {<<"supportingInfo">>, {{bbelement, <<"Claim.SupportingInfo">>}, list}},
            {<<"diagnosis">>, {{bbelement, <<"Claim.Diagnosis">>}, list}},
            {<<"procedure">>, {{bbelement, <<"Claim.Procedure">>}, list}},
            {<<"insurance">>, {{bbelement, <<"Claim.Insurance">>}, non_empty_list}},
            {<<"accident">>, {{bbelement, <<"Claim.Accident">>}, optional}},
            {<<"item">>, {{bbelement, <<"Claim.Item">>}, list}},
            {<<"total">>, {{complex, <<"Money">>}, optional}}
            ],
            [],
            []
} 
%%
%% Claim.Related
%% A provider issued list of professional services and products which have been provided, or are to be provided, to a patient which is sent to an insurer for reimbursement.
%%
    , <<"Claim.Related">> => {<<"BackboneElement">>,
            [
            {<<"claim">>, {{complex, <<"Reference">>}, optional}},
            {<<"relationship">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"reference">>, {{complex, <<"Identifier">>}, optional}}
            ],
            [],
            []
} 
%%
%% Claim.Payee
%% A provider issued list of professional services and products which have been provided, or are to be provided, to a patient which is sent to an insurer for reimbursement.
%%
    , <<"Claim.Payee">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"party">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% Claim.CareTeam
%% A provider issued list of professional services and products which have been provided, or are to be provided, to a patient which is sent to an insurer for reimbursement.
%%
    , <<"Claim.CareTeam">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"provider">>, {{complex, <<"Reference">>}, required}},
            {<<"responsible">>, {{primitive, <<"boolean">>}, optional}},
            {<<"role">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"qualification">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% Claim.SupportingInfo
%% A provider issued list of professional services and products which have been provided, or are to be provided, to a patient which is sent to an insurer for reimbursement.
%%
    , <<"Claim.SupportingInfo">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"timingDate">>, {{primitive, <<"date">>}, optional}},
            {<<"timingPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"valueString">>, {{primitive, <<"string">>}, optional}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"valueAttachment">>, {{complex, <<"Attachment">>}, optional}},
            {<<"valueReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"reason">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            [
            {<<"timingDate">>, <<"timingPeriod">>}, 
            {<<"valueBoolean">>, <<"valueString">>, <<"valueQuantity">>, <<"valueAttachment">>, <<"valueReference">>}
            ]
} 
%%
%% Claim.Diagnosis
%% A provider issued list of professional services and products which have been provided, or are to be provided, to a patient which is sent to an insurer for reimbursement.
%%
    , <<"Claim.Diagnosis">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"diagnosisCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"diagnosisReference">>, {{complex, <<"Reference">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"onAdmission">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"packageCode">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            [
            {<<"diagnosisCodeableConcept">>, <<"diagnosisReference">>}
            ]
} 
%%
%% Claim.Procedure
%% A provider issued list of professional services and products which have been provided, or are to be provided, to a patient which is sent to an insurer for reimbursement.
%%
    , <<"Claim.Procedure">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"procedureCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"procedureReference">>, {{complex, <<"Reference">>}, required}},
            {<<"udi">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            [
            {<<"procedureCodeableConcept">>, <<"procedureReference">>}
            ]
} 
%%
%% Claim.Insurance
%% A provider issued list of professional services and products which have been provided, or are to be provided, to a patient which is sent to an insurer for reimbursement.
%%
    , <<"Claim.Insurance">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"focal">>, {{primitive, <<"boolean">>}, required}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"coverage">>, {{complex, <<"Reference">>}, required}},
            {<<"businessArrangement">>, {{primitive, <<"string">>}, optional}},
            {<<"preAuthRef">>, {{primitive, <<"string">>}, list}},
            {<<"claimResponse">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% Claim.Accident
%% A provider issued list of professional services and products which have been provided, or are to be provided, to a patient which is sent to an insurer for reimbursement.
%%
    , <<"Claim.Accident">> => {<<"BackboneElement">>,
            [
            {<<"date">>, {{primitive, <<"date">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"locationAddress">>, {{complex, <<"Address">>}, optional}},
            {<<"locationReference">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            [
            {<<"locationAddress">>, <<"locationReference">>}
            ]
} 
%%
%% Claim.Item
%% A provider issued list of professional services and products which have been provided, or are to be provided, to a patient which is sent to an insurer for reimbursement.
%%
    , <<"Claim.Item">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"careTeamSequence">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"diagnosisSequence">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"procedureSequence">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"informationSequence">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"revenue">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"productOrService">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"programCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"servicedDate">>, {{primitive, <<"date">>}, optional}},
            {<<"servicedPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"locationCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"locationAddress">>, {{complex, <<"Address">>}, optional}},
            {<<"locationReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"unitPrice">>, {{complex, <<"Money">>}, optional}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"net">>, {{complex, <<"Money">>}, optional}},
            {<<"udi">>, {{complex, <<"Reference">>}, list}},
            {<<"bodySite">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subSite">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"encounter">>, {{complex, <<"Reference">>}, list}},
            {<<"detail">>, {{bbelement, <<"Claim.Detail">>}, list}}
            ],
            [],
            [
            {<<"servicedDate">>, <<"servicedPeriod">>}, 
            {<<"locationCodeableConcept">>, <<"locationAddress">>, <<"locationReference">>}
            ]
} 
%%
%% Claim.Detail
%% A provider issued list of professional services and products which have been provided, or are to be provided, to a patient which is sent to an insurer for reimbursement.
%%
    , <<"Claim.Detail">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"revenue">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"productOrService">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"programCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"unitPrice">>, {{complex, <<"Money">>}, optional}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"net">>, {{complex, <<"Money">>}, optional}},
            {<<"udi">>, {{complex, <<"Reference">>}, list}},
            {<<"subDetail">>, {{bbelement, <<"Claim.SubDetail">>}, list}}
            ],
            [],
            []
} 
%%
%% Claim.SubDetail
%% A provider issued list of professional services and products which have been provided, or are to be provided, to a patient which is sent to an insurer for reimbursement.
%%
    , <<"Claim.SubDetail">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"revenue">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"productOrService">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"programCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"unitPrice">>, {{complex, <<"Money">>}, optional}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"net">>, {{complex, <<"Money">>}, optional}},
            {<<"udi">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% ClaimResponse
%% This resource provides the adjudication details from the processing of a Claim resource.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ClaimResponse">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"financialresourcestatuscodes_list">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"subType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"use">>, {{code, <<"use_list">>}, required}},
            {<<"patient">>, {{complex, <<"Reference">>}, required}},
            {<<"created">>, {{primitive, <<"dateTime">>}, required}},
            {<<"insurer">>, {{complex, <<"Reference">>}, required}},
            {<<"requestor">>, {{complex, <<"Reference">>}, optional}},
            {<<"request">>, {{complex, <<"Reference">>}, optional}},
            {<<"outcome">>, {{code, <<"claimprocessingcodes_list">>}, required}},
            {<<"disposition">>, {{primitive, <<"string">>}, optional}},
            {<<"preAuthRef">>, {{primitive, <<"string">>}, optional}},
            {<<"preAuthPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"payeeType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"item">>, {{bbelement, <<"ClaimResponse.Item">>}, list}},
            {<<"addItem">>, {{bbelement, <<"ClaimResponse.AddItem">>}, list}},
            {<<"adjudication">>, {{bbelement, <<"ClaimResponse.Adjudication">>}, list}},
            {<<"total">>, {{bbelement, <<"ClaimResponse.Total">>}, list}},
            {<<"payment">>, {{bbelement, <<"ClaimResponse.Payment">>}, optional}},
            {<<"fundsReserve">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"formCode">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"form">>, {{complex, <<"Attachment">>}, optional}},
            {<<"processNote">>, {{bbelement, <<"ClaimResponse.ProcessNote">>}, list}},
            {<<"communicationRequest">>, {{complex, <<"Reference">>}, list}},
            {<<"insurance">>, {{bbelement, <<"ClaimResponse.Insurance">>}, list}},
            {<<"error">>, {{bbelement, <<"ClaimResponse.Error">>}, list}}
            ],
            [],
            []
} 
%%
%% ClaimResponse.Item
%% This resource provides the adjudication details from the processing of a Claim resource.
%%
    , <<"ClaimResponse.Item">> => {<<"BackboneElement">>,
            [
            {<<"itemSequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"noteNumber">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"adjudication">>, {{bbelement, <<"ClaimResponse.Adjudication">>}, non_empty_list}},
            {<<"detail">>, {{bbelement, <<"ClaimResponse.Detail">>}, list}}
            ],
            [],
            []
} 
%%
%% ClaimResponse.Adjudication
%% This resource provides the adjudication details from the processing of a Claim resource.
%%
    , <<"ClaimResponse.Adjudication">> => {<<"BackboneElement">>,
            [
            {<<"category">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"reason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"amount">>, {{complex, <<"Money">>}, optional}},
            {<<"value">>, {{primitive, <<"decimal">>}, optional}}
            ],
            [],
            []
} 
%%
%% ClaimResponse.Detail
%% This resource provides the adjudication details from the processing of a Claim resource.
%%
    , <<"ClaimResponse.Detail">> => {<<"BackboneElement">>,
            [
            {<<"detailSequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"noteNumber">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"adjudication">>, {{bbelement, <<"ClaimResponse.Adjudication">>}, non_empty_list}},
            {<<"subDetail">>, {{bbelement, <<"ClaimResponse.SubDetail">>}, list}}
            ],
            [],
            []
} 
%%
%% ClaimResponse.SubDetail
%% This resource provides the adjudication details from the processing of a Claim resource.
%%
    , <<"ClaimResponse.SubDetail">> => {<<"BackboneElement">>,
            [
            {<<"subDetailSequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"noteNumber">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"adjudication">>, {{bbelement, <<"ClaimResponse.Adjudication">>}, list}}
            ],
            [],
            []
} 
%%
%% ClaimResponse.AddItem
%% This resource provides the adjudication details from the processing of a Claim resource.
%%
    , <<"ClaimResponse.AddItem">> => {<<"BackboneElement">>,
            [
            {<<"itemSequence">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"detailSequence">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"subdetailSequence">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"provider">>, {{complex, <<"Reference">>}, list}},
            {<<"productOrService">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"programCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"servicedDate">>, {{primitive, <<"date">>}, optional}},
            {<<"servicedPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"locationCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"locationAddress">>, {{complex, <<"Address">>}, optional}},
            {<<"locationReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"unitPrice">>, {{complex, <<"Money">>}, optional}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"net">>, {{complex, <<"Money">>}, optional}},
            {<<"bodySite">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subSite">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"noteNumber">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"adjudication">>, {{bbelement, <<"ClaimResponse.Adjudication">>}, non_empty_list}},
            {<<"detail">>, {{bbelement, <<"ClaimResponse.Detail1">>}, list}}
            ],
            [],
            [
            {<<"servicedDate">>, <<"servicedPeriod">>}, 
            {<<"locationCodeableConcept">>, <<"locationAddress">>, <<"locationReference">>}
            ]
} 
%%
%% ClaimResponse.Detail1
%% This resource provides the adjudication details from the processing of a Claim resource.
%%
    , <<"ClaimResponse.Detail1">> => {<<"BackboneElement">>,
            [
            {<<"productOrService">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"unitPrice">>, {{complex, <<"Money">>}, optional}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"net">>, {{complex, <<"Money">>}, optional}},
            {<<"noteNumber">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"adjudication">>, {{bbelement, <<"ClaimResponse.Adjudication">>}, non_empty_list}},
            {<<"subDetail">>, {{bbelement, <<"ClaimResponse.SubDetail1">>}, list}}
            ],
            [],
            []
} 
%%
%% ClaimResponse.SubDetail1
%% This resource provides the adjudication details from the processing of a Claim resource.
%%
    , <<"ClaimResponse.SubDetail1">> => {<<"BackboneElement">>,
            [
            {<<"productOrService">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"unitPrice">>, {{complex, <<"Money">>}, optional}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"net">>, {{complex, <<"Money">>}, optional}},
            {<<"noteNumber">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"adjudication">>, {{bbelement, <<"ClaimResponse.Adjudication">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% ClaimResponse.Total
%% This resource provides the adjudication details from the processing of a Claim resource.
%%
    , <<"ClaimResponse.Total">> => {<<"BackboneElement">>,
            [
            {<<"category">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"amount">>, {{complex, <<"Money">>}, required}}
            ],
            [],
            []
} 
%%
%% ClaimResponse.Payment
%% This resource provides the adjudication details from the processing of a Claim resource.
%%
    , <<"ClaimResponse.Payment">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"adjustment">>, {{complex, <<"Money">>}, optional}},
            {<<"adjustmentReason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"date">>, {{primitive, <<"date">>}, optional}},
            {<<"amount">>, {{complex, <<"Money">>}, required}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}}
            ],
            [],
            []
} 
%%
%% ClaimResponse.ProcessNote
%% This resource provides the adjudication details from the processing of a Claim resource.
%%
    , <<"ClaimResponse.ProcessNote">> => {<<"BackboneElement">>,
            [
            {<<"number">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"type">>, {{code, <<"notetype_list">>}, optional}},
            {<<"text">>, {{primitive, <<"string">>}, required}},
            {<<"language">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% ClaimResponse.Insurance
%% This resource provides the adjudication details from the processing of a Claim resource.
%%
    , <<"ClaimResponse.Insurance">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"focal">>, {{primitive, <<"boolean">>}, required}},
            {<<"coverage">>, {{complex, <<"Reference">>}, required}},
            {<<"businessArrangement">>, {{primitive, <<"string">>}, optional}},
            {<<"claimResponse">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% ClaimResponse.Error
%% This resource provides the adjudication details from the processing of a Claim resource.
%%
    , <<"ClaimResponse.Error">> => {<<"BackboneElement">>,
            [
            {<<"itemSequence">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"detailSequence">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"subDetailSequence">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}}
            ],
            [],
            []
} 
%%
%% ClinicalImpression
%% A record of a clinical assessment performed to determine what problem(s) may affect the patient and before planning the treatments or management strategies that are best to manage a patient's condition. Assessments are often 1:1 with a clinical consultation / encounter,  but this varies greatly depending on the clinical workflow. This resource is called "ClinicalImpression" rather than "ClinicalAssessment" to avoid confusion with the recording of assessment tools such as Apgar score.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ClinicalImpression">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"clinicalimpressionstatus_list">>}, required}},
            {<<"statusReason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"effectiveDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"assessor">>, {{complex, <<"Reference">>}, optional}},
            {<<"previous">>, {{complex, <<"Reference">>}, optional}},
            {<<"problem">>, {{complex, <<"Reference">>}, list}},
            {<<"investigation">>, {{bbelement, <<"ClinicalImpression.Investigation">>}, list}},
            {<<"protocol">>, {{primitive, <<"uri">>}, list}},
            {<<"summary">>, {{primitive, <<"string">>}, optional}},
            {<<"finding">>, {{bbelement, <<"ClinicalImpression.Finding">>}, list}},
            {<<"prognosisCodeableConcept">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"prognosisReference">>, {{complex, <<"Reference">>}, list}},
            {<<"supportingInfo">>, {{complex, <<"Reference">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            [
            {<<"effectiveDateTime">>, <<"effectivePeriod">>}
            ]
} 
%%
%% ClinicalImpression.Investigation
%% A record of a clinical assessment performed to determine what problem(s) may affect the patient and before planning the treatments or management strategies that are best to manage a patient's condition. Assessments are often 1:1 with a clinical consultation / encounter,  but this varies greatly depending on the clinical workflow. This resource is called "ClinicalImpression" rather than "ClinicalAssessment" to avoid confusion with the recording of assessment tools such as Apgar score.
%%
    , <<"ClinicalImpression.Investigation">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"item">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% ClinicalImpression.Finding
%% A record of a clinical assessment performed to determine what problem(s) may affect the patient and before planning the treatments or management strategies that are best to manage a patient's condition. Assessments are often 1:1 with a clinical consultation / encounter,  but this varies greatly depending on the clinical workflow. This resource is called "ClinicalImpression" rather than "ClinicalAssessment" to avoid confusion with the recording of assessment tools such as Apgar score.
%%
    , <<"ClinicalImpression.Finding">> => {<<"BackboneElement">>,
            [
            {<<"itemCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"itemReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"basis">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% CodeSystem
%% The CodeSystem resource is used to declare the existence of and describe a code system or code system supplement and its key properties, and optionally define a part or all of its content.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"CodeSystem">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"caseSensitive">>, {{primitive, <<"boolean">>}, optional}},
            {<<"valueSet">>, {{primitive, <<"canonical">>}, optional}},
            {<<"hierarchyMeaning">>, {{code, <<"codesystemhierarchymeaning_list">>}, optional}},
            {<<"compositional">>, {{primitive, <<"boolean">>}, optional}},
            {<<"versionNeeded">>, {{primitive, <<"boolean">>}, optional}},
            {<<"content">>, {{code, <<"codesystemcontentmode_list">>}, required}},
            {<<"supplements">>, {{primitive, <<"canonical">>}, optional}},
            {<<"count">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"filter">>, {{bbelement, <<"CodeSystem.Filter">>}, list}},
            {<<"property">>, {{bbelement, <<"CodeSystem.Property">>}, list}},
            {<<"concept">>, {{bbelement, <<"CodeSystem.Concept">>}, list}}
            ],
            [],
            []
} 
%%
%% CodeSystem.Filter
%% The CodeSystem resource is used to declare the existence of and describe a code system or code system supplement and its key properties, and optionally define a part or all of its content.
%%
    , <<"CodeSystem.Filter">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{primitive, <<"code">>}, required}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"operator">>, {{code, <<"filteroperator_list">>}, non_empty_list}},
            {<<"value">>, {{primitive, <<"string">>}, required}}
            ],
            [],
            []
} 
%%
%% CodeSystem.Property
%% The CodeSystem resource is used to declare the existence of and describe a code system or code system supplement and its key properties, and optionally define a part or all of its content.
%%
    , <<"CodeSystem.Property">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{primitive, <<"code">>}, required}},
            {<<"uri">>, {{primitive, <<"uri">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{code, <<"propertytype_list">>}, required}}
            ],
            [],
            []
} 
%%
%% CodeSystem.Concept
%% The CodeSystem resource is used to declare the existence of and describe a code system or code system supplement and its key properties, and optionally define a part or all of its content.
%%
    , <<"CodeSystem.Concept">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{primitive, <<"code">>}, required}},
            {<<"display">>, {{primitive, <<"string">>}, optional}},
            {<<"definition">>, {{primitive, <<"string">>}, optional}},
            {<<"designation">>, {{bbelement, <<"CodeSystem.Designation">>}, list}},
            {<<"property">>, {{bbelement, <<"CodeSystem.Property1">>}, list}},
            {<<"concept">>, {{bbelement, <<"CodeSystem.Concept">>}, list}}
            ],
            [],
            []
} 
%%
%% CodeSystem.Designation
%% The CodeSystem resource is used to declare the existence of and describe a code system or code system supplement and its key properties, and optionally define a part or all of its content.
%%
    , <<"CodeSystem.Designation">> => {<<"BackboneElement">>,
            [
            {<<"language">>, {{primitive, <<"code">>}, optional}},
            {<<"use">>, {{complex, <<"Coding">>}, optional}},
            {<<"value">>, {{primitive, <<"string">>}, required}}
            ],
            [],
            []
} 
%%
%% CodeSystem.Property1
%% The CodeSystem resource is used to declare the existence of and describe a code system or code system supplement and its key properties, and optionally define a part or all of its content.
%%
    , <<"CodeSystem.Property1">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{primitive, <<"code">>}, required}},
            {<<"valueCode">>, {{primitive, <<"code">>}, required}},
            {<<"valueCoding">>, {{complex, <<"Coding">>}, required}},
            {<<"valueString">>, {{primitive, <<"string">>}, required}},
            {<<"valueInteger">>, {{primitive, <<"integer">>}, required}},
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, required}},
            {<<"valueDateTime">>, {{primitive, <<"dateTime">>}, required}},
            {<<"valueDecimal">>, {{primitive, <<"decimal">>}, required}}
            ],
            [],
            [
            {<<"valueCode">>, <<"valueCoding">>, <<"valueString">>, <<"valueInteger">>, <<"valueBoolean">>, <<"valueDateTime">>, <<"valueDecimal">>}
            ]
} 
%%
%% Communication
%% An occurrence of information being transmitted; e.g. an alert that was sent to a responsible provider, a public health agency that was notified about a reportable condition.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Communication">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"instantiatesCanonical">>, {{primitive, <<"canonical">>}, list}},
            {<<"instantiatesUri">>, {{primitive, <<"uri">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"partOf">>, {{complex, <<"Reference">>}, list}},
            {<<"inResponseTo">>, {{complex, <<"Reference">>}, list}},
            {<<"status">>, {{code, <<"eventstatus_list">>}, required}},
            {<<"statusReason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"priority">>, {{code, <<"requestpriority_list">>}, optional}},
            {<<"medium">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"topic">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"about">>, {{complex, <<"Reference">>}, list}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"sent">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"received">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"recipient">>, {{complex, <<"Reference">>}, list}},
            {<<"sender">>, {{complex, <<"Reference">>}, optional}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"payload">>, {{bbelement, <<"Communication.Payload">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            []
} 
%%
%% Communication.Payload
%% An occurrence of information being transmitted; e.g. an alert that was sent to a responsible provider, a public health agency that was notified about a reportable condition.
%%
    , <<"Communication.Payload">> => {<<"BackboneElement">>,
            [
            {<<"contentString">>, {{primitive, <<"string">>}, required}},
            {<<"contentAttachment">>, {{complex, <<"Attachment">>}, required}},
            {<<"contentReference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            [
            {<<"contentString">>, <<"contentAttachment">>, <<"contentReference">>}
            ]
} 
%%
%% CommunicationRequest
%% A request to convey information; e.g. the CDS system proposes that an alert be sent to a responsible provider, the CDS system proposes that the public health agency be notified about a reportable condition.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"CommunicationRequest">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"replaces">>, {{complex, <<"Reference">>}, list}},
            {<<"groupIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"status">>, {{code, <<"requeststatus_list">>}, required}},
            {<<"statusReason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"priority">>, {{code, <<"requestpriority_list">>}, optional}},
            {<<"doNotPerform">>, {{primitive, <<"boolean">>}, optional}},
            {<<"medium">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"about">>, {{complex, <<"Reference">>}, list}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"payload">>, {{bbelement, <<"CommunicationRequest.Payload">>}, list}},
            {<<"occurrenceDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"occurrencePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"authoredOn">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"requester">>, {{complex, <<"Reference">>}, optional}},
            {<<"recipient">>, {{complex, <<"Reference">>}, list}},
            {<<"sender">>, {{complex, <<"Reference">>}, optional}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            [
            {<<"occurrenceDateTime">>, <<"occurrencePeriod">>}
            ]
} 
%%
%% CommunicationRequest.Payload
%% A request to convey information; e.g. the CDS system proposes that an alert be sent to a responsible provider, the CDS system proposes that the public health agency be notified about a reportable condition.
%%
    , <<"CommunicationRequest.Payload">> => {<<"BackboneElement">>,
            [
            {<<"contentString">>, {{primitive, <<"string">>}, required}},
            {<<"contentAttachment">>, {{complex, <<"Attachment">>}, required}},
            {<<"contentReference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            [
            {<<"contentString">>, <<"contentAttachment">>, <<"contentReference">>}
            ]
} 
%%
%% CompartmentDefinition
%% A compartment definition that defines how resources are accessed on a server.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"CompartmentDefinition">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, required}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"code">>, {{code, <<"compartmenttype_list">>}, required}},
            {<<"search">>, {{primitive, <<"boolean">>}, required}},
            {<<"resource">>, {{bbelement, <<"CompartmentDefinition.Resource">>}, list}}
            ],
            [],
            []
} 
%%
%% CompartmentDefinition.Resource
%% A compartment definition that defines how resources are accessed on a server.
%%
    , <<"CompartmentDefinition.Resource">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{primitive, <<"code">>}, required}},
            {<<"param">>, {{primitive, <<"string">>}, list}},
            {<<"documentation">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% Composition
%% A set of healthcare-related information that is assembled together into a single logical package that provides a single coherent statement of meaning, establishes its own context and that has clinical attestation with regard to who is making the statement. A Composition defines the structure and narrative content necessary for a document. However, a Composition alone does not constitute a document. Rather, the Composition must be the first entry in a Bundle where Bundle.type=document, and any other resources referenced from Composition must be included as subsequent entries in the Bundle (for example Patient, Practitioner, Encounter, etc.).
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Composition">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"status">>, {{code, <<"compositionstatus_list">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, required}},
            {<<"author">>, {{complex, <<"Reference">>}, non_empty_list}},
            {<<"title">>, {{primitive, <<"string">>}, required}},
            {<<"confidentiality">>, {{code, <<"vconfidentialityclassification_list">>}, optional}},
            {<<"attester">>, {{bbelement, <<"Composition.Attester">>}, list}},
            {<<"custodian">>, {{complex, <<"Reference">>}, optional}},
            {<<"relatesTo">>, {{bbelement, <<"Composition.RelatesTo">>}, list}},
            {<<"event">>, {{bbelement, <<"Composition.Event">>}, list}},
            {<<"section">>, {{bbelement, <<"Composition.Section">>}, list}}
            ],
            [],
            []
} 
%%
%% Composition.Attester
%% A set of healthcare-related information that is assembled together into a single logical package that provides a single coherent statement of meaning, establishes its own context and that has clinical attestation with regard to who is making the statement. A Composition defines the structure and narrative content necessary for a document. However, a Composition alone does not constitute a document. Rather, the Composition must be the first entry in a Bundle where Bundle.type=document, and any other resources referenced from Composition must be included as subsequent entries in the Bundle (for example Patient, Practitioner, Encounter, etc.).
%%
    , <<"Composition.Attester">> => {<<"BackboneElement">>,
            [
            {<<"mode">>, {{code, <<"compositionattestationmode_list">>}, required}},
            {<<"time">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"party">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% Composition.RelatesTo
%% A set of healthcare-related information that is assembled together into a single logical package that provides a single coherent statement of meaning, establishes its own context and that has clinical attestation with regard to who is making the statement. A Composition defines the structure and narrative content necessary for a document. However, a Composition alone does not constitute a document. Rather, the Composition must be the first entry in a Bundle where Bundle.type=document, and any other resources referenced from Composition must be included as subsequent entries in the Bundle (for example Patient, Practitioner, Encounter, etc.).
%%
    , <<"Composition.RelatesTo">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{code, <<"documentrelationshiptype_list">>}, required}},
            {<<"targetIdentifier">>, {{complex, <<"Identifier">>}, required}},
            {<<"targetReference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            [
            {<<"targetIdentifier">>, <<"targetReference">>}
            ]
} 
%%
%% Composition.Event
%% A set of healthcare-related information that is assembled together into a single logical package that provides a single coherent statement of meaning, establishes its own context and that has clinical attestation with regard to who is making the statement. A Composition defines the structure and narrative content necessary for a document. However, a Composition alone does not constitute a document. Rather, the Composition must be the first entry in a Bundle where Bundle.type=document, and any other resources referenced from Composition must be included as subsequent entries in the Bundle (for example Patient, Practitioner, Encounter, etc.).
%%
    , <<"Composition.Event">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"detail">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% Composition.Section
%% A set of healthcare-related information that is assembled together into a single logical package that provides a single coherent statement of meaning, establishes its own context and that has clinical attestation with regard to who is making the statement. A Composition defines the structure and narrative content necessary for a document. However, a Composition alone does not constitute a document. Rather, the Composition must be the first entry in a Bundle where Bundle.type=document, and any other resources referenced from Composition must be included as subsequent entries in the Bundle (for example Patient, Practitioner, Encounter, etc.).
%%
    , <<"Composition.Section">> => {<<"BackboneElement">>,
            [
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"author">>, {{complex, <<"Reference">>}, list}},
            {<<"focus">>, {{complex, <<"Reference">>}, optional}},
            {<<"text">>, {{special, <<"Narrative">>}, optional}},
            {<<"mode">>, {{code, <<"listmode_list">>}, optional}},
            {<<"orderedBy">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"entry">>, {{complex, <<"Reference">>}, list}},
            {<<"emptyReason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"section">>, {{bbelement, <<"Composition.Section">>}, list}}
            ],
            [],
            []
} 
%%
%% ConceptMap
%% A statement of relationships from one set of concepts to one or more other concepts - either concepts in code systems, or data element/data element concepts, or classes in class models.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ConceptMap">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"sourceUri">>, {{primitive, <<"uri">>}, optional}},
            {<<"sourceCanonical">>, {{primitive, <<"canonical">>}, optional}},
            {<<"targetUri">>, {{primitive, <<"uri">>}, optional}},
            {<<"targetCanonical">>, {{primitive, <<"canonical">>}, optional}},
            {<<"group">>, {{bbelement, <<"ConceptMap.Group">>}, list}}
            ],
            [],
            [
            {<<"sourceUri">>, <<"sourceCanonical">>}, 
            {<<"targetUri">>, <<"targetCanonical">>}
            ]
} 
%%
%% ConceptMap.Group
%% A statement of relationships from one set of concepts to one or more other concepts - either concepts in code systems, or data element/data element concepts, or classes in class models.
%%
    , <<"ConceptMap.Group">> => {<<"BackboneElement">>,
            [
            {<<"source">>, {{primitive, <<"uri">>}, optional}},
            {<<"sourceVersion">>, {{primitive, <<"string">>}, optional}},
            {<<"target">>, {{primitive, <<"uri">>}, optional}},
            {<<"targetVersion">>, {{primitive, <<"string">>}, optional}},
            {<<"element">>, {{bbelement, <<"ConceptMap.Element">>}, non_empty_list}},
            {<<"unmapped">>, {{bbelement, <<"ConceptMap.Unmapped">>}, optional}}
            ],
            [],
            []
} 
%%
%% ConceptMap.Element
%% A statement of relationships from one set of concepts to one or more other concepts - either concepts in code systems, or data element/data element concepts, or classes in class models.
%%
    , <<"ConceptMap.Element">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{primitive, <<"code">>}, optional}},
            {<<"display">>, {{primitive, <<"string">>}, optional}},
            {<<"target">>, {{bbelement, <<"ConceptMap.Target">>}, list}}
            ],
            [],
            []
} 
%%
%% ConceptMap.Target
%% A statement of relationships from one set of concepts to one or more other concepts - either concepts in code systems, or data element/data element concepts, or classes in class models.
%%
    , <<"ConceptMap.Target">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{primitive, <<"code">>}, optional}},
            {<<"display">>, {{primitive, <<"string">>}, optional}},
            {<<"equivalence">>, {{code, <<"conceptmapequivalence_list">>}, required}},
            {<<"comment">>, {{primitive, <<"string">>}, optional}},
            {<<"dependsOn">>, {{bbelement, <<"ConceptMap.DependsOn">>}, list}},
            {<<"product">>, {{bbelement, <<"ConceptMap.DependsOn">>}, list}}
            ],
            [],
            []
} 
%%
%% ConceptMap.DependsOn
%% A statement of relationships from one set of concepts to one or more other concepts - either concepts in code systems, or data element/data element concepts, or classes in class models.
%%
    , <<"ConceptMap.DependsOn">> => {<<"BackboneElement">>,
            [
            {<<"property">>, {{primitive, <<"uri">>}, required}},
            {<<"system">>, {{primitive, <<"canonical">>}, optional}},
            {<<"value">>, {{primitive, <<"string">>}, required}},
            {<<"display">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% ConceptMap.Unmapped
%% A statement of relationships from one set of concepts to one or more other concepts - either concepts in code systems, or data element/data element concepts, or classes in class models.
%%
    , <<"ConceptMap.Unmapped">> => {<<"BackboneElement">>,
            [
            {<<"mode">>, {{code, <<"conceptmapgroupunmappedmode_list">>}, required}},
            {<<"code">>, {{primitive, <<"code">>}, optional}},
            {<<"display">>, {{primitive, <<"string">>}, optional}},
            {<<"url">>, {{primitive, <<"canonical">>}, optional}}
            ],
            [],
            []
} 
%%
%% Condition
%% A clinical condition, problem, diagnosis, or other event, situation, issue, or clinical concept that has risen to a level of concern.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Condition">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"clinicalStatus">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"verificationStatus">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"severity">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"bodySite">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"onsetDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"onsetAge">>, {{complex, <<"Age">>}, optional}},
            {<<"onsetPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"onsetRange">>, {{complex, <<"Range">>}, optional}},
            {<<"onsetString">>, {{primitive, <<"string">>}, optional}},
            {<<"abatementDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"abatementAge">>, {{complex, <<"Age">>}, optional}},
            {<<"abatementPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"abatementRange">>, {{complex, <<"Range">>}, optional}},
            {<<"abatementString">>, {{primitive, <<"string">>}, optional}},
            {<<"recordedDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"recorder">>, {{complex, <<"Reference">>}, optional}},
            {<<"asserter">>, {{complex, <<"Reference">>}, optional}},
            {<<"stage">>, {{bbelement, <<"Condition.Stage">>}, list}},
            {<<"evidence">>, {{bbelement, <<"Condition.Evidence">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            [
            {<<"onsetDateTime">>, <<"onsetAge">>, <<"onsetPeriod">>, <<"onsetRange">>, <<"onsetString">>}, 
            {<<"abatementDateTime">>, <<"abatementAge">>, <<"abatementPeriod">>, <<"abatementRange">>, <<"abatementString">>}
            ]
} 
%%
%% Condition.Stage
%% A clinical condition, problem, diagnosis, or other event, situation, issue, or clinical concept that has risen to a level of concern.
%%
    , <<"Condition.Stage">> => {<<"BackboneElement">>,
            [
            {<<"summary">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"assessment">>, {{complex, <<"Reference">>}, list}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% Condition.Evidence
%% A clinical condition, problem, diagnosis, or other event, situation, issue, or clinical concept that has risen to a level of concern.
%%
    , <<"Condition.Evidence">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"detail">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% Consent
%% A record of a healthcare consumer’s  choices, which permits or denies identified recipient(s) or recipient role(s) to perform one or more actions within a given policy context, for specific purposes and periods of time.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Consent">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"consentstate_list">>}, required}},
            {<<"scope">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, non_empty_list}},
            {<<"patient">>, {{complex, <<"Reference">>}, optional}},
            {<<"dateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"performer">>, {{complex, <<"Reference">>}, list}},
            {<<"organization">>, {{complex, <<"Reference">>}, list}},
            {<<"sourceAttachment">>, {{complex, <<"Attachment">>}, optional}},
            {<<"sourceReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"policy">>, {{bbelement, <<"Consent.Policy">>}, list}},
            {<<"policyRule">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"verification">>, {{bbelement, <<"Consent.Verification">>}, list}},
            {<<"provision">>, {{bbelement, <<"Consent.Provision">>}, optional}}
            ],
            [],
            [
            {<<"sourceAttachment">>, <<"sourceReference">>}
            ]
} 
%%
%% Consent.Policy
%% A record of a healthcare consumer’s  choices, which permits or denies identified recipient(s) or recipient role(s) to perform one or more actions within a given policy context, for specific purposes and periods of time.
%%
    , <<"Consent.Policy">> => {<<"BackboneElement">>,
            [
            {<<"authority">>, {{primitive, <<"uri">>}, optional}},
            {<<"uri">>, {{primitive, <<"uri">>}, optional}}
            ],
            [],
            []
} 
%%
%% Consent.Verification
%% A record of a healthcare consumer’s  choices, which permits or denies identified recipient(s) or recipient role(s) to perform one or more actions within a given policy context, for specific purposes and periods of time.
%%
    , <<"Consent.Verification">> => {<<"BackboneElement">>,
            [
            {<<"verified">>, {{primitive, <<"boolean">>}, required}},
            {<<"verifiedWith">>, {{complex, <<"Reference">>}, optional}},
            {<<"verificationDate">>, {{primitive, <<"dateTime">>}, optional}}
            ],
            [],
            []
} 
%%
%% Consent.Provision
%% A record of a healthcare consumer’s  choices, which permits or denies identified recipient(s) or recipient role(s) to perform one or more actions within a given policy context, for specific purposes and periods of time.
%%
    , <<"Consent.Provision">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{code, <<"consentprovisiontype_list">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"actor">>, {{bbelement, <<"Consent.Actor">>}, list}},
            {<<"action">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"securityLabel">>, {{complex, <<"Coding">>}, list}},
            {<<"purpose">>, {{complex, <<"Coding">>}, list}},
            {<<"class">>, {{complex, <<"Coding">>}, list}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"dataPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"data">>, {{bbelement, <<"Consent.Data">>}, list}},
            {<<"provision">>, {{bbelement, <<"Consent.Provision">>}, list}}
            ],
            [],
            []
} 
%%
%% Consent.Actor
%% A record of a healthcare consumer’s  choices, which permits or denies identified recipient(s) or recipient role(s) to perform one or more actions within a given policy context, for specific purposes and periods of time.
%%
    , <<"Consent.Actor">> => {<<"BackboneElement">>,
            [
            {<<"role">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"reference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% Consent.Data
%% A record of a healthcare consumer’s  choices, which permits or denies identified recipient(s) or recipient role(s) to perform one or more actions within a given policy context, for specific purposes and periods of time.
%%
    , <<"Consent.Data">> => {<<"BackboneElement">>,
            [
            {<<"meaning">>, {{code, <<"consentdatameaning_list">>}, required}},
            {<<"reference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% Contract
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Contract">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"contractresourcestatuscodes_list">>}, optional}},
            {<<"legalState">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"instantiatesCanonical">>, {{complex, <<"Reference">>}, optional}},
            {<<"instantiatesUri">>, {{primitive, <<"uri">>}, optional}},
            {<<"contentDerivative">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"issued">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"applies">>, {{complex, <<"Period">>}, optional}},
            {<<"expirationType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, list}},
            {<<"authority">>, {{complex, <<"Reference">>}, list}},
            {<<"domain">>, {{complex, <<"Reference">>}, list}},
            {<<"site">>, {{complex, <<"Reference">>}, list}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"subtitle">>, {{primitive, <<"string">>}, optional}},
            {<<"alias">>, {{primitive, <<"string">>}, list}},
            {<<"author">>, {{complex, <<"Reference">>}, optional}},
            {<<"scope">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"topicCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"topicReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subType">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"contentDefinition">>, {{bbelement, <<"Contract.ContentDefinition">>}, optional}},
            {<<"term">>, {{bbelement, <<"Contract.Term">>}, list}},
            {<<"supportingInfo">>, {{complex, <<"Reference">>}, list}},
            {<<"relevantHistory">>, {{complex, <<"Reference">>}, list}},
            {<<"signer">>, {{bbelement, <<"Contract.Signer">>}, list}},
            {<<"friendly">>, {{bbelement, <<"Contract.Friendly">>}, list}},
            {<<"legal">>, {{bbelement, <<"Contract.Legal">>}, list}},
            {<<"rule">>, {{bbelement, <<"Contract.Rule">>}, list}},
            {<<"legallyBindingAttachment">>, {{complex, <<"Attachment">>}, optional}},
            {<<"legallyBindingReference">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            [
            {<<"topicCodeableConcept">>, <<"topicReference">>}, 
            {<<"legallyBindingAttachment">>, <<"legallyBindingReference">>}
            ]
} 
%%
%% Contract.ContentDefinition
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%%
    , <<"Contract.ContentDefinition">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"subType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"publisher">>, {{complex, <<"Reference">>}, optional}},
            {<<"publicationDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publicationStatus">>, {{code, <<"contractresourcepublicationstatuscodes_list">>}, required}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}}
            ],
            [],
            []
} 
%%
%% Contract.Term
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%%
    , <<"Contract.Term">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"issued">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"applies">>, {{complex, <<"Period">>}, optional}},
            {<<"topicCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"topicReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"text">>, {{primitive, <<"string">>}, optional}},
            {<<"securityLabel">>, {{bbelement, <<"Contract.SecurityLabel">>}, list}},
            {<<"offer">>, {{bbelement, <<"Contract.Offer">>}, required}},
            {<<"asset">>, {{bbelement, <<"Contract.Asset">>}, list}},
            {<<"action">>, {{bbelement, <<"Contract.Action">>}, list}},
            {<<"group">>, {{bbelement, <<"Contract.Term">>}, list}}
            ],
            [],
            [
            {<<"topicCodeableConcept">>, <<"topicReference">>}
            ]
} 
%%
%% Contract.SecurityLabel
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%%
    , <<"Contract.SecurityLabel">> => {<<"BackboneElement">>,
            [
            {<<"number">>, {{primitive, <<"unsignedInt">>}, list}},
            {<<"classification">>, {{complex, <<"Coding">>}, required}},
            {<<"category">>, {{complex, <<"Coding">>}, list}},
            {<<"control">>, {{complex, <<"Coding">>}, list}}
            ],
            [],
            []
} 
%%
%% Contract.Offer
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%%
    , <<"Contract.Offer">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"party">>, {{bbelement, <<"Contract.Party">>}, list}},
            {<<"topic">>, {{complex, <<"Reference">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"decision">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"decisionMode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"answer">>, {{bbelement, <<"Contract.Answer">>}, list}},
            {<<"text">>, {{primitive, <<"string">>}, optional}},
            {<<"linkId">>, {{primitive, <<"string">>}, list}},
            {<<"securityLabelNumber">>, {{primitive, <<"unsignedInt">>}, list}}
            ],
            [],
            []
} 
%%
%% Contract.Party
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%%
    , <<"Contract.Party">> => {<<"BackboneElement">>,
            [
            {<<"reference">>, {{complex, <<"Reference">>}, non_empty_list}},
            {<<"role">>, {{complex, <<"CodeableConcept">>}, required}}
            ],
            [],
            []
} 
%%
%% Contract.Answer
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%%
    , <<"Contract.Answer">> => {<<"BackboneElement">>,
            [
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, required}},
            {<<"valueDecimal">>, {{primitive, <<"decimal">>}, required}},
            {<<"valueInteger">>, {{primitive, <<"integer">>}, required}},
            {<<"valueDate">>, {{primitive, <<"date">>}, required}},
            {<<"valueDateTime">>, {{primitive, <<"dateTime">>}, required}},
            {<<"valueTime">>, {{primitive, <<"time">>}, required}},
            {<<"valueString">>, {{primitive, <<"string">>}, required}},
            {<<"valueUri">>, {{primitive, <<"uri">>}, required}},
            {<<"valueAttachment">>, {{complex, <<"Attachment">>}, required}},
            {<<"valueCoding">>, {{complex, <<"Coding">>}, required}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, required}},
            {<<"valueReference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            [
            {<<"valueBoolean">>, <<"valueDecimal">>, <<"valueInteger">>, <<"valueDate">>, <<"valueDateTime">>, <<"valueTime">>, <<"valueString">>, <<"valueUri">>, <<"valueAttachment">>, <<"valueCoding">>, <<"valueQuantity">>, <<"valueReference">>}
            ]
} 
%%
%% Contract.Asset
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%%
    , <<"Contract.Asset">> => {<<"BackboneElement">>,
            [
            {<<"scope">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"typeReference">>, {{complex, <<"Reference">>}, list}},
            {<<"subtype">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"relationship">>, {{complex, <<"Coding">>}, optional}},
            {<<"context">>, {{bbelement, <<"Contract.Context">>}, list}},
            {<<"condition">>, {{primitive, <<"string">>}, optional}},
            {<<"periodType">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"period">>, {{complex, <<"Period">>}, list}},
            {<<"usePeriod">>, {{complex, <<"Period">>}, list}},
            {<<"text">>, {{primitive, <<"string">>}, optional}},
            {<<"linkId">>, {{primitive, <<"string">>}, list}},
            {<<"answer">>, {{bbelement, <<"Contract.Answer">>}, list}},
            {<<"securityLabelNumber">>, {{primitive, <<"unsignedInt">>}, list}},
            {<<"valuedItem">>, {{bbelement, <<"Contract.ValuedItem">>}, list}}
            ],
            [],
            []
} 
%%
%% Contract.Context
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%%
    , <<"Contract.Context">> => {<<"BackboneElement">>,
            [
            {<<"reference">>, {{complex, <<"Reference">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"text">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% Contract.ValuedItem
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%%
    , <<"Contract.ValuedItem">> => {<<"BackboneElement">>,
            [
            {<<"entityCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"entityReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"effectiveTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"unitPrice">>, {{complex, <<"Money">>}, optional}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"points">>, {{primitive, <<"decimal">>}, optional}},
            {<<"net">>, {{complex, <<"Money">>}, optional}},
            {<<"payment">>, {{primitive, <<"string">>}, optional}},
            {<<"paymentDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"responsible">>, {{complex, <<"Reference">>}, optional}},
            {<<"recipient">>, {{complex, <<"Reference">>}, optional}},
            {<<"linkId">>, {{primitive, <<"string">>}, list}},
            {<<"securityLabelNumber">>, {{primitive, <<"unsignedInt">>}, list}}
            ],
            [],
            [
            {<<"entityCodeableConcept">>, <<"entityReference">>}
            ]
} 
%%
%% Contract.Action
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%%
    , <<"Contract.Action">> => {<<"BackboneElement">>,
            [
            {<<"doNotPerform">>, {{primitive, <<"boolean">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"subject">>, {{bbelement, <<"Contract.Subject">>}, list}},
            {<<"intent">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"linkId">>, {{primitive, <<"string">>}, list}},
            {<<"status">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"context">>, {{complex, <<"Reference">>}, optional}},
            {<<"contextLinkId">>, {{primitive, <<"string">>}, list}},
            {<<"occurrenceDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"occurrencePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"occurrenceTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"requester">>, {{complex, <<"Reference">>}, list}},
            {<<"requesterLinkId">>, {{primitive, <<"string">>}, list}},
            {<<"performerType">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"performerRole">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"performer">>, {{complex, <<"Reference">>}, optional}},
            {<<"performerLinkId">>, {{primitive, <<"string">>}, list}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"reason">>, {{primitive, <<"string">>}, list}},
            {<<"reasonLinkId">>, {{primitive, <<"string">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"securityLabelNumber">>, {{primitive, <<"unsignedInt">>}, list}}
            ],
            [],
            [
            {<<"occurrenceDateTime">>, <<"occurrencePeriod">>, <<"occurrenceTiming">>}
            ]
} 
%%
%% Contract.Subject
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%%
    , <<"Contract.Subject">> => {<<"BackboneElement">>,
            [
            {<<"reference">>, {{complex, <<"Reference">>}, non_empty_list}},
            {<<"role">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% Contract.Signer
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%%
    , <<"Contract.Signer">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"Coding">>}, required}},
            {<<"party">>, {{complex, <<"Reference">>}, required}},
            {<<"signature">>, {{complex, <<"Signature">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% Contract.Friendly
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%%
    , <<"Contract.Friendly">> => {<<"BackboneElement">>,
            [
            {<<"contentAttachment">>, {{complex, <<"Attachment">>}, required}},
            {<<"contentReference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            [
            {<<"contentAttachment">>, <<"contentReference">>}
            ]
} 
%%
%% Contract.Legal
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%%
    , <<"Contract.Legal">> => {<<"BackboneElement">>,
            [
            {<<"contentAttachment">>, {{complex, <<"Attachment">>}, required}},
            {<<"contentReference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            [
            {<<"contentAttachment">>, <<"contentReference">>}
            ]
} 
%%
%% Contract.Rule
%% Legally enforceable, formally recorded unilateral or bilateral directive i.e., a policy or agreement.
%%
    , <<"Contract.Rule">> => {<<"BackboneElement">>,
            [
            {<<"contentAttachment">>, {{complex, <<"Attachment">>}, required}},
            {<<"contentReference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            [
            {<<"contentAttachment">>, <<"contentReference">>}
            ]
} 
%%
%% Coverage
%% Financial instrument which may be used to reimburse or pay for health care products and services. Includes both insurance and self-payment.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Coverage">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"financialresourcestatuscodes_list">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"policyHolder">>, {{complex, <<"Reference">>}, optional}},
            {<<"subscriber">>, {{complex, <<"Reference">>}, optional}},
            {<<"subscriberId">>, {{primitive, <<"string">>}, optional}},
            {<<"beneficiary">>, {{complex, <<"Reference">>}, required}},
            {<<"dependent">>, {{primitive, <<"string">>}, optional}},
            {<<"relationship">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"payor">>, {{complex, <<"Reference">>}, non_empty_list}},
            {<<"class">>, {{bbelement, <<"Coverage.Class">>}, list}},
            {<<"order">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"network">>, {{primitive, <<"string">>}, optional}},
            {<<"costToBeneficiary">>, {{bbelement, <<"Coverage.CostToBeneficiary">>}, list}},
            {<<"subrogation">>, {{primitive, <<"boolean">>}, optional}},
            {<<"contract">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% Coverage.Class
%% Financial instrument which may be used to reimburse or pay for health care products and services. Includes both insurance and self-payment.
%%
    , <<"Coverage.Class">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"value">>, {{primitive, <<"string">>}, required}},
            {<<"name">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% Coverage.CostToBeneficiary
%% Financial instrument which may be used to reimburse or pay for health care products and services. Includes both insurance and self-payment.
%%
    , <<"Coverage.CostToBeneficiary">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, required}},
            {<<"valueMoney">>, {{complex, <<"Money">>}, required}},
            {<<"exception">>, {{bbelement, <<"Coverage.Exception">>}, list}}
            ],
            [],
            [
            {<<"valueQuantity">>, <<"valueMoney">>}
            ]
} 
%%
%% Coverage.Exception
%% Financial instrument which may be used to reimburse or pay for health care products and services. Includes both insurance and self-payment.
%%
    , <<"Coverage.Exception">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"period">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            []
} 
%%
%% CoverageEligibilityRequest
%% The CoverageEligibilityRequest provides patient and insurance coverage information to an insurer for them to respond, in the form of an CoverageEligibilityResponse, with information regarding whether the stated coverage is valid and in-force and optionally to provide the insurance details of the policy.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"CoverageEligibilityRequest">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"financialresourcestatuscodes_list">>}, required}},
            {<<"priority">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"purpose">>, {{code, <<"eligibilityrequestpurpose_list">>}, non_empty_list}},
            {<<"patient">>, {{complex, <<"Reference">>}, required}},
            {<<"servicedDate">>, {{primitive, <<"date">>}, optional}},
            {<<"servicedPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"created">>, {{primitive, <<"dateTime">>}, required}},
            {<<"enterer">>, {{complex, <<"Reference">>}, optional}},
            {<<"provider">>, {{complex, <<"Reference">>}, optional}},
            {<<"insurer">>, {{complex, <<"Reference">>}, required}},
            {<<"facility">>, {{complex, <<"Reference">>}, optional}},
            {<<"supportingInfo">>, {{bbelement, <<"CoverageEligibilityRequest.SupportingInfo">>}, list}},
            {<<"insurance">>, {{bbelement, <<"CoverageEligibilityRequest.Insurance">>}, list}},
            {<<"item">>, {{bbelement, <<"CoverageEligibilityRequest.Item">>}, list}}
            ],
            [],
            [
            {<<"servicedDate">>, <<"servicedPeriod">>}
            ]
} 
%%
%% CoverageEligibilityRequest.SupportingInfo
%% The CoverageEligibilityRequest provides patient and insurance coverage information to an insurer for them to respond, in the form of an CoverageEligibilityResponse, with information regarding whether the stated coverage is valid and in-force and optionally to provide the insurance details of the policy.
%%
    , <<"CoverageEligibilityRequest.SupportingInfo">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"information">>, {{complex, <<"Reference">>}, required}},
            {<<"appliesToAll">>, {{primitive, <<"boolean">>}, optional}}
            ],
            [],
            []
} 
%%
%% CoverageEligibilityRequest.Insurance
%% The CoverageEligibilityRequest provides patient and insurance coverage information to an insurer for them to respond, in the form of an CoverageEligibilityResponse, with information regarding whether the stated coverage is valid and in-force and optionally to provide the insurance details of the policy.
%%
    , <<"CoverageEligibilityRequest.Insurance">> => {<<"BackboneElement">>,
            [
            {<<"focal">>, {{primitive, <<"boolean">>}, optional}},
            {<<"coverage">>, {{complex, <<"Reference">>}, required}},
            {<<"businessArrangement">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% CoverageEligibilityRequest.Item
%% The CoverageEligibilityRequest provides patient and insurance coverage information to an insurer for them to respond, in the form of an CoverageEligibilityResponse, with information regarding whether the stated coverage is valid and in-force and optionally to provide the insurance details of the policy.
%%
    , <<"CoverageEligibilityRequest.Item">> => {<<"BackboneElement">>,
            [
            {<<"supportingInfoSequence">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"productOrService">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"provider">>, {{complex, <<"Reference">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"unitPrice">>, {{complex, <<"Money">>}, optional}},
            {<<"facility">>, {{complex, <<"Reference">>}, optional}},
            {<<"diagnosis">>, {{bbelement, <<"CoverageEligibilityRequest.Diagnosis">>}, list}},
            {<<"detail">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% CoverageEligibilityRequest.Diagnosis
%% The CoverageEligibilityRequest provides patient and insurance coverage information to an insurer for them to respond, in the form of an CoverageEligibilityResponse, with information regarding whether the stated coverage is valid and in-force and optionally to provide the insurance details of the policy.
%%
    , <<"CoverageEligibilityRequest.Diagnosis">> => {<<"BackboneElement">>,
            [
            {<<"diagnosisCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"diagnosisReference">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            [
            {<<"diagnosisCodeableConcept">>, <<"diagnosisReference">>}
            ]
} 
%%
%% CoverageEligibilityResponse
%% This resource provides eligibility and plan details from the processing of an CoverageEligibilityRequest resource.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"CoverageEligibilityResponse">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"financialresourcestatuscodes_list">>}, required}},
            {<<"purpose">>, {{code, <<"eligibilityresponsepurpose_list">>}, non_empty_list}},
            {<<"patient">>, {{complex, <<"Reference">>}, required}},
            {<<"servicedDate">>, {{primitive, <<"date">>}, optional}},
            {<<"servicedPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"created">>, {{primitive, <<"dateTime">>}, required}},
            {<<"requestor">>, {{complex, <<"Reference">>}, optional}},
            {<<"request">>, {{complex, <<"Reference">>}, required}},
            {<<"outcome">>, {{code, <<"remittanceoutcome_list">>}, required}},
            {<<"disposition">>, {{primitive, <<"string">>}, optional}},
            {<<"insurer">>, {{complex, <<"Reference">>}, required}},
            {<<"insurance">>, {{bbelement, <<"CoverageEligibilityResponse.Insurance">>}, list}},
            {<<"preAuthRef">>, {{primitive, <<"string">>}, optional}},
            {<<"form">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"error">>, {{bbelement, <<"CoverageEligibilityResponse.Error">>}, list}}
            ],
            [],
            [
            {<<"servicedDate">>, <<"servicedPeriod">>}
            ]
} 
%%
%% CoverageEligibilityResponse.Insurance
%% This resource provides eligibility and plan details from the processing of an CoverageEligibilityRequest resource.
%%
    , <<"CoverageEligibilityResponse.Insurance">> => {<<"BackboneElement">>,
            [
            {<<"coverage">>, {{complex, <<"Reference">>}, required}},
            {<<"inforce">>, {{primitive, <<"boolean">>}, optional}},
            {<<"benefitPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"item">>, {{bbelement, <<"CoverageEligibilityResponse.Item">>}, list}}
            ],
            [],
            []
} 
%%
%% CoverageEligibilityResponse.Item
%% This resource provides eligibility and plan details from the processing of an CoverageEligibilityRequest resource.
%%
    , <<"CoverageEligibilityResponse.Item">> => {<<"BackboneElement">>,
            [
            {<<"category">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"productOrService">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"provider">>, {{complex, <<"Reference">>}, optional}},
            {<<"excluded">>, {{primitive, <<"boolean">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"network">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"unit">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"term">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"benefit">>, {{bbelement, <<"CoverageEligibilityResponse.Benefit">>}, list}},
            {<<"authorizationRequired">>, {{primitive, <<"boolean">>}, optional}},
            {<<"authorizationSupporting">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"authorizationUrl">>, {{primitive, <<"uri">>}, optional}}
            ],
            [],
            []
} 
%%
%% CoverageEligibilityResponse.Benefit
%% This resource provides eligibility and plan details from the processing of an CoverageEligibilityRequest resource.
%%
    , <<"CoverageEligibilityResponse.Benefit">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"allowedUnsignedInt">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"allowedString">>, {{primitive, <<"string">>}, optional}},
            {<<"allowedMoney">>, {{complex, <<"Money">>}, optional}},
            {<<"usedUnsignedInt">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"usedString">>, {{primitive, <<"string">>}, optional}},
            {<<"usedMoney">>, {{complex, <<"Money">>}, optional}}
            ],
            [],
            [
            {<<"allowedUnsignedInt">>, <<"allowedString">>, <<"allowedMoney">>}, 
            {<<"usedUnsignedInt">>, <<"usedString">>, <<"usedMoney">>}
            ]
} 
%%
%% CoverageEligibilityResponse.Error
%% This resource provides eligibility and plan details from the processing of an CoverageEligibilityRequest resource.
%%
    , <<"CoverageEligibilityResponse.Error">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}}
            ],
            [],
            []
} 
%%
%% DetectedIssue
%% Indicates an actual or potential clinical issue with or between one or more active or proposed clinical actions for a patient; e.g. Drug-drug interaction, Ineffective treatment frequency, Procedure-condition conflict, etc.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"DetectedIssue">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"observationstatus_list">>}, required}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"severity">>, {{code, <<"detectedissueseverity_list">>}, optional}},
            {<<"patient">>, {{complex, <<"Reference">>}, optional}},
            {<<"identifiedDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"identifiedPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"author">>, {{complex, <<"Reference">>}, optional}},
            {<<"implicated">>, {{complex, <<"Reference">>}, list}},
            {<<"evidence">>, {{bbelement, <<"DetectedIssue.Evidence">>}, list}},
            {<<"detail">>, {{primitive, <<"string">>}, optional}},
            {<<"reference">>, {{primitive, <<"uri">>}, optional}},
            {<<"mitigation">>, {{bbelement, <<"DetectedIssue.Mitigation">>}, list}}
            ],
            [],
            [
            {<<"identifiedDateTime">>, <<"identifiedPeriod">>}
            ]
} 
%%
%% DetectedIssue.Evidence
%% Indicates an actual or potential clinical issue with or between one or more active or proposed clinical actions for a patient; e.g. Drug-drug interaction, Ineffective treatment frequency, Procedure-condition conflict, etc.
%%
    , <<"DetectedIssue.Evidence">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"detail">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% DetectedIssue.Mitigation
%% Indicates an actual or potential clinical issue with or between one or more active or proposed clinical actions for a patient; e.g. Drug-drug interaction, Ineffective treatment frequency, Procedure-condition conflict, etc.
%%
    , <<"DetectedIssue.Mitigation">> => {<<"BackboneElement">>,
            [
            {<<"action">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"author">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% Device
%% A type of a manufactured item that is used in the provision of healthcare without being substantially changed through that activity. The device may be a medical or non-medical device.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Device">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"definition">>, {{complex, <<"Reference">>}, optional}},
            {<<"udiCarrier">>, {{bbelement, <<"Device.UdiCarrier">>}, list}},
            {<<"status">>, {{code, <<"fhirdevicestatus_list">>}, optional}},
            {<<"statusReason">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"distinctIdentifier">>, {{primitive, <<"string">>}, optional}},
            {<<"manufacturer">>, {{primitive, <<"string">>}, optional}},
            {<<"manufactureDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"expirationDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"lotNumber">>, {{primitive, <<"string">>}, optional}},
            {<<"serialNumber">>, {{primitive, <<"string">>}, optional}},
            {<<"deviceName">>, {{bbelement, <<"Device.DeviceName">>}, list}},
            {<<"modelNumber">>, {{primitive, <<"string">>}, optional}},
            {<<"partNumber">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"specialization">>, {{bbelement, <<"Device.Specialization">>}, list}},
            {<<"version">>, {{bbelement, <<"Device.Version">>}, list}},
            {<<"property">>, {{bbelement, <<"Device.Property">>}, list}},
            {<<"patient">>, {{complex, <<"Reference">>}, optional}},
            {<<"owner">>, {{complex, <<"Reference">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"location">>, {{complex, <<"Reference">>}, optional}},
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"safety">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"parent">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% Device.UdiCarrier
%% A type of a manufactured item that is used in the provision of healthcare without being substantially changed through that activity. The device may be a medical or non-medical device.
%%
    , <<"Device.UdiCarrier">> => {<<"BackboneElement">>,
            [
            {<<"deviceIdentifier">>, {{primitive, <<"string">>}, optional}},
            {<<"issuer">>, {{primitive, <<"uri">>}, optional}},
            {<<"jurisdiction">>, {{primitive, <<"uri">>}, optional}},
            {<<"carrierAIDC">>, {{primitive, <<"base64Binary">>}, optional}},
            {<<"carrierHRF">>, {{primitive, <<"string">>}, optional}},
            {<<"entryType">>, {{code, <<"udientrytype_list">>}, optional}}
            ],
            [],
            []
} 
%%
%% Device.DeviceName
%% A type of a manufactured item that is used in the provision of healthcare without being substantially changed through that activity. The device may be a medical or non-medical device.
%%
    , <<"Device.DeviceName">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"type">>, {{code, <<"devicenametype_list">>}, required}}
            ],
            [],
            []
} 
%%
%% Device.Specialization
%% A type of a manufactured item that is used in the provision of healthcare without being substantially changed through that activity. The device may be a medical or non-medical device.
%%
    , <<"Device.Specialization">> => {<<"BackboneElement">>,
            [
            {<<"systemType">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"version">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% Device.Version
%% A type of a manufactured item that is used in the provision of healthcare without being substantially changed through that activity. The device may be a medical or non-medical device.
%%
    , <<"Device.Version">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"component">>, {{complex, <<"Identifier">>}, optional}},
            {<<"value">>, {{primitive, <<"string">>}, required}}
            ],
            [],
            []
} 
%%
%% Device.Property
%% A type of a manufactured item that is used in the provision of healthcare without being substantially changed through that activity. The device may be a medical or non-medical device.
%%
    , <<"Device.Property">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, list}},
            {<<"valueCode">>, {{complex, <<"CodeableConcept">>}, list}}
            ],
            [],
            []
} 
%%
%% DeviceDefinition
%% The characteristics, operational status and capabilities of a medical-related component of a medical device.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"DeviceDefinition">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"udiDeviceIdentifier">>, {{bbelement, <<"DeviceDefinition.UdiDeviceIdentifier">>}, list}},
            {<<"manufacturerString">>, {{primitive, <<"string">>}, optional}},
            {<<"manufacturerReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"deviceName">>, {{bbelement, <<"DeviceDefinition.DeviceName">>}, list}},
            {<<"modelNumber">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"specialization">>, {{bbelement, <<"DeviceDefinition.Specialization">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, list}},
            {<<"safety">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"shelfLifeStorage">>, {{bbelement, <<"ProductShelfLife">>}, list}},
            {<<"physicalCharacteristics">>, {{bbelement, <<"ProdCharacteristic">>}, optional}},
            {<<"languageCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"capability">>, {{bbelement, <<"DeviceDefinition.Capability">>}, list}},
            {<<"property">>, {{bbelement, <<"DeviceDefinition.Property">>}, list}},
            {<<"owner">>, {{complex, <<"Reference">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"onlineInformation">>, {{primitive, <<"uri">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"parentDevice">>, {{complex, <<"Reference">>}, optional}},
            {<<"material">>, {{bbelement, <<"DeviceDefinition.Material">>}, list}}
            ],
            [],
            [
            {<<"manufacturerString">>, <<"manufacturerReference">>}
            ]
} 
%%
%% DeviceDefinition.UdiDeviceIdentifier
%% The characteristics, operational status and capabilities of a medical-related component of a medical device.
%%
    , <<"DeviceDefinition.UdiDeviceIdentifier">> => {<<"BackboneElement">>,
            [
            {<<"deviceIdentifier">>, {{primitive, <<"string">>}, required}},
            {<<"issuer">>, {{primitive, <<"uri">>}, required}},
            {<<"jurisdiction">>, {{primitive, <<"uri">>}, required}}
            ],
            [],
            []
} 
%%
%% DeviceDefinition.DeviceName
%% The characteristics, operational status and capabilities of a medical-related component of a medical device.
%%
    , <<"DeviceDefinition.DeviceName">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"type">>, {{code, <<"devicenametype_list">>}, required}}
            ],
            [],
            []
} 
%%
%% DeviceDefinition.Specialization
%% The characteristics, operational status and capabilities of a medical-related component of a medical device.
%%
    , <<"DeviceDefinition.Specialization">> => {<<"BackboneElement">>,
            [
            {<<"systemType">>, {{primitive, <<"string">>}, required}},
            {<<"version">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% DeviceDefinition.Capability
%% The characteristics, operational status and capabilities of a medical-related component of a medical device.
%%
    , <<"DeviceDefinition.Capability">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"description">>, {{complex, <<"CodeableConcept">>}, list}}
            ],
            [],
            []
} 
%%
%% DeviceDefinition.Property
%% The characteristics, operational status and capabilities of a medical-related component of a medical device.
%%
    , <<"DeviceDefinition.Property">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, list}},
            {<<"valueCode">>, {{complex, <<"CodeableConcept">>}, list}}
            ],
            [],
            []
} 
%%
%% DeviceDefinition.Material
%% The characteristics, operational status and capabilities of a medical-related component of a medical device.
%%
    , <<"DeviceDefinition.Material">> => {<<"BackboneElement">>,
            [
            {<<"substance">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"alternate">>, {{primitive, <<"boolean">>}, optional}},
            {<<"allergenicIndicator">>, {{primitive, <<"boolean">>}, optional}}
            ],
            [],
            []
} 
%%
%% DeviceMetric
%% Describes a measurement, calculation or setting capability of a medical device.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"DeviceMetric">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"unit">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"source">>, {{complex, <<"Reference">>}, optional}},
            {<<"parent">>, {{complex, <<"Reference">>}, optional}},
            {<<"operationalStatus">>, {{code, <<"devicemetricoperationalstatus_list">>}, optional}},
            {<<"color">>, {{code, <<"devicemetriccolor_list">>}, optional}},
            {<<"category">>, {{code, <<"devicemetriccategory_list">>}, required}},
            {<<"measurementPeriod">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"calibration">>, {{bbelement, <<"DeviceMetric.Calibration">>}, list}}
            ],
            [],
            []
} 
%%
%% DeviceMetric.Calibration
%% Describes a measurement, calculation or setting capability of a medical device.
%%
    , <<"DeviceMetric.Calibration">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{code, <<"devicemetriccalibrationtype_list">>}, optional}},
            {<<"state">>, {{code, <<"devicemetriccalibrationstate_list">>}, optional}},
            {<<"time">>, {{primitive, <<"instant">>}, optional}}
            ],
            [],
            []
} 
%%
%% DeviceRequest
%% Represents a request for a patient to employ a medical device. The device may be an implantable device, or an external assistive device, such as a walker.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"DeviceRequest">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"instantiatesCanonical">>, {{primitive, <<"canonical">>}, list}},
            {<<"instantiatesUri">>, {{primitive, <<"uri">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"priorRequest">>, {{complex, <<"Reference">>}, list}},
            {<<"groupIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"status">>, {{code, <<"requeststatus_list">>}, optional}},
            {<<"intent">>, {{code, <<"requestintent_list">>}, required}},
            {<<"priority">>, {{code, <<"requestpriority_list">>}, optional}},
            {<<"codeReference">>, {{complex, <<"Reference">>}, required}},
            {<<"codeCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"parameter">>, {{bbelement, <<"DeviceRequest.Parameter">>}, list}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"occurrenceDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"occurrencePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"occurrenceTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"authoredOn">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"requester">>, {{complex, <<"Reference">>}, optional}},
            {<<"performerType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"performer">>, {{complex, <<"Reference">>}, optional}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"insurance">>, {{complex, <<"Reference">>}, list}},
            {<<"supportingInfo">>, {{complex, <<"Reference">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"relevantHistory">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            [
            {<<"codeReference">>, <<"codeCodeableConcept">>}, 
            {<<"occurrenceDateTime">>, <<"occurrencePeriod">>, <<"occurrenceTiming">>}
            ]
} 
%%
%% DeviceRequest.Parameter
%% Represents a request for a patient to employ a medical device. The device may be an implantable device, or an external assistive device, such as a walker.
%%
    , <<"DeviceRequest.Parameter">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"valueCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"valueRange">>, {{complex, <<"Range">>}, optional}},
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, optional}}
            ],
            [],
            [
            {<<"valueCodeableConcept">>, <<"valueQuantity">>, <<"valueRange">>, <<"valueBoolean">>}
            ]
} 
%%
%% DeviceUseStatement
%% A record of a device being used by a patient where the record is the result of a report from the patient or another clinician.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"DeviceUseStatement">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"status">>, {{code, <<"deviceusestatementstatus_list">>}, required}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"derivedFrom">>, {{complex, <<"Reference">>}, list}},
            {<<"timingTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"timingPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"timingDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"recordedOn">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"source">>, {{complex, <<"Reference">>}, optional}},
            {<<"device">>, {{complex, <<"Reference">>}, required}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"bodySite">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            [
            {<<"timingTiming">>, <<"timingPeriod">>, <<"timingDateTime">>}
            ]
} 
%%
%% DiagnosticReport
%% The findings and interpretation of diagnostic  tests performed on patients, groups of patients, devices, and locations, and/or specimens derived from these. The report includes clinical context such as requesting and provider information, and some mix of atomic results, images, textual and coded interpretations, and formatted representation of diagnostic reports.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"DiagnosticReport">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"status">>, {{code, <<"diagnosticreportstatus_list">>}, required}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"effectiveDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"issued">>, {{primitive, <<"instant">>}, optional}},
            {<<"performer">>, {{complex, <<"Reference">>}, list}},
            {<<"resultsInterpreter">>, {{complex, <<"Reference">>}, list}},
            {<<"specimen">>, {{complex, <<"Reference">>}, list}},
            {<<"result">>, {{complex, <<"Reference">>}, list}},
            {<<"imagingStudy">>, {{complex, <<"Reference">>}, list}},
            {<<"media">>, {{bbelement, <<"DiagnosticReport.Media">>}, list}},
            {<<"conclusion">>, {{primitive, <<"string">>}, optional}},
            {<<"conclusionCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"presentedForm">>, {{complex, <<"Attachment">>}, list}}
            ],
            [],
            [
            {<<"effectiveDateTime">>, <<"effectivePeriod">>}
            ]
} 
%%
%% DiagnosticReport.Media
%% The findings and interpretation of diagnostic  tests performed on patients, groups of patients, devices, and locations, and/or specimens derived from these. The report includes clinical context such as requesting and provider information, and some mix of atomic results, images, textual and coded interpretations, and formatted representation of diagnostic reports.
%%
    , <<"DiagnosticReport.Media">> => {<<"BackboneElement">>,
            [
            {<<"comment">>, {{primitive, <<"string">>}, optional}},
            {<<"link">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% DocumentManifest
%% A collection of documents compiled for a purpose together with metadata that applies to the collection.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"DocumentManifest">> => {<<"DomainResource">>,
            [
            {<<"masterIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"documentreferencestatus_list">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"created">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"author">>, {{complex, <<"Reference">>}, list}},
            {<<"recipient">>, {{complex, <<"Reference">>}, list}},
            {<<"source">>, {{primitive, <<"uri">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"content">>, {{complex, <<"Reference">>}, non_empty_list}},
            {<<"related">>, {{bbelement, <<"DocumentManifest.Related">>}, list}}
            ],
            [],
            []
} 
%%
%% DocumentManifest.Related
%% A collection of documents compiled for a purpose together with metadata that applies to the collection.
%%
    , <<"DocumentManifest.Related">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"ref">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% DocumentReference
%% A reference to a document of any kind for any purpose. Provides metadata about the document so that the document can be discovered and managed. The scope of a document is any seralized object with a mime-type, so includes formal patient centric documents (CDA), cliical notes, scanned paper, and non-patient specific documents like policy text.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"DocumentReference">> => {<<"DomainResource">>,
            [
            {<<"masterIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"documentreferencestatus_list">>}, required}},
            {<<"docStatus">>, {{code, <<"compositionstatus_list">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"date">>, {{primitive, <<"instant">>}, optional}},
            {<<"author">>, {{complex, <<"Reference">>}, list}},
            {<<"authenticator">>, {{complex, <<"Reference">>}, optional}},
            {<<"custodian">>, {{complex, <<"Reference">>}, optional}},
            {<<"relatesTo">>, {{bbelement, <<"DocumentReference.RelatesTo">>}, list}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"securityLabel">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"content">>, {{bbelement, <<"DocumentReference.Content">>}, non_empty_list}},
            {<<"context">>, {{bbelement, <<"DocumentReference.Context">>}, optional}}
            ],
            [],
            []
} 
%%
%% DocumentReference.RelatesTo
%% A reference to a document of any kind for any purpose. Provides metadata about the document so that the document can be discovered and managed. The scope of a document is any seralized object with a mime-type, so includes formal patient centric documents (CDA), cliical notes, scanned paper, and non-patient specific documents like policy text.
%%
    , <<"DocumentReference.RelatesTo">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{code, <<"documentrelationshiptype_list">>}, required}},
            {<<"target">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% DocumentReference.Content
%% A reference to a document of any kind for any purpose. Provides metadata about the document so that the document can be discovered and managed. The scope of a document is any seralized object with a mime-type, so includes formal patient centric documents (CDA), cliical notes, scanned paper, and non-patient specific documents like policy text.
%%
    , <<"DocumentReference.Content">> => {<<"BackboneElement">>,
            [
            {<<"attachment">>, {{complex, <<"Attachment">>}, required}},
            {<<"format">>, {{complex, <<"Coding">>}, optional}}
            ],
            [],
            []
} 
%%
%% DocumentReference.Context
%% A reference to a document of any kind for any purpose. Provides metadata about the document so that the document can be discovered and managed. The scope of a document is any seralized object with a mime-type, so includes formal patient centric documents (CDA), cliical notes, scanned paper, and non-patient specific documents like policy text.
%%
    , <<"DocumentReference.Context">> => {<<"BackboneElement">>,
            [
            {<<"encounter">>, {{complex, <<"Reference">>}, list}},
            {<<"event">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"facilityType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"practiceSetting">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"sourcePatientInfo">>, {{complex, <<"Reference">>}, optional}},
            {<<"related">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% EffectEvidenceSynthesis
%% The EffectEvidenceSynthesis resource describes the difference in an outcome between exposures states in a population where the effect estimate is derived from a combination of research studies.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"EffectEvidenceSynthesis">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"approvalDate">>, {{primitive, <<"date">>}, optional}},
            {<<"lastReviewDate">>, {{primitive, <<"date">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"topic">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"author">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"editor">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"reviewer">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"endorser">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"relatedArtifact">>, {{complex, <<"RelatedArtifact">>}, list}},
            {<<"synthesisType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"studyType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"population">>, {{complex, <<"Reference">>}, required}},
            {<<"exposure">>, {{complex, <<"Reference">>}, required}},
            {<<"exposureAlternative">>, {{complex, <<"Reference">>}, required}},
            {<<"outcome">>, {{complex, <<"Reference">>}, required}},
            {<<"sampleSize">>, {{bbelement, <<"EffectEvidenceSynthesis.SampleSize">>}, optional}},
            {<<"resultsByExposure">>, {{bbelement, <<"EffectEvidenceSynthesis.ResultsByExposure">>}, list}},
            {<<"effectEstimate">>, {{bbelement, <<"EffectEvidenceSynthesis.EffectEstimate">>}, list}},
            {<<"certainty">>, {{bbelement, <<"EffectEvidenceSynthesis.Certainty">>}, list}}
            ],
            [],
            []
} 
%%
%% EffectEvidenceSynthesis.SampleSize
%% The EffectEvidenceSynthesis resource describes the difference in an outcome between exposures states in a population where the effect estimate is derived from a combination of research studies.
%%
    , <<"EffectEvidenceSynthesis.SampleSize">> => {<<"BackboneElement">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"numberOfStudies">>, {{primitive, <<"integer">>}, optional}},
            {<<"numberOfParticipants">>, {{primitive, <<"integer">>}, optional}}
            ],
            [],
            []
} 
%%
%% EffectEvidenceSynthesis.ResultsByExposure
%% The EffectEvidenceSynthesis resource describes the difference in an outcome between exposures states in a population where the effect estimate is derived from a combination of research studies.
%%
    , <<"EffectEvidenceSynthesis.ResultsByExposure">> => {<<"BackboneElement">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"exposureState">>, {{code, <<"exposurestate_list">>}, optional}},
            {<<"variantState">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"riskEvidenceSynthesis">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% EffectEvidenceSynthesis.EffectEstimate
%% The EffectEvidenceSynthesis resource describes the difference in an outcome between exposures states in a population where the effect estimate is derived from a combination of research studies.
%%
    , <<"EffectEvidenceSynthesis.EffectEstimate">> => {<<"BackboneElement">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"variantState">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"value">>, {{primitive, <<"decimal">>}, optional}},
            {<<"unitOfMeasure">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"precisionEstimate">>, {{bbelement, <<"EffectEvidenceSynthesis.PrecisionEstimate">>}, list}}
            ],
            [],
            []
} 
%%
%% EffectEvidenceSynthesis.PrecisionEstimate
%% The EffectEvidenceSynthesis resource describes the difference in an outcome between exposures states in a population where the effect estimate is derived from a combination of research studies.
%%
    , <<"EffectEvidenceSynthesis.PrecisionEstimate">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"level">>, {{primitive, <<"decimal">>}, optional}},
            {<<"from">>, {{primitive, <<"decimal">>}, optional}},
            {<<"to">>, {{primitive, <<"decimal">>}, optional}}
            ],
            [],
            []
} 
%%
%% EffectEvidenceSynthesis.Certainty
%% The EffectEvidenceSynthesis resource describes the difference in an outcome between exposures states in a population where the effect estimate is derived from a combination of research studies.
%%
    , <<"EffectEvidenceSynthesis.Certainty">> => {<<"BackboneElement">>,
            [
            {<<"rating">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"certaintySubcomponent">>, {{bbelement, <<"EffectEvidenceSynthesis.CertaintySubcomponent">>}, list}}
            ],
            [],
            []
} 
%%
%% EffectEvidenceSynthesis.CertaintySubcomponent
%% The EffectEvidenceSynthesis resource describes the difference in an outcome between exposures states in a population where the effect estimate is derived from a combination of research studies.
%%
    , <<"EffectEvidenceSynthesis.CertaintySubcomponent">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"rating">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            []
} 
%%
%% Encounter
%% An interaction between a patient and healthcare provider(s) for the purpose of providing healthcare service(s) or assessing the health status of a patient.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Encounter">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"encounterstatus_list">>}, required}},
            {<<"statusHistory">>, {{bbelement, <<"Encounter.StatusHistory">>}, list}},
            {<<"class">>, {{complex, <<"Coding">>}, required}},
            {<<"classHistory">>, {{bbelement, <<"Encounter.ClassHistory">>}, list}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"serviceType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"priority">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"episodeOfCare">>, {{complex, <<"Reference">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"participant">>, {{bbelement, <<"Encounter.Participant">>}, list}},
            {<<"appointment">>, {{complex, <<"Reference">>}, list}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"length">>, {{complex, <<"Duration">>}, optional}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"diagnosis">>, {{bbelement, <<"Encounter.Diagnosis">>}, list}},
            {<<"account">>, {{complex, <<"Reference">>}, list}},
            {<<"hospitalization">>, {{bbelement, <<"Encounter.Hospitalization">>}, optional}},
            {<<"location">>, {{bbelement, <<"Encounter.Location">>}, list}},
            {<<"serviceProvider">>, {{complex, <<"Reference">>}, optional}},
            {<<"partOf">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% Encounter.StatusHistory
%% An interaction between a patient and healthcare provider(s) for the purpose of providing healthcare service(s) or assessing the health status of a patient.
%%
    , <<"Encounter.StatusHistory">> => {<<"BackboneElement">>,
            [
            {<<"status">>, {{code, <<"encounterstatus_list">>}, required}},
            {<<"period">>, {{complex, <<"Period">>}, required}}
            ],
            [],
            []
} 
%%
%% Encounter.ClassHistory
%% An interaction between a patient and healthcare provider(s) for the purpose of providing healthcare service(s) or assessing the health status of a patient.
%%
    , <<"Encounter.ClassHistory">> => {<<"BackboneElement">>,
            [
            {<<"class">>, {{complex, <<"Coding">>}, required}},
            {<<"period">>, {{complex, <<"Period">>}, required}}
            ],
            [],
            []
} 
%%
%% Encounter.Participant
%% An interaction between a patient and healthcare provider(s) for the purpose of providing healthcare service(s) or assessing the health status of a patient.
%%
    , <<"Encounter.Participant">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"individual">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% Encounter.Diagnosis
%% An interaction between a patient and healthcare provider(s) for the purpose of providing healthcare service(s) or assessing the health status of a patient.
%%
    , <<"Encounter.Diagnosis">> => {<<"BackboneElement">>,
            [
            {<<"condition">>, {{complex, <<"Reference">>}, required}},
            {<<"use">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"rank">>, {{primitive, <<"positiveInt">>}, optional}}
            ],
            [],
            []
} 
%%
%% Encounter.Hospitalization
%% An interaction between a patient and healthcare provider(s) for the purpose of providing healthcare service(s) or assessing the health status of a patient.
%%
    , <<"Encounter.Hospitalization">> => {<<"BackboneElement">>,
            [
            {<<"preAdmissionIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"origin">>, {{complex, <<"Reference">>}, optional}},
            {<<"admitSource">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"reAdmission">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"dietPreference">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"specialCourtesy">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"specialArrangement">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"destination">>, {{complex, <<"Reference">>}, optional}},
            {<<"dischargeDisposition">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% Encounter.Location
%% An interaction between a patient and healthcare provider(s) for the purpose of providing healthcare service(s) or assessing the health status of a patient.
%%
    , <<"Encounter.Location">> => {<<"BackboneElement">>,
            [
            {<<"location">>, {{complex, <<"Reference">>}, required}},
            {<<"status">>, {{code, <<"encounterlocationstatus_list">>}, optional}},
            {<<"physicalType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            []
} 
%%
%% Endpoint
%% The technical details of an endpoint that can be used for electronic services, such as for web services providing XDS.b or a REST endpoint for another FHIR server. This may include any security context information.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Endpoint">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"endpointstatus_list">>}, required}},
            {<<"connectionType">>, {{complex, <<"Coding">>}, required}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"managingOrganization">>, {{complex, <<"Reference">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"payloadType">>, {{complex, <<"CodeableConcept">>}, non_empty_list}},
            {<<"payloadMimeType">>, {{primitive, <<"code">>}, list}},
            {<<"address">>, {{primitive, <<"url">>}, required}},
            {<<"header">>, {{primitive, <<"string">>}, list}}
            ],
            [],
            []
} 
%%
%% EnrollmentRequest
%% This resource provides the insurance enrollment details to the insurer regarding a specified coverage.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"EnrollmentRequest">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"financialresourcestatuscodes_list">>}, optional}},
            {<<"created">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"insurer">>, {{complex, <<"Reference">>}, optional}},
            {<<"provider">>, {{complex, <<"Reference">>}, optional}},
            {<<"candidate">>, {{complex, <<"Reference">>}, optional}},
            {<<"coverage">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% EnrollmentResponse
%% This resource provides enrollment and plan details from the processing of an EnrollmentRequest resource.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"EnrollmentResponse">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"financialresourcestatuscodes_list">>}, optional}},
            {<<"request">>, {{complex, <<"Reference">>}, optional}},
            {<<"outcome">>, {{code, <<"remittanceoutcome_list">>}, optional}},
            {<<"disposition">>, {{primitive, <<"string">>}, optional}},
            {<<"created">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"organization">>, {{complex, <<"Reference">>}, optional}},
            {<<"requestProvider">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% EpisodeOfCare
%% An association between a patient and an organization / healthcare provider(s) during which time encounters may occur. The managing organization assumes a level of responsibility for the patient during this time.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"EpisodeOfCare">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"episodeofcarestatus_list">>}, required}},
            {<<"statusHistory">>, {{bbelement, <<"EpisodeOfCare.StatusHistory">>}, list}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"diagnosis">>, {{bbelement, <<"EpisodeOfCare.Diagnosis">>}, list}},
            {<<"patient">>, {{complex, <<"Reference">>}, required}},
            {<<"managingOrganization">>, {{complex, <<"Reference">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"referralRequest">>, {{complex, <<"Reference">>}, list}},
            {<<"careManager">>, {{complex, <<"Reference">>}, optional}},
            {<<"team">>, {{complex, <<"Reference">>}, list}},
            {<<"account">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% EpisodeOfCare.StatusHistory
%% An association between a patient and an organization / healthcare provider(s) during which time encounters may occur. The managing organization assumes a level of responsibility for the patient during this time.
%%
    , <<"EpisodeOfCare.StatusHistory">> => {<<"BackboneElement">>,
            [
            {<<"status">>, {{code, <<"episodeofcarestatus_list">>}, required}},
            {<<"period">>, {{complex, <<"Period">>}, required}}
            ],
            [],
            []
} 
%%
%% EpisodeOfCare.Diagnosis
%% An association between a patient and an organization / healthcare provider(s) during which time encounters may occur. The managing organization assumes a level of responsibility for the patient during this time.
%%
    , <<"EpisodeOfCare.Diagnosis">> => {<<"BackboneElement">>,
            [
            {<<"condition">>, {{complex, <<"Reference">>}, required}},
            {<<"role">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"rank">>, {{primitive, <<"positiveInt">>}, optional}}
            ],
            [],
            []
} 
%%
%% EventDefinition
%% The EventDefinition resource provides a reusable description of when a particular event can occur.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"EventDefinition">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"subtitle">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"subjectCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subjectReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"usage">>, {{primitive, <<"string">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"approvalDate">>, {{primitive, <<"date">>}, optional}},
            {<<"lastReviewDate">>, {{primitive, <<"date">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"topic">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"author">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"editor">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"reviewer">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"endorser">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"relatedArtifact">>, {{complex, <<"RelatedArtifact">>}, list}},
            {<<"trigger">>, {{complex, <<"TriggerDefinition">>}, non_empty_list}}
            ],
            [],
            [
            {<<"subjectCodeableConcept">>, <<"subjectReference">>}
            ]
} 
%%
%% Evidence
%% The Evidence resource describes the conditional state (population and any exposures being compared within the population) and outcome (if specified) that the knowledge (evidence, assertion, recommendation) is about.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Evidence">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"shortTitle">>, {{primitive, <<"string">>}, optional}},
            {<<"subtitle">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"approvalDate">>, {{primitive, <<"date">>}, optional}},
            {<<"lastReviewDate">>, {{primitive, <<"date">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"topic">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"author">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"editor">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"reviewer">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"endorser">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"relatedArtifact">>, {{complex, <<"RelatedArtifact">>}, list}},
            {<<"exposureBackground">>, {{complex, <<"Reference">>}, required}},
            {<<"exposureVariant">>, {{complex, <<"Reference">>}, list}},
            {<<"outcome">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% EvidenceVariable
%% The EvidenceVariable resource describes a "PICO" element that knowledge (evidence, assertion, recommendation) is about.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"EvidenceVariable">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"shortTitle">>, {{primitive, <<"string">>}, optional}},
            {<<"subtitle">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"approvalDate">>, {{primitive, <<"date">>}, optional}},
            {<<"lastReviewDate">>, {{primitive, <<"date">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"topic">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"author">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"editor">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"reviewer">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"endorser">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"relatedArtifact">>, {{complex, <<"RelatedArtifact">>}, list}},
            {<<"type">>, {{code, <<"evidencevariabletype_list">>}, optional}},
            {<<"characteristic">>, {{bbelement, <<"EvidenceVariable.Characteristic">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% EvidenceVariable.Characteristic
%% The EvidenceVariable resource describes a "PICO" element that knowledge (evidence, assertion, recommendation) is about.
%%
    , <<"EvidenceVariable.Characteristic">> => {<<"BackboneElement">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"definitionReference">>, {{complex, <<"Reference">>}, required}},
            {<<"definitionCanonical">>, {{primitive, <<"canonical">>}, required}},
            {<<"definitionCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"definitionExpression">>, {{complex, <<"Expression">>}, required}},
            {<<"definitionDataRequirement">>, {{complex, <<"DataRequirement">>}, required}},
            {<<"definitionTriggerDefinition">>, {{complex, <<"TriggerDefinition">>}, required}},
            {<<"usageContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"exclude">>, {{primitive, <<"boolean">>}, optional}},
            {<<"participantEffectiveDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"participantEffectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"participantEffectiveDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"participantEffectiveTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"timeFromStart">>, {{complex, <<"Duration">>}, optional}},
            {<<"groupMeasure">>, {{code, <<"groupmeasure_list">>}, optional}}
            ],
            [],
            [
            {<<"definitionReference">>, <<"definitionCanonical">>, <<"definitionCodeableConcept">>, <<"definitionExpression">>, <<"definitionDataRequirement">>, <<"definitionTriggerDefinition">>}, 
            {<<"participantEffectiveDateTime">>, <<"participantEffectivePeriod">>, <<"participantEffectiveDuration">>, <<"participantEffectiveTiming">>}
            ]
} 
%%
%% ExampleScenario
%% Example of workflow instance.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ExampleScenario">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"actor">>, {{bbelement, <<"ExampleScenario.Actor">>}, list}},
            {<<"instance">>, {{bbelement, <<"ExampleScenario.Instance">>}, list}},
            {<<"process">>, {{bbelement, <<"ExampleScenario.Process">>}, list}},
            {<<"workflow">>, {{primitive, <<"canonical">>}, list}}
            ],
            [],
            []
} 
%%
%% ExampleScenario.Actor
%% Example of workflow instance.
%%
    , <<"ExampleScenario.Actor">> => {<<"BackboneElement">>,
            [
            {<<"actorId">>, {{primitive, <<"string">>}, required}},
            {<<"type">>, {{code, <<"examplescenarioactortype_list">>}, required}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}}
            ],
            [],
            []
} 
%%
%% ExampleScenario.Instance
%% Example of workflow instance.
%%
    , <<"ExampleScenario.Instance">> => {<<"BackboneElement">>,
            [
            {<<"resourceId">>, {{primitive, <<"string">>}, required}},
            {<<"resourceType">>, {{code, <<"resourcetype_list">>}, required}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"version">>, {{bbelement, <<"ExampleScenario.Version">>}, list}},
            {<<"containedInstance">>, {{bbelement, <<"ExampleScenario.ContainedInstance">>}, list}}
            ],
            [],
            []
} 
%%
%% ExampleScenario.Version
%% Example of workflow instance.
%%
    , <<"ExampleScenario.Version">> => {<<"BackboneElement">>,
            [
            {<<"versionId">>, {{primitive, <<"string">>}, required}},
            {<<"description">>, {{primitive, <<"markdown">>}, required}}
            ],
            [],
            []
} 
%%
%% ExampleScenario.ContainedInstance
%% Example of workflow instance.
%%
    , <<"ExampleScenario.ContainedInstance">> => {<<"BackboneElement">>,
            [
            {<<"resourceId">>, {{primitive, <<"string">>}, required}},
            {<<"versionId">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% ExampleScenario.Process
%% Example of workflow instance.
%%
    , <<"ExampleScenario.Process">> => {<<"BackboneElement">>,
            [
            {<<"title">>, {{primitive, <<"string">>}, required}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"preConditions">>, {{primitive, <<"markdown">>}, optional}},
            {<<"postConditions">>, {{primitive, <<"markdown">>}, optional}},
            {<<"step">>, {{bbelement, <<"ExampleScenario.Step">>}, list}}
            ],
            [],
            []
} 
%%
%% ExampleScenario.Step
%% Example of workflow instance.
%%
    , <<"ExampleScenario.Step">> => {<<"BackboneElement">>,
            [
            {<<"process">>, {{bbelement, <<"ExampleScenario.Process">>}, list}},
            {<<"pause">>, {{primitive, <<"boolean">>}, optional}},
            {<<"operation">>, {{bbelement, <<"ExampleScenario.Operation">>}, optional}},
            {<<"alternative">>, {{bbelement, <<"ExampleScenario.Alternative">>}, list}}
            ],
            [],
            []
} 
%%
%% ExampleScenario.Operation
%% Example of workflow instance.
%%
    , <<"ExampleScenario.Operation">> => {<<"BackboneElement">>,
            [
            {<<"number">>, {{primitive, <<"string">>}, required}},
            {<<"type">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"initiator">>, {{primitive, <<"string">>}, optional}},
            {<<"receiver">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"initiatorActive">>, {{primitive, <<"boolean">>}, optional}},
            {<<"receiverActive">>, {{primitive, <<"boolean">>}, optional}},
            {<<"request">>, {{bbelement, <<"ExampleScenario.ContainedInstance">>}, optional}},
            {<<"response">>, {{bbelement, <<"ExampleScenario.ContainedInstance">>}, optional}}
            ],
            [],
            []
} 
%%
%% ExampleScenario.Alternative
%% Example of workflow instance.
%%
    , <<"ExampleScenario.Alternative">> => {<<"BackboneElement">>,
            [
            {<<"title">>, {{primitive, <<"string">>}, required}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"step">>, {{bbelement, <<"ExampleScenario.Step">>}, list}}
            ],
            [],
            []
} 
%%
%% ExplanationOfBenefit
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ExplanationOfBenefit">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"explanationofbenefitstatus_list">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"subType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"use">>, {{code, <<"use_list">>}, required}},
            {<<"patient">>, {{complex, <<"Reference">>}, required}},
            {<<"billablePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"created">>, {{primitive, <<"dateTime">>}, required}},
            {<<"enterer">>, {{complex, <<"Reference">>}, optional}},
            {<<"insurer">>, {{complex, <<"Reference">>}, required}},
            {<<"provider">>, {{complex, <<"Reference">>}, required}},
            {<<"priority">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"fundsReserveRequested">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"fundsReserve">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"related">>, {{bbelement, <<"ExplanationOfBenefit.Related">>}, list}},
            {<<"prescription">>, {{complex, <<"Reference">>}, optional}},
            {<<"originalPrescription">>, {{complex, <<"Reference">>}, optional}},
            {<<"payee">>, {{bbelement, <<"ExplanationOfBenefit.Payee">>}, optional}},
            {<<"referral">>, {{complex, <<"Reference">>}, optional}},
            {<<"facility">>, {{complex, <<"Reference">>}, optional}},
            {<<"claim">>, {{complex, <<"Reference">>}, optional}},
            {<<"claimResponse">>, {{complex, <<"Reference">>}, optional}},
            {<<"outcome">>, {{code, <<"claimprocessingcodes_list">>}, required}},
            {<<"disposition">>, {{primitive, <<"string">>}, optional}},
            {<<"preAuthRef">>, {{primitive, <<"string">>}, list}},
            {<<"preAuthRefPeriod">>, {{complex, <<"Period">>}, list}},
            {<<"careTeam">>, {{bbelement, <<"ExplanationOfBenefit.CareTeam">>}, list}},
            {<<"supportingInfo">>, {{bbelement, <<"ExplanationOfBenefit.SupportingInfo">>}, list}},
            {<<"diagnosis">>, {{bbelement, <<"ExplanationOfBenefit.Diagnosis">>}, list}},
            {<<"procedure">>, {{bbelement, <<"ExplanationOfBenefit.Procedure">>}, list}},
            {<<"precedence">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"insurance">>, {{bbelement, <<"ExplanationOfBenefit.Insurance">>}, non_empty_list}},
            {<<"accident">>, {{bbelement, <<"ExplanationOfBenefit.Accident">>}, optional}},
            {<<"item">>, {{bbelement, <<"ExplanationOfBenefit.Item">>}, list}},
            {<<"addItem">>, {{bbelement, <<"ExplanationOfBenefit.AddItem">>}, list}},
            {<<"adjudication">>, {{bbelement, <<"ExplanationOfBenefit.Adjudication">>}, list}},
            {<<"total">>, {{bbelement, <<"ExplanationOfBenefit.Total">>}, list}},
            {<<"payment">>, {{bbelement, <<"ExplanationOfBenefit.Payment">>}, optional}},
            {<<"formCode">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"form">>, {{complex, <<"Attachment">>}, optional}},
            {<<"processNote">>, {{bbelement, <<"ExplanationOfBenefit.ProcessNote">>}, list}},
            {<<"benefitPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"benefitBalance">>, {{bbelement, <<"ExplanationOfBenefit.BenefitBalance">>}, list}}
            ],
            [],
            []
} 
%%
%% ExplanationOfBenefit.Related
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.Related">> => {<<"BackboneElement">>,
            [
            {<<"claim">>, {{complex, <<"Reference">>}, optional}},
            {<<"relationship">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"reference">>, {{complex, <<"Identifier">>}, optional}}
            ],
            [],
            []
} 
%%
%% ExplanationOfBenefit.Payee
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.Payee">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"party">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% ExplanationOfBenefit.CareTeam
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.CareTeam">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"provider">>, {{complex, <<"Reference">>}, required}},
            {<<"responsible">>, {{primitive, <<"boolean">>}, optional}},
            {<<"role">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"qualification">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% ExplanationOfBenefit.SupportingInfo
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.SupportingInfo">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"timingDate">>, {{primitive, <<"date">>}, optional}},
            {<<"timingPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"valueString">>, {{primitive, <<"string">>}, optional}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"valueAttachment">>, {{complex, <<"Attachment">>}, optional}},
            {<<"valueReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"reason">>, {{complex, <<"Coding">>}, optional}}
            ],
            [],
            [
            {<<"timingDate">>, <<"timingPeriod">>}, 
            {<<"valueBoolean">>, <<"valueString">>, <<"valueQuantity">>, <<"valueAttachment">>, <<"valueReference">>}
            ]
} 
%%
%% ExplanationOfBenefit.Diagnosis
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.Diagnosis">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"diagnosisCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"diagnosisReference">>, {{complex, <<"Reference">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"onAdmission">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"packageCode">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            [
            {<<"diagnosisCodeableConcept">>, <<"diagnosisReference">>}
            ]
} 
%%
%% ExplanationOfBenefit.Procedure
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.Procedure">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"procedureCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"procedureReference">>, {{complex, <<"Reference">>}, required}},
            {<<"udi">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            [
            {<<"procedureCodeableConcept">>, <<"procedureReference">>}
            ]
} 
%%
%% ExplanationOfBenefit.Insurance
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.Insurance">> => {<<"BackboneElement">>,
            [
            {<<"focal">>, {{primitive, <<"boolean">>}, required}},
            {<<"coverage">>, {{complex, <<"Reference">>}, required}},
            {<<"preAuthRef">>, {{primitive, <<"string">>}, list}}
            ],
            [],
            []
} 
%%
%% ExplanationOfBenefit.Accident
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.Accident">> => {<<"BackboneElement">>,
            [
            {<<"date">>, {{primitive, <<"date">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"locationAddress">>, {{complex, <<"Address">>}, optional}},
            {<<"locationReference">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            [
            {<<"locationAddress">>, <<"locationReference">>}
            ]
} 
%%
%% ExplanationOfBenefit.Item
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.Item">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"careTeamSequence">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"diagnosisSequence">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"procedureSequence">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"informationSequence">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"revenue">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"productOrService">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"programCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"servicedDate">>, {{primitive, <<"date">>}, optional}},
            {<<"servicedPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"locationCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"locationAddress">>, {{complex, <<"Address">>}, optional}},
            {<<"locationReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"unitPrice">>, {{complex, <<"Money">>}, optional}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"net">>, {{complex, <<"Money">>}, optional}},
            {<<"udi">>, {{complex, <<"Reference">>}, list}},
            {<<"bodySite">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subSite">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"encounter">>, {{complex, <<"Reference">>}, list}},
            {<<"noteNumber">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"adjudication">>, {{bbelement, <<"ExplanationOfBenefit.Adjudication">>}, list}},
            {<<"detail">>, {{bbelement, <<"ExplanationOfBenefit.Detail">>}, list}}
            ],
            [],
            [
            {<<"servicedDate">>, <<"servicedPeriod">>}, 
            {<<"locationCodeableConcept">>, <<"locationAddress">>, <<"locationReference">>}
            ]
} 
%%
%% ExplanationOfBenefit.Adjudication
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.Adjudication">> => {<<"BackboneElement">>,
            [
            {<<"category">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"reason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"amount">>, {{complex, <<"Money">>}, optional}},
            {<<"value">>, {{primitive, <<"decimal">>}, optional}}
            ],
            [],
            []
} 
%%
%% ExplanationOfBenefit.Detail
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.Detail">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"revenue">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"productOrService">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"programCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"unitPrice">>, {{complex, <<"Money">>}, optional}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"net">>, {{complex, <<"Money">>}, optional}},
            {<<"udi">>, {{complex, <<"Reference">>}, list}},
            {<<"noteNumber">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"adjudication">>, {{bbelement, <<"ExplanationOfBenefit.Adjudication">>}, list}},
            {<<"subDetail">>, {{bbelement, <<"ExplanationOfBenefit.SubDetail">>}, list}}
            ],
            [],
            []
} 
%%
%% ExplanationOfBenefit.SubDetail
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.SubDetail">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"revenue">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"productOrService">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"programCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"unitPrice">>, {{complex, <<"Money">>}, optional}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"net">>, {{complex, <<"Money">>}, optional}},
            {<<"udi">>, {{complex, <<"Reference">>}, list}},
            {<<"noteNumber">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"adjudication">>, {{bbelement, <<"ExplanationOfBenefit.Adjudication">>}, list}}
            ],
            [],
            []
} 
%%
%% ExplanationOfBenefit.AddItem
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.AddItem">> => {<<"BackboneElement">>,
            [
            {<<"itemSequence">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"detailSequence">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"subDetailSequence">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"provider">>, {{complex, <<"Reference">>}, list}},
            {<<"productOrService">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"programCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"servicedDate">>, {{primitive, <<"date">>}, optional}},
            {<<"servicedPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"locationCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"locationAddress">>, {{complex, <<"Address">>}, optional}},
            {<<"locationReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"unitPrice">>, {{complex, <<"Money">>}, optional}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"net">>, {{complex, <<"Money">>}, optional}},
            {<<"bodySite">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subSite">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"noteNumber">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"adjudication">>, {{bbelement, <<"ExplanationOfBenefit.Adjudication">>}, list}},
            {<<"detail">>, {{bbelement, <<"ExplanationOfBenefit.Detail1">>}, list}}
            ],
            [],
            [
            {<<"servicedDate">>, <<"servicedPeriod">>}, 
            {<<"locationCodeableConcept">>, <<"locationAddress">>, <<"locationReference">>}
            ]
} 
%%
%% ExplanationOfBenefit.Detail1
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.Detail1">> => {<<"BackboneElement">>,
            [
            {<<"productOrService">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"unitPrice">>, {{complex, <<"Money">>}, optional}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"net">>, {{complex, <<"Money">>}, optional}},
            {<<"noteNumber">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"adjudication">>, {{bbelement, <<"ExplanationOfBenefit.Adjudication">>}, list}},
            {<<"subDetail">>, {{bbelement, <<"ExplanationOfBenefit.SubDetail1">>}, list}}
            ],
            [],
            []
} 
%%
%% ExplanationOfBenefit.SubDetail1
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.SubDetail1">> => {<<"BackboneElement">>,
            [
            {<<"productOrService">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"unitPrice">>, {{complex, <<"Money">>}, optional}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"net">>, {{complex, <<"Money">>}, optional}},
            {<<"noteNumber">>, {{primitive, <<"positiveInt">>}, list}},
            {<<"adjudication">>, {{bbelement, <<"ExplanationOfBenefit.Adjudication">>}, list}}
            ],
            [],
            []
} 
%%
%% ExplanationOfBenefit.Total
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.Total">> => {<<"BackboneElement">>,
            [
            {<<"category">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"amount">>, {{complex, <<"Money">>}, required}}
            ],
            [],
            []
} 
%%
%% ExplanationOfBenefit.Payment
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.Payment">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"adjustment">>, {{complex, <<"Money">>}, optional}},
            {<<"adjustmentReason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"date">>, {{primitive, <<"date">>}, optional}},
            {<<"amount">>, {{complex, <<"Money">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}}
            ],
            [],
            []
} 
%%
%% ExplanationOfBenefit.ProcessNote
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.ProcessNote">> => {<<"BackboneElement">>,
            [
            {<<"number">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"type">>, {{code, <<"notetype_list">>}, optional}},
            {<<"text">>, {{primitive, <<"string">>}, optional}},
            {<<"language">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% ExplanationOfBenefit.BenefitBalance
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.BenefitBalance">> => {<<"BackboneElement">>,
            [
            {<<"category">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"excluded">>, {{primitive, <<"boolean">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"network">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"unit">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"term">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"financial">>, {{bbelement, <<"ExplanationOfBenefit.Financial">>}, list}}
            ],
            [],
            []
} 
%%
%% ExplanationOfBenefit.Financial
%% This resource provides: the claim details; adjudication details from the processing of a Claim; and optionally account balance information, for informing the subscriber of the benefits provided.
%%
    , <<"ExplanationOfBenefit.Financial">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"allowedUnsignedInt">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"allowedString">>, {{primitive, <<"string">>}, optional}},
            {<<"allowedMoney">>, {{complex, <<"Money">>}, optional}},
            {<<"usedUnsignedInt">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"usedMoney">>, {{complex, <<"Money">>}, optional}}
            ],
            [],
            [
            {<<"allowedUnsignedInt">>, <<"allowedString">>, <<"allowedMoney">>}, 
            {<<"usedUnsignedInt">>, <<"usedMoney">>}
            ]
} 
%%
%% FamilyMemberHistory
%% Significant health conditions for a person related to the patient relevant in the context of care for the patient.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"FamilyMemberHistory">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"instantiatesCanonical">>, {{primitive, <<"canonical">>}, list}},
            {<<"instantiatesUri">>, {{primitive, <<"uri">>}, list}},
            {<<"status">>, {{code, <<"familyhistorystatus_list">>}, required}},
            {<<"dataAbsentReason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"patient">>, {{complex, <<"Reference">>}, required}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"relationship">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"sex">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"bornPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"bornDate">>, {{primitive, <<"date">>}, optional}},
            {<<"bornString">>, {{primitive, <<"string">>}, optional}},
            {<<"ageAge">>, {{complex, <<"Age">>}, optional}},
            {<<"ageRange">>, {{complex, <<"Range">>}, optional}},
            {<<"ageString">>, {{primitive, <<"string">>}, optional}},
            {<<"estimatedAge">>, {{primitive, <<"boolean">>}, optional}},
            {<<"deceasedBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"deceasedAge">>, {{complex, <<"Age">>}, optional}},
            {<<"deceasedRange">>, {{complex, <<"Range">>}, optional}},
            {<<"deceasedDate">>, {{primitive, <<"date">>}, optional}},
            {<<"deceasedString">>, {{primitive, <<"string">>}, optional}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"condition">>, {{bbelement, <<"FamilyMemberHistory.Condition">>}, list}}
            ],
            [],
            [
            {<<"bornPeriod">>, <<"bornDate">>, <<"bornString">>}, 
            {<<"ageAge">>, <<"ageRange">>, <<"ageString">>}, 
            {<<"deceasedBoolean">>, <<"deceasedAge">>, <<"deceasedRange">>, <<"deceasedDate">>, <<"deceasedString">>}
            ]
} 
%%
%% FamilyMemberHistory.Condition
%% Significant health conditions for a person related to the patient relevant in the context of care for the patient.
%%
    , <<"FamilyMemberHistory.Condition">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"outcome">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"contributedToDeath">>, {{primitive, <<"boolean">>}, optional}},
            {<<"onsetAge">>, {{complex, <<"Age">>}, optional}},
            {<<"onsetRange">>, {{complex, <<"Range">>}, optional}},
            {<<"onsetPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"onsetString">>, {{primitive, <<"string">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            [
            {<<"onsetAge">>, <<"onsetRange">>, <<"onsetPeriod">>, <<"onsetString">>}
            ]
} 
%%
%% Flag
%% Prospective warnings of potential issues when providing care to the patient.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Flag">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"flagstatus_list">>}, required}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"author">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% Goal
%% Describes the intended objective(s) for a patient, group or organization care, for example, weight loss, restoring an activity of daily living, obtaining herd immunity via immunization, meeting a process improvement objective, etc.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Goal">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"lifecycleStatus">>, {{code, <<"goallifecyclestatus_list">>}, required}},
            {<<"achievementStatus">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"priority">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"description">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"startDate">>, {{primitive, <<"date">>}, optional}},
            {<<"startCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"target">>, {{bbelement, <<"Goal.Target">>}, list}},
            {<<"statusDate">>, {{primitive, <<"date">>}, optional}},
            {<<"statusReason">>, {{primitive, <<"string">>}, optional}},
            {<<"expressedBy">>, {{complex, <<"Reference">>}, optional}},
            {<<"addresses">>, {{complex, <<"Reference">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"outcomeCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"outcomeReference">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            [
            {<<"startDate">>, <<"startCodeableConcept">>}
            ]
} 
%%
%% Goal.Target
%% Describes the intended objective(s) for a patient, group or organization care, for example, weight loss, restoring an activity of daily living, obtaining herd immunity via immunization, meeting a process improvement objective, etc.
%%
    , <<"Goal.Target">> => {<<"BackboneElement">>,
            [
            {<<"measure">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"detailQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"detailRange">>, {{complex, <<"Range">>}, optional}},
            {<<"detailCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"detailString">>, {{primitive, <<"string">>}, optional}},
            {<<"detailBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"detailInteger">>, {{primitive, <<"integer">>}, optional}},
            {<<"detailRatio">>, {{complex, <<"Ratio">>}, optional}},
            {<<"dueDate">>, {{primitive, <<"date">>}, optional}},
            {<<"dueDuration">>, {{complex, <<"Duration">>}, optional}}
            ],
            [],
            [
            {<<"detailQuantity">>, <<"detailRange">>, <<"detailCodeableConcept">>, <<"detailString">>, <<"detailBoolean">>, <<"detailInteger">>, <<"detailRatio">>}, 
            {<<"dueDate">>, <<"dueDuration">>}
            ]
} 
%%
%% GraphDefinition
%% A formal computable definition of a graph of resources - that is, a coherent set of resources that form a graph by following references. The Graph Definition resource defines a set and makes rules about the set.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"GraphDefinition">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"start">>, {{primitive, <<"code">>}, required}},
            {<<"profile">>, {{primitive, <<"canonical">>}, optional}},
            {<<"link">>, {{bbelement, <<"GraphDefinition.Link">>}, list}}
            ],
            [],
            []
} 
%%
%% GraphDefinition.Link
%% A formal computable definition of a graph of resources - that is, a coherent set of resources that form a graph by following references. The Graph Definition resource defines a set and makes rules about the set.
%%
    , <<"GraphDefinition.Link">> => {<<"BackboneElement">>,
            [
            {<<"path">>, {{primitive, <<"string">>}, optional}},
            {<<"sliceName">>, {{primitive, <<"string">>}, optional}},
            {<<"min">>, {{primitive, <<"integer">>}, optional}},
            {<<"max">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"target">>, {{bbelement, <<"GraphDefinition.Target">>}, list}}
            ],
            [],
            []
} 
%%
%% GraphDefinition.Target
%% A formal computable definition of a graph of resources - that is, a coherent set of resources that form a graph by following references. The Graph Definition resource defines a set and makes rules about the set.
%%
    , <<"GraphDefinition.Target">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{primitive, <<"code">>}, required}},
            {<<"params">>, {{primitive, <<"string">>}, optional}},
            {<<"profile">>, {{primitive, <<"canonical">>}, optional}},
            {<<"compartment">>, {{bbelement, <<"GraphDefinition.Compartment">>}, list}},
            {<<"link">>, {{bbelement, <<"GraphDefinition.Link">>}, list}}
            ],
            [],
            []
} 
%%
%% GraphDefinition.Compartment
%% A formal computable definition of a graph of resources - that is, a coherent set of resources that form a graph by following references. The Graph Definition resource defines a set and makes rules about the set.
%%
    , <<"GraphDefinition.Compartment">> => {<<"BackboneElement">>,
            [
            {<<"use">>, {{code, <<"graphcompartmentuse_list">>}, required}},
            {<<"code">>, {{code, <<"compartmenttype_list">>}, required}},
            {<<"rule">>, {{code, <<"graphcompartmentrule_list">>}, required}},
            {<<"expression">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% Group
%% Represents a defined collection of entities that may be discussed or acted upon collectively but which are not expected to act collectively, and are not formally or legally recognized; i.e. a collection of entities that isn't an Organization.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Group">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"active">>, {{primitive, <<"boolean">>}, optional}},
            {<<"type">>, {{code, <<"grouptype_list">>}, required}},
            {<<"actual">>, {{primitive, <<"boolean">>}, required}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"quantity">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"managingEntity">>, {{complex, <<"Reference">>}, optional}},
            {<<"characteristic">>, {{bbelement, <<"Group.Characteristic">>}, list}},
            {<<"member">>, {{bbelement, <<"Group.Member">>}, list}}
            ],
            [],
            []
} 
%%
%% Group.Characteristic
%% Represents a defined collection of entities that may be discussed or acted upon collectively but which are not expected to act collectively, and are not formally or legally recognized; i.e. a collection of entities that isn't an Organization.
%%
    , <<"Group.Characteristic">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"valueCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, required}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, required}},
            {<<"valueRange">>, {{complex, <<"Range">>}, required}},
            {<<"valueReference">>, {{complex, <<"Reference">>}, required}},
            {<<"exclude">>, {{primitive, <<"boolean">>}, required}},
            {<<"period">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            [
            {<<"valueCodeableConcept">>, <<"valueBoolean">>, <<"valueQuantity">>, <<"valueRange">>, <<"valueReference">>}
            ]
} 
%%
%% Group.Member
%% Represents a defined collection of entities that may be discussed or acted upon collectively but which are not expected to act collectively, and are not formally or legally recognized; i.e. a collection of entities that isn't an Organization.
%%
    , <<"Group.Member">> => {<<"BackboneElement">>,
            [
            {<<"entity">>, {{complex, <<"Reference">>}, required}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"inactive">>, {{primitive, <<"boolean">>}, optional}}
            ],
            [],
            []
} 
%%
%% GuidanceResponse
%% A guidance response is the formal response to a guidance request, including any output parameters returned by the evaluation, as well as the description of any proposed actions to be taken.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"GuidanceResponse">> => {<<"DomainResource">>,
            [
            {<<"requestIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"moduleUri">>, {{primitive, <<"uri">>}, required}},
            {<<"moduleCanonical">>, {{primitive, <<"canonical">>}, required}},
            {<<"moduleCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"status">>, {{code, <<"guidanceresponsestatus_list">>}, required}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"occurrenceDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"performer">>, {{complex, <<"Reference">>}, optional}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"evaluationMessage">>, {{complex, <<"Reference">>}, list}},
            {<<"outputParameters">>, {{complex, <<"Reference">>}, optional}},
            {<<"result">>, {{complex, <<"Reference">>}, optional}},
            {<<"dataRequirement">>, {{complex, <<"DataRequirement">>}, list}}
            ],
            [],
            [
            {<<"moduleUri">>, <<"moduleCanonical">>, <<"moduleCodeableConcept">>}
            ]
} 
%%
%% HealthcareService
%% The details of a healthcare service available at a location.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"HealthcareService">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"active">>, {{primitive, <<"boolean">>}, optional}},
            {<<"providedBy">>, {{complex, <<"Reference">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"specialty">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"location">>, {{complex, <<"Reference">>}, list}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"comment">>, {{primitive, <<"string">>}, optional}},
            {<<"extraDetails">>, {{primitive, <<"markdown">>}, optional}},
            {<<"photo">>, {{complex, <<"Attachment">>}, optional}},
            {<<"telecom">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"coverageArea">>, {{complex, <<"Reference">>}, list}},
            {<<"serviceProvisionCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"eligibility">>, {{bbelement, <<"HealthcareService.Eligibility">>}, list}},
            {<<"program">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"characteristic">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"communication">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"referralMethod">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"appointmentRequired">>, {{primitive, <<"boolean">>}, optional}},
            {<<"availableTime">>, {{bbelement, <<"HealthcareService.AvailableTime">>}, list}},
            {<<"notAvailable">>, {{bbelement, <<"HealthcareService.NotAvailable">>}, list}},
            {<<"availabilityExceptions">>, {{primitive, <<"string">>}, optional}},
            {<<"endpoint">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% HealthcareService.Eligibility
%% The details of a healthcare service available at a location.
%%
    , <<"HealthcareService.Eligibility">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"comment">>, {{primitive, <<"markdown">>}, optional}}
            ],
            [],
            []
} 
%%
%% HealthcareService.AvailableTime
%% The details of a healthcare service available at a location.
%%
    , <<"HealthcareService.AvailableTime">> => {<<"BackboneElement">>,
            [
            {<<"daysOfWeek">>, {{code, <<"daysofweek_list">>}, list}},
            {<<"allDay">>, {{primitive, <<"boolean">>}, optional}},
            {<<"availableStartTime">>, {{primitive, <<"time">>}, optional}},
            {<<"availableEndTime">>, {{primitive, <<"time">>}, optional}}
            ],
            [],
            []
} 
%%
%% HealthcareService.NotAvailable
%% The details of a healthcare service available at a location.
%%
    , <<"HealthcareService.NotAvailable">> => {<<"BackboneElement">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, required}},
            {<<"during">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            []
} 
%%
%% ImagingStudy
%% Representation of the content produced in a DICOM imaging study. A study comprises a set of series, each of which includes a set of Service-Object Pair Instances (SOP Instances - images or other data) acquired or produced in a common context.  A series is of only one modality (e.g. X-ray, CT, MR, ultrasound), but a study may have multiple series of different modalities.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ImagingStudy">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"imagingstudystatus_list">>}, required}},
            {<<"modality">>, {{complex, <<"Coding">>}, list}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"started">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"referrer">>, {{complex, <<"Reference">>}, optional}},
            {<<"interpreter">>, {{complex, <<"Reference">>}, list}},
            {<<"endpoint">>, {{complex, <<"Reference">>}, list}},
            {<<"numberOfSeries">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"numberOfInstances">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"procedureReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"procedureCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"location">>, {{complex, <<"Reference">>}, optional}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"series">>, {{bbelement, <<"ImagingStudy.Series">>}, list}}
            ],
            [],
            []
} 
%%
%% ImagingStudy.Series
%% Representation of the content produced in a DICOM imaging study. A study comprises a set of series, each of which includes a set of Service-Object Pair Instances (SOP Instances - images or other data) acquired or produced in a common context.  A series is of only one modality (e.g. X-ray, CT, MR, ultrasound), but a study may have multiple series of different modalities.
%%
    , <<"ImagingStudy.Series">> => {<<"BackboneElement">>,
            [
            {<<"uid">>, {{primitive, <<"id">>}, required}},
            {<<"number">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"modality">>, {{complex, <<"Coding">>}, required}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"numberOfInstances">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"endpoint">>, {{complex, <<"Reference">>}, list}},
            {<<"bodySite">>, {{complex, <<"Coding">>}, optional}},
            {<<"laterality">>, {{complex, <<"Coding">>}, optional}},
            {<<"specimen">>, {{complex, <<"Reference">>}, list}},
            {<<"started">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"performer">>, {{bbelement, <<"ImagingStudy.Performer">>}, list}},
            {<<"instance">>, {{bbelement, <<"ImagingStudy.Instance">>}, list}}
            ],
            [],
            []
} 
%%
%% ImagingStudy.Performer
%% Representation of the content produced in a DICOM imaging study. A study comprises a set of series, each of which includes a set of Service-Object Pair Instances (SOP Instances - images or other data) acquired or produced in a common context.  A series is of only one modality (e.g. X-ray, CT, MR, ultrasound), but a study may have multiple series of different modalities.
%%
    , <<"ImagingStudy.Performer">> => {<<"BackboneElement">>,
            [
            {<<"function">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"actor">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% ImagingStudy.Instance
%% Representation of the content produced in a DICOM imaging study. A study comprises a set of series, each of which includes a set of Service-Object Pair Instances (SOP Instances - images or other data) acquired or produced in a common context.  A series is of only one modality (e.g. X-ray, CT, MR, ultrasound), but a study may have multiple series of different modalities.
%%
    , <<"ImagingStudy.Instance">> => {<<"BackboneElement">>,
            [
            {<<"uid">>, {{primitive, <<"id">>}, required}},
            {<<"sopClass">>, {{complex, <<"Coding">>}, required}},
            {<<"number">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% Immunization
%% Describes the event of a patient being administered a vaccine or a record of an immunization as reported by a patient, a clinician or another party.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Immunization">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"immunizationstatuscodes_list">>}, required}},
            {<<"statusReason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"vaccineCode">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"patient">>, {{complex, <<"Reference">>}, required}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"occurrenceDateTime">>, {{primitive, <<"dateTime">>}, required}},
            {<<"occurrenceString">>, {{primitive, <<"string">>}, required}},
            {<<"recorded">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"primarySource">>, {{primitive, <<"boolean">>}, optional}},
            {<<"reportOrigin">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"location">>, {{complex, <<"Reference">>}, optional}},
            {<<"manufacturer">>, {{complex, <<"Reference">>}, optional}},
            {<<"lotNumber">>, {{primitive, <<"string">>}, optional}},
            {<<"expirationDate">>, {{primitive, <<"date">>}, optional}},
            {<<"site">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"route">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"doseQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"performer">>, {{bbelement, <<"Immunization.Performer">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"isSubpotent">>, {{primitive, <<"boolean">>}, optional}},
            {<<"subpotentReason">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"education">>, {{bbelement, <<"Immunization.Education">>}, list}},
            {<<"programEligibility">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"fundingSource">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"reaction">>, {{bbelement, <<"Immunization.Reaction">>}, list}},
            {<<"protocolApplied">>, {{bbelement, <<"Immunization.ProtocolApplied">>}, list}}
            ],
            [],
            [
            {<<"occurrenceDateTime">>, <<"occurrenceString">>}
            ]
} 
%%
%% Immunization.Performer
%% Describes the event of a patient being administered a vaccine or a record of an immunization as reported by a patient, a clinician or another party.
%%
    , <<"Immunization.Performer">> => {<<"BackboneElement">>,
            [
            {<<"function">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"actor">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% Immunization.Education
%% Describes the event of a patient being administered a vaccine or a record of an immunization as reported by a patient, a clinician or another party.
%%
    , <<"Immunization.Education">> => {<<"BackboneElement">>,
            [
            {<<"documentType">>, {{primitive, <<"string">>}, optional}},
            {<<"reference">>, {{primitive, <<"uri">>}, optional}},
            {<<"publicationDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"presentationDate">>, {{primitive, <<"dateTime">>}, optional}}
            ],
            [],
            []
} 
%%
%% Immunization.Reaction
%% Describes the event of a patient being administered a vaccine or a record of an immunization as reported by a patient, a clinician or another party.
%%
    , <<"Immunization.Reaction">> => {<<"BackboneElement">>,
            [
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"detail">>, {{complex, <<"Reference">>}, optional}},
            {<<"reported">>, {{primitive, <<"boolean">>}, optional}}
            ],
            [],
            []
} 
%%
%% Immunization.ProtocolApplied
%% Describes the event of a patient being administered a vaccine or a record of an immunization as reported by a patient, a clinician or another party.
%%
    , <<"Immunization.ProtocolApplied">> => {<<"BackboneElement">>,
            [
            {<<"series">>, {{primitive, <<"string">>}, optional}},
            {<<"authority">>, {{complex, <<"Reference">>}, optional}},
            {<<"targetDisease">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"doseNumberPositiveInt">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"doseNumberString">>, {{primitive, <<"string">>}, required}},
            {<<"seriesDosesPositiveInt">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"seriesDosesString">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            [
            {<<"doseNumberPositiveInt">>, <<"doseNumberString">>}, 
            {<<"seriesDosesPositiveInt">>, <<"seriesDosesString">>}
            ]
} 
%%
%% ImmunizationEvaluation
%% Describes a comparison of an immunization event against published recommendations to determine if the administration is "valid" in relation to those  recommendations.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ImmunizationEvaluation">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"immunizationevaluationstatuscodes_list">>}, required}},
            {<<"patient">>, {{complex, <<"Reference">>}, required}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"authority">>, {{complex, <<"Reference">>}, optional}},
            {<<"targetDisease">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"immunizationEvent">>, {{complex, <<"Reference">>}, required}},
            {<<"doseStatus">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"doseStatusReason">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"series">>, {{primitive, <<"string">>}, optional}},
            {<<"doseNumberPositiveInt">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"doseNumberString">>, {{primitive, <<"string">>}, optional}},
            {<<"seriesDosesPositiveInt">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"seriesDosesString">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            [
            {<<"doseNumberPositiveInt">>, <<"doseNumberString">>}, 
            {<<"seriesDosesPositiveInt">>, <<"seriesDosesString">>}
            ]
} 
%%
%% ImmunizationRecommendation
%% A patient's point-in-time set of recommendations (i.e. forecasting) according to a published schedule with optional supporting justification.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ImmunizationRecommendation">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"patient">>, {{complex, <<"Reference">>}, required}},
            {<<"date">>, {{primitive, <<"dateTime">>}, required}},
            {<<"authority">>, {{complex, <<"Reference">>}, optional}},
            {<<"recommendation">>, {{bbelement, <<"ImmunizationRecommendation.Recommendation">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% ImmunizationRecommendation.Recommendation
%% A patient's point-in-time set of recommendations (i.e. forecasting) according to a published schedule with optional supporting justification.
%%
    , <<"ImmunizationRecommendation.Recommendation">> => {<<"BackboneElement">>,
            [
            {<<"vaccineCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"targetDisease">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"contraindicatedVaccineCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"forecastStatus">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"forecastReason">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"dateCriterion">>, {{bbelement, <<"ImmunizationRecommendation.DateCriterion">>}, list}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"series">>, {{primitive, <<"string">>}, optional}},
            {<<"doseNumberPositiveInt">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"doseNumberString">>, {{primitive, <<"string">>}, optional}},
            {<<"seriesDosesPositiveInt">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"seriesDosesString">>, {{primitive, <<"string">>}, optional}},
            {<<"supportingImmunization">>, {{complex, <<"Reference">>}, list}},
            {<<"supportingPatientInformation">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            [
            {<<"doseNumberPositiveInt">>, <<"doseNumberString">>}, 
            {<<"seriesDosesPositiveInt">>, <<"seriesDosesString">>}
            ]
} 
%%
%% ImmunizationRecommendation.DateCriterion
%% A patient's point-in-time set of recommendations (i.e. forecasting) according to a published schedule with optional supporting justification.
%%
    , <<"ImmunizationRecommendation.DateCriterion">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"value">>, {{primitive, <<"dateTime">>}, required}}
            ],
            [],
            []
} 
%%
%% ImplementationGuide
%% A set of rules of how a particular interoperability or standards problem is solved - typically through the use of FHIR resources. This resource is used to gather all the parts of an implementation guide into a logical whole and to publish a computable definition of all the parts.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ImplementationGuide">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, required}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"packageId">>, {{primitive, <<"id">>}, required}},
            {<<"license">>, {{code, <<"spdxlicense_list">>}, optional}},
            {<<"fhirVersion">>, {{code, <<"fhirversion_list">>}, non_empty_list}},
            {<<"dependsOn">>, {{bbelement, <<"ImplementationGuide.DependsOn">>}, list}},
            {<<"global">>, {{bbelement, <<"ImplementationGuide.Global">>}, list}},
            {<<"definition">>, {{bbelement, <<"ImplementationGuide.Definition">>}, optional}},
            {<<"manifest">>, {{bbelement, <<"ImplementationGuide.Manifest">>}, optional}}
            ],
            [],
            []
} 
%%
%% ImplementationGuide.DependsOn
%% A set of rules of how a particular interoperability or standards problem is solved - typically through the use of FHIR resources. This resource is used to gather all the parts of an implementation guide into a logical whole and to publish a computable definition of all the parts.
%%
    , <<"ImplementationGuide.DependsOn">> => {<<"BackboneElement">>,
            [
            {<<"uri">>, {{primitive, <<"canonical">>}, required}},
            {<<"packageId">>, {{primitive, <<"id">>}, optional}},
            {<<"version">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% ImplementationGuide.Global
%% A set of rules of how a particular interoperability or standards problem is solved - typically through the use of FHIR resources. This resource is used to gather all the parts of an implementation guide into a logical whole and to publish a computable definition of all the parts.
%%
    , <<"ImplementationGuide.Global">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{primitive, <<"code">>}, required}},
            {<<"profile">>, {{primitive, <<"canonical">>}, required}}
            ],
            [],
            []
} 
%%
%% ImplementationGuide.Definition
%% A set of rules of how a particular interoperability or standards problem is solved - typically through the use of FHIR resources. This resource is used to gather all the parts of an implementation guide into a logical whole and to publish a computable definition of all the parts.
%%
    , <<"ImplementationGuide.Definition">> => {<<"BackboneElement">>,
            [
            {<<"grouping">>, {{bbelement, <<"ImplementationGuide.Grouping">>}, list}},
            {<<"resource">>, {{bbelement, <<"ImplementationGuide.Resource">>}, non_empty_list}},
            {<<"page">>, {{bbelement, <<"ImplementationGuide.Page">>}, optional}},
            {<<"parameter">>, {{bbelement, <<"ImplementationGuide.Parameter">>}, list}},
            {<<"template">>, {{bbelement, <<"ImplementationGuide.Template">>}, list}}
            ],
            [],
            []
} 
%%
%% ImplementationGuide.Grouping
%% A set of rules of how a particular interoperability or standards problem is solved - typically through the use of FHIR resources. This resource is used to gather all the parts of an implementation guide into a logical whole and to publish a computable definition of all the parts.
%%
    , <<"ImplementationGuide.Grouping">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"description">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% ImplementationGuide.Resource
%% A set of rules of how a particular interoperability or standards problem is solved - typically through the use of FHIR resources. This resource is used to gather all the parts of an implementation guide into a logical whole and to publish a computable definition of all the parts.
%%
    , <<"ImplementationGuide.Resource">> => {<<"BackboneElement">>,
            [
            {<<"reference">>, {{complex, <<"Reference">>}, required}},
            {<<"fhirVersion">>, {{code, <<"fhirversion_list">>}, list}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"exampleBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"exampleCanonical">>, {{primitive, <<"canonical">>}, optional}},
            {<<"groupingId">>, {{primitive, <<"id">>}, optional}}
            ],
            [],
            [
            {<<"exampleBoolean">>, <<"exampleCanonical">>}
            ]
} 
%%
%% ImplementationGuide.Page
%% A set of rules of how a particular interoperability or standards problem is solved - typically through the use of FHIR resources. This resource is used to gather all the parts of an implementation guide into a logical whole and to publish a computable definition of all the parts.
%%
    , <<"ImplementationGuide.Page">> => {<<"BackboneElement">>,
            [
            {<<"nameUrl">>, {{primitive, <<"url">>}, required}},
            {<<"nameReference">>, {{complex, <<"Reference">>}, required}},
            {<<"title">>, {{primitive, <<"string">>}, required}},
            {<<"generation">>, {{code, <<"guidepagegeneration_list">>}, required}},
            {<<"page">>, {{bbelement, <<"ImplementationGuide.Page">>}, list}}
            ],
            [],
            [
            {<<"nameUrl">>, <<"nameReference">>}
            ]
} 
%%
%% ImplementationGuide.Parameter
%% A set of rules of how a particular interoperability or standards problem is solved - typically through the use of FHIR resources. This resource is used to gather all the parts of an implementation guide into a logical whole and to publish a computable definition of all the parts.
%%
    , <<"ImplementationGuide.Parameter">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{code, <<"guideparametercode_list">>}, required}},
            {<<"value">>, {{primitive, <<"string">>}, required}}
            ],
            [],
            []
} 
%%
%% ImplementationGuide.Template
%% A set of rules of how a particular interoperability or standards problem is solved - typically through the use of FHIR resources. This resource is used to gather all the parts of an implementation guide into a logical whole and to publish a computable definition of all the parts.
%%
    , <<"ImplementationGuide.Template">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{primitive, <<"code">>}, required}},
            {<<"source">>, {{primitive, <<"string">>}, required}},
            {<<"scope">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% ImplementationGuide.Manifest
%% A set of rules of how a particular interoperability or standards problem is solved - typically through the use of FHIR resources. This resource is used to gather all the parts of an implementation guide into a logical whole and to publish a computable definition of all the parts.
%%
    , <<"ImplementationGuide.Manifest">> => {<<"BackboneElement">>,
            [
            {<<"rendering">>, {{primitive, <<"url">>}, optional}},
            {<<"resource">>, {{bbelement, <<"ImplementationGuide.Resource1">>}, non_empty_list}},
            {<<"page">>, {{bbelement, <<"ImplementationGuide.Page1">>}, list}},
            {<<"image">>, {{primitive, <<"string">>}, list}},
            {<<"other">>, {{primitive, <<"string">>}, list}}
            ],
            [],
            []
} 
%%
%% ImplementationGuide.Resource1
%% A set of rules of how a particular interoperability or standards problem is solved - typically through the use of FHIR resources. This resource is used to gather all the parts of an implementation guide into a logical whole and to publish a computable definition of all the parts.
%%
    , <<"ImplementationGuide.Resource1">> => {<<"BackboneElement">>,
            [
            {<<"reference">>, {{complex, <<"Reference">>}, required}},
            {<<"exampleBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"exampleCanonical">>, {{primitive, <<"canonical">>}, optional}},
            {<<"relativePath">>, {{primitive, <<"url">>}, optional}}
            ],
            [],
            [
            {<<"exampleBoolean">>, <<"exampleCanonical">>}
            ]
} 
%%
%% ImplementationGuide.Page1
%% A set of rules of how a particular interoperability or standards problem is solved - typically through the use of FHIR resources. This resource is used to gather all the parts of an implementation guide into a logical whole and to publish a computable definition of all the parts.
%%
    , <<"ImplementationGuide.Page1">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"anchor">>, {{primitive, <<"string">>}, list}}
            ],
            [],
            []
} 
%%
%% InsurancePlan
%% Details of a Health Insurance product/plan provided by an organization.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"InsurancePlan">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"alias">>, {{primitive, <<"string">>}, list}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"ownedBy">>, {{complex, <<"Reference">>}, optional}},
            {<<"administeredBy">>, {{complex, <<"Reference">>}, optional}},
            {<<"coverageArea">>, {{complex, <<"Reference">>}, list}},
            {<<"contact">>, {{bbelement, <<"InsurancePlan.Contact">>}, list}},
            {<<"endpoint">>, {{complex, <<"Reference">>}, list}},
            {<<"network">>, {{complex, <<"Reference">>}, list}},
            {<<"coverage">>, {{bbelement, <<"InsurancePlan.Coverage">>}, list}},
            {<<"plan">>, {{bbelement, <<"InsurancePlan.Plan">>}, list}}
            ],
            [],
            []
} 
%%
%% InsurancePlan.Contact
%% Details of a Health Insurance product/plan provided by an organization.
%%
    , <<"InsurancePlan.Contact">> => {<<"BackboneElement">>,
            [
            {<<"purpose">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"name">>, {{complex, <<"HumanName">>}, optional}},
            {<<"telecom">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"address">>, {{complex, <<"Address">>}, optional}}
            ],
            [],
            []
} 
%%
%% InsurancePlan.Coverage
%% Details of a Health Insurance product/plan provided by an organization.
%%
    , <<"InsurancePlan.Coverage">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"network">>, {{complex, <<"Reference">>}, list}},
            {<<"benefit">>, {{bbelement, <<"InsurancePlan.Benefit">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% InsurancePlan.Benefit
%% Details of a Health Insurance product/plan provided by an organization.
%%
    , <<"InsurancePlan.Benefit">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"requirement">>, {{primitive, <<"string">>}, optional}},
            {<<"limit">>, {{bbelement, <<"InsurancePlan.Limit">>}, list}}
            ],
            [],
            []
} 
%%
%% InsurancePlan.Limit
%% Details of a Health Insurance product/plan provided by an organization.
%%
    , <<"InsurancePlan.Limit">> => {<<"BackboneElement">>,
            [
            {<<"value">>, {{complex, <<"Quantity">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% InsurancePlan.Plan
%% Details of a Health Insurance product/plan provided by an organization.
%%
    , <<"InsurancePlan.Plan">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"coverageArea">>, {{complex, <<"Reference">>}, list}},
            {<<"network">>, {{complex, <<"Reference">>}, list}},
            {<<"generalCost">>, {{bbelement, <<"InsurancePlan.GeneralCost">>}, list}},
            {<<"specificCost">>, {{bbelement, <<"InsurancePlan.SpecificCost">>}, list}}
            ],
            [],
            []
} 
%%
%% InsurancePlan.GeneralCost
%% Details of a Health Insurance product/plan provided by an organization.
%%
    , <<"InsurancePlan.GeneralCost">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"groupSize">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"cost">>, {{complex, <<"Money">>}, optional}},
            {<<"comment">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% InsurancePlan.SpecificCost
%% Details of a Health Insurance product/plan provided by an organization.
%%
    , <<"InsurancePlan.SpecificCost">> => {<<"BackboneElement">>,
            [
            {<<"category">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"benefit">>, {{bbelement, <<"InsurancePlan.Benefit1">>}, list}}
            ],
            [],
            []
} 
%%
%% InsurancePlan.Benefit1
%% Details of a Health Insurance product/plan provided by an organization.
%%
    , <<"InsurancePlan.Benefit1">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"cost">>, {{bbelement, <<"InsurancePlan.Cost">>}, list}}
            ],
            [],
            []
} 
%%
%% InsurancePlan.Cost
%% Details of a Health Insurance product/plan provided by an organization.
%%
    , <<"InsurancePlan.Cost">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"applicability">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"qualifiers">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"value">>, {{complex, <<"Quantity">>}, optional}}
            ],
            [],
            []
} 
%%
%% Invoice
%% Invoice containing collected ChargeItems from an Account with calculated individual and total price for Billing purpose.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Invoice">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"invoicestatus_list">>}, required}},
            {<<"cancelledReason">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"recipient">>, {{complex, <<"Reference">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"participant">>, {{bbelement, <<"Invoice.Participant">>}, list}},
            {<<"issuer">>, {{complex, <<"Reference">>}, optional}},
            {<<"account">>, {{complex, <<"Reference">>}, optional}},
            {<<"lineItem">>, {{bbelement, <<"Invoice.LineItem">>}, list}},
            {<<"totalPriceComponent">>, {{bbelement, <<"Invoice.PriceComponent">>}, list}},
            {<<"totalNet">>, {{complex, <<"Money">>}, optional}},
            {<<"totalGross">>, {{complex, <<"Money">>}, optional}},
            {<<"paymentTerms">>, {{primitive, <<"markdown">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            []
} 
%%
%% Invoice.Participant
%% Invoice containing collected ChargeItems from an Account with calculated individual and total price for Billing purpose.
%%
    , <<"Invoice.Participant">> => {<<"BackboneElement">>,
            [
            {<<"role">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"actor">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% Invoice.LineItem
%% Invoice containing collected ChargeItems from an Account with calculated individual and total price for Billing purpose.
%%
    , <<"Invoice.LineItem">> => {<<"BackboneElement">>,
            [
            {<<"sequence">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"chargeItemReference">>, {{complex, <<"Reference">>}, required}},
            {<<"chargeItemCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"priceComponent">>, {{bbelement, <<"Invoice.PriceComponent">>}, list}}
            ],
            [],
            [
            {<<"chargeItemReference">>, <<"chargeItemCodeableConcept">>}
            ]
} 
%%
%% Invoice.PriceComponent
%% Invoice containing collected ChargeItems from an Account with calculated individual and total price for Billing purpose.
%%
    , <<"Invoice.PriceComponent">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{code, <<"invoicepricecomponenttype_list">>}, required}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"factor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"amount">>, {{complex, <<"Money">>}, optional}}
            ],
            [],
            []
} 
%%
%% Library
%% The Library resource is a general-purpose container for knowledge asset definitions. It can be used to describe and expose existing knowledge assets such as logic libraries and information model descriptions, as well as to describe a collection of knowledge assets.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Library">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"subtitle">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"subjectCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subjectReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"usage">>, {{primitive, <<"string">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"approvalDate">>, {{primitive, <<"date">>}, optional}},
            {<<"lastReviewDate">>, {{primitive, <<"date">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"topic">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"author">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"editor">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"reviewer">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"endorser">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"relatedArtifact">>, {{complex, <<"RelatedArtifact">>}, list}},
            {<<"parameter">>, {{complex, <<"ParameterDefinition">>}, list}},
            {<<"dataRequirement">>, {{complex, <<"DataRequirement">>}, list}},
            {<<"content">>, {{complex, <<"Attachment">>}, list}}
            ],
            [],
            [
            {<<"subjectCodeableConcept">>, <<"subjectReference">>}
            ]
} 
%%
%% Linkage
%% Identifies two or more records (resource instances) that refer to the same real-world "occurrence".
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Linkage">> => {<<"DomainResource">>,
            [
            {<<"active">>, {{primitive, <<"boolean">>}, optional}},
            {<<"author">>, {{complex, <<"Reference">>}, optional}},
            {<<"item">>, {{bbelement, <<"Linkage.Item">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% Linkage.Item
%% Identifies two or more records (resource instances) that refer to the same real-world "occurrence".
%%
    , <<"Linkage.Item">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{code, <<"linkagetype_list">>}, required}},
            {<<"resource">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% List
%% A list is a curated collection of resources.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"List">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"liststatus_list">>}, required}},
            {<<"mode">>, {{code, <<"listmode_list">>}, required}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"source">>, {{complex, <<"Reference">>}, optional}},
            {<<"orderedBy">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"entry">>, {{bbelement, <<"List.Entry">>}, list}},
            {<<"emptyReason">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% List.Entry
%% A list is a curated collection of resources.
%%
    , <<"List.Entry">> => {<<"BackboneElement">>,
            [
            {<<"flag">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"deleted">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"item">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% Location
%% Details and position information for a physical place where services are provided and resources and participants may be stored, found, contained, or accommodated.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Location">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"locationstatus_list">>}, optional}},
            {<<"operationalStatus">>, {{complex, <<"Coding">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"alias">>, {{primitive, <<"string">>}, list}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"mode">>, {{code, <<"locationmode_list">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"telecom">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"address">>, {{complex, <<"Address">>}, optional}},
            {<<"physicalType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"position">>, {{bbelement, <<"Location.Position">>}, optional}},
            {<<"managingOrganization">>, {{complex, <<"Reference">>}, optional}},
            {<<"partOf">>, {{complex, <<"Reference">>}, optional}},
            {<<"hoursOfOperation">>, {{bbelement, <<"Location.HoursOfOperation">>}, list}},
            {<<"availabilityExceptions">>, {{primitive, <<"string">>}, optional}},
            {<<"endpoint">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% Location.Position
%% Details and position information for a physical place where services are provided and resources and participants may be stored, found, contained, or accommodated.
%%
    , <<"Location.Position">> => {<<"BackboneElement">>,
            [
            {<<"longitude">>, {{primitive, <<"decimal">>}, required}},
            {<<"latitude">>, {{primitive, <<"decimal">>}, required}},
            {<<"altitude">>, {{primitive, <<"decimal">>}, optional}}
            ],
            [],
            []
} 
%%
%% Location.HoursOfOperation
%% Details and position information for a physical place where services are provided and resources and participants may be stored, found, contained, or accommodated.
%%
    , <<"Location.HoursOfOperation">> => {<<"BackboneElement">>,
            [
            {<<"daysOfWeek">>, {{code, <<"daysofweek_list">>}, list}},
            {<<"allDay">>, {{primitive, <<"boolean">>}, optional}},
            {<<"openingTime">>, {{primitive, <<"time">>}, optional}},
            {<<"closingTime">>, {{primitive, <<"time">>}, optional}}
            ],
            [],
            []
} 
%%
%% Measure
%% The Measure resource provides the definition of a quality measure.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Measure">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"subtitle">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"subjectCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subjectReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"usage">>, {{primitive, <<"string">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"approvalDate">>, {{primitive, <<"date">>}, optional}},
            {<<"lastReviewDate">>, {{primitive, <<"date">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"topic">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"author">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"editor">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"reviewer">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"endorser">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"relatedArtifact">>, {{complex, <<"RelatedArtifact">>}, list}},
            {<<"library">>, {{primitive, <<"canonical">>}, list}},
            {<<"disclaimer">>, {{primitive, <<"markdown">>}, optional}},
            {<<"scoring">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"compositeScoring">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"riskAdjustment">>, {{primitive, <<"string">>}, optional}},
            {<<"rateAggregation">>, {{primitive, <<"string">>}, optional}},
            {<<"rationale">>, {{primitive, <<"markdown">>}, optional}},
            {<<"clinicalRecommendationStatement">>, {{primitive, <<"markdown">>}, optional}},
            {<<"improvementNotation">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"definition">>, {{primitive, <<"markdown">>}, list}},
            {<<"guidance">>, {{primitive, <<"markdown">>}, optional}},
            {<<"group">>, {{bbelement, <<"Measure.Group">>}, list}},
            {<<"supplementalData">>, {{bbelement, <<"Measure.SupplementalData">>}, list}}
            ],
            [],
            [
            {<<"subjectCodeableConcept">>, <<"subjectReference">>}
            ]
} 
%%
%% Measure.Group
%% The Measure resource provides the definition of a quality measure.
%%
    , <<"Measure.Group">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"population">>, {{bbelement, <<"Measure.Population">>}, list}},
            {<<"stratifier">>, {{bbelement, <<"Measure.Stratifier">>}, list}}
            ],
            [],
            []
} 
%%
%% Measure.Population
%% The Measure resource provides the definition of a quality measure.
%%
    , <<"Measure.Population">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"criteria">>, {{complex, <<"Expression">>}, required}}
            ],
            [],
            []
} 
%%
%% Measure.Stratifier
%% The Measure resource provides the definition of a quality measure.
%%
    , <<"Measure.Stratifier">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"criteria">>, {{complex, <<"Expression">>}, optional}},
            {<<"component">>, {{bbelement, <<"Measure.Component">>}, list}}
            ],
            [],
            []
} 
%%
%% Measure.Component
%% The Measure resource provides the definition of a quality measure.
%%
    , <<"Measure.Component">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"criteria">>, {{complex, <<"Expression">>}, required}}
            ],
            [],
            []
} 
%%
%% Measure.SupplementalData
%% The Measure resource provides the definition of a quality measure.
%%
    , <<"Measure.SupplementalData">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"usage">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"criteria">>, {{complex, <<"Expression">>}, required}}
            ],
            [],
            []
} 
%%
%% MeasureReport
%% The MeasureReport resource contains the results of the calculation of a measure; and optionally a reference to the resources involved in that calculation.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MeasureReport">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"measurereportstatus_list">>}, required}},
            {<<"type">>, {{code, <<"measurereporttype_list">>}, required}},
            {<<"measure">>, {{primitive, <<"canonical">>}, required}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"reporter">>, {{complex, <<"Reference">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, required}},
            {<<"improvementNotation">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"group">>, {{bbelement, <<"MeasureReport.Group">>}, list}},
            {<<"evaluatedResource">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% MeasureReport.Group
%% The MeasureReport resource contains the results of the calculation of a measure; and optionally a reference to the resources involved in that calculation.
%%
    , <<"MeasureReport.Group">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"population">>, {{bbelement, <<"MeasureReport.Population">>}, list}},
            {<<"measureScore">>, {{complex, <<"Quantity">>}, optional}},
            {<<"stratifier">>, {{bbelement, <<"MeasureReport.Stratifier">>}, list}}
            ],
            [],
            []
} 
%%
%% MeasureReport.Population
%% The MeasureReport resource contains the results of the calculation of a measure; and optionally a reference to the resources involved in that calculation.
%%
    , <<"MeasureReport.Population">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"count">>, {{primitive, <<"integer">>}, optional}},
            {<<"subjectResults">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% MeasureReport.Stratifier
%% The MeasureReport resource contains the results of the calculation of a measure; and optionally a reference to the resources involved in that calculation.
%%
    , <<"MeasureReport.Stratifier">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"stratum">>, {{bbelement, <<"MeasureReport.Stratum">>}, list}}
            ],
            [],
            []
} 
%%
%% MeasureReport.Stratum
%% The MeasureReport resource contains the results of the calculation of a measure; and optionally a reference to the resources involved in that calculation.
%%
    , <<"MeasureReport.Stratum">> => {<<"BackboneElement">>,
            [
            {<<"value">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"component">>, {{bbelement, <<"MeasureReport.Component">>}, list}},
            {<<"population">>, {{bbelement, <<"MeasureReport.Population1">>}, list}},
            {<<"measureScore">>, {{complex, <<"Quantity">>}, optional}}
            ],
            [],
            []
} 
%%
%% MeasureReport.Component
%% The MeasureReport resource contains the results of the calculation of a measure; and optionally a reference to the resources involved in that calculation.
%%
    , <<"MeasureReport.Component">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"value">>, {{complex, <<"CodeableConcept">>}, required}}
            ],
            [],
            []
} 
%%
%% MeasureReport.Population1
%% The MeasureReport resource contains the results of the calculation of a measure; and optionally a reference to the resources involved in that calculation.
%%
    , <<"MeasureReport.Population1">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"count">>, {{primitive, <<"integer">>}, optional}},
            {<<"subjectResults">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% Media
%% A photo, video, or audio recording acquired or used in healthcare. The actual content may be inline or provided by direct reference.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Media">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"partOf">>, {{complex, <<"Reference">>}, list}},
            {<<"status">>, {{code, <<"eventstatus_list">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"modality">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"view">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"createdDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"createdPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"issued">>, {{primitive, <<"instant">>}, optional}},
            {<<"operator">>, {{complex, <<"Reference">>}, optional}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"bodySite">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"deviceName">>, {{primitive, <<"string">>}, optional}},
            {<<"device">>, {{complex, <<"Reference">>}, optional}},
            {<<"height">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"width">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"frames">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"duration">>, {{primitive, <<"decimal">>}, optional}},
            {<<"content">>, {{complex, <<"Attachment">>}, required}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            [
            {<<"createdDateTime">>, <<"createdPeriod">>}
            ]
} 
%%
%% Medication
%% This resource is primarily used for the identification and definition of a medication for the purposes of prescribing, dispensing, and administering a medication as well as for making statements about medication use.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Medication">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"status">>, {{code, <<"medicationstatuscodes_list">>}, optional}},
            {<<"manufacturer">>, {{complex, <<"Reference">>}, optional}},
            {<<"form">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"amount">>, {{complex, <<"Ratio">>}, optional}},
            {<<"ingredient">>, {{bbelement, <<"Medication.Ingredient">>}, list}},
            {<<"batch">>, {{bbelement, <<"Medication.Batch">>}, optional}}
            ],
            [],
            []
} 
%%
%% Medication.Ingredient
%% This resource is primarily used for the identification and definition of a medication for the purposes of prescribing, dispensing, and administering a medication as well as for making statements about medication use.
%%
    , <<"Medication.Ingredient">> => {<<"BackboneElement">>,
            [
            {<<"itemCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"itemReference">>, {{complex, <<"Reference">>}, required}},
            {<<"isActive">>, {{primitive, <<"boolean">>}, optional}},
            {<<"strength">>, {{complex, <<"Ratio">>}, optional}}
            ],
            [],
            [
            {<<"itemCodeableConcept">>, <<"itemReference">>}
            ]
} 
%%
%% Medication.Batch
%% This resource is primarily used for the identification and definition of a medication for the purposes of prescribing, dispensing, and administering a medication as well as for making statements about medication use.
%%
    , <<"Medication.Batch">> => {<<"BackboneElement">>,
            [
            {<<"lotNumber">>, {{primitive, <<"string">>}, optional}},
            {<<"expirationDate">>, {{primitive, <<"dateTime">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicationAdministration
%% Describes the event of a patient consuming or otherwise being administered a medication.  This may be as simple as swallowing a tablet or it may be a long running infusion.  Related resources tie this event to the authorizing prescription, and the specific encounter between patient and health care practitioner.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MedicationAdministration">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"instantiates">>, {{primitive, <<"uri">>}, list}},
            {<<"partOf">>, {{complex, <<"Reference">>}, list}},
            {<<"status">>, {{primitive, <<"code">>}, required}},
            {<<"statusReason">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"medicationCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"medicationReference">>, {{complex, <<"Reference">>}, required}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"context">>, {{complex, <<"Reference">>}, optional}},
            {<<"supportingInformation">>, {{complex, <<"Reference">>}, list}},
            {<<"effectiveDateTime">>, {{primitive, <<"dateTime">>}, required}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, required}},
            {<<"performer">>, {{bbelement, <<"MedicationAdministration.Performer">>}, list}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"request">>, {{complex, <<"Reference">>}, optional}},
            {<<"device">>, {{complex, <<"Reference">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"dosage">>, {{bbelement, <<"MedicationAdministration.Dosage">>}, optional}},
            {<<"eventHistory">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            [
            {<<"medicationCodeableConcept">>, <<"medicationReference">>}, 
            {<<"effectiveDateTime">>, <<"effectivePeriod">>}
            ]
} 
%%
%% MedicationAdministration.Performer
%% Describes the event of a patient consuming or otherwise being administered a medication.  This may be as simple as swallowing a tablet or it may be a long running infusion.  Related resources tie this event to the authorizing prescription, and the specific encounter between patient and health care practitioner.
%%
    , <<"MedicationAdministration.Performer">> => {<<"BackboneElement">>,
            [
            {<<"function">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"actor">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% MedicationAdministration.Dosage
%% Describes the event of a patient consuming or otherwise being administered a medication.  This may be as simple as swallowing a tablet or it may be a long running infusion.  Related resources tie this event to the authorizing prescription, and the specific encounter between patient and health care practitioner.
%%
    , <<"MedicationAdministration.Dosage">> => {<<"BackboneElement">>,
            [
            {<<"text">>, {{primitive, <<"string">>}, optional}},
            {<<"site">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"route">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"method">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"dose">>, {{complex, <<"Quantity">>}, optional}},
            {<<"rateRatio">>, {{complex, <<"Ratio">>}, optional}},
            {<<"rateQuantity">>, {{complex, <<"Quantity">>}, optional}}
            ],
            [],
            [
            {<<"rateRatio">>, <<"rateQuantity">>}
            ]
} 
%%
%% MedicationDispense
%% Indicates that a medication product is to be or has been dispensed for a named person/patient.  This includes a description of the medication product (supply) provided and the instructions for administering the medication.  The medication dispense is the result of a pharmacy system responding to a medication order.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MedicationDispense">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"partOf">>, {{complex, <<"Reference">>}, list}},
            {<<"status">>, {{primitive, <<"code">>}, required}},
            {<<"statusReasonCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"statusReasonReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"medicationCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"medicationReference">>, {{complex, <<"Reference">>}, required}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"context">>, {{complex, <<"Reference">>}, optional}},
            {<<"supportingInformation">>, {{complex, <<"Reference">>}, list}},
            {<<"performer">>, {{bbelement, <<"MedicationDispense.Performer">>}, list}},
            {<<"location">>, {{complex, <<"Reference">>}, optional}},
            {<<"authorizingPrescription">>, {{complex, <<"Reference">>}, list}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"daysSupply">>, {{complex, <<"Quantity">>}, optional}},
            {<<"whenPrepared">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"whenHandedOver">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"destination">>, {{complex, <<"Reference">>}, optional}},
            {<<"receiver">>, {{complex, <<"Reference">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"dosageInstruction">>, {{bbelement, <<"Dosage">>}, list}},
            {<<"substitution">>, {{bbelement, <<"MedicationDispense.Substitution">>}, optional}},
            {<<"detectedIssue">>, {{complex, <<"Reference">>}, list}},
            {<<"eventHistory">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            [
            {<<"statusReasonCodeableConcept">>, <<"statusReasonReference">>}, 
            {<<"medicationCodeableConcept">>, <<"medicationReference">>}
            ]
} 
%%
%% MedicationDispense.Performer
%% Indicates that a medication product is to be or has been dispensed for a named person/patient.  This includes a description of the medication product (supply) provided and the instructions for administering the medication.  The medication dispense is the result of a pharmacy system responding to a medication order.
%%
    , <<"MedicationDispense.Performer">> => {<<"BackboneElement">>,
            [
            {<<"function">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"actor">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% MedicationDispense.Substitution
%% Indicates that a medication product is to be or has been dispensed for a named person/patient.  This includes a description of the medication product (supply) provided and the instructions for administering the medication.  The medication dispense is the result of a pharmacy system responding to a medication order.
%%
    , <<"MedicationDispense.Substitution">> => {<<"BackboneElement">>,
            [
            {<<"wasSubstituted">>, {{primitive, <<"boolean">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"reason">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"responsibleParty">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% MedicationKnowledge
%% Information about a medication that is used to support knowledge.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MedicationKnowledge">> => {<<"DomainResource">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"status">>, {{primitive, <<"code">>}, optional}},
            {<<"manufacturer">>, {{complex, <<"Reference">>}, optional}},
            {<<"doseForm">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"amount">>, {{complex, <<"Quantity">>}, optional}},
            {<<"synonym">>, {{primitive, <<"string">>}, list}},
            {<<"relatedMedicationKnowledge">>, {{bbelement, <<"MedicationKnowledge.RelatedMedicationKnowledge">>}, list}},
            {<<"associatedMedication">>, {{complex, <<"Reference">>}, list}},
            {<<"productType">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"monograph">>, {{bbelement, <<"MedicationKnowledge.Monograph">>}, list}},
            {<<"ingredient">>, {{bbelement, <<"MedicationKnowledge.Ingredient">>}, list}},
            {<<"preparationInstruction">>, {{primitive, <<"markdown">>}, optional}},
            {<<"intendedRoute">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"cost">>, {{bbelement, <<"MedicationKnowledge.Cost">>}, list}},
            {<<"monitoringProgram">>, {{bbelement, <<"MedicationKnowledge.MonitoringProgram">>}, list}},
            {<<"administrationGuidelines">>, {{bbelement, <<"MedicationKnowledge.AdministrationGuidelines">>}, list}},
            {<<"medicineClassification">>, {{bbelement, <<"MedicationKnowledge.MedicineClassification">>}, list}},
            {<<"packaging">>, {{bbelement, <<"MedicationKnowledge.Packaging">>}, optional}},
            {<<"drugCharacteristic">>, {{bbelement, <<"MedicationKnowledge.DrugCharacteristic">>}, list}},
            {<<"contraindication">>, {{complex, <<"Reference">>}, list}},
            {<<"regulatory">>, {{bbelement, <<"MedicationKnowledge.Regulatory">>}, list}},
            {<<"kinetics">>, {{bbelement, <<"MedicationKnowledge.Kinetics">>}, list}}
            ],
            [],
            []
} 
%%
%% MedicationKnowledge.RelatedMedicationKnowledge
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.RelatedMedicationKnowledge">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"reference">>, {{complex, <<"Reference">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% MedicationKnowledge.Monograph
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.Monograph">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"source">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicationKnowledge.Ingredient
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.Ingredient">> => {<<"BackboneElement">>,
            [
            {<<"itemCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"itemReference">>, {{complex, <<"Reference">>}, required}},
            {<<"isActive">>, {{primitive, <<"boolean">>}, optional}},
            {<<"strength">>, {{complex, <<"Ratio">>}, optional}}
            ],
            [],
            [
            {<<"itemCodeableConcept">>, <<"itemReference">>}
            ]
} 
%%
%% MedicationKnowledge.Cost
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.Cost">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"source">>, {{primitive, <<"string">>}, optional}},
            {<<"cost">>, {{complex, <<"Money">>}, required}}
            ],
            [],
            []
} 
%%
%% MedicationKnowledge.MonitoringProgram
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.MonitoringProgram">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicationKnowledge.AdministrationGuidelines
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.AdministrationGuidelines">> => {<<"BackboneElement">>,
            [
            {<<"dosage">>, {{bbelement, <<"MedicationKnowledge.Dosage">>}, list}},
            {<<"indicationCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"indicationReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"patientCharacteristics">>, {{bbelement, <<"MedicationKnowledge.PatientCharacteristics">>}, list}}
            ],
            [],
            [
            {<<"indicationCodeableConcept">>, <<"indicationReference">>}
            ]
} 
%%
%% MedicationKnowledge.Dosage
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.Dosage">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"dosage">>, {{bbelement, <<"Dosage">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% MedicationKnowledge.PatientCharacteristics
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.PatientCharacteristics">> => {<<"BackboneElement">>,
            [
            {<<"characteristicCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"characteristicQuantity">>, {{complex, <<"Quantity">>}, required}},
            {<<"value">>, {{primitive, <<"string">>}, list}}
            ],
            [],
            [
            {<<"characteristicCodeableConcept">>, <<"characteristicQuantity">>}
            ]
} 
%%
%% MedicationKnowledge.MedicineClassification
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.MedicineClassification">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"classification">>, {{complex, <<"CodeableConcept">>}, list}}
            ],
            [],
            []
} 
%%
%% MedicationKnowledge.Packaging
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.Packaging">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicationKnowledge.DrugCharacteristic
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.DrugCharacteristic">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"valueCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"valueString">>, {{primitive, <<"string">>}, optional}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"valueBase64Binary">>, {{primitive, <<"base64Binary">>}, optional}}
            ],
            [],
            [
            {<<"valueCodeableConcept">>, <<"valueString">>, <<"valueQuantity">>, <<"valueBase64Binary">>}
            ]
} 
%%
%% MedicationKnowledge.Regulatory
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.Regulatory">> => {<<"BackboneElement">>,
            [
            {<<"regulatoryAuthority">>, {{complex, <<"Reference">>}, required}},
            {<<"substitution">>, {{bbelement, <<"MedicationKnowledge.Substitution">>}, list}},
            {<<"schedule">>, {{bbelement, <<"MedicationKnowledge.Schedule">>}, list}},
            {<<"maxDispense">>, {{bbelement, <<"MedicationKnowledge.MaxDispense">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicationKnowledge.Substitution
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.Substitution">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"allowed">>, {{primitive, <<"boolean">>}, required}}
            ],
            [],
            []
} 
%%
%% MedicationKnowledge.Schedule
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.Schedule">> => {<<"BackboneElement">>,
            [
            {<<"schedule">>, {{complex, <<"CodeableConcept">>}, required}}
            ],
            [],
            []
} 
%%
%% MedicationKnowledge.MaxDispense
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.MaxDispense">> => {<<"BackboneElement">>,
            [
            {<<"quantity">>, {{complex, <<"Quantity">>}, required}},
            {<<"period">>, {{complex, <<"Duration">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicationKnowledge.Kinetics
%% Information about a medication that is used to support knowledge.
%%
    , <<"MedicationKnowledge.Kinetics">> => {<<"BackboneElement">>,
            [
            {<<"areaUnderCurve">>, {{complex, <<"Quantity">>}, list}},
            {<<"lethalDose50">>, {{complex, <<"Quantity">>}, list}},
            {<<"halfLifePeriod">>, {{complex, <<"Duration">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicationRequest
%% An order or request for both supply of the medication and the instructions for administration of the medication to a patient. The resource is called "MedicationRequest" rather than "MedicationPrescription" or "MedicationOrder" to generalize the use across inpatient and outpatient settings, including care plans, etc., and to harmonize with workflow patterns.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MedicationRequest">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"medicationrequeststatus_list">>}, required}},
            {<<"statusReason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"intent">>, {{code, <<"medicationrequestintent_list">>}, required}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"priority">>, {{code, <<"requestpriority_list">>}, optional}},
            {<<"doNotPerform">>, {{primitive, <<"boolean">>}, optional}},
            {<<"reportedBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"reportedReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"medicationCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"medicationReference">>, {{complex, <<"Reference">>}, required}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"supportingInformation">>, {{complex, <<"Reference">>}, list}},
            {<<"authoredOn">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"requester">>, {{complex, <<"Reference">>}, optional}},
            {<<"performer">>, {{complex, <<"Reference">>}, optional}},
            {<<"performerType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"recorder">>, {{complex, <<"Reference">>}, optional}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"instantiatesCanonical">>, {{primitive, <<"canonical">>}, list}},
            {<<"instantiatesUri">>, {{primitive, <<"uri">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"groupIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"courseOfTherapyType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"insurance">>, {{complex, <<"Reference">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"dosageInstruction">>, {{bbelement, <<"Dosage">>}, list}},
            {<<"dispenseRequest">>, {{bbelement, <<"MedicationRequest.DispenseRequest">>}, optional}},
            {<<"substitution">>, {{bbelement, <<"MedicationRequest.Substitution">>}, optional}},
            {<<"priorPrescription">>, {{complex, <<"Reference">>}, optional}},
            {<<"detectedIssue">>, {{complex, <<"Reference">>}, list}},
            {<<"eventHistory">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            [
            {<<"reportedBoolean">>, <<"reportedReference">>}, 
            {<<"medicationCodeableConcept">>, <<"medicationReference">>}
            ]
} 
%%
%% MedicationRequest.DispenseRequest
%% An order or request for both supply of the medication and the instructions for administration of the medication to a patient. The resource is called "MedicationRequest" rather than "MedicationPrescription" or "MedicationOrder" to generalize the use across inpatient and outpatient settings, including care plans, etc., and to harmonize with workflow patterns.
%%
    , <<"MedicationRequest.DispenseRequest">> => {<<"BackboneElement">>,
            [
            {<<"initialFill">>, {{bbelement, <<"MedicationRequest.InitialFill">>}, optional}},
            {<<"dispenseInterval">>, {{complex, <<"Duration">>}, optional}},
            {<<"validityPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"numberOfRepeatsAllowed">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"expectedSupplyDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"performer">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicationRequest.InitialFill
%% An order or request for both supply of the medication and the instructions for administration of the medication to a patient. The resource is called "MedicationRequest" rather than "MedicationPrescription" or "MedicationOrder" to generalize the use across inpatient and outpatient settings, including care plans, etc., and to harmonize with workflow patterns.
%%
    , <<"MedicationRequest.InitialFill">> => {<<"BackboneElement">>,
            [
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"duration">>, {{complex, <<"Duration">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicationRequest.Substitution
%% An order or request for both supply of the medication and the instructions for administration of the medication to a patient. The resource is called "MedicationRequest" rather than "MedicationPrescription" or "MedicationOrder" to generalize the use across inpatient and outpatient settings, including care plans, etc., and to harmonize with workflow patterns.
%%
    , <<"MedicationRequest.Substitution">> => {<<"BackboneElement">>,
            [
            {<<"allowedBoolean">>, {{primitive, <<"boolean">>}, required}},
            {<<"allowedCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"reason">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            [
            {<<"allowedBoolean">>, <<"allowedCodeableConcept">>}
            ]
} 
%%
%% MedicationStatement
%% A record of a medication that is being consumed by a patient.   
%% A MedicationStatement may indicate that the patient may be taking the medication now or has taken the medication in the past or will be taking the medication in the future.  The source of this information can be the patient, significant other (such as a family member or spouse), or a clinician.  A common scenario where this information is captured is during the history taking process during a patient visit or stay.   The medication information may come from sources such as the patient's memory, from a prescription bottle,  or from a list of medications the patient, clinician or other party maintains. 
%%
%% The primary difference between a medication statement and a medication administration is that the medication administration has complete administration information and is based on actual administration information from the person who administered the medication.  A medication statement is often, if not always, less specific.  There is no required date/time when the medication was administered, in fact we only know that a source has reported the patient is taking this medication, where details such as time, quantity, or rate or even medication product may be incomplete or missing or less precise.  As stated earlier, the medication statement information may come from the patient's memory, from a prescription bottle or from a list of medications the patient, clinician or other party maintains.  Medication administration is more formal and is not missing detailed information.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MedicationStatement">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"partOf">>, {{complex, <<"Reference">>}, list}},
            {<<"status">>, {{code, <<"medicationstatuscodes_list">>}, required}},
            {<<"statusReason">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"medicationCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"medicationReference">>, {{complex, <<"Reference">>}, required}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"context">>, {{complex, <<"Reference">>}, optional}},
            {<<"effectiveDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"dateAsserted">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"informationSource">>, {{complex, <<"Reference">>}, optional}},
            {<<"derivedFrom">>, {{complex, <<"Reference">>}, list}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"dosage">>, {{bbelement, <<"Dosage">>}, list}}
            ],
            [],
            [
            {<<"medicationCodeableConcept">>, <<"medicationReference">>}, 
            {<<"effectiveDateTime">>, <<"effectivePeriod">>}
            ]
} 
%%
%% MedicinalProduct
%% Detailed definition of a medicinal product, typically for uses other than direct patient care (e.g. regulatory use).
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MedicinalProduct">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"domain">>, {{complex, <<"Coding">>}, optional}},
            {<<"combinedPharmaceuticalDoseForm">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"legalStatusOfSupply">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"additionalMonitoringIndicator">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"specialMeasures">>, {{primitive, <<"string">>}, list}},
            {<<"paediatricUseIndicator">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"productClassification">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"marketingStatus">>, {{bbelement, <<"MarketingStatus">>}, list}},
            {<<"pharmaceuticalProduct">>, {{complex, <<"Reference">>}, list}},
            {<<"packagedMedicinalProduct">>, {{complex, <<"Reference">>}, list}},
            {<<"attachedDocument">>, {{complex, <<"Reference">>}, list}},
            {<<"masterFile">>, {{complex, <<"Reference">>}, list}},
            {<<"contact">>, {{complex, <<"Reference">>}, list}},
            {<<"clinicalTrial">>, {{complex, <<"Reference">>}, list}},
            {<<"name">>, {{bbelement, <<"MedicinalProduct.Name">>}, non_empty_list}},
            {<<"crossReference">>, {{complex, <<"Identifier">>}, list}},
            {<<"manufacturingBusinessOperation">>, {{bbelement, <<"MedicinalProduct.ManufacturingBusinessOperation">>}, list}},
            {<<"specialDesignation">>, {{bbelement, <<"MedicinalProduct.SpecialDesignation">>}, list}}
            ],
            [],
            []
} 
%%
%% MedicinalProduct.Name
%% Detailed definition of a medicinal product, typically for uses other than direct patient care (e.g. regulatory use).
%%
    , <<"MedicinalProduct.Name">> => {<<"BackboneElement">>,
            [
            {<<"productName">>, {{primitive, <<"string">>}, required}},
            {<<"namePart">>, {{bbelement, <<"MedicinalProduct.NamePart">>}, list}},
            {<<"countryLanguage">>, {{bbelement, <<"MedicinalProduct.CountryLanguage">>}, list}}
            ],
            [],
            []
} 
%%
%% MedicinalProduct.NamePart
%% Detailed definition of a medicinal product, typically for uses other than direct patient care (e.g. regulatory use).
%%
    , <<"MedicinalProduct.NamePart">> => {<<"BackboneElement">>,
            [
            {<<"part">>, {{primitive, <<"string">>}, required}},
            {<<"type">>, {{complex, <<"Coding">>}, required}}
            ],
            [],
            []
} 
%%
%% MedicinalProduct.CountryLanguage
%% Detailed definition of a medicinal product, typically for uses other than direct patient care (e.g. regulatory use).
%%
    , <<"MedicinalProduct.CountryLanguage">> => {<<"BackboneElement">>,
            [
            {<<"country">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"language">>, {{complex, <<"CodeableConcept">>}, required}}
            ],
            [],
            []
} 
%%
%% MedicinalProduct.ManufacturingBusinessOperation
%% Detailed definition of a medicinal product, typically for uses other than direct patient care (e.g. regulatory use).
%%
    , <<"MedicinalProduct.ManufacturingBusinessOperation">> => {<<"BackboneElement">>,
            [
            {<<"operationType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"authorisationReferenceNumber">>, {{complex, <<"Identifier">>}, optional}},
            {<<"effectiveDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"confidentialityIndicator">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"manufacturer">>, {{complex, <<"Reference">>}, list}},
            {<<"regulator">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicinalProduct.SpecialDesignation
%% Detailed definition of a medicinal product, typically for uses other than direct patient care (e.g. regulatory use).
%%
    , <<"MedicinalProduct.SpecialDesignation">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"intendedUse">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"indicationCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"indicationReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"status">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"species">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            [
            {<<"indicationCodeableConcept">>, <<"indicationReference">>}
            ]
} 
%%
%% MedicinalProductAuthorization
%% The regulatory authorization of a medicinal product.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MedicinalProductAuthorization">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"country">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"status">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"statusDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"restoreDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"validityPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"dataExclusivityPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"dateOfFirstAuthorization">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"internationalBirthDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"legalBasis">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"jurisdictionalAuthorization">>, {{bbelement, <<"MedicinalProductAuthorization.JurisdictionalAuthorization">>}, list}},
            {<<"holder">>, {{complex, <<"Reference">>}, optional}},
            {<<"regulator">>, {{complex, <<"Reference">>}, optional}},
            {<<"procedure">>, {{bbelement, <<"MedicinalProductAuthorization.Procedure">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicinalProductAuthorization.JurisdictionalAuthorization
%% The regulatory authorization of a medicinal product.
%%
    , <<"MedicinalProductAuthorization.JurisdictionalAuthorization">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"country">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"legalStatusOfSupply">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"validityPeriod">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicinalProductAuthorization.Procedure
%% The regulatory authorization of a medicinal product.
%%
    , <<"MedicinalProductAuthorization.Procedure">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"datePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"dateDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"application">>, {{bbelement, <<"MedicinalProductAuthorization.Procedure">>}, list}}
            ],
            [],
            [
            {<<"datePeriod">>, <<"dateDateTime">>}
            ]
} 
%%
%% MedicinalProductContraindication
%% The clinical particulars - indications, contraindications etc. of a medicinal product, including for regulatory purposes.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MedicinalProductContraindication">> => {<<"DomainResource">>,
            [
            {<<"subject">>, {{complex, <<"Reference">>}, list}},
            {<<"disease">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"diseaseStatus">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"comorbidity">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"therapeuticIndication">>, {{complex, <<"Reference">>}, list}},
            {<<"otherTherapy">>, {{bbelement, <<"MedicinalProductContraindication.OtherTherapy">>}, list}},
            {<<"population">>, {{bbelement, <<"Population">>}, list}}
            ],
            [],
            []
} 
%%
%% MedicinalProductContraindication.OtherTherapy
%% The clinical particulars - indications, contraindications etc. of a medicinal product, including for regulatory purposes.
%%
    , <<"MedicinalProductContraindication.OtherTherapy">> => {<<"BackboneElement">>,
            [
            {<<"therapyRelationshipType">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"medicationCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"medicationReference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            [
            {<<"medicationCodeableConcept">>, <<"medicationReference">>}
            ]
} 
%%
%% MedicinalProductIndication
%% Indication for the Medicinal Product.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MedicinalProductIndication">> => {<<"DomainResource">>,
            [
            {<<"subject">>, {{complex, <<"Reference">>}, list}},
            {<<"diseaseSymptomProcedure">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"diseaseStatus">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"comorbidity">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"intendedEffect">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"duration">>, {{complex, <<"Quantity">>}, optional}},
            {<<"otherTherapy">>, {{bbelement, <<"MedicinalProductIndication.OtherTherapy">>}, list}},
            {<<"undesirableEffect">>, {{complex, <<"Reference">>}, list}},
            {<<"population">>, {{bbelement, <<"Population">>}, list}}
            ],
            [],
            []
} 
%%
%% MedicinalProductIndication.OtherTherapy
%% Indication for the Medicinal Product.
%%
    , <<"MedicinalProductIndication.OtherTherapy">> => {<<"BackboneElement">>,
            [
            {<<"therapyRelationshipType">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"medicationCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"medicationReference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            [
            {<<"medicationCodeableConcept">>, <<"medicationReference">>}
            ]
} 
%%
%% MedicinalProductIngredient
%% An ingredient of a manufactured item or pharmaceutical product.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MedicinalProductIngredient">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"role">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"allergenicIndicator">>, {{primitive, <<"boolean">>}, optional}},
            {<<"manufacturer">>, {{complex, <<"Reference">>}, list}},
            {<<"specifiedSubstance">>, {{bbelement, <<"MedicinalProductIngredient.SpecifiedSubstance">>}, list}},
            {<<"substance">>, {{bbelement, <<"MedicinalProductIngredient.Substance">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicinalProductIngredient.SpecifiedSubstance
%% An ingredient of a manufactured item or pharmaceutical product.
%%
    , <<"MedicinalProductIngredient.SpecifiedSubstance">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"group">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"confidentiality">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"strength">>, {{bbelement, <<"MedicinalProductIngredient.Strength">>}, list}}
            ],
            [],
            []
} 
%%
%% MedicinalProductIngredient.Strength
%% An ingredient of a manufactured item or pharmaceutical product.
%%
    , <<"MedicinalProductIngredient.Strength">> => {<<"BackboneElement">>,
            [
            {<<"presentation">>, {{complex, <<"Ratio">>}, required}},
            {<<"presentationLowLimit">>, {{complex, <<"Ratio">>}, optional}},
            {<<"concentration">>, {{complex, <<"Ratio">>}, optional}},
            {<<"concentrationLowLimit">>, {{complex, <<"Ratio">>}, optional}},
            {<<"measurementPoint">>, {{primitive, <<"string">>}, optional}},
            {<<"country">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"referenceStrength">>, {{bbelement, <<"MedicinalProductIngredient.ReferenceStrength">>}, list}}
            ],
            [],
            []
} 
%%
%% MedicinalProductIngredient.ReferenceStrength
%% An ingredient of a manufactured item or pharmaceutical product.
%%
    , <<"MedicinalProductIngredient.ReferenceStrength">> => {<<"BackboneElement">>,
            [
            {<<"substance">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"strength">>, {{complex, <<"Ratio">>}, required}},
            {<<"strengthLowLimit">>, {{complex, <<"Ratio">>}, optional}},
            {<<"measurementPoint">>, {{primitive, <<"string">>}, optional}},
            {<<"country">>, {{complex, <<"CodeableConcept">>}, list}}
            ],
            [],
            []
} 
%%
%% MedicinalProductIngredient.Substance
%% An ingredient of a manufactured item or pharmaceutical product.
%%
    , <<"MedicinalProductIngredient.Substance">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"strength">>, {{bbelement, <<"MedicinalProductIngredient.Strength">>}, list}}
            ],
            [],
            []
} 
%%
%% MedicinalProductInteraction
%% The interactions of the medicinal product with other medicinal products, or other forms of interactions.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MedicinalProductInteraction">> => {<<"DomainResource">>,
            [
            {<<"subject">>, {{complex, <<"Reference">>}, list}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"interactant">>, {{bbelement, <<"MedicinalProductInteraction.Interactant">>}, list}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"effect">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"incidence">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"management">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicinalProductInteraction.Interactant
%% The interactions of the medicinal product with other medicinal products, or other forms of interactions.
%%
    , <<"MedicinalProductInteraction.Interactant">> => {<<"BackboneElement">>,
            [
            {<<"itemReference">>, {{complex, <<"Reference">>}, required}},
            {<<"itemCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}}
            ],
            [],
            [
            {<<"itemReference">>, <<"itemCodeableConcept">>}
            ]
} 
%%
%% MedicinalProductManufactured
%% The manufactured item as contained in the packaged medicinal product.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MedicinalProductManufactured">> => {<<"DomainResource">>,
            [
            {<<"manufacturedDoseForm">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"unitOfPresentation">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, required}},
            {<<"manufacturer">>, {{complex, <<"Reference">>}, list}},
            {<<"ingredient">>, {{complex, <<"Reference">>}, list}},
            {<<"physicalCharacteristics">>, {{bbelement, <<"ProdCharacteristic">>}, optional}},
            {<<"otherCharacteristics">>, {{complex, <<"CodeableConcept">>}, list}}
            ],
            [],
            []
} 
%%
%% MedicinalProductPackaged
%% A medicinal product in a container or package.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MedicinalProductPackaged">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"subject">>, {{complex, <<"Reference">>}, list}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"legalStatusOfSupply">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"marketingStatus">>, {{bbelement, <<"MarketingStatus">>}, list}},
            {<<"marketingAuthorization">>, {{complex, <<"Reference">>}, optional}},
            {<<"manufacturer">>, {{complex, <<"Reference">>}, list}},
            {<<"batchIdentifier">>, {{bbelement, <<"MedicinalProductPackaged.BatchIdentifier">>}, list}},
            {<<"packageItem">>, {{bbelement, <<"MedicinalProductPackaged.PackageItem">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% MedicinalProductPackaged.BatchIdentifier
%% A medicinal product in a container or package.
%%
    , <<"MedicinalProductPackaged.BatchIdentifier">> => {<<"BackboneElement">>,
            [
            {<<"outerPackaging">>, {{complex, <<"Identifier">>}, required}},
            {<<"immediatePackaging">>, {{complex, <<"Identifier">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicinalProductPackaged.PackageItem
%% A medicinal product in a container or package.
%%
    , <<"MedicinalProductPackaged.PackageItem">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, required}},
            {<<"material">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"alternateMaterial">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"device">>, {{complex, <<"Reference">>}, list}},
            {<<"manufacturedItem">>, {{complex, <<"Reference">>}, list}},
            {<<"packageItem">>, {{bbelement, <<"MedicinalProductPackaged.PackageItem">>}, list}},
            {<<"physicalCharacteristics">>, {{bbelement, <<"ProdCharacteristic">>}, optional}},
            {<<"otherCharacteristics">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"shelfLifeStorage">>, {{bbelement, <<"ProductShelfLife">>}, list}},
            {<<"manufacturer">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% MedicinalProductPharmaceutical
%% A pharmaceutical product described in terms of its composition and dose form.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MedicinalProductPharmaceutical">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"administrableDoseForm">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"unitOfPresentation">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"ingredient">>, {{complex, <<"Reference">>}, list}},
            {<<"device">>, {{complex, <<"Reference">>}, list}},
            {<<"characteristics">>, {{bbelement, <<"MedicinalProductPharmaceutical.Characteristics">>}, list}},
            {<<"routeOfAdministration">>, {{bbelement, <<"MedicinalProductPharmaceutical.RouteOfAdministration">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% MedicinalProductPharmaceutical.Characteristics
%% A pharmaceutical product described in terms of its composition and dose form.
%%
    , <<"MedicinalProductPharmaceutical.Characteristics">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"status">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicinalProductPharmaceutical.RouteOfAdministration
%% A pharmaceutical product described in terms of its composition and dose form.
%%
    , <<"MedicinalProductPharmaceutical.RouteOfAdministration">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"firstDose">>, {{complex, <<"Quantity">>}, optional}},
            {<<"maxSingleDose">>, {{complex, <<"Quantity">>}, optional}},
            {<<"maxDosePerDay">>, {{complex, <<"Quantity">>}, optional}},
            {<<"maxDosePerTreatmentPeriod">>, {{complex, <<"Ratio">>}, optional}},
            {<<"maxTreatmentPeriod">>, {{complex, <<"Duration">>}, optional}},
            {<<"targetSpecies">>, {{bbelement, <<"MedicinalProductPharmaceutical.TargetSpecies">>}, list}}
            ],
            [],
            []
} 
%%
%% MedicinalProductPharmaceutical.TargetSpecies
%% A pharmaceutical product described in terms of its composition and dose form.
%%
    , <<"MedicinalProductPharmaceutical.TargetSpecies">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"withdrawalPeriod">>, {{bbelement, <<"MedicinalProductPharmaceutical.WithdrawalPeriod">>}, list}}
            ],
            [],
            []
} 
%%
%% MedicinalProductPharmaceutical.WithdrawalPeriod
%% A pharmaceutical product described in terms of its composition and dose form.
%%
    , <<"MedicinalProductPharmaceutical.WithdrawalPeriod">> => {<<"BackboneElement">>,
            [
            {<<"tissue">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"value">>, {{complex, <<"Quantity">>}, required}},
            {<<"supportingInformation">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% MedicinalProductUndesirableEffect
%% Describe the undesirable effects of the medicinal product.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MedicinalProductUndesirableEffect">> => {<<"DomainResource">>,
            [
            {<<"subject">>, {{complex, <<"Reference">>}, list}},
            {<<"symptomConditionEffect">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"classification">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"frequencyOfOccurrence">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"population">>, {{bbelement, <<"Population">>}, list}}
            ],
            [],
            []
} 
%%
%% MessageDefinition
%% Defines the characteristics of a message that can be shared between systems, including the type of event that initiates the message, the content to be transmitted and what response(s), if any, are permitted.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MessageDefinition">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"replaces">>, {{primitive, <<"canonical">>}, list}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, required}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"base">>, {{primitive, <<"canonical">>}, optional}},
            {<<"parent">>, {{primitive, <<"canonical">>}, list}},
            {<<"eventCoding">>, {{complex, <<"Coding">>}, required}},
            {<<"eventUri">>, {{primitive, <<"uri">>}, required}},
            {<<"category">>, {{code, <<"messagesignificancecategory_list">>}, optional}},
            {<<"focus">>, {{bbelement, <<"MessageDefinition.Focus">>}, list}},
            {<<"responseRequired">>, {{code, <<"messageheaderresponserequest_list">>}, optional}},
            {<<"allowedResponse">>, {{bbelement, <<"MessageDefinition.AllowedResponse">>}, list}},
            {<<"graph">>, {{primitive, <<"canonical">>}, list}}
            ],
            [],
            [
            {<<"eventCoding">>, <<"eventUri">>}
            ]
} 
%%
%% MessageDefinition.Focus
%% Defines the characteristics of a message that can be shared between systems, including the type of event that initiates the message, the content to be transmitted and what response(s), if any, are permitted.
%%
    , <<"MessageDefinition.Focus">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{primitive, <<"code">>}, required}},
            {<<"profile">>, {{primitive, <<"canonical">>}, optional}},
            {<<"min">>, {{primitive, <<"unsignedInt">>}, required}},
            {<<"max">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% MessageDefinition.AllowedResponse
%% Defines the characteristics of a message that can be shared between systems, including the type of event that initiates the message, the content to be transmitted and what response(s), if any, are permitted.
%%
    , <<"MessageDefinition.AllowedResponse">> => {<<"BackboneElement">>,
            [
            {<<"message">>, {{primitive, <<"canonical">>}, required}},
            {<<"situation">>, {{primitive, <<"markdown">>}, optional}}
            ],
            [],
            []
} 
%%
%% MessageHeader
%% The header for a message exchange that is either requesting or responding to an action.  The reference(s) that are the subject of the action as well as other information related to the action are typically transmitted in a bundle in which the MessageHeader resource instance is the first resource in the bundle.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MessageHeader">> => {<<"DomainResource">>,
            [
            {<<"eventCoding">>, {{complex, <<"Coding">>}, required}},
            {<<"eventUri">>, {{primitive, <<"uri">>}, required}},
            {<<"destination">>, {{bbelement, <<"MessageHeader.Destination">>}, list}},
            {<<"sender">>, {{complex, <<"Reference">>}, optional}},
            {<<"enterer">>, {{complex, <<"Reference">>}, optional}},
            {<<"author">>, {{complex, <<"Reference">>}, optional}},
            {<<"source">>, {{bbelement, <<"MessageHeader.Source">>}, required}},
            {<<"responsible">>, {{complex, <<"Reference">>}, optional}},
            {<<"reason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"response">>, {{bbelement, <<"MessageHeader.Response">>}, optional}},
            {<<"focus">>, {{complex, <<"Reference">>}, list}},
            {<<"definition">>, {{primitive, <<"canonical">>}, optional}}
            ],
            [],
            [
            {<<"eventCoding">>, <<"eventUri">>}
            ]
} 
%%
%% MessageHeader.Destination
%% The header for a message exchange that is either requesting or responding to an action.  The reference(s) that are the subject of the action as well as other information related to the action are typically transmitted in a bundle in which the MessageHeader resource instance is the first resource in the bundle.
%%
    , <<"MessageHeader.Destination">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"target">>, {{complex, <<"Reference">>}, optional}},
            {<<"endpoint">>, {{primitive, <<"url">>}, required}},
            {<<"receiver">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% MessageHeader.Source
%% The header for a message exchange that is either requesting or responding to an action.  The reference(s) that are the subject of the action as well as other information related to the action are typically transmitted in a bundle in which the MessageHeader resource instance is the first resource in the bundle.
%%
    , <<"MessageHeader.Source">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"software">>, {{primitive, <<"string">>}, optional}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactPoint">>}, optional}},
            {<<"endpoint">>, {{primitive, <<"url">>}, required}}
            ],
            [],
            []
} 
%%
%% MessageHeader.Response
%% The header for a message exchange that is either requesting or responding to an action.  The reference(s) that are the subject of the action as well as other information related to the action are typically transmitted in a bundle in which the MessageHeader resource instance is the first resource in the bundle.
%%
    , <<"MessageHeader.Response">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{primitive, <<"id">>}, required}},
            {<<"code">>, {{code, <<"responsetype_list">>}, required}},
            {<<"details">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% MolecularSequence
%% Raw data describing a biological sequence.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"MolecularSequence">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"type">>, {{code, <<"sequencetype_list">>}, optional}},
            {<<"coordinateSystem">>, {{primitive, <<"integer">>}, required}},
            {<<"patient">>, {{complex, <<"Reference">>}, optional}},
            {<<"specimen">>, {{complex, <<"Reference">>}, optional}},
            {<<"device">>, {{complex, <<"Reference">>}, optional}},
            {<<"performer">>, {{complex, <<"Reference">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"referenceSeq">>, {{bbelement, <<"MolecularSequence.ReferenceSeq">>}, optional}},
            {<<"variant">>, {{bbelement, <<"MolecularSequence.Variant">>}, list}},
            {<<"observedSeq">>, {{primitive, <<"string">>}, optional}},
            {<<"quality">>, {{bbelement, <<"MolecularSequence.Quality">>}, list}},
            {<<"readCoverage">>, {{primitive, <<"integer">>}, optional}},
            {<<"repository">>, {{bbelement, <<"MolecularSequence.Repository">>}, list}},
            {<<"pointer">>, {{complex, <<"Reference">>}, list}},
            {<<"structureVariant">>, {{bbelement, <<"MolecularSequence.StructureVariant">>}, list}}
            ],
            [],
            []
} 
%%
%% MolecularSequence.ReferenceSeq
%% Raw data describing a biological sequence.
%%
    , <<"MolecularSequence.ReferenceSeq">> => {<<"BackboneElement">>,
            [
            {<<"chromosome">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"genomeBuild">>, {{primitive, <<"string">>}, optional}},
            {<<"orientation">>, {{code, <<"orientationtype_list">>}, optional}},
            {<<"referenceSeqId">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"referenceSeqPointer">>, {{complex, <<"Reference">>}, optional}},
            {<<"referenceSeqString">>, {{primitive, <<"string">>}, optional}},
            {<<"strand">>, {{code, <<"strandtype_list">>}, optional}},
            {<<"windowStart">>, {{primitive, <<"integer">>}, optional}},
            {<<"windowEnd">>, {{primitive, <<"integer">>}, optional}}
            ],
            [],
            []
} 
%%
%% MolecularSequence.Variant
%% Raw data describing a biological sequence.
%%
    , <<"MolecularSequence.Variant">> => {<<"BackboneElement">>,
            [
            {<<"start">>, {{primitive, <<"integer">>}, optional}},
            {<<"end">>, {{primitive, <<"integer">>}, optional}},
            {<<"observedAllele">>, {{primitive, <<"string">>}, optional}},
            {<<"referenceAllele">>, {{primitive, <<"string">>}, optional}},
            {<<"cigar">>, {{primitive, <<"string">>}, optional}},
            {<<"variantPointer">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% MolecularSequence.Quality
%% Raw data describing a biological sequence.
%%
    , <<"MolecularSequence.Quality">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{code, <<"qualitytype_list">>}, required}},
            {<<"standardSequence">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"start">>, {{primitive, <<"integer">>}, optional}},
            {<<"end">>, {{primitive, <<"integer">>}, optional}},
            {<<"score">>, {{complex, <<"Quantity">>}, optional}},
            {<<"method">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"truthTP">>, {{primitive, <<"decimal">>}, optional}},
            {<<"queryTP">>, {{primitive, <<"decimal">>}, optional}},
            {<<"truthFN">>, {{primitive, <<"decimal">>}, optional}},
            {<<"queryFP">>, {{primitive, <<"decimal">>}, optional}},
            {<<"gtFP">>, {{primitive, <<"decimal">>}, optional}},
            {<<"precision">>, {{primitive, <<"decimal">>}, optional}},
            {<<"recall">>, {{primitive, <<"decimal">>}, optional}},
            {<<"fScore">>, {{primitive, <<"decimal">>}, optional}},
            {<<"roc">>, {{bbelement, <<"MolecularSequence.Roc">>}, optional}}
            ],
            [],
            []
} 
%%
%% MolecularSequence.Roc
%% Raw data describing a biological sequence.
%%
    , <<"MolecularSequence.Roc">> => {<<"BackboneElement">>,
            [
            {<<"score">>, {{primitive, <<"integer">>}, list}},
            {<<"numTP">>, {{primitive, <<"integer">>}, list}},
            {<<"numFP">>, {{primitive, <<"integer">>}, list}},
            {<<"numFN">>, {{primitive, <<"integer">>}, list}},
            {<<"precision">>, {{primitive, <<"decimal">>}, list}},
            {<<"sensitivity">>, {{primitive, <<"decimal">>}, list}},
            {<<"fMeasure">>, {{primitive, <<"decimal">>}, list}}
            ],
            [],
            []
} 
%%
%% MolecularSequence.Repository
%% Raw data describing a biological sequence.
%%
    , <<"MolecularSequence.Repository">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{code, <<"repositorytype_list">>}, required}},
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"datasetId">>, {{primitive, <<"string">>}, optional}},
            {<<"variantsetId">>, {{primitive, <<"string">>}, optional}},
            {<<"readsetId">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% MolecularSequence.StructureVariant
%% Raw data describing a biological sequence.
%%
    , <<"MolecularSequence.StructureVariant">> => {<<"BackboneElement">>,
            [
            {<<"variantType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"exact">>, {{primitive, <<"boolean">>}, optional}},
            {<<"length">>, {{primitive, <<"integer">>}, optional}},
            {<<"outer">>, {{bbelement, <<"MolecularSequence.Outer">>}, optional}},
            {<<"inner">>, {{bbelement, <<"MolecularSequence.Inner">>}, optional}}
            ],
            [],
            []
} 
%%
%% MolecularSequence.Outer
%% Raw data describing a biological sequence.
%%
    , <<"MolecularSequence.Outer">> => {<<"BackboneElement">>,
            [
            {<<"start">>, {{primitive, <<"integer">>}, optional}},
            {<<"end">>, {{primitive, <<"integer">>}, optional}}
            ],
            [],
            []
} 
%%
%% MolecularSequence.Inner
%% Raw data describing a biological sequence.
%%
    , <<"MolecularSequence.Inner">> => {<<"BackboneElement">>,
            [
            {<<"start">>, {{primitive, <<"integer">>}, optional}},
            {<<"end">>, {{primitive, <<"integer">>}, optional}}
            ],
            [],
            []
} 
%%
%% NamingSystem
%% A curated namespace that issues unique symbols within that namespace for the identification of concepts, people, devices, etc.  Represents a "System" used within the Identifier and Coding data types.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"NamingSystem">> => {<<"DomainResource">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"kind">>, {{code, <<"namingsystemtype_list">>}, required}},
            {<<"date">>, {{primitive, <<"dateTime">>}, required}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"responsible">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"usage">>, {{primitive, <<"string">>}, optional}},
            {<<"uniqueId">>, {{bbelement, <<"NamingSystem.UniqueId">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% NamingSystem.UniqueId
%% A curated namespace that issues unique symbols within that namespace for the identification of concepts, people, devices, etc.  Represents a "System" used within the Identifier and Coding data types.
%%
    , <<"NamingSystem.UniqueId">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{code, <<"namingsystemidentifiertype_list">>}, required}},
            {<<"value">>, {{primitive, <<"string">>}, required}},
            {<<"preferred">>, {{primitive, <<"boolean">>}, optional}},
            {<<"comment">>, {{primitive, <<"string">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            []
} 
%%
%% NutritionOrder
%% A request to supply a diet, formula feeding (enteral) or oral nutritional supplement to a patient/resident.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"NutritionOrder">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"instantiatesCanonical">>, {{primitive, <<"canonical">>}, list}},
            {<<"instantiatesUri">>, {{primitive, <<"uri">>}, list}},
            {<<"instantiates">>, {{primitive, <<"uri">>}, list}},
            {<<"status">>, {{code, <<"requeststatus_list">>}, required}},
            {<<"intent">>, {{code, <<"requestintent_list">>}, required}},
            {<<"patient">>, {{complex, <<"Reference">>}, required}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"dateTime">>, {{primitive, <<"dateTime">>}, required}},
            {<<"orderer">>, {{complex, <<"Reference">>}, optional}},
            {<<"allergyIntolerance">>, {{complex, <<"Reference">>}, list}},
            {<<"foodPreferenceModifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"excludeFoodModifier">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"oralDiet">>, {{bbelement, <<"NutritionOrder.OralDiet">>}, optional}},
            {<<"supplement">>, {{bbelement, <<"NutritionOrder.Supplement">>}, list}},
            {<<"enteralFormula">>, {{bbelement, <<"NutritionOrder.EnteralFormula">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            []
} 
%%
%% NutritionOrder.OralDiet
%% A request to supply a diet, formula feeding (enteral) or oral nutritional supplement to a patient/resident.
%%
    , <<"NutritionOrder.OralDiet">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"schedule">>, {{bbelement, <<"Timing">>}, list}},
            {<<"nutrient">>, {{bbelement, <<"NutritionOrder.Nutrient">>}, list}},
            {<<"texture">>, {{bbelement, <<"NutritionOrder.Texture">>}, list}},
            {<<"fluidConsistencyType">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"instruction">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% NutritionOrder.Nutrient
%% A request to supply a diet, formula feeding (enteral) or oral nutritional supplement to a patient/resident.
%%
    , <<"NutritionOrder.Nutrient">> => {<<"BackboneElement">>,
            [
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"amount">>, {{complex, <<"Quantity">>}, optional}}
            ],
            [],
            []
} 
%%
%% NutritionOrder.Texture
%% A request to supply a diet, formula feeding (enteral) or oral nutritional supplement to a patient/resident.
%%
    , <<"NutritionOrder.Texture">> => {<<"BackboneElement">>,
            [
            {<<"modifier">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"foodType">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% NutritionOrder.Supplement
%% A request to supply a diet, formula feeding (enteral) or oral nutritional supplement to a patient/resident.
%%
    , <<"NutritionOrder.Supplement">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"productName">>, {{primitive, <<"string">>}, optional}},
            {<<"schedule">>, {{bbelement, <<"Timing">>}, list}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"instruction">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% NutritionOrder.EnteralFormula
%% A request to supply a diet, formula feeding (enteral) or oral nutritional supplement to a patient/resident.
%%
    , <<"NutritionOrder.EnteralFormula">> => {<<"BackboneElement">>,
            [
            {<<"baseFormulaType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"baseFormulaProductName">>, {{primitive, <<"string">>}, optional}},
            {<<"additiveType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"additiveProductName">>, {{primitive, <<"string">>}, optional}},
            {<<"caloricDensity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"routeofAdministration">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"administration">>, {{bbelement, <<"NutritionOrder.Administration">>}, list}},
            {<<"maxVolumeToDeliver">>, {{complex, <<"Quantity">>}, optional}},
            {<<"administrationInstruction">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% NutritionOrder.Administration
%% A request to supply a diet, formula feeding (enteral) or oral nutritional supplement to a patient/resident.
%%
    , <<"NutritionOrder.Administration">> => {<<"BackboneElement">>,
            [
            {<<"schedule">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"rateQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"rateRatio">>, {{complex, <<"Ratio">>}, optional}}
            ],
            [],
            [
            {<<"rateQuantity">>, <<"rateRatio">>}
            ]
} 
%%
%% Observation
%% Measurements and simple assertions made about a patient, device or other subject.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Observation">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"partOf">>, {{complex, <<"Reference">>}, list}},
            {<<"status">>, {{code, <<"observationstatus_list">>}, required}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"focus">>, {{complex, <<"Reference">>}, list}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"effectiveDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"effectiveTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"effectiveInstant">>, {{primitive, <<"instant">>}, optional}},
            {<<"issued">>, {{primitive, <<"instant">>}, optional}},
            {<<"performer">>, {{complex, <<"Reference">>}, list}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"valueCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"valueString">>, {{primitive, <<"string">>}, optional}},
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"valueInteger">>, {{primitive, <<"integer">>}, optional}},
            {<<"valueRange">>, {{complex, <<"Range">>}, optional}},
            {<<"valueRatio">>, {{complex, <<"Ratio">>}, optional}},
            {<<"valueSampledData">>, {{complex, <<"SampledData">>}, optional}},
            {<<"valueTime">>, {{primitive, <<"time">>}, optional}},
            {<<"valueDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"valuePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"dataAbsentReason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"interpretation">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"bodySite">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"method">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"specimen">>, {{complex, <<"Reference">>}, optional}},
            {<<"device">>, {{complex, <<"Reference">>}, optional}},
            {<<"referenceRange">>, {{bbelement, <<"Observation.ReferenceRange">>}, list}},
            {<<"hasMember">>, {{complex, <<"Reference">>}, list}},
            {<<"derivedFrom">>, {{complex, <<"Reference">>}, list}},
            {<<"component">>, {{bbelement, <<"Observation.Component">>}, list}}
            ],
            [],
            [
            {<<"effectiveDateTime">>, <<"effectivePeriod">>, <<"effectiveTiming">>, <<"effectiveInstant">>}, 
            {<<"valueQuantity">>, <<"valueCodeableConcept">>, <<"valueString">>, <<"valueBoolean">>, <<"valueInteger">>, <<"valueRange">>, <<"valueRatio">>, <<"valueSampledData">>, <<"valueTime">>, <<"valueDateTime">>, <<"valuePeriod">>}
            ]
} 
%%
%% Observation.ReferenceRange
%% Measurements and simple assertions made about a patient, device or other subject.
%%
    , <<"Observation.ReferenceRange">> => {<<"BackboneElement">>,
            [
            {<<"low">>, {{complex, <<"Quantity">>}, optional}},
            {<<"high">>, {{complex, <<"Quantity">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"appliesTo">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"age">>, {{complex, <<"Range">>}, optional}},
            {<<"text">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% Observation.Component
%% Measurements and simple assertions made about a patient, device or other subject.
%%
    , <<"Observation.Component">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"valueCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"valueString">>, {{primitive, <<"string">>}, optional}},
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"valueInteger">>, {{primitive, <<"integer">>}, optional}},
            {<<"valueRange">>, {{complex, <<"Range">>}, optional}},
            {<<"valueRatio">>, {{complex, <<"Ratio">>}, optional}},
            {<<"valueSampledData">>, {{complex, <<"SampledData">>}, optional}},
            {<<"valueTime">>, {{primitive, <<"time">>}, optional}},
            {<<"valueDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"valuePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"dataAbsentReason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"interpretation">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"referenceRange">>, {{bbelement, <<"Observation.ReferenceRange">>}, list}}
            ],
            [],
            [
            {<<"valueQuantity">>, <<"valueCodeableConcept">>, <<"valueString">>, <<"valueBoolean">>, <<"valueInteger">>, <<"valueRange">>, <<"valueRatio">>, <<"valueSampledData">>, <<"valueTime">>, <<"valueDateTime">>, <<"valuePeriod">>}
            ]
} 
%%
%% ObservationDefinition
%% Set of definitional characteristics for a kind of observation or measurement produced or consumed by an orderable health care service.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ObservationDefinition">> => {<<"DomainResource">>,
            [
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"permittedDataType">>, {{code, <<"observationdatatype_list">>}, list}},
            {<<"multipleResultsAllowed">>, {{primitive, <<"boolean">>}, optional}},
            {<<"method">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"preferredReportName">>, {{primitive, <<"string">>}, optional}},
            {<<"quantitativeDetails">>, {{bbelement, <<"ObservationDefinition.QuantitativeDetails">>}, optional}},
            {<<"qualifiedInterval">>, {{bbelement, <<"ObservationDefinition.QualifiedInterval">>}, list}},
            {<<"validCodedValueSet">>, {{complex, <<"Reference">>}, optional}},
            {<<"normalCodedValueSet">>, {{complex, <<"Reference">>}, optional}},
            {<<"abnormalCodedValueSet">>, {{complex, <<"Reference">>}, optional}},
            {<<"criticalCodedValueSet">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% ObservationDefinition.QuantitativeDetails
%% Set of definitional characteristics for a kind of observation or measurement produced or consumed by an orderable health care service.
%%
    , <<"ObservationDefinition.QuantitativeDetails">> => {<<"BackboneElement">>,
            [
            {<<"customaryUnit">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"unit">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"conversionFactor">>, {{primitive, <<"decimal">>}, optional}},
            {<<"decimalPrecision">>, {{primitive, <<"integer">>}, optional}}
            ],
            [],
            []
} 
%%
%% ObservationDefinition.QualifiedInterval
%% Set of definitional characteristics for a kind of observation or measurement produced or consumed by an orderable health care service.
%%
    , <<"ObservationDefinition.QualifiedInterval">> => {<<"BackboneElement">>,
            [
            {<<"category">>, {{code, <<"observationrangecategory_list">>}, optional}},
            {<<"range">>, {{complex, <<"Range">>}, optional}},
            {<<"context">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"appliesTo">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"gender">>, {{code, <<"administrativegender_list">>}, optional}},
            {<<"age">>, {{complex, <<"Range">>}, optional}},
            {<<"gestationalAge">>, {{complex, <<"Range">>}, optional}},
            {<<"condition">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% OperationDefinition
%% A formal computable definition of an operation (on the RESTful interface) or a named query (using the search interaction).
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"OperationDefinition">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"kind">>, {{code, <<"operationkind_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"affectsState">>, {{primitive, <<"boolean">>}, optional}},
            {<<"code">>, {{primitive, <<"code">>}, required}},
            {<<"comment">>, {{primitive, <<"markdown">>}, optional}},
            {<<"base">>, {{primitive, <<"canonical">>}, optional}},
            {<<"resource">>, {{primitive, <<"code">>}, list}},
            {<<"system">>, {{primitive, <<"boolean">>}, required}},
            {<<"type">>, {{primitive, <<"boolean">>}, required}},
            {<<"instance">>, {{primitive, <<"boolean">>}, required}},
            {<<"inputProfile">>, {{primitive, <<"canonical">>}, optional}},
            {<<"outputProfile">>, {{primitive, <<"canonical">>}, optional}},
            {<<"parameter">>, {{bbelement, <<"OperationDefinition.Parameter">>}, list}},
            {<<"overload">>, {{bbelement, <<"OperationDefinition.Overload">>}, list}}
            ],
            [],
            []
} 
%%
%% OperationDefinition.Parameter
%% A formal computable definition of an operation (on the RESTful interface) or a named query (using the search interaction).
%%
    , <<"OperationDefinition.Parameter">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"code">>}, required}},
            {<<"use">>, {{code, <<"operationparameteruse_list">>}, required}},
            {<<"min">>, {{primitive, <<"integer">>}, required}},
            {<<"max">>, {{primitive, <<"string">>}, required}},
            {<<"documentation">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{primitive, <<"code">>}, optional}},
            {<<"targetProfile">>, {{primitive, <<"canonical">>}, list}},
            {<<"searchType">>, {{code, <<"searchparamtype_list">>}, optional}},
            {<<"binding">>, {{bbelement, <<"OperationDefinition.Binding">>}, optional}},
            {<<"referencedFrom">>, {{bbelement, <<"OperationDefinition.ReferencedFrom">>}, list}},
            {<<"part">>, {{bbelement, <<"OperationDefinition.Parameter">>}, list}}
            ],
            [],
            []
} 
%%
%% OperationDefinition.Binding
%% A formal computable definition of an operation (on the RESTful interface) or a named query (using the search interaction).
%%
    , <<"OperationDefinition.Binding">> => {<<"BackboneElement">>,
            [
            {<<"strength">>, {{code, <<"bindingstrength_list">>}, required}},
            {<<"valueSet">>, {{primitive, <<"canonical">>}, required}}
            ],
            [],
            []
} 
%%
%% OperationDefinition.ReferencedFrom
%% A formal computable definition of an operation (on the RESTful interface) or a named query (using the search interaction).
%%
    , <<"OperationDefinition.ReferencedFrom">> => {<<"BackboneElement">>,
            [
            {<<"source">>, {{primitive, <<"string">>}, required}},
            {<<"sourceId">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% OperationDefinition.Overload
%% A formal computable definition of an operation (on the RESTful interface) or a named query (using the search interaction).
%%
    , <<"OperationDefinition.Overload">> => {<<"BackboneElement">>,
            [
            {<<"parameterName">>, {{primitive, <<"string">>}, list}},
            {<<"comment">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% OperationOutcome
%% A collection of error, warning, or information messages that result from a system action.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"OperationOutcome">> => {<<"DomainResource">>,
            [
            {<<"issue">>, {{bbelement, <<"OperationOutcome.Issue">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% OperationOutcome.Issue
%% A collection of error, warning, or information messages that result from a system action.
%%
    , <<"OperationOutcome.Issue">> => {<<"BackboneElement">>,
            [
            {<<"severity">>, {{code, <<"issueseverity_list">>}, required}},
            {<<"code">>, {{code, <<"issuetype_list">>}, required}},
            {<<"details">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"diagnostics">>, {{primitive, <<"string">>}, optional}},
            {<<"location">>, {{primitive, <<"string">>}, list}},
            {<<"expression">>, {{primitive, <<"string">>}, list}}
            ],
            [],
            []
} 
%%
%% Organization
%% A formally or informally recognized grouping of people or organizations formed for the purpose of achieving some form of collective action.  Includes companies, institutions, corporations, departments, community groups, healthcare practice groups, payer/insurer, etc.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Organization">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"active">>, {{primitive, <<"boolean">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"alias">>, {{primitive, <<"string">>}, list}},
            {<<"telecom">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"address">>, {{complex, <<"Address">>}, list}},
            {<<"partOf">>, {{complex, <<"Reference">>}, optional}},
            {<<"contact">>, {{bbelement, <<"Organization.Contact">>}, list}},
            {<<"endpoint">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% Organization.Contact
%% A formally or informally recognized grouping of people or organizations formed for the purpose of achieving some form of collective action.  Includes companies, institutions, corporations, departments, community groups, healthcare practice groups, payer/insurer, etc.
%%
    , <<"Organization.Contact">> => {<<"BackboneElement">>,
            [
            {<<"purpose">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"name">>, {{complex, <<"HumanName">>}, optional}},
            {<<"telecom">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"address">>, {{complex, <<"Address">>}, optional}}
            ],
            [],
            []
} 
%%
%% OrganizationAffiliation
%% Defines an affiliation/assotiation/relationship between 2 distinct oganizations, that is not a part-of relationship/sub-division relationship.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"OrganizationAffiliation">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"active">>, {{primitive, <<"boolean">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"organization">>, {{complex, <<"Reference">>}, optional}},
            {<<"participatingOrganization">>, {{complex, <<"Reference">>}, optional}},
            {<<"network">>, {{complex, <<"Reference">>}, list}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"specialty">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"location">>, {{complex, <<"Reference">>}, list}},
            {<<"healthcareService">>, {{complex, <<"Reference">>}, list}},
            {<<"telecom">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"endpoint">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% Parameters
%% This resource is a non-persisted resource used to pass information into and back from an [operation](operations.html). It has no other use, and there is no RESTful endpoint associated with it.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Parameters">> => {<<"Resource">>,
            [
            {<<"parameter">>, {{bbelement, <<"Parameters.Parameter">>}, list}}
            ],
            [],
            []
} 
%%
%% Parameters.Parameter
%% This resource is a non-persisted resource used to pass information into and back from an [operation](operations.html). It has no other use, and there is no RESTful endpoint associated with it.
%%
    , <<"Parameters.Parameter">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"valueBase64Binary">>, {{primitive, <<"base64Binary">>}, optional}},
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"valueCanonical">>, {{primitive, <<"canonical">>}, optional}},
            {<<"valueCode">>, {{primitive, <<"code">>}, optional}},
            {<<"valueDate">>, {{primitive, <<"date">>}, optional}},
            {<<"valueDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"valueDecimal">>, {{primitive, <<"decimal">>}, optional}},
            {<<"valueId">>, {{primitive, <<"id">>}, optional}},
            {<<"valueInstant">>, {{primitive, <<"instant">>}, optional}},
            {<<"valueInteger">>, {{primitive, <<"integer">>}, optional}},
            {<<"valueMarkdown">>, {{primitive, <<"markdown">>}, optional}},
            {<<"valueOid">>, {{primitive, <<"oid">>}, optional}},
            {<<"valuePositiveInt">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"valueString">>, {{primitive, <<"string">>}, optional}},
            {<<"valueTime">>, {{primitive, <<"time">>}, optional}},
            {<<"valueUnsignedInt">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"valueUri">>, {{primitive, <<"uri">>}, optional}},
            {<<"valueUrl">>, {{primitive, <<"url">>}, optional}},
            {<<"valueUuid">>, {{primitive, <<"uuid">>}, optional}},
            {<<"valueAddress">>, {{complex, <<"Address">>}, optional}},
            {<<"valueAge">>, {{complex, <<"Age">>}, optional}},
            {<<"valueAnnotation">>, {{complex, <<"Annotation">>}, optional}},
            {<<"valueAttachment">>, {{complex, <<"Attachment">>}, optional}},
            {<<"valueCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"valueCoding">>, {{complex, <<"Coding">>}, optional}},
            {<<"valueContactPoint">>, {{complex, <<"ContactPoint">>}, optional}},
            {<<"valueCount">>, {{complex, <<"Count">>}, optional}},
            {<<"valueDistance">>, {{complex, <<"Distance">>}, optional}},
            {<<"valueDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"valueHumanName">>, {{complex, <<"HumanName">>}, optional}},
            {<<"valueIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"valueMoney">>, {{complex, <<"Money">>}, optional}},
            {<<"valuePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"valueRange">>, {{complex, <<"Range">>}, optional}},
            {<<"valueRatio">>, {{complex, <<"Ratio">>}, optional}},
            {<<"valueReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"valueSampledData">>, {{complex, <<"SampledData">>}, optional}},
            {<<"valueSignature">>, {{complex, <<"Signature">>}, optional}},
            {<<"valueTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"valueContactDetail">>, {{complex, <<"ContactDetail">>}, optional}},
            {<<"valueContributor">>, {{complex, <<"Contributor">>}, optional}},
            {<<"valueDataRequirement">>, {{complex, <<"DataRequirement">>}, optional}},
            {<<"valueExpression">>, {{complex, <<"Expression">>}, optional}},
            {<<"valueParameterDefinition">>, {{complex, <<"ParameterDefinition">>}, optional}},
            {<<"valueRelatedArtifact">>, {{complex, <<"RelatedArtifact">>}, optional}},
            {<<"valueTriggerDefinition">>, {{complex, <<"TriggerDefinition">>}, optional}},
            {<<"valueUsageContext">>, {{complex, <<"UsageContext">>}, optional}},
            {<<"valueDosage">>, {{bbelement, <<"Dosage">>}, optional}},
            {<<"resource">>, {{complex, <<"ResourceContainer">>}, optional}},
            {<<"part">>, {{bbelement, <<"Parameters.Parameter">>}, list}}
            ],
            [],
            [
            {<<"valueBase64Binary">>, <<"valueBoolean">>, <<"valueCanonical">>, <<"valueCode">>, <<"valueDate">>, <<"valueDateTime">>, <<"valueDecimal">>, <<"valueId">>, <<"valueInstant">>, <<"valueInteger">>, <<"valueMarkdown">>, <<"valueOid">>, <<"valuePositiveInt">>, <<"valueString">>, <<"valueTime">>, <<"valueUnsignedInt">>, <<"valueUri">>, <<"valueUrl">>, <<"valueUuid">>, <<"valueAddress">>, <<"valueAge">>, <<"valueAnnotation">>, <<"valueAttachment">>, <<"valueCodeableConcept">>, <<"valueCoding">>, <<"valueContactPoint">>, <<"valueCount">>, <<"valueDistance">>, <<"valueDuration">>, <<"valueHumanName">>, <<"valueIdentifier">>, <<"valueMoney">>, <<"valuePeriod">>, <<"valueQuantity">>, <<"valueRange">>, <<"valueRatio">>, <<"valueReference">>, <<"valueSampledData">>, <<"valueSignature">>, <<"valueTiming">>, <<"valueContactDetail">>, <<"valueContributor">>, <<"valueDataRequirement">>, <<"valueExpression">>, <<"valueParameterDefinition">>, <<"valueRelatedArtifact">>, <<"valueTriggerDefinition">>, <<"valueUsageContext">>, <<"valueDosage">>}
            ]
} 
%%
%% Patient
%% Demographics and other administrative information about an individual or animal receiving care or other health-related services.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Patient">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"active">>, {{primitive, <<"boolean">>}, optional}},
            {<<"name">>, {{complex, <<"HumanName">>}, list}},
            {<<"telecom">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"gender">>, {{code, <<"administrativegender_list">>}, optional}},
            {<<"birthDate">>, {{primitive, <<"date">>}, optional}},
            {<<"deceasedBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"deceasedDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"address">>, {{complex, <<"Address">>}, list}},
            {<<"maritalStatus">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"multipleBirthBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"multipleBirthInteger">>, {{primitive, <<"integer">>}, optional}},
            {<<"photo">>, {{complex, <<"Attachment">>}, list}},
            {<<"contact">>, {{bbelement, <<"Patient.Contact">>}, list}},
            {<<"communication">>, {{bbelement, <<"Patient.Communication">>}, list}},
            {<<"generalPractitioner">>, {{complex, <<"Reference">>}, list}},
            {<<"managingOrganization">>, {{complex, <<"Reference">>}, optional}},
            {<<"link">>, {{bbelement, <<"Patient.Link">>}, list}}
            ],
            [],
            [
            {<<"deceasedBoolean">>, <<"deceasedDateTime">>}, 
            {<<"multipleBirthBoolean">>, <<"multipleBirthInteger">>}
            ]
} 
%%
%% Patient.Contact
%% Demographics and other administrative information about an individual or animal receiving care or other health-related services.
%%
    , <<"Patient.Contact">> => {<<"BackboneElement">>,
            [
            {<<"relationship">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"name">>, {{complex, <<"HumanName">>}, optional}},
            {<<"telecom">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"address">>, {{complex, <<"Address">>}, optional}},
            {<<"gender">>, {{code, <<"administrativegender_list">>}, optional}},
            {<<"organization">>, {{complex, <<"Reference">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            []
} 
%%
%% Patient.Communication
%% Demographics and other administrative information about an individual or animal receiving care or other health-related services.
%%
    , <<"Patient.Communication">> => {<<"BackboneElement">>,
            [
            {<<"language">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"preferred">>, {{primitive, <<"boolean">>}, optional}}
            ],
            [],
            []
} 
%%
%% Patient.Link
%% Demographics and other administrative information about an individual or animal receiving care or other health-related services.
%%
    , <<"Patient.Link">> => {<<"BackboneElement">>,
            [
            {<<"other">>, {{complex, <<"Reference">>}, required}},
            {<<"type">>, {{code, <<"linktype_list">>}, required}}
            ],
            [],
            []
} 
%%
%% PaymentNotice
%% This resource provides the status of the payment for goods and services rendered, and the request and response resource references.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"PaymentNotice">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"financialresourcestatuscodes_list">>}, required}},
            {<<"request">>, {{complex, <<"Reference">>}, optional}},
            {<<"response">>, {{complex, <<"Reference">>}, optional}},
            {<<"created">>, {{primitive, <<"dateTime">>}, required}},
            {<<"provider">>, {{complex, <<"Reference">>}, optional}},
            {<<"payment">>, {{complex, <<"Reference">>}, required}},
            {<<"paymentDate">>, {{primitive, <<"date">>}, optional}},
            {<<"payee">>, {{complex, <<"Reference">>}, optional}},
            {<<"recipient">>, {{complex, <<"Reference">>}, required}},
            {<<"amount">>, {{complex, <<"Money">>}, required}},
            {<<"paymentStatus">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% PaymentReconciliation
%% This resource provides the details including amount of a payment and allocates the payment items being paid.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"PaymentReconciliation">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"financialresourcestatuscodes_list">>}, required}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"created">>, {{primitive, <<"dateTime">>}, required}},
            {<<"paymentIssuer">>, {{complex, <<"Reference">>}, optional}},
            {<<"request">>, {{complex, <<"Reference">>}, optional}},
            {<<"requestor">>, {{complex, <<"Reference">>}, optional}},
            {<<"outcome">>, {{code, <<"remittanceoutcome_list">>}, optional}},
            {<<"disposition">>, {{primitive, <<"string">>}, optional}},
            {<<"paymentDate">>, {{primitive, <<"date">>}, required}},
            {<<"paymentAmount">>, {{complex, <<"Money">>}, required}},
            {<<"paymentIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"detail">>, {{bbelement, <<"PaymentReconciliation.Detail">>}, list}},
            {<<"formCode">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"processNote">>, {{bbelement, <<"PaymentReconciliation.ProcessNote">>}, list}}
            ],
            [],
            []
} 
%%
%% PaymentReconciliation.Detail
%% This resource provides the details including amount of a payment and allocates the payment items being paid.
%%
    , <<"PaymentReconciliation.Detail">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"predecessor">>, {{complex, <<"Identifier">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"request">>, {{complex, <<"Reference">>}, optional}},
            {<<"submitter">>, {{complex, <<"Reference">>}, optional}},
            {<<"response">>, {{complex, <<"Reference">>}, optional}},
            {<<"date">>, {{primitive, <<"date">>}, optional}},
            {<<"responsible">>, {{complex, <<"Reference">>}, optional}},
            {<<"payee">>, {{complex, <<"Reference">>}, optional}},
            {<<"amount">>, {{complex, <<"Money">>}, optional}}
            ],
            [],
            []
} 
%%
%% PaymentReconciliation.ProcessNote
%% This resource provides the details including amount of a payment and allocates the payment items being paid.
%%
    , <<"PaymentReconciliation.ProcessNote">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{code, <<"notetype_list">>}, optional}},
            {<<"text">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% Person
%% Demographics and administrative information about a person independent of a specific health-related context.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Person">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"name">>, {{complex, <<"HumanName">>}, list}},
            {<<"telecom">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"gender">>, {{code, <<"administrativegender_list">>}, optional}},
            {<<"birthDate">>, {{primitive, <<"date">>}, optional}},
            {<<"address">>, {{complex, <<"Address">>}, list}},
            {<<"photo">>, {{complex, <<"Attachment">>}, optional}},
            {<<"managingOrganization">>, {{complex, <<"Reference">>}, optional}},
            {<<"active">>, {{primitive, <<"boolean">>}, optional}},
            {<<"link">>, {{bbelement, <<"Person.Link">>}, list}}
            ],
            [],
            []
} 
%%
%% Person.Link
%% Demographics and administrative information about a person independent of a specific health-related context.
%%
    , <<"Person.Link">> => {<<"BackboneElement">>,
            [
            {<<"target">>, {{complex, <<"Reference">>}, required}},
            {<<"assurance">>, {{code, <<"identityassurancelevel_list">>}, optional}}
            ],
            [],
            []
} 
%%
%% PlanDefinition
%% This resource allows for the definition of various types of plans as a sharable, consumable, and executable artifact. The resource is general enough to support the description of a broad range of clinical artifacts such as clinical decision support rules, order sets and protocols.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"PlanDefinition">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"subtitle">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"subjectCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subjectReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"usage">>, {{primitive, <<"string">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"approvalDate">>, {{primitive, <<"date">>}, optional}},
            {<<"lastReviewDate">>, {{primitive, <<"date">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"topic">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"author">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"editor">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"reviewer">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"endorser">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"relatedArtifact">>, {{complex, <<"RelatedArtifact">>}, list}},
            {<<"library">>, {{primitive, <<"canonical">>}, list}},
            {<<"goal">>, {{bbelement, <<"PlanDefinition.Goal">>}, list}},
            {<<"action">>, {{bbelement, <<"PlanDefinition.Action">>}, list}}
            ],
            [],
            [
            {<<"subjectCodeableConcept">>, <<"subjectReference">>}
            ]
} 
%%
%% PlanDefinition.Goal
%% This resource allows for the definition of various types of plans as a sharable, consumable, and executable artifact. The resource is general enough to support the description of a broad range of clinical artifacts such as clinical decision support rules, order sets and protocols.
%%
    , <<"PlanDefinition.Goal">> => {<<"BackboneElement">>,
            [
            {<<"category">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"description">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"priority">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"start">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"addresses">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"documentation">>, {{complex, <<"RelatedArtifact">>}, list}},
            {<<"target">>, {{bbelement, <<"PlanDefinition.Target">>}, list}}
            ],
            [],
            []
} 
%%
%% PlanDefinition.Target
%% This resource allows for the definition of various types of plans as a sharable, consumable, and executable artifact. The resource is general enough to support the description of a broad range of clinical artifacts such as clinical decision support rules, order sets and protocols.
%%
    , <<"PlanDefinition.Target">> => {<<"BackboneElement">>,
            [
            {<<"measure">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"detailQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"detailRange">>, {{complex, <<"Range">>}, optional}},
            {<<"detailCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"due">>, {{complex, <<"Duration">>}, optional}}
            ],
            [],
            [
            {<<"detailQuantity">>, <<"detailRange">>, <<"detailCodeableConcept">>}
            ]
} 
%%
%% PlanDefinition.Action
%% This resource allows for the definition of various types of plans as a sharable, consumable, and executable artifact. The resource is general enough to support the description of a broad range of clinical artifacts such as clinical decision support rules, order sets and protocols.
%%
    , <<"PlanDefinition.Action">> => {<<"BackboneElement">>,
            [
            {<<"prefix">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"textEquivalent">>, {{primitive, <<"string">>}, optional}},
            {<<"priority">>, {{code, <<"requestpriority_list">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reason">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"documentation">>, {{complex, <<"RelatedArtifact">>}, list}},
            {<<"goalId">>, {{primitive, <<"id">>}, list}},
            {<<"subjectCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subjectReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"trigger">>, {{complex, <<"TriggerDefinition">>}, list}},
            {<<"condition">>, {{bbelement, <<"PlanDefinition.Condition">>}, list}},
            {<<"input">>, {{complex, <<"DataRequirement">>}, list}},
            {<<"output">>, {{complex, <<"DataRequirement">>}, list}},
            {<<"relatedAction">>, {{bbelement, <<"PlanDefinition.RelatedAction">>}, list}},
            {<<"timingDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"timingAge">>, {{complex, <<"Age">>}, optional}},
            {<<"timingPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"timingDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"timingRange">>, {{complex, <<"Range">>}, optional}},
            {<<"timingTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"participant">>, {{bbelement, <<"PlanDefinition.Participant">>}, list}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"groupingBehavior">>, {{code, <<"actiongroupingbehavior_list">>}, optional}},
            {<<"selectionBehavior">>, {{code, <<"actionselectionbehavior_list">>}, optional}},
            {<<"requiredBehavior">>, {{code, <<"actionrequiredbehavior_list">>}, optional}},
            {<<"precheckBehavior">>, {{code, <<"actionprecheckbehavior_list">>}, optional}},
            {<<"cardinalityBehavior">>, {{code, <<"actioncardinalitybehavior_list">>}, optional}},
            {<<"definitionCanonical">>, {{primitive, <<"canonical">>}, optional}},
            {<<"definitionUri">>, {{primitive, <<"uri">>}, optional}},
            {<<"transform">>, {{primitive, <<"canonical">>}, optional}},
            {<<"dynamicValue">>, {{bbelement, <<"PlanDefinition.DynamicValue">>}, list}},
            {<<"action">>, {{bbelement, <<"PlanDefinition.Action">>}, list}}
            ],
            [],
            [
            {<<"subjectCodeableConcept">>, <<"subjectReference">>}, 
            {<<"timingDateTime">>, <<"timingAge">>, <<"timingPeriod">>, <<"timingDuration">>, <<"timingRange">>, <<"timingTiming">>}, 
            {<<"definitionCanonical">>, <<"definitionUri">>}
            ]
} 
%%
%% PlanDefinition.Condition
%% This resource allows for the definition of various types of plans as a sharable, consumable, and executable artifact. The resource is general enough to support the description of a broad range of clinical artifacts such as clinical decision support rules, order sets and protocols.
%%
    , <<"PlanDefinition.Condition">> => {<<"BackboneElement">>,
            [
            {<<"kind">>, {{code, <<"actionconditionkind_list">>}, required}},
            {<<"expression">>, {{complex, <<"Expression">>}, optional}}
            ],
            [],
            []
} 
%%
%% PlanDefinition.RelatedAction
%% This resource allows for the definition of various types of plans as a sharable, consumable, and executable artifact. The resource is general enough to support the description of a broad range of clinical artifacts such as clinical decision support rules, order sets and protocols.
%%
    , <<"PlanDefinition.RelatedAction">> => {<<"BackboneElement">>,
            [
            {<<"actionId">>, {{primitive, <<"id">>}, required}},
            {<<"relationship">>, {{code, <<"actionrelationshiptype_list">>}, required}},
            {<<"offsetDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"offsetRange">>, {{complex, <<"Range">>}, optional}}
            ],
            [],
            [
            {<<"offsetDuration">>, <<"offsetRange">>}
            ]
} 
%%
%% PlanDefinition.Participant
%% This resource allows for the definition of various types of plans as a sharable, consumable, and executable artifact. The resource is general enough to support the description of a broad range of clinical artifacts such as clinical decision support rules, order sets and protocols.
%%
    , <<"PlanDefinition.Participant">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{code, <<"actionparticipanttype_list">>}, required}},
            {<<"role">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% PlanDefinition.DynamicValue
%% This resource allows for the definition of various types of plans as a sharable, consumable, and executable artifact. The resource is general enough to support the description of a broad range of clinical artifacts such as clinical decision support rules, order sets and protocols.
%%
    , <<"PlanDefinition.DynamicValue">> => {<<"BackboneElement">>,
            [
            {<<"path">>, {{primitive, <<"string">>}, optional}},
            {<<"expression">>, {{complex, <<"Expression">>}, optional}}
            ],
            [],
            []
} 
%%
%% Practitioner
%% A person who is directly or indirectly involved in the provisioning of healthcare.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Practitioner">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"active">>, {{primitive, <<"boolean">>}, optional}},
            {<<"name">>, {{complex, <<"HumanName">>}, list}},
            {<<"telecom">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"address">>, {{complex, <<"Address">>}, list}},
            {<<"gender">>, {{code, <<"administrativegender_list">>}, optional}},
            {<<"birthDate">>, {{primitive, <<"date">>}, optional}},
            {<<"photo">>, {{complex, <<"Attachment">>}, list}},
            {<<"qualification">>, {{bbelement, <<"Practitioner.Qualification">>}, list}},
            {<<"communication">>, {{complex, <<"CodeableConcept">>}, list}}
            ],
            [],
            []
} 
%%
%% Practitioner.Qualification
%% A person who is directly or indirectly involved in the provisioning of healthcare.
%%
    , <<"Practitioner.Qualification">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"issuer">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% PractitionerRole
%% A specific set of Roles/Locations/specialties/services that a practitioner may perform at an organization for a period of time.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"PractitionerRole">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"active">>, {{primitive, <<"boolean">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"practitioner">>, {{complex, <<"Reference">>}, optional}},
            {<<"organization">>, {{complex, <<"Reference">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"specialty">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"location">>, {{complex, <<"Reference">>}, list}},
            {<<"healthcareService">>, {{complex, <<"Reference">>}, list}},
            {<<"telecom">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"availableTime">>, {{bbelement, <<"PractitionerRole.AvailableTime">>}, list}},
            {<<"notAvailable">>, {{bbelement, <<"PractitionerRole.NotAvailable">>}, list}},
            {<<"availabilityExceptions">>, {{primitive, <<"string">>}, optional}},
            {<<"endpoint">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% PractitionerRole.AvailableTime
%% A specific set of Roles/Locations/specialties/services that a practitioner may perform at an organization for a period of time.
%%
    , <<"PractitionerRole.AvailableTime">> => {<<"BackboneElement">>,
            [
            {<<"daysOfWeek">>, {{code, <<"daysofweek_list">>}, list}},
            {<<"allDay">>, {{primitive, <<"boolean">>}, optional}},
            {<<"availableStartTime">>, {{primitive, <<"time">>}, optional}},
            {<<"availableEndTime">>, {{primitive, <<"time">>}, optional}}
            ],
            [],
            []
} 
%%
%% PractitionerRole.NotAvailable
%% A specific set of Roles/Locations/specialties/services that a practitioner may perform at an organization for a period of time.
%%
    , <<"PractitionerRole.NotAvailable">> => {<<"BackboneElement">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, required}},
            {<<"during">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            []
} 
%%
%% Procedure
%% An action that is or was performed on or for a patient. This can be a physical intervention like an operation, or less invasive like long term services, counseling, or hypnotherapy.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Procedure">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"instantiatesCanonical">>, {{primitive, <<"canonical">>}, list}},
            {<<"instantiatesUri">>, {{primitive, <<"uri">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"partOf">>, {{complex, <<"Reference">>}, list}},
            {<<"status">>, {{code, <<"eventstatus_list">>}, required}},
            {<<"statusReason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"performedDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"performedPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"performedString">>, {{primitive, <<"string">>}, optional}},
            {<<"performedAge">>, {{complex, <<"Age">>}, optional}},
            {<<"performedRange">>, {{complex, <<"Range">>}, optional}},
            {<<"recorder">>, {{complex, <<"Reference">>}, optional}},
            {<<"asserter">>, {{complex, <<"Reference">>}, optional}},
            {<<"performer">>, {{bbelement, <<"Procedure.Performer">>}, list}},
            {<<"location">>, {{complex, <<"Reference">>}, optional}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"bodySite">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"outcome">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"report">>, {{complex, <<"Reference">>}, list}},
            {<<"complication">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"complicationDetail">>, {{complex, <<"Reference">>}, list}},
            {<<"followUp">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"focalDevice">>, {{bbelement, <<"Procedure.FocalDevice">>}, list}},
            {<<"usedReference">>, {{complex, <<"Reference">>}, list}},
            {<<"usedCode">>, {{complex, <<"CodeableConcept">>}, list}}
            ],
            [],
            [
            {<<"performedDateTime">>, <<"performedPeriod">>, <<"performedString">>, <<"performedAge">>, <<"performedRange">>}
            ]
} 
%%
%% Procedure.Performer
%% An action that is or was performed on or for a patient. This can be a physical intervention like an operation, or less invasive like long term services, counseling, or hypnotherapy.
%%
    , <<"Procedure.Performer">> => {<<"BackboneElement">>,
            [
            {<<"function">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"actor">>, {{complex, <<"Reference">>}, required}},
            {<<"onBehalfOf">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% Procedure.FocalDevice
%% An action that is or was performed on or for a patient. This can be a physical intervention like an operation, or less invasive like long term services, counseling, or hypnotherapy.
%%
    , <<"Procedure.FocalDevice">> => {<<"BackboneElement">>,
            [
            {<<"action">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"manipulated">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            []
} 
%%
%% Provenance
%% Provenance of a resource is a record that describes entities and processes involved in producing and delivering or otherwise influencing that resource. Provenance provides a critical foundation for assessing authenticity, enabling trust, and allowing reproducibility. Provenance assertions are a form of contextual metadata and can themselves become important records with their own provenance. Provenance statement indicates clinical significance in terms of confidence in authenticity, reliability, and trustworthiness, integrity, and stage in lifecycle (e.g. Document Completion - has the artifact been legally authenticated), all of which may impact security, privacy, and trust policies.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Provenance">> => {<<"DomainResource">>,
            [
            {<<"target">>, {{complex, <<"Reference">>}, non_empty_list}},
            {<<"occurredPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"occurredDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"recorded">>, {{primitive, <<"instant">>}, required}},
            {<<"policy">>, {{primitive, <<"uri">>}, list}},
            {<<"location">>, {{complex, <<"Reference">>}, optional}},
            {<<"reason">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"activity">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"agent">>, {{bbelement, <<"Provenance.Agent">>}, non_empty_list}},
            {<<"entity">>, {{bbelement, <<"Provenance.Entity">>}, list}},
            {<<"signature">>, {{complex, <<"Signature">>}, list}}
            ],
            [],
            [
            {<<"occurredPeriod">>, <<"occurredDateTime">>}
            ]
} 
%%
%% Provenance.Agent
%% Provenance of a resource is a record that describes entities and processes involved in producing and delivering or otherwise influencing that resource. Provenance provides a critical foundation for assessing authenticity, enabling trust, and allowing reproducibility. Provenance assertions are a form of contextual metadata and can themselves become important records with their own provenance. Provenance statement indicates clinical significance in terms of confidence in authenticity, reliability, and trustworthiness, integrity, and stage in lifecycle (e.g. Document Completion - has the artifact been legally authenticated), all of which may impact security, privacy, and trust policies.
%%
    , <<"Provenance.Agent">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"role">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"who">>, {{complex, <<"Reference">>}, required}},
            {<<"onBehalfOf">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% Provenance.Entity
%% Provenance of a resource is a record that describes entities and processes involved in producing and delivering or otherwise influencing that resource. Provenance provides a critical foundation for assessing authenticity, enabling trust, and allowing reproducibility. Provenance assertions are a form of contextual metadata and can themselves become important records with their own provenance. Provenance statement indicates clinical significance in terms of confidence in authenticity, reliability, and trustworthiness, integrity, and stage in lifecycle (e.g. Document Completion - has the artifact been legally authenticated), all of which may impact security, privacy, and trust policies.
%%
    , <<"Provenance.Entity">> => {<<"BackboneElement">>,
            [
            {<<"role">>, {{code, <<"provenanceentityrole_list">>}, required}},
            {<<"what">>, {{complex, <<"Reference">>}, required}},
            {<<"agent">>, {{bbelement, <<"Provenance.Agent">>}, list}}
            ],
            [],
            []
} 
%%
%% Questionnaire
%% A structured set of questions intended to guide the collection of answers from end-users. Questionnaires provide detailed control over order, presentation, phraseology and grouping to allow coherent, consistent data collection.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Questionnaire">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"derivedFrom">>, {{primitive, <<"canonical">>}, list}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"subjectType">>, {{primitive, <<"code">>}, list}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"approvalDate">>, {{primitive, <<"date">>}, optional}},
            {<<"lastReviewDate">>, {{primitive, <<"date">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"code">>, {{complex, <<"Coding">>}, list}},
            {<<"item">>, {{bbelement, <<"Questionnaire.Item">>}, list}}
            ],
            [],
            []
} 
%%
%% Questionnaire.Item
%% A structured set of questions intended to guide the collection of answers from end-users. Questionnaires provide detailed control over order, presentation, phraseology and grouping to allow coherent, consistent data collection.
%%
    , <<"Questionnaire.Item">> => {<<"BackboneElement">>,
            [
            {<<"linkId">>, {{primitive, <<"string">>}, required}},
            {<<"definition">>, {{primitive, <<"uri">>}, optional}},
            {<<"code">>, {{complex, <<"Coding">>}, list}},
            {<<"prefix">>, {{primitive, <<"string">>}, optional}},
            {<<"text">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{code, <<"questionnaireitemtype_list">>}, required}},
            {<<"enableWhen">>, {{bbelement, <<"Questionnaire.EnableWhen">>}, list}},
            {<<"enableBehavior">>, {{code, <<"enablewhenbehavior_list">>}, optional}},
            {<<"required">>, {{primitive, <<"boolean">>}, optional}},
            {<<"repeats">>, {{primitive, <<"boolean">>}, optional}},
            {<<"readOnly">>, {{primitive, <<"boolean">>}, optional}},
            {<<"maxLength">>, {{primitive, <<"integer">>}, optional}},
            {<<"answerValueSet">>, {{primitive, <<"canonical">>}, optional}},
            {<<"answerOption">>, {{bbelement, <<"Questionnaire.AnswerOption">>}, list}},
            {<<"initial">>, {{bbelement, <<"Questionnaire.Initial">>}, list}},
            {<<"item">>, {{bbelement, <<"Questionnaire.Item">>}, list}}
            ],
            [],
            []
} 
%%
%% Questionnaire.EnableWhen
%% A structured set of questions intended to guide the collection of answers from end-users. Questionnaires provide detailed control over order, presentation, phraseology and grouping to allow coherent, consistent data collection.
%%
    , <<"Questionnaire.EnableWhen">> => {<<"BackboneElement">>,
            [
            {<<"question">>, {{primitive, <<"string">>}, required}},
            {<<"operator">>, {{code, <<"questionnaireitemoperator_list">>}, required}},
            {<<"answerBoolean">>, {{primitive, <<"boolean">>}, required}},
            {<<"answerDecimal">>, {{primitive, <<"decimal">>}, required}},
            {<<"answerInteger">>, {{primitive, <<"integer">>}, required}},
            {<<"answerDate">>, {{primitive, <<"date">>}, required}},
            {<<"answerDateTime">>, {{primitive, <<"dateTime">>}, required}},
            {<<"answerTime">>, {{primitive, <<"time">>}, required}},
            {<<"answerString">>, {{primitive, <<"string">>}, required}},
            {<<"answerCoding">>, {{complex, <<"Coding">>}, required}},
            {<<"answerQuantity">>, {{complex, <<"Quantity">>}, required}},
            {<<"answerReference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            [
            {<<"answerBoolean">>, <<"answerDecimal">>, <<"answerInteger">>, <<"answerDate">>, <<"answerDateTime">>, <<"answerTime">>, <<"answerString">>, <<"answerCoding">>, <<"answerQuantity">>, <<"answerReference">>}
            ]
} 
%%
%% Questionnaire.AnswerOption
%% A structured set of questions intended to guide the collection of answers from end-users. Questionnaires provide detailed control over order, presentation, phraseology and grouping to allow coherent, consistent data collection.
%%
    , <<"Questionnaire.AnswerOption">> => {<<"BackboneElement">>,
            [
            {<<"valueInteger">>, {{primitive, <<"integer">>}, required}},
            {<<"valueDate">>, {{primitive, <<"date">>}, required}},
            {<<"valueTime">>, {{primitive, <<"time">>}, required}},
            {<<"valueString">>, {{primitive, <<"string">>}, required}},
            {<<"valueCoding">>, {{complex, <<"Coding">>}, required}},
            {<<"valueReference">>, {{complex, <<"Reference">>}, required}},
            {<<"initialSelected">>, {{primitive, <<"boolean">>}, optional}}
            ],
            [],
            [
            {<<"valueInteger">>, <<"valueDate">>, <<"valueTime">>, <<"valueString">>, <<"valueCoding">>, <<"valueReference">>}
            ]
} 
%%
%% Questionnaire.Initial
%% A structured set of questions intended to guide the collection of answers from end-users. Questionnaires provide detailed control over order, presentation, phraseology and grouping to allow coherent, consistent data collection.
%%
    , <<"Questionnaire.Initial">> => {<<"BackboneElement">>,
            [
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, required}},
            {<<"valueDecimal">>, {{primitive, <<"decimal">>}, required}},
            {<<"valueInteger">>, {{primitive, <<"integer">>}, required}},
            {<<"valueDate">>, {{primitive, <<"date">>}, required}},
            {<<"valueDateTime">>, {{primitive, <<"dateTime">>}, required}},
            {<<"valueTime">>, {{primitive, <<"time">>}, required}},
            {<<"valueString">>, {{primitive, <<"string">>}, required}},
            {<<"valueUri">>, {{primitive, <<"uri">>}, required}},
            {<<"valueAttachment">>, {{complex, <<"Attachment">>}, required}},
            {<<"valueCoding">>, {{complex, <<"Coding">>}, required}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, required}},
            {<<"valueReference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            [
            {<<"valueBoolean">>, <<"valueDecimal">>, <<"valueInteger">>, <<"valueDate">>, <<"valueDateTime">>, <<"valueTime">>, <<"valueString">>, <<"valueUri">>, <<"valueAttachment">>, <<"valueCoding">>, <<"valueQuantity">>, <<"valueReference">>}
            ]
} 
%%
%% QuestionnaireResponse
%% A structured set of questions and their answers. The questions are ordered and grouped into coherent subsets, corresponding to the structure of the grouping of the questionnaire being responded to.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"QuestionnaireResponse">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"partOf">>, {{complex, <<"Reference">>}, list}},
            {<<"questionnaire">>, {{primitive, <<"canonical">>}, optional}},
            {<<"status">>, {{code, <<"questionnaireresponsestatus_list">>}, required}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"authored">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"author">>, {{complex, <<"Reference">>}, optional}},
            {<<"source">>, {{complex, <<"Reference">>}, optional}},
            {<<"item">>, {{bbelement, <<"QuestionnaireResponse.Item">>}, list}}
            ],
            [],
            []
} 
%%
%% QuestionnaireResponse.Item
%% A structured set of questions and their answers. The questions are ordered and grouped into coherent subsets, corresponding to the structure of the grouping of the questionnaire being responded to.
%%
    , <<"QuestionnaireResponse.Item">> => {<<"BackboneElement">>,
            [
            {<<"linkId">>, {{primitive, <<"string">>}, required}},
            {<<"definition">>, {{primitive, <<"uri">>}, optional}},
            {<<"text">>, {{primitive, <<"string">>}, optional}},
            {<<"answer">>, {{bbelement, <<"QuestionnaireResponse.Answer">>}, list}},
            {<<"item">>, {{bbelement, <<"QuestionnaireResponse.Item">>}, list}}
            ],
            [],
            []
} 
%%
%% QuestionnaireResponse.Answer
%% A structured set of questions and their answers. The questions are ordered and grouped into coherent subsets, corresponding to the structure of the grouping of the questionnaire being responded to.
%%
    , <<"QuestionnaireResponse.Answer">> => {<<"BackboneElement">>,
            [
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"valueDecimal">>, {{primitive, <<"decimal">>}, optional}},
            {<<"valueInteger">>, {{primitive, <<"integer">>}, optional}},
            {<<"valueDate">>, {{primitive, <<"date">>}, optional}},
            {<<"valueDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"valueTime">>, {{primitive, <<"time">>}, optional}},
            {<<"valueString">>, {{primitive, <<"string">>}, optional}},
            {<<"valueUri">>, {{primitive, <<"uri">>}, optional}},
            {<<"valueAttachment">>, {{complex, <<"Attachment">>}, optional}},
            {<<"valueCoding">>, {{complex, <<"Coding">>}, optional}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"valueReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"item">>, {{bbelement, <<"QuestionnaireResponse.Item">>}, list}}
            ],
            [],
            [
            {<<"valueBoolean">>, <<"valueDecimal">>, <<"valueInteger">>, <<"valueDate">>, <<"valueDateTime">>, <<"valueTime">>, <<"valueString">>, <<"valueUri">>, <<"valueAttachment">>, <<"valueCoding">>, <<"valueQuantity">>, <<"valueReference">>}
            ]
} 
%%
%% RelatedPerson
%% Information about a person that is involved in the care for a patient, but who is not the target of healthcare, nor has a formal responsibility in the care process.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"RelatedPerson">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"active">>, {{primitive, <<"boolean">>}, optional}},
            {<<"patient">>, {{complex, <<"Reference">>}, required}},
            {<<"relationship">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"name">>, {{complex, <<"HumanName">>}, list}},
            {<<"telecom">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"gender">>, {{code, <<"administrativegender_list">>}, optional}},
            {<<"birthDate">>, {{primitive, <<"date">>}, optional}},
            {<<"address">>, {{complex, <<"Address">>}, list}},
            {<<"photo">>, {{complex, <<"Attachment">>}, list}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"communication">>, {{bbelement, <<"RelatedPerson.Communication">>}, list}}
            ],
            [],
            []
} 
%%
%% RelatedPerson.Communication
%% Information about a person that is involved in the care for a patient, but who is not the target of healthcare, nor has a formal responsibility in the care process.
%%
    , <<"RelatedPerson.Communication">> => {<<"BackboneElement">>,
            [
            {<<"language">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"preferred">>, {{primitive, <<"boolean">>}, optional}}
            ],
            [],
            []
} 
%%
%% RequestGroup
%% A group of related requests that can be used to capture intended activities that have inter-dependencies such as "give this medication after that one".
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"RequestGroup">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"instantiatesCanonical">>, {{primitive, <<"canonical">>}, list}},
            {<<"instantiatesUri">>, {{primitive, <<"uri">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"replaces">>, {{complex, <<"Reference">>}, list}},
            {<<"groupIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"status">>, {{code, <<"requeststatus_list">>}, required}},
            {<<"intent">>, {{code, <<"requestintent_list">>}, required}},
            {<<"priority">>, {{code, <<"requestpriority_list">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"authoredOn">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"author">>, {{complex, <<"Reference">>}, optional}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"action">>, {{bbelement, <<"RequestGroup.Action">>}, list}}
            ],
            [],
            []
} 
%%
%% RequestGroup.Action
%% A group of related requests that can be used to capture intended activities that have inter-dependencies such as "give this medication after that one".
%%
    , <<"RequestGroup.Action">> => {<<"BackboneElement">>,
            [
            {<<"prefix">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"textEquivalent">>, {{primitive, <<"string">>}, optional}},
            {<<"priority">>, {{code, <<"requestpriority_list">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"documentation">>, {{complex, <<"RelatedArtifact">>}, list}},
            {<<"condition">>, {{bbelement, <<"RequestGroup.Condition">>}, list}},
            {<<"relatedAction">>, {{bbelement, <<"RequestGroup.RelatedAction">>}, list}},
            {<<"timingDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"timingAge">>, {{complex, <<"Age">>}, optional}},
            {<<"timingPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"timingDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"timingRange">>, {{complex, <<"Range">>}, optional}},
            {<<"timingTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"participant">>, {{complex, <<"Reference">>}, list}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"groupingBehavior">>, {{code, <<"actiongroupingbehavior_list">>}, optional}},
            {<<"selectionBehavior">>, {{code, <<"actionselectionbehavior_list">>}, optional}},
            {<<"requiredBehavior">>, {{code, <<"actionrequiredbehavior_list">>}, optional}},
            {<<"precheckBehavior">>, {{code, <<"actionprecheckbehavior_list">>}, optional}},
            {<<"cardinalityBehavior">>, {{code, <<"actioncardinalitybehavior_list">>}, optional}},
            {<<"resource">>, {{complex, <<"Reference">>}, optional}},
            {<<"action">>, {{bbelement, <<"RequestGroup.Action">>}, list}}
            ],
            [],
            [
            {<<"timingDateTime">>, <<"timingAge">>, <<"timingPeriod">>, <<"timingDuration">>, <<"timingRange">>, <<"timingTiming">>}
            ]
} 
%%
%% RequestGroup.Condition
%% A group of related requests that can be used to capture intended activities that have inter-dependencies such as "give this medication after that one".
%%
    , <<"RequestGroup.Condition">> => {<<"BackboneElement">>,
            [
            {<<"kind">>, {{code, <<"actionconditionkind_list">>}, required}},
            {<<"expression">>, {{complex, <<"Expression">>}, optional}}
            ],
            [],
            []
} 
%%
%% RequestGroup.RelatedAction
%% A group of related requests that can be used to capture intended activities that have inter-dependencies such as "give this medication after that one".
%%
    , <<"RequestGroup.RelatedAction">> => {<<"BackboneElement">>,
            [
            {<<"actionId">>, {{primitive, <<"id">>}, required}},
            {<<"relationship">>, {{code, <<"actionrelationshiptype_list">>}, required}},
            {<<"offsetDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"offsetRange">>, {{complex, <<"Range">>}, optional}}
            ],
            [],
            [
            {<<"offsetDuration">>, <<"offsetRange">>}
            ]
} 
%%
%% ResearchDefinition
%% The ResearchDefinition resource describes the conditional state (population and any exposures being compared within the population) and outcome (if specified) that the knowledge (evidence, assertion, recommendation) is about.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ResearchDefinition">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"shortTitle">>, {{primitive, <<"string">>}, optional}},
            {<<"subtitle">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"subjectCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subjectReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"comment">>, {{primitive, <<"string">>}, list}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"usage">>, {{primitive, <<"string">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"approvalDate">>, {{primitive, <<"date">>}, optional}},
            {<<"lastReviewDate">>, {{primitive, <<"date">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"topic">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"author">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"editor">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"reviewer">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"endorser">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"relatedArtifact">>, {{complex, <<"RelatedArtifact">>}, list}},
            {<<"library">>, {{primitive, <<"canonical">>}, list}},
            {<<"population">>, {{complex, <<"Reference">>}, required}},
            {<<"exposure">>, {{complex, <<"Reference">>}, optional}},
            {<<"exposureAlternative">>, {{complex, <<"Reference">>}, optional}},
            {<<"outcome">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            [
            {<<"subjectCodeableConcept">>, <<"subjectReference">>}
            ]
} 
%%
%% ResearchElementDefinition
%% The ResearchElementDefinition resource describes a "PICO" element that knowledge (evidence, assertion, recommendation) is about.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ResearchElementDefinition">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"shortTitle">>, {{primitive, <<"string">>}, optional}},
            {<<"subtitle">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"subjectCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subjectReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"comment">>, {{primitive, <<"string">>}, list}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"usage">>, {{primitive, <<"string">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"approvalDate">>, {{primitive, <<"date">>}, optional}},
            {<<"lastReviewDate">>, {{primitive, <<"date">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"topic">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"author">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"editor">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"reviewer">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"endorser">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"relatedArtifact">>, {{complex, <<"RelatedArtifact">>}, list}},
            {<<"library">>, {{primitive, <<"canonical">>}, list}},
            {<<"type">>, {{code, <<"researchelementtype_list">>}, required}},
            {<<"variableType">>, {{code, <<"variabletype_list">>}, optional}},
            {<<"characteristic">>, {{bbelement, <<"ResearchElementDefinition.Characteristic">>}, non_empty_list}}
            ],
            [],
            [
            {<<"subjectCodeableConcept">>, <<"subjectReference">>}
            ]
} 
%%
%% ResearchElementDefinition.Characteristic
%% The ResearchElementDefinition resource describes a "PICO" element that knowledge (evidence, assertion, recommendation) is about.
%%
    , <<"ResearchElementDefinition.Characteristic">> => {<<"BackboneElement">>,
            [
            {<<"definitionCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"definitionCanonical">>, {{primitive, <<"canonical">>}, required}},
            {<<"definitionExpression">>, {{complex, <<"Expression">>}, required}},
            {<<"definitionDataRequirement">>, {{complex, <<"DataRequirement">>}, required}},
            {<<"usageContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"exclude">>, {{primitive, <<"boolean">>}, optional}},
            {<<"unitOfMeasure">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"studyEffectiveDescription">>, {{primitive, <<"string">>}, optional}},
            {<<"studyEffectiveDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"studyEffectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"studyEffectiveDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"studyEffectiveTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"studyEffectiveTimeFromStart">>, {{complex, <<"Duration">>}, optional}},
            {<<"studyEffectiveGroupMeasure">>, {{code, <<"groupmeasure_list">>}, optional}},
            {<<"participantEffectiveDescription">>, {{primitive, <<"string">>}, optional}},
            {<<"participantEffectiveDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"participantEffectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"participantEffectiveDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"participantEffectiveTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"participantEffectiveTimeFromStart">>, {{complex, <<"Duration">>}, optional}},
            {<<"participantEffectiveGroupMeasure">>, {{code, <<"groupmeasure_list">>}, optional}}
            ],
            [],
            [
            {<<"definitionCodeableConcept">>, <<"definitionCanonical">>, <<"definitionExpression">>, <<"definitionDataRequirement">>}, 
            {<<"studyEffectiveDateTime">>, <<"studyEffectivePeriod">>, <<"studyEffectiveDuration">>, <<"studyEffectiveTiming">>}, 
            {<<"participantEffectiveDateTime">>, <<"participantEffectivePeriod">>, <<"participantEffectiveDuration">>, <<"participantEffectiveTiming">>}
            ]
} 
%%
%% ResearchStudy
%% A process where a researcher or organization plans and then executes a series of steps intended to increase the field of healthcare-related knowledge.  This includes studies of safety, efficacy, comparative effectiveness and other information about medications, devices, therapies and other interventional and investigative techniques.  A ResearchStudy involves the gathering of information about human or animal subjects.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ResearchStudy">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"protocol">>, {{complex, <<"Reference">>}, list}},
            {<<"partOf">>, {{complex, <<"Reference">>}, list}},
            {<<"status">>, {{code, <<"researchstudystatus_list">>}, required}},
            {<<"primaryPurposeType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"phase">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"focus">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"condition">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"relatedArtifact">>, {{complex, <<"RelatedArtifact">>}, list}},
            {<<"keyword">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"location">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"enrollment">>, {{complex, <<"Reference">>}, list}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"sponsor">>, {{complex, <<"Reference">>}, optional}},
            {<<"principalInvestigator">>, {{complex, <<"Reference">>}, optional}},
            {<<"site">>, {{complex, <<"Reference">>}, list}},
            {<<"reasonStopped">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"arm">>, {{bbelement, <<"ResearchStudy.Arm">>}, list}},
            {<<"objective">>, {{bbelement, <<"ResearchStudy.Objective">>}, list}}
            ],
            [],
            []
} 
%%
%% ResearchStudy.Arm
%% A process where a researcher or organization plans and then executes a series of steps intended to increase the field of healthcare-related knowledge.  This includes studies of safety, efficacy, comparative effectiveness and other information about medications, devices, therapies and other interventional and investigative techniques.  A ResearchStudy involves the gathering of information about human or animal subjects.
%%
    , <<"ResearchStudy.Arm">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% ResearchStudy.Objective
%% A process where a researcher or organization plans and then executes a series of steps intended to increase the field of healthcare-related knowledge.  This includes studies of safety, efficacy, comparative effectiveness and other information about medications, devices, therapies and other interventional and investigative techniques.  A ResearchStudy involves the gathering of information about human or animal subjects.
%%
    , <<"ResearchStudy.Objective">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% ResearchSubject
%% A physical entity which is the primary unit of operational and/or administrative interest in a study.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ResearchSubject">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"researchsubjectstatus_list">>}, required}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"study">>, {{complex, <<"Reference">>}, required}},
            {<<"individual">>, {{complex, <<"Reference">>}, required}},
            {<<"assignedArm">>, {{primitive, <<"string">>}, optional}},
            {<<"actualArm">>, {{primitive, <<"string">>}, optional}},
            {<<"consent">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% RiskAssessment
%% An assessment of the likely outcome(s) for a patient or other subject as well as the likelihood of each outcome.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"RiskAssessment">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, optional}},
            {<<"parent">>, {{complex, <<"Reference">>}, optional}},
            {<<"status">>, {{code, <<"observationstatus_list">>}, required}},
            {<<"method">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"occurrenceDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"occurrencePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"condition">>, {{complex, <<"Reference">>}, optional}},
            {<<"performer">>, {{complex, <<"Reference">>}, optional}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"basis">>, {{complex, <<"Reference">>}, list}},
            {<<"prediction">>, {{bbelement, <<"RiskAssessment.Prediction">>}, list}},
            {<<"mitigation">>, {{primitive, <<"string">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            [
            {<<"occurrenceDateTime">>, <<"occurrencePeriod">>}
            ]
} 
%%
%% RiskAssessment.Prediction
%% An assessment of the likely outcome(s) for a patient or other subject as well as the likelihood of each outcome.
%%
    , <<"RiskAssessment.Prediction">> => {<<"BackboneElement">>,
            [
            {<<"outcome">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"probabilityDecimal">>, {{primitive, <<"decimal">>}, optional}},
            {<<"probabilityRange">>, {{complex, <<"Range">>}, optional}},
            {<<"qualitativeRisk">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"relativeRisk">>, {{primitive, <<"decimal">>}, optional}},
            {<<"whenPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"whenRange">>, {{complex, <<"Range">>}, optional}},
            {<<"rationale">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            [
            {<<"probabilityDecimal">>, <<"probabilityRange">>}, 
            {<<"whenPeriod">>, <<"whenRange">>}
            ]
} 
%%
%% RiskEvidenceSynthesis
%% The RiskEvidenceSynthesis resource describes the likelihood of an outcome in a population plus exposure state where the risk estimate is derived from a combination of research studies.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"RiskEvidenceSynthesis">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"approvalDate">>, {{primitive, <<"date">>}, optional}},
            {<<"lastReviewDate">>, {{primitive, <<"date">>}, optional}},
            {<<"effectivePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"topic">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"author">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"editor">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"reviewer">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"endorser">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"relatedArtifact">>, {{complex, <<"RelatedArtifact">>}, list}},
            {<<"synthesisType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"studyType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"population">>, {{complex, <<"Reference">>}, required}},
            {<<"exposure">>, {{complex, <<"Reference">>}, optional}},
            {<<"outcome">>, {{complex, <<"Reference">>}, required}},
            {<<"sampleSize">>, {{bbelement, <<"RiskEvidenceSynthesis.SampleSize">>}, optional}},
            {<<"riskEstimate">>, {{bbelement, <<"RiskEvidenceSynthesis.RiskEstimate">>}, optional}},
            {<<"certainty">>, {{bbelement, <<"RiskEvidenceSynthesis.Certainty">>}, list}}
            ],
            [],
            []
} 
%%
%% RiskEvidenceSynthesis.SampleSize
%% The RiskEvidenceSynthesis resource describes the likelihood of an outcome in a population plus exposure state where the risk estimate is derived from a combination of research studies.
%%
    , <<"RiskEvidenceSynthesis.SampleSize">> => {<<"BackboneElement">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"numberOfStudies">>, {{primitive, <<"integer">>}, optional}},
            {<<"numberOfParticipants">>, {{primitive, <<"integer">>}, optional}}
            ],
            [],
            []
} 
%%
%% RiskEvidenceSynthesis.RiskEstimate
%% The RiskEvidenceSynthesis resource describes the likelihood of an outcome in a population plus exposure state where the risk estimate is derived from a combination of research studies.
%%
    , <<"RiskEvidenceSynthesis.RiskEstimate">> => {<<"BackboneElement">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"value">>, {{primitive, <<"decimal">>}, optional}},
            {<<"unitOfMeasure">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"denominatorCount">>, {{primitive, <<"integer">>}, optional}},
            {<<"numeratorCount">>, {{primitive, <<"integer">>}, optional}},
            {<<"precisionEstimate">>, {{bbelement, <<"RiskEvidenceSynthesis.PrecisionEstimate">>}, list}}
            ],
            [],
            []
} 
%%
%% RiskEvidenceSynthesis.PrecisionEstimate
%% The RiskEvidenceSynthesis resource describes the likelihood of an outcome in a population plus exposure state where the risk estimate is derived from a combination of research studies.
%%
    , <<"RiskEvidenceSynthesis.PrecisionEstimate">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"level">>, {{primitive, <<"decimal">>}, optional}},
            {<<"from">>, {{primitive, <<"decimal">>}, optional}},
            {<<"to">>, {{primitive, <<"decimal">>}, optional}}
            ],
            [],
            []
} 
%%
%% RiskEvidenceSynthesis.Certainty
%% The RiskEvidenceSynthesis resource describes the likelihood of an outcome in a population plus exposure state where the risk estimate is derived from a combination of research studies.
%%
    , <<"RiskEvidenceSynthesis.Certainty">> => {<<"BackboneElement">>,
            [
            {<<"rating">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"certaintySubcomponent">>, {{bbelement, <<"RiskEvidenceSynthesis.CertaintySubcomponent">>}, list}}
            ],
            [],
            []
} 
%%
%% RiskEvidenceSynthesis.CertaintySubcomponent
%% The RiskEvidenceSynthesis resource describes the likelihood of an outcome in a population plus exposure state where the risk estimate is derived from a combination of research studies.
%%
    , <<"RiskEvidenceSynthesis.CertaintySubcomponent">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"rating">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            []
} 
%%
%% Schedule
%% A container for slots of time that may be available for booking appointments.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Schedule">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"active">>, {{primitive, <<"boolean">>}, optional}},
            {<<"serviceCategory">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"serviceType">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"specialty">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"actor">>, {{complex, <<"Reference">>}, non_empty_list}},
            {<<"planningHorizon">>, {{complex, <<"Period">>}, optional}},
            {<<"comment">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% SearchParameter
%% A search parameter that defines a named search item that can be used to search/filter on a resource.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"SearchParameter">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, required}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"derivedFrom">>, {{primitive, <<"canonical">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, required}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"code">>, {{primitive, <<"code">>}, required}},
            {<<"base">>, {{primitive, <<"code">>}, non_empty_list}},
            {<<"type">>, {{code, <<"searchparamtype_list">>}, required}},
            {<<"expression">>, {{primitive, <<"string">>}, optional}},
            {<<"xpath">>, {{primitive, <<"string">>}, optional}},
            {<<"xpathUsage">>, {{code, <<"xpathusagetype_list">>}, optional}},
            {<<"target">>, {{primitive, <<"code">>}, list}},
            {<<"multipleOr">>, {{primitive, <<"boolean">>}, optional}},
            {<<"multipleAnd">>, {{primitive, <<"boolean">>}, optional}},
            {<<"comparator">>, {{code, <<"searchcomparator_list">>}, list}},
            {<<"modifier">>, {{code, <<"searchmodifiercode_list">>}, list}},
            {<<"chain">>, {{primitive, <<"string">>}, list}},
            {<<"component">>, {{bbelement, <<"SearchParameter.Component">>}, list}}
            ],
            [],
            []
} 
%%
%% SearchParameter.Component
%% A search parameter that defines a named search item that can be used to search/filter on a resource.
%%
    , <<"SearchParameter.Component">> => {<<"BackboneElement">>,
            [
            {<<"definition">>, {{primitive, <<"canonical">>}, required}},
            {<<"expression">>, {{primitive, <<"string">>}, required}}
            ],
            [],
            []
} 
%%
%% ServiceRequest
%% A record of a request for service such as diagnostic investigations, treatments, or operations to be performed.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ServiceRequest">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"instantiatesCanonical">>, {{primitive, <<"canonical">>}, list}},
            {<<"instantiatesUri">>, {{primitive, <<"uri">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"replaces">>, {{complex, <<"Reference">>}, list}},
            {<<"requisition">>, {{complex, <<"Identifier">>}, optional}},
            {<<"status">>, {{code, <<"requeststatus_list">>}, required}},
            {<<"intent">>, {{code, <<"requestintent_list">>}, required}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"priority">>, {{code, <<"requestpriority_list">>}, optional}},
            {<<"doNotPerform">>, {{primitive, <<"boolean">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"orderDetail">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"quantityQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"quantityRatio">>, {{complex, <<"Ratio">>}, optional}},
            {<<"quantityRange">>, {{complex, <<"Range">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, required}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"occurrenceDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"occurrencePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"occurrenceTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"asNeededBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"asNeededCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"authoredOn">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"requester">>, {{complex, <<"Reference">>}, optional}},
            {<<"performerType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"performer">>, {{complex, <<"Reference">>}, list}},
            {<<"locationCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"locationReference">>, {{complex, <<"Reference">>}, list}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"insurance">>, {{complex, <<"Reference">>}, list}},
            {<<"supportingInfo">>, {{complex, <<"Reference">>}, list}},
            {<<"specimen">>, {{complex, <<"Reference">>}, list}},
            {<<"bodySite">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"patientInstruction">>, {{primitive, <<"string">>}, optional}},
            {<<"relevantHistory">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            [
            {<<"quantityQuantity">>, <<"quantityRatio">>, <<"quantityRange">>}, 
            {<<"occurrenceDateTime">>, <<"occurrencePeriod">>, <<"occurrenceTiming">>}, 
            {<<"asNeededBoolean">>, <<"asNeededCodeableConcept">>}
            ]
} 
%%
%% Slot
%% A slot of time on a schedule that may be available for booking appointments.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Slot">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"serviceCategory">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"serviceType">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"specialty">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"appointmentType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"schedule">>, {{complex, <<"Reference">>}, required}},
            {<<"status">>, {{code, <<"slotstatus_list">>}, required}},
            {<<"start">>, {{primitive, <<"instant">>}, required}},
            {<<"end">>, {{primitive, <<"instant">>}, required}},
            {<<"overbooked">>, {{primitive, <<"boolean">>}, optional}},
            {<<"comment">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% Specimen
%% A sample to be used for analysis.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Specimen">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"accessionIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"status">>, {{code, <<"specimenstatus_list">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subject">>, {{complex, <<"Reference">>}, optional}},
            {<<"receivedTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"parent">>, {{complex, <<"Reference">>}, list}},
            {<<"request">>, {{complex, <<"Reference">>}, list}},
            {<<"collection">>, {{bbelement, <<"Specimen.Collection">>}, optional}},
            {<<"processing">>, {{bbelement, <<"Specimen.Processing">>}, list}},
            {<<"container">>, {{bbelement, <<"Specimen.Container">>}, list}},
            {<<"condition">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            []
} 
%%
%% Specimen.Collection
%% A sample to be used for analysis.
%%
    , <<"Specimen.Collection">> => {<<"BackboneElement">>,
            [
            {<<"collector">>, {{complex, <<"Reference">>}, optional}},
            {<<"collectedDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"collectedPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"duration">>, {{complex, <<"Duration">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"method">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"bodySite">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"fastingStatusCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"fastingStatusDuration">>, {{complex, <<"Duration">>}, optional}}
            ],
            [],
            [
            {<<"collectedDateTime">>, <<"collectedPeriod">>}, 
            {<<"fastingStatusCodeableConcept">>, <<"fastingStatusDuration">>}
            ]
} 
%%
%% Specimen.Processing
%% A sample to be used for analysis.
%%
    , <<"Specimen.Processing">> => {<<"BackboneElement">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"procedure">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"additive">>, {{complex, <<"Reference">>}, list}},
            {<<"timeDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"timePeriod">>, {{complex, <<"Period">>}, optional}}
            ],
            [],
            [
            {<<"timeDateTime">>, <<"timePeriod">>}
            ]
} 
%%
%% Specimen.Container
%% A sample to be used for analysis.
%%
    , <<"Specimen.Container">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"capacity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"specimenQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"additiveCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"additiveReference">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            [
            {<<"additiveCodeableConcept">>, <<"additiveReference">>}
            ]
} 
%%
%% SpecimenDefinition
%% A kind of specimen with associated set of requirements.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"SpecimenDefinition">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"typeCollected">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"patientPreparation">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"timeAspect">>, {{primitive, <<"string">>}, optional}},
            {<<"collection">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"typeTested">>, {{bbelement, <<"SpecimenDefinition.TypeTested">>}, list}}
            ],
            [],
            []
} 
%%
%% SpecimenDefinition.TypeTested
%% A kind of specimen with associated set of requirements.
%%
    , <<"SpecimenDefinition.TypeTested">> => {<<"BackboneElement">>,
            [
            {<<"isDerived">>, {{primitive, <<"boolean">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"preference">>, {{code, <<"specimencontainedpreference_list">>}, required}},
            {<<"container">>, {{bbelement, <<"SpecimenDefinition.Container">>}, optional}},
            {<<"requirement">>, {{primitive, <<"string">>}, optional}},
            {<<"retentionTime">>, {{complex, <<"Duration">>}, optional}},
            {<<"rejectionCriterion">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"handling">>, {{bbelement, <<"SpecimenDefinition.Handling">>}, list}}
            ],
            [],
            []
} 
%%
%% SpecimenDefinition.Container
%% A kind of specimen with associated set of requirements.
%%
    , <<"SpecimenDefinition.Container">> => {<<"BackboneElement">>,
            [
            {<<"material">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"cap">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"capacity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"minimumVolumeQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"minimumVolumeString">>, {{primitive, <<"string">>}, optional}},
            {<<"additive">>, {{bbelement, <<"SpecimenDefinition.Additive">>}, list}},
            {<<"preparation">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            [
            {<<"minimumVolumeQuantity">>, <<"minimumVolumeString">>}
            ]
} 
%%
%% SpecimenDefinition.Additive
%% A kind of specimen with associated set of requirements.
%%
    , <<"SpecimenDefinition.Additive">> => {<<"BackboneElement">>,
            [
            {<<"additiveCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"additiveReference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            [
            {<<"additiveCodeableConcept">>, <<"additiveReference">>}
            ]
} 
%%
%% SpecimenDefinition.Handling
%% A kind of specimen with associated set of requirements.
%%
    , <<"SpecimenDefinition.Handling">> => {<<"BackboneElement">>,
            [
            {<<"temperatureQualifier">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"temperatureRange">>, {{complex, <<"Range">>}, optional}},
            {<<"maxDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"instruction">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% StructureDefinition
%% A definition of a FHIR structure. This resource is used to describe the underlying resources, data types defined in FHIR, and also for describing extensions and constraints on resources and data types.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"StructureDefinition">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, required}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"keyword">>, {{complex, <<"Coding">>}, list}},
            {<<"fhirVersion">>, {{code, <<"fhirversion_list">>}, optional}},
            {<<"mapping">>, {{bbelement, <<"StructureDefinition.Mapping">>}, list}},
            {<<"kind">>, {{code, <<"structuredefinitionkind_list">>}, required}},
            {<<"abstract">>, {{primitive, <<"boolean">>}, required}},
            {<<"context">>, {{bbelement, <<"StructureDefinition.Context">>}, list}},
            {<<"contextInvariant">>, {{primitive, <<"string">>}, list}},
            {<<"type">>, {{primitive, <<"uri">>}, required}},
            {<<"baseDefinition">>, {{primitive, <<"canonical">>}, optional}},
            {<<"derivation">>, {{code, <<"typederivationrule_list">>}, optional}},
            {<<"snapshot">>, {{bbelement, <<"StructureDefinition.Snapshot">>}, optional}},
            {<<"differential">>, {{bbelement, <<"StructureDefinition.Differential">>}, optional}}
            ],
            [],
            []
} 
%%
%% StructureDefinition.Mapping
%% A definition of a FHIR structure. This resource is used to describe the underlying resources, data types defined in FHIR, and also for describing extensions and constraints on resources and data types.
%%
    , <<"StructureDefinition.Mapping">> => {<<"BackboneElement">>,
            [
            {<<"identity">>, {{primitive, <<"id">>}, required}},
            {<<"uri">>, {{primitive, <<"uri">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"comment">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% StructureDefinition.Context
%% A definition of a FHIR structure. This resource is used to describe the underlying resources, data types defined in FHIR, and also for describing extensions and constraints on resources and data types.
%%
    , <<"StructureDefinition.Context">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{code, <<"extensioncontexttype_list">>}, required}},
            {<<"expression">>, {{primitive, <<"string">>}, required}}
            ],
            [],
            []
} 
%%
%% StructureDefinition.Snapshot
%% A definition of a FHIR structure. This resource is used to describe the underlying resources, data types defined in FHIR, and also for describing extensions and constraints on resources and data types.
%%
    , <<"StructureDefinition.Snapshot">> => {<<"BackboneElement">>,
            [
            {<<"element">>, {{bbelement, <<"ElementDefinition">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% StructureDefinition.Differential
%% A definition of a FHIR structure. This resource is used to describe the underlying resources, data types defined in FHIR, and also for describing extensions and constraints on resources and data types.
%%
    , <<"StructureDefinition.Differential">> => {<<"BackboneElement">>,
            [
            {<<"element">>, {{bbelement, <<"ElementDefinition">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% StructureMap
%% A Map of relationships between 2 structures that can be used to transform data.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"StructureMap">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, required}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"structure">>, {{bbelement, <<"StructureMap.Structure">>}, list}},
            {<<"import">>, {{primitive, <<"canonical">>}, list}},
            {<<"group">>, {{bbelement, <<"StructureMap.Group">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% StructureMap.Structure
%% A Map of relationships between 2 structures that can be used to transform data.
%%
    , <<"StructureMap.Structure">> => {<<"BackboneElement">>,
            [
            {<<"url">>, {{primitive, <<"canonical">>}, required}},
            {<<"mode">>, {{code, <<"structuremapmodelmode_list">>}, required}},
            {<<"alias">>, {{primitive, <<"string">>}, optional}},
            {<<"documentation">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% StructureMap.Group
%% A Map of relationships between 2 structures that can be used to transform data.
%%
    , <<"StructureMap.Group">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"id">>}, required}},
            {<<"extends">>, {{primitive, <<"id">>}, optional}},
            {<<"typeMode">>, {{code, <<"structuremapgrouptypemode_list">>}, required}},
            {<<"documentation">>, {{primitive, <<"string">>}, optional}},
            {<<"input">>, {{bbelement, <<"StructureMap.Input">>}, non_empty_list}},
            {<<"rule">>, {{bbelement, <<"StructureMap.Rule">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% StructureMap.Input
%% A Map of relationships between 2 structures that can be used to transform data.
%%
    , <<"StructureMap.Input">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"id">>}, required}},
            {<<"type">>, {{primitive, <<"string">>}, optional}},
            {<<"mode">>, {{code, <<"structuremapinputmode_list">>}, required}},
            {<<"documentation">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% StructureMap.Rule
%% A Map of relationships between 2 structures that can be used to transform data.
%%
    , <<"StructureMap.Rule">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"id">>}, required}},
            {<<"source">>, {{bbelement, <<"StructureMap.Source">>}, non_empty_list}},
            {<<"target">>, {{bbelement, <<"StructureMap.Target">>}, list}},
            {<<"rule">>, {{bbelement, <<"StructureMap.Rule">>}, list}},
            {<<"dependent">>, {{bbelement, <<"StructureMap.Dependent">>}, list}},
            {<<"documentation">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% StructureMap.Source
%% A Map of relationships between 2 structures that can be used to transform data.
%%
    , <<"StructureMap.Source">> => {<<"BackboneElement">>,
            [
            {<<"context">>, {{primitive, <<"id">>}, required}},
            {<<"min">>, {{primitive, <<"integer">>}, optional}},
            {<<"max">>, {{primitive, <<"string">>}, optional}},
            {<<"type">>, {{primitive, <<"string">>}, optional}},
            {<<"defaultValueBase64Binary">>, {{primitive, <<"base64Binary">>}, optional}},
            {<<"defaultValueBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"defaultValueCanonical">>, {{primitive, <<"canonical">>}, optional}},
            {<<"defaultValueCode">>, {{primitive, <<"code">>}, optional}},
            {<<"defaultValueDate">>, {{primitive, <<"date">>}, optional}},
            {<<"defaultValueDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"defaultValueDecimal">>, {{primitive, <<"decimal">>}, optional}},
            {<<"defaultValueId">>, {{primitive, <<"id">>}, optional}},
            {<<"defaultValueInstant">>, {{primitive, <<"instant">>}, optional}},
            {<<"defaultValueInteger">>, {{primitive, <<"integer">>}, optional}},
            {<<"defaultValueMarkdown">>, {{primitive, <<"markdown">>}, optional}},
            {<<"defaultValueOid">>, {{primitive, <<"oid">>}, optional}},
            {<<"defaultValuePositiveInt">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"defaultValueString">>, {{primitive, <<"string">>}, optional}},
            {<<"defaultValueTime">>, {{primitive, <<"time">>}, optional}},
            {<<"defaultValueUnsignedInt">>, {{primitive, <<"unsignedInt">>}, optional}},
            {<<"defaultValueUri">>, {{primitive, <<"uri">>}, optional}},
            {<<"defaultValueUrl">>, {{primitive, <<"url">>}, optional}},
            {<<"defaultValueUuid">>, {{primitive, <<"uuid">>}, optional}},
            {<<"defaultValueAddress">>, {{complex, <<"Address">>}, optional}},
            {<<"defaultValueAge">>, {{complex, <<"Age">>}, optional}},
            {<<"defaultValueAnnotation">>, {{complex, <<"Annotation">>}, optional}},
            {<<"defaultValueAttachment">>, {{complex, <<"Attachment">>}, optional}},
            {<<"defaultValueCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"defaultValueCoding">>, {{complex, <<"Coding">>}, optional}},
            {<<"defaultValueContactPoint">>, {{complex, <<"ContactPoint">>}, optional}},
            {<<"defaultValueCount">>, {{complex, <<"Count">>}, optional}},
            {<<"defaultValueDistance">>, {{complex, <<"Distance">>}, optional}},
            {<<"defaultValueDuration">>, {{complex, <<"Duration">>}, optional}},
            {<<"defaultValueHumanName">>, {{complex, <<"HumanName">>}, optional}},
            {<<"defaultValueIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"defaultValueMoney">>, {{complex, <<"Money">>}, optional}},
            {<<"defaultValuePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"defaultValueQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"defaultValueRange">>, {{complex, <<"Range">>}, optional}},
            {<<"defaultValueRatio">>, {{complex, <<"Ratio">>}, optional}},
            {<<"defaultValueReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"defaultValueSampledData">>, {{complex, <<"SampledData">>}, optional}},
            {<<"defaultValueSignature">>, {{complex, <<"Signature">>}, optional}},
            {<<"defaultValueTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"defaultValueContactDetail">>, {{complex, <<"ContactDetail">>}, optional}},
            {<<"defaultValueContributor">>, {{complex, <<"Contributor">>}, optional}},
            {<<"defaultValueDataRequirement">>, {{complex, <<"DataRequirement">>}, optional}},
            {<<"defaultValueExpression">>, {{complex, <<"Expression">>}, optional}},
            {<<"defaultValueParameterDefinition">>, {{complex, <<"ParameterDefinition">>}, optional}},
            {<<"defaultValueRelatedArtifact">>, {{complex, <<"RelatedArtifact">>}, optional}},
            {<<"defaultValueTriggerDefinition">>, {{complex, <<"TriggerDefinition">>}, optional}},
            {<<"defaultValueUsageContext">>, {{complex, <<"UsageContext">>}, optional}},
            {<<"defaultValueDosage">>, {{bbelement, <<"Dosage">>}, optional}},
            {<<"element">>, {{primitive, <<"string">>}, optional}},
            {<<"listMode">>, {{code, <<"structuremapsourcelistmode_list">>}, optional}},
            {<<"variable">>, {{primitive, <<"id">>}, optional}},
            {<<"condition">>, {{primitive, <<"string">>}, optional}},
            {<<"check">>, {{primitive, <<"string">>}, optional}},
            {<<"logMessage">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            [
            {<<"defaultValueBase64Binary">>, <<"defaultValueBoolean">>, <<"defaultValueCanonical">>, <<"defaultValueCode">>, <<"defaultValueDate">>, <<"defaultValueDateTime">>, <<"defaultValueDecimal">>, <<"defaultValueId">>, <<"defaultValueInstant">>, <<"defaultValueInteger">>, <<"defaultValueMarkdown">>, <<"defaultValueOid">>, <<"defaultValuePositiveInt">>, <<"defaultValueString">>, <<"defaultValueTime">>, <<"defaultValueUnsignedInt">>, <<"defaultValueUri">>, <<"defaultValueUrl">>, <<"defaultValueUuid">>, <<"defaultValueAddress">>, <<"defaultValueAge">>, <<"defaultValueAnnotation">>, <<"defaultValueAttachment">>, <<"defaultValueCodeableConcept">>, <<"defaultValueCoding">>, <<"defaultValueContactPoint">>, <<"defaultValueCount">>, <<"defaultValueDistance">>, <<"defaultValueDuration">>, <<"defaultValueHumanName">>, <<"defaultValueIdentifier">>, <<"defaultValueMoney">>, <<"defaultValuePeriod">>, <<"defaultValueQuantity">>, <<"defaultValueRange">>, <<"defaultValueRatio">>, <<"defaultValueReference">>, <<"defaultValueSampledData">>, <<"defaultValueSignature">>, <<"defaultValueTiming">>, <<"defaultValueContactDetail">>, <<"defaultValueContributor">>, <<"defaultValueDataRequirement">>, <<"defaultValueExpression">>, <<"defaultValueParameterDefinition">>, <<"defaultValueRelatedArtifact">>, <<"defaultValueTriggerDefinition">>, <<"defaultValueUsageContext">>, <<"defaultValueDosage">>}
            ]
} 
%%
%% StructureMap.Target
%% A Map of relationships between 2 structures that can be used to transform data.
%%
    , <<"StructureMap.Target">> => {<<"BackboneElement">>,
            [
            {<<"context">>, {{primitive, <<"id">>}, optional}},
            {<<"contextType">>, {{code, <<"structuremapcontexttype_list">>}, optional}},
            {<<"element">>, {{primitive, <<"string">>}, optional}},
            {<<"variable">>, {{primitive, <<"id">>}, optional}},
            {<<"listMode">>, {{code, <<"structuremaptargetlistmode_list">>}, list}},
            {<<"listRuleId">>, {{primitive, <<"id">>}, optional}},
            {<<"transform">>, {{code, <<"structuremaptransform_list">>}, optional}},
            {<<"parameter">>, {{bbelement, <<"StructureMap.Parameter">>}, list}}
            ],
            [],
            []
} 
%%
%% StructureMap.Parameter
%% A Map of relationships between 2 structures that can be used to transform data.
%%
    , <<"StructureMap.Parameter">> => {<<"BackboneElement">>,
            [
            {<<"valueId">>, {{primitive, <<"id">>}, required}},
            {<<"valueString">>, {{primitive, <<"string">>}, required}},
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, required}},
            {<<"valueInteger">>, {{primitive, <<"integer">>}, required}},
            {<<"valueDecimal">>, {{primitive, <<"decimal">>}, required}}
            ],
            [],
            [
            {<<"valueId">>, <<"valueString">>, <<"valueBoolean">>, <<"valueInteger">>, <<"valueDecimal">>}
            ]
} 
%%
%% StructureMap.Dependent
%% A Map of relationships between 2 structures that can be used to transform data.
%%
    , <<"StructureMap.Dependent">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"id">>}, required}},
            {<<"variable">>, {{primitive, <<"string">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% Subscription
%% The subscription resource is used to define a push-based subscription from a server to another system. Once a subscription is registered with the server, the server checks every resource that is created or updated, and if the resource matches the given criteria, it sends a message on the defined "channel" so that another system can take an appropriate action.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Subscription">> => {<<"DomainResource">>,
            [
            {<<"status">>, {{code, <<"subscriptionstatus_list">>}, required}},
            {<<"contact">>, {{complex, <<"ContactPoint">>}, list}},
            {<<"end">>, {{primitive, <<"instant">>}, optional}},
            {<<"reason">>, {{primitive, <<"string">>}, required}},
            {<<"criteria">>, {{primitive, <<"string">>}, required}},
            {<<"error">>, {{primitive, <<"string">>}, optional}},
            {<<"channel">>, {{bbelement, <<"Subscription.Channel">>}, required}}
            ],
            [],
            []
} 
%%
%% Subscription.Channel
%% The subscription resource is used to define a push-based subscription from a server to another system. Once a subscription is registered with the server, the server checks every resource that is created or updated, and if the resource matches the given criteria, it sends a message on the defined "channel" so that another system can take an appropriate action.
%%
    , <<"Subscription.Channel">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{code, <<"subscriptionchanneltype_list">>}, required}},
            {<<"endpoint">>, {{primitive, <<"url">>}, optional}},
            {<<"payload">>, {{primitive, <<"code">>}, optional}},
            {<<"header">>, {{primitive, <<"string">>}, list}}
            ],
            [],
            []
} 
%%
%% Substance
%% A homogeneous material with a definite composition.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Substance">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"fhirsubstancestatus_list">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"instance">>, {{bbelement, <<"Substance.Instance">>}, list}},
            {<<"ingredient">>, {{bbelement, <<"Substance.Ingredient">>}, list}}
            ],
            [],
            []
} 
%%
%% Substance.Instance
%% A homogeneous material with a definite composition.
%%
    , <<"Substance.Instance">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"expiry">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}}
            ],
            [],
            []
} 
%%
%% Substance.Ingredient
%% A homogeneous material with a definite composition.
%%
    , <<"Substance.Ingredient">> => {<<"BackboneElement">>,
            [
            {<<"quantity">>, {{complex, <<"Ratio">>}, optional}},
            {<<"substanceCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"substanceReference">>, {{complex, <<"Reference">>}, required}}
            ],
            [],
            [
            {<<"substanceCodeableConcept">>, <<"substanceReference">>}
            ]
} 
%%
%% SubstanceNucleicAcid
%% Nucleic acids are defined by three distinct elements: the base, sugar and linkage. Individual substance/moiety IDs will be created for each of these elements. The nucleotide sequence will be always entered in the 5’-3’ direction.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"SubstanceNucleicAcid">> => {<<"DomainResource">>,
            [
            {<<"sequenceType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"numberOfSubunits">>, {{primitive, <<"integer">>}, optional}},
            {<<"areaOfHybridisation">>, {{primitive, <<"string">>}, optional}},
            {<<"oligoNucleotideType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subunit">>, {{bbelement, <<"SubstanceNucleicAcid.Subunit">>}, list}}
            ],
            [],
            []
} 
%%
%% SubstanceNucleicAcid.Subunit
%% Nucleic acids are defined by three distinct elements: the base, sugar and linkage. Individual substance/moiety IDs will be created for each of these elements. The nucleotide sequence will be always entered in the 5’-3’ direction.
%%
    , <<"SubstanceNucleicAcid.Subunit">> => {<<"BackboneElement">>,
            [
            {<<"subunit">>, {{primitive, <<"integer">>}, optional}},
            {<<"sequence">>, {{primitive, <<"string">>}, optional}},
            {<<"length">>, {{primitive, <<"integer">>}, optional}},
            {<<"sequenceAttachment">>, {{complex, <<"Attachment">>}, optional}},
            {<<"fivePrime">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"threePrime">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"linkage">>, {{bbelement, <<"SubstanceNucleicAcid.Linkage">>}, list}},
            {<<"sugar">>, {{bbelement, <<"SubstanceNucleicAcid.Sugar">>}, list}}
            ],
            [],
            []
} 
%%
%% SubstanceNucleicAcid.Linkage
%% Nucleic acids are defined by three distinct elements: the base, sugar and linkage. Individual substance/moiety IDs will be created for each of these elements. The nucleotide sequence will be always entered in the 5’-3’ direction.
%%
    , <<"SubstanceNucleicAcid.Linkage">> => {<<"BackboneElement">>,
            [
            {<<"connectivity">>, {{primitive, <<"string">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"residueSite">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstanceNucleicAcid.Sugar
%% Nucleic acids are defined by three distinct elements: the base, sugar and linkage. Individual substance/moiety IDs will be created for each of these elements. The nucleotide sequence will be always entered in the 5’-3’ direction.
%%
    , <<"SubstanceNucleicAcid.Sugar">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"residueSite">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstancePolymer
%% Todo.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"SubstancePolymer">> => {<<"DomainResource">>,
            [
            {<<"class">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"geometry">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"copolymerConnectivity">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"modification">>, {{primitive, <<"string">>}, list}},
            {<<"monomerSet">>, {{bbelement, <<"SubstancePolymer.MonomerSet">>}, list}},
            {<<"repeat">>, {{bbelement, <<"SubstancePolymer.Repeat">>}, list}}
            ],
            [],
            []
} 
%%
%% SubstancePolymer.MonomerSet
%% Todo.
%%
    , <<"SubstancePolymer.MonomerSet">> => {<<"BackboneElement">>,
            [
            {<<"ratioType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"startingMaterial">>, {{bbelement, <<"SubstancePolymer.StartingMaterial">>}, list}}
            ],
            [],
            []
} 
%%
%% SubstancePolymer.StartingMaterial
%% Todo.
%%
    , <<"SubstancePolymer.StartingMaterial">> => {<<"BackboneElement">>,
            [
            {<<"material">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"isDefining">>, {{primitive, <<"boolean">>}, optional}},
            {<<"amount">>, {{bbelement, <<"SubstanceAmount">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstancePolymer.Repeat
%% Todo.
%%
    , <<"SubstancePolymer.Repeat">> => {<<"BackboneElement">>,
            [
            {<<"numberOfUnits">>, {{primitive, <<"integer">>}, optional}},
            {<<"averageMolecularFormula">>, {{primitive, <<"string">>}, optional}},
            {<<"repeatUnitAmountType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"repeatUnit">>, {{bbelement, <<"SubstancePolymer.RepeatUnit">>}, list}}
            ],
            [],
            []
} 
%%
%% SubstancePolymer.RepeatUnit
%% Todo.
%%
    , <<"SubstancePolymer.RepeatUnit">> => {<<"BackboneElement">>,
            [
            {<<"orientationOfPolymerisation">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"repeatUnit">>, {{primitive, <<"string">>}, optional}},
            {<<"amount">>, {{bbelement, <<"SubstanceAmount">>}, optional}},
            {<<"degreeOfPolymerisation">>, {{bbelement, <<"SubstancePolymer.DegreeOfPolymerisation">>}, list}},
            {<<"structuralRepresentation">>, {{bbelement, <<"SubstancePolymer.StructuralRepresentation">>}, list}}
            ],
            [],
            []
} 
%%
%% SubstancePolymer.DegreeOfPolymerisation
%% Todo.
%%
    , <<"SubstancePolymer.DegreeOfPolymerisation">> => {<<"BackboneElement">>,
            [
            {<<"degree">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"amount">>, {{bbelement, <<"SubstanceAmount">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstancePolymer.StructuralRepresentation
%% Todo.
%%
    , <<"SubstancePolymer.StructuralRepresentation">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"representation">>, {{primitive, <<"string">>}, optional}},
            {<<"attachment">>, {{complex, <<"Attachment">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstanceProtein
%% A SubstanceProtein is defined as a single unit of a linear amino acid sequence, or a combination of subunits that are either covalently linked or have a defined invariant stoichiometric relationship. This includes all synthetic, recombinant and purified SubstanceProteins of defined sequence, whether the use is therapeutic or prophylactic. This set of elements will be used to describe albumins, coagulation factors, cytokines, growth factors, peptide/SubstanceProtein hormones, enzymes, toxins, toxoids, recombinant vaccines, and immunomodulators.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"SubstanceProtein">> => {<<"DomainResource">>,
            [
            {<<"sequenceType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"numberOfSubunits">>, {{primitive, <<"integer">>}, optional}},
            {<<"disulfideLinkage">>, {{primitive, <<"string">>}, list}},
            {<<"subunit">>, {{bbelement, <<"SubstanceProtein.Subunit">>}, list}}
            ],
            [],
            []
} 
%%
%% SubstanceProtein.Subunit
%% A SubstanceProtein is defined as a single unit of a linear amino acid sequence, or a combination of subunits that are either covalently linked or have a defined invariant stoichiometric relationship. This includes all synthetic, recombinant and purified SubstanceProteins of defined sequence, whether the use is therapeutic or prophylactic. This set of elements will be used to describe albumins, coagulation factors, cytokines, growth factors, peptide/SubstanceProtein hormones, enzymes, toxins, toxoids, recombinant vaccines, and immunomodulators.
%%
    , <<"SubstanceProtein.Subunit">> => {<<"BackboneElement">>,
            [
            {<<"subunit">>, {{primitive, <<"integer">>}, optional}},
            {<<"sequence">>, {{primitive, <<"string">>}, optional}},
            {<<"length">>, {{primitive, <<"integer">>}, optional}},
            {<<"sequenceAttachment">>, {{complex, <<"Attachment">>}, optional}},
            {<<"nTerminalModificationId">>, {{complex, <<"Identifier">>}, optional}},
            {<<"nTerminalModification">>, {{primitive, <<"string">>}, optional}},
            {<<"cTerminalModificationId">>, {{complex, <<"Identifier">>}, optional}},
            {<<"cTerminalModification">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstanceReferenceInformation
%% Todo.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"SubstanceReferenceInformation">> => {<<"DomainResource">>,
            [
            {<<"comment">>, {{primitive, <<"string">>}, optional}},
            {<<"gene">>, {{bbelement, <<"SubstanceReferenceInformation.Gene">>}, list}},
            {<<"geneElement">>, {{bbelement, <<"SubstanceReferenceInformation.GeneElement">>}, list}},
            {<<"classification">>, {{bbelement, <<"SubstanceReferenceInformation.Classification">>}, list}},
            {<<"target">>, {{bbelement, <<"SubstanceReferenceInformation.Target">>}, list}}
            ],
            [],
            []
} 
%%
%% SubstanceReferenceInformation.Gene
%% Todo.
%%
    , <<"SubstanceReferenceInformation.Gene">> => {<<"BackboneElement">>,
            [
            {<<"geneSequenceOrigin">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"gene">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"source">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% SubstanceReferenceInformation.GeneElement
%% Todo.
%%
    , <<"SubstanceReferenceInformation.GeneElement">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"element">>, {{complex, <<"Identifier">>}, optional}},
            {<<"source">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% SubstanceReferenceInformation.Classification
%% Todo.
%%
    , <<"SubstanceReferenceInformation.Classification">> => {<<"BackboneElement">>,
            [
            {<<"domain">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"classification">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"subtype">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"source">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% SubstanceReferenceInformation.Target
%% Todo.
%%
    , <<"SubstanceReferenceInformation.Target">> => {<<"BackboneElement">>,
            [
            {<<"target">>, {{complex, <<"Identifier">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"interaction">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"organism">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"organismType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"amountQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"amountRange">>, {{complex, <<"Range">>}, optional}},
            {<<"amountString">>, {{primitive, <<"string">>}, optional}},
            {<<"amountType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"source">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            [
            {<<"amountQuantity">>, <<"amountRange">>, <<"amountString">>}
            ]
} 
%%
%% SubstanceSourceMaterial
%% Source material shall capture information on the taxonomic and anatomical origins as well as the fraction of a material that can result in or can be modified to form a substance. This set of data elements shall be used to define polymer substances isolated from biological matrices. Taxonomic and anatomical origins shall be described using a controlled vocabulary as required. This information is captured for naturally derived polymers ( . starch) and structurally diverse substances. For Organisms belonging to the Kingdom Plantae the Substance level defines the fresh material of a single species or infraspecies, the Herbal Drug and the Herbal preparation. For Herbal preparations, the fraction information will be captured at the Substance information level and additional information for herbal extracts will be captured at the Specified Substance Group 1 information level. See for further explanation the Substance Class: Structurally Diverse and the herbal annex.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"SubstanceSourceMaterial">> => {<<"DomainResource">>,
            [
            {<<"sourceMaterialClass">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"sourceMaterialType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"sourceMaterialState">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"organismId">>, {{complex, <<"Identifier">>}, optional}},
            {<<"organismName">>, {{primitive, <<"string">>}, optional}},
            {<<"parentSubstanceId">>, {{complex, <<"Identifier">>}, list}},
            {<<"parentSubstanceName">>, {{primitive, <<"string">>}, list}},
            {<<"countryOfOrigin">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"geographicalLocation">>, {{primitive, <<"string">>}, list}},
            {<<"developmentStage">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"fractionDescription">>, {{bbelement, <<"SubstanceSourceMaterial.FractionDescription">>}, list}},
            {<<"organism">>, {{bbelement, <<"SubstanceSourceMaterial.Organism">>}, optional}},
            {<<"partDescription">>, {{bbelement, <<"SubstanceSourceMaterial.PartDescription">>}, list}}
            ],
            [],
            []
} 
%%
%% SubstanceSourceMaterial.FractionDescription
%% Source material shall capture information on the taxonomic and anatomical origins as well as the fraction of a material that can result in or can be modified to form a substance. This set of data elements shall be used to define polymer substances isolated from biological matrices. Taxonomic and anatomical origins shall be described using a controlled vocabulary as required. This information is captured for naturally derived polymers ( . starch) and structurally diverse substances. For Organisms belonging to the Kingdom Plantae the Substance level defines the fresh material of a single species or infraspecies, the Herbal Drug and the Herbal preparation. For Herbal preparations, the fraction information will be captured at the Substance information level and additional information for herbal extracts will be captured at the Specified Substance Group 1 information level. See for further explanation the Substance Class: Structurally Diverse and the herbal annex.
%%
    , <<"SubstanceSourceMaterial.FractionDescription">> => {<<"BackboneElement">>,
            [
            {<<"fraction">>, {{primitive, <<"string">>}, optional}},
            {<<"materialType">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstanceSourceMaterial.Organism
%% Source material shall capture information on the taxonomic and anatomical origins as well as the fraction of a material that can result in or can be modified to form a substance. This set of data elements shall be used to define polymer substances isolated from biological matrices. Taxonomic and anatomical origins shall be described using a controlled vocabulary as required. This information is captured for naturally derived polymers ( . starch) and structurally diverse substances. For Organisms belonging to the Kingdom Plantae the Substance level defines the fresh material of a single species or infraspecies, the Herbal Drug and the Herbal preparation. For Herbal preparations, the fraction information will be captured at the Substance information level and additional information for herbal extracts will be captured at the Specified Substance Group 1 information level. See for further explanation the Substance Class: Structurally Diverse and the herbal annex.
%%
    , <<"SubstanceSourceMaterial.Organism">> => {<<"BackboneElement">>,
            [
            {<<"family">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"genus">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"species">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"intraspecificType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"intraspecificDescription">>, {{primitive, <<"string">>}, optional}},
            {<<"author">>, {{bbelement, <<"SubstanceSourceMaterial.Author">>}, list}},
            {<<"hybrid">>, {{bbelement, <<"SubstanceSourceMaterial.Hybrid">>}, optional}},
            {<<"organismGeneral">>, {{bbelement, <<"SubstanceSourceMaterial.OrganismGeneral">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstanceSourceMaterial.Author
%% Source material shall capture information on the taxonomic and anatomical origins as well as the fraction of a material that can result in or can be modified to form a substance. This set of data elements shall be used to define polymer substances isolated from biological matrices. Taxonomic and anatomical origins shall be described using a controlled vocabulary as required. This information is captured for naturally derived polymers ( . starch) and structurally diverse substances. For Organisms belonging to the Kingdom Plantae the Substance level defines the fresh material of a single species or infraspecies, the Herbal Drug and the Herbal preparation. For Herbal preparations, the fraction information will be captured at the Substance information level and additional information for herbal extracts will be captured at the Specified Substance Group 1 information level. See for further explanation the Substance Class: Structurally Diverse and the herbal annex.
%%
    , <<"SubstanceSourceMaterial.Author">> => {<<"BackboneElement">>,
            [
            {<<"authorType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"authorDescription">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstanceSourceMaterial.Hybrid
%% Source material shall capture information on the taxonomic and anatomical origins as well as the fraction of a material that can result in or can be modified to form a substance. This set of data elements shall be used to define polymer substances isolated from biological matrices. Taxonomic and anatomical origins shall be described using a controlled vocabulary as required. This information is captured for naturally derived polymers ( . starch) and structurally diverse substances. For Organisms belonging to the Kingdom Plantae the Substance level defines the fresh material of a single species or infraspecies, the Herbal Drug and the Herbal preparation. For Herbal preparations, the fraction information will be captured at the Substance information level and additional information for herbal extracts will be captured at the Specified Substance Group 1 information level. See for further explanation the Substance Class: Structurally Diverse and the herbal annex.
%%
    , <<"SubstanceSourceMaterial.Hybrid">> => {<<"BackboneElement">>,
            [
            {<<"maternalOrganismId">>, {{primitive, <<"string">>}, optional}},
            {<<"maternalOrganismName">>, {{primitive, <<"string">>}, optional}},
            {<<"paternalOrganismId">>, {{primitive, <<"string">>}, optional}},
            {<<"paternalOrganismName">>, {{primitive, <<"string">>}, optional}},
            {<<"hybridType">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstanceSourceMaterial.OrganismGeneral
%% Source material shall capture information on the taxonomic and anatomical origins as well as the fraction of a material that can result in or can be modified to form a substance. This set of data elements shall be used to define polymer substances isolated from biological matrices. Taxonomic and anatomical origins shall be described using a controlled vocabulary as required. This information is captured for naturally derived polymers ( . starch) and structurally diverse substances. For Organisms belonging to the Kingdom Plantae the Substance level defines the fresh material of a single species or infraspecies, the Herbal Drug and the Herbal preparation. For Herbal preparations, the fraction information will be captured at the Substance information level and additional information for herbal extracts will be captured at the Specified Substance Group 1 information level. See for further explanation the Substance Class: Structurally Diverse and the herbal annex.
%%
    , <<"SubstanceSourceMaterial.OrganismGeneral">> => {<<"BackboneElement">>,
            [
            {<<"kingdom">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"phylum">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"class">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"order">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstanceSourceMaterial.PartDescription
%% Source material shall capture information on the taxonomic and anatomical origins as well as the fraction of a material that can result in or can be modified to form a substance. This set of data elements shall be used to define polymer substances isolated from biological matrices. Taxonomic and anatomical origins shall be described using a controlled vocabulary as required. This information is captured for naturally derived polymers ( . starch) and structurally diverse substances. For Organisms belonging to the Kingdom Plantae the Substance level defines the fresh material of a single species or infraspecies, the Herbal Drug and the Herbal preparation. For Herbal preparations, the fraction information will be captured at the Substance information level and additional information for herbal extracts will be captured at the Specified Substance Group 1 information level. See for further explanation the Substance Class: Structurally Diverse and the herbal annex.
%%
    , <<"SubstanceSourceMaterial.PartDescription">> => {<<"BackboneElement">>,
            [
            {<<"part">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"partLocation">>, {{complex, <<"CodeableConcept">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstanceSpecification
%% The detailed description of a substance, typically at a level beyond what is used for prescribing.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"SubstanceSpecification">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"status">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"domain">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"source">>, {{complex, <<"Reference">>}, list}},
            {<<"comment">>, {{primitive, <<"string">>}, optional}},
            {<<"moiety">>, {{bbelement, <<"SubstanceSpecification.Moiety">>}, list}},
            {<<"property">>, {{bbelement, <<"SubstanceSpecification.Property">>}, list}},
            {<<"referenceInformation">>, {{complex, <<"Reference">>}, optional}},
            {<<"structure">>, {{bbelement, <<"SubstanceSpecification.Structure">>}, optional}},
            {<<"code">>, {{bbelement, <<"SubstanceSpecification.Code">>}, list}},
            {<<"name">>, {{bbelement, <<"SubstanceSpecification.Name">>}, list}},
            {<<"molecularWeight">>, {{bbelement, <<"SubstanceSpecification.MolecularWeight">>}, list}},
            {<<"relationship">>, {{bbelement, <<"SubstanceSpecification.Relationship">>}, list}},
            {<<"nucleicAcid">>, {{complex, <<"Reference">>}, optional}},
            {<<"polymer">>, {{complex, <<"Reference">>}, optional}},
            {<<"protein">>, {{complex, <<"Reference">>}, optional}},
            {<<"sourceMaterial">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstanceSpecification.Moiety
%% The detailed description of a substance, typically at a level beyond what is used for prescribing.
%%
    , <<"SubstanceSpecification.Moiety">> => {<<"BackboneElement">>,
            [
            {<<"role">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"stereochemistry">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"opticalActivity">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"molecularFormula">>, {{primitive, <<"string">>}, optional}},
            {<<"amountQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"amountString">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            [
            {<<"amountQuantity">>, <<"amountString">>}
            ]
} 
%%
%% SubstanceSpecification.Property
%% The detailed description of a substance, typically at a level beyond what is used for prescribing.
%%
    , <<"SubstanceSpecification.Property">> => {<<"BackboneElement">>,
            [
            {<<"category">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"parameters">>, {{primitive, <<"string">>}, optional}},
            {<<"definingSubstanceReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"definingSubstanceCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"amountQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"amountString">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            [
            {<<"definingSubstanceReference">>, <<"definingSubstanceCodeableConcept">>}, 
            {<<"amountQuantity">>, <<"amountString">>}
            ]
} 
%%
%% SubstanceSpecification.Structure
%% The detailed description of a substance, typically at a level beyond what is used for prescribing.
%%
    , <<"SubstanceSpecification.Structure">> => {<<"BackboneElement">>,
            [
            {<<"stereochemistry">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"opticalActivity">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"molecularFormula">>, {{primitive, <<"string">>}, optional}},
            {<<"molecularFormulaByMoiety">>, {{primitive, <<"string">>}, optional}},
            {<<"isotope">>, {{bbelement, <<"SubstanceSpecification.Isotope">>}, list}},
            {<<"molecularWeight">>, {{bbelement, <<"SubstanceSpecification.MolecularWeight">>}, optional}},
            {<<"source">>, {{complex, <<"Reference">>}, list}},
            {<<"representation">>, {{bbelement, <<"SubstanceSpecification.Representation">>}, list}}
            ],
            [],
            []
} 
%%
%% SubstanceSpecification.Isotope
%% The detailed description of a substance, typically at a level beyond what is used for prescribing.
%%
    , <<"SubstanceSpecification.Isotope">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"name">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"substitution">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"halfLife">>, {{complex, <<"Quantity">>}, optional}},
            {<<"molecularWeight">>, {{bbelement, <<"SubstanceSpecification.MolecularWeight">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstanceSpecification.MolecularWeight
%% The detailed description of a substance, typically at a level beyond what is used for prescribing.
%%
    , <<"SubstanceSpecification.MolecularWeight">> => {<<"BackboneElement">>,
            [
            {<<"method">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"amount">>, {{complex, <<"Quantity">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstanceSpecification.Representation
%% The detailed description of a substance, typically at a level beyond what is used for prescribing.
%%
    , <<"SubstanceSpecification.Representation">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"representation">>, {{primitive, <<"string">>}, optional}},
            {<<"attachment">>, {{complex, <<"Attachment">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstanceSpecification.Code
%% The detailed description of a substance, typically at a level beyond what is used for prescribing.
%%
    , <<"SubstanceSpecification.Code">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"status">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"statusDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"comment">>, {{primitive, <<"string">>}, optional}},
            {<<"source">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% SubstanceSpecification.Name
%% The detailed description of a substance, typically at a level beyond what is used for prescribing.
%%
    , <<"SubstanceSpecification.Name">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"status">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"preferred">>, {{primitive, <<"boolean">>}, optional}},
            {<<"language">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"domain">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"synonym">>, {{bbelement, <<"SubstanceSpecification.Name">>}, list}},
            {<<"translation">>, {{bbelement, <<"SubstanceSpecification.Name">>}, list}},
            {<<"official">>, {{bbelement, <<"SubstanceSpecification.Official">>}, list}},
            {<<"source">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% SubstanceSpecification.Official
%% The detailed description of a substance, typically at a level beyond what is used for prescribing.
%%
    , <<"SubstanceSpecification.Official">> => {<<"BackboneElement">>,
            [
            {<<"authority">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"status">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}}
            ],
            [],
            []
} 
%%
%% SubstanceSpecification.Relationship
%% The detailed description of a substance, typically at a level beyond what is used for prescribing.
%%
    , <<"SubstanceSpecification.Relationship">> => {<<"BackboneElement">>,
            [
            {<<"substanceReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"substanceCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"relationship">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"isDefining">>, {{primitive, <<"boolean">>}, optional}},
            {<<"amountQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"amountRange">>, {{complex, <<"Range">>}, optional}},
            {<<"amountRatio">>, {{complex, <<"Ratio">>}, optional}},
            {<<"amountString">>, {{primitive, <<"string">>}, optional}},
            {<<"amountRatioLowLimit">>, {{complex, <<"Ratio">>}, optional}},
            {<<"amountType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"source">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            [
            {<<"substanceReference">>, <<"substanceCodeableConcept">>}, 
            {<<"amountQuantity">>, <<"amountRange">>, <<"amountRatio">>, <<"amountString">>}
            ]
} 
%%
%% SupplyDelivery
%% Record of delivery of what is supplied.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"SupplyDelivery">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"partOf">>, {{complex, <<"Reference">>}, list}},
            {<<"status">>, {{code, <<"supplydeliverystatus_list">>}, optional}},
            {<<"patient">>, {{complex, <<"Reference">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"suppliedItem">>, {{bbelement, <<"SupplyDelivery.SuppliedItem">>}, optional}},
            {<<"occurrenceDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"occurrencePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"occurrenceTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"supplier">>, {{complex, <<"Reference">>}, optional}},
            {<<"destination">>, {{complex, <<"Reference">>}, optional}},
            {<<"receiver">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            [
            {<<"occurrenceDateTime">>, <<"occurrencePeriod">>, <<"occurrenceTiming">>}
            ]
} 
%%
%% SupplyDelivery.SuppliedItem
%% Record of delivery of what is supplied.
%%
    , <<"SupplyDelivery.SuppliedItem">> => {<<"BackboneElement">>,
            [
            {<<"quantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"itemCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"itemReference">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            [
            {<<"itemCodeableConcept">>, <<"itemReference">>}
            ]
} 
%%
%% SupplyRequest
%% A record of a request for a medication, substance or device used in the healthcare setting.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"SupplyRequest">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"supplyrequeststatus_list">>}, optional}},
            {<<"category">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"priority">>, {{code, <<"requestpriority_list">>}, optional}},
            {<<"itemCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"itemReference">>, {{complex, <<"Reference">>}, required}},
            {<<"quantity">>, {{complex, <<"Quantity">>}, required}},
            {<<"parameter">>, {{bbelement, <<"SupplyRequest.Parameter">>}, list}},
            {<<"occurrenceDateTime">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"occurrencePeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"occurrenceTiming">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"authoredOn">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"requester">>, {{complex, <<"Reference">>}, optional}},
            {<<"supplier">>, {{complex, <<"Reference">>}, list}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, list}},
            {<<"deliverFrom">>, {{complex, <<"Reference">>}, optional}},
            {<<"deliverTo">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            [
            {<<"itemCodeableConcept">>, <<"itemReference">>}, 
            {<<"occurrenceDateTime">>, <<"occurrencePeriod">>, <<"occurrenceTiming">>}
            ]
} 
%%
%% SupplyRequest.Parameter
%% A record of a request for a medication, substance or device used in the healthcare setting.
%%
    , <<"SupplyRequest.Parameter">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"valueCodeableConcept">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, optional}},
            {<<"valueRange">>, {{complex, <<"Range">>}, optional}},
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, optional}}
            ],
            [],
            [
            {<<"valueCodeableConcept">>, <<"valueQuantity">>, <<"valueRange">>, <<"valueBoolean">>}
            ]
} 
%%
%% Task
%% A task to be performed.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"Task">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"instantiatesCanonical">>, {{primitive, <<"canonical">>}, optional}},
            {<<"instantiatesUri">>, {{primitive, <<"uri">>}, optional}},
            {<<"basedOn">>, {{complex, <<"Reference">>}, list}},
            {<<"groupIdentifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"partOf">>, {{complex, <<"Reference">>}, list}},
            {<<"status">>, {{code, <<"taskstatus_list">>}, required}},
            {<<"statusReason">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"businessStatus">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"intent">>, {{code, <<"taskintent_list">>}, required}},
            {<<"priority">>, {{code, <<"requestpriority_list">>}, optional}},
            {<<"code">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"focus">>, {{complex, <<"Reference">>}, optional}},
            {<<"for">>, {{complex, <<"Reference">>}, optional}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"executionPeriod">>, {{complex, <<"Period">>}, optional}},
            {<<"authoredOn">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"lastModified">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"requester">>, {{complex, <<"Reference">>}, optional}},
            {<<"performerType">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"owner">>, {{complex, <<"Reference">>}, optional}},
            {<<"location">>, {{complex, <<"Reference">>}, optional}},
            {<<"reasonCode">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"reasonReference">>, {{complex, <<"Reference">>}, optional}},
            {<<"insurance">>, {{complex, <<"Reference">>}, list}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}},
            {<<"relevantHistory">>, {{complex, <<"Reference">>}, list}},
            {<<"restriction">>, {{bbelement, <<"Task.Restriction">>}, optional}},
            {<<"input">>, {{bbelement, <<"Task.Input">>}, list}},
            {<<"output">>, {{bbelement, <<"Task.Output">>}, list}}
            ],
            [],
            []
} 
%%
%% Task.Restriction
%% A task to be performed.
%%
    , <<"Task.Restriction">> => {<<"BackboneElement">>,
            [
            {<<"repetitions">>, {{primitive, <<"positiveInt">>}, optional}},
            {<<"period">>, {{complex, <<"Period">>}, optional}},
            {<<"recipient">>, {{complex, <<"Reference">>}, list}}
            ],
            [],
            []
} 
%%
%% Task.Input
%% A task to be performed.
%%
    , <<"Task.Input">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"valueBase64Binary">>, {{primitive, <<"base64Binary">>}, required}},
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, required}},
            {<<"valueCanonical">>, {{primitive, <<"canonical">>}, required}},
            {<<"valueCode">>, {{primitive, <<"code">>}, required}},
            {<<"valueDate">>, {{primitive, <<"date">>}, required}},
            {<<"valueDateTime">>, {{primitive, <<"dateTime">>}, required}},
            {<<"valueDecimal">>, {{primitive, <<"decimal">>}, required}},
            {<<"valueId">>, {{primitive, <<"id">>}, required}},
            {<<"valueInstant">>, {{primitive, <<"instant">>}, required}},
            {<<"valueInteger">>, {{primitive, <<"integer">>}, required}},
            {<<"valueMarkdown">>, {{primitive, <<"markdown">>}, required}},
            {<<"valueOid">>, {{primitive, <<"oid">>}, required}},
            {<<"valuePositiveInt">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"valueString">>, {{primitive, <<"string">>}, required}},
            {<<"valueTime">>, {{primitive, <<"time">>}, required}},
            {<<"valueUnsignedInt">>, {{primitive, <<"unsignedInt">>}, required}},
            {<<"valueUri">>, {{primitive, <<"uri">>}, required}},
            {<<"valueUrl">>, {{primitive, <<"url">>}, required}},
            {<<"valueUuid">>, {{primitive, <<"uuid">>}, required}},
            {<<"valueAddress">>, {{complex, <<"Address">>}, required}},
            {<<"valueAge">>, {{complex, <<"Age">>}, required}},
            {<<"valueAnnotation">>, {{complex, <<"Annotation">>}, required}},
            {<<"valueAttachment">>, {{complex, <<"Attachment">>}, required}},
            {<<"valueCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"valueCoding">>, {{complex, <<"Coding">>}, required}},
            {<<"valueContactPoint">>, {{complex, <<"ContactPoint">>}, required}},
            {<<"valueCount">>, {{complex, <<"Count">>}, required}},
            {<<"valueDistance">>, {{complex, <<"Distance">>}, required}},
            {<<"valueDuration">>, {{complex, <<"Duration">>}, required}},
            {<<"valueHumanName">>, {{complex, <<"HumanName">>}, required}},
            {<<"valueIdentifier">>, {{complex, <<"Identifier">>}, required}},
            {<<"valueMoney">>, {{complex, <<"Money">>}, required}},
            {<<"valuePeriod">>, {{complex, <<"Period">>}, required}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, required}},
            {<<"valueRange">>, {{complex, <<"Range">>}, required}},
            {<<"valueRatio">>, {{complex, <<"Ratio">>}, required}},
            {<<"valueReference">>, {{complex, <<"Reference">>}, required}},
            {<<"valueSampledData">>, {{complex, <<"SampledData">>}, required}},
            {<<"valueSignature">>, {{complex, <<"Signature">>}, required}},
            {<<"valueTiming">>, {{bbelement, <<"Timing">>}, required}},
            {<<"valueContactDetail">>, {{complex, <<"ContactDetail">>}, required}},
            {<<"valueContributor">>, {{complex, <<"Contributor">>}, required}},
            {<<"valueDataRequirement">>, {{complex, <<"DataRequirement">>}, required}},
            {<<"valueExpression">>, {{complex, <<"Expression">>}, required}},
            {<<"valueParameterDefinition">>, {{complex, <<"ParameterDefinition">>}, required}},
            {<<"valueRelatedArtifact">>, {{complex, <<"RelatedArtifact">>}, required}},
            {<<"valueTriggerDefinition">>, {{complex, <<"TriggerDefinition">>}, required}},
            {<<"valueUsageContext">>, {{complex, <<"UsageContext">>}, required}},
            {<<"valueDosage">>, {{bbelement, <<"Dosage">>}, required}}
            ],
            [],
            [
            {<<"valueBase64Binary">>, <<"valueBoolean">>, <<"valueCanonical">>, <<"valueCode">>, <<"valueDate">>, <<"valueDateTime">>, <<"valueDecimal">>, <<"valueId">>, <<"valueInstant">>, <<"valueInteger">>, <<"valueMarkdown">>, <<"valueOid">>, <<"valuePositiveInt">>, <<"valueString">>, <<"valueTime">>, <<"valueUnsignedInt">>, <<"valueUri">>, <<"valueUrl">>, <<"valueUuid">>, <<"valueAddress">>, <<"valueAge">>, <<"valueAnnotation">>, <<"valueAttachment">>, <<"valueCodeableConcept">>, <<"valueCoding">>, <<"valueContactPoint">>, <<"valueCount">>, <<"valueDistance">>, <<"valueDuration">>, <<"valueHumanName">>, <<"valueIdentifier">>, <<"valueMoney">>, <<"valuePeriod">>, <<"valueQuantity">>, <<"valueRange">>, <<"valueRatio">>, <<"valueReference">>, <<"valueSampledData">>, <<"valueSignature">>, <<"valueTiming">>, <<"valueContactDetail">>, <<"valueContributor">>, <<"valueDataRequirement">>, <<"valueExpression">>, <<"valueParameterDefinition">>, <<"valueRelatedArtifact">>, <<"valueTriggerDefinition">>, <<"valueUsageContext">>, <<"valueDosage">>}
            ]
} 
%%
%% Task.Output
%% A task to be performed.
%%
    , <<"Task.Output">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"valueBase64Binary">>, {{primitive, <<"base64Binary">>}, required}},
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, required}},
            {<<"valueCanonical">>, {{primitive, <<"canonical">>}, required}},
            {<<"valueCode">>, {{primitive, <<"code">>}, required}},
            {<<"valueDate">>, {{primitive, <<"date">>}, required}},
            {<<"valueDateTime">>, {{primitive, <<"dateTime">>}, required}},
            {<<"valueDecimal">>, {{primitive, <<"decimal">>}, required}},
            {<<"valueId">>, {{primitive, <<"id">>}, required}},
            {<<"valueInstant">>, {{primitive, <<"instant">>}, required}},
            {<<"valueInteger">>, {{primitive, <<"integer">>}, required}},
            {<<"valueMarkdown">>, {{primitive, <<"markdown">>}, required}},
            {<<"valueOid">>, {{primitive, <<"oid">>}, required}},
            {<<"valuePositiveInt">>, {{primitive, <<"positiveInt">>}, required}},
            {<<"valueString">>, {{primitive, <<"string">>}, required}},
            {<<"valueTime">>, {{primitive, <<"time">>}, required}},
            {<<"valueUnsignedInt">>, {{primitive, <<"unsignedInt">>}, required}},
            {<<"valueUri">>, {{primitive, <<"uri">>}, required}},
            {<<"valueUrl">>, {{primitive, <<"url">>}, required}},
            {<<"valueUuid">>, {{primitive, <<"uuid">>}, required}},
            {<<"valueAddress">>, {{complex, <<"Address">>}, required}},
            {<<"valueAge">>, {{complex, <<"Age">>}, required}},
            {<<"valueAnnotation">>, {{complex, <<"Annotation">>}, required}},
            {<<"valueAttachment">>, {{complex, <<"Attachment">>}, required}},
            {<<"valueCodeableConcept">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"valueCoding">>, {{complex, <<"Coding">>}, required}},
            {<<"valueContactPoint">>, {{complex, <<"ContactPoint">>}, required}},
            {<<"valueCount">>, {{complex, <<"Count">>}, required}},
            {<<"valueDistance">>, {{complex, <<"Distance">>}, required}},
            {<<"valueDuration">>, {{complex, <<"Duration">>}, required}},
            {<<"valueHumanName">>, {{complex, <<"HumanName">>}, required}},
            {<<"valueIdentifier">>, {{complex, <<"Identifier">>}, required}},
            {<<"valueMoney">>, {{complex, <<"Money">>}, required}},
            {<<"valuePeriod">>, {{complex, <<"Period">>}, required}},
            {<<"valueQuantity">>, {{complex, <<"Quantity">>}, required}},
            {<<"valueRange">>, {{complex, <<"Range">>}, required}},
            {<<"valueRatio">>, {{complex, <<"Ratio">>}, required}},
            {<<"valueReference">>, {{complex, <<"Reference">>}, required}},
            {<<"valueSampledData">>, {{complex, <<"SampledData">>}, required}},
            {<<"valueSignature">>, {{complex, <<"Signature">>}, required}},
            {<<"valueTiming">>, {{bbelement, <<"Timing">>}, required}},
            {<<"valueContactDetail">>, {{complex, <<"ContactDetail">>}, required}},
            {<<"valueContributor">>, {{complex, <<"Contributor">>}, required}},
            {<<"valueDataRequirement">>, {{complex, <<"DataRequirement">>}, required}},
            {<<"valueExpression">>, {{complex, <<"Expression">>}, required}},
            {<<"valueParameterDefinition">>, {{complex, <<"ParameterDefinition">>}, required}},
            {<<"valueRelatedArtifact">>, {{complex, <<"RelatedArtifact">>}, required}},
            {<<"valueTriggerDefinition">>, {{complex, <<"TriggerDefinition">>}, required}},
            {<<"valueUsageContext">>, {{complex, <<"UsageContext">>}, required}},
            {<<"valueDosage">>, {{bbelement, <<"Dosage">>}, required}}
            ],
            [],
            [
            {<<"valueBase64Binary">>, <<"valueBoolean">>, <<"valueCanonical">>, <<"valueCode">>, <<"valueDate">>, <<"valueDateTime">>, <<"valueDecimal">>, <<"valueId">>, <<"valueInstant">>, <<"valueInteger">>, <<"valueMarkdown">>, <<"valueOid">>, <<"valuePositiveInt">>, <<"valueString">>, <<"valueTime">>, <<"valueUnsignedInt">>, <<"valueUri">>, <<"valueUrl">>, <<"valueUuid">>, <<"valueAddress">>, <<"valueAge">>, <<"valueAnnotation">>, <<"valueAttachment">>, <<"valueCodeableConcept">>, <<"valueCoding">>, <<"valueContactPoint">>, <<"valueCount">>, <<"valueDistance">>, <<"valueDuration">>, <<"valueHumanName">>, <<"valueIdentifier">>, <<"valueMoney">>, <<"valuePeriod">>, <<"valueQuantity">>, <<"valueRange">>, <<"valueRatio">>, <<"valueReference">>, <<"valueSampledData">>, <<"valueSignature">>, <<"valueTiming">>, <<"valueContactDetail">>, <<"valueContributor">>, <<"valueDataRequirement">>, <<"valueExpression">>, <<"valueParameterDefinition">>, <<"valueRelatedArtifact">>, <<"valueTriggerDefinition">>, <<"valueUsageContext">>, <<"valueDosage">>}
            ]
} 
%%
%% TerminologyCapabilities
%% A TerminologyCapabilities resource documents a set of capabilities (behaviors) of a FHIR Terminology Server that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"TerminologyCapabilities">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, required}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"kind">>, {{code, <<"capabilitystatementkind_list">>}, required}},
            {<<"software">>, {{bbelement, <<"TerminologyCapabilities.Software">>}, optional}},
            {<<"implementation">>, {{bbelement, <<"TerminologyCapabilities.Implementation">>}, optional}},
            {<<"lockedDate">>, {{primitive, <<"boolean">>}, optional}},
            {<<"codeSystem">>, {{bbelement, <<"TerminologyCapabilities.CodeSystem">>}, list}},
            {<<"expansion">>, {{bbelement, <<"TerminologyCapabilities.Expansion">>}, optional}},
            {<<"codeSearch">>, {{code, <<"codesearchsupport_list">>}, optional}},
            {<<"validateCode">>, {{bbelement, <<"TerminologyCapabilities.ValidateCode">>}, optional}},
            {<<"translation">>, {{bbelement, <<"TerminologyCapabilities.Translation">>}, optional}},
            {<<"closure">>, {{bbelement, <<"TerminologyCapabilities.Closure">>}, optional}}
            ],
            [],
            []
} 
%%
%% TerminologyCapabilities.Software
%% A TerminologyCapabilities resource documents a set of capabilities (behaviors) of a FHIR Terminology Server that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"TerminologyCapabilities.Software">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"version">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% TerminologyCapabilities.Implementation
%% A TerminologyCapabilities resource documents a set of capabilities (behaviors) of a FHIR Terminology Server that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"TerminologyCapabilities.Implementation">> => {<<"BackboneElement">>,
            [
            {<<"description">>, {{primitive, <<"string">>}, required}},
            {<<"url">>, {{primitive, <<"url">>}, optional}}
            ],
            [],
            []
} 
%%
%% TerminologyCapabilities.CodeSystem
%% A TerminologyCapabilities resource documents a set of capabilities (behaviors) of a FHIR Terminology Server that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"TerminologyCapabilities.CodeSystem">> => {<<"BackboneElement">>,
            [
            {<<"uri">>, {{primitive, <<"canonical">>}, optional}},
            {<<"version">>, {{bbelement, <<"TerminologyCapabilities.Version">>}, list}},
            {<<"subsumption">>, {{primitive, <<"boolean">>}, optional}}
            ],
            [],
            []
} 
%%
%% TerminologyCapabilities.Version
%% A TerminologyCapabilities resource documents a set of capabilities (behaviors) of a FHIR Terminology Server that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"TerminologyCapabilities.Version">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{primitive, <<"string">>}, optional}},
            {<<"isDefault">>, {{primitive, <<"boolean">>}, optional}},
            {<<"compositional">>, {{primitive, <<"boolean">>}, optional}},
            {<<"language">>, {{primitive, <<"code">>}, list}},
            {<<"filter">>, {{bbelement, <<"TerminologyCapabilities.Filter">>}, list}},
            {<<"property">>, {{primitive, <<"code">>}, list}}
            ],
            [],
            []
} 
%%
%% TerminologyCapabilities.Filter
%% A TerminologyCapabilities resource documents a set of capabilities (behaviors) of a FHIR Terminology Server that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"TerminologyCapabilities.Filter">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{primitive, <<"code">>}, required}},
            {<<"op">>, {{primitive, <<"code">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% TerminologyCapabilities.Expansion
%% A TerminologyCapabilities resource documents a set of capabilities (behaviors) of a FHIR Terminology Server that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"TerminologyCapabilities.Expansion">> => {<<"BackboneElement">>,
            [
            {<<"hierarchical">>, {{primitive, <<"boolean">>}, optional}},
            {<<"paging">>, {{primitive, <<"boolean">>}, optional}},
            {<<"incomplete">>, {{primitive, <<"boolean">>}, optional}},
            {<<"parameter">>, {{bbelement, <<"TerminologyCapabilities.Parameter">>}, list}},
            {<<"textFilter">>, {{primitive, <<"markdown">>}, optional}}
            ],
            [],
            []
} 
%%
%% TerminologyCapabilities.Parameter
%% A TerminologyCapabilities resource documents a set of capabilities (behaviors) of a FHIR Terminology Server that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"TerminologyCapabilities.Parameter">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"code">>}, required}},
            {<<"documentation">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% TerminologyCapabilities.ValidateCode
%% A TerminologyCapabilities resource documents a set of capabilities (behaviors) of a FHIR Terminology Server that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"TerminologyCapabilities.ValidateCode">> => {<<"BackboneElement">>,
            [
            {<<"translations">>, {{primitive, <<"boolean">>}, required}}
            ],
            [],
            []
} 
%%
%% TerminologyCapabilities.Translation
%% A TerminologyCapabilities resource documents a set of capabilities (behaviors) of a FHIR Terminology Server that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"TerminologyCapabilities.Translation">> => {<<"BackboneElement">>,
            [
            {<<"needsMap">>, {{primitive, <<"boolean">>}, required}}
            ],
            [],
            []
} 
%%
%% TerminologyCapabilities.Closure
%% A TerminologyCapabilities resource documents a set of capabilities (behaviors) of a FHIR Terminology Server that may be used as a statement of actual server functionality or a statement of required or desired server implementation.
%%
    , <<"TerminologyCapabilities.Closure">> => {<<"BackboneElement">>,
            [
            {<<"translation">>, {{primitive, <<"boolean">>}, optional}}
            ],
            [],
            []
} 
%%
%% TestReport
%% A summary of information based on the results of executing a TestScript.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"TestReport">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"testreportstatus_list">>}, required}},
            {<<"testScript">>, {{complex, <<"Reference">>}, required}},
            {<<"result">>, {{code, <<"testreportresult_list">>}, required}},
            {<<"score">>, {{primitive, <<"decimal">>}, optional}},
            {<<"tester">>, {{primitive, <<"string">>}, optional}},
            {<<"issued">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"participant">>, {{bbelement, <<"TestReport.Participant">>}, list}},
            {<<"setup">>, {{bbelement, <<"TestReport.Setup">>}, optional}},
            {<<"test">>, {{bbelement, <<"TestReport.Test">>}, list}},
            {<<"teardown">>, {{bbelement, <<"TestReport.Teardown">>}, optional}}
            ],
            [],
            []
} 
%%
%% TestReport.Participant
%% A summary of information based on the results of executing a TestScript.
%%
    , <<"TestReport.Participant">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{code, <<"testreportparticipanttype_list">>}, required}},
            {<<"uri">>, {{primitive, <<"uri">>}, required}},
            {<<"display">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% TestReport.Setup
%% A summary of information based on the results of executing a TestScript.
%%
    , <<"TestReport.Setup">> => {<<"BackboneElement">>,
            [
            {<<"action">>, {{bbelement, <<"TestReport.Action">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% TestReport.Action
%% A summary of information based on the results of executing a TestScript.
%%
    , <<"TestReport.Action">> => {<<"BackboneElement">>,
            [
            {<<"operation">>, {{bbelement, <<"TestReport.Operation">>}, optional}},
            {<<"assert">>, {{bbelement, <<"TestReport.Assert">>}, optional}}
            ],
            [],
            []
} 
%%
%% TestReport.Operation
%% A summary of information based on the results of executing a TestScript.
%%
    , <<"TestReport.Operation">> => {<<"BackboneElement">>,
            [
            {<<"result">>, {{code, <<"testreportactionresult_list">>}, required}},
            {<<"message">>, {{primitive, <<"markdown">>}, optional}},
            {<<"detail">>, {{primitive, <<"uri">>}, optional}}
            ],
            [],
            []
} 
%%
%% TestReport.Assert
%% A summary of information based on the results of executing a TestScript.
%%
    , <<"TestReport.Assert">> => {<<"BackboneElement">>,
            [
            {<<"result">>, {{code, <<"testreportactionresult_list">>}, required}},
            {<<"message">>, {{primitive, <<"markdown">>}, optional}},
            {<<"detail">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% TestReport.Test
%% A summary of information based on the results of executing a TestScript.
%%
    , <<"TestReport.Test">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"action">>, {{bbelement, <<"TestReport.Action1">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% TestReport.Action1
%% A summary of information based on the results of executing a TestScript.
%%
    , <<"TestReport.Action1">> => {<<"BackboneElement">>,
            [
            {<<"operation">>, {{bbelement, <<"TestReport.Operation">>}, optional}},
            {<<"assert">>, {{bbelement, <<"TestReport.Assert">>}, optional}}
            ],
            [],
            []
} 
%%
%% TestReport.Teardown
%% A summary of information based on the results of executing a TestScript.
%%
    , <<"TestReport.Teardown">> => {<<"BackboneElement">>,
            [
            {<<"action">>, {{bbelement, <<"TestReport.Action2">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% TestReport.Action2
%% A summary of information based on the results of executing a TestScript.
%%
    , <<"TestReport.Action2">> => {<<"BackboneElement">>,
            [
            {<<"operation">>, {{bbelement, <<"TestReport.Operation">>}, required}}
            ],
            [],
            []
} 
%%
%% TestScript
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"TestScript">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, required}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, optional}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"origin">>, {{bbelement, <<"TestScript.Origin">>}, list}},
            {<<"destination">>, {{bbelement, <<"TestScript.Destination">>}, list}},
            {<<"metadata">>, {{bbelement, <<"TestScript.Metadata">>}, optional}},
            {<<"fixture">>, {{bbelement, <<"TestScript.Fixture">>}, list}},
            {<<"profile">>, {{complex, <<"Reference">>}, list}},
            {<<"variable">>, {{bbelement, <<"TestScript.Variable">>}, list}},
            {<<"setup">>, {{bbelement, <<"TestScript.Setup">>}, optional}},
            {<<"test">>, {{bbelement, <<"TestScript.Test">>}, list}},
            {<<"teardown">>, {{bbelement, <<"TestScript.Teardown">>}, optional}}
            ],
            [],
            []
} 
%%
%% TestScript.Origin
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.Origin">> => {<<"BackboneElement">>,
            [
            {<<"index">>, {{primitive, <<"integer">>}, required}},
            {<<"profile">>, {{complex, <<"Coding">>}, required}}
            ],
            [],
            []
} 
%%
%% TestScript.Destination
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.Destination">> => {<<"BackboneElement">>,
            [
            {<<"index">>, {{primitive, <<"integer">>}, required}},
            {<<"profile">>, {{complex, <<"Coding">>}, required}}
            ],
            [],
            []
} 
%%
%% TestScript.Metadata
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.Metadata">> => {<<"BackboneElement">>,
            [
            {<<"link">>, {{bbelement, <<"TestScript.Link">>}, list}},
            {<<"capability">>, {{bbelement, <<"TestScript.Capability">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% TestScript.Link
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.Link">> => {<<"BackboneElement">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, required}},
            {<<"description">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% TestScript.Capability
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.Capability">> => {<<"BackboneElement">>,
            [
            {<<"required">>, {{primitive, <<"boolean">>}, required}},
            {<<"validated">>, {{primitive, <<"boolean">>}, required}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"origin">>, {{primitive, <<"integer">>}, list}},
            {<<"destination">>, {{primitive, <<"integer">>}, optional}},
            {<<"link">>, {{primitive, <<"uri">>}, list}},
            {<<"capabilities">>, {{primitive, <<"canonical">>}, required}}
            ],
            [],
            []
} 
%%
%% TestScript.Fixture
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.Fixture">> => {<<"BackboneElement">>,
            [
            {<<"autocreate">>, {{primitive, <<"boolean">>}, required}},
            {<<"autodelete">>, {{primitive, <<"boolean">>}, required}},
            {<<"resource">>, {{complex, <<"Reference">>}, optional}}
            ],
            [],
            []
} 
%%
%% TestScript.Variable
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.Variable">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"defaultValue">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"expression">>, {{primitive, <<"string">>}, optional}},
            {<<"headerField">>, {{primitive, <<"string">>}, optional}},
            {<<"hint">>, {{primitive, <<"string">>}, optional}},
            {<<"path">>, {{primitive, <<"string">>}, optional}},
            {<<"sourceId">>, {{primitive, <<"id">>}, optional}}
            ],
            [],
            []
} 
%%
%% TestScript.Setup
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.Setup">> => {<<"BackboneElement">>,
            [
            {<<"action">>, {{bbelement, <<"TestScript.Action">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% TestScript.Action
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.Action">> => {<<"BackboneElement">>,
            [
            {<<"operation">>, {{bbelement, <<"TestScript.Operation">>}, optional}},
            {<<"assert">>, {{bbelement, <<"TestScript.Assert">>}, optional}}
            ],
            [],
            []
} 
%%
%% TestScript.Operation
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.Operation">> => {<<"BackboneElement">>,
            [
            {<<"type">>, {{complex, <<"Coding">>}, optional}},
            {<<"resource">>, {{primitive, <<"code">>}, optional}},
            {<<"label">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"accept">>, {{primitive, <<"code">>}, optional}},
            {<<"contentType">>, {{primitive, <<"code">>}, optional}},
            {<<"destination">>, {{primitive, <<"integer">>}, optional}},
            {<<"encodeRequestUrl">>, {{primitive, <<"boolean">>}, required}},
            {<<"method">>, {{code, <<"testscriptrequestmethodcode_list">>}, optional}},
            {<<"origin">>, {{primitive, <<"integer">>}, optional}},
            {<<"params">>, {{primitive, <<"string">>}, optional}},
            {<<"requestHeader">>, {{bbelement, <<"TestScript.RequestHeader">>}, list}},
            {<<"requestId">>, {{primitive, <<"id">>}, optional}},
            {<<"responseId">>, {{primitive, <<"id">>}, optional}},
            {<<"sourceId">>, {{primitive, <<"id">>}, optional}},
            {<<"targetId">>, {{primitive, <<"id">>}, optional}},
            {<<"url">>, {{primitive, <<"string">>}, optional}}
            ],
            [],
            []
} 
%%
%% TestScript.RequestHeader
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.RequestHeader">> => {<<"BackboneElement">>,
            [
            {<<"field">>, {{primitive, <<"string">>}, required}},
            {<<"value">>, {{primitive, <<"string">>}, required}}
            ],
            [],
            []
} 
%%
%% TestScript.Assert
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.Assert">> => {<<"BackboneElement">>,
            [
            {<<"label">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"direction">>, {{code, <<"assertiondirectiontype_list">>}, optional}},
            {<<"compareToSourceId">>, {{primitive, <<"string">>}, optional}},
            {<<"compareToSourceExpression">>, {{primitive, <<"string">>}, optional}},
            {<<"compareToSourcePath">>, {{primitive, <<"string">>}, optional}},
            {<<"contentType">>, {{primitive, <<"code">>}, optional}},
            {<<"expression">>, {{primitive, <<"string">>}, optional}},
            {<<"headerField">>, {{primitive, <<"string">>}, optional}},
            {<<"minimumId">>, {{primitive, <<"string">>}, optional}},
            {<<"navigationLinks">>, {{primitive, <<"boolean">>}, optional}},
            {<<"operator">>, {{code, <<"assertionoperatortype_list">>}, optional}},
            {<<"path">>, {{primitive, <<"string">>}, optional}},
            {<<"requestMethod">>, {{code, <<"testscriptrequestmethodcode_list">>}, optional}},
            {<<"requestURL">>, {{primitive, <<"string">>}, optional}},
            {<<"resource">>, {{primitive, <<"code">>}, optional}},
            {<<"response">>, {{code, <<"assertionresponsetypes_list">>}, optional}},
            {<<"responseCode">>, {{primitive, <<"string">>}, optional}},
            {<<"sourceId">>, {{primitive, <<"id">>}, optional}},
            {<<"validateProfileId">>, {{primitive, <<"id">>}, optional}},
            {<<"value">>, {{primitive, <<"string">>}, optional}},
            {<<"warningOnly">>, {{primitive, <<"boolean">>}, required}}
            ],
            [],
            []
} 
%%
%% TestScript.Test
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.Test">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"description">>, {{primitive, <<"string">>}, optional}},
            {<<"action">>, {{bbelement, <<"TestScript.Action1">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% TestScript.Action1
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.Action1">> => {<<"BackboneElement">>,
            [
            {<<"operation">>, {{bbelement, <<"TestScript.Operation">>}, optional}},
            {<<"assert">>, {{bbelement, <<"TestScript.Assert">>}, optional}}
            ],
            [],
            []
} 
%%
%% TestScript.Teardown
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.Teardown">> => {<<"BackboneElement">>,
            [
            {<<"action">>, {{bbelement, <<"TestScript.Action2">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% TestScript.Action2
%% A structured set of tests against a FHIR server or client implementation to determine compliance against the FHIR specification.
%%
    , <<"TestScript.Action2">> => {<<"BackboneElement">>,
            [
            {<<"operation">>, {{bbelement, <<"TestScript.Operation">>}, required}}
            ],
            [],
            []
} 
%%
%% ValueSet
%% A ValueSet resource instance specifies a set of codes drawn from one or more code systems, intended for use in a particular context. Value sets link between [[[CodeSystem]]] definitions and their use in [coded elements](terminologies.html).
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"ValueSet">> => {<<"DomainResource">>,
            [
            {<<"url">>, {{primitive, <<"uri">>}, optional}},
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"name">>, {{primitive, <<"string">>}, optional}},
            {<<"title">>, {{primitive, <<"string">>}, optional}},
            {<<"status">>, {{code, <<"publicationstatus_list">>}, required}},
            {<<"experimental">>, {{primitive, <<"boolean">>}, optional}},
            {<<"date">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"publisher">>, {{primitive, <<"string">>}, optional}},
            {<<"contact">>, {{complex, <<"ContactDetail">>}, list}},
            {<<"description">>, {{primitive, <<"markdown">>}, optional}},
            {<<"useContext">>, {{complex, <<"UsageContext">>}, list}},
            {<<"jurisdiction">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"immutable">>, {{primitive, <<"boolean">>}, optional}},
            {<<"purpose">>, {{primitive, <<"markdown">>}, optional}},
            {<<"copyright">>, {{primitive, <<"markdown">>}, optional}},
            {<<"compose">>, {{bbelement, <<"ValueSet.Compose">>}, optional}},
            {<<"expansion">>, {{bbelement, <<"ValueSet.Expansion">>}, optional}}
            ],
            [],
            []
} 
%%
%% ValueSet.Compose
%% A ValueSet resource instance specifies a set of codes drawn from one or more code systems, intended for use in a particular context. Value sets link between [[[CodeSystem]]] definitions and their use in [coded elements](terminologies.html).
%%
    , <<"ValueSet.Compose">> => {<<"BackboneElement">>,
            [
            {<<"lockedDate">>, {{primitive, <<"date">>}, optional}},
            {<<"inactive">>, {{primitive, <<"boolean">>}, optional}},
            {<<"include">>, {{bbelement, <<"ValueSet.Include">>}, non_empty_list}},
            {<<"exclude">>, {{bbelement, <<"ValueSet.Include">>}, list}}
            ],
            [],
            []
} 
%%
%% ValueSet.Include
%% A ValueSet resource instance specifies a set of codes drawn from one or more code systems, intended for use in a particular context. Value sets link between [[[CodeSystem]]] definitions and their use in [coded elements](terminologies.html).
%%
    , <<"ValueSet.Include">> => {<<"BackboneElement">>,
            [
            {<<"system">>, {{primitive, <<"uri">>}, optional}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"concept">>, {{bbelement, <<"ValueSet.Concept">>}, list}},
            {<<"filter">>, {{bbelement, <<"ValueSet.Filter">>}, list}},
            {<<"valueSet">>, {{primitive, <<"canonical">>}, list}}
            ],
            [],
            []
} 
%%
%% ValueSet.Concept
%% A ValueSet resource instance specifies a set of codes drawn from one or more code systems, intended for use in a particular context. Value sets link between [[[CodeSystem]]] definitions and their use in [coded elements](terminologies.html).
%%
    , <<"ValueSet.Concept">> => {<<"BackboneElement">>,
            [
            {<<"code">>, {{primitive, <<"code">>}, required}},
            {<<"display">>, {{primitive, <<"string">>}, optional}},
            {<<"designation">>, {{bbelement, <<"ValueSet.Designation">>}, list}}
            ],
            [],
            []
} 
%%
%% ValueSet.Designation
%% A ValueSet resource instance specifies a set of codes drawn from one or more code systems, intended for use in a particular context. Value sets link between [[[CodeSystem]]] definitions and their use in [coded elements](terminologies.html).
%%
    , <<"ValueSet.Designation">> => {<<"BackboneElement">>,
            [
            {<<"language">>, {{primitive, <<"code">>}, optional}},
            {<<"use">>, {{complex, <<"Coding">>}, optional}},
            {<<"value">>, {{primitive, <<"string">>}, required}}
            ],
            [],
            []
} 
%%
%% ValueSet.Filter
%% A ValueSet resource instance specifies a set of codes drawn from one or more code systems, intended for use in a particular context. Value sets link between [[[CodeSystem]]] definitions and their use in [coded elements](terminologies.html).
%%
    , <<"ValueSet.Filter">> => {<<"BackboneElement">>,
            [
            {<<"property">>, {{primitive, <<"code">>}, required}},
            {<<"op">>, {{code, <<"filteroperator_list">>}, required}},
            {<<"value">>, {{primitive, <<"string">>}, required}}
            ],
            [],
            []
} 
%%
%% ValueSet.Expansion
%% A ValueSet resource instance specifies a set of codes drawn from one or more code systems, intended for use in a particular context. Value sets link between [[[CodeSystem]]] definitions and their use in [coded elements](terminologies.html).
%%
    , <<"ValueSet.Expansion">> => {<<"BackboneElement">>,
            [
            {<<"identifier">>, {{primitive, <<"uri">>}, optional}},
            {<<"timestamp">>, {{primitive, <<"dateTime">>}, required}},
            {<<"total">>, {{primitive, <<"integer">>}, optional}},
            {<<"offset">>, {{primitive, <<"integer">>}, optional}},
            {<<"parameter">>, {{bbelement, <<"ValueSet.Parameter">>}, list}},
            {<<"contains">>, {{bbelement, <<"ValueSet.Contains">>}, list}}
            ],
            [],
            []
} 
%%
%% ValueSet.Parameter
%% A ValueSet resource instance specifies a set of codes drawn from one or more code systems, intended for use in a particular context. Value sets link between [[[CodeSystem]]] definitions and their use in [coded elements](terminologies.html).
%%
    , <<"ValueSet.Parameter">> => {<<"BackboneElement">>,
            [
            {<<"name">>, {{primitive, <<"string">>}, required}},
            {<<"valueString">>, {{primitive, <<"string">>}, optional}},
            {<<"valueBoolean">>, {{primitive, <<"boolean">>}, optional}},
            {<<"valueInteger">>, {{primitive, <<"integer">>}, optional}},
            {<<"valueDecimal">>, {{primitive, <<"decimal">>}, optional}},
            {<<"valueUri">>, {{primitive, <<"uri">>}, optional}},
            {<<"valueCode">>, {{primitive, <<"code">>}, optional}},
            {<<"valueDateTime">>, {{primitive, <<"dateTime">>}, optional}}
            ],
            [],
            [
            {<<"valueString">>, <<"valueBoolean">>, <<"valueInteger">>, <<"valueDecimal">>, <<"valueUri">>, <<"valueCode">>, <<"valueDateTime">>}
            ]
} 
%%
%% ValueSet.Contains
%% A ValueSet resource instance specifies a set of codes drawn from one or more code systems, intended for use in a particular context. Value sets link between [[[CodeSystem]]] definitions and their use in [coded elements](terminologies.html).
%%
    , <<"ValueSet.Contains">> => {<<"BackboneElement">>,
            [
            {<<"system">>, {{primitive, <<"uri">>}, optional}},
            {<<"abstract">>, {{primitive, <<"boolean">>}, optional}},
            {<<"inactive">>, {{primitive, <<"boolean">>}, optional}},
            {<<"version">>, {{primitive, <<"string">>}, optional}},
            {<<"code">>, {{primitive, <<"code">>}, optional}},
            {<<"display">>, {{primitive, <<"string">>}, optional}},
            {<<"designation">>, {{bbelement, <<"ValueSet.Designation">>}, list}},
            {<<"contains">>, {{bbelement, <<"ValueSet.Contains">>}, list}}
            ],
            [],
            []
} 
%%
%% VerificationResult
%% Describes validation requirements, source(s), status and dates for one or more elements.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"VerificationResult">> => {<<"DomainResource">>,
            [
            {<<"target">>, {{complex, <<"Reference">>}, list}},
            {<<"targetLocation">>, {{primitive, <<"string">>}, list}},
            {<<"need">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"status">>, {{code, <<"status_list">>}, required}},
            {<<"statusDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"validationType">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"validationProcess">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"frequency">>, {{bbelement, <<"Timing">>}, optional}},
            {<<"lastPerformed">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"nextScheduled">>, {{primitive, <<"date">>}, optional}},
            {<<"failureAction">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"primarySource">>, {{bbelement, <<"VerificationResult.PrimarySource">>}, list}},
            {<<"attestation">>, {{bbelement, <<"VerificationResult.Attestation">>}, optional}},
            {<<"validator">>, {{bbelement, <<"VerificationResult.Validator">>}, list}}
            ],
            [],
            []
} 
%%
%% VerificationResult.PrimarySource
%% Describes validation requirements, source(s), status and dates for one or more elements.
%%
    , <<"VerificationResult.PrimarySource">> => {<<"BackboneElement">>,
            [
            {<<"who">>, {{complex, <<"Reference">>}, optional}},
            {<<"type">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"communicationMethod">>, {{complex, <<"CodeableConcept">>}, list}},
            {<<"validationStatus">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"validationDate">>, {{primitive, <<"dateTime">>}, optional}},
            {<<"canPushUpdates">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"pushTypeAvailable">>, {{complex, <<"CodeableConcept">>}, list}}
            ],
            [],
            []
} 
%%
%% VerificationResult.Attestation
%% Describes validation requirements, source(s), status and dates for one or more elements.
%%
    , <<"VerificationResult.Attestation">> => {<<"BackboneElement">>,
            [
            {<<"who">>, {{complex, <<"Reference">>}, optional}},
            {<<"onBehalfOf">>, {{complex, <<"Reference">>}, optional}},
            {<<"communicationMethod">>, {{complex, <<"CodeableConcept">>}, optional}},
            {<<"date">>, {{primitive, <<"date">>}, optional}},
            {<<"sourceIdentityCertificate">>, {{primitive, <<"string">>}, optional}},
            {<<"proxyIdentityCertificate">>, {{primitive, <<"string">>}, optional}},
            {<<"proxySignature">>, {{complex, <<"Signature">>}, optional}},
            {<<"sourceSignature">>, {{complex, <<"Signature">>}, optional}}
            ],
            [],
            []
} 
%%
%% VerificationResult.Validator
%% Describes validation requirements, source(s), status and dates for one or more elements.
%%
    , <<"VerificationResult.Validator">> => {<<"BackboneElement">>,
            [
            {<<"organization">>, {{complex, <<"Reference">>}, required}},
            {<<"identityCertificate">>, {{primitive, <<"string">>}, optional}},
            {<<"attestationSignature">>, {{complex, <<"Signature">>}, optional}}
            ],
            [],
            []
} 
%%
%% VisionPrescription
%% An authorization for the provision of glasses and/or contact lenses to a patient.
%% If the element is present, it must have either a @value, an @id, or extensions
%%
    , <<"VisionPrescription">> => {<<"DomainResource">>,
            [
            {<<"identifier">>, {{complex, <<"Identifier">>}, list}},
            {<<"status">>, {{code, <<"financialresourcestatuscodes_list">>}, required}},
            {<<"created">>, {{primitive, <<"dateTime">>}, required}},
            {<<"patient">>, {{complex, <<"Reference">>}, required}},
            {<<"encounter">>, {{complex, <<"Reference">>}, optional}},
            {<<"dateWritten">>, {{primitive, <<"dateTime">>}, required}},
            {<<"prescriber">>, {{complex, <<"Reference">>}, required}},
            {<<"lensSpecification">>, {{bbelement, <<"VisionPrescription.LensSpecification">>}, non_empty_list}}
            ],
            [],
            []
} 
%%
%% VisionPrescription.LensSpecification
%% An authorization for the provision of glasses and/or contact lenses to a patient.
%%
    , <<"VisionPrescription.LensSpecification">> => {<<"BackboneElement">>,
            [
            {<<"product">>, {{complex, <<"CodeableConcept">>}, required}},
            {<<"eye">>, {{code, <<"visioneyes_list">>}, required}},
            {<<"sphere">>, {{primitive, <<"decimal">>}, optional}},
            {<<"cylinder">>, {{primitive, <<"decimal">>}, optional}},
            {<<"axis">>, {{primitive, <<"integer">>}, optional}},
            {<<"prism">>, {{bbelement, <<"VisionPrescription.Prism">>}, list}},
            {<<"add">>, {{primitive, <<"decimal">>}, optional}},
            {<<"power">>, {{primitive, <<"decimal">>}, optional}},
            {<<"backCurve">>, {{primitive, <<"decimal">>}, optional}},
            {<<"diameter">>, {{primitive, <<"decimal">>}, optional}},
            {<<"duration">>, {{complex, <<"Quantity">>}, optional}},
            {<<"color">>, {{primitive, <<"string">>}, optional}},
            {<<"brand">>, {{primitive, <<"string">>}, optional}},
            {<<"note">>, {{complex, <<"Annotation">>}, list}}
            ],
            [],
            []
} 
%%
%% VisionPrescription.Prism
%% An authorization for the provision of glasses and/or contact lenses to a patient.
%%
    , <<"VisionPrescription.Prism">> => {<<"BackboneElement">>,
            [
            {<<"amount">>, {{primitive, <<"decimal">>}, required}},
            {<<"base">>, {{code, <<"visionbase_list">>}, required}}
            ],
            [],
            []
}        }).


-endif.
