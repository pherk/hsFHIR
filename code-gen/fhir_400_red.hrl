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
            [{<<"url">>, <<"uri_primitive">>}],
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
        }).

-endif.

