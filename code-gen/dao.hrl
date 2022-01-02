-ifndef(DAO_N1QL).
-define(DAO_N1QL, true).

-type search_datatype() :: boolean | date | number | 'reference' | string | token | composite | quantity | uri | special.
-type search_comparator() :: 	eq | ne | gt | lt | ge | le | sa | eb | ap.
-type search_modifier() :: missing | exact | contains | 'not' | text | in | 'not-in' | below | above | type | identifier | ofType.
-type indextype() :: no_index | simple | array | array2.
-record(search_info, {
          fhir_datatype :: atom(),
          search_datatype  :: search_datatype(),
          path      :: tuple(),
          target    :: [atom()],
          multipleOr :: boolean(),
          multipleAnd :: boolean(),
          comparator :: [search_comparator()],
          modifier   :: [search_modifier()],
          index     :: indextype()
         }).
-type search_info() :: #search_info{}.

-record(search_parameter, {
          code :: binary(),
          search_info :: map()
         }).
-type search_parameter() :: #search_parameter{}.

-define(RESERVED_KEYS,
    #{
      <<"_content">>     => {param, token}
    , <<"_id">>          => {param, token}
    , <<"_lastUpdated">> => {param, date}
    , <<"_profile">>     => {param, token}
    , <<"_query">>       => {param, token} % ??
    , <<"_security">>    => {param, token}
    , <<"_tag">>         => {param, token}
    , <<"_elements">>    => {param, token}
    , <<"_count">>       => {result, number}
    , <<"_format">>      => {result, token}
    , <<"_include">>     => {result, token}
    , <<"_pretty">>      => {result, boolean}
    , <<"_revinclude">>  => {result, token}
    , <<"_sort">>        => {result, token}
    , <<"_source">>      => {result, token} % ???
    , <<"_summary">>     => {result, token}
    , <<"_total">>       => {result, token}      % none, estimate, accurate 
    , <<"_contained">>   => {result, token}
    , <<"_containedType">> => {result, token}
    , <<"page">>         => {result, number}
    , <<"_text">>        => {special, string}
    , <<"_type">>        => {special, token}
    , <<"_filter">>      => {special, token}
    }).
%%  
-define(modifier_restriction,
    #{
       <<"missing">> => <<"*">>
     , <<"exact">>  => string
     , <<"contains" => string
     , <<"starts-with" => string      % non-standard
     , <<"text">    => token
     , <<"text">    => token
     , <<"in">      => token
     , <<"below">   => [token, uri]
     , <<"above">   => [token, uri]
     , <<"not-in">  => token
     , <<"text">    => token
     , <<"type">    => reference
     }).

-define(EXPAND_KEYS, #{
      <<"filter">>  => {}	
    , <<"date">>  => {}	
    , <<"offset">>  => {}	
    , <<"count">>  => {}	
    , <<"includeDesignations">>  => {}	
    , <<"designation">>  => {}	
    , <<"includeDefinition">>  => {}	
    , <<"activeOnly">>  => {}	
    , <<"excludeNested">>  => {}	
    , <<"excludeNotForUI">>  => {}	
    , <<"excludePostCoordinated">>  => {}	
    , <<"displayLanguage">>  => {}	
    , <<"exclude-system">>  => {}	
    , <<"system-version">>  => {}	
    , <<"check-system-version">>  => {}	
    , <<"force-system-version">>  => {}	
    }).

-define(SUMMARY_TOKENS, [
    <<"true">>,      % Return a limited subset of elements from the resource. 
                     % This subset SHOULD consist solely of all supported elements that are marked as "summary"
    <<"text">>,      % Return only the "text" element, the 'id' element, 
                     % the 'meta' element, and only top-level mandatory elements
    <<"data">>,      % Remove the text element
    <<"count">>,     % Search only: just return a count of the matching resources, without returning the actual matches
    <<"false">>      % Return all parts of the resource(s)
    ]).

-define(SEARCH_INFO,
    #{
         <<"Resource">>         => #{
             <<"_content">>     => {string, string, undefined, simple},
             <<"_id">>          => {id, token, {<<"id">>}, simple},
             <<"_lastUpdated">>	=> {date, date, {<<"meta">>,<<"lastUpdated">>}, simple},
             <<"_profile">>     => {uri, string, {<<"meta">>,<<"profile">>}, cc},
             <<"_query">>       => {token, string, undefined, simple},
             <<"_security">>    => {token, string, {<<"meta">>,<<"security">>}, cc},
             <<"_source">>      => {uri, string, {<<"meta">>,<<"source">>}, simpe},
             <<"_tag">>         => {token, string, {<<"meta">>,<<"tag">>}, cc}
            },
         <<"DomainResource">>         => #{   % includes Resouce map
             <<"_content">>     => {string, string, undefined, simple},
             <<"_id">>          => {id, token, {<<"id">>}, simple},
             <<"_lastUpdated">>	=> {date, date, {<<"meta">>,<<"lastUpdated">>}, simple},
             <<"_profile">>     => {uri, string, {<<"meta">>,<<"profile">>}, cc},
             <<"_query">>       => {token, string, undefined, simple},
             <<"_security">>    => {token, string, {<<"meta">>,<<"security">>}, cc},
             <<"_source">>      => {uri, string, {<<"meta">>,<<"source">>}, simpe},
             <<"_tag">>         => {token, string, {<<"meta">>,<<"tag">>}, cc},
             <<"_text">>         => {string, string, {<<"text">>}, simple}
            },
         <<"metadata">>         => #{
             <<"mode">>         => {mode, token, {<<"mode">>}, no}
            },
         <<"ActivityDefinition">> => #{
             <<"identifier">>   => {'Identifier', token, {<<"identifier">>,<<"value">>}, array}
            },
         <<"CarePlan">> => #{
             <<"identifier">>   => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"status">>       => {boolean, boolean, {<<"status">>}, simple},
             <<"subject">>      => {'Reference', token, {<<"subject">>}, simple}
            },
         <<"CareTeam">> => #{
             <<"identifier">>       => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"status">>     => {boolean, boolean, {<<"status">>}, simple},
             <<"subject">>    => {'Reference', token, {<<"subject">>}, simple}
            },
         <<"ClinicalImpression">> => #{
             <<"identifier">>       => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"status">>     => {boolean, boolean, {<<"status">>}, simple},
             <<"subject">>    => {'Reference', token, {<<"subject">>}, simple}
            },
         <<"CodeSystem">> => #{
             <<"code">>       => {code, token},
             <<"property">>   => {code, token},
             <<"system">>     => {uri, token},
             <<"version">>    => {uri, token}
            },
         <<"Communication">> => #{
             <<"identifier">>       => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"status">>     => {boolean, boolean, {<<"status">>}, simple},
             <<"subject">>    => {'Reference', token, {<<"subject">>}, simple}
            },
         <<"Composition">> => #{
             <<"identifier">>       => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"status">>     => {boolean, boolean, {<<"status">>}, simple},
             <<"subject">>    => {'Reference', token, {<<"subject">>}, simple}
            },
         <<"Condition">> => #{
             <<"identifier">>       => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"subject">>    => {'Reference', token, {<<"subject">>}, simple}
            },
         <<"Consent">> => #{
             <<"identifier">>       => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"status">>     => {boolean, boolean, {<<"status">>}, simple},
             <<"subject">>    => {'Reference', token, {<<"subject">>}, simple}
            },
         <<"Encounter">> => #{
             <<"identifier">>       => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"date">>             => {'Period', date, {<<"period">>, [<<"start">>, <<"end">>]}, simple},
             <<"participant">>      => {'Reference', token, {<<"participant">>,<<"individual">>}, array},
             <<"participant-type">> => {code, token, {<<"participant">>,<<"type">>}, array_array},
             <<"status">>     => {boolean, boolean, {<<"status">>}, simple},
             <<"subject">>    => {'Reference', token, {<<"subject">>}, simple},
             <<"class">>       => {'Coding', token},
             <<"location-period">>    => {'Period', date, {<<"location.period">>,[<<"start">>,<<"end">>]}, simple},
             <<"reason-code">>  => {'CodeableConcept', token},
             <<"serviceType">>  => {'CodeableConcept', token},
             <<"type">>       => {code, token}
            },
         <<"EpisodeOfCare">> => #{
             <<"identifier">>       => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"status">>     => {boolean, boolean, {<<"status">>}, simple},
             <<"subject">>    => {'Reference', token, {<<"subject">>}, simple}
            },
         <<"Goal">> => #{
             <<"identifier">>       => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"lifecycle-status">>     => {boolean, boolean, {<<"lifecycleStatus">>}, simple},
             <<"subject">>    => {'Reference', token, {<<"subject">>}, simple}
            },
         <<"Observation">> => #{
             <<"identifier">>       => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"status">>     => {boolean, boolean, {<<"status">>}, simple},
             <<"subject">>    => {'Reference', token, {<<"subject">>}, simple}
            },
         <<"Order">> => #{
             <<"identifier">>       => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"status">>     => {boolean, boolean, {<<"status">>}, simple},
             <<"subject">>    => {'Reference', token, {<<"subject">>}, simple}
            },
         <<"Patient">> => #{
             <<"identifier">> => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"active">>     => {code, token, {<<"active">>}, simple},
             <<"birthdate">>  => {date, date, {<<"birthDate">>}, simple},
             <<"family">>     => {string, string, {<<"name">>,<<"family">>}, [array,simple]},
             <<"given">>      => {string, string, {<<"name">>,<<"given">>}, [array,array]},
             <<"name-use">>   => {code, token, {<<"name">>,<<"use">>}, [array,simple]},
             <<"maritalStatus">>   => {code, token, {<<"maritalStatus">>}, cc},
             <<"language">>   => {code, token, {<<"communication">>, <<"language">>}, [array,cc]},
             <<"gender">>     => {code, token, {<<"gender">>}, simple}
            },
         <<"PlanDefinition">> => #{
             <<"identifier">> => {'Identifier', token, {<<"identifier">>,<<"value">>}, array}
            },
         <<"Practitioner">> => #{
             <<"identifier">> => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"active">>     => {code, token, {<<"active">>}, simple},
             <<"birthdate">>  => {date, date, {<<"birthDate">>}, simple},
             <<"family">>     => {string, string, {<<"name">>,<<"family">>}, [array,simple]},
             <<"given">>      => {string, string, {<<"name">>,<<"given">>}, [array,array]},
             <<"name-use">>   => {code, token, {<<"name">>,<<"use">>}, [array,simple]},
             <<"communication">>   => {code, token, {<<"communication">>}, [array,cc]},
             <<"gender">>     => {code, token, {<<"gender">>}, simple},
             <<"address-city">> => {string, string, {<<"address">>,<<"city">>}, [array,simple]},
             <<"address-postalcode">> =>    {string,string, {<<"address">>,<<"postalcode">>},[array,simple]},
             <<"address-use">> => {code,token, {<<"address">>,<<"use">>}, [array,simple]},
             <<"email">>       => {string,token, {<<"telecom">>,<<"system">>}, [array,simple]},
             <<"phone">>       => {string,token, {<<"telecom">>,<<"system">>}, [array,simple]}
            },
         <<"PractitionerRole">> => #{
             <<"identifier">>  => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"active">>      => {code, token, {<<"active">>}, simple},
             <<"date">>        => {'Period', date, {<<"period">>,[<<"start">>,<<"end">>]}, simple},
             <<"email">>       => {string,token, {<<"telecom">>,<<"system">>}, [array,simple]},
             <<"endpoint">>    => {'Reference', token, {<<"endpoint">>}, simple},
             <<"location">>    => {'Reference', token, {<<"location">>}, simple},
             <<"organization">>    => {'Reference', token, {<<"organization">>}, simple},
             <<"phone">>       => {string,token, {<<"telecom">>,<<"system">>}, [array,simple]},
             <<"practitioner">>    => {'Reference', token, {<<"practitioner">>}, simple},
             <<"role">>        => {string,token, {<<"code">>}, [array,simple]},
             <<"service">>     => {'Reference', token, {<<"healthcareService">>}, simple},
             <<"specialty">>   => {string,token, {<<"specialty">>}, [array,simple]},
             <<"telecom">>     => {string,token, {<<"telecom">>, <<"system">>}, [array,simple]}
            },
         <<"Protocol">> => #{
             <<"identifier">> => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"status">>     => {boolean, boolean, {<<"status">>}, simple},
             <<"subject">>    => {'Reference', token, {<<"subject">>}, simple}
            },
         <<"Provenance">> => #{
             <<"identifier">> => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"status">>     => {boolean, boolean, {<<"status">>}, simple},
             <<"subject">>    => {'Reference', token, {<<"subject">>}, simple}
            },
         <<"Questionnaire">> => #{
             <<"identifier">> => {'Identifier', token, {<<"identifier">>,<<"value">>}, array}
            },
         <<"QuestionnaireResponse">> => #{
             <<"identifier">>       => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"status">>     => {boolean, boolean, {<<"status">>}, simple},
             <<"subject">>    => {'Reference', token, {<<"subject">>}, simple}
            },
         <<"ReferralRequest">> => #{
             <<"identifier">>       => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"status">>     => {boolean, boolean, {<<"status">>}, simple}
            },
         <<"RequestGroup">> => #{
             <<"identifier">>       => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"status">>     => {boolean, boolean, {<<"status">>}, simple},
             <<"subject">>    => {'Reference', token, {<<"subject">>}, simple}
            },
         <<"Task">> => #{
             <<"identifier">>       => {'Identifier', token, {<<"identifier">>, [<<"value">>,<<"system">>]}, array},
             <<"code">>             => {'CodeableConcept', token, {<<"code">>}, simple},
             <<"for">>              => {'Reference', reference, {<<"for">>, <<"reference">>}, simple},
             <<"owner">>            => {'Reference', reference, {<<"owner">>}, simple},
             <<"requester">>        => {'Reference', reference, {<<"requester">>}, simple},
             <<"recipient">>        => {'Reference', reference, {<<"restriction">>,<<"recipient">>}, array},
             <<"restriction-date">> => {'Period', date, {<<"restriction.period">>,[<<"start">>,<<"end">>]}, simple},
             <<"status">>           => {boolean, boolean, {<<"status">>}, simple}
            },
         <<"ValueSet">> => #{
             <<"system">>     => {uri, token},
             <<"code">>       => {code, token},
             <<"url">>        => {uri, token},
             <<"display">>    => {string, token}
            },
         <<"Other">> => #{   % used only for test purposes (query.erl)
             <<"identifier">>  => {'Identifier', token, {<<"identifier">>,<<"value">>}, array},
             <<"code">>       => {code, token},
             <<"end">>        => {integer, number},
             <<"recipient">>  => {'Reference', 'reference'},
             <<"start">>      => {integer, number},
             <<"status">>     => {code, token},
             <<"subject">>    => {'Reference', 'reference'},
             <<"type">>       => {code, token},
             <<"value-quantity">> => {quantity, quantity}
            }
     }).

-define(MANDATORY,
    #{
         <<"ActivityDefinition">> => [
             <<"status">>
            ],
         <<"CarePlan">> => [
             <<"status">>,
             <<"intent">>,
             <<"subject">>
            ],
         <<"CareTeam">> => [
            ],
         <<"ClinicalImpression">> => [
             <<"status">>,
             <<"subject">>
            ],
         <<"CodeSystem">> => [
             <<"content">>, 
             <<"status">>
            ],
         <<"Communication">> => [
             <<"status">>
            ],
         <<"Composition">> => [
             <<"status">>,       
             <<"type">>,
             <<"date">>,
             <<"author">>,
             <<"title">>
            ],
         <<"Condition">> => [
             <<"subject">>
            ],
         <<"Consent">> => [
             <<"status">>,
             <<"scope">>,
             <<"category">>
            ],
         <<"Encounter">> => [
             <<"status">>,
             <<"class">>
            ],
         <<"EpisodeOfCare">> => [
             <<"status">>,
             <<"patient">>
            ],
         <<"Goal">> => [
             <<"lifecycleStatus">>,
             <<"description">>,
             <<"subject">>
            ],
         <<"Group">> => [
             <<"type">>,
             <<"actual">> 
            ],
         <<"Observation">> => [
             <<"status">>,
             <<"code">>
             ],
         <<"Order">> => [
             <<"status">>,
             <<"subject">>
            ],
         <<"Patient">> => [
           ],
         <<"PlanDefinition">> => [
             <<"status">>
            ],
         <<"Practitioner">> => [
            ],
         <<"PractitionerRole">> => [
            ],
         <<"Procedure">> => [
             <<"status">>,
             <<"subject">>
            ],
         <<"Provenance">> => [
             <<"target">>,
             <<"recorded">>,
             <<"agent">>
            ],
         <<"Questionnaire">> => [
             <<"status">>
            ],
         <<"QuestionnaireResponse">> => [
             <<"status">>
            ],
         <<"RequestGroup">> => [
             <<"status">>,
             <<"intent">>
            ],
         <<"Task">> => [
             <<"status">>  
            ],
         <<"ValueSet">> => [
             <<"status">>
            ]
     }).

-define(patient_search,
        <<"SELECT id, text.div FROM `nabu` WHERE resourceType='Patient' and ANY n IN name SATISFIES n.family like 'V%' and n.`use`='official' END;">>
       ).

-endif.
