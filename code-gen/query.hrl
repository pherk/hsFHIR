-ifndef(QUERY).
-define(QUERY, true).

%%  
%%  Modifications
%%  for simple props 'token' type is changed to 'code' to facilitate facet construction
%%  
%%  # History
%%   In addition to the standard _format parameter, the parameters to this interaction may also include:
%%  _count : integer	single	Number of return records requested. The server is not bound to return the number requested, but cannot return more
%%  _since : instant	single	Only include resource versions that were created at or after the given instant in time
%%  _at : dateTime	single	Only include resource versions that were current at some point during the time period specified in the date time value (may be more than one)
%% 
%%  ## All types
%%  
%%  _id
%%  _lastUpdated
%%  _tag
%%  _profile
%%  _security
%%  _text
%%  _content
%%  _list
%%  _has
%%  _type
%%  
%%  # Special
%%  
%%  _filter
%%  
%%  # Value prefixes for ordered params e.g. number, date, quantity
%%  
%%  eq, ne, le, ge, lt, gt
%%  sa - starts after provided value
%%  eb - ends before
%%  ap - approximately by 10%
%%  
%%  # Parameter modifier
%%  
%%  string 
%%  default search -> starts-with(content, value) but case-insensitive, accent normalization
%%  :contains -> contains()
%%  :exact -> as the name says
%%  
%%  token
%%  [parameter]=[system]|[code]: the value of [code] matches a Coding.code or Identifier.value, and the value of [system] matches the system property of the Identifier or Coding
%%  
%%  Modifier 	Use
%%  :text 	The search parameter is processed as a string that searches text associated with the code/value - either CodeableConcept.text, Coding.display, or Identifier.type.text.
%%  :not 	Reverse the code matching described in the paragraph above. Note that this includes resources that have no value for the parameter - e.g. ?gender:not=male includes all patients that do not have gender = male, including patients that do not have a gender at all
%%  :above 	The search parameter is a concept with the form [system]|[code], and the search parameter tests whether the coding in a resource subsumes the specified search code. For example, the search concept has an is-a relationship with the coding in the resource, and this includes the coding itself.
%%  :below 	the search parameter is a concept with the form [system]|[code], and the search parameter tests whether the coding in a resource is subsumed by the specified search code. For example, the coding in the resource has an is-a relationship with the search concept, and this includes the coding itself.
%%  :in 	The search parameter is a URI (relative or absolute) that identifies a value set, and the search parameter tests whether the coding is in the specified value set. The reference may be literal (to an address where the value set can be found) or logical (a reference to ValueSet.url). If the server can treat the reference as a literal URL, it does, else it tries to match known logical ValueSet.url values.
%%  :not-in 	The search parameter is a URI (relative or absolute) that identifies a value set, and the search parameter tests whether the coding is not in the specified value set.
%% 
%%  # Text search
%%  
%%  AND OR
%%  _text
%%  _content
%%  
%%

-define(reserved_search, 
    [
          <<"_content">>
        , <<"_id">>
        , <<"_lastUpdated">>
        , <<"_profile">>
        , <<"_query">>
        , <<"_security">>
        , <<"_source">>
        , <<"_tag">>
    ]).
-define(reserved_special, 
          <<"_text">>
        , <<"_filter">>
    ]).
-define(reserved_result, 
          <<"_count">>
        , <<"_sort">>
        , <<"_include">>
        , <<"_revinclude">>
        , <<"_summary">>
        , <<"_contained">>
        , <<"_containedtype">>
        , <<"_elements">>
        , <<"_source">>
        , <<"_total">>           % none, estimate, accurate 
        , <<"page">>
    ]).
-define(modifierasc,  <<"asc">>).
-define(modifierdesc,  <<"desc">>).
-define(chainseparator   ,  <<".">>).
-define(modifierseparator,  <<":">>).
-define(containedtrue   ,  <<"true">>).
-define(containedfalse  ,  <<"false">>).
-define(containedboth   ,  <<"both">>).
-define(containedtypecontainer,  <<"container">>).
-define(containedtypecontained,  <<"contained">>).

-define(modifiers, 
    [
      <<"missing">>
    , <<"contains">>
    , <<"exact">>
    , <<"text">>
    , <<"not">>, <<"below">>, <<"above">>, <<"in">>, <<"not-in">>, <<"of-type">>   
    , <<"identifier">>
    ]).

-define(prefixes, 
    [
      <<"eq">>, <<"ne">>, <<"le">>, <<"ge">>, <<"lt">>, <<"gt">>
    , <<"sa">>
    , <<"eb">>
    , <<"ap">>
    , <<"[A-Z][A-Za-z]+">> % FHIR type for _include, _revinclude 
    ]).
-define(compartmentnames, 
    [
      <<"Patient">>
    , <<"Encounter">>
    , <<"ReleatedPerson">>
    , <<"Practitioner">>
    , <<"Device">>
    ]).
-define(historyparams, 
    [
      <<"_count">>  % integer The maximum number of search results on a page, excluding related resources included by _include or _revinclude or OperationOutcomes. The server is not bound to return the number requested, but cannot return more 
    , <<"_since">>  % instant	Only include resource versions that were created at or after the given instant in time 
    , <<"_at">>     % date(Time)	Only include resource versions that were current at some point during the time period specified in the date time value (see Search notes on date searching) 
    , <<"_list">>   % reference Only include resource versions that are referenced in the specified list (current list references are allowed)   
    ]).

-endif.
