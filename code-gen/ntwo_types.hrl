-ifndef(NTWO_TYPES).
-define(NTWO_TYPES, true).

-type kv() :: {binary(), binary()}.
-type kvlist() :: list({binary(), binary()}).
-type tkvlist() :: {kvlist()} | kvlist().
-type namespace() :: binary().
-type fhir_type() :: binary().
-type ns_type() :: {namespace(), fhir_type()} | fhir_type().
-type sf() :: {ns_type(), kvlist(), sf()} | binary(). 

-type http_code() :: 200 | 201 | 204 | 400 | 404 | 405 | 500.

-record(dao, {
       uri :: binary(),
       etag :: binary(),
       resource :: term(),
       state :: map()
    }).
-type dao() :: #dao{}.

-type dao_response() :: {error, binary()} | {ok, dao()}.

-define(Server, <<"http://localhost">>).
-define(Base, <<"/nabu">>).
-define(Supported_Resources, [<<"Patient">>]).

-endif.
