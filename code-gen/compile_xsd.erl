-module(compile_xsd).
%-compile(export_all).
%%%
%%% FHIR 4.0.0
%%% Datatypes
%%%
%%% transform parsed XSD DomainResources into modules with types and functions for decoding and encoding
%%% from and to proplists
%%%
%%% TODOs in fhir_400.hrl
%%% - resource:ResourceContainer
%%% - extensions:Extension
%%% - extensions:ModifierExtension
-include("../include/fhir.hrl").
-include("../include/fhir_400.hrl").

-export([compile/1]).

-define(RESOURCES, [<<"AdverseEvent">>,
                    <<"Basic">>,
                    <<"Binary">>,
                    <<"CapabilityStatement">>,
                    <<"ClinicalImpression">>,
                    <<"CodeSystem">>,
                    <<"FamilyMemberHistory">>,
                    <<"HealthcareService">>,
                    <<"Parameters">>,
                    <<"RiskAssessment">>,
                    <<"SearchParameter">>
                   ]).

-spec compile(Dir :: binary()) -> (ok | {error, binary()}).
compile(Dir) ->
    AllTypes = maps:keys(?fhir_xsd),
    Types = ?RESOURCES,
    lists:foreach(fun (T) -> compile_type(T,AllTypes,Dir) end, Types),
    ok.

compile_type(T,Types,Dir) ->
    case maps:find(T, ?fhir_xsd) of
      {ok, DT} ->
           BBEs = backbones(T,Types),
		   module({T,DT},BBEs,Dir);
	  error   -> ok
    end.

backbones(M, Types) ->
    lists:filtermap(fun (T) -> backbone(T,M) end, Types).

backbone(T,DR) ->
    % io:format("backbone: ~p: ~p~n",[T, DR]),
    case binary:split(T,[DR,<<".">>]) of
        [<<>>, <<>>] -> false;
        [<<>>,_]     -> BDT = maps:get(T, ?fhir_xsd),
                        {true, {T, BDT}};
        _            -> false
    end.

    
module({Type, _DT}=DR, Backbones, Dir) ->
    % io:format("module: ~p~n~p~n",[DR, Backbones]),
    {Mod,Fun} = type_to_fun(Type),
    File = list_to_binary([Dir,<<"/">>,string:lowercase(Type),<<".erl">>]),
    io:format("write module: ~p~n",[File]),
    {ok, S} = file:open(File, [write]),
    io:format(S,"-module(~s).~n",[Mod]),
    io:format(S,"%%%~n",[]),
    io:format(S,"%%% FHIR 4.0.0 ~s~n",[Type]),
    io:format(S,"%%%~n",[]),
    io:format(S,"-include(\"fhir.hrl\").~n",[]),
    io:format(S,"-include(\"primitives.hrl\").~n",[]),
    io:format(S,"-include(\"codes.hrl\").~n",[]),
    write_types([DR] ++ Backbones, S),
    io:format(S,"%%~n",[]),
    io:format(S,"%% API exports~n",[]),
    io:format(S,"%% -exports([]).~n",[]),
    io:format(S,"%%~n~n",[]),
    io:format(S,"%%=====================================================================~n",[]),
    io:format(S,"%% API exports~n",[]),
    io:format(S,"%%=====================================================================~n",[]),
    write_fields([DR] ++ Backbones, S),
    io:format(S,"%%~n",[]),
    write_funs([DR] ++ Backbones, S),
    io:format(S,"%%~n",[]),
    io:format(S,"%%=====================================================================~n",[]),
    io:format(S,"%% Eunit tests~n",[]),
    io:format(S,"%%=====================================================================~n",[]),
    io:format(S,"-ifdef(TEST).~n~n",[]),
    io:format(S,"-include_lib(\"eunit/include/eunit.hrl\").~n",[]),
    io:format(S,"-define(asrtto(A, B), ?assertEqual(B, ~s:~s(A))).~n",[Mod, Fun]),
    io:format(S,"-define(asrtp(A, B), ?assertEqual(B, encode:to_proplist(A))).~n",[]),
    io:format(S,"-define(asrtjson(A, B), ?assertEqual(B, jiffy:encode(encode:to_proplist(A)))).~n~n~n",[]),
    io:format(S,"-endif.~n",[]),
    file:close(S).

write_types(List, S) -> 
	lists:foreach(fun (T) -> write_type(T,S) end, List).

write_type({Type,{Base,Info,_Attrs,_Restrictions}}, S) ->
    io:format(S,"~n",[]),
    RecType = Type,
    io:format(S,"-record('~s', { anyAttribs :: anyAttribs(),~n",[RecType]),
    RProps = resolve_base(Base,Info),       % prefix props from base type
    write_props(RProps,0,S),
    io:format(S,"    }).~n",[]),
    io:format(S,"-type '~s'() :: #'~s'{}.~n~n",[RecType,RecType]).

write_props([],_,_) -> ok;
write_props([{Name,Info}|T],I,S) -> 
    % io:format("~p~n",[Info]),
    Type = type(Info),
    Prop = escape_reserved(Name),
    case I of
        0 -> io:format(S,"      ~s :: ~s~n",[Prop,Type]);
        _ -> io:format(S,"    , ~s :: ~s~n",[Prop,Type])
    end,
    write_props(T,I+1,S).

escape_reserved(<<"identifier">>) -> <<"'identifier'">>;
escape_reserved(<<"reference">>) -> <<"'reference'">>;
escape_reserved(<<"when">>) -> <<"'when'">>;
escape_reserved(<<"start">>) -> <<"'start'">>;
escape_reserved(<<"end">>) -> <<"'end'">>;
escape_reserved(Key) -> Key.

type({{Class, Name}, Rep}) ->
    Type = case Class of
        primitive -> [Name,<<"()">>];                             % fhir names are compatible
        complex   -> [<<"complex:'">>,    Name, <<"'()">>];
        metadata  -> [<<"metadata:'">>,   Name,<<"'()">>];
        special   -> [<<"special:'">>,    Name,<<"'()">>];
        extension -> [<<"extensions:'">>, Name,<<"'()">>];
        resource  -> [<<"resource:'">>,   Name,<<"'()">>];
        code      -> [<<"code()">>];   % TODO split off _list, code name is in map now
        bbelement -> case Name of
                         <<"Dosage">> -> [<<"metadata:'">>, Name, <<"'()">>];
                         <<"Timing">> -> [<<"complex:'">>, Name, <<"'()">>];
                         _            -> [<<"'">>, Name, <<"'()">>]
                     end
        end,
    case Rep of
        optional -> list_to_binary([Type, <<" | undefined">>]);
        required -> list_to_binary(Type);
        list   -> list_to_binary([<<"[">>,Type,<<"]">>]);
        non_empty_list -> list_to_binary([<<"[">>,Type,<<"]">>])
    end.

-spec resolve_base(Base :: binary()) -> list().
resolve_base(Base) ->
    resolve_base(Base,[]).
resolve_base(undefined, L) -> L;
resolve_base(<<"">>, L) -> L;
resolve_base(<<"BackboneElement">>, L) -> L;
resolve_base(Base, L) ->
    % io:format("rb: ~p~n", [Base]),
    {NewBase, BI, Attrs, _Restrictions} = maps:get(Base,?fhir_xsd),
    resolve_base(NewBase, Attrs++BI++L).

write_fields(List, S) -> 
	lists:foreach(fun (T) -> write_field(T,S) end, List).
write_field({Type,{Base,Info,_Attrs,_Restrictions}}, S) ->
    io:format("fields('~s') ->           record_info(fields, '~s');~n",[Type,Type]).

write_funs(List, S) -> 
	lists:foreach(fun (T) -> write_fun(T,S) end, List).

write_fun({Type,{Base,Info,_Attrs,_Restrictions}}, S) ->
    {_, Fun} = type_to_fun(Type),
    io:format(S,"~s({Props}) -> ~s(Props);~n",[Fun,Fun]),
    io:format(S,"~s(Props) -> ~n",[Fun]),
    io:format(S,"    DT = decode:xsd_info(~p),~n",[Type]),
    io:format(S,"    #'~s'{~n",[Type]),
    RProps = resolve_base(Base,Info),       % prefix props from base type
    write_values(RProps,0,S),
    io:format(S,"    }.~n~n",[]).

write_values([],_,_) -> ok;
write_values([{Name,_}|T],I,S) -> 
    case I of
        0 -> io:format(S,"      anyAttribs = decode:attrs(Props, DT)~n",[]),
             io:format(S,"    , ~s = decode:value(~p, Props, DT)~n",[Name,Name]);
        _ -> io:format(S,"    , ~s = decode:value(~p, Props, DT)~n",[Name,Name])
    end,
    write_values(T,I+1,S).

type_to_fun(<<"Group">>) ->
    {'Group', 'to_group'};
type_to_fun(S) ->
    Parts = binary:split(S,<<".">>),
    Mod = string:lowercase(hd(Parts)),
    FPs = binary_join([decap(P) || P <- Parts],<<"_">>),
    {binary_to_atom(Mod,latin1),binary_to_atom(<<"to_", FPs/binary>>,latin1)}.

decap(B) -> <<H:1/binary, T/binary>> =B,
            NH = string:lowercase(H),
            <<NH/binary,T/binary>>.

-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
   <<>>;
binary_join([Part], _Sep) ->
   Part;
binary_join(List, Sep) ->
   lists:foldr(fun (A, B) ->
     if
       bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
       true -> A
     end
   end, <<>>, List).

capitalize(S) ->
    F = fun([H|T]) -> [string:to_upper(H) | string:to_lower(T)] end,
    string:join(lists:map(F, string:tokens(S, " ")), " ").
