-module(xsd2hs).
%-compile(export_all).
%%%
%%% FHIR 4.0.0
%%% Datatypes
%%%
%%% transform parsed XSD DomainResources into modules with types and functions for decoding and encoding
%%% from and to proplists
%%%
%%% it's a ugly hack, but it does the job
%%% important work is done in compile-xsd-erlang.xql, which generates the header files (codes, fhir_400)
%%%
%%% TODOs 
%%% - additional attrs, xml:id for Resource, xmlns and id (in bbelement only) are handled
%%% - codes n fhir_400.hrl should be in camel case
%%%
%%% TODOs in fhir_400.hrl
%%% - extensions:Extension
%%% - extensions:ModifierExtension
-include("../include/codes.hrl").
-include("../include/fhir.hrl").
-include("../include/fhir_400.hrl").

-export([compile/1]).

-define(RESOURCES,[
                    <<"ActivityDefinition">>
                  , <<"AdverseEvent">>
                  , <<"AllergyIntolerance">>
                  , <<"Binary">>
%%                  , <<"Bundle">>
                  , <<"CapabilityStatement">>
                  , <<"CarePlan">>
                  , <<"CareTeam">>
                  , <<"Claim">>
                  , <<"ClinicalImpression">>
                  , <<"CodeSystem">>
                  , <<"Communication">>
                  , <<"Composition">>
                  , <<"Condition">>
                  , <<"Consent">>
                  , <<"Coverage">>
                  , <<"Device">>
                  , <<"DiagnosticReport">>
                  , <<"Encounter">>
                  , <<"EpisodeOfCare">>
                  , <<"ExplanationOfBenefit">>
                  , <<"FamilyHistory">>
                  , <<"Goal">>
                  , <<"Group">>
                  , <<"HealthcareService">>
                  , <<"ICalendar">>
                  , <<"ImagingStudy">>
                  , <<"Immunization">>
                  , <<"Leave">>
                  , <<"Library">>
                  , <<"Location">>
                  , <<"MedicationRequest">>
                  , <<"Observation">>
                  , <<"OperationOutcome">>
                  , <<"Organization">>
%%                  , <<"Parameters">> 
                  , <<"Patient">>
                  , <<"PlanDefinition">>
                  , <<"Practitioner">>
                  , <<"PractitionerRole">>
                  , <<"Procedure">>
                  , <<"Provenance">>
                  , <<"Questionnaire">>
                  , <<"QuestionnaireResponse">>
                  , <<"RequestGroup">>
                  , <<"ResourceContainer">>
                  , <<"RiskAssessment">>
                  , <<"SearchParameter">>
                  , <<"ServiceRequest">>
                  , <<"Task">>
                  ]).
-define(INTERNALCODES,#{
                    <<"administrativegender">> => <<"AdministrativeGender">>
                  , <<"addressuse">> => <<"AddressUse">>
                  , <<"contactpointsystem">> => <<"ContactPointSystem">>
                  , <<"contactpointuse">> => <<"ContactPointUse">>
                  , <<"fhirversion">> => <<"FHIRVersion">>
                  , <<"identifieruse">> => <<"IdentifierUse">>
                  , <<"language">> => <<"Language">>
                  , <<"linktype">> => <<"LinkType">>
                  , <<"mimetype">> => <<"MimeType">>
                  , <<"nameuse">> => <<"NameUse">>
                  , <<"narrativestatus">> => <<"NarrativeStatus">>
                  , <<"productshelflife">> => <<"ProductShelfLife">>
                  , <<"prodcharacteristic">> => <<"ProdCharacteristic">>
                  , <<"publicationstatus">> => <<"PublicationStatus">>
                  , <<"quantitycomparator">> => <<"QuantityComparator">>
                  }).
-define(PROPSNOTWANTED,#{
                    <<"contained">> => <<"commented-out">>
                  }).

-spec compile(Version::binary(), Dir :: binary()) -> (ok | {error, binary()}).
compile(Version,Dir) ->
    AllTypes = maps:keys(?fhir_xsd),
    Types = ?RESOURCES,
    lists:foreach(fun (T) -> compile_type(Version,T,AllTypes,Dir) end, Types),
    ok.

compile_type(Version,T,Types,Dir) ->
    case maps:find(T, ?fhir_xsd) of
      {ok, DT} ->
           BBEs = backbones(T,Types),
           module(Version,{T,DT},BBEs,Dir);
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


module(Version,{Type, _DT}=DR, Backbones, Dir) ->
    % io:format("module: ~p~n~p~n",[DR, Backbones]),
    File = list_to_binary([Dir,<<"/">>,Type,<<".hs">>]),
    io:format("write module: ~p~n",[File]),
    {ok, S} = file:open(File, [write]),
    io:format(S,"{-# LANGUAGE NoImplicitPrelude  #-}~n~n",[]),

    io:format(S,"{-# LANGUAGE DataKinds         #-}~n",[]),
    io:format(S,"{-# LANGUAGE DerivingStrategies #-}~n",[]),
    io:format(S,"{-# LANGUAGE DeriveGeneric     #-}~n",[]),
    io:format(S,"{-# LANGUAGE FlexibleInstances #-}~n",[]),
    io:format(S,"{-# LANGUAGE GADTs #-}~n",[]),
    io:format(S,"{-# LANGUAGE InstanceSigs #-}~n",[]),
    io:format(S,"{-# LANGUAGE LambdaCase #-}~n",[]),
    io:format(S,"{-# LANGUAGE OverloadedStrings #-}~n",[]),
    io:format(S,"{-# LANGUAGE PolyKinds #-}~n",[]),
    io:format(S,"{-# LANGUAGE RecordWildCards #-}~n",[]),
    io:format(S,"{-# LANGUAGE TypeOperators     #-}~n~n",[]),
    io:format(S,"--~n",[]),
    io:format(S,"-- FHIR ~s ~s~n",[Version,Type]),
    io:format(S,"--~n~n",[]),
    io:format(S,"module Data.FHIR.Resources.~s where~n~n",[Type]),

    io:format(S,"import Data.Aeson~n",[]),
    io:format(S,"import Data.Aeson.Types hiding (parseJSON)~n~n",[]),

    io:format(S,"import qualified Data.HashMap.Strict as HM~n",[]),
    io:format(S,"import GHC.TypeLits~n~n",[]),

    io:format(S,"import RIO                  hiding(fromString)~n",[]),
    io:format(S,"import qualified RIO.Vector as V~n",[]),
    io:format(S,"import           Data.FHIR.Datatypes~n",[]),
    io:format(S,"import           Data.FHIR.Datatypes.XML~n",[]),
    io:format(S,"import           Data.FHIR.Datatypes.XmlUtils~n",[]),
    io:format(S,"import           Data.FHIR.Resources.DomainResource~n",[]),
    io:format(S,"import qualified Xmlbf  as Xmlbf~n~n",[]),

    write_types([DR] ++ Backbones, S),
    io:format(S,"~n",[]),
    file:close(S).

write_types(List, S) ->
    lists:foreach(fun (T) -> write_type(T,S) end, List).

write_type({Type,{Base,Info,Attrs,Restrictions}}=R, S) ->
    DataType = list_to_binary(conc(Type)),
    FieldPrefix = decap(DataType),
    write_code_sum_types(Info,Type,S),
    write_poly_sum_types(Restrictions,Type,S),
    write_data(DataType,Base,Attrs,Info,FieldPrefix,S),
    write_toJson(R,FieldPrefix,S),
    write_fromJson(R,FieldPrefix,S),
    write_toXml(R,FieldPrefix,S),
    write_fromXml(R,FieldPrefix,S),
    io:format(S,"~n~n",[]).

write_data(DataType,Base,Attrs,Info,FieldPrefix,S) ->
    {RAttrs,RProps} = resolve_base(Base,Attrs,Info),
    io:format(S,"data ~s = ~s {~n",[DataType,DataType]),
%%    io:format(S,"  anyAttribs :: anyAttribs(),~n",[]),
    I = write_props(attrs,RAttrs,DataType,FieldPrefix,0,S),
    _ = write_props(props,RProps,DataType,FieldPrefix,I,S),
    io:format(S,"  } deriving stock (Eq, Show)~n",[]),
    io:format(S,"--~n~n",[]).

write_code_sum_types([],_,_) -> ok;
%% some codes are defined in Internal.hs
write_code_sum_types([{Name,{{code,CT},_}}|T],Type,S) ->
    K = list_to_binary([string:lowercase(CT)]),
    case maps:get(K,?INTERNALCODES,no) of
      %%no -> DT = list_to_binary([lists:nth(1,string:split(Type,".")),cap(Name)]),
      no -> DT = list_to_binary([conc(Type),cap(Name)]),
            CST  = maps:get(K, ?fhir_codes),
            CDT  = capitals(DT),
            write_code_sum_type(CST,DT,CDT,0,S),
            write_code_json(CST,DT,CDT,S),
            write_code_xml(CST,DT,CDT,S),
            write_code_sum_types(T,Type,S);
      _  -> write_code_sum_types(T,Type,S)
    end;
write_code_sum_types([_|T],Type,S) -> 
    write_code_sum_types(T,Type,S).

write_code_sum_type(CST,DT,TP,I,S) ->
    io:format(S,"data ~s~n",[DT]),
    write_code_st_items(CST,TP,I,S),
    io:format(S,"  deriving stock (Eq, Show)~n~n",[]).

write_code_st_items([],_,_,_) -> ok;
write_code_st_items([H|T],TP,I,S) ->
    case I of
      0 -> io:format(S,"    = ~s~s~n",[TP,camel_code(H)]);
      _ -> io:format(S,"    | ~s~s~n",[TP,camel_code(H)]) 
    end,
    write_code_st_items(T,TP,I+1,S).

write_code_json(CST,DT,TP,S) ->
    io:format(S,"instance ToJSON ~s where~n",[DT]),
    write_code_toJSON(CST,TP,0,S),
    io:format(S,"instance FromJSON ~s where~n",[DT]),
    write_code_parseJSON(CST,TP,0,S),
    io:format(S,"~n",[]).

write_code_toJSON([],_,I,_) -> I;
write_code_toJSON([H|T],TP,I,S) ->
    io:format(S,"    toJSON ~s~s = String \"~s\"~n",[TP,camel_code(H),H]),
    write_code_toJSON(T,TP,I+1,S).

write_code_parseJSON([],_,I,_) -> I;
write_code_parseJSON([H|T],TP,I,S) ->
    io:format(S,"    parseJSON \"~s\" = return ~s~s~n",[H,TP,camel_code(H)]),
    write_code_parseJSON(T,TP,I+1,S).

write_code_xml(CST,DT,TP,S) ->
    write_code_toXml(CST,DT,TP,0,S),
    write_code_fromXml(CST,DT,TP,0,S),
    io:format(S,"~n~n",[]).

write_code_toXml([],_,_,I,_) -> I;
write_code_toXml([H|T],DT,TP,I,S) ->
    io:format(S,"to~s ~s~s = \"~s\"~n",[DT,TP,camel_code(H),H]),
    write_code_toXml(T,DT,TP,I+1,S).

write_code_fromXml([],_,_,I,_) -> I;
write_code_fromXml([H|T],DT,TP,I,S) ->
    io:format(S,"from~s \"~s\" = ~s~s~n",[DT,H,TP,camel_code(H)]),
    write_code_fromXml(T,DT,TP,I+1,S).


write_poly_sum_types([],_,_) -> ok;
write_poly_sum_types([H|T],Type,S) -> 
    HL = erlang:tuple_to_list(H),
    PL = binary:longest_common_prefix(HL),
    Field = cap(binary:part(lists:nth(1,HL),0,PL)),
    io:format(S,"data ~s~s~n",[conc(Type),Field]),
    write_sum_type(HL,Type,PL,0,S),
    io:format(S,"    deriving stock (Eq, Show)~n~n",[]),
    write_poly_sum_types(T,Type,S). 

write_sum_type([],_,_,_,_) -> ok;
write_sum_type([H|T],Type,PL,I,S) ->
    SubType = case cap(binary:part(H,PL,erlang:byte_size(H)-PL)) of
                <<"String">> -> <<"Text">>;
                ST           -> ST
              end,
    case I of
      0 -> io:format(S,"    = ~s~s ~s~n",[conc(Type),cap(H),SubType]);
      _ -> io:format(S,"    | ~s~s ~s~n",[conc(Type),cap(H),SubType]) 
    end,
    write_sum_type(T,Type,PL,I+1,S).


write_props(_,[],_,_,I,_) -> I;
write_props(attrs,[{Name,Info}|T],Type,FP,I,S) ->
    % io:format("~p~n",[Info]),
    Prop = cap(Name),
    PT   = type(Type,Prop,Info,FP),
    case I of
        0 -> io:format(S,"    ~sAttr~s :: ~s~n",[FP,Prop,PT]);
        _ -> io:format(S,"  , ~sAttr~s :: ~s~n",[FP,Prop,PT])
    end,
    write_props(attrs,T,Type,FP,I+1,S);
write_props(props,[{Name,Info}|T],Type,FP,I,S) ->
    Prop = cap(Name),
    PT = type(Type,Prop,Info,FP),
    case maps:get(Name,?PROPSNOTWANTED,no) of
        no -> case I of
                0 -> io:format(S,"    ~s~s :: ~s~n",[FP,Prop,PT]);
                _ -> io:format(S,"  , ~s~s :: ~s~n",[FP,Prop,PT]) 
              end;
        _  -> io:format(S,"--    ~s~s :: ~s~n",[FP,Prop,PT])
    end,
    write_props(props,T,Type,FP,I+1,S).

type(Type, Prop,{{Class, CT}, Rep},_) ->
    T = case Class of
        primitive -> case CT of 
                       <<"string">> -> <<"Text">>;
                       _            -> cap(CT)
                     end;
        complex   -> CT;
        metadata  -> CT;
        special   -> CT;
        extension -> CT;
        resource  -> CT;
        code      -> case maps:get(CT,?INTERNALCODES,no) of
                      no -> list_to_binary([Type,Prop]);
                      C  -> C
                     end;
        bbelement -> case CT of
                         <<"Dosage">> -> CT;
                         <<"Timing">> -> CT;
                         _            -> conc(CT)
                     end
        end,
    case Rep of
        optional -> list_to_binary([<<"Maybe ">>, T]);
        required -> list_to_binary([T]);
        list   -> list_to_binary([<<"[">>,T,<<"]">>]);
        non_empty_list -> list_to_binary([<<"[">>,T,<<"]">>])
    end;
%% choice type
%%type([{<<"deceasedBoolean">>, {{primitive,<<"boolean">>},optional}},
%%      {<<"deceasedDateTime">>,{{primitive,<<"dateTime">>},optional}}])
type(Type,Prop,CT,FP) when is_list(CT) -> 
   {_,{_,R}} = lists:nth(1,CT),
   case R of
     optional -> list_to_binary([<<"Maybe ">>,cap(FP),Prop]);
     required -> list_to_binary([cap(FP),Prop]);
     list     -> list_to_binary([<<"[">>,cap(FP),Prop,<<"]">>]);
     non_empty_list -> list_to_binary([<<"[">>,cap(FP),Prop,<<"]">>]) 
   end.
%%type([{ExpName, {{Class, Type}, Rep}}|Tail], Acc) ->
%%    Acc1 = [cap(ExpName),<<"Choice">>] ++ Acc,
%%    type(Tail,Acc1).

-spec resolve_base(Base :: binary()) -> list().
resolve_base(Base) ->
    resolve_base(Base,[],[]).
resolve_base(undefined, A,L) -> {A,L};
resolve_base(<<"">>, A, L) -> {A,L};
%%resolve_base(<<"BackboneElement">>, L) -> L;
%%resolve_base(<<"DomainResource">>, L) -> L;
%%resolve_base(<<"Resource">>, L) -> L;
resolve_base(Base, A, L) ->
    % io:format("rb: ~p~n", [Base]),
    {NewBase, BI, Attrs, _Restrictions} = maps:get(Base,?fhir_xsd),
%%    NAttrs = prefixAttrs(Base,Attrs),
    resolve_base(NewBase, Attrs++A, BI++L).

prefixAttrs(Base, Attrs) -> [{list_to_binary([decap(Base),cap(N)]),Info} || {N,Info} <- Attrs].

write_toJson({Type,{Base,Info,Attrs,_Restrictions}},FP,S) ->
    {RAttrs,RProps} = resolve_base(Base,Attrs,Info),
    io:format(S,"instance ToJSON ~s where~n",[conc(Type)]),
    io:format(S,"  toJSON p = object $~n",[]),
    io:format(S,"    filter (\\(_,v) -> (v /= Null) && (v/=(Array V.empty))) $~n",[]),
    io:format(S,"    [~n",[]),
    I = write_resourceType(Base,Type,0,S),
    I1 = write_json_encoders(attrs,RAttrs,FP,I,S),
    _ = write_json_encoders(props,RProps,FP,I1,S),
    io:format(S,"    ]~n",[]),
    write_choice(tojson,RProps,Type,FP,S).

write_resourceType(<<"Resource">>,Type,I,S) -> 
    case I of
      0 -> io:format(S,"     (\"resourceType\" , String \"~s\")~n",[Type]),
           1;
      _ -> io:format(S,"     (\"resourceType\" , String \"~s\")~n",[Type]),
           I+1
    end;
write_resourceType(<<"DomainResource">>,Type,I,S) -> 
    case I of
      0 -> io:format(S,"     (\"resourceType\" , String \"~s\")~n",[Type]),
           1;
      _ -> io:format(S,"     (\"resourceType\" , String \"~s\")~n",[Type]),
           I+1
    end;
write_resourceType(_, _ ,I,_) -> I.

write_fromJson({Type,{Base,Info,Attrs,_Restrictions}},FP,S) ->
    {RAttrs,RProps} = resolve_base(Base,Attrs,Info),
    io:format(S,"instance FromJSON ~s where~n",[conc(Type)]),
    io:format(S,"  parseJSON = withObject \"~s\" $ \\o -> do~n",[conc(Type)]),
    write_json_rt(Base,Type,0,S),
    I = write_json_decoders(attrs,RAttrs,0,S),
    _ = write_json_decoders(props,RProps,I,S),
    io:format(S,"        return ~s{~n",[conc(Type)]),
    I1 = write_fields(json,attrs,RAttrs,FP,0,S),
    _  = write_fields(json,props,RProps,FP,I1,S),
    io:format(S,"          }~n",[]),
    write_json_rt_catchall(Base,Type,0,S),
    write_choice(fromjson,RProps,Type,FP,S).

write_json_rt(<<"Resource">>,Type,I,S) -> write_json_rt(<<"DomainResource">>,Type,I,S);
write_json_rt(<<"DomainResource">>,Type,I,S) ->
    io:format(S,"    rt     <- o .:  \"resourceType\"  :: Parser Text~n",[]),
    io:format(S,"    case rt of~n",[]),
    io:format(S,"      \"~s\" -> do~n",[Type]),
    I;
write_json_rt(_, _ ,I,_) -> I.

write_json_rt_catchall(<<"Resource">>,T,_,S) -> io:format(S,"      _ -> fail \"not a ~s\"~n",[T]);
write_json_rt_catchall(<<"DomainResource">>,T,_,S) -> io:format(S,"      _ -> fail \"not a ~s\"~n",[T]);
write_json_rt_catchall(_,_,_,_) -> ok.

write_choice(toxml,Props,Type,FP,S) -> 
    case lists:filtermap(fun (P) -> choice(toxml,P,Type,FP) end, Props) of
     [] -> ok;
     [H|T] -> case isDR(Type) of
                true  -> ok;
                false -> io:format(S,"       where ~n",[])
              end,
              write_where_clause(toxml,H,S),   
              [write_where_clause(toxml,C,S) || C <-T ]
    end;
write_choice(Dir,Props,Type,FP,S) -> 
    case lists:filtermap(fun (P) -> choice(Dir,P,Type,FP) end, Props) of
     [] -> ok;
     [H|T] -> io:format(S,"    where ~n",[]),
              write_where_clause(Dir,H,S),   
              [write_where_clause(Dir,C,S) || C <-T ]
    end.   

write_where_clause(toxml,Cs,S) -> [io:format(S,"          ~s~n",[C]) || C <-Cs];
write_where_clause(_    ,Cs,S) -> [io:format(S,"      ~s~n",[C]) || C <-Cs].

write_json_encoders(_,[],_,I,_) -> I;
write_json_encoders(attrs,[{N,CT}|T],FP,I,S) ->
    case I of
      0 -> io:format(S,"      \"~s\" .= toJSON (~sAttr~s p)~n",[N,FP,cap(N)]);
      _ -> io:format(S,"    , \"~s\" .= toJSON (~sAttr~s p)~n",[N,FP,cap(N)])
    end,
    write_json_encoders(attrs,T,FP,I+1,S);
write_json_encoders(props,[{N,CT}|T],FP,I,S) ->
    case maps:get(N,?PROPSNOTWANTED,no) of
      no -> case I of
              0 -> io:format(S,"      ~s~n",[encoder(json,N,CT,FP)]);
              _ -> io:format(S,"    , ~s~n",[encoder(json,N,CT,FP)])
            end;
      _  -> io:format(S,"--    , \"~s\" .= toJSON (~s~s p)~n",[N,FP,cap(N)])
    end,
    write_json_encoders(props,T,FP,I+1,S).

write_json_decoders(_,[],I,_) -> I;
write_json_decoders(attrs,[{Name,Info}|T],I,S) ->
    case I of
      0 -> io:format(S,"        ~s <- ~s~n",[hsescape(Name),decoder(json,Name,Info)]);
      _ -> io:format(S,"        ~s <- ~s~n",[hsescape(Name),decoder(json,Name,Info)])
    end,
    write_json_decoders(attrs,T,I+1,S);
write_json_decoders(props,[{Name,Info}|T],I,S) ->
    case maps:get(Name,?PROPSNOTWANTED,no) of
      no -> case I of
              0 -> io:format(S,"        ~s <- ~s~n",[hsescape(Name),decoder(json,Name,Info)]);
              _ -> io:format(S,"        ~s <- ~s~n",[hsescape(Name),decoder(json,Name,Info)])
            end;
      _  -> io:format(S,"--        ~s <- ~s~n",[hsescape(Name),decoder(json,Name,Info)])
    end,
    write_json_decoders(props,T,I+1,S).

write_toXml({Type,{Base,Info,Attrs,_Restrictions}},FP,S) ->
    {RAttrs,RProps} = resolve_base(Base,Attrs,Info),
    io:format(S,"instance Xmlbf.ToXml ~s where~n",[conc(Type)]),
    write_toxml_rt(Base,Type,0,S),
    io:format(S,"             [~n",[]),
    I = write_xml_encoders(attrs,RAttrs,FP,0,S),
    _ = write_xml_encoders(props,RProps,FP,I,S),
    io:format(S,"             ]~n",[]),
    write_choice(toxml,RProps,Type,FP,S).

write_fromXml({Type,{Base,Info,Attrs,_Restrictions}},FP,S) ->
    {RAttrs,RProps} = resolve_base(Base,Attrs,Info),
    io:format(S,"instance Xmlbf.FromXml ~s where~n",[conc(Type)]),
%%    write_fromxml_rt(Base,Type,0,S),  -- already eaten by DomainResourceC
    io:format(S,"  fromXml = do~n",[]),
    I = write_xml_decoders(attrs,RAttrs,FP,0,S),
    _ = write_xml_decoders(props,RProps,FP,I,S),
    io:format(S,"    return ~s {~n",[conc(Type)]),
    I1 = write_fields(xml,attrs,RAttrs,FP,0,S),
    _  = write_fields(xml,props,RProps,FP,I1,S),
    io:format(S,"          }~n~n",[]),
    write_choice(fromxml,RProps,Type,FP,S).

write_toxml_rt(<<"Resource">>,Type,I,S) -> write_toxml_rt(<<"DomainResource">>,Type,I,S);
write_toxml_rt(<<"DomainResource">>,Type,_,S) ->
    io:format(S,"  toXml p = Xmlbf.element \"~s\" as cs~n",[Type]),
    io:format(S,"    where as = HM.fromList $ catMaybes $~n",[]),
    io:format(S,"                 fmap toAttr [~n",[]),
    io:format(S,"                     Val \"xmlns\" \"http://hl7.org/fhir\"~n",[]),
    io:format(S,"                  -- OptVal \"xml:id\" (domainResourceAttribs ps)~n",[]),
    io:format(S,"                   ]~n",[]),

    io:format(S,"          cs = concatMap toElement $~n",[]);
write_toxml_rt(_,_,_,S) ->
    io:format(S,"  toXml p = concatMap toElement $~n",[]).

choice(tojson,{N,CT},Type,FP) when is_list(CT) ->
   {_,{_,R}} = lists:nth(1,CT),
   Cs = [case R of
          optional -> list_to_binary([<<"to">>,cap(N),<<"JSON (Just (">>,conc(cap(Type)),cap(N),cap(T),<<" c)) = (\"">>,N,cap(T),<<"\", toJSON c)">>]);
          required -> list_to_binary([<<"to">>,cap(N),<<"JSON (     (">>,conc(cap(Type)),cap(N),cap(T),<<" c)) = (\"">>,N,cap(T),<<"\", toJSON c)">>]);
          list     -> list_to_binary([<<"to">>,cap(N),<<"JSON (     (">>,conc(cap(Type)),cap(N),cap(T),<<" c)) = (\"">>,N,cap(T),<<"\", toJSON c)">>]);
          non_empty_list -> list_to_binary([<<"to">>,cap(N),<<"JSON (     (">>,conc(cap(Type)),cap(N),cap(T),<<" c)) = (\"">>,N,cap(T),<<"\", toJSON c)">>])
         end
        || {_ ,{{_,T},_}} <- CT],  
   Cs1 = case R of
           optional -> [list_to_binary([<<"to">>,cap(N),<<"JSON (     Nothing   ) = (\"">>,N,<<"\", Null)">>]) | Cs];
           required -> Cs;
           list     -> Cs;
           non_empty_list -> Cs
         end,
   {true,Cs1}; 
choice(fromjson,{N,CT},Type,_ ) when is_list(CT) ->
   {_,{_,R}} = lists:nth(1,CT),
   Fs = [mkFnT(N,T) || {_ ,{{_,T},_}} <- CT],
   Cs0 = list_to_binary([<<"parse">>,cap(N),<<" o = ">>,binary_join(Fs,<<" o <|> ">>),<<" o">>]),
   Cs1 = [case R of
          optional -> list_to_binary([mkFnT(N,T),<<" o = do\n">>,
                                      <<"                has <- o .: \"">>,CN,<<"\"\n">>,
                                      <<"                return $ Just (">>,conc(Type),cap(CN),<<" has)">>]);
          required -> list_to_binary([mkFnT(N,T),<<" o = do\n">>,
                                      <<"                has <- o .: \"">>,CN,<<"\"\n">>,
                                      <<"                return $ ">>,conc(Type),cap(CN),<<" has">>]);
          list     -> list_to_binary([mkFnT(N,T),<<" o = do\n">>,
                                      <<"                has <- o .:? \"">>,CN,<<"\" .!= []\n">>,
                                      <<"                return $ ">>,conc(Type),cap(CN),<<" has">>]);
          non_empty_list -> list_to_binary([mkFnT(N,T),<<" o = do\n">>,
                                      <<"                has <- o .:  \"">>,CN,<<"\"\n">>,
                                      <<"                return $ ">>,conc(Type),cap(CN),<<" has">>])
         end
        || {CN,{{_,T},_}} <- CT],  
   Cs2 = case R of
           optional -> [];
           required -> [];
           list     -> [];
           non_empty_list -> []
         end,
   {true, [Cs0]++Cs1++Cs2};
choice(toxml,{N,CT},Type,FP) when is_list(CT) ->
   {_,{_,R}} = lists:nth(1,CT),
   Cs = [case R of
          optional -> list_to_binary([<<"to">>,cap(N),      <<"Xml (Just (">>,conc(cap(Type)),cap(N),cap(T),<<" p)) = ">>, encoder(xmlchoice,N,{{C,T},R},FP)]);
          required -> list_to_binary([<<"to">>,cap(N),      <<"Xml (     (">>,conc(cap(Type)),cap(N),cap(T),<<" p)) = ">>, encoder(xmlchoice,N,{{C,T},R},FP)]);
          list     -> list_to_binary([<<"to">>,cap(N),      <<"Xml (     (">>,conc(cap(Type)),cap(N),cap(T),<<" p)) = ">>, encoder(xmlchoice,N,{{C,T},R},FP)]);
          non_empty_list -> list_to_binary([<<"to">>,cap(N),<<"Xml (     (">>,conc(cap(Type)),cap(N),cap(T),<<" p)) = ">>, encoder(xmlchoice,N,{{C,T},R},FP)])
         end
        || {CN ,{{C,T},_}} <- CT],  
   Cs1 = case R of
           optional -> [list_to_binary([<<"to">>,cap(N),<<"Xml ( Nothing   ) = (OptVal \"">>,N,<<"\" Nothing)">>]) | Cs];
           required -> Cs;
           list     -> Cs;
           non_empty_list -> Cs
         end,
   {true,Cs1}; 
choice(fromxml,{N,CT},Type,_ ) when is_list(CT) ->
   {_,{{_,_},R}} = lists:nth(1,CT),
   Fs = [mkFnT(N,T) || {_ ,{{_,T},_}} <- CT],
   Cs0 = case R of 
           optional -> list_to_binary([<<"from">>,cap(N),<<"Xml = ">>,binary_join(Fs,<<" <|> ">>), <<" <|> pure Nothing">>]);
           required -> list_to_binary([<<"from">>,cap(N),<<"Xml = ">>,binary_join(Fs,<<" <|> ">>)]);
           list     -> list_to_binary([<<"from">>,cap(N),<<"Xml = ">>,binary_join(Fs,<<" <|> ">>)]);
           non_empty_list -> list_to_binary([<<"from">>,cap(N),<<"Xml = ">>,binary_join(Fs,<<" <|> ">>)])
         end,
   Cs1 = [case R of
          optional -> list_to_binary([mkFnT(N,T),<<" = do\n">>,
                                      <<"                has <- Xmlbf.pElement \"">>,CN,<<"\" ">>,getXmlProp(C),<<"\n">>,
                                      <<"                return $ Just (">>,conc(Type),cap(CN),convertProp(C,R,T),<<")">>]);
          required -> list_to_binary([mkFnT(N,T),<<" = do\n">>,
                                      <<"                has <- Xmlbf.pElement \"">>,CN,<<"\" ">>,getXmlProp(C),<<"\n">>,
                                      <<"                return $ ">>,conc(Type),cap(CN),convertProp(C,R,T)]);
          list     -> list_to_binary([mkFnT(N,T),<<" = do\n">>,
                                      <<"                has <- many $ Xmlbf.pElement \"">>,CN,<<"\" ">>,getXmlProp(C),<<"\n">>,
                                      <<"                return $ ">>,conc(Type),cap(CN),convertProp(C,R,T)]);
          non_empty_list -> error(<<"nyi">>)
         end
        || {CN,{{C,T},_}} <- CT],  
   Cs2 = case R of
           optional -> [];
           required -> [];
           list     -> [];
           non_empty_list -> []
         end,
   {true, [Cs0]++Cs1++Cs2};
choice(_,{_,C},_,_) when is_tuple(C) -> false.

mkFnT(N,T) -> list_to_binary([<<"parse">>,cap(N),cap(T)]).

getXmlProp(primitive) -> <<"(Xmlbf.pAttr \"value\")">>;
getXmlProp(code)      -> <<"(Xmlbf.pAttr \"value\")">>;
getXmlProp(C)         -> <<"Xmlbf.fromXml">>.

convertProp(primitive,optional,<<"integer">>) -> [<<" (     fromInt has)">>];
convertProp(primitive,optional,T) -> [<<" (     from">>,cap(T),<<" has)">>];
convertProp(primitive,required,<<"integer">>) -> [<<" (     fromInt has)">>];
convertProp(primitive,required,T) -> [<<" (     from">>,cap(T),<<" has)">>];
convertProp(code,optional,T)      -> [<<" (     from">>,cap(T),<<" has)">>];
convertProp(code,required,T)      -> [<<" (     from">>,cap(T),<<" has)">>];
convertProp(C,_,_)                -> [<<" (                      has)">>].

encoder(xmlchoice,N,{{primitive,<<"integer">>},optional      },FP) -> list_to_binary([<<"Val   \"">>,N,<<"Integer\" (toInt p)">>]);
encoder(xmlchoice,N,{{primitive,T},optional      },FP) -> list_to_binary([<<"Val   \"">>,N,cap(T),<<"\" (to">>,cap(T),<<" p)">>]);
encoder(xmlchoice,N,{{code,     CT},optional      },FP) -> 
    case maps:get(CT,?INTERNALCODES,no) of
      no -> list_to_binary([<<"Val   \"">>,N,cap(CT),<<"\" (to">>,cap(FP),cap(N),<<" p">>]);
      CN -> list_to_binary([<<"Val   \"">>,N,cap(CN),<<"\" (to">>,        cap(CN),<<" p">>])
    end;
encoder(xmlchoice,N,{{_,        T},optional      },FP) -> list_to_binary([<<"Prop  \"">>,N,cap(T),<<"\" (HM.empty, Xmlbf.toXml p)">>]);
encoder(xmlchoice,N,{{primitive,<<"integer">>},required      },FP) -> list_to_binary([<<"Val      \"">>,N,<<"Integer\" (     toInt p)">>]);
encoder(xmlchoice,N,{{primitive,T},required      },FP) -> list_to_binary([<<"Val      \"">>,N,cap(T),<<"\" (     to">>,cap(T),<<" p)">>]);
encoder(xmlchoice,N,{{code     ,CT},required      },FP) -> 
    case maps:get(CT,?INTERNALCODES,no) of
      no -> list_to_binary([<<"Val      \"">>,N,cap(CT),<<"\" (     to">>,cap(FP),cap(N),<<" p)">>]);
      CN -> list_to_binary([<<"Val      \"">>,N,cap(CN),<<"\" (     to">>,        cap(CN),<<" p)">>])
    end;
encoder(xmlchoice,N,{{_        ,T},required      },FP) -> list_to_binary([<<"Prop     \"">>,N,cap(T),<<"\" (HM.empty, Xmlbf.toXml p)">>]);
encoder(xmlchoice,N,{{primitive,<<"integer">>},list          },FP) -> list_to_binary([<<"ValList  \"">>,N,<<"Integer\" (fmap toInt p)">>]);
encoder(xmlchoice,N,{{primitive,T},list          },FP) -> list_to_binary([<<"ValList  \"">>,N,cap(T),<<"\" (fmap to">>,cap(T),<<" p)">>]);
encoder(xmlchoice,N,{{code     ,CT},list          },FP) -> 
    case maps:get(CT,?INTERNALCODES,no) of
      no -> list_to_binary([<<"ValList  \"">>,N,cap(CT),<<"\" (fmap to">>,cap(FP),cap(N),<<" p)">>]);
      CN -> list_to_binary([<<"ValList  \"">>,N,cap(CN),<<"\" (fmap to">>,        cap(CN),<<" p)">>])
    end;
encoder(xmlchoice,N,{{_        ,T},list          },FP) -> list_to_binary([<<"PropList \"">>,N,cap(T),<<"\" (fmap Xmlbf.toXml  p)">>]);
encoder(xmlchoice,N,{{primitive,<<"integer">>},non_empty_list},FP) -> list_to_binary([<<"ValList  \"">>,N,<<"Integer\" (fmap toInt p)">>]);
encoder(xmlchoice,N,{{primitive,T},non_empty_list},FP) -> list_to_binary([<<"ValList  \"">>,N,cap(T),<<"\" (fmap to">>,cap(T),<<" p)">>]);
encoder(xmlchoice,N,{{code     ,CT},non_empty_list},FP) -> 
    case maps:get(CT,?INTERNALCODES,no) of
      no -> list_to_binary([<<"ValList  \"">>,N,cap(CT),<<"\" (fmap to">>,cap(FP),cap(N),<<" p)">>]);
      CN -> list_to_binary([<<"ValList  \"">>,N,cap(CN),<<"\" (fmap to">>,        cap(CN),<<" p)">>])
    end;
encoder(xmlchoice,N,{{_        ,T},non_empty_list},FP) -> list_to_binary([<<"PropList \"">>,N,cap(T),<<"\" (fmap Xmlbf.toXml p)">>]);


encoder(json,N,{{_,_},optional      },FP) -> list_to_binary([<<" \"">>,N,<<"\" .= toJSON (">>,FP,cap(N),<<" p)">>]);
encoder(json,N,{{_,_},required      },FP) -> list_to_binary([<<" \"">>,N,<<"\" .= toJSON (">>,FP,cap(N),<<" p)">>]);
encoder(json,N,{{_,_},list          },FP) -> list_to_binary([<<" \"">>,N,<<"\" .= toJSON (">>,FP,cap(N),<<" p)">>]);
encoder(json,N,{{_,_},non_empty_list},FP) -> list_to_binary([<<" \"">>,N,<<"\" .= toJSON (">>,FP,cap(N),<<" p)">>]);
encoder(json,N,CT,FP) when is_list(CT) -> list_to_binary([<<"to">>,cap(N),<<"JSON (">>,FP,cap(N),<<" p)">>]);
encoder(xml,N,{{primitive,<<"integer">>},optional      },FP) -> list_to_binary([<<"OptVal   \"">>,N,<<"\" (fmap toInt (">>,FP,cap(N),<<" p))">>]);
encoder(xml,N,{{primitive,T},optional      },FP) -> list_to_binary([<<"OptVal   \"">>,N,<<"\" (fmap to">>,cap(T),<<" (">>,FP,cap(N),<<" p))">>]);
encoder(xml,N,{{code,     CT},optional      },FP) -> 
    case maps:get(CT,?INTERNALCODES,no) of
      no -> list_to_binary([<<"OptVal   \"">>,N,<<"\" (fmap to">>,cap(FP),cap(N),<<" (">>,FP,cap(N),<<" p))">>]);
      CN -> list_to_binary([<<"OptVal   \"">>,N,<<"\" (fmap to">>,        cap(CN),<<" (">>,FP,cap(N),<<" p))">>])
    end;
encoder(xml,N,{{_,        _},optional      },FP) -> list_to_binary([<<"OptProp  \"">>,N,<<"\" (fmap Xmlbf.toXml (">>,FP,cap(N),<<" p))">>]);
encoder(xml,N,{{primitive,<<"integer">>},required      },FP) -> list_to_binary([<<"Val      \"">>,N,<<"\" (     toInt (">>,FP,cap(N),<<" p))">>]);
encoder(xml,N,{{primitive,T},required      },FP) -> list_to_binary([<<"Val      \"">>,N,<<"\" (     to">>,cap(T),<<" (">>,FP,cap(N),<<" p))">>]);
encoder(xml,N,{{code     ,CT},required      },FP) -> 
    case maps:get(CT,?INTERNALCODES,no) of
      no -> list_to_binary([<<"Val      \"">>,N,<<"\" (     to">>,cap(FP),cap(N),<<" (">>,FP,cap(N),<<" p))">>]);
      CN -> list_to_binary([<<"Val      \"">>,N,<<"\" (     to">>,        cap(CN),<<" (">>,FP,cap(N),<<" p))">>])
    end;
encoder(xml,N,{{_        ,_},required      },FP) -> list_to_binary([<<"Prop     \"">>,N,<<"\" (HM.empty, Xmlbf.toXml (">> ,FP,cap(N), <<" p))">>]);
encoder(xml,N,{{primitive,<<"integer">>},list          },FP) -> list_to_binary([<<"ValList  \"">>,N,<<"\" (fmap toInt (">>,FP,cap(N),<<" p))">>]);
encoder(xml,N,{{primitive,T},list          },FP) -> list_to_binary([<<"ValList  \"">>,N,<<"\" (fmap to">>,cap(T),<<" (">>,FP,cap(N),<<" p))">>]);
encoder(xml,N,{{code     ,CT},list          },FP) -> 
    case maps:get(CT,?INTERNALCODES,no) of
      no -> list_to_binary([<<"ValList  \"">>,N,<<"\" (fmap to">>,cap(FP),cap(N),<<" (">>,FP,cap(N),<<" p))">>]);
      CN -> list_to_binary([<<"ValList  \"">>,N,<<"\" (fmap to">>,        cap(CN),<<" (">>,FP,cap(N),<<" p))">>])
    end;
encoder(xml,N,{{_        ,_},list          },FP) -> list_to_binary([<<"PropList \"">>,N,<<"\" (fmap Xmlbf.toXml (">>,FP,cap(N),<<" p))">>]);
encoder(xml,N,{{primitive,<<"integer">>},non_empty_list},FP) -> list_to_binary([<<"ValList  \"">>,N,<<"\" (fmap toInt (">>,FP,cap(N),<<" p))">>]);
encoder(xml,N,{{primitive,T},non_empty_list},FP) -> list_to_binary([<<"ValList  \"">>,N,<<"\" (fmap to">>,cap(T),<<" (">>,FP,cap(N),<<" p))">>]);
encoder(xml,N,{{code     ,CT},non_empty_list},FP) -> 
    case maps:get(CT,?INTERNALCODES,no) of
      no -> list_to_binary([<<"ValList  \"">>,N,<<"\" (fmap to">>,cap(FP),cap(N),<<" (">>,FP,cap(N),<<" p))">>]);
      CN -> list_to_binary([<<"ValList  \"">>,N,<<"\" (fmap to">>,        cap(CN),<<" (">>,FP,cap(N),<<" p))">>])
    end;
encoder(xml,N,{{_        ,_},non_empty_list},FP) -> list_to_binary([<<"PropList \"">>,N,<<"\" (fmap Xmlbf.toXml (">>,FP,cap(N),<<" p))">>]);
encoder(xml,N,CT,FP) when is_list(CT) -> list_to_binary([<<"to">>,cap(N),<<"Xml (">>,FP,cap(N),<<" p)">>]).

decoder(json,N,{{_,_},optional      }) -> list_to_binary([<<"o .:? \"">>,N,<<"\"">>]);
decoder(json,N,{{_,_},required      }) -> list_to_binary([<<"o .:  \"">>,N,<<"\"">>]);
decoder(json,N,{{_,_},list          }) -> list_to_binary([<<"o .:? \"">>,N,<<"\" .!= []">>]);
decoder(json,N,{{_,_},non_empty_list}) -> list_to_binary([<<"o .:? \"">>,N,<<"\" .!= []">>]); %% should not happen
decoder(json,N,CT) when is_list(CT) -> list_to_binary([<<"parse">>,cap(N),<<" o">>]);
decoder(xml,N,{{primitive,_},optional      }) -> list_to_binary([<<"optional $ Xmlbf.pElement \"">>,N,<<"\" (Xmlbf.pAttr \"value\")">>]);
decoder(xml,N,{{code     ,_},optional      }) -> list_to_binary([<<"optional $ Xmlbf.pElement \"">>,N,<<"\" (Xmlbf.pAttr \"value\")">>]);
decoder(xml,N,{{_        ,_},optional      }) -> list_to_binary([<<"optional $ Xmlbf.pElement \"">>,N,<<"\" Xmlbf.fromXml">>]);
decoder(xml,N,{{primitive,_},required      }) -> list_to_binary([<<"           Xmlbf.pElement \"">>,N,<<"\" (Xmlbf.pAttr \"value\")">>]);
decoder(xml,N,{{code     ,_},required      }) -> list_to_binary([<<"           Xmlbf.pElement \"">>,N,<<"\" (Xmlbf.pAttr \"value\")">>]);
decoder(xml,N,{{_        ,_},required      }) -> list_to_binary([<<"           Xmlbf.pElement \"">>,N,<<"\" Xmlbf.fromXml">>]);
decoder(xml,N,{{primitive,_},list          }) -> list_to_binary([<<"many     $ Xmlbf.pElement \"">>,N,<<"\" (Xmlbf.pAttr \"value\")">>]);
decoder(xml,N,{{code     ,_},list          }) -> list_to_binary([<<"many     $ Xmlbf.pElement \"">>,N,<<"\" (Xmlbf.pAttr \"value\")">>]);
decoder(xml,N,{{_        ,_},list          }) -> list_to_binary([<<"many     $ Xmlbf.pElement \"">>,N,<<"\" Xmlbf.fromXml">>]);
decoder(xml,N,{{C,T},non_empty_list}) -> decoder(xml,N,{{C,T},list});
decoder(xml,N,CT) when is_list(CT) -> list_to_binary([<<"from">>,cap(N),<<"Xml">>]).

write_xml_encoders(_,[],_,I,_) -> I;
write_xml_encoders(attrs,[{Name,_}|T],FP,I,S) ->
    case I of
      0 -> io:format(S,"               OptVal \"~s\"   (~sAttr~s p)~n",[Name,FP,cap(Name)]);
      _ -> io:format(S,"             , OptVal \"~s\"   (~sAttr~s p)~n",[Name,FP,cap(Name)])
    end,
    write_xml_encoders(attrs,T,FP,I+1,S);
write_xml_encoders(props,[{Name,Info}|T],FP,I,S) ->
    case maps:get(Name,?PROPSNOTWANTED,no) of
      no -> case I of
              0 -> io:format(S,"               ~s~n",[encoder(xml,Name,Info,FP)]);
              _ -> io:format(S,"             , ~s~n",[encoder(xml,Name,Info,FP)]) 
            end;
      _ -> io:format(S,"--             , ~s~n",[encoder(xml,Name,Info,FP)]) 
    end,
    write_xml_encoders(props,T,FP,I+1,S).

write_xml_decoders(_,[],_,I,_) -> I;
write_xml_decoders(attrs,[{Name,Info}|T],FP,I,S) ->
    case I of
      0 -> io:format(S,"    ~s <- optional $ Xmlbf.pAttr \"~s\"~n", [hsescape(Name), Name]);
      _ -> io:format(S,"    ~s <- optional $ Xmlbf.pAttr \"~s\"~n", [hsescape(Name), Name]) 
    end,
    write_xml_decoders(attrs,T,FP,I+1,S);
write_xml_decoders(props,[{Name,Info}|T],FP,I,S) ->
    case maps:get(Name,?PROPSNOTWANTED,no) of
      no -> case I of
              0 -> io:format(S,"    ~s <- ~s~n", [hsescape(Name),decoder(xml,Name,Info)]);
              _ -> io:format(S,"    ~s <- ~s~n", [hsescape(Name),decoder(xml,Name,Info)]) 
            end;
      _ -> io:format(S,"--    ~s <- ~s~n", [hsescape(Name),decoder(xml,Name,Info)]) 
    end,
    write_xml_decoders(props,T,FP,I+1,S).

write_fields(_,_,[],_,I,_) -> I;
write_fields(M,attrs,[{Name,_}|T],FP,I,S) ->
    case I of
        0 -> % io:format(S,"          anyAttribs = decode:attrs(Props, DT)~n",[]),
             io:format(S,"            ~s~s~s = ~s~n",[FP,<<"Attr">>,cap(Name),hsescape(Name)]);
        _ -> io:format(S,"          , ~s~s~s = ~s~n",[FP,<<"Attr">>,cap(Name),hsescape(Name)])
    end,
    write_fields(M,attrs,T,FP,I+1,S);
write_fields(xml,props,[{Name,{{primitive,<<"integer">>},R}}|T],FP,I,S) ->
    Map = hasMap(R),
    case maps:get(Name,?PROPSNOTWANTED,no) of
      no -> case I of
              0 -> % io:format(S,"          anyAttribs = decode:attrs(Props, DT)~n",[]),
                   io:format(S,"            ~s~s = ~s fromInt ~s~n",[FP,cap(Name),Map,hsescape(Name)]);
              _ -> io:format(S,"          , ~s~s = ~s fromInt ~s~n",[FP,cap(Name),Map,hsescape(Name)])
            end;
      _ -> io:format(S,"--          , ~s~s = ~s fromInt ~s~n",[FP,cap(Name),Map,hsescape(Name)])
    end,
    write_fields(xml,props,T,FP,I+1,S);
write_fields(xml,props,[{Name,{{primitive,CT},R}}|T],FP,I,S) ->
    Map = hasMap(R),
    case maps:get(Name,?PROPSNOTWANTED,no) of
      no -> case I of
              0 -> % io:format(S,"          anyAttribs = decode:attrs(Props, DT)~n",[]),
                   io:format(S,"            ~s~s = ~s from~s ~s~n",[FP,cap(Name),Map,cap(CT),hsescape(Name)]);
              _ -> io:format(S,"          , ~s~s = ~s from~s ~s~n",[FP,cap(Name),Map,cap(CT),hsescape(Name)])
            end;
      _ -> io:format(S,"--          , ~s~s = ~s from~s ~s~n",[FP,cap(Name),Map,cap(CT),hsescape(Name)])
    end,
    write_fields(xml,props,T,FP,I+1,S);
write_fields(xml,props,[{Name,{{code,CT},R}}|T],FP,I,S) ->
    Map = hasMap(R),
    case maps:get(Name,?PROPSNOTWANTED,no) of
      no -> case maps:get(CT,?INTERNALCODES,no) of
              no -> case I of
                      0 -> % io:format(S,"          anyAttribs = decode:attrs(Props, DT)~n",[]),
                           io:format(S,"            ~s~s = ~s from~s~s ~s~n",[FP,cap(Name),Map,cap(FP),cap(Name),hsescape(Name)]);
                      _ -> io:format(S,"          , ~s~s = ~s from~s~s ~s~n",[FP,cap(Name),Map,cap(FP),cap(Name),hsescape(Name)])
                    end;
              CN -> case I of
                      0 -> % io:format(S,"          anyAttribs = decode:attrs(Props, DT)~n",[]),
                           io:format(S,"            ~s~s = ~s from~s ~s~n",[FP,cap(Name),Map,cap(CN),hsescape(Name)]);
                      _ -> io:format(S,"          , ~s~s = ~s from~s ~s~n",[FP,cap(Name),Map,cap(CN),hsescape(Name)])
                    end
            end;
      _  -> io:format(S,"          , ~s~s = ~s from~s ~s~n",[FP,cap(Name),Map,cap(Name),hsescape(Name)])
    end,
    write_fields(xml,props,T,FP,I+1,S);
write_fields(M,props,[{Name,_}|T],FP,I,S) ->
    case maps:get(Name,?PROPSNOTWANTED,no) of
      no -> case I of
              0 -> % io:format(S,"          anyAttribs = decode:attrs(Props, DT)~n",[]),
                   io:format(S,"            ~s~s = ~s~n",[FP,cap(Name),hsescape(Name)]);
              _ -> io:format(S,"          , ~s~s = ~s~n",[FP,cap(Name),hsescape(Name)])
            end;
      _ -> io:format(S,"--          , ~s~s = ~s~n",[FP,cap(Name),hsescape(Name)])
    end,
    write_fields(M,props,T,FP,I+1,S).

hasMap(optional) -> "fmap";
hasMap(required) -> "    ";
hasMap(list)     -> "fmap";
hasMap(non_empty_list) -> "fmap".

hsescape(<<"class">>) -> <<"cl">>;
hsescape(<<"data">>) -> <<"dt">>;
hsescape(<<"instance">>) -> <<"ins">>;
hsescape(<<"type">>) -> <<"ty">>;
hsescape(S) -> S.

type_to_fun(S) ->
    Parts = binary:split(S,<<".">>),
    Mod = string:lowercase(hd(Parts)),
    FPs = binary_join([decap(P) || P <- Parts],<<"_">>),
    {binary_to_atom(Mod,latin1),binary_to_atom(<<"to_", FPs/binary>>,latin1)}.

decap(B) -> <<H:1/binary, T/binary>> =B,
            NH = string:lowercase(H),
            <<NH/binary,T/binary>>.

cap(B) -> <<H:1/binary, T/binary>> =B,
            NH = string:uppercase(H),
            <<NH/binary,T/binary>>.

conc(S) -> string:replace(S,".","",all).
isDR(T) -> case string:find(T,".") of
             nomatch -> true;
             _       -> false
           end.
camel_code(<<"=">>) -> <<"Eq">>;
camel_code(<<"!=">>) -> <<"NotEq">>;
camel_code(<<"<">>) -> <<"Le">>;
camel_code(<<">">>) -> <<"Ge">>;
camel_code(<<"<=">>) -> <<"LT">>;
camel_code(<<">=">>) -> <<"GT">>;
camel_code(S) -> binary_join([cap(S) || S <- string:split(S,"-",all)],<<"">>).

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

%%
%% longest common prefix for char lists
%%
lcp([]) -> [];
lcp([S]) -> S;
lcp(L) -> lcp_(L).

lcp_([[H|T]|R]) ->
  case strip(H, R, []) of
    false -> [];
    Ts -> [H|lcp_([T|Ts])]
  end.

strip(_, [], Ts) -> Ts;
strip(H, [[H|T]|R], Ts) -> strip(H, R, [T|Ts]);
strip(_, _, _) -> false.

capitals(S) -> list_to_binary(re:split(S,"[a-z]",[trim])).
