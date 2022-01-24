%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing store schema used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_store_schema).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_store_schema{}.
-export_type([record/0]).


%% @TODO VFS-7687 Limit available data specs for certain store types
%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    encode_with(Record, fun jsonable_record:to_json/2).


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    decode_with(json, RecordJson, fun jsonable_record:from_json/2).

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, NestedRecordEncoder) ->
    encode_with(Record, NestedRecordEncoder).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, NestedRecordDecoder) ->
    decode_with(db, RecordJson, NestedRecordDecoder).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec encode_with(record(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_with(Schema, NestedRecordEncoder) ->
    Type = Schema#atm_store_schema.type,
    #{
        <<"id">> => Schema#atm_store_schema.id,
        <<"name">> => Schema#atm_store_schema.name,
        <<"description">> => Schema#atm_store_schema.description,
        <<"type">> => automation:store_type_to_json(Type),
        <<"config">> => atm_store_config:encode(Schema#atm_store_schema.config, Type, NestedRecordEncoder),
        <<"requiresInitialContent">> => Schema#atm_store_schema.requires_initial_content,
        <<"defaultInitialContent">> => utils:undefined_to_null(Schema#atm_store_schema.default_initial_content)
    }.


-spec decode_with(db | json, json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(db, SchemaJson, NestedRecordDecoder) ->
    Type = automation:store_type_from_json(maps:get(<<"type">>, SchemaJson)),
    #atm_store_schema{
        id = maps:get(<<"id">>, SchemaJson),
        name = maps:get(<<"name">>, SchemaJson),
        description = maps:get(<<"description">>, SchemaJson),
        type = Type,
        config = atm_store_config:decode(maps:get(<<"config">>, SchemaJson), Type, NestedRecordDecoder),
        requires_initial_content = maps:get(<<"requiresInitialContent">>, SchemaJson),
        default_initial_content = utils:null_to_undefined(maps:get(<<"defaultInitialContent">>, SchemaJson, null))
    };
decode_with(json, SchemaJson, NestedRecordDecoder) ->
    Schema = decode_with(db, SchemaJson, NestedRecordDecoder),
    % these two stores do not allow initial content to be specified - implicitly adjust corresponding fields
    case Schema#atm_store_schema.type == time_series orelse Schema#atm_store_schema.type == audit_log of
        true ->
            Schema#atm_store_schema{
                requires_initial_content = false,
                default_initial_content = undefined
            };
        false ->
            Schema
    end.
