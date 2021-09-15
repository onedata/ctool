%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Base model for data specs used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_resource_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_resource_spec{}.
-export_type([record/0]).

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_map().
to_json(Record) ->
    #{
        <<"cpuRequested">> => utils:undefined_to_null(Record#atm_resource_spec.cpu_requested),
        <<"cpuLimit">> => utils:undefined_to_null(Record#atm_resource_spec.cpu_limit),
        <<"memoryRequested">> => utils:undefined_to_null(Record#atm_resource_spec.memory_requested),
        <<"memoryLimit">> => utils:undefined_to_null(Record#atm_resource_spec.memory_limit),
        <<"ephemeralStorageRequested">> => utils:undefined_to_null(Record#atm_resource_spec.ephemeral_storage_requested),
        <<"ephemeralStorageLimit">> => utils:undefined_to_null(Record#atm_resource_spec.ephemeral_storage_limit)
    }.


-spec from_json(json_utils:json_map()) -> record().
from_json(RecordJson) ->
    #atm_resource_spec{
        cpu_requested = utils:null_to_undefined(maps:get(<<"cpuRequested">>, RecordJson)),
        cpu_limit = utils:null_to_undefined(maps:get(<<"cpuLimit">>, RecordJson)),
        memory_requested = utils:null_to_undefined(maps:get(<<"memoryRequested">>, RecordJson)),
        memory_limit = utils:null_to_undefined(maps:get(<<"memoryLimit">>, RecordJson)),
        ephemeral_storage_requested = utils:null_to_undefined(maps:get(<<"ephemeralStorageRequested">>, RecordJson)),
        ephemeral_storage_limit = utils:null_to_undefined(maps:get(<<"ephemeralStorageLimit">>, RecordJson))
    }.

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, _NestedRecordEncoder) ->
    to_json(Record).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, _NestedRecordDecoder) ->
    from_json(RecordJson).

