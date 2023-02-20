%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing task schema used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_task_schema).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_task_schema{}.
-export_type([record/0]).

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    encode_with(Record, fun jsonable_record:to_json/2).


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    decode_with(RecordJson, fun jsonable_record:from_json/2).

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
    decode_with(RecordJson, NestedRecordDecoder).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec encode_with(record(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_with(Record, NestedRecordEncoder) ->
    #{
        <<"id">> => Record#atm_task_schema.id,
        <<"name">> => Record#atm_task_schema.name,
        <<"lambdaId">> => Record#atm_task_schema.lambda_id,
        <<"lambdaRevisionNumber">> => Record#atm_task_schema.lambda_revision_number,
        <<"lambdaConfig">> => Record#atm_task_schema.lambda_config,
        <<"argumentMappings">> => [NestedRecordEncoder(M, atm_task_schema_argument_mapper) || M <- Record#atm_task_schema.argument_mappings],
        <<"resultMappings">> => [NestedRecordEncoder(M, atm_task_schema_result_mapper) || M <- Record#atm_task_schema.result_mappings],
        <<"resourceSpecOverride">> => case Record#atm_task_schema.resource_spec_override of
            undefined -> null;
            ResourceSpec -> NestedRecordEncoder(ResourceSpec, atm_resource_spec)
        end,
        <<"timeSeriesStoreConfig">> => case Record#atm_task_schema.time_series_store_config of
            undefined -> null;
            TimeSeriesStoreConfig -> NestedRecordEncoder(TimeSeriesStoreConfig, atm_time_series_store_config)
        end
    }.


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedRecordDecoder) ->
    #atm_task_schema{
        id = maps:get(<<"id">>, RecordJson),
        name = maps:get(<<"name">>, RecordJson),
        lambda_id = maps:get(<<"lambdaId">>, RecordJson),
        lambda_revision_number = maps:get(<<"lambdaRevisionNumber">>, RecordJson),
        lambda_config = maps:get(<<"lambdaConfig">>, RecordJson, #{}),
        argument_mappings = [NestedRecordDecoder(M, atm_task_schema_argument_mapper) || M <- maps:get(<<"argumentMappings">>, RecordJson)],
        result_mappings = [NestedRecordDecoder(M, atm_task_schema_result_mapper) || M <- maps:get(<<"resultMappings">>, RecordJson)],
        resource_spec_override = case maps:get(<<"resourceSpecOverride">>, RecordJson, null) of
            null -> undefined;
            ResourceSpecJson -> NestedRecordDecoder(ResourceSpecJson, atm_resource_spec)
        end,
        time_series_store_config = case maps:get(<<"timeSeriesStoreConfig">>, RecordJson, null) of
            null -> undefined;
            TimeSeriesStoreConfigJson -> NestedRecordDecoder(TimeSeriesStoreConfigJson, atm_time_series_store_config)
        end
    }.
