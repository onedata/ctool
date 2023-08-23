%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing lane schema used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lane_schema).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_lane_schema{}.
-export_type([record/0]).

% used as a sane default when no value is provided
% or when an older schema without this field is loaded from DB
-define(DEFAULT_INSTANT_FAILURE_EXCEPTION_THRESHOLD, 0.1).

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
        <<"id">> => Record#atm_lane_schema.id,
        <<"name">> => Record#atm_lane_schema.name,
        <<"parallelBoxes">> => [NestedRecordEncoder(M, atm_parallel_box_schema) || M <- Record#atm_lane_schema.parallel_boxes],
        <<"storeIteratorSpec">> => NestedRecordEncoder(Record#atm_lane_schema.store_iterator_spec, atm_store_iterator_spec),
        <<"maxRetries">> => Record#atm_lane_schema.max_retries,
        <<"instantFailureExceptionThreshold">> => Record#atm_lane_schema.instant_failure_exception_threshold,
        <<"dashboardSpec">> => case Record#atm_lane_schema.dashboard_spec of
            undefined -> null;
            DashboardSpec -> NestedRecordEncoder(DashboardSpec, ts_dashboard_spec)
        end
    }.


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson,  NestedRecordDecoder) ->
    #atm_lane_schema{
        id = maps:get(<<"id">>, RecordJson),
        name = maps:get(<<"name">>, RecordJson),
        parallel_boxes = [NestedRecordDecoder(M, atm_parallel_box_schema) || M <- maps:get(<<"parallelBoxes">>, RecordJson)],
        store_iterator_spec = NestedRecordDecoder(maps:get(<<"storeIteratorSpec">>, RecordJson), atm_store_iterator_spec),
        max_retries = maps:get(<<"maxRetries">>, RecordJson),
        instant_failure_exception_threshold = maps:get(
            <<"instantFailureExceptionThreshold">>, RecordJson, ?DEFAULT_INSTANT_FAILURE_EXCEPTION_THRESHOLD
        ),
        dashboard_spec = case maps:get(<<"dashboardSpec">>, RecordJson, null) of
            null -> undefined;
            DashboardSpecJson -> NestedRecordDecoder(DashboardSpecJson, ts_dashboard_spec)
        end
    }.
