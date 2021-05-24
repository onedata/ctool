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

-spec encode_with(record(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_with(Record, NestedRecordEncoder) ->
    #{
        <<"id">> => Record#atm_lane_schema.id,
        <<"name">> => Record#atm_lane_schema.name,
        <<"parallelBoxes">> => [NestedRecordEncoder(M, atm_parallel_box_schema) || M <- Record#atm_lane_schema.parallel_boxes],
        <<"storeIteratorRecord">> => NestedRecordEncoder(Record#atm_lane_schema.store_iterator_spec, atm_store_iterator_spec)
    }.


-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedRecordDecoder) ->
    #atm_lane_schema{
        id = maps:get(<<"id">>, RecordJson),
        name = maps:get(<<"name">>, RecordJson),
        parallel_boxes = [NestedRecordDecoder(M, atm_parallel_box_schema) || M <- maps:get(<<"parallelBoxes">>, RecordJson)],
        store_iterator_spec = NestedRecordDecoder(maps:get(<<"storeIteratorRecord">>, RecordJson), atm_store_iterator_spec)
    }.
