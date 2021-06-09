%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing store iterator spec used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_store_iterator_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_store_iterator_spec{}.
-type strategy() :: atm_store_iterator_serial_strategy:record() | atm_store_iterator_batch_strategy:record().
-type strategy_record_type() :: atm_store_iterator_serial_strategy | atm_store_iterator_batch_strategy.
-type strategy_type() :: serial | batch.

-export_type([record/0, strategy/0]).

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
    StrategyRecord = Record#atm_store_iterator_spec.strategy,
    StrategyRecordType = utils:record_type(StrategyRecord),
    StrategyType = record_type_to_strategy_type(StrategyRecordType),
    #{
        <<"strategy">> => maps:merge(
            #{<<"type">> => strategy_type_to_json(StrategyType)},
            NestedRecordEncoder(StrategyRecord, StrategyRecordType)
        ),
        <<"storeSchemaId">> => Record#atm_store_iterator_spec.store_schema_id
    }.


-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedRecordDecoder) ->
    StrategyJson = maps:get(<<"strategy">>, RecordJson),
    StrategyType = strategy_type_from_json(maps:get(<<"type">>, StrategyJson)),
    StrategyRecordType = strategy_type_to_record_type(StrategyType),
    #atm_store_iterator_spec{
        store_schema_id = maps:get(<<"storeSchemaId">>, RecordJson),
        strategy = NestedRecordDecoder(StrategyJson, StrategyRecordType)
    }.


-spec record_type_to_strategy_type(strategy_record_type()) -> strategy_type().
record_type_to_strategy_type(atm_store_iterator_serial_strategy) -> serial;
record_type_to_strategy_type(atm_store_iterator_batch_strategy) -> batch.


-spec strategy_type_to_record_type(strategy_type()) -> strategy_record_type().
strategy_type_to_record_type(serial) -> atm_store_iterator_serial_strategy;
strategy_type_to_record_type(batch) -> atm_store_iterator_batch_strategy.


-spec strategy_type_to_json(strategy_type()) -> json_utils:json_term().
strategy_type_to_json(serial) -> <<"serial">>;
strategy_type_to_json(batch) -> <<"batch">>.


-spec strategy_type_from_json(json_utils:json_term()) -> strategy_type().
strategy_type_from_json(<<"serial">>) -> serial;
strategy_type_from_json(<<"batch">>) -> batch.
