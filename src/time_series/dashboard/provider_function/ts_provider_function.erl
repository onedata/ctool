%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing generic time series provider function object.
%%% The specializations of the provider functions are grouped into
%%% two classes: data generators and transformers, but they share the
%%% same basic record structure.
%%% @end
%%%-------------------------------------------------------------------
-module(ts_provider_function).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("time_series/dashboard.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


% acts as discriminator for polymorphism, @see function_name_to_record_type/1 for possible values
-type function_name() :: binary().

%% @formatter:off
-type record() :: ts_data_generator_current_value:record()
                | ts_data_generator_get_dynamic_series_config:record()
                | ts_data_generator_get_dynamic_series_group_config:record()
                | ts_data_generator_literal:record()
                | ts_data_generator_load_series:record()

                | ts_transformer_abs:record()
                | ts_transformer_multiply:record()
                | ts_transformer_replace_empty:record().

-type record_type() :: ts_data_generator_current_value
                     | ts_data_generator_get_dynamic_series_config
                     | ts_data_generator_get_dynamic_series_group_config
                     | ts_data_generator_literal
                     | ts_data_generator_load_series

                     | ts_transformer_abs
                     | ts_transformer_multiply
                     | ts_transformer_replace_empty.
%% @formatter:on

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
    RecordType = utils:record_type(Record),
    FunctionName = record_type_to_function_name(RecordType),
    #{
        <<"functionName">> => FunctionName,
        <<"functionArguments">> => NestedRecordEncoder(Record, RecordType)
    }.


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedRecordDecoder) ->
    FunctionName = maps:get(<<"functionName">>, RecordJson),
    FunctionArgs = maps:get(<<"functionArguments">>, RecordJson, #{}),
    RecordType = function_name_to_record_type(FunctionName),
    NestedRecordDecoder(FunctionArgs, RecordType).


-spec record_type_to_function_name(record_type()) -> function_name().
record_type_to_function_name(ts_data_generator_literal) -> <<"literal">>;
record_type_to_function_name(ts_data_generator_current_value) -> <<"currentValue">>;
record_type_to_function_name(ts_data_generator_get_dynamic_series_config) -> <<"getDynamicSeriesConfig">>;
record_type_to_function_name(ts_data_generator_get_dynamic_series_group_config) -> <<"getDynamicSeriesGroupConfig">>;
record_type_to_function_name(ts_data_generator_load_series) -> <<"loadSeries">>;

record_type_to_function_name(ts_transformer_abs) -> <<"abs">>;
record_type_to_function_name(ts_transformer_multiply) -> <<"multiply">>;
record_type_to_function_name(ts_transformer_rate) -> <<"rate">>;
record_type_to_function_name(ts_transformer_time_derivative) -> <<"timeDerivative">>;
record_type_to_function_name(ts_transformer_replace_empty) -> <<"replaceEmpty">>.


-spec function_name_to_record_type(function_name()) -> record_type().
function_name_to_record_type(<<"literal">>) -> ts_data_generator_literal;
function_name_to_record_type(<<"currentValue">>) -> ts_data_generator_current_value;
function_name_to_record_type(<<"getDynamicSeriesConfig">>) -> ts_data_generator_get_dynamic_series_config;
function_name_to_record_type(<<"getDynamicSeriesGroupConfig">>) -> ts_data_generator_get_dynamic_series_group_config;
function_name_to_record_type(<<"loadSeries">>) -> ts_data_generator_load_series;

function_name_to_record_type(<<"abs">>) -> ts_transformer_abs;
function_name_to_record_type(<<"multiply">>) -> ts_transformer_multiply;
function_name_to_record_type(<<"rate">>) -> ts_transformer_rate;
function_name_to_record_type(<<"timeDerivative">>) -> ts_transformer_time_derivative;
function_name_to_record_type(<<"replaceEmpty">>) -> ts_transformer_replace_empty.
