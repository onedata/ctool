%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing a template from which a time series instance is created.
%%%
%%% These schemas are used in time series collections - its schema defines a
%%% list of time series schemas. During the lifecycle of the collection,
%%% corresponding time series instances are instantiated using the schemas.
%%% Later, the schemas can be used to infer what time series instances have
%%% been created based on what schemas (using the name generators).
%%%
%%% Each time series schema can have any number of metric configs, which
%%% will be used to create metrics for the corresponding time series instance.
%%% @end
%%%-------------------------------------------------------------------
-module(time_series_schema).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").


%% API
-export([name_generator_type_to_json/1, name_generator_type_from_json/1]).
-export([name_generator_to_json/1, name_generator_from_json/1]).

%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type record() :: #time_series_schema{}.
-export_type([record/0]).


% This pair defines a generator of time series names, which indicates what time
% series names can be created from this schema. In case of 'exact' generator, only
% one time series with the exact name may be created. The 'add_prefix' generator
% indicates that any number of time series can be created from the schema, but they
% will all share the same name prefix.
% Knowing the generators, one can examine a time series collection and infer what
% time series have been created from what time series schemas.
-type name_generator_type() :: exact | add_prefix.
-type name_generator() :: binary().
-export_type([name_generator_type/0, name_generator/0]).


%%%===================================================================
%%% API
%%%===================================================================


-spec name_generator_type_to_json(name_generator_type()) -> json_utils:json_term().
name_generator_type_to_json(exact) -> <<"exact">>;
name_generator_type_to_json(add_prefix) -> <<"addPrefix">>.

-spec name_generator_type_from_json(json_utils:json_term()) -> name_generator_type().
name_generator_type_from_json(<<"exact">>) -> exact;
name_generator_type_from_json(<<"addPrefix">>) -> add_prefix.


-spec name_generator_to_json(name_generator()) -> json_utils:json_term().
name_generator_to_json(NameGenerator) when is_binary(NameGenerator) -> NameGenerator.

-spec name_generator_from_json(json_utils:json_term()) -> name_generator().
name_generator_from_json(NameGenerator) when is_binary(NameGenerator) -> NameGenerator.



%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    encode_with(Record, fun jsonable_record:to_json/2).


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    decode_with(validate, RecordJson, fun jsonable_record:from_json/2).

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
    decode_with(skip_validation, RecordJson, NestedRecordDecoder).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec encode_with(record(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_with(Record, NestedRecordEncoder) ->
    #{
        <<"nameGeneratorType">> => name_generator_type_to_json(Record#time_series_schema.name_generator_type),
        <<"nameGenerator">> => name_generator_to_json(Record#time_series_schema.name_generator),
        <<"unit">> => time_series:unit_to_json(Record#time_series_schema.unit),
        <<"metrics">> => maps:map(fun(MetricName, MetricConfig) when is_binary(MetricName) ->
            NestedRecordEncoder(MetricConfig, metric_config)
        end, Record#time_series_schema.metrics)
    }.


%% @private
-spec decode_with(jsonable_record:validation_strategy(), json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(skip_validation, RecordJson, NestedRecordDecoder) ->
    #time_series_schema{
        name_generator_type = name_generator_type_from_json(maps:get(<<"nameGeneratorType">>, RecordJson)),
        name_generator = name_generator_from_json(maps:get(<<"nameGenerator">>, RecordJson)),
        unit = time_series:unit_from_json(maps:get(<<"unit">>, RecordJson)),
        metrics = maps:map(fun(MetricName, MetricConfig) when is_binary(MetricName) ->
            NestedRecordDecoder(MetricConfig, metric_config)
        end, maps:get(<<"metrics">>, RecordJson))
    };
decode_with(validate, RecordJson, NestedRecordDecoder) ->
    Spec = decode_with(skip_validation, RecordJson, NestedRecordDecoder),

    maps:size(Spec#time_series_schema.metrics) == 0 andalso throw(?ERROR_BAD_VALUE_EMPTY(<<"metrics">>)),

    % it is not allowed to specify two metrics with the same resolution and aggregator
    maps:fold(fun(_MetricName, MetricConfig, AlreadyUsedResolutionsAndAggregators) ->
        ResolutionAndAggregator = {
            MetricConfig#metric_config.resolution,
            MetricConfig#metric_config.aggregator
        },
        ordsets:is_element(ResolutionAndAggregator, AlreadyUsedResolutionsAndAggregators) andalso throw(
            ?ERROR_BAD_DATA(<<"metrics">>, <<"There cannot be two metrics with the same resolution and aggregator">>)
        ),
        ordsets:add_element(ResolutionAndAggregator, AlreadyUsedResolutionsAndAggregators)
    end, ordsets:new(), Spec#time_series_schema.metrics),
    Spec.
