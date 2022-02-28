%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing time series schema used in automation machinery.
%%%
%%% A single time series schema provides a template from which a single
%%% time series instance will be created during workflow execution.
%%% These schemas are used in time series stores - its config defines a
%%% list of time series schemas. When a store is instantiated during
%%% workflow execution, corresponding time series instances are created
%%% using the schemas and they constitute a time series collection
%%% associated with the store.
%%%
%%% Each time series schema can have any number of metric configs, which
%%% will be used to create metrics for the corresponding time series instance.
%%%
%%% The module @see atm_time_series_names describes how measurements are
%%% mapped into time series instances based on name generators specified
%%% in the schema.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_schema).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").


%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type record() :: #atm_time_series_schema{}.
-export_type([record/0]).

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
        <<"nameGeneratorType">> => atm_time_series_names:target_ts_name_generator_type_to_json(Record#atm_time_series_schema.name_generator_type),
        <<"nameGenerator">> => atm_time_series_names:target_ts_name_generator_to_json(Record#atm_time_series_schema.name_generator),
        <<"unit">> => time_series:unit_to_json(Record#atm_time_series_schema.unit),
        <<"metrics">> => [NestedRecordEncoder(M, metric_config) || M <- Record#atm_time_series_schema.metrics]
    }.


%% @private
-spec decode_with(validate | skip_validation, json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(skip_validation, RecordJson, NestedRecordDecoder) ->
    #atm_time_series_schema{
        name_generator_type = atm_time_series_names:target_ts_name_generator_type_from_json(maps:get(<<"nameGeneratorType">>, RecordJson)),
        name_generator = atm_time_series_names:target_ts_name_generator_from_json(maps:get(<<"nameGenerator">>, RecordJson)),
        unit = time_series:unit_from_json(maps:get(<<"unit">>, RecordJson)),
        metrics = [NestedRecordDecoder(M, metric_config) || M <- maps:get(<<"metrics">>, RecordJson)]
    };
decode_with(validate, RecordJson, NestedRecordDecoder) ->
    Spec = decode_with(skip_validation, RecordJson, NestedRecordDecoder),
    % metric ids must be unique and it is not allowed to specify two metrics with the same resolution and aggregator
    lists:foldl(fun(MetricSpec, AlreadyUsedResolutionsAggregatorsAndLabels) ->
        Label = MetricSpec#metric_config.label,
        ResolutionAndAggregator = {
            MetricSpec#metric_config.resolution,
            MetricSpec#metric_config.aggregator
        },
        ordsets:is_element(Label, AlreadyUsedResolutionsAggregatorsAndLabels) andalso throw(
            ?ERROR_BAD_DATA(<<"metrics">>, <<"There cannot be two metrics with the same label">>)
        ),
        ordsets:is_element(ResolutionAndAggregator, AlreadyUsedResolutionsAggregatorsAndLabels) andalso throw(
            ?ERROR_BAD_DATA(<<"metrics">>, <<"There cannot be two metrics with the same resolution and aggregator">>)
        ),
        ordsets:add_element(Label, ordsets:add_element(ResolutionAndAggregator, AlreadyUsedResolutionsAggregatorsAndLabels))
    end, ordsets:new(), Spec#atm_time_series_schema.metrics),
    Spec.
