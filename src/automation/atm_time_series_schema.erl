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
%%% Each time series schema can have any number of metric schemas, which
%%% will be used to create metrics for the corresponding time series instance.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_schema).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").
-include("errors.hrl").


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
        <<"nameSelector">> => atm_time_series_attribute:name_selector_to_json(Record#atm_time_series_schema.name_selector),
        <<"name">> => atm_time_series_attribute:name_to_json(Record#atm_time_series_schema.name),
        <<"unit">> => atm_time_series_attribute:unit_to_json(Record#atm_time_series_schema.unit),
        <<"metrics">> => [NestedRecordEncoder(M, atm_time_series_metric_schema) || M <- Record#atm_time_series_schema.metrics]
    }.


%% @private
-spec decode_with(validate | skip_validation, json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(skip_validation, RecordJson, NestedRecordDecoder) ->
    #atm_time_series_schema{
        name_selector = atm_time_series_attribute:name_selector_from_json(maps:get(<<"nameSelector">>, RecordJson)),
        name = atm_time_series_attribute:name_from_json(maps:get(<<"name">>, RecordJson)),
        unit = atm_time_series_attribute:unit_from_json(maps:get(<<"unit">>, RecordJson)),
        metrics = [NestedRecordDecoder(M, atm_time_series_metric_schema) || M <- maps:get(<<"metrics">>, RecordJson)]
    };
decode_with(validate, RecordJson, NestedRecordDecoder) ->
    Spec = decode_with(skip_validation, RecordJson, NestedRecordDecoder),
    atm_time_series_attribute:validate_name(Spec#atm_time_series_schema.name_selector, Spec#atm_time_series_schema.name),
    % metric ids should be unique and it is not allowed to specify two metrics with the same resolution and aggregator
    lists:foldl(fun(MetricSpec, AlreadyUsedResolutionsAggregatorsAndIds) ->
        Id = MetricSpec#atm_time_series_metric_schema.id,
        ResolutionAndAggregator = {
            MetricSpec#atm_time_series_metric_schema.resolution,
            MetricSpec#atm_time_series_metric_schema.aggregator
        },
        ordsets:is_element(Id, AlreadyUsedResolutionsAggregatorsAndIds) andalso throw(
            ?ERROR_BAD_DATA(<<"metrics">>, <<"There cannot be two metrics with the same ID">>)
        ),
        ordsets:is_element(ResolutionAndAggregator, AlreadyUsedResolutionsAggregatorsAndIds) andalso throw(
            ?ERROR_BAD_DATA(<<"metrics">>, <<"There cannot be two metrics with the same resolution and aggregator">>)
        ),
        ordsets:add_element(Id, ordsets:add_element(ResolutionAndAggregator, AlreadyUsedResolutionsAggregatorsAndIds))
    end, ordsets:new(), Spec#atm_time_series_schema.metrics),
    Spec.
