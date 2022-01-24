%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing time series specification used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").
-include("errors.hrl").


%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type record() :: #atm_time_series_spec{}.
-export_type([record/0]).

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

%% @private
-spec encode_with(record(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_with(Record, NestedRecordEncoder) ->
    #{
        <<"nameSelector">> => atm_time_series_data_spec:name_selector_to_json(Record#atm_time_series_spec.name_selector),
        <<"name">> => Record#atm_time_series_spec.name,
        <<"unit">> => atm_time_series_data_spec:unit_to_json(Record#atm_time_series_spec.unit),
        <<"metrics">> => [NestedRecordEncoder(M, atm_time_series_metric_spec) || M <- Record#atm_time_series_spec.metrics]
    }.


%% @private
-spec decode_with(db | json, json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(db, RecordJson, NestedRecordDecoder) ->
    #atm_time_series_spec{
        name_selector = atm_time_series_data_spec:name_selector_from_json(maps:get(<<"nameSelector">>, RecordJson)),
        name = maps:get(<<"name">>, RecordJson),
        unit = atm_time_series_data_spec:unit_from_json(maps:get(<<"unit">>, RecordJson)),
        metrics = [NestedRecordDecoder(M, atm_time_series_metric_spec) || M <- maps:get(<<"metrics">>, RecordJson)]
    };
decode_with(json, RecordJson, NestedRecordDecoder) ->
    Spec = decode_with(db, RecordJson, NestedRecordDecoder),
    atm_time_series_data_spec:validate_name(Spec#atm_time_series_spec.name_selector, Spec#atm_time_series_spec.name),
    % metric ids should be unique and it is not allowed to specify two metrics with the same resolution and aggregator
    lists:foldl(fun(MetricSpec, AlreadyUsedResolutionsAggregatorsAndIds) ->
        Id = MetricSpec#atm_time_series_metric_spec.id,
        ResolutionAndAggregator = {
            MetricSpec#atm_time_series_metric_spec.resolution,
            MetricSpec#atm_time_series_metric_spec.aggregator
        },
        ordsets:is_element(Id, AlreadyUsedResolutionsAggregatorsAndIds) andalso throw(
            ?ERROR_BAD_DATA(<<"metrics">>, <<"There cannot be two metrics with the same ID">>)
        ),
        ordsets:is_element(ResolutionAndAggregator, AlreadyUsedResolutionsAggregatorsAndIds) andalso throw(
            ?ERROR_BAD_DATA(<<"metrics">>, <<"There cannot be two metrics with the same resolution and aggregator">>)
        ),
        ordsets:add_element(Id, ordsets:add_element(ResolutionAndAggregator, AlreadyUsedResolutionsAggregatorsAndIds))
    end, ordsets:new(), Spec#atm_time_series_spec.metrics),
    Spec.
