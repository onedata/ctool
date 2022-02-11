%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing time series metric schema used in automation machinery.
%%%
%%% Provides a template for metric configuration and is used by time series schema.
%%% Each time series schema defines a list of metrics, which will be created
%%% within the corresponding time series instance.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_metric_schema).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").
-include("errors.hrl").


%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type record() :: #atm_time_series_metric_schema{}.
-export_type([record/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_map().
to_json(Record) ->
    encode(Record).


-spec from_json(json_utils:json_map()) -> record().
from_json(RecordJson) ->
    decode(validate, RecordJson).

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, _NestedRecordEncoder) ->
    encode(Record).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, _NestedRecordDecoder) ->
    decode(skip_validation, RecordJson).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec aggregator_to_json(time_series:metric_aggregator()) -> json_utils:json_term().
aggregator_to_json(sum) -> <<"sum">>;
aggregator_to_json(max) -> <<"max">>;
aggregator_to_json(min) -> <<"min">>;
aggregator_to_json(first) -> <<"first">>;
aggregator_to_json(last) -> <<"last">>.


%% @private
-spec aggregator_from_json(json_utils:json_term()) -> time_series:metric_aggregator().
aggregator_from_json(<<"sum">>) -> sum;
aggregator_from_json(<<"max">>) -> max;
aggregator_from_json(<<"min">>) -> min;
aggregator_from_json(<<"first">>) -> first;
aggregator_from_json(<<"last">>) -> last.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec encode(record()) -> json_utils:json_term().
encode(Record) ->
    #{
        <<"id">> => Record#atm_time_series_metric_schema.id,
        <<"resolution">> => Record#atm_time_series_metric_schema.resolution,
        <<"retention">> => Record#atm_time_series_metric_schema.retention,
        <<"aggregator">> => aggregator_to_json(Record#atm_time_series_metric_schema.aggregator)
    }.


%% @private
-spec decode(validate | skip_validation, json_utils:json_term()) -> record().
decode(skip_validation, RecordJson) ->
    #atm_time_series_metric_schema{
        id = maps:get(<<"id">>, RecordJson),
        resolution = maps:get(<<"resolution">>, RecordJson),
        retention = maps:get(<<"retention">>, RecordJson),
        aggregator = aggregator_from_json(maps:get(<<"aggregator">>, RecordJson))
    };
decode(validate, RecordJson) ->
    Spec = decode(skip_validation, RecordJson),
    lists:member(Spec#atm_time_series_metric_schema.resolution, time_series:allowed_metric_resolutions()) orelse throw(
        ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"resolution">>, time_series:allowed_metric_resolutions())
    ),
    Spec.
