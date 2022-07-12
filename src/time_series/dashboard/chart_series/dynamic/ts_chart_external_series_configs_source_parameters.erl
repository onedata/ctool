%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing parameters of a specification of a source for
%%% external series configs of a time series.
%%% @end
%%%-------------------------------------------------------------------
-module(ts_chart_external_series_configs_source_parameters).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("time_series/dashboard.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type record() :: #ts_chart_external_series_configs_source_parameters{}.
-export_type([record/0]).


%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    #{
        <<"timeSeriesNameGenerator">> => Record#ts_chart_external_series_configs_source_parameters.time_series_name_generator,
        <<"metricNames">> => Record#ts_chart_external_series_configs_source_parameters.metric_names
    }.


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    #ts_chart_external_series_configs_source_parameters{
        time_series_name_generator = maps:get(<<"timeSeriesNameGenerator">>, RecordJson),
        metric_names = maps:get(<<"metricNames">>, RecordJson)
    }.

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, _NestedRecordEncoder) ->
    to_json(Record).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, _NestedRecordDecoder) ->
    from_json(RecordJson).
