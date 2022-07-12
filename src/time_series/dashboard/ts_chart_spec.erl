%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing a specification of a time series chart.
%%% @end
%%%-------------------------------------------------------------------
-module(ts_chart_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("time_series/dashboard.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #ts_chart_spec{}.
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
encode_with(Record, NestedEncoder) ->
    #{
        <<"title">> => case Record#ts_chart_spec.title of
            undefined -> null;
            Title -> NestedEncoder(Title, ts_title)
        end,
        <<"yAxes">> => [NestedEncoder(R, ts_chart_y_axis_spec) || R <- Record#ts_chart_spec.y_axes],
        <<"seriesGroupBuilders">> => [NestedEncoder(R, ts_chart_series_group_builder) || R <- Record#ts_chart_spec.series_group_builders],
        <<"seriesBuilders">> => [NestedEncoder(R, ts_chart_series_builder) || R <- Record#ts_chart_spec.series_builders]
    }.


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedDecoder) ->
    #ts_chart_spec{
        title = case maps:get(<<"title">>, RecordJson, null) of
            null -> undefined;
            Title -> NestedDecoder(Title, ts_title)
        end,
        y_axes = [NestedDecoder(R, ts_chart_y_axis_spec) || R <- maps:get(<<"yAxes">>, RecordJson)],
        series_group_builders = [NestedDecoder(R, ts_chart_series_group_builder) || R <- maps:get(<<"seriesGroupBuilders">>, RecordJson, [])],
        series_builders = [NestedDecoder(R, ts_chart_series_builder) || R <- maps:get(<<"seriesBuilders">>, RecordJson, [])]
    }.
