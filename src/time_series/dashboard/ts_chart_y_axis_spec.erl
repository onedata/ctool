%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing a specification of y axis of a time series chart.
%%% @end
%%%-------------------------------------------------------------------
-module(ts_chart_y_axis_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("time_series/dashboard.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


%% @formatter:off
-type unit_options() :: #{
    custom_name => binary(),
    use_metric_suffix => boolean(),
    format => si | iec
}.
-export_type([unit_options/0]).
%% @formatter:on

-type record() :: #ts_chart_y_axis_spec{}.
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
        <<"id">> => Record#ts_chart_y_axis_spec.id,
        <<"name">> => Record#ts_chart_y_axis_spec.name,
        <<"minInterval">> => utils:undefined_to_null(Record#ts_chart_y_axis_spec.min_interval),
        <<"unitName">> => Record#ts_chart_y_axis_spec.unit_name,
        <<"unitOptions">> => case Record#ts_chart_y_axis_spec.unit_options of
            undefined -> null;
            UnitOptions -> unit_options_to_json(UnitOptions)
        end,
        <<"valueProvider">> => NestedEncoder(Record#ts_chart_y_axis_spec.value_provider, ts_provider_function)
    }.


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedDecoder) ->
    #ts_chart_y_axis_spec{
        id = maps:get(<<"id">>, RecordJson),
        name = maps:get(<<"name">>, RecordJson),
        min_interval = utils:null_to_undefined(maps:get(<<"minInterval">>, RecordJson)),
        unit_name = maps:get(<<"unitName">>, RecordJson, <<"none">>),
        unit_options = case maps:get(<<"unitOptions">>, RecordJson, null) of
            null -> undefined;
            UnitOptionsJson -> unit_options_from_json(UnitOptionsJson)
        end,
        value_provider = case maps:get(<<"valueProvider">>, RecordJson, null) of
            null -> #ts_data_generator_current_value{};
            ValueProvider -> NestedDecoder(ValueProvider, ts_provider_function)
        end
    }.


%% @private
-spec unit_options_to_json(unit_options()) -> json_utils:json_term().
unit_options_to_json(UnitOptions) ->
    maps_utils:map_key_value(fun
        (custom_name, Bin) when is_binary(Bin) -> {<<"customName">>, Bin};
        (use_metric_suffix, Bool) when is_boolean(Bool) -> {<<"useMetricSuffix">>, Bool};
        (format, si) -> {<<"format">>, <<"si">>};
        (format, iec) -> {<<"format">>, <<"iec">>}
    end, UnitOptions).


%% @private
-spec unit_options_from_json(json_utils:json_term()) -> unit_options().
unit_options_from_json(UnitOptionsJson) ->
    maps_utils:map_key_value(fun
        (<<"customName">>, Bin) when is_binary(Bin) -> {custom_name, Bin};
        (<<"useMetricSuffix">>, Bool) when is_boolean(Bool) -> {use_metric_suffix, Bool};
        (<<"format">>, <<"si">>) -> {format, si};
        (<<"format">>, <<"iec">>) -> {format, iec}
    end, UnitOptionsJson).
