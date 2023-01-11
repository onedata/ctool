%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing a static template for generating series for
%%% time series charts.
%%% @end
%%%-------------------------------------------------------------------
-module(ts_chart_static_series_template).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("time_series/dashboard.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type chart_type() :: line | bar.

-type record() :: #ts_chart_static_series_template{}.
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
        <<"id">> => Record#ts_chart_static_series_template.id,
        <<"name">> => Record#ts_chart_static_series_template.name,
        <<"color">> => utils:undefined_to_null(Record#ts_chart_static_series_template.color),
        <<"type">> => chart_type_to_json(Record#ts_chart_static_series_template.type),
        <<"yAxisId">> => Record#ts_chart_static_series_template.y_axis_id,
        <<"groupId">> => utils:undefined_to_null(Record#ts_chart_static_series_template.group_id),
        <<"dataProvider">> => NestedEncoder(Record#ts_chart_static_series_template.data_provider, ts_provider_function)
    }.


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedDecoder) ->
    #ts_chart_static_series_template{
        id = maps:get(<<"id">>, RecordJson),
        name = maps:get(<<"name">>, RecordJson),
        color = utils:null_to_undefined(maps:get(<<"color">>, RecordJson, null)),
        type = chart_type_from_json(maps:get(<<"type">>, RecordJson)),
        y_axis_id = maps:get(<<"yAxisId">>, RecordJson),
        group_id = utils:null_to_undefined(maps:get(<<"groupId">>, RecordJson, null)),
        data_provider = NestedDecoder(maps:get(<<"dataProvider">>, RecordJson), ts_provider_function)
    }.


%% @private
-spec chart_type_to_json(chart_type()) -> json_utils:json_term().
chart_type_to_json(line) -> <<"line">>;
chart_type_to_json(bar) -> <<"bar">>.


%% @private
-spec chart_type_from_json(json_utils:json_term()) -> chart_type().
chart_type_from_json(<<"line">>) -> line;
chart_type_from_json(<<"bar">>) -> bar.
