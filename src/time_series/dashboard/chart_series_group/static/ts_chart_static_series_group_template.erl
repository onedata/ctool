%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing a static template for generating series groups for
%%% time series charts.
%%% @end
%%%-------------------------------------------------------------------
-module(ts_chart_static_series_group_template).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("time_series/dashboard.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #ts_chart_static_series_group_template{}.
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
        <<"id">> => Record#ts_chart_static_series_group_template.id,
        <<"name">> => Record#ts_chart_static_series_group_template.name,
        <<"stacked">> => utils:undefined_to_null(Record#ts_chart_static_series_group_template.stacked),
        <<"showSum">> => Record#ts_chart_static_series_group_template.show_sum,
        <<"subgroups">> => [NestedEncoder(R, ?MODULE) || R <- Record#ts_chart_static_series_group_template.subgroups]
    }.


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedDecoder) ->
    #ts_chart_static_series_group_template{
        id = maps:get(<<"id">>, RecordJson),
        name = maps:get(<<"name">>, RecordJson, <<"">>),
        stacked = utils:null_to_undefined(maps:get(<<"stacked">>, RecordJson, false)),
        show_sum = maps:get(<<"showSum">>, RecordJson, false),
        subgroups = [NestedDecoder(R, ?MODULE) || R <- maps:get(<<"subgroups">>, RecordJson, [])]
    }.
