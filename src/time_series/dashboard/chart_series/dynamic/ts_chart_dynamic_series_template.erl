%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing a dynamic template for generating series for
%%% time series charts.
%%% @end
%%%-------------------------------------------------------------------
-module(ts_chart_dynamic_series_template).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("time_series/dashboard.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type record() :: #ts_chart_dynamic_series_template{}.
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
        <<"idProvider">> => NestedEncoder(Record#ts_chart_dynamic_series_template.id_provider, ts_provider_function),
        <<"nameProvider">> => NestedEncoder(Record#ts_chart_dynamic_series_template.name_provider, ts_provider_function),
        <<"colorProvider">> => case Record#ts_chart_dynamic_series_template.color_provider of
            undefined -> null;
            Value -> NestedEncoder(Value, ts_provider_function)
        end,
        <<"typeProvider">> => NestedEncoder(Record#ts_chart_dynamic_series_template.type_provider, ts_provider_function),
        <<"yAxisIdProvider">> => NestedEncoder(Record#ts_chart_dynamic_series_template.y_axis_id_provider, ts_provider_function),
        <<"groupIdProvider">> => case Record#ts_chart_dynamic_series_template.group_id_provider of
            undefined -> null;
            Value -> NestedEncoder(Value, ts_provider_function)
        end,
        <<"dataProvider">> => NestedEncoder(Record#ts_chart_dynamic_series_template.data_provider, ts_provider_function)
    }.


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedDecoder) ->
    #ts_chart_dynamic_series_template{
        id_provider = NestedDecoder(maps:get(<<"idProvider">>, RecordJson), ts_provider_function),
        name_provider = NestedDecoder(maps:get(<<"nameProvider">>, RecordJson), ts_provider_function),
        color_provider = case maps:get(<<"colorProvider">>, RecordJson, null) of
            null -> undefined;
            Value -> NestedDecoder(Value, ts_provider_function)
        end,
        type_provider = NestedDecoder(maps:get(<<"typeProvider">>, RecordJson), ts_provider_function),
        y_axis_id_provider = NestedDecoder(maps:get(<<"yAxisIdProvider">>, RecordJson), ts_provider_function),
        group_id_provider = case maps:get(<<"groupIdProvider">>, RecordJson, null) of
            null -> undefined;
            Value -> NestedDecoder(Value, ts_provider_function)
        end,
        data_provider = NestedDecoder(maps:get(<<"dataProvider">>, RecordJson), ts_provider_function)
    }.
