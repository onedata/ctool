%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing a dynamic template for generating series group for
%%% time series charts.
%%% @end
%%%-------------------------------------------------------------------
-module(ts_chart_dynamic_series_group_template).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("time_series/dashboard.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type record() :: #ts_chart_dynamic_series_group_template{}.
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
        <<"idProvider">> => NestedEncoder(Record#ts_chart_dynamic_series_group_template.id_provider, ts_provider_function),
        <<"nameProvider">> => case Record#ts_chart_dynamic_series_group_template.name_provider of
            undefined -> null;
            Value -> NestedEncoder(Value, ts_provider_function)
        end,
        <<"stackedProvider">> => case Record#ts_chart_dynamic_series_group_template.stacked_provider of
            undefined -> null;
            Value -> NestedEncoder(Value, ts_provider_function)
        end,
        <<"showSumProvider">> => case Record#ts_chart_dynamic_series_group_template.show_sum_provider of
            undefined -> null;
            Value -> NestedEncoder(Value, ts_provider_function)
        end,
        <<"subgroups_provider">> => case Record#ts_chart_dynamic_series_group_template.subgroups_provider of
            undefined -> null;
            Value -> NestedEncoder(Value, ts_provider_function)
        end
    }.


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedDecoder) ->
    #ts_chart_dynamic_series_group_template{
        id_provider = NestedDecoder(maps:get(<<"idProvider">>, RecordJson), ts_provider_function),
        name_provider = case maps:get(<<"nameProvider">>, RecordJson, null) of
            null -> undefined;
            Value -> NestedDecoder(Value, ts_provider_function)
        end,
        stacked_provider = case maps:get(<<"stackedProvider">>, RecordJson, null) of
            null -> undefined;
            Value -> NestedDecoder(Value, ts_provider_function)
        end,
        show_sum_provider = case maps:get(<<"showSumProvider">>, RecordJson, null) of
            null -> undefined;
            Value -> NestedDecoder(Value, ts_provider_function)
        end,
        subgroups_provider = case maps:get(<<"subgroups_provider">>, RecordJson, null) of
            null -> undefined;
            Value -> NestedDecoder(Value, ts_provider_function)
        end
    }.
