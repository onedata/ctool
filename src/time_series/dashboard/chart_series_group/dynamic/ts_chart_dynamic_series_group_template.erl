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
encode_with(#ts_chart_dynamic_series_group_template{
    id_provider = IdProvider,
    name_provider = NameProvider,
    stacked_provider = StackedProvider,
    show_sum_provider = ShowSumProvider,
    subgroups_provider = SubgroupsProvider
}, NestedEncoder) ->
    #{
        <<"idProvider">> => NestedEncoder(IdProvider, ts_provider_function),
        <<"nameProvider">> => encode_nullable_provider_function(NameProvider, NestedEncoder),
        <<"stackedProvider">> => encode_nullable_provider_function(StackedProvider, NestedEncoder),
        <<"showSumProvider">> => encode_nullable_provider_function(ShowSumProvider, NestedEncoder),
        <<"subgroupsProvider">> => encode_nullable_provider_function(SubgroupsProvider, NestedEncoder)
    }.


%% @private
-spec encode_nullable_provider_function(
    undefined | ts_provider_function:record(),
    persistent_record:nested_record_encoder()
) ->
    json_utils:json_term().
encode_nullable_provider_function(undefined, _NestedEncoder) ->
    null;
encode_nullable_provider_function(Value, NestedEncoder) ->
    NestedEncoder(Value, ts_provider_function).


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedDecoder) ->
    #ts_chart_dynamic_series_group_template{
        id_provider = NestedDecoder(maps:get(<<"idProvider">>, RecordJson), ts_provider_function),
        name_provider = decode_nullable_provider_function(maps:get(<<"nameProvider">>, RecordJson, null), NestedDecoder),
        stacked_provider = decode_nullable_provider_function(maps:get(<<"stackedProvider">>, RecordJson, null), NestedDecoder),
        show_sum_provider = decode_nullable_provider_function(maps:get(<<"showSumProvider">>, RecordJson, null), NestedDecoder),
        subgroups_provider = decode_nullable_provider_function(maps:get(<<"subgroupsProvider">>, RecordJson, null), NestedDecoder)
    }.


%% @private
-spec decode_nullable_provider_function(
    undefined | ts_provider_function:record(),
    persistent_record:nested_record_decoder()
) ->
    json_utils:json_term().
decode_nullable_provider_function(null, _NestedDecoder) ->
    undefined;
decode_nullable_provider_function(Value, NestedDecoder) ->
    NestedDecoder(Value, ts_provider_function).
