%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing a definition of a source for external series group configs
%%% of a time series.
%%% @end
%%%-------------------------------------------------------------------
-module(ts_chart_dynamic_series_group_configs_source).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("time_series/dashboard.hrl").


%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #ts_chart_dynamic_series_group_configs_source{}.
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
        <<"sourceType">> => time_series:dynamic_source_type_to_json(Record#ts_chart_dynamic_series_group_configs_source.source_type),
        <<"sourceSpec">> => NestedEncoder(
            Record#ts_chart_dynamic_series_group_configs_source.source_spec,
            ts_chart_external_series_group_configs_source_spec
        )
    }.


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedDecoder) ->
    #ts_chart_dynamic_series_group_configs_source{
        source_type = time_series:dynamic_source_type_from_json(maps:get(<<"sourceType">>, RecordJson)),
        source_spec = NestedDecoder(
            maps:get(<<"sourceSpec">>, RecordJson),
            ts_chart_external_series_group_configs_source_spec
        )
    }.
