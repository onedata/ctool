%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing a specialization of time series transformer:
%%% the "replace_empty" transformer that replaces the input value with a
%%% newly generated value if it is empty.
%%% @end
%%%-------------------------------------------------------------------
-module(ts_transformer_replace_empty).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("time_series/dashboard.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type record() :: #ts_transformer_replace_empty{}.
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
        <<"inputDataProvider">> => NestedEncoder(Record#ts_transformer_replace_empty.input_data_provider, ts_provider_function),
        <<"fallbackValueProvider">> => NestedEncoder(Record#ts_transformer_replace_empty.fallback_value_provider, ts_provider_function),
        <<"strategyProvider">> => case Record#ts_transformer_replace_empty.strategy_provider of
            undefined -> null;
            Value -> NestedEncoder(Value, ts_provider_function)
        end
    }.


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedDecoder) ->
    #ts_transformer_replace_empty{
        input_data_provider = NestedDecoder(maps:get(<<"inputDataProvider">>, RecordJson), ts_provider_function),
        fallback_value_provider = NestedDecoder(maps:get(<<"fallbackValueProvider">>, RecordJson), ts_provider_function),
        strategy_provider = case maps:get(<<"strategyProvider">>, RecordJson, null) of
            null -> undefined;
            Value -> NestedDecoder(Value, ts_provider_function)
        end
    }.
