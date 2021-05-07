%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing lambda result specification used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lambda_result_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_lambda_result_spec{}.
-export_type([record/0]).

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Spec) ->
    encode_with(Spec, fun jsonable_record:to_json/2).


-spec from_json(json_utils:json_term()) -> record().
from_json(SpecJson) ->
    decode_with(SpecJson, fun jsonable_record:from_json/2).

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Spec, NestedRecordEncoder) ->
    encode_with(Spec, NestedRecordEncoder).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(SpecJson, NestedRecordDecoder) ->
    decode_with(SpecJson, NestedRecordDecoder).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec encode_with(record(), fun((record(), atm_data_spec) -> json_utils:json_term())) ->
    json_utils:json_term().
encode_with(Spec, EncodeDataSpecFun) ->
    #{
        <<"name">> => Spec#atm_lambda_result_spec.name,
        <<"dataSpec">> => EncodeDataSpecFun(Spec#atm_lambda_result_spec.data_spec, atm_data_spec),
        <<"isBatch">> => Spec#atm_lambda_result_spec.is_batch
    }.


-spec decode_with(json_utils:json_term(), fun((json_utils:json_term(), atm_data_spec) -> record())) ->
    record().
decode_with(SpecJson, DecodeDataSpecFun) ->
    #atm_lambda_result_spec{
        name = maps:get(<<"name">>, SpecJson),
        data_spec = DecodeDataSpecFun(maps:get(<<"dataSpec">>, SpecJson), atm_data_spec),
        is_batch = maps:get(<<"isBatch">>, SpecJson)
    }.

