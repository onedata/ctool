%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Base model for data specs used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_data_spec).
-author("Bartosz Walkowicz").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").


%% API
-export([get_type/1, get_value_constraints/1]).

%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_data_spec{}.
-export_type([record/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec get_type(record()) -> atm_data_type:type().
get_type(#atm_data_spec{type = Type}) ->
    Type.


-spec get_value_constraints(record()) -> atm_data_type:value_constraints().
get_value_constraints(#atm_data_spec{value_constraints = ValueConstraints}) ->
    ValueConstraints.

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================


-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    encode_with(Record, fun jsonable_record:to_json/2).


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    decode_with(validate, RecordJson, fun jsonable_record:from_json/2).

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
    decode_with(skip_validation, RecordJson, NestedRecordDecoder).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec encode_with(record(), persistent_record:nested_record_encoder()) ->
    json_utils:json_map().
encode_with(#atm_data_spec{type = Type, value_constraints = ValueConstraints}, NestedRecordEncoder) ->
    #{
        <<"type">> => atm_data_type:type_to_json(Type),
        <<"valueConstraints">> => atm_data_type:encode_value_constraints(
            Type, ValueConstraints, NestedRecordEncoder
        )
    }.


%% @private
-spec decode_with(jsonable_record:validation_strategy(), json_utils:json_map(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(ValidationStrategy, RecordJson, NestedRecordDecoder) ->
    Type = atm_data_type:type_from_json(maps:get(<<"type">>, RecordJson)),
    ValueConstraints = maps:get(<<"valueConstraints">>, RecordJson, #{}),
    #atm_data_spec{
        type = Type,
        value_constraints = atm_data_type:decode_value_constraints(
            Type, ValidationStrategy, ValueConstraints, NestedRecordDecoder
        )
    }.
