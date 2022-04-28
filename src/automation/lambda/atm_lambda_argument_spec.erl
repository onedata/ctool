%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing lambda argument specification used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lambda_argument_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").


%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_lambda_argument_spec{}.
-export_type([record/0]).

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
    json_utils:json_term().
encode_with(Record, NestedRecordEncoder) ->
    #{
        <<"name">> => Record#atm_lambda_argument_spec.name,
        <<"dataSpec">> => NestedRecordEncoder(Record#atm_lambda_argument_spec.data_spec, atm_data_spec),
        <<"isOptional">> => Record#atm_lambda_argument_spec.is_optional,
        <<"defaultValue">> => utils:undefined_to_null(Record#atm_lambda_argument_spec.default_value)
    }.


%% @private
-spec decode_with(automation:validation_strategy(), json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(skip_validation, RecordJson, NestedRecordDecoder) ->
    #atm_lambda_argument_spec{
        name = maps:get(<<"name">>, RecordJson),
        data_spec = NestedRecordDecoder(maps:get(<<"dataSpec">>, RecordJson), atm_data_spec),
        is_optional = maps:get(<<"isOptional">>, RecordJson),
        default_value = utils:null_to_undefined(maps:get(<<"defaultValue">>, RecordJson, null))
    };
decode_with(validate, RecordJson, NestedRecordDecoder) ->
    #atm_lambda_argument_spec{name = Name} = ArgumentSpec = decode_with(skip_validation, RecordJson, NestedRecordDecoder),
    str_utils:validate_name(Name) orelse throw(?ERROR_BAD_VALUE_NAME(<<"argumentSpec.name">>)),
    ArgumentSpec.
