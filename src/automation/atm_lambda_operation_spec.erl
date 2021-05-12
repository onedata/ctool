%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing lambda operation spec used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lambda_operation_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% API
-export([get_engine/1]).
-export([allowed_engines_for_custom_lambdas/0]).
-export([engine_to_json/1, engine_from_json/1]).

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type engine() :: onedata_function | openfaas | atm_workflow | user_form.
-type record() :: atm_onedata_function_operation_spec:record()
| atm_openfaas_operation_spec:record()
| atm_workflow_operation_spec:record()
| atm_user_form_operation_spec:record().
-type record_type() :: atm_onedata_function_operation_spec
| atm_openfaas_operation_spec
| atm_atm_workflow_operation_spec
| atm_user_form_operation_spec.

-export_type([engine/0, record/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_engine(record()) -> engine().
get_engine(#atm_onedata_function_operation_spec{}) -> onedata_function;
get_engine(#atm_openfaas_operation_spec{}) -> openfaas;
get_engine(#atm_workflow_operation_spec{}) -> atm_workflow;
get_engine(#atm_user_form_operation_spec{}) -> user_form.


%%--------------------------------------------------------------------
%% @doc
%% Users are allowed to create custom lambdas, but the onedata_function
%% engine type is restricted to predefined lambdas only.
%% @end
%%--------------------------------------------------------------------
-spec allowed_engines_for_custom_lambdas() -> [engine()].
allowed_engines_for_custom_lambdas() ->
    [openfaas, atm_workflow, user_form].


-spec engine_to_json(engine()) -> json_utils:json_term().
engine_to_json(onedata_function) -> <<"onedataFunction">>;
engine_to_json(openfaas) -> <<"openfaas">>;
engine_to_json(atm_workflow) -> <<"atmWorkflow">>;
engine_to_json(user_form) -> <<"userForm">>.


-spec engine_from_json(json_utils:json_term()) -> engine().
engine_from_json(<<"onedataFunction">>) -> onedata_function;
engine_from_json(<<"openfaas">>) -> openfaas;
engine_from_json(<<"atmWorkflow">>) -> atm_workflow;
engine_from_json(<<"userForm">>) -> user_form.

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
db_encode(Spec, NestedRecordEncoder) ->
    encode_with(Spec, NestedRecordEncoder).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(SpecJson, NestedRecordDecoder) ->
    decode_with(SpecJson, NestedRecordDecoder).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec encode_with(record(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_with(Record, NestedRecordEncoder) ->
    RecordType = utils:record_type(Record),
    Engine = record_type_to_engine(RecordType),
    maps:merge(
        #{<<"engine">> => engine_to_json(Engine)},
        NestedRecordEncoder(Record, RecordType)
    ).


-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(SpecJson, NestedRecordDecoder) ->
    Engine = engine_from_json(maps:get(<<"engine">>, SpecJson)),
    RecordType = engine_to_record_type(Engine),
    NestedRecordDecoder(SpecJson, RecordType).


-spec record_type_to_engine(record_type()) -> engine().
record_type_to_engine(atm_onedata_function_operation_spec) -> onedata_function;
record_type_to_engine(atm_openfaas_operation_spec) -> openfaas;
record_type_to_engine(atm_workflow_operation_spec) -> atm_workflow;
record_type_to_engine(atm_user_form_operation_spec) -> user_form.


-spec engine_to_record_type(engine()) -> record_type().
engine_to_record_type(onedata_function) -> atm_onedata_function_operation_spec;
engine_to_record_type(openfaas) -> atm_openfaas_operation_spec;
engine_to_record_type(atm_workflow) -> atm_workflow_operation_spec;
engine_to_record_type(user_form) -> atm_user_form_operation_spec.
