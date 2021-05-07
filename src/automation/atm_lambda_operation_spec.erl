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
-type spec() :: atm_onedata_function_operation_spec:record()
| atm_openfaas_operation_spec:record()
| atm_workflow_operation_spec:record()
| atm_user_form_operation_spec:record().
-type spec_module() :: atm_onedata_function_operation_spec
| atm_openfaas_operation_spec
| atm_atm_workflow_operation_spec
| atm_user_form_operation_spec.

-type record() :: #atm_lambda_operation_spec{}.
-export_type([engine/0, spec/0, record/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_engine(record()) -> engine().
get_engine(#atm_lambda_operation_spec{spec = #atm_onedata_function_operation_spec{}}) -> onedata_function;
get_engine(#atm_lambda_operation_spec{spec = #atm_openfaas_operation_spec{}}) -> openfaas;
get_engine(#atm_lambda_operation_spec{spec = #atm_workflow_operation_spec{}}) -> atm_workflow;
get_engine(#atm_lambda_operation_spec{spec = #atm_user_form_operation_spec{}}) -> user_form.


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
to_json(#atm_lambda_operation_spec{spec = Spec}) ->
    SpecModule = utils:record_type(Spec),
    Engine = spec_module_to_engine(SpecModule),
    maps:merge(
        #{<<"engine">> => engine_to_json(Engine)},
        SpecModule:to_json(Spec)
    ).


-spec from_json(json_utils:json_term()) -> record().
from_json(SpecJson) ->
    Engine = engine_from_json(maps:get(<<"engine">>, SpecJson)),
    SpecModule = engine_to_spec_module(Engine),
    #atm_lambda_operation_spec{
        spec = SpecModule:from_json(SpecJson)
    }.

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Spec, _NestedRecordEncoder) ->
    to_json(Spec).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(SpecJson, _NestedRecordDecoder) ->
    from_json(SpecJson).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec spec_module_to_engine(spec_module()) -> engine().
spec_module_to_engine(atm_onedata_function_operation_spec) -> onedata_function;
spec_module_to_engine(atm_openfaas_operation_spec) -> openfaas;
spec_module_to_engine(atm_workflow_operation_spec) -> atm_workflow;
spec_module_to_engine(atm_user_form_operation_spec) -> user_form.


-spec engine_to_spec_module(engine()) -> spec_module().
engine_to_spec_module(onedata_function) -> atm_onedata_function_operation_spec;
engine_to_spec_module(openfaas) -> atm_openfaas_operation_spec;
engine_to_spec_module(atm_workflow) -> atm_workflow_operation_spec;
engine_to_spec_module(user_form) -> atm_user_form_operation_spec.
