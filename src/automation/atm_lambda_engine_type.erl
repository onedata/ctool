%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Jsonable record expressing lambda engine type used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lambda_engine_type).
-author("Lukasz Opiola").

-behaviour(persistent_record).

-include("automation/automation.hrl").

%% API
-export([allowed_types_for_custom_lambdas/0]).

%% Persistent record callbacks
-export([version/0, to_json/1, from_json/1]).


-type type() :: onedata_function | openfaas | atm_workflow | user_form.
-type record() :: #atm_lambda_engine_type{}.
-export_type([record/0, type/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Users are allowed to create custom lambdas, but the onedata_function
%% engine type is restricted to predefined lambdas only.
%% @end
%%--------------------------------------------------------------------
-spec allowed_types_for_custom_lambdas() -> [record()].
allowed_types_for_custom_lambdas() ->
    [
        #atm_lambda_engine_type{type = openfaas},
        #atm_lambda_engine_type{type = atm_workflow},
        #atm_lambda_engine_type{type = user_form}
    ].

%%%===================================================================
%%% Persistent record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec to_json(record()) -> json_utils:json_term().
to_json(#atm_lambda_engine_type{type = onedata_function}) -> <<"onedataFunction">>;
to_json(#atm_lambda_engine_type{type = openfaas}) -> <<"openfaas">>;
to_json(#atm_lambda_engine_type{type = atm_workflow}) -> <<"atmWorkflow">>;
to_json(#atm_lambda_engine_type{type = user_form}) -> <<"userForm">>.


-spec from_json(json_utils:json_term()) -> record().
from_json(<<"onedataFunction">>) -> #atm_lambda_engine_type{type = onedata_function};
from_json(<<"openfaas">>) -> #atm_lambda_engine_type{type = openfaas};
from_json(<<"atmWorkflow">>) -> #atm_lambda_engine_type{type = atm_workflow};
from_json(<<"userForm">>) -> #atm_lambda_engine_type{type = user_form}.