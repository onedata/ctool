%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates concepts related to automation that are common
%%% for all Onedata services.
%%% @end
%%%-------------------------------------------------------------------
-module(automation).
-author("Lukasz Opiola").

-include("automation/automation.hrl").

% Arbitrary name, relevant for users, given to an automation related model.
-type name() :: binary().

% A short summary of what a lambda function does, in plaintext.
-type lambda_summary() :: binary().
% An extended description of a lambda function, in markdown format.
-type lambda_description() :: binary().
% Type of engine used to run a lambda function.
-type lambda_engine() :: onedata_function | open_faas | atm_workflow | user_form.
% Reference to the operation relevant for a specific lambda_engine:
%   onedata_function -> function identifier, eg. "calculate_checksum"
%   open_faas        -> docker image, eg. "onedata/open_faas_virus_checker:v19"
%   atm_workflow     -> Id of an atm_workflow to be run, eg. "ee1154e75e664dd888fb8959cf5e245b74b190d6"
%   user_form        -> Id of a user form that should be manually filled by a user to complete the lambda execution.
-type lambda_operation_ref() :: binary().

-export_type([name/0]).
-export_type([lambda_summary/0, lambda_description/0]).
-export_type([lambda_engine/0, lambda_operation_ref/0]).

%%% API
-export([lambda_engine_to_json/1, lambda_engine_from_json/1]).
-export([encode_lambda_engine/1, decode_lambda_engine/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec lambda_engine_to_json(lambda_engine()) -> json_utils:json_term().
lambda_engine_to_json(onedata_function) -> <<"onedataFunction">>;
lambda_engine_to_json(open_faas) -> <<"openFaas">>;
lambda_engine_to_json(atm_workflow) -> <<"atmWorkflow">>;
lambda_engine_to_json(user_form) -> <<"userForm">>.


-spec lambda_engine_from_json(json_utils:json_term()) -> lambda_engine().
lambda_engine_from_json(<<"onedataFunction">>) -> onedata_function;
lambda_engine_from_json(<<"openFaas">>) -> open_faas;
lambda_engine_from_json(<<"atmWorkflow">>) -> atm_workflow;
lambda_engine_from_json(<<"userForm">>) -> user_form.


-spec encode_lambda_engine(lambda_engine()) -> binary().
encode_lambda_engine(Engine) ->
    json_utils:encode(lambda_engine_to_json(Engine)).


-spec decode_lambda_engine(binary()) -> lambda_engine().
decode_lambda_engine(EncodedEngine) ->
    lambda_engine_from_json(json_utils:decode(EncodedEngine)).