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

%% API
-export([store_type_to_json/1, store_type_from_json/1]).
-export([workflow_schema_state_to_json/1, workflow_schema_state_from_json/1]).

% Identifier of an instance of an automation-related model.
-type id() :: binary().
% Arbitrary name, relevant for users, given to an automation-related model.
-type name() :: binary().
% A short summary assigned to an automation-related model, in plaintext.
-type summary() :: binary().
% An extended description assigned to an automation-related model, in markdown format.
-type description() :: binary().

% Reference to the operation relevant for a specific atm_lambda_engine_type:type()
%   onedata_function -> function identifier, eg. "calculate_checksum"
%   openfaas        -> docker image, eg. "onedata/openfaas_virus_checker:v19"
%   atm_workflow     -> Id of an atm_workflow to be run, eg. "ee1154e75e664dd888fb8959cf5e245b74b190d6"
%   user_form        -> Id of a user form that should be manually filled by a user to complete the lambda execution.
-type lambda_operation_ref() :: binary().

-type store_type() :: single_value | list | map | tree_forest | range | histogram.

-type workflow_schema_state() :: incomplete | ready | deprecated.

-export_type([id/0, name/0, summary/0, description/0]).
-export_type([lambda_operation_ref/0]).
-export_type([store_type/0]).
-export_type([workflow_schema_state/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec store_type_to_json(automation:store_type()) -> json_utils:json_term().
store_type_to_json(single_value) -> <<"singleValue">>;
store_type_to_json(list) -> <<"list">>;
store_type_to_json(map) -> <<"map">>;
store_type_to_json(tree_forest) -> <<"treeForest">>;
store_type_to_json(range) -> <<"range">>;
store_type_to_json(histogram) -> <<"histogram">>.


-spec store_type_from_json(json_utils:json_term()) -> automation:store_type().
store_type_from_json(<<"singleValue">>) -> single_value;
store_type_from_json(<<"list">>) -> list;
store_type_from_json(<<"map">>) -> map;
store_type_from_json(<<"treeForest">>) -> tree_forest;
store_type_from_json(<<"range">>) -> range;
store_type_from_json(<<"histogram">>) -> histogram.


-spec workflow_schema_state_to_json(workflow_schema_state()) -> json_utils:json_term().
workflow_schema_state_to_json(incomplete) -> <<"incomplete">>;
workflow_schema_state_to_json(ready) -> <<"ready">>;
workflow_schema_state_to_json(deprecated) -> <<"deprecated">>.


-spec workflow_schema_state_from_json(json_utils:json_term()) -> workflow_schema_state().
workflow_schema_state_from_json(<<"incomplete">>) -> incomplete;
workflow_schema_state_from_json(<<"ready">>) -> ready;
workflow_schema_state_from_json(<<"deprecated">>) -> deprecated.
