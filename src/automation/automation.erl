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
-export([sanitize_binary/3]).
-export([store_type_to_json/1, store_type_from_json/1]).
-export([all_store_types/0]).
-export([lifecycle_state_to_json/1, lifecycle_state_from_json/1]).
-export([all_lifecycle_states/0]).

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

-type store_type() :: single_value | list | tree_forest | range | time_series | audit_log.

% Object kept in store and returned when iterating over it.
-type item() :: json_utils:json_term().

% Additional information solely for the potential users to improve joint management
% of lambdas and workflow schemas. These states do not impact the ability to
% execute a workflow, apart from the fact that a warning may be displayed in
% graphical interface concerning incompleteness or deprecation.
-type lifecycle_state() :: draft | stable | deprecated.

-export_type([id/0, name/0, summary/0, description/0]).
-export_type([lambda_operation_ref/0]).
-export_type([store_type/0, item/0]).
-export_type([lifecycle_state/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @TODO VFS-8507 Implement data sanitizers for all fields in automation models
-spec sanitize_binary(binary(), term(), pos_integer()) -> binary() | no_return().
sanitize_binary(_Key, Value, SizeLimit) when is_binary(Value) andalso byte_size(Value) =< SizeLimit ->
    Value;
sanitize_binary(Key, Value, SizeLimit) when is_binary(Value) ->
    throw(?ERROR_BAD_VALUE_BINARY_TOO_LARGE(Key, SizeLimit));
sanitize_binary(Key, _Value, _SizeLimit) ->
    throw(?ERROR_BAD_VALUE_BINARY(Key)).


-spec store_type_to_json(store_type()) -> json_utils:json_term().
store_type_to_json(single_value) -> <<"singleValue">>;
store_type_to_json(list) -> <<"list">>;
store_type_to_json(tree_forest) -> <<"treeForest">>;
store_type_to_json(range) -> <<"range">>;
store_type_to_json(time_series) -> <<"timeSeries">>;
store_type_to_json(audit_log) -> <<"auditLog">>.


-spec store_type_from_json(json_utils:json_term()) -> store_type().
store_type_from_json(<<"singleValue">>) -> single_value;
store_type_from_json(<<"list">>) -> list;
store_type_from_json(<<"treeForest">>) -> tree_forest;
store_type_from_json(<<"range">>) -> range;
store_type_from_json(<<"timeSeries">>) -> time_series;
store_type_from_json(<<"auditLog">>) -> audit_log.


-spec all_store_types() -> [store_type()].
all_store_types() ->
    [single_value, list, tree_forest, range, time_series, audit_log].


-spec lifecycle_state_to_json(lifecycle_state()) -> json_utils:json_term().
lifecycle_state_to_json(draft) -> <<"draft">>;
lifecycle_state_to_json(stable) -> <<"stable">>;
lifecycle_state_to_json(deprecated) -> <<"deprecated">>.


-spec lifecycle_state_from_json(json_utils:json_term()) -> lifecycle_state().
lifecycle_state_from_json(<<"draft">>) -> draft;
lifecycle_state_from_json(<<"stable">>) -> stable;
lifecycle_state_from_json(<<"deprecated">>) -> deprecated.


-spec all_lifecycle_states() -> [lifecycle_state()].
all_lifecycle_states() ->
    [draft, stable, deprecated].
