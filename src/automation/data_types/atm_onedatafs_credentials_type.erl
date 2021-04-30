%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module implementing the OnedataFS data type used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_onedatafs_credentials_type).
-author("Lukasz Opiola").

-behaviour(atm_data_type).

-include("automation/automation.hrl").

%% atm_data_type callbacks
-export([value_constraints_to_json/1, value_constraints_from_json/1]).

%%%===================================================================
%%% atm_data_type callbacks
%%%===================================================================

-spec value_constraints_to_json(atm_data_type:value_constraints()) -> json_utils:json_map().
value_constraints_to_json(_Constraints) ->
    #{}.


-spec value_constraints_from_json(json_utils:json_map()) -> atm_data_type:value_constraints().
value_constraints_from_json(_ConstraintsJson) ->
    #{}.
