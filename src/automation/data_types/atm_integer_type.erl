%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module implementing the integer data type used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_integer_type).
-author("Lukasz Opiola").

-behaviour(atm_data_type).

-include("automation/automation.hrl").

%% atm_data_type callbacks
-export([is_instance/1]).
-export([value_constraints_to_json/1, value_constraints_from_json/1]).

%%%===================================================================
%%% atm_data_type callbacks
%%%===================================================================

-spec is_instance(json_utils:json_term()) -> boolean().
is_instance(Value) when is_integer(Value) -> true;
is_instance(_Value) -> false.


-spec value_constraints_to_json(atm_data_type:value_constraints()) -> json_utils:json_map().
value_constraints_to_json(_Constraints) ->
    #{}.


-spec value_constraints_from_json(json_utils:json_map()) -> atm_data_type:value_constraints().
value_constraints_from_json(_ConstraintsJson) ->
    #{}.
