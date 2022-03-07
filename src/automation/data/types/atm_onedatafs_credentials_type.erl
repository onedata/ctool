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
-export([is_instance/1]).
-export([encode_value_constraints/2, decode_value_constraints/3]).

%%%===================================================================
%%% atm_data_type callbacks
%%%===================================================================

%% @TODO VFS-7687 Implement all automation data types and validators
-spec is_instance(json_utils:json_term()) -> boolean().
is_instance(Value) when is_map(Value) -> true;
is_instance(_Value) -> false.


-spec encode_value_constraints(atm_data_type:value_constraints(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_value_constraints(_Constraints, _Encoder) ->
    #{}.


-spec decode_value_constraints(
    automation:validation_strategy(),
    json_utils:json_term(),
    persistent_record:nested_record_decoder()
) ->
    atm_data_type:value_constraints().
decode_value_constraints(_ValidationStrategy, _ConstraintsJson, _Decoder) ->
    #{}.
