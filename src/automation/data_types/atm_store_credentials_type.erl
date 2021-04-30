%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module implementing the store credentials data type used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_store_credentials_type).
-author("Lukasz Opiola").

-behaviour(atm_data_type).

-include("automation/automation.hrl").

%% atm_data_type callbacks
-export([value_constraints_to_json/1, value_constraints_from_json/1]).

-type store_type() :: single_value | list | map | forest | range | histogram.

%%%===================================================================
%%% atm_data_type callbacks
%%%===================================================================

-spec value_constraints_to_json(atm_data_type:value_constraints()) -> json_utils:json_map().
value_constraints_to_json(Constraints) ->
    StoreType = maps:get(store_type, Constraints),
    #{
        <<"storeType">> => store_type_to_json(StoreType)
    }.


-spec value_constraints_from_json(json_utils:json_map()) -> atm_data_type:value_constraints().
value_constraints_from_json(ConstraintsJson) ->
    StoreTypeJson = maps:get(<<"storeType">>, ConstraintsJson),
    #{
        store_type => store_type_from_json(StoreTypeJson)
    }.


-spec store_type_to_json(store_type()) -> json_utils:json_term().
store_type_to_json(single_value) -> <<"singleValue">>;
store_type_to_json(list) -> <<"list">>;
store_type_to_json(map) -> <<"map">>;
store_type_to_json(forest) -> <<"forest">>;
store_type_to_json(range) -> <<"range">>;
store_type_to_json(histogram) -> <<"histogram">>.


-spec store_type_from_json(json_utils:json_term()) -> store_type().
store_type_from_json(<<"singleValue">>) -> single_value;
store_type_from_json(<<"list">>) -> list;
store_type_from_json(<<"map">>) -> map;
store_type_from_json(<<"forest">>) -> forest;
store_type_from_json(<<"range">>) -> range;
store_type_from_json(<<"histogram">>) -> histogram.

