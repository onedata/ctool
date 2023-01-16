%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module implementing the number data type used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_number_type).
-author("Lukasz Opiola").

-behaviour(atm_data_type).

-include("automation/automation.hrl").

%% atm_data_type callbacks
-export([is_instance/1]).
-export([encode_value_constraints/2, decode_value_constraints/3]).

-type type() :: integer | float | any.

%%%===================================================================
%%% atm_data_type callbacks
%%%===================================================================

-spec is_instance(json_utils:json_term()) -> boolean().
is_instance(Value) when is_number(Value) -> true;
is_instance(_Value) -> false.


-spec encode_value_constraints(atm_data_type:value_constraints(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_value_constraints(Constraints, _Encoder) ->
    Type = maps:get(type, Constraints, any),
    #{
        <<"type">> => type_to_json(Type)
    }.


-spec decode_value_constraints(
    jsonable_record:validation_strategy(),
    json_utils:json_term(),
    persistent_record:nested_record_decoder()
) ->
    atm_data_type:value_constraints().
decode_value_constraints(_ValidationStrategy, ConstraintsJson, _Decoder) ->
    TypeJson = maps:get(<<"type">>, ConstraintsJson, <<"any">>),
    #{
        type => type_from_json(TypeJson)
    }.


-spec type_to_json(type()) -> json_utils:json_term().
type_to_json(integer) -> <<"integer">>;
type_to_json(float) -> <<"float">>;
type_to_json(any) -> <<"any">>.


-spec type_from_json(json_utils:json_term()) -> type().
type_from_json(<<"integer">>) -> integer;
type_from_json(<<"float">>) -> float;
type_from_json(<<"any">>) -> any.
