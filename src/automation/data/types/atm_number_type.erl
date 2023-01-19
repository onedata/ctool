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
    #{
        <<"type">> => type_to_json(maps:get(type, Constraints, any)),
        <<"allowedValues">> => utils:undefined_to_null(maps:get(allowed_values, Constraints, undefined))
    }.


-spec decode_value_constraints(
    jsonable_record:validation_strategy(),
    json_utils:json_term(),
    persistent_record:nested_record_decoder()
) ->
    atm_data_type:value_constraints().
decode_value_constraints(skip_validation, ConstraintsJson, _Decoder) ->
    #{
        type => type_from_json(maps:get(<<"type">>, ConstraintsJson, <<"any">>)),
        allowed_values => utils:null_to_undefined(maps:get(<<"allowedValues">>, ConstraintsJson, null))
    };
decode_value_constraints(validate, ConstraintsJson, _Decoder) ->
    Constraints = decode_value_constraints(skip_validation, ConstraintsJson, _Decoder),
    AllowedValues = maps:get(allowed_values, Constraints),
    IsValid = case AllowedValues of
        undefined -> true;
        List when is_list(List) -> lists:all(fun is_instance/1, AllowedValues);
        _ -> false
    end,
    IsValid orelse throw(?ERROR_BAD_DATA(<<"valueConstraints">>, <<"You must provide a list of numbers">>)),
    Constraints.


-spec type_to_json(type()) -> json_utils:json_term().
type_to_json(integer) -> <<"integer">>;
type_to_json(float) -> <<"float">>;
type_to_json(any) -> <<"any">>.


-spec type_from_json(json_utils:json_term()) -> type().
type_from_json(<<"integer">>) -> integer;
type_from_json(<<"float">>) -> float;
type_from_json(<<"any">>) -> any.
