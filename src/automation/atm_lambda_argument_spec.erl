%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Base model for lambda argument specification used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lambda_argument_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).

-include("automation/automation.hrl").
-include("logging.hrl").

%% Jsonable record callbacks
-export([version/0, to_json/1, from_json/1]).


-type record() :: #atm_lambda_argument_spec{}.


-export_type([record/0]).


%%%===================================================================
%%% Jsonable record callbacks
%%%===================================================================


-spec version() -> record_json_encoder:model_version().
version() ->
    1.


-spec to_json(record()) -> json_utils:json_map().
to_json(Spec) ->
    #{
        <<"name">> => Spec#atm_lambda_argument_spec.name,
        <<"dataSpec">> => atm_data_spec:to_json(Spec#atm_lambda_argument_spec.data_spec),
        <<"isArray">> => Spec#atm_lambda_argument_spec.is_array,
        <<"isOptional">> => Spec#atm_lambda_argument_spec.is_optional,
        <<"defaultValue">> => Spec#atm_lambda_argument_spec.default_value
    }.


-spec from_json(json_utils:json_map()) -> record().
from_json(SpecJson) ->
    #atm_lambda_argument_spec{
        name = maps:get(<<"name">>, SpecJson),
        data_spec = atm_data_spec:from_json(maps:get(<<"dataSpec">>, SpecJson)),
        is_array = maps:get(<<"isArray">>, SpecJson),
        is_optional = maps:get(<<"isOptional">>, SpecJson),
        default_value = maps:get(<<"defaultValue">>, SpecJson)
    }.
