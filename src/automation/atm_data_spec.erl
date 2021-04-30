%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Base model for data specs used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_data_spec).
-author("Bartosz Walkowicz").

-behaviour(jsonable_record).

-include("automation/automation.hrl").
-include("logging.hrl").

%% API
-export([get_type/1]).

%% Jsonable record callbacks
-export([version/0, to_json/1, from_json/1]).


-type record() :: #atm_data_spec{}.


-export_type([record/0]).


%%%===================================================================
%%% API functions
%%%===================================================================


-spec get_type(record()) -> atm_data_type:type().
get_type(#atm_data_spec{type = Type}) ->
    Type.


%%%===================================================================
%%% Jsonable record callbacks
%%%===================================================================


-spec version() -> record_json_encoder:model_version().
version() ->
    1.


-spec to_json(record()) -> json_utils:json_map().
to_json(#atm_data_spec{type = Type, value_constraints = ValueConstraints}) ->
    #{
        <<"type">> => atm_data_type:type_to_json(Type),
        <<"valueConstraints">> => atm_data_type:value_constraints_to_json(Type, ValueConstraints)
    }.


-spec from_json(json_utils:json_map()) -> record().
from_json(#{<<"type">> := TypeJson, <<"valueConstraints">> := ValueConstraints}) ->
    Type = atm_data_type:type_from_json(TypeJson),
    #atm_data_spec{
        type = Type,
        value_constraints = atm_data_type:value_constraints_from_json(Type, ValueConstraints)
    }.
