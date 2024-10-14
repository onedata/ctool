%%%-------------------------------------------------------------------
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for ?MODULE.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_atm_task_arg_mapper_unsupported_value_builder).

-behaviour(od_error).

-include("errors.hrl").
-include("http/codes.hrl").


-type t() :: #od_error{type :: ?MODULE}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(Type, SupportedTypes)) ->
    TypeJson = atm_task_argument_value_builder:type_to_json(Type),
    SupportedTypesJson = lists:map(fun atm_task_argument_value_builder:type_to_json/1, SupportedTypes),

    #{
        <<"id">> => ?ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_ID,
        <<"details">> => #{
            <<"type">> => TypeJson,
            <<"supported">> => SupportedTypesJson
        },
        <<"description">> => ?fmt(
            "Bad automation task argument value builder: type \"~ts\" not supported - must be one of: ~ts.",
            [TypeJson, ?fmt_csv(SupportedTypesJson)]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{
    <<"id">> := ?ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_ID,
    <<"details">> := #{
        <<"type">> := TypeJson,
        <<"supported">> := SupportedTypesJson
    }
}) ->
    Type = atm_task_argument_value_builder:type_from_json(TypeJson),
    SupportedTypes = lists:map(fun atm_task_argument_value_builder:type_from_json/1, SupportedTypesJson),

    ?ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(Type, SupportedTypes).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
