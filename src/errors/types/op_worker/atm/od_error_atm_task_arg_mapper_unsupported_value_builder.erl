%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_atm_task_arg_mapper_unsupported_value_builder'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_atm_task_arg_mapper_unsupported_value_builder).

-behaviour(od_error).

-include("errors.hrl").
-include("http/codes.hrl").


-type t() :: #od_error{type :: ?MODULE}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1, to_errno/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_MATCH(Type, Supported)) ->
    TypeJson = atm_task_argument_value_builder:type_to_json(Type),
    SupportedJson = lists:map(fun atm_task_argument_value_builder:type_to_json/1, Supported),
    SupportedPrint = od_error:format_csv(SupportedJson),

    #{
        <<"id">> => ?ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_ID,
        <<"details">> => #{
            <<"type">> => TypeJson,
            <<"supported">> => SupportedJson
        },
        <<"description">> => od_error:format_description(
            "Bad automation task argument value builder: type \"~ts\" not supported - must be one of: ~ts.",
            [TypeJson, SupportedPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    TypeJson = maps:get(<<"type">>, DetailsJson),
    Type = atm_task_argument_value_builder:type_from_json(TypeJson),
    SupportedJson = maps:get(<<"supported">>, DetailsJson),
    Supported = lists:map(fun atm_task_argument_value_builder:type_from_json/1, SupportedJson),

    ?new_ERROR_ATM_TASK_ARG_MAPPER_UNSUPPORTED_VALUE_BUILDER(Type, Supported).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.
