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
-module(od_error_atm_task_arg_mapper_for_nonexistent_lambda_arg).

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
to_json(?ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(ArgName)) ->
    #{
        <<"id">> => ?ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG_ID,
        <<"details">> => #{
            <<"argument">> => ArgName
        },
        <<"description">> => ?fmt(
            "Found excessive argument mapper for nonexistent lambda argument: ~ts.",
            [ArgName]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{
    <<"id">> := ?ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG_ID,
    <<"details">> := #{
        <<"argument">> := ArgName
    }
}) ->
    ?ERROR_ATM_TASK_ARG_MAPPER_FOR_NONEXISTENT_LAMBDA_ARG(ArgName).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
