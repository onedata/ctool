%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_atm_task_arg_mapper_iterated_item_query_failed'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_atm_task_arg_mapper_iterated_item_query_failed).

-behaviour(od_error).

-include("errors.hrl").
-include("http/codes.hrl").


-type t() :: {error, #od_error{type :: ?MODULE}}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1, to_errno/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED(ErrorCtx, Value, Query)) ->
    #{
        <<"id">> => ?ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"value">> => Value,
            <<"query">> => Query
        },
        <<"description">> => od_error:format_description(
            "Failed to perform a query on the iterated item: \"~ts\".",
            [Query]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Value = maps:get(<<"value">>, DetailsJson),
    Query = maps:get(<<"query">>, DetailsJson),

    ?ERR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED(ErrorCtx, Value, Query).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.
