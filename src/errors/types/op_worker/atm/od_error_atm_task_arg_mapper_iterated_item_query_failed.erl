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


-type t() :: #od_error{type :: ?MODULE}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED(Value, Query)) ->
    #{
        <<"id">> => ?ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_ID,
        <<"details">> => #{
            <<"value">> => Value,
            <<"query">> => Query
        },
        <<"description">> => <<"Failed to perform query on iterated item (see details).">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Value = maps:get(<<"value">>, DetailsJson),
    Query = maps:get(<<"query">>, DetailsJson),

    ?ERROR_ATM_TASK_ARG_MAPPER_ITERATED_ITEM_QUERY_FAILED(Value, Query).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
