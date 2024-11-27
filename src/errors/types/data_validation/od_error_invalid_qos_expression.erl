%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_invalid_qos_expression'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_invalid_qos_expression).

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
to_json(?ERROR_INVALID_QOS_EXPRESSION_MATCH(Reason)) ->
    #{
        <<"id">> => ?ERROR_INVALID_QOS_EXPRESSION_ID,
        <<"details">> => #{
            <<"reason">> => Reason
        },
        <<"description">> => od_error:format_description(
            "Invalid QoS expression: ~ts.",
            [Reason]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_INVALID_QOS_EXPRESSION_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Reason = maps:get(<<"reason">>, DetailsJson),

    ?new_ERROR_INVALID_QOS_EXPRESSION(Reason).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.
