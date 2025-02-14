%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_no_connection_to_new_node'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_no_connection_to_new_node).

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
to_json(?ERR_NO_CONNECTION_TO_NEW_NODE(ErrorCtx, Hostname)) ->
    #{
        <<"id">> => ?ERR_NO_CONNECTION_TO_NEW_NODE_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"hostname">> => Hostname
        },
        <<"description">> => od_error:format_description(
            "Cannot add node \"~ts\", connection failed.",
            [Hostname]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_NO_CONNECTION_TO_NEW_NODE_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Hostname = maps:get(<<"hostname">>, DetailsJson),

    ?ERR_NO_CONNECTION_TO_NEW_NODE(ErrorCtx, Hostname).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.
