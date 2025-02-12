%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_dns_servers_unreachable'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_dns_servers_unreachable).

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
to_json(?ERR_DNS_SERVERS_UNREACHABLE(ErrorCtx, Servers)) ->
    ServersJson = lists:map(fun
        (default) ->
            <<"system defaults">>;
        (Ip) ->
            element(2, {ok, _} = ip_utils:to_binary(Ip))
    end, Servers),
    ServersPrint = od_error:format_csv(ServersJson),

    #{
        <<"id">> => ?ERR_DNS_SERVERS_UNREACHABLE_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"servers">> => ServersJson
        },
        <<"description">> => od_error:format_description(
            "Error fetching DNS records. Used servers: ~ts.",
            [ServersPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_DNS_SERVERS_UNREACHABLE_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    ServersJson = maps:get(<<"servers">>, DetailsJson),
    Servers = lists:map(fun
        (<<"system defaults">>) ->
            default;
        (Ip) ->
            element(2, {ok, _} = ip_utils:to_ip4_address(Ip))
    end, ServersJson),

    ?ERR_DNS_SERVERS_UNREACHABLE(ErrorCtx, Servers).


-spec to_http_code(t()) -> ?HTTP_503_SERVICE_UNAVAILABLE.
to_http_code(_) ->
    ?HTTP_503_SERVICE_UNAVAILABLE.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.
