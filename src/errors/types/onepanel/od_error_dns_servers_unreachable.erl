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
-module(od_error_dns_servers_unreachable).

-behaviour(od_error).

-include("errors.hrl").
-include("http/codes.hrl").

-type t() :: #od_error{type :: ?MODULE}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1]).

-define(DNS_DEFAULTS, <<"system defaults">>).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERROR_DNS_SERVERS_UNREACHABLE(UsedServers)) ->
    Servers = lists:map(fun
        (default) -> ?DNS_DEFAULTS;
        (IP) -> element(2, {ok, _} = ip_utils:to_binary(IP))
    end, UsedServers),

    #{
        <<"id">> => ?ERROR_DNS_SERVERS_UNREACHABLE_ID,
        <<"details">> => #{
            <<"servers">> => Servers
        },
        <<"description">> => ?fmt("Error fetching DNS records. Used servers: ~ts.",
            [?fmt_csv(Servers)])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_DNS_SERVERS_UNREACHABLE_ID, <<"details">> := #{
    <<"servers">> := UsedServers
}}) ->
    Servers = lists:map(fun
        (?DNS_DEFAULTS) -> default;
        (IP) -> element(2, {ok, _} = ip_utils:to_ip4_address(IP))
    end, UsedServers),

    ?ERROR_DNS_SERVERS_UNREACHABLE(Servers).


-spec to_http_code(t()) -> 503.
to_http_code(_) ->
    ?HTTP_503_SERVICE_UNAVAILABLE.
