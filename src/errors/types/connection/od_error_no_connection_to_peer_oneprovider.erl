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
-module(od_error_no_connection_to_peer_oneprovider).

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
to_json(?ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER) ->
    #{
        <<"id">> => ?ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER_ID,
        <<"description">> => <<"No connection to peer Oneprovider.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER_ID}) ->
    ?ERROR_NO_CONNECTION_TO_PEER_ONEPROVIDER.


-spec to_http_code(t()) -> 503.
to_http_code(_) ->
    ?HTTP_503_SERVICE_UNAVAILABLE.
