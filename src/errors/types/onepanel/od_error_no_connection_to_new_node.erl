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
-module(od_error_no_connection_to_new_node).

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
to_json(?ERROR_NO_CONNECTION_TO_NEW_NODE(Hostname)) ->
    #{
        <<"id">> => ?ERROR_NO_CONNECTION_TO_NEW_NODE_ID,
        <<"details">> => #{<<"hostname">> => Hostname},
        <<"description">> => ?fmt("Cannot add node \"~ts\", connection failed.", [Hostname])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_NO_CONNECTION_TO_NEW_NODE_ID, <<"details">> := #{
    <<"hostname">> := Hostname
}}) ->
    ?ERROR_NO_CONNECTION_TO_NEW_NODE(Hostname).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
