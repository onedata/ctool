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
-module(od_error_token_service_forbidden).

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
to_json(?ERROR_TOKEN_SERVICE_FORBIDDEN(Service)) ->
    #{
        <<"id">> => ?ERROR_TOKEN_SERVICE_FORBIDDEN_ID,
        <<"details">> => #{
            <<"service">> => aai:service_to_json(Service)
        },
        <<"description">> => ?fmt("The service ~ts is forbidden for this subject.", [aai:service_to_printable(Service)])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_TOKEN_SERVICE_FORBIDDEN_ID, <<"details">> := #{<<"service">> := Service}}) ->
    ?ERROR_TOKEN_SERVICE_FORBIDDEN(aai:service_from_json(Service)).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
