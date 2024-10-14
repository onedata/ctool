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
-module(od_error_token_caveat_unverified).

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
to_json(?ERROR_TOKEN_CAVEAT_UNVERIFIED(Caveat)) ->
    #{
        <<"id">> => ?ERROR_TOKEN_CAVEAT_UNVERIFIED_ID,
        <<"details">> => #{
            <<"caveat">> => caveats:to_json(Caveat)
        },
        <<"description">> => ?fmt(
            "Provided token is not valid - ~ts.",
            [caveats:unverified_description(Caveat)]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_TOKEN_CAVEAT_UNVERIFIED_ID, <<"details">> := #{<<"caveat">> := Caveat}}) ->
    ?ERROR_TOKEN_CAVEAT_UNVERIFIED(caveats:from_json(Caveat)).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
