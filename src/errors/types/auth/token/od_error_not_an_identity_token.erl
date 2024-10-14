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
-module(od_error_not_an_identity_token).

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
to_json(?ERROR_NOT_AN_IDENTITY_TOKEN(ReceivedTokenType)) ->
    #{
        <<"id">> => ?ERROR_NOT_AN_IDENTITY_TOKEN_ID,
        <<"details">> => #{
            <<"received">> => token_type:to_json(ReceivedTokenType)
        },
        <<"description">> => ?fmt(
            "Expected an identity token, but received a(n) ~ts.",
            [token_type:to_printable(ReceivedTokenType)]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_NOT_AN_IDENTITY_TOKEN_ID, <<"details">> := #{<<"received">> := ReceivedTokenType}}) ->
    ?ERROR_NOT_AN_IDENTITY_TOKEN(token_type:from_json(ReceivedTokenType)).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
