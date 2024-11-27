%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_bad_consumer_token'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_bad_consumer_token).

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
to_json(?ERROR_BAD_CONSUMER_TOKEN_MATCH(TokenError)) ->
    TokenErrorJson = errors:to_json(TokenError),
    TokenErrorPrint = maps:get(<<"description">>, TokenErrorJson),

    #{
        <<"id">> => ?ERROR_BAD_CONSUMER_TOKEN_ID,
        <<"details">> => #{
            <<"tokenError">> => TokenErrorJson
        },
        <<"description">> => od_error:format_description(
            "Provided consumer token is not valid. ~ts",
            [TokenErrorPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_BAD_CONSUMER_TOKEN_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    TokenErrorJson = maps:get(<<"tokenError">>, DetailsJson),
    TokenError = errors:from_json(TokenErrorJson),

    ?new_ERROR_BAD_CONSUMER_TOKEN(TokenError).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.
