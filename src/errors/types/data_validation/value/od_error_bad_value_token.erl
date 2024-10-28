%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_bad_value_token'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_bad_value_token).

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
to_json(?ERROR_BAD_VALUE_TOKEN(Key, TokenError)) ->
    TokenErrorJson = errors:to_json(TokenError),

    #{
        <<"id">> => ?ERROR_BAD_VALUE_TOKEN_ID,
        <<"details">> => #{
            <<"key">> => Key,
            <<"tokenError">> => TokenErrorJson
        },
        <<"description">> => ?fmt(
            "Bad value: provided \"~ts\" is not a valid token (see details).",
            [Key]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_BAD_VALUE_TOKEN_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Key = maps:get(<<"key">>, DetailsJson),
    TokenErrorJson = maps:get(<<"tokenError">>, DetailsJson),
    TokenError = errors:from_json(TokenErrorJson),

    ?ERROR_BAD_VALUE_TOKEN(Key, TokenError).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
