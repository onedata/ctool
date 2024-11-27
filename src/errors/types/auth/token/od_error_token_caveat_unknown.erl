%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_token_caveat_unknown'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_token_caveat_unknown).

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
to_json(?ERROR_TOKEN_CAVEAT_UNKNOWN_MATCH(Caveat)) ->
    #{
        <<"id">> => ?ERROR_TOKEN_CAVEAT_UNKNOWN_ID,
        <<"details">> => #{
            <<"caveat">> => Caveat
        },
        <<"description">> => od_error:format_description(
            "Unknown caveat - '~ts'.",
            [Caveat]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_TOKEN_CAVEAT_UNKNOWN_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Caveat = maps:get(<<"caveat">>, DetailsJson),

    ?new_ERROR_TOKEN_CAVEAT_UNKNOWN(Caveat).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.
