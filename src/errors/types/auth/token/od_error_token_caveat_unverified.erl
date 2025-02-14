%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_token_caveat_unverified'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_token_caveat_unverified).

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
to_json(?ERR_TOKEN_CAVEAT_UNVERIFIED(ErrorCtx, Caveat)) ->
    CaveatJson = caveats:to_json(Caveat),
    CaveatPrint = caveats:unverified_description(Caveat),

    #{
        <<"id">> => ?ERR_TOKEN_CAVEAT_UNVERIFIED_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"caveat">> => CaveatJson
        },
        <<"description">> => od_error:format_description(
            "Provided token is not valid: ~ts.",
            [CaveatPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_TOKEN_CAVEAT_UNVERIFIED_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    CaveatJson = maps:get(<<"caveat">>, DetailsJson),
    Caveat = caveats:from_json(CaveatJson),

    ?ERR_TOKEN_CAVEAT_UNVERIFIED(ErrorCtx, Caveat).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.
