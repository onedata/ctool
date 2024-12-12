%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_unauthorized'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_unauthorized).

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
to_json(?ERR_UNAUTHORIZED(ErrorCtx, AuthError)) ->
    {AuthErrorJson, AuthErrorPrint} = case AuthError of
        undefined ->
            {null, <<"no details available.">>};
        _ ->
            AuthErrorJsonTmp = errors:to_json(AuthError),
            AuthErrorPrintTmp = maps:get(<<"description">>, AuthErrorJsonTmp),
            {AuthErrorJsonTmp, AuthErrorPrintTmp}
    end,

    #{
        <<"id">> => ?ERR_UNAUTHORIZED_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"authError">> => AuthErrorJson
        },
        <<"description">> => od_error:format_description(
            "You must authenticate yourself to perform this operation: ~ts",
            [AuthErrorPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_UNAUTHORIZED_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson, #{}),

    AuthError = case maps:get(<<"authError">>, DetailsJson, null) of
        null ->
            undefined;
        AuthErrorJson ->
            errors:from_json(AuthErrorJson)
    end,

    ?ERR_UNAUTHORIZED(ErrorCtx, AuthError).


-spec to_http_code(t()) -> ?HTTP_401_UNAUTHORIZED.
to_http_code(_) ->
    ?HTTP_401_UNAUTHORIZED.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EACCES}.
