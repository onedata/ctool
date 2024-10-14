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
-module(od_error_unauthorized).

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
to_json(?ERROR_UNAUTHORIZED(AuthError)) ->
    AuthErrorJson = case AuthError of
        undefined -> null;
        _ -> errors:to_json(AuthError)
    end,

    #{
        <<"id">> => ?ERROR_UNAUTHORIZED_ID,
        <<"details">> => #{
            <<"authError">> => AuthErrorJson
        },
        <<"description">> => <<"You must authenticate yourself to perform this operation.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(ErrorJson = #{<<"id">> := ?ERROR_UNAUTHORIZED_ID}) ->
    DetailsJson = maps:get(<<"details">>, ErrorJson, #{}),
    AuthError = case maps:get(<<"authError">>, DetailsJson, null) of
        null -> undefined;
        AuthErrorJson -> errors:from_json(AuthErrorJson)
    end,

    ?ERROR_UNAUTHORIZED(AuthError).


-spec to_http_code(t()) -> 401.
to_http_code(_) ->
    ?HTTP_401_UNAUTHORIZED.
