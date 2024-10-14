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
-module(od_error_invite_token_target_id_invalid).

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
to_json(?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(Id)) ->
    #{
        <<"id">> => ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID_ID,
        <<"details">> => #{
            <<"id">> => Id
        },
        <<"description">> => ?fmt("The target id '~ts' is invalid for this type of invite token.", [Id])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID_ID, <<"details">> := #{<<"id">> := Id}}) ->
    ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(Id).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
