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
-module(od_error_invite_token_consumer_invalid).

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
to_json(?ERROR_INVITE_TOKEN_CONSUMER_INVALID(Consumer)) ->
    #{
        <<"id">> => ?ERROR_INVITE_TOKEN_CONSUMER_INVALID_ID,
        <<"details">> => #{
            <<"consumer">> => aai:subject_to_json(Consumer)
        },
        <<"description">> => ?fmt(
            "The consumer '~ts' is invalid for this type of invite token.",
            [aai:subject_to_printable(Consumer)]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_INVITE_TOKEN_CONSUMER_INVALID_ID, <<"details">> := #{<<"consumer">> := Consumer}}) ->
    ?ERROR_INVITE_TOKEN_CONSUMER_INVALID(aai:subject_from_json(Consumer)).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
