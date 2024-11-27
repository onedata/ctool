%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_invite_token_consumer_invalid'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_invite_token_consumer_invalid).

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
to_json(?ERROR_INVITE_TOKEN_CONSUMER_INVALID_MATCH(Consumer)) ->
    ConsumerJson = aai:subject_to_json(Consumer),
    ConsumerPrint = aai:subject_to_printable(Consumer),

    #{
        <<"id">> => ?ERROR_INVITE_TOKEN_CONSUMER_INVALID_ID,
        <<"details">> => #{
            <<"consumer">> => ConsumerJson
        },
        <<"description">> => od_error:format_description(
            "The consumer '~ts' is invalid for this type of invite token.",
            [ConsumerPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_INVITE_TOKEN_CONSUMER_INVALID_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    ConsumerJson = maps:get(<<"consumer">>, DetailsJson),
    Consumer = aai:subject_from_json(ConsumerJson),

    ?new_ERROR_INVITE_TOKEN_CONSUMER_INVALID(Consumer).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.
