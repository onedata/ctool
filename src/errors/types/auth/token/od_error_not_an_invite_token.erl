%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_not_an_invite_token'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_not_an_invite_token).

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
to_json(?ERROR_NOT_AN_INVITE_TOKEN_MATCH(ExpectedInviteType, Received)) ->
    ExpectedInviteTypeJson = case ExpectedInviteType of
        any ->
            <<"any">>;
        _ ->
            token_type:invite_type_to_str(ExpectedInviteType)
    end,
    ReceivedJson = token_type:to_json(Received),
    ReceivedPrint = token_type:to_printable(Received),

    #{
        <<"id">> => ?ERROR_NOT_AN_INVITE_TOKEN_ID,
        <<"details">> => #{
            <<"expectedInviteType">> => ExpectedInviteTypeJson,
            <<"received">> => ReceivedJson
        },
        <<"description">> => od_error:format_description(
            "Expected an invitation token of type '~ts', but received a(n) ~ts.",
            [ExpectedInviteType, ReceivedPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_NOT_AN_INVITE_TOKEN_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    ExpectedInviteTypeJson = maps:get(<<"expectedInviteType">>, DetailsJson),
    ExpectedInviteType = case ExpectedInviteTypeJson of
        <<"any">> ->
            any;
        _ ->
            token_type:invite_type_from_str(ExpectedInviteTypeJson)
    end,
    ReceivedJson = maps:get(<<"received">>, DetailsJson),
    Received = token_type:from_json(ReceivedJson),

    ?new_ERROR_NOT_AN_INVITE_TOKEN(ExpectedInviteType, Received).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.
