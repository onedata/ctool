%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_bad_message'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_bad_message).

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
to_json(?ERROR_BAD_MESSAGE_MATCH(Message)) ->
    #{
        <<"id">> => ?ERROR_BAD_MESSAGE_ID,
        <<"details">> => #{
            <<"message">> => Message
        },
        <<"description">> => <<"This message could not be understood by the server.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_BAD_MESSAGE_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Message = maps:get(<<"message">>, DetailsJson),

    ?new_ERROR_BAD_MESSAGE(Message).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.
