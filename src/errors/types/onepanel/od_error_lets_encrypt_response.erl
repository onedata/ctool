%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_lets_encrypt_response'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_lets_encrypt_response).

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
to_json(?ERROR_LETS_ENCRYPT_RESPONSE(ProblemDocument, ErrorMessage)) ->
    ProblemDocumentJson = utils:undefined_to_null(ProblemDocument),

    #{
        <<"id">> => ?ERROR_LETS_ENCRYPT_RESPONSE_ID,
        <<"details">> => #{
            <<"problemDocument">> => ProblemDocumentJson,
            <<"errorMessage">> => ErrorMessage
        },
        <<"description">> => ?fmt(
            "Bad Let's Encrypt response: ~ts.",
            [ErrorMessage]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_LETS_ENCRYPT_RESPONSE_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    ProblemDocument = utils:null_to_undefined(maps:get(<<"problemDocument">>, DetailsJson, null)),
    ErrorMessage = maps:get(<<"errorMessage">>, DetailsJson),

    ?ERROR_LETS_ENCRYPT_RESPONSE(ProblemDocument, ErrorMessage).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
