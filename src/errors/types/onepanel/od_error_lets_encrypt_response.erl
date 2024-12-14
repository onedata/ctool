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


-type t() :: {error, #od_error{type :: ?MODULE}}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1, to_errno/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERR_LETS_ENCRYPT_RESPONSE(ErrorCtx, ProblemDocument, ErrorMessage)) ->
    ProblemDocumentJson = utils:undefined_to_null(ProblemDocument),

    #{
        <<"id">> => ?ERR_LETS_ENCRYPT_RESPONSE_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"problemDocument">> => ProblemDocumentJson,
            <<"errorMessage">> => ErrorMessage
        },
        <<"description">> => od_error:format_description(
            "Bad Let's Encrypt response: ~ts. See the details for more information.",
            [ErrorMessage]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_LETS_ENCRYPT_RESPONSE_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    ProblemDocument = utils:null_to_undefined(maps:get(<<"problemDocument">>, DetailsJson, null)),
    ErrorMessage = maps:get(<<"errorMessage">>, DetailsJson),

    ?ERR_LETS_ENCRYPT_RESPONSE(ErrorCtx, ProblemDocument, ErrorMessage).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.
