%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_bad_idp_access_token'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_bad_idp_access_token).

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
to_json(?ERR_BAD_IDP_ACCESS_TOKEN(ErrorCtx, Idp)) ->
    IdpJson = atom_to_binary(Idp, utf8),

    #{
        <<"id">> => ?ERR_BAD_IDP_ACCESS_TOKEN_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"idp">> => IdpJson
        },
        <<"description">> => od_error:format_description(
            "Provided access token for IdP \"~ts\" is not valid.",
            [IdpJson]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_BAD_IDP_ACCESS_TOKEN_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    IdpJson = maps:get(<<"idp">>, DetailsJson),
    Idp = binary_to_existing_atom(IdpJson, utf8),

    ?ERR_BAD_IDP_ACCESS_TOKEN(ErrorCtx, Idp).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.
