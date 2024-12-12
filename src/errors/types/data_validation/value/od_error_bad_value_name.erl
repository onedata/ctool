%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_bad_value_name'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_bad_value_name).

-behaviour(od_error).

-include("errors.hrl").
-include("http/codes.hrl").
-include("validation.hrl").

-type t() :: {error, #od_error{type :: ?MODULE}}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1, to_errno/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERR_BAD_VALUE_NAME(ErrorCtx, Key)) ->
    KeyJson = utils:undefined_to_null(Key),

    #{
        <<"id">> => ?ERR_BAD_VALUE_NAME_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"key">> => KeyJson
        },
        <<"description">> => od_error:format_description(
            "Bad value provided for \"~ts\": ~ts",
            [KeyJson, ?NAME_REQUIREMENTS_DESCRIPTION]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_BAD_VALUE_NAME_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson, #{}),

    Key = utils:null_to_undefined(maps:get(<<"key">>, DetailsJson, null)),

    ?ERR_BAD_VALUE_NAME(ErrorCtx, Key).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.
