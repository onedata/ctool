%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_bad_data'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_bad_data).

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
to_json(?ERR_BAD_DATA(ErrorCtx, Key, ?ERR = SpecificError)) ->
    #{
        <<"id">> => ?ERR_BAD_DATA_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"key">> => Key,
            <<"specificError">> => errors:to_json(SpecificError)
        },
        <<"description">> => od_error:format_description("Bad value provided for \"~ts\" (see details).", [Key])
    };
% TODO VFS-12486 Remove hint variant
to_json(?ERR_BAD_DATA(ErrorCtx, Key, HumanReadableHint)) ->
    HumanReadableHintJson = utils:undefined_to_null(HumanReadableHint),

    #{
        <<"id">> => ?ERR_BAD_DATA_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"key">> => Key,
            <<"hint">> => HumanReadableHintJson
        },
        <<"description">> => od_error:format_description("Bad value provided for \"~ts\": ~ts.", [Key, HumanReadableHintJson])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(ErrorJson = #{<<"id">> := ?ERR_BAD_DATA_ID, <<"details">> := #{
    <<"key">> := Key,
    <<"specificError">> := SpecificError
}}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, ErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    ?ERR_BAD_DATA(ErrorCtx, Key, errors:from_json(SpecificError));

% TODO VFS-12486 Remove hint variant
from_json(ErrorJson = #{<<"id">> := ?ERR_BAD_DATA_ID, <<"details">> := DetailsJson}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, ErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    Key = maps:get(<<"key">>, DetailsJson),
    HumanReadableHint = utils:null_to_undefined(maps:get(<<"hint">>, DetailsJson, null)),

    ?ERR_BAD_DATA(ErrorCtx, Key, HumanReadableHint).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.
