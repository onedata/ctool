%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_file_access'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_file_access).

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
to_json(?ERR_FILE_ACCESS(ErrorCtx, Path, Errno)) ->
    PathJson = str_utils:to_binary(filename:flatten(Path)),
    ErrnoJson = erlang:atom_to_binary(Errno, utf8),

    #{
        <<"id">> => ?ERR_FILE_ACCESS_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"path">> => PathJson,
            <<"errno">> => ErrnoJson
        },
        <<"description">> => od_error:format_description(
            "Cannot access file \"~ts\": ~ts.",
            [PathJson, ErrnoJson]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_FILE_ACCESS_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Path = maps:get(<<"path">>, DetailsJson),
    ErrnoJson = maps:get(<<"errno">>, DetailsJson),
    Errno = erlang:binary_to_existing_atom(ErrnoJson, utf8),

    ?ERR_FILE_ACCESS(ErrorCtx, Path, Errno).


-spec to_http_code(t()) -> ?HTTP_500_INTERNAL_SERVER_ERROR.
to_http_code(_) ->
    ?HTTP_500_INTERNAL_SERVER_ERROR.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EIO}.
