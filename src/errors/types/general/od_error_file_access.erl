%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
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


-type t() :: #od_error{type :: ?MODULE}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1, to_errno/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERROR_FILE_ACCESS_MATCH(Path, Errno)) ->
    PathJson = str_utils:to_binary(filename:flatten(Path)),
    ErrnoJson = atom_to_binary(Errno, utf8),

    #{
        <<"id">> => ?ERROR_FILE_ACCESS_ID,
        <<"details">> => #{
            <<"path">> => PathJson,
            <<"errno">> => ErrnoJson
        },
        <<"description">> => od_error:format_description(
            "Cannot access file \"~ts\": ~ts.",
            [PathJson, Errno]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_FILE_ACCESS_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Path = maps:get(<<"path">>, DetailsJson),
    ErrnoJson = maps:get(<<"errno">>, DetailsJson),
    Errno = binary_to_existing_atom(ErrnoJson, utf8),

    ?new_ERROR_FILE_ACCESS(Path, Errno).


-spec to_http_code(t()) -> ?HTTP_500_INTERNAL_SERVER_ERROR.
to_http_code(_) ->
    ?HTTP_500_INTERNAL_SERVER_ERROR.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EIO}.
