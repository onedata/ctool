%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_space_already_supported_with_imported_storage'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_space_already_supported_with_imported_storage).

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
to_json(?ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(ErrorCtx, SpaceId, StorageId)) ->
    #{
        <<"id">> => ?ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"spaceId">> => SpaceId,
            <<"storageId">> => StorageId
        },
        <<"description">> => od_error:format_description(
            "Space \"~ts\" is already supported with an imported storage backend (\"~ts\"). A space can be supported by no more than one imported storage backend.",
            [SpaceId, StorageId]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    SpaceId = maps:get(<<"spaceId">>, DetailsJson),
    StorageId = maps:get(<<"storageId">>, DetailsJson),

    ?ERR_SPACE_ALREADY_SUPPORTED_WITH_IMPORTED_STORAGE(ErrorCtx, SpaceId, StorageId).


-spec to_http_code(t()) -> ?HTTP_409_CONFLICT.
to_http_code(_) ->
    ?HTTP_409_CONFLICT.


-spec to_errno(t()) -> false.
to_errno(_) ->
    false.
