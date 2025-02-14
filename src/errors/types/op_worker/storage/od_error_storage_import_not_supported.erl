%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_storage_import_not_supported'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_storage_import_not_supported).

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
to_json(?ERR_STORAGE_IMPORT_NOT_SUPPORTED(ErrorCtx, StorageId, ObjectStorages)) ->
    ObjectStoragesPrint = od_error:format_csv(ObjectStorages),

    #{
        <<"id">> => ?ERR_STORAGE_IMPORT_NOT_SUPPORTED_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"storageId">> => StorageId,
            <<"objectStorages">> => ObjectStorages
        },
        <<"description">> => od_error:format_description(
            "Cannot configure storage import for the storage backend \"~ts\".  This operation requires a storage backend with canonical path type and  for object storage (any of: ~ts), it requires the block size set to 0.",
            [StorageId, ObjectStoragesPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_STORAGE_IMPORT_NOT_SUPPORTED_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    StorageId = maps:get(<<"storageId">>, DetailsJson),
    ObjectStorages = maps:get(<<"objectStorages">>, DetailsJson),

    ?ERR_STORAGE_IMPORT_NOT_SUPPORTED(ErrorCtx, StorageId, ObjectStorages).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?ENOTSUP}.
