%%%-------------------------------------------------------------------
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for ?MODULE.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_auto_storage_import_not_supported).

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
to_json(?ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(StorageId, SupportedStorages, SupportedObjectStorages)) ->
    #{
        <<"id">> => ?ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_ID,
        <<"details">> => #{
            <<"storageId">> => StorageId,
            <<"supportedStorages">> => SupportedStorages,
            <<"supportedObjectStorages">> => SupportedObjectStorages
        },
        <<"description">> => ?fmt(
            "Cannot configure auto storage import on storage ~ts - this operation requires any of: ~ts storage with canonical path type and on "
            "object storages (any of: ~ts) it requires blockSize = 0.",
            [StorageId, ?fmt_csv(SupportedStorages), ?fmt_csv(SupportedObjectStorages)]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_ID, <<"details">> := #{
    <<"storageId">> := StorageId,
    <<"supportedStorages">> := SupportedStorages,
    <<"supportedObjectStorages">> := SupportedObjectStorages
}}) ->
    ?ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(StorageId, SupportedStorages, SupportedObjectStorages).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
