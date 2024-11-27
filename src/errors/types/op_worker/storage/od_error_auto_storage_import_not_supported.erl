%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_auto_storage_import_not_supported'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_auto_storage_import_not_supported).

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
to_json(?ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_MATCH(StorageId, SupportedStorages, SupportedObjectStorages)) ->
    SupportedStoragesPrint = od_error:format_csv(SupportedStorages),
    SupportedObjectStoragesPrint = od_error:format_csv(SupportedObjectStorages),

    #{
        <<"id">> => ?ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_ID,
        <<"details">> => #{
            <<"storageId">> => StorageId,
            <<"supportedStorages">> => SupportedStorages,
            <<"supportedObjectStorages">> => SupportedObjectStorages
        },
        <<"description">> => od_error:format_description(
            "Cannot configure auto storage import on storage ~ts - this operation requires any of: ~ts storage with canonical path type and on object storages (any of: ~ts) it requires blockSize = 0.",
            [StorageId, SupportedStoragesPrint, SupportedObjectStoragesPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    StorageId = maps:get(<<"storageId">>, DetailsJson),
    SupportedStorages = maps:get(<<"supportedStorages">>, DetailsJson),
    SupportedObjectStorages = maps:get(<<"supportedObjectStorages">>, DetailsJson),

    ?new_ERROR_AUTO_STORAGE_IMPORT_NOT_SUPPORTED(StorageId, SupportedStorages, SupportedObjectStorages).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.
