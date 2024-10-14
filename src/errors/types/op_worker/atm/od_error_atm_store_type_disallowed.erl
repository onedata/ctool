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
-module(od_error_atm_store_type_disallowed).

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
to_json(?ERROR_ATM_STORE_TYPE_DISALLOWED(AtmStoreSchemaId, AllowedTypes)) ->
    AllowedTypesJson = lists:map(fun automation:store_type_to_json/1, AllowedTypes),

    #{
        <<"id">> => ?ERROR_ATM_STORE_TYPE_DISALLOWED_ID,
        <<"details">> => #{
            <<"atmStoreSchemaId">> => AtmStoreSchemaId,
            <<"allowed">> => AllowedTypesJson
        },
        <<"description">> => ?fmt(
            "Bad automation store: the type of store (schema id: \"~ts\") must be one of: ~ts.",
            [AtmStoreSchemaId, ?fmt_csv(AllowedTypesJson)]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{
    <<"id">> := ?ERROR_ATM_STORE_TYPE_DISALLOWED_ID,
    <<"details">> := #{
        <<"atmStoreSchemaId">> := AtmStoreSchemaId,
        <<"allowed">> := AllowedTypesJson
    }
}) ->
    AllowedTypes = lists:map(fun automation:store_type_from_json/1, AllowedTypesJson),
    ?ERROR_ATM_STORE_TYPE_DISALLOWED(AtmStoreSchemaId, AllowedTypes).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
