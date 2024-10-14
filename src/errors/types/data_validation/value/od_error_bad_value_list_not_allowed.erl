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
-module(od_error_bad_value_list_not_allowed).

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
to_json(?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(Key, AllowedValues)) ->
    #{
        <<"id">> => ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED_ID,
        <<"details">> => #{
            <<"key">> => Key,
            <<"allowed">> => AllowedValues
        },
        <<"description">> => ?fmt(
            "Bad value: provided \"~ts\" must be a list containing zero or more following values: ~ts.",
            [Key, ?fmt_csv(AllowedValues)]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED_ID, <<"details">> := #{
    <<"key">> := Key,
    <<"allowed">> := Allowed
}}) ->
    ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(Key, Allowed).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
