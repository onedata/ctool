%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_bad_value_list_not_allowed'.
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
to_json(?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(Key, Allowed)) ->
    AllowedPrint = ?fmt_csv(Allowed),

    #{
        <<"id">> => ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED_ID,
        <<"details">> => #{
            <<"key">> => Key,
            <<"allowed">> => Allowed
        },
        <<"description">> => ?fmt(
            "Bad value: provided \"~ts\" must be a list containing zero or more following values: ~ts.",
            [Key, AllowedPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    Key = maps:get(<<"key">>, DetailsJson),
    Allowed = maps:get(<<"allowed">>, DetailsJson),

    ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(Key, Allowed).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
