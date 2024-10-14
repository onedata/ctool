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
-module(od_error_bad_value_name).

-behaviour(od_error).

-include("errors.hrl").
-include("http/codes.hrl").
-include("validation.hrl").


-type t() :: #od_error{type :: ?MODULE}.

-export_type([t/0]).

%% od_error callbacks
-export([to_json/1, from_json/1, to_http_code/1]).


%%%===================================================================
%%% od_error callbacks
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(?ERROR_BAD_VALUE_NAME(Key)) ->
    KeyJson = utils:undefined_to_null(Key),

    #{
        <<"id">> => <<"badValueName">>,
        <<"details">> => #{<<"key">> => KeyJson},
        <<"description">> => ?fmt("Bad value provided for \"~ts\": ~ts", [KeyJson, ?NAME_REQUIREMENTS_DESCRIPTION])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(ErrorJson = #{<<"id">> := <<"badValueName">>}) ->
    DetailsJson = maps:get(<<"details">>, ErrorJson, #{}),
    Key = utils:null_to_undefined(maps:get(<<"key">>, DetailsJson, null)),

    ?ERROR_BAD_VALUE_NAME(Key).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
