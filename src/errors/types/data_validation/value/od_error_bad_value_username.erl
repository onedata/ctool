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
-module(od_error_bad_value_username).

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
to_json(?ERROR_BAD_VALUE_USERNAME) ->
    #{
        <<"id">> => ?ERROR_BAD_VALUE_USERNAME_ID,
        <<"description">> => <<"Bad value: ", (?USERNAME_REQUIREMENTS_DESCRIPTION)/binary>>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_BAD_VALUE_USERNAME_ID}) ->
    ?ERROR_BAD_VALUE_USERNAME.


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
