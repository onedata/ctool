%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_transfer_already_ended'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_transfer_already_ended).

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
to_json(?ERROR_TRANSFER_ALREADY_ENDED) ->
    #{
        <<"id">> => ?ERROR_TRANSFER_ALREADY_ENDED_ID,
        <<"description">> => <<"Specified transfer has already ended.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_TRANSFER_ALREADY_ENDED_ID}) ->
    ?ERROR_TRANSFER_ALREADY_ENDED.


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
