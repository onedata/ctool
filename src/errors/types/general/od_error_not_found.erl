%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_not_found'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_not_found).

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
to_json(?ERR_NOT_FOUND(ErrorCtx)) ->
    #{
        <<"id">> => ?ERR_NOT_FOUND_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"description">> => <<"The requested resource could not be found.">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_NOT_FOUND_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),
    ?ERR_NOT_FOUND(ErrorCtx).


-spec to_http_code(t()) -> ?HTTP_404_NOT_FOUND.
to_http_code(_) ->
    ?HTTP_404_NOT_FOUND.


-spec to_errno(t()) -> {true, od_error:errno()}.
to_errno(_) ->
    {true, ?EINVAL}.
