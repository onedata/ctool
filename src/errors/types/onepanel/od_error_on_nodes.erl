%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_on_nodes'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_on_nodes).

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
to_json(?ERR_ON_NODES(ErrorCtx, Error, Hostnames)) ->
    ErrorJson = errors:to_json(Error),
    ErrorPrint = maps:get(<<"description">>, ErrorJson),
    HostnamesPrint = od_error:format_csv(Hostnames),

    #{
        <<"id">> => ?ERR_ON_NODES_ID,
        <<"ctx">> => od_error:ctx_to_json(ErrorCtx),
        <<"details">> => #{
            <<"error">> => ErrorJson,
            <<"hostnames">> => Hostnames
        },
        <<"description">> => od_error:format_description(
            "Error on nodes ~ts: ~ts",
            [HostnamesPrint, ErrorPrint]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERR_ON_NODES_ID}) ->
    ErrorCtxJson = maps:get(<<"ctx">>, OdErrorJson, #{}),
    ErrorCtx = od_error:ctx_from_json(ErrorCtxJson),

    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    ErrorJson = maps:get(<<"error">>, DetailsJson),
    Error = errors:from_json(ErrorJson),
    Hostnames = maps:get(<<"hostnames">>, DetailsJson),

    ?ERR_ON_NODES(ErrorCtx, Error, Hostnames).


-spec to_http_code(t()) -> od_error:http_code().
to_http_code(?ERR_ON_NODES(Error, _)) ->
    errors:to_http_code(Error).



-spec to_errno(t()) -> false.
to_errno(_) ->
    false.
