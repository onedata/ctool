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
-module(od_error_on_nodes).

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
to_json(?ERROR_ON_NODES(Error, Hostnames)) ->
    #{<<"description">> := Description} = InnerError = errors:to_json(Error),

    #{
        <<"id">> => ?ERROR_ON_NODES_ID,
        <<"details">> => #{
            <<"error">> => InnerError,
            <<"hostnames">> => Hostnames
        },
        <<"description">> => ?fmt("Error on nodes ~ts: ~ts",
            [?fmt_csv(Hostnames), Description])
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{<<"id">> := ?ERROR_ON_NODES_ID, <<"details">> := #{
    <<"error">> := Error,
    <<"hostnames">> := Hostnames
}}) ->
    ?ERROR_ON_NODES(errors:from_json(Error), Hostnames).


% TODO a jednak to_http_code jest kontekstowe XD
-spec to_http_code(t()) -> 400.
to_http_code(?ERROR_ON_NODES(Error, _)) ->
    errors:to_http_code(Error).
