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
-module(od_error_atm_task_result_missing).

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
to_json(?ERROR_ATM_TASK_RESULT_MISSING(MissingResultName, ReceivedResultNames)) ->
    #{
        <<"id">> => ?ERROR_ATM_TASK_RESULT_MISSING_ID,
        <<"details">> => #{
            <<"missingResultName">> => MissingResultName,
            <<"receivedResultNames">> => ReceivedResultNames
        },
        <<"description">> => ?fmt(
            "Missing required value for result '~ts' in the lambda output. Received values for result names: ~ts.",
            [MissingResultName, ?fmt_csv(ReceivedResultNames)]
        )
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{
    <<"id">> := ?ERROR_ATM_TASK_RESULT_MISSING_ID,
    <<"details">> := #{
        <<"missingResultName">> := MissingResultName,
        <<"receivedResultNames">> := ReceivedResultNames
    }
}) ->
    ?ERROR_ATM_TASK_RESULT_MISSING(MissingResultName, ReceivedResultNames).


-spec to_http_code(t()) -> 400.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
