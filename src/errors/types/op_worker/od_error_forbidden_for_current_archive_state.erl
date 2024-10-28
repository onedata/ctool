%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_forbidden_for_current_archive_state'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_forbidden_for_current_archive_state).

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
to_json(?ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE(CurrentState, AllowedStates)) ->
    #{
        <<"id">> => ?ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE_ID,
        <<"description">> => ?fmt(
            "This operation is forbidden while the archive state is ~ts. Allowed states are: ~ts.",
            [CurrentState, ?fmt_csv(AllowedStates)]
        ),
        <<"details">> => #{
            <<"allowedStates">> => json_utils:encode(AllowedStates),
            <<"currentState">> => json_utils:encode(CurrentState)
        }
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{
    <<"id">> := ?ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE_ID,
    <<"details">> := #{
        <<"allowedStates">> := AllowedStates,
        <<"currentState">> := CurrentState
    }
}) ->
    ?ERROR_FORBIDDEN_FOR_CURRENT_ARCHIVE_STATE(
        binary_to_existing_atom(json_utils:decode(CurrentState)),
        [binary_to_existing_atom(StateBin) || StateBin <- json_utils:decode(AllowedStates)]
    ).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
