%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements od_error for 'od_error_atm_invalid_status_transition'.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_atm_invalid_status_transition).

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
to_json(?ERROR_ATM_INVALID_STATUS_TRANSITION(PrevStatus, NewStatus)) ->
    PrevStatusJson = atom_to_binary(PrevStatus, utf8),
    NewStatusJson = atom_to_binary(NewStatus, utf8),

    #{
        <<"id">> => ?ERROR_ATM_INVALID_STATUS_TRANSITION_ID,
        <<"details">> => #{
            <<"prevStatus">> => PrevStatusJson,
            <<"newStatus">> => NewStatusJson
        },
        <<"description">> => <<"Invalid status transition (see details).">>
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(OdErrorJson = #{<<"id">> := ?ERROR_ATM_INVALID_STATUS_TRANSITION_ID}) ->
    DetailsJson = maps:get(<<"details">>, OdErrorJson),

    PrevStatusJson = maps:get(<<"prevStatus">>, DetailsJson),
    PrevStatus = binary_to_existing_atom(PrevStatusJson, utf8),
    NewStatusJson = maps:get(<<"newStatus">>, DetailsJson),
    NewStatus = binary_to_existing_atom(NewStatusJson, utf8),

    ?ERROR_ATM_INVALID_STATUS_TRANSITION(PrevStatus, NewStatus).


-spec to_http_code(t()) -> ?HTTP_400_BAD_REQUEST.
to_http_code(_) ->
    ?HTTP_400_BAD_REQUEST.
