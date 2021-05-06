%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Jsonable record expressing space mounting options used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_mount_space_options).
-author("Lukasz Opiola").

-behaviour(persistent_record).

-include("automation/automation.hrl").

%% Persistent record callbacks
-export([version/0, to_json/1, from_json/1]).


-type record() :: #atm_mount_space_options{}.
-export_type([record/0]).

%%%===================================================================
%%% Jsonable record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec to_json(record()) -> json_utils:json_term().
to_json(Spec) ->
    #{
        <<"mountOneclient">> => Spec#atm_mount_space_options.mount_oneclient,
        <<"mountPoint">> => Spec#atm_mount_space_options.mount_point,
        <<"oneclientOptions">> => Spec#atm_mount_space_options.oneclient_options
    }.


-spec from_json(json_utils:json_term()) -> record().
from_json(SpecJson) ->
    #atm_mount_space_options{
        mount_oneclient = maps:get(<<"mountOneclient">>, SpecJson, false),
        mount_point = maps:get(<<"mountPoint">>, SpecJson, <<"/mnt/onedata">>),
        oneclient_options = maps:get(<<"oneclientOptions">>, SpecJson, <<"">>)
    }.
