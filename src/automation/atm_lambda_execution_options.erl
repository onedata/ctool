%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Jsonable record expressing lambda execution options used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lambda_execution_options).
-author("Lukasz Opiola").

-behaviour(persistent_record).

-include("automation/automation.hrl").

%% Persistent record callbacks
-export([version/0, to_json/1, from_json/1]).


-type record() :: #atm_lambda_execution_options{}.
-export_type([record/0]).

%%%===================================================================
%%% Persistent record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec to_json(record()) -> json_utils:json_term().
to_json(Spec) ->
    #{
        <<"readonly">> => Spec#atm_lambda_execution_options.readonly,
        <<"mountSpaceOptions">> => jsonable_record:to_json(
            Spec#atm_lambda_execution_options.mount_space_options, atm_mount_space_options
        )
    }.


-spec from_json(json_utils:json_term()) -> record().
from_json(SpecJson) ->
    #atm_lambda_execution_options{
        readonly = maps:get(<<"readonly">>, SpecJson, false),
        mount_space_options = jsonable_record:from_json(
            maps:get(<<"mountSpaceOptions">>, SpecJson, #{}), atm_mount_space_options
        )
    }.
