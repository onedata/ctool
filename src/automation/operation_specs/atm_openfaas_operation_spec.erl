%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing onedata function operation spec used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_openfaas_operation_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).

-include("automation/automation.hrl").

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).


-type record() :: #atm_openfaas_operation_spec{}.
-export_type([record/0]).

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Spec) ->
    #{
        <<"dockerImage">> => Spec#atm_openfaas_operation_spec.docker_image,
        <<"dockerExecutionOptions">> => atm_docker_execution_options:to_json(Spec#atm_openfaas_operation_spec.docker_execution_options)
    }.


-spec from_json(json_utils:json_term()) -> record().
from_json(SpecJson) ->
    #atm_openfaas_operation_spec{
        docker_image = maps:get(<<"dockerImage">>, SpecJson),
        docker_execution_options = atm_docker_execution_options:from_json(maps:get(<<"dockerExecutionOptions">>, SpecJson, #{}))
    }.
