%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Jsonable record expressing lambda result specification used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lambda_result_spec).
-author("Lukasz Opiola").

-behaviour(persistent_record).

-include("automation/automation.hrl").

%% Persistent record callbacks
-export([version/0, to_json/1, from_json/1]).


-type record() :: #atm_lambda_result_spec{}.
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
        <<"name">> => Spec#atm_lambda_result_spec.name,
        <<"dataSpec">> => jsonable_record:to_json(Spec#atm_lambda_result_spec.data_spec, atm_data_spec),
        <<"isBatch">> => Spec#atm_lambda_result_spec.is_batch
    }.


-spec from_json(json_utils:json_term()) -> record().
from_json(SpecJson) ->
    #atm_lambda_result_spec{
        name = maps:get(<<"name">>, SpecJson),
        data_spec = jsonable_record:from_json(maps:get(<<"dataSpec">>, SpecJson), atm_data_spec),
        is_batch = maps:get(<<"isBatch">>, SpecJson)
    }.
