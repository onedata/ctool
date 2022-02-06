%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Base model for task result mapper used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_task_schema_result_mapper).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").


%% API
-export([all_dispatch_functions/0]).

%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_task_schema_result_mapper{}.
%%% @TODO VFS-8958 Refine and implement time series selectors and store mappers - add missing dispatch function
-type dispatch_function() :: add | remove | set | append | extend.
-export_type([record/0, dispatch_function/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec all_dispatch_functions() -> [dispatch_function()].
all_dispatch_functions() ->
    [add, remove, set, append, extend].

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_map().
to_json(Record) ->
    #{
        <<"resultName">> => Record#atm_task_schema_result_mapper.result_name,
        <<"storeSchemaId">> => Record#atm_task_schema_result_mapper.store_schema_id,
        <<"dispatchFunction">> => dispatch_function_to_json(Record#atm_task_schema_result_mapper.dispatch_function)
    }.


-spec from_json(json_utils:json_map()) -> record().
from_json(RecordJson) ->
    #atm_task_schema_result_mapper{
        result_name = maps:get(<<"resultName">>, RecordJson),
        store_schema_id = maps:get(<<"storeSchemaId">>, RecordJson),
        dispatch_function = dispatch_function_from_json(maps:get(<<"dispatchFunction">>, RecordJson))
    }.

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, _NestedRecordEncoder) ->
    to_json(Record).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, _NestedRecordDecoder) ->
    from_json(RecordJson).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec dispatch_function_to_json(dispatch_function()) -> json_utils:json_term().
dispatch_function_to_json(add) -> <<"add">>;
dispatch_function_to_json(remove) -> <<"remove">>;
dispatch_function_to_json(set) -> <<"set">>;
dispatch_function_to_json(append) -> <<"append">>;
dispatch_function_to_json(extend) -> <<"extend">>.


-spec dispatch_function_from_json(json_utils:json_term()) -> dispatch_function().
dispatch_function_from_json(<<"add">>) -> add;
dispatch_function_from_json(<<"remove">>) -> remove;
dispatch_function_from_json(<<"set">>) -> set;
dispatch_function_from_json(<<"append">>) -> append;
dispatch_function_from_json(<<"extend">>) -> extend.
