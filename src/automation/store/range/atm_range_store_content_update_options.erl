%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing store content update options specialization for
%%% range store used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_range_store_content_update_options).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").


%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type start_num() :: integer().
-type end_num() :: integer().
-type step() :: integer().
-export_type([start_num/0, end_num/0, step/0]).

-type record() :: #atm_range_store_content_update_options{}.
-export_type([record/0]).


-define(DEFAULT_START_NUM, 0).
-define(DEFAULT_STEP, 1).


%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    #{
        <<"start">> => Record#atm_range_store_content_update_options.start_num,
        <<"end">> => Record#atm_range_store_content_update_options.end_num,
        <<"step">> => Record#atm_range_store_content_update_options.step
    }.


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    #atm_range_store_content_update_options{
        start_num = maps:get(<<"start">>, RecordJson, ?DEFAULT_START_NUM),
        end_num = maps:get(<<"end">>, RecordJson),
        step = maps:get(<<"step">>, RecordJson, ?DEFAULT_STEP)
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
