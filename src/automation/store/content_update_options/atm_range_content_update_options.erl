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
-module(atm_range_content_update_options).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").


%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type start_num() :: undefined | integer().  % optional, defaults to 0
-type end_num() :: integer().
-type step() :: undefined | integer().  % optional, defaults to 1
-export_type([start_num/0, end_num/0, step/0]).

-type record() :: #atm_range_content_update_options{}.
-export_type([record/0]).


%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    #{
        <<"start">> => utils:undefined_to_null(Record#atm_range_content_update_options.start_num),
        <<"end">> => Record#atm_range_content_update_options.end_num,
        <<"step">> => utils:undefined_to_null(Record#atm_range_content_update_options.step)
    }.


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    #atm_range_content_update_options{
        start_num = utils:null_to_undefined(maps:get(<<"start">>, RecordJson, undefined)),
        end_num = maps:get(<<"end">>, RecordJson),
        step = utils:null_to_undefined(maps:get(<<"step">>, RecordJson, undefined))
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
