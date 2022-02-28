%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing store content update options specialization for
%%% list store used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_list_store_content_update_options).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").


%% API
-export([function_to_json/1, function_from_json/1]).

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type update_function() :: append | extend.
-export_type([update_function/0]).

-type record() :: #atm_list_store_content_update_options{}.
-export_type([record/0]).


%%%===================================================================
%%% API
%%%===================================================================

%% @private
-spec function_to_json(update_function()) -> json_utils:json_term().
function_to_json(append) -> <<"append">>;
function_to_json(extend) -> <<"extend">>.


%% @private
-spec function_from_json(json_utils:json_term()) -> update_function().
function_from_json(<<"append">>) -> append;
function_from_json(<<"extend">>) -> extend.

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    #{
        <<"function">> => function_to_json(Record#atm_list_store_content_update_options.function)
    }.


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    #atm_list_store_content_update_options{
        function = function_from_json(maps:get(<<"function">>, RecordJson))
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
