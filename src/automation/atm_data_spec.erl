%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Base model for data specs used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_data_spec).
-author("Bartosz Walkowicz").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").


%% API
-export([get_type/1, get_value_constraints/1]).

%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_data_spec{}.
-export_type([record/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec get_type(record()) -> atm_data_type:type().
get_type(#atm_data_spec{type = Type}) ->
    Type.


-spec get_value_constraints(record()) -> atm_data_type:value_constraints().
get_value_constraints(#atm_data_spec{value_constraints = ValueConstraints}) ->
    ValueConstraints.

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_map().
to_json(#atm_data_spec{type = Type, value_constraints = ValueConstraints}) ->
    #{
        <<"type">> => atm_data_type:type_to_json(Type),
        <<"valueConstraints">> => atm_data_type:value_constraints_to_json(Type, ValueConstraints)
    }.


-spec from_json(json_utils:json_map()) -> record().
from_json(RecordJson) ->
    Type = atm_data_type:type_from_json(maps:get(<<"type">>, RecordJson)),
    ValueConstraints = maps:get(<<"valueConstraints">>, RecordJson, #{}),
    #atm_data_spec{
        type = Type,
        value_constraints = atm_data_type:value_constraints_from_json(Type, ValueConstraints)
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
