%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing tree forest store config used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_tree_forest_store_config).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").


%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_tree_forest_store_config{}.
-export_type([record/0]).

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    encode_with(Record, fun jsonable_record:to_json/2).


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    decode_with(json, RecordJson, fun jsonable_record:from_json/2).

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, NestedRecordEncoder) ->
    encode_with(Record, NestedRecordEncoder).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, NestedRecordDecoder) ->
    decode_with(db, RecordJson, NestedRecordDecoder).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec encode_with(record(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_with(Record, NestedRecordEncoder) ->
    #{
        <<"dataSpec">> => NestedRecordEncoder(Record#atm_tree_forest_store_config.data_spec, atm_data_spec)
    }.


-spec decode_with(json | db, json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(db, RecordJson, NestedRecordDecoder) ->
    #atm_tree_forest_store_config{
        data_spec = NestedRecordDecoder(maps:get(<<"dataSpec">>, RecordJson), atm_data_spec)
    };
decode_with(json, RecordJson, NestedRecordDecoder) ->
    Spec = decode_with(db, RecordJson, NestedRecordDecoder),
    atm_data_spec:assert_has_allowed_data_type(
        <<"treeForestStoreConfig.dataSpec.type">>,
        Spec#atm_tree_forest_store_config.data_spec,
        [atm_file_type, atm_dataset_type]
    ),
    Spec.
