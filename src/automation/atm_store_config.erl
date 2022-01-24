%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for encoding and decoding specialized store configs.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_store_config).
-author("Lukasz Opiola").

-include("automation/automation.hrl").


%% API
-export([encode/3, decode/3]).


-type record() :: atm_single_value_store_config:record()
| atm_list_store_config:record()
| atm_map_store_config:record()
| atm_tree_forest_store_config:record()
| atm_range_store_config:record()
| atm_time_series_store_config:record()
| atm_audit_log_store_config:record().
-type record_type() :: atm_single_value_store_config
| atm_list_store_config
| atm_map_store_config
| atm_tree_forest_store_config
| atm_range_store_config
| atm_time_series_store_config
| atm_audit_log_store_config.
-export_type([record/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec encode(record(), automation:store_type(), persistent_record:nested_record_encoder()) ->
    json_utils:json_map().
encode(Record, StoreType, NestedRecordEncoder) ->
    NestedRecordEncoder(Record, store_type_to_record_type(StoreType)).


-spec decode(json_utils:json_map(), automation:store_type(), persistent_record:nested_record_decoder()) ->
    record().
decode(RecordJson, StoreType, NestedRecordDecoder) ->
    NestedRecordDecoder(RecordJson, store_type_to_record_type(StoreType)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec store_type_to_record_type(automation:store_type()) -> record_type().
store_type_to_record_type(single_value) -> atm_single_value_store_config;
store_type_to_record_type(list) -> atm_list_store_config;
store_type_to_record_type(map) -> atm_map_store_config;
store_type_to_record_type(tree_forest) -> atm_tree_forest_store_config;
store_type_to_record_type(range) -> atm_range_store_config;
store_type_to_record_type(time_series) -> atm_time_series_store_config;
store_type_to_record_type(audit_log) -> atm_audit_log_store_config.
