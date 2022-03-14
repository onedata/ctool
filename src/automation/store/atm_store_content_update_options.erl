%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing "polymorphic" store content update options used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_store_content_update_options).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% API
-export([infer_store_type/1]).

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: atm_audit_log_store_content_update_options:record()
| atm_list_store_content_update_options:record()
| atm_range_store_content_update_options:record()
| atm_single_value_store_content_update_options:record()
| atm_time_series_store_content_update_options:record()
| atm_tree_forest_store_content_update_options:record().
-type record_type() :: atm_audit_log_store_content_update_options
| atm_list_store_content_update_options
| atm_range_store_content_update_options
| atm_single_value_store_content_update_options
| atm_time_series_store_content_update_options
| atm_tree_forest_store_content_update_options.
-type type() :: record_type().

-export_type([record/0]).

% @TODO VFS-8042 Consider making store update options optional. For simple
% stores that do not have any options, this will be more convenient.
% For the other, where sane defaults are not possible, the defaults may be
% empty to trigger errors about missing fields.

%%%===================================================================
%%% API
%%%===================================================================

-spec infer_store_type(record()) -> automation:store_type().
infer_store_type(#atm_audit_log_store_content_update_options{}) -> audit_log;
infer_store_type(#atm_list_store_content_update_options{}) -> list;
infer_store_type(#atm_range_store_content_update_options{}) -> range;
infer_store_type(#atm_single_value_store_content_update_options{}) -> single_value;
infer_store_type(#atm_time_series_store_content_update_options{}) -> time_series;
infer_store_type(#atm_tree_forest_store_content_update_options{}) -> tree_forest.

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    encode_with(Record, fun jsonable_record:to_json/2).


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    decode_with(RecordJson, fun jsonable_record:from_json/2).

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
    decode_with(RecordJson, NestedRecordDecoder).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec encode_with(record(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_with(Record, NestedRecordEncoder) ->
    RecordType = utils:record_type(Record),
    maps:merge(
        #{<<"type">> => type_to_json(RecordType)},
        NestedRecordEncoder(Record, RecordType)
    ).


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedRecordDecoder) ->
    RecordType = type_from_json(maps:get(<<"type">>, RecordJson)),
    NestedRecordDecoder(RecordJson, RecordType).


%% @private
-spec type_to_json(type()) -> json_utils:json_term().
type_to_json(atm_audit_log_store_content_update_options) -> <<"auditLogStoreContentUpdateOptions">>;
type_to_json(atm_list_store_content_update_options) -> <<"listStoreContentUpdateOptions">>;
type_to_json(atm_range_store_content_update_options) -> <<"rangeStoreContentUpdateOptions">>;
type_to_json(atm_single_value_store_content_update_options) -> <<"singleValueStoreContentUpdateOptions">>;
type_to_json(atm_time_series_store_content_update_options) -> <<"timeSeriesStoreContentUpdateOptions">>;
type_to_json(atm_tree_forest_store_content_update_options) -> <<"treeForestStoreContentUpdateOptions">>.


%% @private
-spec type_from_json(json_utils:json_term()) -> type().
type_from_json(<<"auditLogStoreContentUpdateOptions">>) -> atm_audit_log_store_content_update_options;
type_from_json(<<"listStoreContentUpdateOptions">>) -> atm_list_store_content_update_options;
type_from_json(<<"rangeStoreContentUpdateOptions">>) -> atm_range_store_content_update_options;
type_from_json(<<"singleValueStoreContentUpdateOptions">>) -> atm_single_value_store_content_update_options;
type_from_json(<<"timeSeriesStoreContentUpdateOptions">>) -> atm_time_series_store_content_update_options;
type_from_json(<<"treeForestStoreContentUpdateOptions">>) -> atm_tree_forest_store_content_update_options.
