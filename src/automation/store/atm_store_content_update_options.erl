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

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: atm_single_value_content_update_options:record()
| atm_list_content_update_options:record()
| atm_tree_forest_content_update_options:record()
| atm_range_content_update_options:record()
| atm_time_series_content_update_options:record()
| atm_audit_log_content_update_options:record().
-type record_type() :: atm_single_value_content_update_options
| atm_list_content_update_options
| atm_tree_forest_content_update_options
| atm_range_content_update_options
| atm_time_series_content_update_options
| atm_audit_log_content_update_options.
-type type() :: record_type().

-export_type([record/0]).


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
type_to_json(atm_single_value_content_update_options) -> <<"singleValueContentUpdateOptions">>;
type_to_json(atm_list_content_update_options) -> <<"listContentUpdateOptions">>;
type_to_json(atm_tree_forest_content_update_options) -> <<"treeForestContentUpdateOptions">>;
type_to_json(atm_range_content_update_options) -> <<"rangeContentUpdateOptions">>;
type_to_json(atm_time_series_content_update_options) -> <<"timeSeriesContentUpdateOptions">>;
type_to_json(atm_audit_log_content_update_options) -> <<"auditLogContentUpdateOptions">>.


%% @private
-spec type_from_json(json_utils:json_term()) -> type().
type_from_json(<<"singleValueContentUpdateOptions">>) -> atm_single_value_content_update_options;
type_from_json(<<"listContentUpdateOptions">>) -> atm_list_content_update_options;
type_from_json(<<"treeForestContentUpdateOptions">>) -> atm_tree_forest_content_update_options;
type_from_json(<<"rangeContentUpdateOptions">>) -> atm_range_content_update_options;
type_from_json(<<"timeSeriesContentUpdateOptions">>) -> atm_time_series_content_update_options;
type_from_json(<<"auditLogContentUpdateOptions">>) -> atm_audit_log_content_update_options.
