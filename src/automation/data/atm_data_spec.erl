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
-export([get_data_type/1]).

%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, upgrade_encoded_record/2, db_encode/2, db_decode/2]).


-type model() ::
    atm_array_data_spec |
    atm_boolean_data_spec |
    atm_dataset_data_spec |
    atm_file_data_spec |
    atm_number_data_spec |
    atm_object_data_spec |
    atm_range_data_spec |
    atm_string_data_spec |
    atm_time_series_measurement_data_spec.

-type record() ::
    atm_array_data_spec:record() |
    atm_boolean_data_spec:record() |
    atm_dataset_data_spec:record() |
    atm_file_data_spec:record() |
    atm_number_data_spec:record() |
    atm_object_data_spec:record() |
    atm_range_data_spec:record() |
    atm_string_data_spec:record() |
    atm_time_series_measurement_data_spec:record().

-export_type([record/0]).


%%%===================================================================
%%% API functions
%%%===================================================================


-spec get_data_type(record()) -> atm_data_type:type().
get_data_type(Record) ->
    model_to_data_type(utils:record_type(Record)).


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
    2.  %% TODO upgrade/test upgrade?


-spec upgrade_encoded_record(persistent_record:record_version(), json_utils:json_term()) ->
    {persistent_record:record_version(), json_utils:json_term()}.
upgrade_encoded_record(1, RecordJson0) ->
    {ValueConstraints, RecordJson1} = maps:take(<<"valueConstraints">>, RecordJson0),
    {2, maps:merge(RecordJson1, ValueConstraints)}.


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
    json_utils:json_map().
encode_with(Record, NestedRecordEncoder) ->
    Model = utils:record_type(Record),

    maps:merge(
        #{<<"type">> => atm_data_type:type_to_json(model_to_data_type(Model))},
        NestedRecordEncoder(Record, Model)
    ).


%% @private
-spec decode_with(json_utils:json_map(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(#{<<"type">> := TypeJson} = RecordJson, NestedRecordDecoder) ->
    NestedRecordDecoder(RecordJson, data_type_to_model(atm_data_type:type_from_json(TypeJson))).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec model_to_data_type(model()) -> atm_data_type:type().
model_to_data_type(atm_array_data_spec) -> atm_array_type;
model_to_data_type(atm_boolean_data_spec) -> atm_boolean_type;
model_to_data_type(atm_dataset_data_spec) -> atm_dataset_type;
model_to_data_type(atm_file_data_spec) -> atm_file_type;
model_to_data_type(atm_number_data_spec) -> atm_number_type;
model_to_data_type(atm_object_data_spec) -> atm_object_type;
model_to_data_type(atm_range_data_spec) -> atm_range_type;
model_to_data_type(atm_string_data_spec) -> atm_string_type;
model_to_data_type(atm_time_series_measurement_data_spec) -> atm_time_series_measurement_type.


%% @private
-spec data_type_to_model(atm_data_type:type()) -> model().
data_type_to_model(atm_array_type) -> atm_array_data_spec;
data_type_to_model(atm_boolean_type) -> atm_boolean_data_spec;
data_type_to_model(atm_dataset_type) -> atm_dataset_data_spec;
data_type_to_model(atm_file_type) -> atm_file_data_spec;
data_type_to_model(atm_number_type) -> atm_number_data_spec;
data_type_to_model(atm_object_type) -> atm_object_data_spec;
data_type_to_model(atm_range_type) -> atm_range_data_spec;
data_type_to_model(atm_string_type) -> atm_string_data_spec;
data_type_to_model(atm_time_series_measurement_type) -> atm_time_series_measurement_data_spec.
