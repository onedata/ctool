%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles encoding of data types and corresponding value
%%% constraints and defines common interface for data type modules.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_data_type).
-author("Lukasz Opiola").

-export([all_data_types/0]).
-export([is_instance/2]).
-export([type_to_json/1, type_from_json/1]).
-export([encode_value_constraints/3, decode_value_constraints/4]).

%% @formatter:off
-type type() :: atm_archive_type
              | atm_array_type
              | atm_dataset_type
              | atm_integer_type
              | atm_file_type
              | atm_object_type
              | atm_onedatafs_credentials_type
              | atm_range_type
              | atm_store_credentials_type
              | atm_string_type
              | atm_time_series_measurement_type.
%% @formatter:on
-type value_constraints() :: map().
-export_type([type/0, value_constraints/0]).

%%%===================================================================
%%% atm_data_type behaviour
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Checks if given value is of the type represented by the implementing module.
%% @end
%%--------------------------------------------------------------------
-callback is_instance(json_utils:json_term()) -> boolean().


%%--------------------------------------------------------------------
%% @doc
%% Encodes the type constraints using given encoder.
%% @end
%%--------------------------------------------------------------------
-callback encode_value_constraints(value_constraints(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().


%%--------------------------------------------------------------------
%% @doc
%% Decodes the type constraints using given decoder and applying (optional) validation.
%% @end
%%--------------------------------------------------------------------
-callback decode_value_constraints(
    automation:validation_strategy(),
    json_utils:json_term(),
    persistent_record:nested_record_decoder()
) ->
    value_constraints().

%%%===================================================================
%%% API functions
%%%===================================================================

-spec all_data_types() -> [type()].
all_data_types() -> [
    atm_archive_type,
    atm_array_type,
    atm_dataset_type,
    atm_integer_type,
    atm_file_type,
    atm_object_type,
    atm_onedatafs_credentials_type,
    atm_range_type,
    atm_store_credentials_type,
    atm_string_type,
    atm_time_series_measurement_type
].


-spec is_instance(type(), json_utils:json_term()) -> boolean().
is_instance(TypeName, Value) ->
    TypeName:is_instance(Value).


-spec type_to_json(type()) -> json_utils:json_term().
type_to_json(atm_archive_type) -> <<"archive">>;
type_to_json(atm_array_type) -> <<"array">>;
type_to_json(atm_dataset_type) -> <<"dataset">>;
type_to_json(atm_integer_type) -> <<"integer">>;
type_to_json(atm_file_type) -> <<"file">>;
type_to_json(atm_object_type) -> <<"object">>;
type_to_json(atm_onedatafs_credentials_type) -> <<"onedatafsCredentials">>;
type_to_json(atm_range_type) -> <<"range">>;
type_to_json(atm_store_credentials_type) -> <<"storeCredentials">>;
type_to_json(atm_string_type) -> <<"string">>;
type_to_json(atm_time_series_measurement_type) -> <<"timeSeriesMeasurement">>.


-spec type_from_json(json_utils:json_term()) -> type().
type_from_json(<<"archive">>) -> atm_archive_type;
type_from_json(<<"array">>) -> atm_array_type;
type_from_json(<<"dataset">>) -> atm_dataset_type;
type_from_json(<<"integer">>) -> atm_integer_type;
type_from_json(<<"file">>) -> atm_file_type;
type_from_json(<<"object">>) -> atm_object_type;
type_from_json(<<"onedatafsCredentials">>) -> atm_onedatafs_credentials_type;
type_from_json(<<"range">>) -> atm_range_type;
type_from_json(<<"storeCredentials">>) -> atm_store_credentials_type;
type_from_json(<<"string">>) -> atm_string_type;
type_from_json(<<"timeSeriesMeasurement">>) -> atm_time_series_measurement_type.


-spec encode_value_constraints(
    type(),
    value_constraints(),
    persistent_record:nested_record_encoder()
) ->
    json_utils:json_term().
encode_value_constraints(TypeName, Constraints, NestedRecordEncoder) ->
    TypeName:encode_value_constraints(Constraints, NestedRecordEncoder).


-spec decode_value_constraints(
    type(),
    automation:validation_strategy(),
    json_utils:json_term(),
    persistent_record:nested_record_decoder()
) ->
    value_constraints().
decode_value_constraints(TypeName, ValidationStrategy, ConstraintsJson, NestedRecordDecoder) ->
    TypeName:decode_value_constraints(ValidationStrategy, ConstraintsJson, NestedRecordDecoder).
