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

%% @formatter:off
-type type() :: atm_array_type
              | atm_boolean_type
              | atm_dataset_type
              | atm_file_type
              | atm_group_type
              | atm_number_type
              | atm_object_type
              | atm_range_type
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


%%%===================================================================
%%% API functions
%%%===================================================================

-spec all_data_types() -> [type()].
all_data_types() -> [
    atm_array_type,
    atm_boolean_type,
    atm_dataset_type,
    atm_file_type,
    atm_group_type,
    atm_number_type,
    atm_object_type,
    atm_range_type,
    atm_string_type,
    atm_time_series_measurement_type
].


%% @TODO VFS-7687 Implement all automation data types and validators
-spec is_instance(type(), json_utils:json_term()) -> boolean().
is_instance(atm_array_type, Value) when is_list(Value) ->
    true;
is_instance(atm_boolean_type, Value) when is_boolean(Value) ->
    true;
is_instance(atm_dataset_type, #{<<"datasetId">> := DatasetId}) when is_binary(DatasetId) ->
    true;
is_instance(atm_file_type, #{<<"fileId">> := FileId}) when is_binary(FileId) ->
    true;
is_instance(atm_group_type, #{<<"groupId">> := GroupId}) when is_binary(GroupId) ->
    true;
is_instance(atm_number_type, Value) when is_number(Value) ->
    true;
is_instance(atm_object_type, Value) when is_map(Value) ->
    true;
is_instance(atm_range_type, #{<<"end">> := End} = Range) when is_integer(End) ->
    % the "end" parameter is required, others are optional
    is_integer(maps:get(<<"start">>, Range, 0)) andalso is_integer(maps:get(<<"step">>, Range, 1));
is_instance(atm_string_type, Value) when is_binary(Value) ->
    true;
is_instance(atm_time_series_measurement_type, #{
    <<"tsName">> := TsName,
    <<"timestamp">> := Timestamp,
    <<"value">> := Value
}) when is_binary(TsName), is_integer(Timestamp), Timestamp >= 0, is_number(Value) ->
    true;
is_instance(_, _) ->
    false.


-spec type_to_json(type()) -> json_utils:json_term().
type_to_json(atm_array_type) -> <<"array">>;
type_to_json(atm_boolean_type) -> <<"boolean">>;
type_to_json(atm_dataset_type) -> <<"dataset">>;
type_to_json(atm_file_type) -> <<"file">>;
type_to_json(atm_group_type) -> <<"group">>;
type_to_json(atm_number_type) -> <<"number">>;
type_to_json(atm_object_type) -> <<"object">>;
type_to_json(atm_range_type) -> <<"range">>;
type_to_json(atm_string_type) -> <<"string">>;
type_to_json(atm_time_series_measurement_type) -> <<"timeSeriesMeasurement">>.


-spec type_from_json(json_utils:json_term()) -> type().
type_from_json(<<"array">>) -> atm_array_type;
type_from_json(<<"boolean">>) -> atm_boolean_type;
type_from_json(<<"dataset">>) -> atm_dataset_type;
type_from_json(<<"file">>) -> atm_file_type;
type_from_json(<<"group">>) -> atm_group_type;
type_from_json(<<"number">>) -> atm_number_type;
type_from_json(<<"object">>) -> atm_object_type;
type_from_json(<<"range">>) -> atm_range_type;
type_from_json(<<"string">>) -> atm_string_type;
type_from_json(<<"timeSeriesMeasurement">>) -> atm_time_series_measurement_type.
