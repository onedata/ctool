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
-export([value_constraints_to_json/2, value_constraints_from_json/2]).

-type type() :: atm_integer_type | atm_string_type | atm_object_type
| atm_file_type | atm_histogram_type
| atm_dataset_type | atm_archive_type
| atm_store_credentials_type | atm_onedatafs_credentials_type.
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
%% Encodes the type constraints to json term.
%% @end
%%--------------------------------------------------------------------
-callback value_constraints_to_json(value_constraints()) -> json_utils:json_term().


%%--------------------------------------------------------------------
%% @doc
%% Decodes the type constraints from json term.
%% @end
%%--------------------------------------------------------------------
-callback value_constraints_from_json(json_utils:json_term()) -> value_constraints().

%%%===================================================================
%%% API functions
%%%===================================================================

-spec all_data_types() -> [type()].
all_data_types() -> [
    atm_integer_type, atm_string_type, atm_object_type, atm_file_type, atm_histogram_type,
    atm_dataset_type, atm_archive_type, atm_store_credentials_type, atm_onedatafs_credentials_type
].


-spec is_instance(type(), term()) -> boolean().
is_instance(TypeName, Value) ->
    TypeName:is_instance(Value).


-spec type_to_json(type()) -> json_utils:json_term().
type_to_json(atm_integer_type) -> <<"integer">>;
type_to_json(atm_string_type) -> <<"string">>;
type_to_json(atm_object_type) -> <<"object">>;
type_to_json(atm_file_type) -> <<"file">>;
type_to_json(atm_histogram_type) -> <<"histogram">>;
type_to_json(atm_dataset_type) -> <<"dataset">>;
type_to_json(atm_archive_type) -> <<"archive">>;
type_to_json(atm_store_credentials_type) -> <<"storeCredentials">>;
type_to_json(atm_onedatafs_credentials_type) -> <<"onedatafsCredentials">>.


-spec type_from_json(json_utils:json_term()) -> type().
type_from_json(<<"integer">>) -> atm_integer_type;
type_from_json(<<"string">>) -> atm_string_type;
type_from_json(<<"object">>) -> atm_object_type;
type_from_json(<<"file">>) -> atm_file_type;
type_from_json(<<"histogram">>) -> atm_histogram_type;
type_from_json(<<"dataset">>) -> atm_dataset_type;
type_from_json(<<"archive">>) -> atm_archive_type;
type_from_json(<<"storeCredentials">>) -> atm_store_credentials_type;
type_from_json(<<"onedatafsCredentials">>) -> atm_onedatafs_credentials_type.


-spec value_constraints_to_json(type(), value_constraints()) -> json_utils:json_term().
value_constraints_to_json(TypeName, Constraints) ->
    TypeName:value_constraints_to_json(Constraints).


-spec value_constraints_from_json(type(), json_utils:json_term()) -> value_constraints().
value_constraints_from_json(TypeName, ConstraintsJson) ->
    TypeName:value_constraints_from_json(ConstraintsJson).
