%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Base model for argument value builder used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_task_argument_value_builder).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_task_argument_value_builder{}.
-type type() :: iterated_item | const | object | store_credentials | onedatafs_credentials.
%% @formatter:off
%% Information required for given value builder type to build a value:
%%  * iterated_item: a recipe, expressed as an array, specifying how to extract a value from
%%    an object that will be fed into the task by the store iterator of the current lane, eg:
%%      undefined - the value will be expanded to the incoming object as-is (the field is optional)
%%      [] - the same as undefined
%%      ["key1", "key2", "1"] - the value will be expanded to the JSON term residing
%%          under specified nested keys, e.g. given one of below incoming objects:
%%              {"key1": {"key2": {"1": "foo"}}
%%              {"key1": {"key2": ["zeroIndex", "foo"]}
%%          the recipe would yield value "foo" (arrays are treated as objects with integer keys)
%%
%%  * const: a constant value, eg. 13, "some text", true
%%
%%  * object: a specification of an object to be built - can include only
%%    nested #atm_task_argument_value_builder{} records as values:
%%      #{
%%          "myStringValue": #atm_task_argument_value_builder{type = const, recipe = "some text"},
%%          "myIntegerValue": #atm_task_argument_value_builder{type = const, recipe = 13},
%%          "myBoolValue": #atm_task_argument_value_builder{type = const, recipe = true},
%%          "myStoreCredentials": #atm_task_argument_value_builder{type = store_credentials, recipe = "$storeSchemaId"},
%%          "myNestedObject": #atm_task_argument_value_builder{type = object, recipe = #{...}}
%%       }
%%
%%  * store_credentials: the id of a specific store schema - the value will be expanded
%%    during execution to the credentials allowing access to the store that was instantiated from the schema
%%
%%  * onedatafs_credentials: always undefined - the recipe is ignored as there is no parametrization,
%%    the value will be expanded to credentials required to mount a OnedataFS client
%%    during actual workflow execution.
%%
%% @formatter:on
-type recipe() :: undefined | json_utils:json_term().
-export_type([record/0, type/0, recipe/0]).

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_map().
to_json(#atm_task_argument_value_builder{type = Type, recipe = Recipe}) ->
    #{
        <<"valueBuilderType">> => type_to_json(Type),
        <<"valueBuilderRecipe">> => recipe_to_json(Type, Recipe)
    }.


-spec from_json(json_utils:json_map()) -> record().
from_json(RecordJson) ->
    Type = type_from_json(maps:get(<<"valueBuilderType">>, RecordJson)),
    #atm_task_argument_value_builder{
        type = Type,
        recipe = recipe_from_json(Type, maps:get(<<"valueBuilderRecipe">>, RecordJson))
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec type_to_json(type()) -> json_utils:json_term().
type_to_json(iterated_item) -> <<"iteratedItem">>;
type_to_json(const) -> <<"const">>;
type_to_json(object) -> <<"object">>;
type_to_json(store_credentials) -> <<"storeCredentials">>;
type_to_json(onedatafs_credentials) -> <<"onedatafsCredentials">>.


-spec type_from_json(json_utils:json_term()) -> type().
type_from_json(<<"iteratedItem">>) -> iterated_item;
type_from_json(<<"const">>) -> const;
type_from_json(<<"object">>) -> object;
type_from_json(<<"storeCredentials">>) -> store_credentials;
type_from_json(<<"onedatafsCredentials">>) -> onedatafs_credentials.


-spec recipe_to_json(type(), recipe()) -> json_utils:json_term().
recipe_to_json(iterated_item, undefined) ->
    null;
recipe_to_json(iterated_item, List) when is_list(List) ->
    List;

recipe_to_json(const, Val) ->
    Val;

recipe_to_json(object, ObjectSpec) ->
    maps:map(fun(Key, #atm_task_argument_value_builder{} = NestedBuilder) when is_binary(Key) ->
        to_json(NestedBuilder)
    end, ObjectSpec);

recipe_to_json(store_credentials, StoreSchemaId) when is_binary(StoreSchemaId) ->
    StoreSchemaId;

recipe_to_json(onedatafs_credentials, _) ->
    null.


-spec recipe_from_json(type(), json_utils:json_term()) -> recipe().
recipe_from_json(iterated_item, null) ->
    undefined;
recipe_from_json(iterated_item, List) when is_list(List) ->
    List;

recipe_from_json(const, Val) ->
    Val;

recipe_from_json(object, ObjectSpec) ->
    maps:map(fun(Key, NestedBuilderJson) when is_binary(Key); is_map(NestedBuilderJson) ->
        from_json(NestedBuilderJson)
    end, ObjectSpec);

recipe_from_json(store_credentials, StoreSchemaId) when is_binary(StoreSchemaId) ->
    StoreSchemaId;

recipe_from_json(onedatafs_credentials, _) ->
    undefined.
