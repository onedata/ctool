%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module implementing the group data spec used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_group_data_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% API
-export([allowed_group_attributes/0]).

%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type record() :: #atm_group_data_spec{}.

-type attribute_name() :: group_id | name | type.

-export_type([record/0, attribute_name/0]).


%%%===================================================================
%%% API
%%%===================================================================


-spec allowed_group_attributes() -> [attribute_name()].
allowed_group_attributes() ->
    [group_id, name, type].


%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================


-spec to_json(record()) -> json_utils:json_term().
to_json(#atm_group_data_spec{attributes = Attributes}) ->
    #{
        <<"attributes">> => utils:undefined_to_null(utils:convert_defined(Attributes, fun(List) ->
            lists:usort(lists:map(fun attribute_name_to_json/1, List))
        end))
    }.


-spec from_json(json_utils:json_term()) -> record().
from_json(#{<<"attributes">> := AttributesJson}) ->
    #atm_group_data_spec{
        attributes = utils:convert_defined(utils:null_to_undefined(AttributesJson), fun(List) ->
            lists:usort(lists:map(fun attribute_name_from_json/1, List))
        end)
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


%% @private
-spec attribute_name_to_json(attribute_name()) -> json_utils:json_term().
attribute_name_to_json(group_id) -> <<"groupId">>;
attribute_name_to_json(name) -> <<"name">>;
attribute_name_to_json(type) -> <<"type">>.


%% @private
-spec attribute_name_from_json(json_utils:json_term()) -> attribute_name().
attribute_name_from_json(<<"groupId">>) -> group_id;
attribute_name_from_json(<<"name">>) -> name;
attribute_name_from_json(<<"type">>) -> type.
