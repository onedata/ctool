%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module implementing the file data spec used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_file_data_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").

%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type record() :: #atm_file_data_spec{}.
-type file_type() :: 'REG' | 'DIR' | 'SYMLNK' | 'ANY'.
-type attribute() ::
    name |
    type |
    mode |
    size |
    atime |
    mtime |
    ctime |
    owner_id |
    file_id |
    parent_id |
    provider_id |
    storage_user_id |
    storage_group_id |
    shares |
    hardlinks_count |
    index.

-export_type([file_type/0, attribute/0, record/0]).

-define(ALL_ATTRIBUTES, [
    name, type, mode, size, atime, mtime, ctime,
    owner_id, file_id, parent_id, provider_id,
    storage_user_id, storage_group_id,
    shares, hardlinks_count, index
]).


%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================


-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    db_encode(Record, fun jsonable_record:to_json/2).


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    db_decode(RecordJson, fun jsonable_record:from_json/2).


%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================


-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record = #atm_file_data_spec{file_type = FileType}, _NestedRecordEncoder) ->
    #{
        <<"fileType">> => file_type_to_json(FileType),
        <<"attributes">> => case Record#atm_file_data_spec.attributes of
            undefined -> null;
            Attributes -> lists:map(fun attribute_to_json/1, Attributes)
        end
    }.


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, _NestedRecordDecoder) ->
    #atm_file_data_spec{
        file_type = file_type_from_json(maps:get(<<"fileType">>, RecordJson, <<"ANY">>)),
        attributes = case maps:find(<<"attributes">>, RecordJson) of
            error ->
                % Set all possible attributes when field is missing to previous
                % version behaviour when all file attrs where always resolved
                lists:usort(?ALL_ATTRIBUTES);
            {ok, null} ->
                undefined;
            {ok, AttributesJson} ->
                lists:usort(lists:map(fun attribute_from_json/1, AttributesJson))
        end
    }.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec file_type_to_json(file_type()) -> json_utils:json_term().
file_type_to_json('REG') -> <<"REG">>;
file_type_to_json('DIR') -> <<"DIR">>;
file_type_to_json('SYMLNK') -> <<"SYMLNK">>;
file_type_to_json('ANY') -> <<"ANY">>.


%% @private
-spec file_type_from_json(json_utils:json_term()) -> file_type().
file_type_from_json(<<"REG">>) -> 'REG';
file_type_from_json(<<"DIR">>) -> 'DIR';
file_type_from_json(<<"SYMLNK">>) -> 'SYMLNK';
file_type_from_json(<<"ANY">>) -> 'ANY'.


%% @private
-spec attribute_to_json(attribute()) -> binary().
attribute_to_json(name) -> <<"name">>;
attribute_to_json(type) -> <<"type">>;
attribute_to_json(mode) -> <<"mode">>;
attribute_to_json(size) -> <<"size">>;
attribute_to_json(atime) -> <<"atime">>;
attribute_to_json(mtime) -> <<"mtime">>;
attribute_to_json(ctime) -> <<"ctime">>;
attribute_to_json(owner_id) -> <<"owner_id">>;
attribute_to_json(file_id) -> <<"file_id">>;
attribute_to_json(parent_id) -> <<"parent_id">>;
attribute_to_json(provider_id) -> <<"provider_id">>;
attribute_to_json(storage_user_id) -> <<"storage_user_id">>;
attribute_to_json(storage_group_id) -> <<"storage_group_id">>;
attribute_to_json(shares) -> <<"shares">>;
attribute_to_json(hardlinks_count) -> <<"hardlinks_count">>;
attribute_to_json(index) -> <<"index">>.


%% @private
-spec attribute_from_json(binary()) -> attribute().
attribute_from_json(<<"name">>) -> name;
attribute_from_json(<<"type">>) -> type;
attribute_from_json(<<"mode">>) -> mode;
attribute_from_json(<<"size">>) -> size;
attribute_from_json(<<"atime">>) -> atime;
attribute_from_json(<<"mtime">>) -> mtime;
attribute_from_json(<<"ctime">>) -> ctime;
attribute_from_json(<<"owner_id">>) -> owner_id;
attribute_from_json(<<"file_id">>) -> file_id;
attribute_from_json(<<"parent_id">>) -> parent_id;
attribute_from_json(<<"provider_id">>) -> provider_id;
attribute_from_json(<<"storage_user_id">>) -> storage_user_id;
attribute_from_json(<<"storage_group_id">>) -> storage_group_id;
attribute_from_json(<<"shares">>) -> shares;
attribute_from_json(<<"hardlinks_count">>) -> hardlinks_count;
attribute_from_json(<<"index">>) -> index.
