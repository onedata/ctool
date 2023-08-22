%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module defines persistent record interface - implemented for erlang
%%% records that are to be stored in persistent database and to that end require
%%% an encoder and decoder to/from JSON. Each such record should have a
%%% dedicated module implementing the callbacks.
%%% @end
%%%-------------------------------------------------------------------
-module(persistent_record).
-author("Lukasz Opiola").

-include_lib("ctool/include/logging.hrl").

%% API
-export([
    to_string/2, from_string/2,
    to_json/1, to_json/2, from_json/2
]).

-type record_version() :: non_neg_integer().
-export_type([record_version/0]).

-type nested_record_encoder() :: fun((jsonable_record:record(), jsonable_record:record_type()) -> json_utils:json_term()).
-type nested_record_decoder() :: fun((json_utils:json_term(), jsonable_record:record_type()) -> jsonable_record:record()).
-export_type([nested_record_encoder/0, nested_record_decoder/0]).

%%%===================================================================
%%% persistent_record behaviour definition
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Encodes a record into a database-compliant JSON object. The NestedRecordEncoder
%% passed in the second argument can be called to encode any nested record.
%% @end
%%--------------------------------------------------------------------
-callback db_encode(jsonable_record:record(), nested_record_encoder()) -> json_utils:json_term().


%%--------------------------------------------------------------------
%% @doc
%% Decodes a record from a database-compliant JSON object. The NestedRecordDecoder
%% passed in the second argument can be called to decode any nested record.
%% @end
%%--------------------------------------------------------------------
-callback db_decode(json_utils:json_term(), nested_record_decoder()) -> jsonable_record:record().


%%--------------------------------------------------------------------
%% @doc
%% Returns the current version of the record's definition (as defined in code).
%% The version is used to compare versions and trigger an upgrade if needed.
%% @end
%%--------------------------------------------------------------------
-callback version() -> record_version().


%%--------------------------------------------------------------------
%% @doc
%% Upgrades older records (must be implemented if record version > 1).
%% The upgrade is run for the encoded representation of the record.
%% @end
%%--------------------------------------------------------------------
-callback upgrade_encoded_record(record_version(), json_utils:json_term()) ->
    {record_version(), json_utils:json_term()}.


-optional_callbacks([upgrade_encoded_record/2]).

%%%===================================================================
%%% API functions
%%%===================================================================


-spec to_string(jsonable_record:record(), jsonable_record:record_type()) -> binary().
to_string(Record, RecordType) ->
    json_utils:encode(to_json(Record, RecordType)).


-spec from_string(binary(), jsonable_record:record_type()) -> jsonable_record:record().
from_string(JsonEncodedRecord, RecordType) ->
    from_json(json_utils:decode(JsonEncodedRecord), RecordType).



-spec to_json(jsonable_record:record()) -> json_utils:json_term().
to_json(JsonableRecord) ->
    to_json(JsonableRecord, utils:record_type(JsonableRecord)).

-spec to_json(jsonable_record:record(), jsonable_record:record_type()) ->
    json_utils:json_term().
to_json(Record, RecordType) ->
    #{
        <<"_version">> => RecordType:version(),
        <<"_data">> => RecordType:db_encode(Record, fun to_json/2)
    }.


-spec from_json(json_utils:json_term(), jsonable_record:record_type()) ->
    jsonable_record:record().
from_json(
    #{<<"_version">> := CurrentRecordVersion, <<"_data">> := RecordJson},
    RecordType
) ->
    TargetRecordVersion = RecordType:version(),
    UpgradedRecordData = upgrade_encoded_record(
        TargetRecordVersion, CurrentRecordVersion, RecordType, RecordJson
    ),
    RecordType:db_decode(UpgradedRecordData, fun from_json/2).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec upgrade_encoded_record(record_version(), record_version(), jsonable_record:record_type(), json_utils:json_term()) ->
    json_utils:json_term() | no_return().
upgrade_encoded_record(Version, Version, _RecordType, Json) ->
    Json;

upgrade_encoded_record(TargetVersion, CurrentVersion, RecordType, _Json) when CurrentVersion > TargetVersion ->
    ?emergency(
        "Upgrade requested for record '~p' with future version ~B (known versions up to: ~B)",
        [RecordType, CurrentVersion, TargetVersion]
    ),
    error({future_version, RecordType, CurrentVersion, TargetVersion});

upgrade_encoded_record(TargetVersion, CurrentVersion, RecordType, Json) ->
    {NewVersion, Json2} = try
        RecordType:upgrade_encoded_record(CurrentVersion, Json)
    catch
        error:undef ->
            ?emergency(
                "Missing upgrade procedure for record '~p' from version ~B to ~B",
                [RecordType, CurrentVersion, TargetVersion]
            ),
            error({missing_upgrader, RecordType, CurrentVersion, TargetVersion})
    end,
    upgrade_encoded_record(TargetVersion, NewVersion, RecordType, Json2).