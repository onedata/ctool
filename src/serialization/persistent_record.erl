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
%%%
%%% This behaviour extends the jsonable_record behaviour, but due to Erlang's
%%% lack of inheritance, some callbacks are duplicated.
%%% @end
%%%-------------------------------------------------------------------
-module(persistent_record).
-author("Lukasz Opiola").

-include_lib("ctool/include/logging.hrl").

%% API
-export([encode/2, decode/2]).

-type record_version() :: non_neg_integer().
-export_type([record_version/0]).

%%%===================================================================
%%% persistent_record behaviour definition
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Encodes record as json object.
%% @end
%%--------------------------------------------------------------------
-callback to_json(jsonable_record:record()) -> json_utils:json_term().


%%--------------------------------------------------------------------
%% @doc
%% Decodes record from json object.
%% @end
%%--------------------------------------------------------------------
-callback from_json(json_utils:json_term()) -> jsonable_record:record().


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
%% @end
%%--------------------------------------------------------------------
-callback upgrade_json(record_version(), json_utils:json_term()) ->
    {record_version(), json_utils:json_term()}.


-optional_callbacks([upgrade_json/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec encode(jsonable_record:record(), jsonable_record:record_type()) -> binary().
encode(JsonableRecord, RecordType) ->
    json_utils:encode(#{
        <<"_version">> => RecordType:version(),
        <<"_data">> => RecordType:to_json(JsonableRecord)
    }).


-spec decode(binary(), jsonable_record:record_type()) -> jsonable_record:record().
decode(EncodedRecord, RecordType) ->
    #{
        <<"_version">> := CurrentRecordVersion,
        <<"_data">> := RecordJson
    } = json_utils:decode(EncodedRecord),

    TargetRecordVersion = RecordType:version(),

    UpgradedRecordData = upgrade_record_json(
        TargetRecordVersion, CurrentRecordVersion, RecordType, RecordJson
    ),

    RecordType:from_json(UpgradedRecordData).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec upgrade_record_json(record_version(), record_version(), jsonable_record:record_type(), json_utils:json_term()) ->
    json_utils:json_term() | no_return().
upgrade_record_json(Version, Version, _RecordType, Json) ->
    Json;

upgrade_record_json(TargetVersion, CurrentVersion, RecordType, _Json) when CurrentVersion > TargetVersion ->
    ?emergency(
        "Upgrade requested for record '~p' with future version ~B (known versions up to: ~B)",
        [RecordType, CurrentVersion, TargetVersion]
    ),
    error({future_version, RecordType, CurrentVersion, TargetVersion});

upgrade_record_json(TargetVersion, CurrentVersion, RecordType, Json) ->
    {NewVersion, Json2} = try
        RecordType:upgrade_json(CurrentVersion, Json)
    catch
        error:undef ->
            ?emergency(
                "Missing upgrade procedure for record '~p' from version ~B to ~B",
                [RecordType, CurrentVersion, TargetVersion]
            ),
            error({missing_upgrader, RecordType, CurrentVersion, TargetVersion})
    end,
    upgrade_record_json(TargetVersion, NewVersion, RecordType, Json2).