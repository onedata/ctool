%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is responsible for encoding/decoding records implementing
%%% 'jsonable_record' behaviour.
%%% @end
%%%-------------------------------------------------------------------
-module(record_json_encoder).
-author("Bartosz Walkowicz").

-include_lib("ctool/include/logging.hrl").

%% API
-export([encode/1, decode/1]).


-type record_name() :: module().
-type record_version() :: non_neg_integer().
-type record() :: tuple().

-export_type([record_name/0, record_version/0, record/0]).


%%%===================================================================
%%% API functions
%%%===================================================================


-spec encode(record()) -> binary().
encode(JsonableRecord) ->
    RecordType = utils:record_type(JsonableRecord),

    json_utils:encode(#{
        <<"_recordType">> => atom_to_binary(RecordType, utf8),
        <<"_version">> => RecordType:version(),
        <<"_data">> => RecordType:to_json(JsonableRecord)
    }).


-spec decode(binary()) -> record().
decode(EncodedRecord) ->
    #{
        <<"_recordType">> := RecordBin,
        <<"_version">> := CurrentRecordVersion,
        <<"_data">> := RecordJson
    } = json_utils:decode(EncodedRecord),

    RecordType = binary_to_existing_atom(RecordBin, utf8),
    TargetRecordVersion = RecordType:version(),

    UpgradedRecordData = upgrade_record_json(
        TargetRecordVersion, CurrentRecordVersion, RecordType, RecordJson
    ),

    RecordType:from_json(UpgradedRecordData).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec upgrade_record_json(record_version(), record_version(), record_name(), json_utils:json_map()) ->
    json_utils:json_map() | no_return().
upgrade_record_json(Version, Version, _RecordName, Json) ->
    Json;

upgrade_record_json(TargetVersion, CurrentVersion, RecordName, _Json) when CurrentVersion > TargetVersion ->
    ?emergency(
        "Upgrade requested for json RecordName '~p' with future version ~p (known versions up to: ~p)",
        [RecordName, CurrentVersion, TargetVersion]
    ),
    error({future_version, RecordName, CurrentVersion, TargetVersion});

upgrade_record_json(TargetVersion, CurrentVersion, RecordName, Json) ->
    {NewVersion, Json2} = RecordName:upgrade_json(CurrentVersion, Json),
    upgrade_record_json(TargetVersion, NewVersion, RecordName, Json2).