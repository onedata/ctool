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

-export_type([file_type/0, record/0]).


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
db_encode(#atm_file_data_spec{file_type = FileType}, _NestedRecordEncoder) ->
    #{<<"fileType">> => file_type_to_json(FileType)}.


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, _NestedRecordDecoder) ->
    FileTypeJson = maps:get(<<"fileType">>, RecordJson, <<"ANY">>),
    #atm_file_data_spec{file_type = file_type_from_json(FileTypeJson)}.


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
