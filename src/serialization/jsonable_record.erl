%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module defines jsonable record interface - implemented for erlang
%%% records that are to be encoded/decoded to/from JSON. Each such record should
%%% have a dedicated module implementing the callbacks.
%%%
%%% This interface is intended especially for records transmitted via system APIs
%%% and sub-records of parent persistent records - those that are not stored
%%% directly in the database, but as a part of a bigger document.
%%% @end
%%%-------------------------------------------------------------------
-module(jsonable_record).
-author("Lukasz Opiola").

%% API
-export([to_json/1, to_json/2]).
-export([from_json/2]).
-export([list_to_json/2, list_from_json/2]).

-type record() :: tuple().
-type record_type() :: module().
-export_type([record/0, record_type/0]).

% Indicates if record validation should be performed.
-type validation_strategy() :: validate | skip_validation.
-export_type([validation_strategy/0]).


%%%===================================================================
%%% jsonable_record behaviour
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Encodes record as json object.
%% @end
%%--------------------------------------------------------------------
-callback to_json(record()) -> json_utils:json_term().


%%--------------------------------------------------------------------
%% @doc
%% Decodes record from json object.
%% @end
%%--------------------------------------------------------------------
-callback from_json(json_utils:json_term()) -> record().

%%%===================================================================
%%% API functions
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(JsonableRecord) ->
    to_json(JsonableRecord, utils:record_type(JsonableRecord)).

-spec to_json(record(), record_type()) -> json_utils:json_term().
to_json(JsonableRecord, RecordType) ->
    RecordType:to_json(JsonableRecord).


-spec from_json(json_utils:json_term(), record_type()) -> record().
from_json(EncodedRecord, RecordType) ->
    RecordType:from_json(EncodedRecord).


-spec list_to_json([record()], record_type()) -> [json_utils:json_term()].
list_to_json(JsonableRecords, RecordType) ->
    [RecordType:to_json(R) || R <- JsonableRecords].


-spec list_from_json([json_utils:json_term()], record_type()) -> [record()].
list_from_json(EncodedRecords, RecordType) ->
    [RecordType:from_json(R) || R <- EncodedRecords].
