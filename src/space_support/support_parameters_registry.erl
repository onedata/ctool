%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing a registry of support parameters per supporting provider.
%%% @end
%%%-------------------------------------------------------------------
-module(support_parameters_registry).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("space_support/support_parameters.hrl").


%% API
-export([update_entry/3, remove_entry/2]).

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type registry() :: #{onedata:provider_id() => support_parameters:record()}.
-export_type([registry/0]).

-type record() :: #support_parameters_registry{}.
-export_type([record/0]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @see support_parameters:update/2
%% Takes default parameters as previous value if there was no preexisting entry.
%% @end
%%--------------------------------------------------------------------
-spec update_entry(onedata:provider_id(), support_parameters:record(), record()) ->
    {ok, record()} | errors:error().
update_entry(ProviderId, ParametersOverlay, Record = #support_parameters_registry{registry = Registry}) ->
    PreviousParameters = maps:get(ProviderId, Registry, ?DEFAULT_SUPPORT_PARAMETERS),
    case support_parameters:update(PreviousParameters, ParametersOverlay) of
        {error, _} = Error ->
            Error;
        {ok, NewParameters} ->
            {ok, Record#support_parameters_registry{
                registry = Registry#{
                    ProviderId => NewParameters
                }
            }}
    end.


-spec remove_entry(onedata:provider_id(), record()) -> record().
remove_entry(ProviderId, Record) ->
    Record#support_parameters_registry{
        registry = maps:remove(ProviderId, Record#support_parameters_registry.registry)
    }.

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    encode_with(Record, fun jsonable_record:to_json/2).


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    decode_with(RecordJson, fun jsonable_record:from_json/2).

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, NestedRecordEncoder) ->
    encode_with(Record, NestedRecordEncoder).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, NestedRecordDecoder) ->
    decode_with(RecordJson, NestedRecordDecoder).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec encode_with(record(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_with(Record, NestedRecordEncoder) ->
    maps:map(fun(_ProviderId, SupportParameters) ->
        NestedRecordEncoder(SupportParameters, support_parameters)
    end, Record#support_parameters_registry.registry).


%% @private
-spec decode_with(json_utils:json_term(), persistent_record:nested_record_decoder()) ->
    record().
decode_with(RecordJson, NestedRecordDecoder) ->
    #support_parameters_registry{registry = maps:map(fun(_ProviderId, SupportParameters) ->
        NestedRecordDecoder(SupportParameters, support_parameters)
    end, RecordJson)}.
