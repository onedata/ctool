%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Base model for data specs used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_resource_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").
-include("errors.hrl").

%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type record() :: #atm_resource_spec{}.
-export_type([record/0]).

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_map().
to_json(R) ->
    #{
        <<"cpuRequested">> => parse_value(R#atm_resource_spec.cpu_requested, float, disallow_undefined, to_json),
        <<"cpuLimit">> => parse_value(R#atm_resource_spec.cpu_limit, float, allow_undefined, to_json),
        <<"memoryRequested">> => parse_value(R#atm_resource_spec.memory_requested, integer, disallow_undefined, to_json),
        <<"memoryLimit">> => parse_value(R#atm_resource_spec.memory_limit, integer, allow_undefined, to_json),
        <<"ephemeralStorageRequested">> => parse_value(R#atm_resource_spec.ephemeral_storage_requested, integer, disallow_undefined, to_json),
        <<"ephemeralStorageLimit">> => parse_value(R#atm_resource_spec.ephemeral_storage_limit, integer, allow_undefined, to_json)
    }.


-spec from_json(json_utils:json_map()) -> record().
from_json(J) ->
    #atm_resource_spec{
        cpu_requested = parse_value(maps:get(<<"cpuRequested">>, J), float, disallow_undefined, from_json),
        cpu_limit = parse_value(maps:get(<<"cpuLimit">>, J), float, allow_undefined, from_json),
        memory_requested = parse_value(maps:get(<<"memoryRequested">>, J), integer, disallow_undefined, from_json),
        memory_limit = parse_value(maps:get(<<"memoryLimit">>, J), integer, allow_undefined, from_json),
        ephemeral_storage_requested = parse_value(maps:get(<<"ephemeralStorageRequested">>, J), integer, disallow_undefined, from_json),
        ephemeral_storage_limit = parse_value(maps:get(<<"ephemeralStorageLimit">>, J), integer, allow_undefined, from_json)
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
%%% Helper functions
%%%===================================================================

%% @private
-spec parse_value(term(), float | integer, allow_undefined | disallow_undefined, to_json | from_json) ->
    float() | integer() | undefined | null.
parse_value(undefined, _, allow_undefined, to_json) ->
    null;
parse_value(null, _, allow_undefined, from_json) ->
    undefined;
parse_value(Value, integer, _, _) when is_integer(Value) andalso Value > 0 ->
    Value;
parse_value(Value, float, _, _) when is_float(Value) andalso Value > 0.0 ->
    Value;
parse_value(Value, float, UndefinedValuePolicy, Conversion) ->
    % accept also integers and convert them to float
    parse_value(Value, integer, UndefinedValuePolicy, Conversion) * 1.0;
parse_value(_, _, _, _) ->
    throw(?ERROR_BAD_DATA(<<"atmResourceSpec">>)).