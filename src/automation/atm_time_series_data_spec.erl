%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing time series specification used in automation machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_data_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").
-include("errors.hrl").


%% API
-export([name_selector_to_json/1, name_selector_from_json/1]).
-export([unit_to_json/1, unit_from_json/1]).
-export([validate_name/2]).

%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type name_selector() :: fixed | pattern.
-type unit() :: none
| milliseconds | seconds
| bits | bytes
| hertz | counts_per_sec | bytes_per_sec | ops_per_sec | requests_per_sec
| reads_per_sec | writes_per_sec | io_ops_per_sec
| percent | percent_normalized
| boolean
| {custom, automation:name()}.
-export_type([name_selector/0, unit/0]).

-type record() :: #atm_time_series_data_spec{}.
-export_type([record/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec name_selector_to_json(name_selector()) -> json_utils:json_term().
name_selector_to_json(fixed) -> <<"fixed">>;
name_selector_to_json(pattern) -> <<"pattern">>.


-spec name_selector_from_json(json_utils:json_term()) -> name_selector().
name_selector_from_json(<<"fixed">>) -> fixed;
name_selector_from_json(<<"pattern">>) -> pattern.


-spec unit_to_json(unit()) -> json_utils:json_term().
unit_to_json(none) -> <<"none">>;
unit_to_json(milliseconds) -> <<"milliseconds">>;
unit_to_json(seconds) -> <<"seconds">>;
unit_to_json(bits) -> <<"bits">>;
unit_to_json(bytes) -> <<"bytes">>;
unit_to_json(hertz) -> <<"hertz">>;
unit_to_json(counts_per_sec) -> <<"countsPerSec">>;
unit_to_json(bytes_per_sec) -> <<"bytesPerSec">>;
unit_to_json(ops_per_sec) -> <<"opsPerSec">>;
unit_to_json(requests_per_sec) -> <<"requestsPerSec">>;
unit_to_json(reads_per_sec) -> <<"readsPerSec">>;
unit_to_json(writes_per_sec) -> <<"writesPerSec">>;
unit_to_json(io_ops_per_sec) -> <<"ioOpsPerSec">>;
unit_to_json(percent) -> <<"percent">>;
unit_to_json(percent_normalized) -> <<"percentNormalized">>;
unit_to_json(boolean) -> <<"boolean">>;
unit_to_json({custom, UnitName}) -> <<"custom:", UnitName/binary>>.


-spec unit_from_json(json_utils:json_term()) -> unit().
unit_from_json(<<"none">>) -> none;
unit_from_json(<<"milliseconds">>) -> milliseconds;
unit_from_json(<<"seconds">>) -> seconds;
unit_from_json(<<"bits">>) -> bits;
unit_from_json(<<"bytes">>) -> bytes;
unit_from_json(<<"hertz">>) -> hertz;
unit_from_json(<<"countsPerSec">>) -> counts_per_sec;
unit_from_json(<<"bytesPerSec">>) -> bytes_per_sec;
unit_from_json(<<"opsPerSec">>) -> ops_per_sec;
unit_from_json(<<"requestsPerSec">>) -> requests_per_sec;
unit_from_json(<<"readsPerSec">>) -> reads_per_sec;
unit_from_json(<<"writesPerSec">>) -> writes_per_sec;
unit_from_json(<<"ioOpsPerSec">>) -> io_ops_per_sec;
unit_from_json(<<"percent">>) -> percent;
unit_from_json(<<"percentNormalized">>) -> percent_normalized;
unit_from_json(<<"boolean">>) -> boolean;
unit_from_json(<<"custom:", UnitName/binary>>) -> {custom, UnitName}.


-spec validate_name(name_selector(), automation:name()) -> ok | no_return().
validate_name(fixed, Binary) when is_binary(Binary) ->
    ok;
validate_name(pattern, Pattern) ->
    case string:split(Pattern, <<"*">>, all) of
        [_, _] ->
            ok;
        _ ->
            throw(?ERROR_BAD_DATA(<<"name">>, <<"The name pattern must contain exacly one wildcard character (*)">>))
    end.

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_map().
to_json(Record) ->
    encode(Record).


-spec from_json(json_utils:json_map()) -> record().
from_json(RecordJson) ->
    decode(json, RecordJson).

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, _NestedRecordEncoder) ->
    encode(Record).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, _NestedRecordDecoder) ->
    decode(db, RecordJson).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec encode(record()) -> json_utils:json_term().
encode(Record) ->
    #{
        <<"nameSelector">> => name_selector_to_json(Record#atm_time_series_data_spec.name_selector),
        <<"name">> => Record#atm_time_series_data_spec.name,
        <<"unit">> => unit_to_json(Record#atm_time_series_data_spec.unit)
    }.


%% @private
-spec decode(json | db, json_utils:json_term()) -> record().
decode(db, RecordJson) ->
    #atm_time_series_data_spec{
        name_selector = name_selector_from_json(maps:get(<<"nameSelector">>, RecordJson)),
        name = maps:get(<<"name">>, RecordJson),
        unit = unit_from_json(maps:get(<<"unit">>, RecordJson))
    };
decode(json, RecordJson) ->
    Spec = decode(db, RecordJson),
    validate_name(Spec#atm_time_series_data_spec.name_selector, Spec#atm_time_series_data_spec.name),
    Spec.