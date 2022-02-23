%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing time series measurements specification used in automation machinery.
%%%
%%% This record is used as value constraints of the atm_time_series_measurements_type - see
%%% the module for more information.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_measurements_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").
-include("errors.hrl").


%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type record() :: #atm_time_series_measurements_spec{}.
-export_type([record/0]).

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_map().
to_json(Record) ->
    encode(Record).


-spec from_json(json_utils:json_map()) -> record().
from_json(RecordJson) ->
    decode(validate, RecordJson).

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
    decode(skip_validation, RecordJson).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec encode(record()) -> json_utils:json_term().
encode(Record) ->
    #{
        <<"nameSelector">> => atm_time_series_attribute:name_selector_to_json(Record#atm_time_series_measurements_spec.name_selector),
        <<"name">> => atm_time_series_attribute:name_to_json(Record#atm_time_series_measurements_spec.name),
        <<"unit">> => atm_time_series_attribute:unit_to_json(Record#atm_time_series_measurements_spec.unit)
    }.


%% @private
-spec decode(validate | skip_validation, json_utils:json_term()) -> record().
decode(skip_validation, RecordJson) ->
    #atm_time_series_measurements_spec{
        name_selector = atm_time_series_attribute:name_selector_from_json(maps:get(<<"nameSelector">>, RecordJson)),
        name = atm_time_series_attribute:name_from_json(maps:get(<<"name">>, RecordJson)),
        unit = atm_time_series_attribute:unit_from_json(maps:get(<<"unit">>, RecordJson))
    };
decode(validate, RecordJson) ->
    Spec = decode(skip_validation, RecordJson),
    atm_time_series_attribute:validate_name(
        Spec#atm_time_series_measurements_spec.name_selector,
        Spec#atm_time_series_measurements_spec.name
    ),
    Spec.