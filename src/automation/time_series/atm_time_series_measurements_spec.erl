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
%%% This record is used as value constraints of the @see atm_time_series_measurements_type.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_measurements_spec).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").


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
    decode(RecordJson).

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
    decode(RecordJson).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec encode(record()) -> json_utils:json_term().
encode(Record) ->
    #{
        <<"nameMatcherType">> => atm_time_series_names:measurement_ts_name_matcher_type_to_json(
            Record#atm_time_series_measurements_spec.name_matcher_type
        ),
        <<"nameMatcher">> => atm_time_series_names:measurement_ts_name_matcher_to_json(
            Record#atm_time_series_measurements_spec.name_matcher
        ),
        <<"unit">> => time_series:unit_to_json(
            Record#atm_time_series_measurements_spec.unit
        )
    }.


%% @private
-spec decode(json_utils:json_term()) -> record().
decode(RecordJson) ->
    #atm_time_series_measurements_spec{
        name_matcher_type = atm_time_series_names:measurement_ts_name_matcher_type_from_json(
            maps:get(<<"nameMatcherType">>, RecordJson)
        ),
        name_matcher = atm_time_series_names:measurement_ts_name_matcher_from_json(
            maps:get(<<"nameMatcher">>, RecordJson)
        ),
        unit = time_series:unit_from_json(
            maps:get(<<"unit">>, RecordJson)
        )
    }.
