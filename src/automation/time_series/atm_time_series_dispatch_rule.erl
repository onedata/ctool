%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing time series dispatch rule used in automation machinery.
%%%
%%% Dispatch rules are used to determine the target time series when inserting a time
%%% series measurements array into a time series store - @see atm_time_series_names.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_dispatch_rule).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("automation/automation.hrl").


%% Jsonable record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).

-type record() :: #atm_time_series_dispatch_rule{}.
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
        <<"measurementTimeSeriesNameMatcherType">> => atm_time_series_names:measurement_ts_name_matcher_type_to_json(
            Record#atm_time_series_dispatch_rule.measurement_ts_name_matcher_type
        ),
        <<"measurementTimeSeriesNameMatcher">> => atm_time_series_names:measurement_ts_name_matcher_to_json(
            Record#atm_time_series_dispatch_rule.measurement_ts_name_matcher
        ),
        <<"targetTimeSeriesNameGenerator">> => atm_time_series_names:target_ts_name_generator_to_json(
            Record#atm_time_series_dispatch_rule.target_ts_name_generator
        ),
        <<"prefixCombiner">> => atm_time_series_names:prefix_combiner_to_json(
            Record#atm_time_series_dispatch_rule.prefix_combiner
        )
    }.


%% @private
-spec decode(json_utils:json_term()) -> record().
decode(RecordJson) ->
    #atm_time_series_dispatch_rule{
        measurement_ts_name_matcher_type = atm_time_series_names:measurement_ts_name_matcher_type_from_json(
            maps:get(<<"measurementTimeSeriesNameMatcherType">>, RecordJson)
        ),
        measurement_ts_name_matcher = atm_time_series_names:measurement_ts_name_matcher_from_json(
            maps:get(<<"measurementTimeSeriesNameMatcher">>, RecordJson)
        ),
        target_ts_name_generator = atm_time_series_names:measurement_ts_name_matcher_from_json(
            maps:get(<<"targetTimeSeriesNameGenerator">>, RecordJson)
        ),
        prefix_combiner = atm_time_series_names:prefix_combiner_from_json(
            maps:get(<<"prefixCombiner">>, RecordJson)
        )
    }.