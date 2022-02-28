%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module implementing the time series data type used in automation machinery.
%%%
%%% Time series measurements data is represented as an array of data points, each specifying
%%% the target time series name, measurement time and measurement value, e.g.:
%%%    [
%%%        {tsName: data_transferred, timestamp: 12345, value: 17},
%%%        {tsName: latency, timestamp: 12389, value: 8},
%%%        {tsName: video_files, timestamp: 14321, value: 385}
%%%    ]
%%%
%%% The @see atm_time_series_measurements_spec record defining value constraints serves as
%%% a contract that specifies what data points are expected in the array. If a data point
%%% that do not match any spec appears, the whole array is considered invalid.
%%%
%%% The purpose of this data type is to be consumed by a time series store.
%%% The target time series in the store is selected using the procedure described
%%% in @see atm_time_series_names.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_measurements_type).
-author("Lukasz Opiola").

-behaviour(atm_data_type).

-include("automation/automation.hrl").

%% atm_data_type callbacks
-export([is_instance/1]).
-export([value_constraints_to_json/1, value_constraints_from_json/1]).

%%%===================================================================
%%% atm_data_type callbacks
%%%===================================================================

%% @TODO VFS-7687 Implement all automation data types and validators
-spec is_instance(json_utils:json_term()) -> boolean().
is_instance(Value) when not is_list(Value) ->
    false;
is_instance(Measurements) ->
    lists:all(fun(Measurement) ->
        maps:is_key(<<"tsName">>, Measurement) andalso
            maps:is_key(<<"timestamp">>, Measurement) andalso
            maps:is_key(<<"value">>, Measurement)
    end, Measurements).


-spec value_constraints_to_json(atm_data_type:value_constraints()) -> json_utils:json_map().
value_constraints_to_json(#{specs := Specs}) ->
    #{<<"specs">> => jsonable_record:list_to_json(Specs, atm_time_series_measurements_spec)}.


-spec value_constraints_from_json(json_utils:json_map()) -> atm_data_type:value_constraints().
value_constraints_from_json(#{<<"specs">> := SpecsJson}) ->
    #{specs => jsonable_record:list_from_json(SpecsJson, atm_time_series_measurements_spec)}.
