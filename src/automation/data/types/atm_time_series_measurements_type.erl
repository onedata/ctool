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
-export([encode_value_constraints/2, decode_value_constraints/3]).

%%%===================================================================
%%% atm_data_type callbacks
%%%===================================================================

%% @TODO VFS-7687 Implement all automation data types and validators
-spec is_instance(json_utils:json_term()) -> boolean().
is_instance(Value) when not is_list(Value) ->
    false;
is_instance(Measurements) ->
    lists:all(fun
        (#{
            <<"tsName">> := TsName,
            <<"timestamp">> := Timestamp,
            <<"value">> := Value
        }) when is_binary(TsName), is_integer(Timestamp), is_number(Value) ->
            true;
        (_) ->
            false
    end, Measurements).


-spec encode_value_constraints(atm_data_type:value_constraints(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_value_constraints(#{specs := Specs}, NestedRecordEncoder) ->
    #{<<"specs">> => [NestedRecordEncoder(S, atm_time_series_measurements_spec) || S <- Specs]}.


-spec decode_value_constraints(
    automation:validation_strategy(),
    json_utils:json_term(),
    persistent_record:nested_record_decoder()
) ->
    atm_data_type:value_constraints().
decode_value_constraints(skip_validation, #{<<"specs">> := SpecsJson}, NestedRecordDecoder) ->
    #{specs => [NestedRecordDecoder(S, atm_time_series_measurements_spec) || S <- SpecsJson]};
decode_value_constraints(validate, DataJson, NestedRecordDecoder) ->
    Result = #{specs := Specs} = decode_value_constraints(skip_validation, DataJson, NestedRecordDecoder),
    lists:foldl(fun(#atm_time_series_measurements_spec{name_matcher = NameMatcher}, AlreadyUsedNameMatchers) ->
        ordsets:is_element(NameMatcher, AlreadyUsedNameMatchers) andalso throw(
            ?ERROR_BAD_DATA(
                <<"valueConstraints.specs">>,
                <<"There cannot be two measurement specs with the same name matcher">>
            )
        ),
        ordsets:add_element(NameMatcher, AlreadyUsedNameMatchers)
    end, ordsets:new(), Specs),
    Result.
