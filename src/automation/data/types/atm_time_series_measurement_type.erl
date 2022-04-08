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
%%% Time series measurement represents as an input measurement for time series, specifying
%%% the target time series name, measurement time and measurement value, e.g.:
%%%    [
%%%        {tsName: data_transferred, timestamp: 12345, value: 17},
%%%        {tsName: latency, timestamp: 12389, value: 8},
%%%        {tsName: video_files, timestamp: 14321, value: 385}
%%%    ]
%%%
%%% A list of @see atm_time_series_measurement_spec records defining value constraints
%%% serve as a contract that specifies what data points are allowed. If a measurement
%%% does not match any of the specs, it is considered invalid.
%%%
%%% The purpose of this data type is to be consumed by a time series store.
%%% The target time series in the store is selected using the procedure described
%%% in @see atm_time_series_names.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_measurement_type).
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
is_instance(#{
    <<"tsName">> := TsName,
    <<"timestamp">> := Timestamp,
    <<"value">> := Value
}) when is_binary(TsName), is_integer(Timestamp), is_number(Value) ->
    true;
is_instance(_) ->
    false.


-spec encode_value_constraints(atm_data_type:value_constraints(), persistent_record:nested_record_encoder()) ->
    json_utils:json_term().
encode_value_constraints(#{specs := Specs}, NestedRecordEncoder) ->
    #{<<"specs">> => [NestedRecordEncoder(S, atm_time_series_measurement_spec) || S <- Specs]}.


-spec decode_value_constraints(
    automation:validation_strategy(),
    json_utils:json_term(),
    persistent_record:nested_record_decoder()
) ->
    atm_data_type:value_constraints().
decode_value_constraints(skip_validation, #{<<"specs">> := SpecsJson}, NestedRecordDecoder) ->
    #{specs => [NestedRecordDecoder(S, atm_time_series_measurement_spec) || S <- SpecsJson]};
decode_value_constraints(validate, DataJson, NestedRecordDecoder) ->
    Result = #{specs := Specs} = decode_value_constraints(skip_validation, DataJson, NestedRecordDecoder),
    lists:foldl(fun(#atm_time_series_measurement_spec{name_matcher = NameMatcher}, AlreadyUsedNameMatchers) ->
        ordsets:is_element(NameMatcher, AlreadyUsedNameMatchers) andalso throw(
            ?ERROR_BAD_DATA(
                <<"valueConstraints.specs">>,
                <<"There cannot be two measurement specs with the same name matcher">>
            )
        ),
        ordsets:add_element(NameMatcher, AlreadyUsedNameMatchers)
    end, ordsets:new(), Specs),
    Result.
