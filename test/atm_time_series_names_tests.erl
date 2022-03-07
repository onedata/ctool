%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of atm_time_series_name modules.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_time_series_names_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("automation/automation.hrl").


find_matching_measurements_spec_test() ->
    MeasurementTSName = <<"file_count_mp3">>,

    MatchingSpecAlpha = #atm_time_series_measurements_spec{name_matcher_type = exact, name_matcher = <<"file_count_mp3">>},
    MatchingSpecGamma = #atm_time_series_measurements_spec{name_matcher_type = has_prefix, name_matcher = <<"file_count_">>},
    MatchingSpecDelta = #atm_time_series_measurements_spec{name_matcher_type = has_prefix, name_matcher = <<"f">>},

    NonmatchingSpecTheta = #atm_time_series_measurements_spec{name_matcher_type = exact, name_matcher = <<"file_count_">>},
    NonmatchingSpecKappa = #atm_time_series_measurements_spec{name_matcher_type = has_prefix, name_matcher = <<"count_">>},
    NonmatchingSpecOmega = #atm_time_series_measurements_spec{name_matcher_type = has_prefix, name_matcher = <<"mp3">>},

    ?assertEqual(error, atm_time_series_names:find_matching_measurements_spec(MeasurementTSName, [])),
    ?assertEqual(error, atm_time_series_names:find_matching_measurements_spec(MeasurementTSName, [
        NonmatchingSpecKappa,
        NonmatchingSpecOmega,
        NonmatchingSpecTheta
    ])),
    ?assertEqual({ok, MatchingSpecAlpha}, atm_time_series_names:find_matching_measurements_spec(MeasurementTSName, [
        NonmatchingSpecKappa,
        NonmatchingSpecOmega,
        MatchingSpecAlpha,
        NonmatchingSpecTheta,
        MatchingSpecGamma
    ])),
    ?assertEqual({ok, MatchingSpecDelta}, atm_time_series_names:find_matching_measurements_spec(MeasurementTSName, [
        MatchingSpecDelta,
        NonmatchingSpecKappa,
        NonmatchingSpecOmega,
        MatchingSpecAlpha,
        NonmatchingSpecTheta
    ])),
    ?assertEqual({ok, MatchingSpecGamma}, atm_time_series_names:find_matching_measurements_spec(MeasurementTSName, [
        NonmatchingSpecKappa,
        NonmatchingSpecOmega,
        NonmatchingSpecTheta,
        MatchingSpecGamma
    ])).


-record(testcase, {
    measurement_ts_name :: atm_time_series_names:measurement_ts_name(),
    measurement_ts_name_matcher :: {atm_time_series_names:measurement_ts_name_matcher_type(), atm_time_series_names:measurement_ts_name_matcher()},
    target_ts_name_generator :: {atm_time_series_names:target_ts_name_generator_type(), atm_time_series_names:target_ts_name_generator()},
    prefix_combiner_variants :: [atm_time_series_names:prefix_combiner()],
    expected_final_ts_name :: binary()
}).

resolve_target_ts_name_test() ->
    run_testcase(#testcase{
        measurement_ts_name = <<"files_mp3">>,
        measurement_ts_name_matcher = {exact, <<"files_mp3">>},
        target_ts_name_generator = {exact, <<"count_mp3">>},
        prefix_combiner_variants = [concatenate, converge, overwrite],
        expected_final_ts_name = <<"count_mp3">>
    }),
    run_testcase(#testcase{
        measurement_ts_name = <<"files_mp3">>,
        measurement_ts_name_matcher = {has_prefix, <<"files_">>},
        target_ts_name_generator = {exact, <<"count_mp3">>},
        prefix_combiner_variants = [concatenate, converge, overwrite],
        expected_final_ts_name = <<"count_mp3">>
    }),
    run_testcase(#testcase{
        measurement_ts_name = <<"files_mp3">>,
        measurement_ts_name_matcher = {exact, <<"files_mp3">>},
        target_ts_name_generator = {add_prefix, <<"count_">>},
        prefix_combiner_variants = [concatenate, converge, overwrite],
        expected_final_ts_name = <<"count_files_mp3">>
    }),
    run_testcase(#testcase{
        measurement_ts_name = <<"files_mp3">>,
        measurement_ts_name_matcher = {has_prefix, <<"files_">>},
        target_ts_name_generator = {add_prefix, <<"count_">>},
        prefix_combiner_variants = [concatenate, converge],
        expected_final_ts_name = <<"count_files_mp3">>
    }),
    run_testcase(#testcase{
        measurement_ts_name = <<"files_mp3">>,
        measurement_ts_name_matcher = {has_prefix, <<"files_">>},
        target_ts_name_generator = {add_prefix, <<"count_">>},
        prefix_combiner_variants = [overwrite],
        expected_final_ts_name = <<"count_mp3">>
    }),
    run_testcase(#testcase{
        measurement_ts_name = <<"count_mp3">>,
        measurement_ts_name_matcher = {has_prefix, <<"count_">>},
        target_ts_name_generator = {add_prefix, <<"count_">>},
        prefix_combiner_variants = [concatenate],
        expected_final_ts_name = <<"count_count_mp3">>
    }),
    run_testcase(#testcase{
        measurement_ts_name = <<"count_mp3">>,
        measurement_ts_name_matcher = {has_prefix, <<"count_">>},
        target_ts_name_generator = {add_prefix, <<"count_">>},
        prefix_combiner_variants = [converge, overwrite],
        expected_final_ts_name = <<"count_mp3">>
    }),
    run_testcase(#testcase{
        measurement_ts_name = <<"count_f_mp3">>,
        measurement_ts_name_matcher = {has_prefix, <<"count_f_">>},
        target_ts_name_generator = {add_prefix, <<"count_">>},
        prefix_combiner_variants = [concatenate],
        expected_final_ts_name = <<"count_count_f_mp3">>
    }),
    run_testcase(#testcase{
        measurement_ts_name = <<"count_f_mp3">>,
        measurement_ts_name_matcher = {has_prefix, <<"count_f_">>},
        target_ts_name_generator = {add_prefix, <<"count_">>},
        prefix_combiner_variants = [converge],
        expected_final_ts_name = <<"count_f_mp3">>
    }),
    run_testcase(#testcase{
        measurement_ts_name = <<"count_f_mp3">>,
        measurement_ts_name_matcher = {has_prefix, <<"count_f_">>},
        target_ts_name_generator = {add_prefix, <<"count_">>},
        prefix_combiner_variants = [overwrite],
        expected_final_ts_name = <<"count_mp3">>
    }),
    run_testcase(#testcase{
        measurement_ts_name = <<"cmp3">>,
        measurement_ts_name_matcher = {has_prefix, <<"c">>},
        target_ts_name_generator = {add_prefix, <<"count_">>},
        prefix_combiner_variants = [concatenate, converge],
        expected_final_ts_name = <<"count_cmp3">>
    }),
    run_testcase(#testcase{
        measurement_ts_name = <<"cmp3">>,
        measurement_ts_name_matcher = {has_prefix, <<"c">>},
        target_ts_name_generator = {add_prefix, <<"count_">>},
        prefix_combiner_variants = [overwrite],
        expected_final_ts_name = <<"count_mp3">>
    }).


run_testcase(#testcase{
    measurement_ts_name = MeasurementTsName,
    measurement_ts_name_matcher = {MeasurementTsNameMatcherType, MeasurementTsNameMatcher},
    target_ts_name_generator = {TargetTsNameMatcherType, TargetTsNameMatcher},
    prefix_combiner_variants = PrefixCombinerVariants,
    expected_final_ts_name = ExpectedFinalTsName
}) ->
    lists:foreach(fun(PrefixCombiner) ->
        TimeSeriesMeasurementsSpec = #atm_time_series_measurements_spec{
            name_matcher_type = MeasurementTsNameMatcherType,
            name_matcher = MeasurementTsNameMatcher
        },
        TimeSeriesSchema = #atm_time_series_schema{
            name_generator_type = TargetTsNameMatcherType,
            name_generator = TargetTsNameMatcher
        },
        TimeSeriesDispatchRule = #atm_time_series_dispatch_rule{
            measurement_ts_name_matcher = MeasurementTsNameMatcher,
            target_ts_name_generator = TargetTsNameMatcher,
            prefix_combiner = PrefixCombiner
        },

        ?assertEqual({ok, TimeSeriesDispatchRule}, atm_time_series_names:find_referencing_dispatch_rule(
            TimeSeriesMeasurementsSpec, [TimeSeriesDispatchRule]
        )),
        ?assertEqual({ok, TimeSeriesSchema}, atm_time_series_names:find_referenced_time_series_schema(
            TimeSeriesDispatchRule, [TimeSeriesSchema]
        )),

        ?assertEqual(ExpectedFinalTsName, atm_time_series_names:resolve_target_ts_name(
            MeasurementTsName, TimeSeriesMeasurementsSpec, TimeSeriesSchema, PrefixCombiner
        ))
    end, PrefixCombinerVariants).


-endif.