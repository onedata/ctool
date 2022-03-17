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


find_matching_matcher_test() ->
    MeasurementTSName = <<"file_count_mp3">>,

    MatchingMatcherAlpha = {exact, <<"file_count_mp3">>},
    MatchingMatcherGamma = {has_prefix, <<"file_count_">>},
    MatchingMatcherDelta = {has_prefix, <<"f">>},

    NonmatchingMatcherTheta = {exact, <<"file_count_">>},
    NonmatchingMatcherKappa = {has_prefix, <<"count_">>},
    NonmatchingMatcherOmega = {has_prefix, <<"mp3">>},

    find_matching_matcher_test_base(MeasurementTSName, error, []),
    find_matching_matcher_test_base(MeasurementTSName, error, [
        NonmatchingMatcherKappa,
        NonmatchingMatcherOmega,
        NonmatchingMatcherTheta
    ]),
    find_matching_matcher_test_base(MeasurementTSName, {ok, MatchingMatcherAlpha}, [
        NonmatchingMatcherKappa,
        NonmatchingMatcherOmega,
        MatchingMatcherAlpha,
        NonmatchingMatcherTheta,
        MatchingMatcherGamma
    ]),
    find_matching_matcher_test_base(MeasurementTSName, {ok, MatchingMatcherDelta}, [
        MatchingMatcherDelta,
        NonmatchingMatcherKappa,
        NonmatchingMatcherOmega,
        MatchingMatcherAlpha,
        NonmatchingMatcherTheta
    ]),
    find_matching_matcher_test_base(MeasurementTSName, {ok, MatchingMatcherGamma}, [
        NonmatchingMatcherKappa,
        NonmatchingMatcherOmega,
        NonmatchingMatcherTheta,
        MatchingMatcherGamma
    ]).

%% @private
find_matching_matcher_test_base(MeasurementTSName, ExpectedResult, MatchersToCheck) ->
    FindMeasurementsSpecExpectedResult = case ExpectedResult of
        error ->
            error;
        {ok, Matcher1} ->
            {ok, matcher_to_measurements_spec(Matcher1)}
    end,
    ?assertEqual(
        FindMeasurementsSpecExpectedResult,
        atm_time_series_names:find_matching_measurements_spec(
            MeasurementTSName,
            [matcher_to_measurements_spec(M) || M <- MatchersToCheck]
        )
    ),

    FindDispatchRuleExpectedResult = case ExpectedResult of
        error ->
            error;
        {ok, Matcher2} ->
            {ok, matcher_to_dispatch_rule(Matcher2)}
    end,
    ?assertEqual(
        FindDispatchRuleExpectedResult,
        atm_time_series_names:find_matching_dispatch_rule(
            MeasurementTSName,
            [matcher_to_dispatch_rule(M) || M <- MatchersToCheck]
        )
    ).

%% @private
matcher_to_measurements_spec({NameMatcherType, NameMatcher}) ->
    #atm_time_series_measurements_spec{
        name_matcher_type = NameMatcherType,
        name_matcher = NameMatcher
    }.

%% @private
matcher_to_dispatch_rule({NameMatcherType, NameMatcher}) ->
    #atm_time_series_dispatch_rule{
        measurement_ts_name_matcher_type = NameMatcherType,
        measurement_ts_name_matcher = NameMatcher
    }.


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
        TimeSeriesSchema = #atm_time_series_schema{
            name_generator_type = TargetTsNameMatcherType,
            name_generator = TargetTsNameMatcher
        },
        TimeSeriesDispatchRule = #atm_time_series_dispatch_rule{
            measurement_ts_name_matcher_type = MeasurementTsNameMatcherType,
            measurement_ts_name_matcher = MeasurementTsNameMatcher,
            target_ts_name_generator = TargetTsNameMatcher,
            prefix_combiner = PrefixCombiner
        },

        ?assertEqual({ok, TimeSeriesSchema}, atm_time_series_names:find_referenced_time_series_schema(
            TimeSeriesDispatchRule, [TimeSeriesSchema]
        )),

        ?assertEqual(ExpectedFinalTsName, atm_time_series_names:resolve_target_ts_name(
            MeasurementTsName, TimeSeriesSchema, TimeSeriesDispatchRule
        ))
    end, PrefixCombinerVariants).


-endif.