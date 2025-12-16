%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of the onedata_calver module.
%%% @end
%%%-------------------------------------------------------------------
-module(onedata_calver_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


%%%===================================================================
%%% Tests
%%%===================================================================


invalid_version_test() ->
    ?assertException(error, {invalid_version, <<"">>}, cmp(<<"20.02.0-alpha17">>, <<"">>)),
    ?assertException(error, {invalid_version, <<"20-02.0-alpha17">>}, cmp_year(<<"20-02.0-alpha17">>, <<"20.02.0">>)),
    ?assertException(error, {invalid_version, <<"gibberish">>}, cmp(<<"25.1.2-alpha.25">>, <<"gibberish">>)),
    ?assertException(error, {invalid_version, <<"2000-rc6">>}, cmp_year(<<"2000-rc6">>, <<"25.1.2-rc.1">>)),
    ?assertException(error, {invalid_version, <<"25.1.1.1">>}, cmp(<<"21.02.8">>, <<"25.1.1.1">>)),
    ?assertException(error, {invalid_version, <<"25.1.1.1-rc7">>}, cmp_year(<<"21.02.8">>, <<"25.1.1.1-rc7">>)).


basic_equality_test() ->
    ?assertEqual(equal, cmp(<<"20.02.0-alpha17">>, <<"20.02.0-alpha17">>)),
    ?assertEqual(equal, cmp(<<"20.02.0-alpha17">>, <<"20.02.0-alpha17">>)),
    ?assertEqual(equal, cmp(<<"21.02.8">>, <<"21.02.8">>)),
    ?assertEqual(equal, cmp(<<"25.0">>, <<"25.0">>)),
    ?assertEqual(equal, cmp(<<"25.0">>, <<"25.0.0">>)),
    ?assertEqual(equal, cmp(<<"25.1.2">>, <<"25.1.2">>)),
    ?assertEqual(equal, cmp(<<"25.1.2-alpha.25">>, <<"25.1.2-alpha.25">>)),
    ?assertEqual(equal, cmp(<<"25.1-alpha.25">>, <<"25.1.0-alpha.25">>)),
    ?assertEqual(equal, cmp(<<"25.1.2-rc.1">>, <<"25.1.2-rc.1">>)).


year_comparison_test() ->
    ?assertEqual(lower, cmp(<<"21.02.8">>, <<"25.0.1">>)),
    ?assertEqual(lower, cmp(<<"25.1">>, <<"26.0">>)),
    ?assertEqual(lower, cmp_year(<<"25.1">>, <<"26.0">>)),

    ?assertEqual(greater, cmp(<<"26.0">>, <<"25.9.9">>)),
    ?assertEqual(greater, cmp_year(<<"26.0">>, <<"25.9.9">>)),

    ?assertEqual(greater, cmp(<<"26.0">>, <<"18.0.0-rc2">>)),
    ?assertEqual(greater, cmp_year(<<"26.0">>, <<"18.0.0-rc2">>)),

    ?assertEqual(greater, cmp(<<"29.0">>, <<"26.19.19">>)),
    ?assertEqual(greater, cmp_year(<<"29.0">>, <<"26.19.19">>)),

    ?assertEqual(greater, cmp(<<"29.0">>, <<"20.02.1">>)),
    ?assertEqual(greater, cmp_year(<<"29.0">>, <<"20.02.1">>)),

    ?assertEqual(equal, cmp(<<"27.0">>, <<"27.0">>)),
    ?assertEqual(equal, cmp_year(<<"27.0">>, <<"27.1.1">>)).


minor_and_patch_comparison_test() ->
    ?assertEqual(lower, cmp(<<"21.02.7">>, <<"21.02.8">>)),
    ?assertEqual(lower, cmp(<<"20.02.0-beta3">>, <<"21.02.8">>)),
    ?assertEqual(lower, cmp(<<"25.0">>, <<"25.1">>)),
    ?assertEqual(lower, cmp(<<"25.1">>, <<"25.10">>)),
    ?assertEqual(lower, cmp(<<"25.10">>, <<"25.100">>)),
    ?assertEqual(greater, cmp(<<"25.0.1">>, <<"25.0">>)),
    ?assertEqual(greater, cmp(<<"25.0.1">>, <<"18.02.0-rc7">>)),
    ?assertEqual(lower, cmp(<<"25.1">>, <<"25.1.1">>)),
    ?assertEqual(greater, cmp(<<"25.7">>, <<"25.0.1">>)),
    ?assertEqual(greater, cmp(<<"25.2.3">>, <<"25.2.2">>)).


label_vs_full_release_test() ->
    ?assertEqual(lower, cmp(<<"19.02.0-beta2">>, <<"19.02.0">>)),
    ?assertEqual(lower, cmp(<<"20.02.0-alpha1">>, <<"20.02.0">>)),
    ?assertEqual(lower, cmp(<<"21.02.0-rc3">>, <<"21.02.0">>)),
    ?assertEqual(lower, cmp(<<"25.0-alpha.1">>, <<"25.0">>)),
    ?assertEqual(lower, cmp(<<"25.0-rc.9">>, <<"25.0">>)),
    ?assertEqual(greater, cmp(<<"25.0">>, <<"25.0-beta.1">>)).


label_ordering_test() ->
    assert_ordered([
        <<"18.02.0-alpha1">>,
        <<"18.02.0-rc3">>,
        <<"19.02.0-beta6">>,
        <<"20.02.0-alpha1">>,
        <<"20.02.0-beta1">>,
        <<"20.02.0-rc1">>,
        <<"25.0-alpha.1">>,
        <<"25.0-alpha.2">>,
        <<"25.0.0-beta.1">>,
        <<"25.0-beta.23">>,
        <<"25.0-rc.1">>,
        <<"25.0.0-rc.10">>,
        <<"25.0-rc.100">>,
        <<"25.0.0-rc.1000">>,
        <<"25.0">>
    ]).


label_ordinal_comparison_test() ->
    ?assertEqual(lower, cmp(<<"20.02.0-alpha26">>, <<"20.02.0-alpha27">>)),
    ?assertEqual(lower, cmp(<<"25.0-alpha.1">>, <<"25.0-alpha.2">>)),
    ?assertEqual(greater, cmp(<<"25.0-beta.10">>, <<"25.0-beta.2">>)),
    ?assertEqual(equal, cmp(<<"25.0-rc.3">>, <<"25.0-rc.3">>)).


patch_after_full_release_test() ->
    assert_ordered([
        <<"25.0">>,
        <<"25.0.1">>,
        <<"25.0.2">>,
        <<"25.1">>,
        <<"25.1.1">>
    ]).


mixed_sequence_test() ->
    assert_ordered([
        <<"18.02.0-alpha1">>,
        <<"18.02.0-rc3">>,

        <<"19.02.0-beta6">>,
        <<"19.02.1">>,
        <<"19.02.2">>,
        <<"19.02.3">>,

        <<"20.02.0-alpha1">>,
        <<"20.02.0-beta1">>,
        <<"20.02.0-rc1">>,
        <<"20.02.0">>,
        <<"20.02.17">>,

        <<"21.02.1">>,
        <<"21.02.4">>,
        <<"21.02.8">>,

        <<"25.1-alpha.1">>,
        <<"25.1.0-alpha.2">>,
        <<"25.1-beta.1">>,
        <<"25.1-rc.1">>,
        <<"25.1">>,
        <<"25.1.1">>,
        <<"25.1.2-alpha.1">>,
        <<"25.1.2-beta.1">>,
        <<"25.1.2-rc.1">>,
        <<"25.1.2-rc.2">>,
        <<"25.1.2">>,
        <<"25.2">>,
        <<"25.2.1">>,

        <<"26.0-beta.15">>
    ]).


%%%===================================================================
%%% Helpers
%%%===================================================================


-spec cmp(onedata_calver:version(), onedata_calver:version()) -> lower | equal | greater.
cmp(V1, V2) ->
    onedata_calver:compare(V1, V2).


-spec cmp_year(onedata_calver:version(), onedata_calver:version()) -> lower | equal | greater.
cmp_year(V1, V2) ->
    onedata_calver:compare_year(V1, V2).


-spec assert_ordered([onedata_calver:version()]) -> ok.
assert_ordered([]) ->
    ok;
assert_ordered([_]) ->
    ok;
assert_ordered([A, B | Rest]) ->
    ?assertEqual(lower, cmp(A, B)),
    ?assert(lists:member(cmp_year(A, B), [lower, equal])),
    assert_ordered([B | Rest]).


-endif.
