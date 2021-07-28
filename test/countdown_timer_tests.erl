%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Eunit tests for countdown_timer module.
%%%--------------------------------------------------------------------
-module(countdown_timer_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Eunit tests
%%%===================================================================

countdown_timer_test_() ->
    {foreach,
        fun() -> clock_freezer_mock:setup_for_eunit([?MODULE]) end,
        fun(_) -> clock_freezer_mock:teardown_for_eunit() end,
        [
            {"count down seconds", fun count_down_seconds/0},
            {"count down millis", fun count_down_millis/0}
        ]
    }.


count_down_seconds() ->
    ZeroTimer = countdown_timer:start_seconds(0),
    ?assert(countdown_timer:is_expired(ZeroTimer)),
    ?assertEqual(0, countdown_timer:seconds_left(ZeroTimer)),
    ?assertEqual(0, countdown_timer:millis_left(ZeroTimer)),

    CountdownTimer = countdown_timer:start_seconds(7),
    ?assertNot(countdown_timer:is_expired(CountdownTimer)),
    ?assertEqual(7, countdown_timer:seconds_left(CountdownTimer)),
    ?assertEqual(7000, countdown_timer:millis_left(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(6980),
    ?assertNot(countdown_timer:is_expired(CountdownTimer)),
    % seconds should be rounded up
    ?assertEqual(1, countdown_timer:seconds_left(CountdownTimer)),
    ?assertEqual(20, countdown_timer:millis_left(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(19),
    ?assertNot(countdown_timer:is_expired(CountdownTimer)),
    ?assertEqual(1, countdown_timer:seconds_left(CountdownTimer)),
    ?assertEqual(1, countdown_timer:millis_left(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(1),
    ?assert(countdown_timer:is_expired(CountdownTimer)),
    ?assertEqual(0, countdown_timer:seconds_left(CountdownTimer)),
    ?assertEqual(0, countdown_timer:millis_left(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(1),
    ?assert(countdown_timer:is_expired(CountdownTimer)),
    % after expiration, seconds left should always show 0
    ?assertEqual(0, countdown_timer:seconds_left(CountdownTimer)),
    ?assertEqual(0, countdown_timer:millis_left(ZeroTimer)),

    clock_freezer_mock:simulate_millis_passing(58431),
    ?assert(countdown_timer:is_expired(CountdownTimer)),
    ?assertEqual(0, countdown_timer:seconds_left(CountdownTimer)),
    ?assertEqual(0, countdown_timer:millis_left(ZeroTimer)).


count_down_millis() ->
    ZeroTimer = countdown_timer:start_millis(0),
    ?assert(countdown_timer:is_expired(ZeroTimer)),
    ?assertEqual(0, countdown_timer:millis_left(ZeroTimer)),
    ?assertEqual(0, countdown_timer:seconds_left(ZeroTimer)),

    CountdownTimer = countdown_timer:start_millis(561),
    ?assertNot(countdown_timer:is_expired(CountdownTimer)),
    ?assertEqual(561, countdown_timer:millis_left(CountdownTimer)),
    ?assertEqual(1, countdown_timer:seconds_left(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(350),
    ?assertNot(countdown_timer:is_expired(CountdownTimer)),
    ?assertEqual(211, countdown_timer:millis_left(CountdownTimer)),
    ?assertEqual(1, countdown_timer:seconds_left(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(210),
    ?assertNot(countdown_timer:is_expired(CountdownTimer)),
    ?assertEqual(1, countdown_timer:millis_left(CountdownTimer)),
    ?assertEqual(1, countdown_timer:seconds_left(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(1),
    ?assert(countdown_timer:is_expired(CountdownTimer)),
    ?assertEqual(0, countdown_timer:millis_left(CountdownTimer)),
    ?assertEqual(0, countdown_timer:seconds_left(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(1),
    ?assert(countdown_timer:is_expired(CountdownTimer)),
    % after expiration, millis left should always show 0
    ?assertEqual(0, countdown_timer:millis_left(CountdownTimer)),
    ?assertEqual(0, countdown_timer:seconds_left(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(93692823),
    ?assert(countdown_timer:is_expired(CountdownTimer)),
    ?assertEqual(0, countdown_timer:millis_left(CountdownTimer)),
    ?assertEqual(0, countdown_timer:seconds_left(CountdownTimer)).


-endif.
