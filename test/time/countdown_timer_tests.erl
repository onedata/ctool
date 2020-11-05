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
        fun clock_freezer_mock:setup/0,
        fun(_) -> clock_freezer_mock:teardown() end,
        [
            {"count down seconds", fun count_down_seconds/0},
            {"count down millis", fun count_down_millis/0},
            {"count down infinity", fun count_down_infinity/0}
        ]
    }.


count_down_seconds() ->
    CountdownTimer = countdown_timer:start_seconds(7),
    ?assertNot(countdown_timer:is_expired(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(6980),
    ?assertNot(countdown_timer:is_expired(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(20),
    ?assert(countdown_timer:is_expired(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(58431),
    ?_assert(countdown_timer:is_expired(CountdownTimer)).


count_down_millis() ->
    CountdownTimer = countdown_timer:start_millis(561),
    ?assertNot(countdown_timer:is_expired(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(350),
    ?assertNot(countdown_timer:is_expired(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(211),
    ?assert(countdown_timer:is_expired(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(1),
    ?assert(countdown_timer:is_expired(CountdownTimer)),

    clock_freezer_mock:simulate_millis_passing(93692823),
    ?_assert(countdown_timer:is_expired(CountdownTimer)).


count_down_infinity() ->
    lists:foreach(fun(CountdownTimer) ->
        ?assertNot(countdown_timer:is_expired(CountdownTimer)),

        lists:foreach(fun(_) ->
            clock_freezer_mock:simulate_millis_passing(rand:uniform(99999999)),
            ?assertNot(countdown_timer:is_expired(CountdownTimer))
        end, lists:seq(1, 100))
    end, [
        countdown_timer:start_seconds(infinity),
        countdown_timer:start_millis(infinity)
    ]).



-endif.
