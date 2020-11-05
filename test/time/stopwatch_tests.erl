%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Eunit tests for stopwatch module.
%%%--------------------------------------------------------------------
-module(stopwatch_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Eunit tests
%%%===================================================================

stopwatch_test_() ->
    {foreach,
        fun clock_freezer_mock:setup/0,
        fun(_) -> clock_freezer_mock:teardown() end, [
        fun(_) ->
            Stopwatch = stopwatch:start(),

            ?assertEqual(0, stopwatch:read_seconds(Stopwatch)),
            ?assertEqual(0, stopwatch:read_millis(Stopwatch)),
            ?assertEqual(0, stopwatch:read_micros(Stopwatch)),
            ?assertEqual(0, stopwatch:read_nanos(Stopwatch)),

            clock_freezer_mock:simulate_millis_passing(123),

            ?assertEqual(0, stopwatch:read_seconds(Stopwatch)),
            ?assertEqual(123, stopwatch:read_millis(Stopwatch)),
            ?assertEqual(123000, stopwatch:read_micros(Stopwatch)),
            ?assertEqual(123000000, stopwatch:read_nanos(Stopwatch)),

            clock_freezer_mock:simulate_millis_passing(8950),

            ?assertEqual(9, stopwatch:read_seconds(Stopwatch)),
            ?assertEqual(9073, stopwatch:read_millis(Stopwatch)),
            ?assertEqual(9073000, stopwatch:read_micros(Stopwatch)),
            ?_assertEqual(9073000000, stopwatch:read_nanos(Stopwatch))
        end
    ]}.

-endif.
