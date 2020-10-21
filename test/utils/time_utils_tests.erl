%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Eunit tests for time_utils module.
%%%--------------------------------------------------------------------
-module(time_utils_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

% a fake timestamp from a remote server, very different than the local one
-define(FAKE_REMOTE_TIMESTAMP, 1000000000000).
-define(SYSTEM_TIMESTAMP(), erlang:system_time(millisecond)).

%%%===================================================================
%%% Eunit tests
%%%===================================================================

time_utils_test_() ->
    {setup, fun node_cache:init/0, []}.


successful_synchronization_test() ->
    time_utils:reset_to_local_time(),
    StartingLocalTimestamp = ?SYSTEM_TIMESTAMP(),
    % before synchronization, the clock should show the local time
    ?assert(is_clock_in_sync(StartingLocalTimestamp)),
    ?assertNot(is_clock_in_sync(?FAKE_REMOTE_TIMESTAMP)),

    FetchRemoteTimestamp = fun() ->
        RandDelay = rand:uniform(900),
        timer:sleep(RandDelay div 2),
        Result = ?FAKE_REMOTE_TIMESTAMP + ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
        timer:sleep(RandDelay div 2),
        Result
    end,

    ?assertEqual(ok, time_utils:synchronize_with_remote_clock(FetchRemoteTimestamp)),
    TimeSinceStart = ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
    % after synchronization, the clock should show the remote time
    ?assertNot(is_clock_in_sync(StartingLocalTimestamp + TimeSinceStart)),
    ?assert(is_clock_in_sync(?FAKE_REMOTE_TIMESTAMP + TimeSinceStart)),
    % the clock can be reset to the local time
    time_utils:reset_to_local_time(),
    ?assert(is_clock_in_sync(StartingLocalTimestamp + TimeSinceStart)),
    ?assertNot(is_clock_in_sync(?FAKE_REMOTE_TIMESTAMP + TimeSinceStart)).


crashed_synchronization_test() ->
    time_utils:reset_to_local_time(),
    StartingLocalTimestamp = ?SYSTEM_TIMESTAMP(),

    FetchRemoteTimestamp = fun() ->
        throw(fail)
    end,

    ?assertEqual(error, time_utils:synchronize_with_remote_clock(FetchRemoteTimestamp)),
    TimeSinceStart = ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
    % as the synchronization failed, the clock should continue to show the local time
    ?assert(is_clock_in_sync(StartingLocalTimestamp + TimeSinceStart)),
    ?assertNot(is_clock_in_sync(?FAKE_REMOTE_TIMESTAMP + TimeSinceStart)).


timed_out_synchronization_test() ->
    {timeout, 100, fun() ->
        time_utils:reset_to_local_time(),
        StartingLocalTimestamp = ?SYSTEM_TIMESTAMP(),

        FetchRemoteTimestamp = fun() ->
            RandDelay = 10000 + rand:uniform(500),
            timer:sleep(RandDelay div 2),
            Result = ?FAKE_REMOTE_TIMESTAMP + ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
            timer:sleep(RandDelay div 2),
            Result
        end,

        ?assertEqual(error, time_utils:synchronize_with_remote_clock(FetchRemoteTimestamp)),
        TimeSinceStart = ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
        % as the synchronization failed, the clock should continue to show the local time
        ?assert(is_clock_in_sync(StartingLocalTimestamp + TimeSinceStart)),
        ?assertNot(is_clock_in_sync(?FAKE_REMOTE_TIMESTAMP + TimeSinceStart))
    end}.


delay_too_high_synchronization_test() ->
    {timeout, 100, fun() ->
        time_utils:reset_to_local_time(),
        StartingLocalTimestamp = ?SYSTEM_TIMESTAMP(),

        % if the delay is higher than 1000ms and higher than half the bias, it is
        % deemed to high - in this case delay is ~1100-1300ms, while the bias is ~2000ms
        FetchRemoteTimestamp = fun() ->
            RandDelay = 1100 + rand:uniform(200),
            timer:sleep(RandDelay div 2),
            Result = 2000 + ?SYSTEM_TIMESTAMP() + ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
            timer:sleep(RandDelay div 2),
            Result
        end,

        ?assertEqual(error, time_utils:synchronize_with_remote_clock(FetchRemoteTimestamp)),
        TimeSinceStart = ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
        % as the synchronization failed, the clock should continue to show the local time
        ?assert(is_clock_in_sync(StartingLocalTimestamp + TimeSinceStart)),
        ?assertNot(is_clock_in_sync(?FAKE_REMOTE_TIMESTAMP + TimeSinceStart))
    end}.


failed_synchronization_does_not_change_previous_bias_test() ->
    {timeout, 100, fun() ->
        time_utils:reset_to_local_time(),
        StartingLocalTimestamp = ?SYSTEM_TIMESTAMP(),

        SuccessfulFetchRemoteTimestamp = fun() ->
            ?FAKE_REMOTE_TIMESTAMP + ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp
        end,

        CrashingFetchRemoteTimestamp = fun() ->
            error(this_is_not_good)
        end,

        TimingOutFetchRemoteTimestamp = fun() ->
            timer:sleep(11000),
            ?FAKE_REMOTE_TIMESTAMP + ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp
        end,

        DelayTooHighFetchRemoteTimestamp = fun() ->
            timer:sleep(500),
            Result = 1500 + ?SYSTEM_TIMESTAMP() + ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
            timer:sleep(500),
            Result
        end,

        ?assertEqual(ok, time_utils:synchronize_with_remote_clock(SuccessfulFetchRemoteTimestamp)),
        TimeSinceStartA = ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
        % after synchronization, the clock should show the remote time
        ?assertNot(is_clock_in_sync(StartingLocalTimestamp + TimeSinceStartA)),
        ?assert(is_clock_in_sync(?FAKE_REMOTE_TIMESTAMP + TimeSinceStartA)),

        % crashed attempt to synchronize should not change the previous bias
        ?assertEqual(error, time_utils:synchronize_with_remote_clock(CrashingFetchRemoteTimestamp)),
        TimeSinceStartB = ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
        ?assertNot(is_clock_in_sync(StartingLocalTimestamp + TimeSinceStartB)),
        ?assert(is_clock_in_sync(?FAKE_REMOTE_TIMESTAMP + TimeSinceStartB)),

        % timed out attempt to synchronize should not change the previous bias
        ?assertEqual(error, time_utils:synchronize_with_remote_clock(TimingOutFetchRemoteTimestamp)),
        TimeSinceStartC = ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
        ?assertNot(is_clock_in_sync(StartingLocalTimestamp + TimeSinceStartC)),
        ?assert(is_clock_in_sync(?FAKE_REMOTE_TIMESTAMP + TimeSinceStartC)),

        % attempt to synchronize with too high delay should not change the previous bias
        ?assertEqual(error, time_utils:synchronize_with_remote_clock(DelayTooHighFetchRemoteTimestamp)),
        TimeSinceStartD = ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
        ?assertNot(is_clock_in_sync(StartingLocalTimestamp + TimeSinceStartD)),
        ?assert(is_clock_in_sync(?FAKE_REMOTE_TIMESTAMP + TimeSinceStartD))
    end}.


general_conversion_test() ->
    Seconds = time_utils:timestamp_seconds(),
    DateTime = time_utils:seconds_to_datetime(Seconds),
    Iso8601 = time_utils:datetime_to_iso8601(DateTime),

    ?assertEqual(Seconds, time_utils:datetime_to_seconds(DateTime)),
    ?assertEqual(Seconds, time_utils:iso8601_to_seconds(Iso8601)),

    ?assertEqual(DateTime, time_utils:seconds_to_datetime(Seconds)),
    ?assertEqual(DateTime, time_utils:iso8601_to_datetime(Iso8601)),

    ?assertEqual(Iso8601, time_utils:seconds_to_iso8601(Seconds)),
    ?assertEqual(Iso8601, time_utils:datetime_to_iso8601(DateTime)).


specific_conversion_test() ->
    Seconds = 1600956903,
    SecondsMidnight = 1600905600,
    Iso8601 = <<"2020-09-24T14:15:03Z">>,
    Iso8601DateOnly = <<"2020-09-24">>,
    DateTime = {{2020, 9, 24}, {14, 15, 3}},
    DateTimeMidnight = {{2020, 9, 24}, {0, 0, 0}},

    ?assertEqual(Seconds, time_utils:datetime_to_seconds(DateTime)),
    ?assertEqual(Seconds, time_utils:iso8601_to_seconds(Iso8601)),

    ?assertEqual(DateTime, time_utils:seconds_to_datetime(Seconds)),
    ?assertEqual(DateTime, time_utils:iso8601_to_datetime(Iso8601)),

    ?assertEqual(Iso8601, time_utils:seconds_to_iso8601(Seconds)),
    ?assertEqual(Iso8601, time_utils:datetime_to_iso8601(DateTime)),

    ?assertEqual(SecondsMidnight, time_utils:iso8601_to_seconds(Iso8601DateOnly)),
    ?assertEqual(DateTimeMidnight, time_utils:iso8601_to_datetime(Iso8601DateOnly)).

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @private
is_clock_in_sync(ExpectedTime) ->
    MeasuredTime = time_utils:timestamp_millis(),
    % assume the clock is synchronized if the time read is less than 1s apart from expected
    MeasuredTime - ExpectedTime > -1000 andalso MeasuredTime - ExpectedTime < 1000.


-endif.
