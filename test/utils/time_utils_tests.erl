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

successful_synchronization_test() ->
    time_utils:reset_to_local_time(),
    StartingLocalTimestamp = ?SYSTEM_TIMESTAMP(),
    % before synchronization, the clock should show the local time
    ?assert(are_within_one_second(time_utils:timestamp_millis(), StartingLocalTimestamp)),
    ?assertNot(are_within_one_second(time_utils:timestamp_millis(), ?FAKE_REMOTE_TIMESTAMP)),

    FetchRemoteTimestamp = fun() ->
        RandDelay = rand:uniform(500),
        timer:sleep(RandDelay div 2),
        Result = ?FAKE_REMOTE_TIMESTAMP + ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
        timer:sleep(RandDelay div 2),
        Result
    end,

    ?assertEqual(ok, time_utils:synchronize_with_remote_clock(FetchRemoteTimestamp)),
    TimePassedSinceStart = ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
    MeasuredTimestamp = time_utils:timestamp_millis(),
    % after synchronization, the clock should show the remote time
    ?assertNot(are_within_one_second(MeasuredTimestamp, StartingLocalTimestamp + TimePassedSinceStart)),
    ?assert(are_within_one_second(MeasuredTimestamp, ?FAKE_REMOTE_TIMESTAMP + TimePassedSinceStart)),
    % the clock can be reset to the local time
    time_utils:reset_to_local_time(),
    NewMeasuredTimestamp = time_utils:timestamp_millis(),
    ?assert(are_within_one_second(NewMeasuredTimestamp, StartingLocalTimestamp + TimePassedSinceStart)),
    ?assertNot(are_within_one_second(NewMeasuredTimestamp, ?FAKE_REMOTE_TIMESTAMP + TimePassedSinceStart)).


failed_synchronization_test() ->
    time_utils:reset_to_local_time(),
    StartingLocalTimestamp = ?SYSTEM_TIMESTAMP(),

    FetchRemoteTimestamp = fun() ->
        throw(fail)
    end,

    ?assertEqual(error, time_utils:synchronize_with_remote_clock(FetchRemoteTimestamp)),
    TimePassedSinceStart = ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
    MeasuredTimestamp = time_utils:timestamp_millis(),
    % as the synchronization failed, the clock should continue to show the local time
    ?assert(are_within_one_second(MeasuredTimestamp, StartingLocalTimestamp + TimePassedSinceStart)),
    ?assertNot(are_within_one_second(MeasuredTimestamp, ?FAKE_REMOTE_TIMESTAMP + TimePassedSinceStart)).


timed_out_synchronization_test_() ->
    {timeout, 60, fun() ->
        time_utils:reset_to_local_time(),
        StartingLocalTimestamp = ?SYSTEM_TIMESTAMP(),

        FetchRemoteTimestamp = fun() ->
            RandDelay = 2000 + rand:uniform(200),
            timer:sleep(RandDelay div 2),
            Result = ?FAKE_REMOTE_TIMESTAMP + ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
            timer:sleep(RandDelay div 2),
            Result
        end,

        ?assertEqual(error, time_utils:synchronize_with_remote_clock(FetchRemoteTimestamp)),
        TimePassedSinceStart = ?SYSTEM_TIMESTAMP() - StartingLocalTimestamp,
        MeasuredTimestamp = time_utils:timestamp_millis(),
        % as the synchronization failed, the clock should continue to show the local time
        ?assert(are_within_one_second(MeasuredTimestamp, StartingLocalTimestamp + TimePassedSinceStart)),
        ?assertNot(are_within_one_second(MeasuredTimestamp, ?FAKE_REMOTE_TIMESTAMP + TimePassedSinceStart))
    end}.


%% @private helper
are_within_one_second(MillisA, MillisB) ->
    MillisA - MillisB > -1000 andalso MillisA - MillisB < 1000.


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


-endif.
