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

% initial difference between the local time and the time shown by the remote server/node clock
-define(INITIAL_REMOTE_TIME_SHIFT, 1000000000).
-define(DUMMY_REMOTE_NODE, 'dummy@example.com').
% times shown by the local and remote *system* clocks - not to be confused with time shown by
% time_utils:timestamp_*/1, which is adjusted with bias.
-define(LOCAL_SYSTEM_TIMESTAMP(), erlang:system_time(millisecond)).
-define(REMOTE_SYSTEM_TIMESTAMP(), ?LOCAL_SYSTEM_TIMESTAMP() + ?INITIAL_REMOTE_TIME_SHIFT).

% overrides of the env variables to make the tests faster, without changing the tested logic
-define(TEST_SYNC_REQUEST_REPEATS, 3).
-define(TEST_SATISFYING_SYNC_DELAY_MILLIS, 100).
-define(TEST_MAX_ALLOWED_SYNC_DELAY_MILLIS, 1000).

%%%===================================================================
%%% Eunit tests - synchronize_local_clock_with_remote/1 & reset_to_local_time/0
%%%===================================================================

synchronize_local_clock_with_remote_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"successful", fun successful_synchronize_local_clock_with_remote/0},
            {"crashed", fun crashed_synchronize_local_clock_with_remote/0},
            {"timed out", fun timed_out_synchronize_local_clock_with_remote/0},
            {"delay too high", fun delay_too_high_synchronize_local_clock_with_remote/0},
            {"fail does not change bias", fun failed_synchronize_local_clock_with_remote_does_not_change_previous_bias/0}
        ]
    }.


successful_synchronize_local_clock_with_remote() ->
    ?assert(are_clocks_in_sync(local_clock, local_system_clock)),

    ?assertEqual(ok, time_utils:synchronize_local_clock_with_remote(gen_delayed_timestamp_callback(
        rand:uniform(90), fun() -> ?REMOTE_SYSTEM_TIMESTAMP() end)
    )),
    ?assert(are_clocks_in_sync(local_clock, remote_system_clock)),

    time_utils:reset_to_system_time(),
    ?assert(are_clocks_in_sync(local_clock, local_system_clock)).


crashed_synchronize_local_clock_with_remote() ->
    assert_local_clock_time_does_not_change_upon_sync_fail(fun crash_with_random_reason/0).


timed_out_synchronize_local_clock_with_remote() ->
    assert_local_clock_time_does_not_change_upon_sync_fail(gen_delayed_timestamp_callback(
        ?TEST_MAX_ALLOWED_SYNC_DELAY_MILLIS + 3 + rand:uniform(65),
        fun() -> ?REMOTE_SYSTEM_TIMESTAMP() end
    )).


delay_too_high_synchronize_local_clock_with_remote() ->
    % if the delay is higher than 1000ms and higher than half the bias, it is
    % deemed to high - in this case delay is ~110-130ms, while the bias is ~200ms
    assert_local_clock_time_does_not_change_upon_sync_fail(gen_delayed_timestamp_callback(
        110 + rand:uniform(20),
        fun() -> 200 + ?LOCAL_SYSTEM_TIMESTAMP() end
    )).


failed_synchronize_local_clock_with_remote_does_not_change_previous_bias() ->
    {timeout, 100, fun() ->
        ?assertEqual(ok, time_utils:synchronize_local_clock_with_remote(fun() -> ?REMOTE_SYSTEM_TIMESTAMP() end)),
        ?assert(are_clocks_in_sync(local_clock, remote_system_clock)),

        assert_local_clock_time_does_not_change_upon_sync_fail(fun crash_with_random_reason/0),

        assert_local_clock_time_does_not_change_upon_sync_fail(gen_delayed_timestamp_callback(
            ?TEST_MAX_ALLOWED_SYNC_DELAY_MILLIS + 5 + rand:uniform(15),  % timeout
            fun() -> -1500 + ?LOCAL_SYSTEM_TIMESTAMP() end
        )),

        assert_local_clock_time_does_not_change_upon_sync_fail(gen_delayed_timestamp_callback(
            1000,  % delay > bias/2
            fun() -> -1500 + ?LOCAL_SYSTEM_TIMESTAMP() end
        ))
    end}.


assert_local_clock_time_does_not_change_upon_sync_fail(FetchRemoteTimestamp) ->
    % check the time of the clock before sync attempt
    ExpClockTimeAfterwards = case are_clocks_in_sync(local_clock, local_system_clock) of
        true -> local_system_clock;
        false -> remote_system_clock
    end,
    ?assertEqual(error, time_utils:synchronize_local_clock_with_remote(FetchRemoteTimestamp)),
    % as the synchronization failed, the clock should continue to show the same time as before
    ?assert(are_clocks_in_sync(local_clock, ExpClockTimeAfterwards)).

%%%===================================================================
%%% Eunit tests - synchronize_node_clock_with_local/1
%%%===================================================================

synchronize_node_clock_with_local_test_() ->
    {foreach,
        fun() ->
            setup(),
            % remote node clock synchronization should converge to the local_clock (rather than system_clock)
            % randomize some initial local bias to make sure this works as expected
            RandomBiasSeconds = lists_utils:random_element([0, (60 + rand:uniform(1000))]),
            ctool:set_env(time_utils_clock_bias_nanos, RandomBiasSeconds * 1000000000000)
        end,
        fun teardown/1,
        [
            {"successful", fun successful_synchronize_node_clock_with_local/0},
            {"crashed", fun crashed_synchronize_node_clock_with_local/0},
            {"bad RPC", fun bad_rpc_synchronize_node_clock_with_local/0},
            {"timed out", fun timed_out_synchronize_node_clock_with_local/0},
            {"delay too high", fun delay_too_high_synchronize_node_clock_with_local/0},
            {"fail does not change bias", fun failed_synchronize_node_clock_with_local_does_not_change_previous_bias/0}
        ]
    }.


successful_synchronize_node_clock_with_local() ->
    ?assert(are_clocks_in_sync(remote_clock, remote_system_clock)),

    mock_next_remote_timestamp_rpc_response(gen_delayed_timestamp_callback(
        rand:uniform(?TEST_SATISFYING_SYNC_DELAY_MILLIS - 10),
        fun() -> ?REMOTE_SYSTEM_TIMESTAMP() end
    )),
    ?assertEqual(ok, time_utils:synchronize_node_clock_with_local(?DUMMY_REMOTE_NODE)),
    ?assert(are_clocks_in_sync(remote_clock, local_clock)).


crashed_synchronize_node_clock_with_local() ->
    assert_remote_clock_time_does_not_change_upon_sync_fail(fun crash_with_random_reason/0).


bad_rpc_synchronize_node_clock_with_local() ->
    assert_remote_clock_time_does_not_change_upon_sync_fail(fun() -> {badrpc, nodedown} end).


timed_out_synchronize_node_clock_with_local() ->
    assert_remote_clock_time_does_not_change_upon_sync_fail(gen_delayed_timestamp_callback(
        ?TEST_MAX_ALLOWED_SYNC_DELAY_MILLIS + 10 + rand:uniform(50),
        fun() -> ?REMOTE_SYSTEM_TIMESTAMP() end
    )).


delay_too_high_synchronize_node_clock_with_local() ->
    % if the delay is higher than ?SATISFYING_SYNC_DELAY_MILLIS and higher than half the bias,
    % it is deemed to high - in this case delay is ~320-370ms, while the bias is ~-500ms
    assert_remote_clock_time_does_not_change_upon_sync_fail(gen_delayed_timestamp_callback(
        320 + rand:uniform(50),
        fun() -> ?LOCAL_SYSTEM_TIMESTAMP() - 500 end
    )).


failed_synchronize_node_clock_with_local_does_not_change_previous_bias() ->
    mock_next_remote_timestamp_rpc_response(fun() -> ?REMOTE_SYSTEM_TIMESTAMP() end),
    ?assertEqual(ok, time_utils:synchronize_node_clock_with_local(?DUMMY_REMOTE_NODE)),
    ?assert(are_clocks_in_sync(remote_clock, local_clock)),

    % check that failed synchronizations does not change the remote clock
    assert_remote_clock_time_does_not_change_upon_sync_fail(fun crash_with_random_reason/0),

    assert_remote_clock_time_does_not_change_upon_sync_fail(fun() -> {badrpc, nodedown} end),

    assert_remote_clock_time_does_not_change_upon_sync_fail(gen_delayed_timestamp_callback(
        ?TEST_MAX_ALLOWED_SYNC_DELAY_MILLIS + 3 + rand:uniform(10),  % timeout
        fun() -> ?LOCAL_SYSTEM_TIMESTAMP() end
    )),

    assert_remote_clock_time_does_not_change_upon_sync_fail(gen_delayed_timestamp_callback(
        ?TEST_SATISFYING_SYNC_DELAY_MILLIS + 3 + rand:uniform(20),  % delay > bias/2
        fun() -> ?LOCAL_SYSTEM_TIMESTAMP() + 150 end
    )).


assert_remote_clock_time_does_not_change_upon_sync_fail(MockedTimestampResponse) ->
    % check the time of the clock before sync attempt
    ExpClockTimeAfterwards = case are_clocks_in_sync(remote_clock, local_clock) of
        true -> local_clock;
        false -> remote_system_clock
    end,
    mock_next_remote_timestamp_rpc_response(MockedTimestampResponse),
    ?assertEqual(error, time_utils:synchronize_node_clock_with_local(?DUMMY_REMOTE_NODE)),
    % as the synchronization failed, the clock should continue to show the same time as before
    ?assert(are_clocks_in_sync(remote_clock, ExpClockTimeAfterwards)).

%%%===================================================================
%%% Eunit tests - format conversion
%%%===================================================================

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
setup() ->
    time_utils:reset_to_system_time(),
    ctool:set_env(clock_sync_request_repeats, ?TEST_SYNC_REQUEST_REPEATS),
    ctool:set_env(clock_sync_satisfying_delay, ?TEST_SATISFYING_SYNC_DELAY_MILLIS),
    ctool:set_env(clock_sync_max_allowed_delay, ?TEST_MAX_ALLOWED_SYNC_DELAY_MILLIS),
    set_bias_nanos_at_remote_node(0),

    meck:new(rpc, [unstick, passthrough]),
    meck:expect(rpc, call, fun
        (?DUMMY_REMOTE_NODE, time_utils, timestamp_millis, []) ->
            eval_mocked_remote_timestamp_rpc_response();
        (?DUMMY_REMOTE_NODE, ctool, set_env, [time_utils_clock_bias_nanos, Bias]) ->
            set_bias_nanos_at_remote_node(Bias)
    end).


%% @private
teardown(_) ->
    ok = meck:unload(rpc).


%% @private
%% simulates the bias being set on a remote node with RPC
set_bias_nanos_at_remote_node(Bias) ->
    ctool:set_env(mocked_remote_node_bias_nanos, Bias).


%% @private
%% returns the bias that was simulated to be set on a remote node with RPC
get_bias_nanos_at_remote_node() ->
    ctool:get_env(mocked_remote_node_bias_nanos, 0).


%% @private
mock_next_remote_timestamp_rpc_response(Fun) ->
    ctool:set_env(mocked_remote_timestamp_response, Fun).


%% @private
eval_mocked_remote_timestamp_rpc_response() ->
    Fun = ctool:get_env(mocked_remote_timestamp_response),
    Fun().


%% @private
are_clocks_in_sync(ClockA, ClockB) ->
    TimestampA = get_timestamp(ClockA),
    TimestampB = get_timestamp(ClockB),
    % assume the clock is synchronized if the time read is less than 1s apart from expected
    % (the ?INITIAL_REMOTE_TIME_SHIFT is far more than that)
    TimestampA - TimestampB > -1000 andalso TimestampA - TimestampB < 1000.


%% @private
get_timestamp(local_system_clock) ->
    ?LOCAL_SYSTEM_TIMESTAMP();
get_timestamp(remote_system_clock) ->
    ?REMOTE_SYSTEM_TIMESTAMP();
get_timestamp(local_clock) ->
    time_utils:timestamp_millis();
get_timestamp(remote_clock) ->
    ?REMOTE_SYSTEM_TIMESTAMP() + get_bias_nanos_at_remote_node() div 1000000.


%% @private
crash_with_random_reason() ->
    case rand:uniform(3) of
        1 -> error(crashed);
        2 -> throw(thrown);
        3 -> exit(goodbye)
    end.


%% @private
gen_delayed_timestamp_callback(Delay, Fun) ->
    fun() ->
        timer:sleep(Delay div 2),
        Result = Fun(),  % evaluate the Fun in the middle of simulated delay
        timer:sleep(Delay div 2),
        Result
    end.

-endif.
