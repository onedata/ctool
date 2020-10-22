%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Eunit tests for clock module.
%%%--------------------------------------------------------------------
-module(clock_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

% initial difference between the local time and the time shown by the remote server/node clock (millis)
-define(INITIAL_REMOTE_TIME_SHIFT, 1000000000).
-define(DUMMY_REMOTE_NODE, 'dummy@example.com').
% Times shown by the local and remote *system* clocks, which are frozen and manually adjusted in these tests.
% Not to be confused with time shown by clock:timestamp_*/1, which is adjusted with bias.
-define(LOCAL_SYSTEM_TIMESTAMP(), get_frozen_system_time_millis()).
-define(REMOTE_SYSTEM_TIMESTAMP(), ?LOCAL_SYSTEM_TIMESTAMP() + ?INITIAL_REMOTE_TIME_SHIFT).

% overrides of the env variables to make the tests faster, without changing the tested logic
-define(TEST_SYNC_REQUEST_REPEATS, 3).
-define(TEST_SATISFYING_SYNC_DELAY_MILLIS, 100).
-define(TEST_MAX_ALLOWED_SYNC_DELAY_MILLIS, 1000).
-define(TEST_BIAS_BACKUP_VALIDITY_SECONDS, 900).

%%%===================================================================
%%% Eunit tests - synchronize_local_clock_with_remote/1
%%%===================================================================

local_clock_sync_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"successful", fun successful_synchronize_local_clock_with_remote/0},
            {"crashed", fun crashed_synchronize_local_clock_with_remote/0},
            {"timed out", fun timed_out_synchronize_local_clock_with_remote/0},
            {"delay ok", fun delay_ok_synchronize_local_clock_with_remote/0},
            {"delay too high", fun delay_too_high_synchronize_local_clock_with_remote/0},
            {"fail does not change bias", fun failed_synchronize_local_clock_with_remote_does_not_change_previous_bias/0}
        ]
    }.


successful_synchronize_local_clock_with_remote() ->
    ?assert(are_clocks_in_sync(local_clock, local_system_clock)),
    ?assertNot(clock:is_synchronized()),

    ?assertEqual(ok, clock:synchronize_local_with_remote_server(gen_delayed_timestamp_callback(
        rand:uniform(90), fun() -> ?REMOTE_SYSTEM_TIMESTAMP() end)
    )),
    ?assert(are_clocks_in_sync(local_clock, remote_system_clock)),
    ?assert(clock:is_synchronized()).


crashed_synchronize_local_clock_with_remote() ->
    assert_local_clock_time_does_not_change_upon_sync_fail(fun crash_with_random_reason/0).


timed_out_synchronize_local_clock_with_remote() ->
    assert_local_clock_time_does_not_change_upon_sync_fail(gen_delayed_timestamp_callback(
        ?TEST_MAX_ALLOWED_SYNC_DELAY_MILLIS + 3 + rand:uniform(65),
        fun() -> ?REMOTE_SYSTEM_TIMESTAMP() end
    )).


delay_ok_synchronize_local_clock_with_remote() ->
    % if the delay is higher than ?SATISFYING_SYNC_DELAY_MILLIS, but lower than half the bias, it is accepted
    ?assertEqual(ok, clock:synchronize_local_with_remote_server(gen_delayed_timestamp_callback(
        110 + rand:uniform(20),
        fun() -> -300 + ?LOCAL_SYSTEM_TIMESTAMP() end
    ))),
    ?assertEqual(ok, clock:synchronize_local_with_remote_server(gen_delayed_timestamp_callback(
        110 + rand:uniform(20),
        fun() -> 300 + ?LOCAL_SYSTEM_TIMESTAMP() end
    ))).


delay_too_high_synchronize_local_clock_with_remote() ->
    % if the delay is higher than ?SATISFYING_SYNC_DELAY_MILLIS and higher than half the bias, it is
    % deemed to high - in this case delay is ~110-130ms, while the bias is ~200ms
    assert_local_clock_time_does_not_change_upon_sync_fail(gen_delayed_timestamp_callback(
        110 + rand:uniform(20),
        fun() -> -200 + ?LOCAL_SYSTEM_TIMESTAMP() end
    )).


failed_synchronize_local_clock_with_remote_does_not_change_previous_bias() ->
    {timeout, 100, fun() ->
        ?assertEqual(ok, clock:synchronize_local_with_remote_server(fun() -> ?REMOTE_SYSTEM_TIMESTAMP() end)),
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
    % check the clock sync before the attempt
    WasClockSynchronized = clock:is_synchronized(),
    PreviousReferenceClock = case are_clocks_in_sync(local_clock, local_system_clock) of
        true -> local_system_clock;
        false -> remote_system_clock
    end,
    ?assertEqual(error, clock:synchronize_local_with_remote_server(FetchRemoteTimestamp)),
    % as the synchronization failed, the clock should continue to show the same time as before
    ?assertEqual(WasClockSynchronized, clock:is_synchronized()),
    ?assert(are_clocks_in_sync(local_clock, PreviousReferenceClock)).

%%%===================================================================
%%% Eunit tests - synchronize_node_clock_with_local/1
%%%===================================================================

remote_node_clock_sync_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"successful", fun successful_synchronize_node_clock_with_local/0},
            {"crashed", fun crashed_synchronize_node_clock_with_local/0},
            {"bad RPC", fun bad_rpc_synchronize_node_clock_with_local/0},
            {"timed out", fun timed_out_synchronize_node_clock_with_local/0},
            {"delay ok", fun delay_ok_synchronize_node_clock_with_local/0},
            {"delay too high", fun delay_too_high_synchronize_node_clock_with_local/0},
            {"fail does not change bias", fun failed_synchronize_node_clock_with_local_does_not_change_previous_bias/0}
        ]
    }.


successful_synchronize_node_clock_with_local() ->
    % remote node clock synchronization should converge to the local_clock (rather than system_clock)
    % randomize some initial local bias to make sure this works as expected
    RandomBiasSeconds = lists_utils:random_element([0, (60 + rand:uniform(1000))]),
    ctool:set_env(clock_bias_nanos, RandomBiasSeconds * 1000 * 1000 * 1000 * 1000),

    ?assert(are_clocks_in_sync(remote_clock, remote_system_clock)),
    ?assertNot(is_clock_synchronized_on_remote_node()),

    mock_next_remote_timestamp_rpc_response(gen_delayed_timestamp_callback(
        rand:uniform(?TEST_SATISFYING_SYNC_DELAY_MILLIS - 10),
        fun() -> ?REMOTE_SYSTEM_TIMESTAMP() end
    )),
    ?assertEqual(ok, clock:synchronize_remote_with_local(?DUMMY_REMOTE_NODE)),
    ?assert(is_clock_synchronized_on_remote_node()),
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


delay_ok_synchronize_node_clock_with_local() ->
    % if the delay is higher than ?SATISFYING_SYNC_DELAY_MILLIS, but lower than half the bias, it is accepted
    mock_next_remote_timestamp_rpc_response(gen_delayed_timestamp_callback(
        110 + rand:uniform(20),
        fun() -> -300 + ?LOCAL_SYSTEM_TIMESTAMP() end
    )),
    ?assertEqual(ok, clock:synchronize_remote_with_local(?DUMMY_REMOTE_NODE)),

    mock_next_remote_timestamp_rpc_response(gen_delayed_timestamp_callback(
        110 + rand:uniform(20),
        fun() -> 300 + ?LOCAL_SYSTEM_TIMESTAMP() end
    )),
    ?assertEqual(ok, clock:synchronize_remote_with_local(?DUMMY_REMOTE_NODE)).


delay_too_high_synchronize_node_clock_with_local() ->
    % if the delay is higher than ?SATISFYING_SYNC_DELAY_MILLIS and higher than half the bias,
    % it is deemed to high - in this case delay is ~320-370ms, while the bias is ~-500ms
    assert_remote_clock_time_does_not_change_upon_sync_fail(gen_delayed_timestamp_callback(
        320 + rand:uniform(50),
        fun() -> ?LOCAL_SYSTEM_TIMESTAMP() - 500 end
    )).


failed_synchronize_node_clock_with_local_does_not_change_previous_bias() ->
    mock_next_remote_timestamp_rpc_response(fun() -> ?REMOTE_SYSTEM_TIMESTAMP() end),
    ?assertEqual(ok, clock:synchronize_remote_with_local(?DUMMY_REMOTE_NODE)),
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
        fun() -> ?LOCAL_SYSTEM_TIMESTAMP() + 100 end
    )).


assert_remote_clock_time_does_not_change_upon_sync_fail(MockedTimestampResponse) ->
    % check the time of the clock before sync attempt
    WasClockSynchronized = is_clock_synchronized_on_remote_node(),
    PreviousReferenceClock = case are_clocks_in_sync(remote_clock, local_clock) of
        true -> local_clock;
        false -> remote_system_clock
    end,
    mock_next_remote_timestamp_rpc_response(MockedTimestampResponse),
    ?assertEqual(error, clock:synchronize_remote_with_local(?DUMMY_REMOTE_NODE)),
    % as the synchronization failed, the clock should continue to show the same time as before
    ?assertEqual(WasClockSynchronized, is_clock_synchronized_on_remote_node()),
    ?assert(are_clocks_in_sync(remote_clock, PreviousReferenceClock)).

%%%===================================================================
%%% Eunit tests - time sync backup
%%%===================================================================

% Both local an remote procedures rely on clock:store_bias_millis/2 function
% to store the bias in cache and on disk (it is executed with RPC for the remote),
% so it is enough to test the local procedure.
time_sync_backup_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"reset and restore", fun reset_and_restore/0},
            {"successful restore from disc", fun successful_restore_from_disc/0},
            {"failed restore - stale", fun failed_restore_from_disc_due_to_stale_backup/0},
            {"failed restore - not found", fun failed_restore_from_disc_due_to_inexistent_backup/0},
            {"failed restore - parse error", fun failed_restore_from_disc_due_to_backup_parsing_error/0}
        ]
    }.


reset_and_restore() ->
    ?assertEqual(ok, clock:synchronize_local_with_remote_server(fun() -> ?REMOTE_SYSTEM_TIMESTAMP() end)),

    clock:reset_to_system_time(),
    ?assert(are_clocks_in_sync(local_clock, local_system_clock)),
    ?assertNot(clock:is_synchronized()),

    ?assertEqual(true, clock:try_to_restore_previous_synchronization()),
    ?assert(are_clocks_in_sync(local_clock, remote_system_clock)),
    ?assert(clock:is_synchronized()).


successful_restore_from_disc() ->
    % simulate an existing backup file that was stored some time ago,
    % with bias equal to the time shift between local and remote clocks
    mock_existing_backup_file(json_utils:encode(#{
        <<"biasMilliseconds">> => ?INITIAL_REMOTE_TIME_SHIFT,
        <<"backupTimestampMilliseconds">> => current_system_time_minus_seconds(?TEST_BIAS_BACKUP_VALIDITY_SECONDS - 50)
    })),
    ?assertEqual(true, clock:try_to_restore_previous_synchronization()),
    ?assert(are_clocks_in_sync(local_clock, remote_system_clock)).


failed_restore_from_disc_due_to_stale_backup() ->
    mock_existing_backup_file(json_utils:encode(#{
        <<"biasMilliseconds">> => ?INITIAL_REMOTE_TIME_SHIFT,
        <<"backupTimestampMilliseconds">> => current_system_time_minus_seconds(?TEST_BIAS_BACKUP_VALIDITY_SECONDS + 1)
    })),
    ?assertEqual(false, clock:try_to_restore_previous_synchronization()),
    ?assert(are_clocks_in_sync(local_clock, local_system_clock)).


failed_restore_from_disc_due_to_inexistent_backup() ->
    ?assertEqual(false, clock:try_to_restore_previous_synchronization()),
    ?assert(are_clocks_in_sync(local_clock, local_system_clock)).


failed_restore_from_disc_due_to_backup_parsing_error() ->
    mock_existing_backup_file(json_utils:encode(#{<<"unfonformant">> => <<"json">>})),
    ?assertEqual(false, clock:try_to_restore_previous_synchronization()),
    ?assert(are_clocks_in_sync(local_clock, local_system_clock)),

    mock_existing_backup_file(<<"gibberish">>),
    ?assertEqual(false, clock:try_to_restore_previous_synchronization()),
    ?assert(are_clocks_in_sync(local_clock, local_system_clock)).

%%%===================================================================
%%% System time mocks
%%%===================================================================

% for the sake of the tests in this module, the system clock is frozen, in order
% to be able to accurately mock delays and biases
freeze_system_time() ->
    meck:new(clock, [passthrough]),
    meck:expect(clock, read_clock_time, fun
        F(Clock, millis) ->
            F(Clock, nanos) div 1000000;
        F(system_clock, nanos) ->
            get_frozen_system_time_millis() * 1000000;
        F(local_clock, nanos) ->
            get_frozen_system_time_millis() * 1000000 + ctool:get_env(clock_bias_nanos, 0)
    end).


unfreeze_system_time() ->
    ok = meck:unload(clock).


get_frozen_system_time_millis() ->
    ctool:get_env(frozen_system_time_millis, 1500000000).


simulate_time_passing(Millis) ->
    ctool:set_env(frozen_system_time_millis, get_frozen_system_time_millis() + Millis).

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @private
setup() ->
    clock:reset_to_system_time(),
    ctool:set_env(clock_sync_request_repeats, ?TEST_SYNC_REQUEST_REPEATS),
    ctool:set_env(clock_sync_satisfying_delay, ?TEST_SATISFYING_SYNC_DELAY_MILLIS),
    ctool:set_env(clock_sync_max_allowed_delay, ?TEST_MAX_ALLOWED_SYNC_DELAY_MILLIS),
    ctool:set_env(clock_sync_backup_validity_secs, ?TEST_BIAS_BACKUP_VALIDITY_SECONDS),
    unset_bias_nanos_at_remote_node(),

    freeze_system_time(),

    TmpPath = mochitemp:mkdtemp(),
    BackupFile = filename:join(TmpPath, "time_synchronization_data.json"),
    ctool:set_env(clock_sync_backup_file, BackupFile),

    meck:new(rpc, [unstick, passthrough]),
    meck:expect(rpc, call, fun
        (?DUMMY_REMOTE_NODE, clock, read_clock_time, [system_clock, millis]) ->
            eval_mocked_remote_timestamp_rpc_response();
        (?DUMMY_REMOTE_NODE, clock, store_bias_millis, [local_clock, BiasMillis]) ->
            set_bias_nanos_at_remote_node(BiasMillis * 1000000)
    end).


%% @private
teardown(_) ->
    BackupFile = ctool:get_env(clock_sync_backup_file),
    TmpPath = filename:dirname(BackupFile),
    mochitemp:rmtempdir(TmpPath),

    unfreeze_system_time(),
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
unset_bias_nanos_at_remote_node() ->
    ctool:unset_env(mocked_remote_node_bias_nanos).


%% @private
mock_next_remote_timestamp_rpc_response(Fun) ->
    ctool:set_env(mocked_remote_timestamp_response, Fun).


%% @private
eval_mocked_remote_timestamp_rpc_response() ->
    Fun = ctool:get_env(mocked_remote_timestamp_response),
    Fun().


%% @private
is_clock_synchronized_on_remote_node() ->
    is_integer(ctool:get_env(mocked_remote_node_bias_nanos, undefined)).


%% @private
mock_existing_backup_file(Content) ->
    ?assertEqual(ok, file:write_file(ctool:get_env(clock_sync_backup_file), Content)).


%% @private
current_system_time_minus_seconds(Seconds) ->
    ?LOCAL_SYSTEM_TIMESTAMP() - timer:seconds(Seconds).


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
    clock:timestamp_millis();
get_timestamp(remote_clock) ->
    ?REMOTE_SYSTEM_TIMESTAMP() + get_bias_nanos_at_remote_node() div 1000000.


%% @private
crash_with_random_reason() ->
    case rand:uniform(3) of
        1 -> error(crashed);
        2 -> throw(hot_potato);
        3 -> exit(goodbye)
    end.


%% @private
gen_delayed_timestamp_callback(Delay, Fun) ->
    fun() ->
        simulate_time_passing(Delay div 2),
        Result = Fun(),  % evaluate the Fun in the middle of simulated delay
        simulate_time_passing(Delay div 2),
        Result
    end.

-endif.
