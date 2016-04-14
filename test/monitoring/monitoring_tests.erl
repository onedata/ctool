%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Unit tests for monitoring module.
%%% @end
%%%--------------------------------------------------------------------
-module(monitoring_tests).
-author("Lukasz Opiola").
-author("Krzysztof Trzepla").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


is_valid_name_test() ->
    ?assert(monitoring:is_valid_name("azAZ09_", 12)),
    ?assertNot(monitoring:is_valid_name("azAZ09_", 13)),
    ?assertNot(monitoring:is_valid_name("", 20)),
    ?assertNot(monitoring:is_valid_name("", 10)),
    ?assertNot(monitoring:is_valid_name("az.AZ", 1)).


is_valid_character_test() ->
    ?assert(monitoring:is_valid_character($a)),
    ?assert(monitoring:is_valid_character($z)),
    ?assert(monitoring:is_valid_character($A)),
    ?assert(monitoring:is_valid_character($Z)),
    ?assert(monitoring:is_valid_character($0)),
    ?assert(monitoring:is_valid_character($9)),
    ?assert(monitoring:is_valid_character($_)),
    ?assertNot(monitoring:is_valid_character($.)),
    ?assertNot(monitoring:is_valid_character($/)).


get_single_core_cpu_stats_test() ->
    SampleFile = create_sample_cpu_stats_file(1),
    ?assertNotEqual(error, SampleFile),
    ExpectedCpuStats = [{<<"cpu">>, 100 * 60 / 450}],
    {ok, Fd} = SampleFile,
    meck:new(file, [unstick, passthrough]),
    meck:expect(file, open, fun("/proc/stat", [read]) -> {ok, Fd} end),
    {ActualCpuStats, _} = monitoring:get_cpu_stats([{<<"core0">>, 0, 0}, {<<"cpu">>, 0, 0}]),
    ?assert(meck:validate(file)),
    ?assertEqual(ok, meck:unload(file)),
    ?assertEqual(ExpectedCpuStats, ActualCpuStats).


get_multi_core_cpu_stats_test() ->
    SampleFile = create_sample_cpu_stats_file(4),
    ?assertNotEqual(error, SampleFile),
    ExpectedCpuStats = [{<<"cpu">>, 100 * 60 / 450}, {<<"core0">>, 100 * 60 / 450}, {<<"core1">>, 100 * 60 / 450},
        {<<"core2">>, 100 * 60 / 450}, {<<"core3">>, 100 * 60 / 450}],
    {ok, Fd} = SampleFile,
    meck:new(file, [unstick, passthrough]),
    meck:expect(file, open, fun("/proc/stat", [read]) -> {ok, Fd} end),
    {ActualCpuStats, _} = monitoring:get_cpu_stats([{<<"core3">>, 0, 0}, {<<"core2">>, 0, 0}, {<<"core1">>, 0, 0},
        {<<"core0">>, 0, 0}, {<<"cpu">>, 0, 0}]),
    ?assert(meck:validate(file)),
    ?assertEqual(ok, meck:unload(file)),
    ?assertEqual(ExpectedCpuStats, ActualCpuStats).


 get_cpu_stats_error_test() ->
     meck:new(file, [unstick, passthrough]),
     meck:expect(file, open, fun("/proc/stat", [read]) -> {error, dupa} end),
     %% TODO logger:should_log is mocked because it fails because there is no logger_plugin module
     meck:new(logger, [passthrough]),
     meck:expect(logger, should_log, fun(_) -> false end),
     Result = monitoring:get_cpu_stats([{<<"core3">>, 0, 0}, {<<"core2">>, 0, 0}, {<<"core1">>, 0, 0},
         {<<"core0">>, 0, 0}, {<<"cpu">>, 0, 0}]),
     ?assertEqual(Result, {[], []}),
     ?assert(meck:validate(logger)),
     ?assertEqual(ok, meck:unload(logger)),
     ?assert(meck:validate(file)),
     ?assertEqual(ok, meck:unload(file)).


 get_memory_stats_test() ->
     SampleFile = create_sample_memory_stats_file(),
     ?assertNotEqual(error, SampleFile),
     {ok, Fd, ExpectedMemoryStats} = SampleFile,
     meck:new(file, [unstick, passthrough]),
     meck:expect(file, open, fun("/proc/meminfo", [read]) -> {ok, Fd} end),
     ActualMemoryStats = monitoring:get_memory_stats(),
     ?assert(meck:validate(file)),
     ?assertEqual(ok, meck:unload(file)),
     ?assertEqual(ExpectedMemoryStats, ActualMemoryStats).


 get_memory_stats_error_test() ->
     meck:new(file, [unstick, passthrough]),
     meck:expect(file, open, fun("/proc/meminfo", [read]) -> error end),
     %% TODO logger:should_log is mocked because it fails because there is no logger_plugin module
     meck:new(logger, [passthrough]),
     meck:expect(logger, should_log, fun(_) -> false end),
     Result = monitoring:get_memory_stats(),
     ?assertEqual(Result, []),
     ?assert(meck:validate(file)),
     ?assertEqual(ok, meck:unload(file)),
     ?assert(meck:validate(logger)),
     ?assertEqual(ok, meck:unload(logger)).


 calculate_network_stats_test() ->
     RxBytes = 1000, TxBytes = 2000, RxPackets = 3000, TxPackets = 4000,
     ElapsedTime = 3.5,
     NetworkStats = [{rx_b, <<"eth0">>, 0}, {tx_b, <<"eth0">>, 0},
         {rx_p, <<"eth0">>, 0}, {tx_p, <<"eth0">>, 0}],
     CurrentNetworkStats = [{rx_b, <<"eth0">>, RxBytes}, {tx_b, <<"eth0">>, TxBytes},
         {rx_p, <<"eth0">>, RxPackets}, {tx_p, <<"eth0">>, TxPackets}],
     ExpectedNetworkStats = [{<<"net_rx_b_eth0">>, RxBytes}, {<<"net_tx_b_eth0">>, TxBytes},
         {<<"net_rx_pps_eth0">>, RxPackets / ElapsedTime}, {<<"net_tx_pps_eth0">>, TxPackets / ElapsedTime}],
     ActualNetworkStats = monitoring:calculate_network_stats(CurrentNetworkStats, NetworkStats, [], ElapsedTime),
     ?assertEqual(ExpectedNetworkStats, ActualNetworkStats).


 get_interface_stats_test() ->
     ExpectedInterfaceStats = 1000,
     meck:new(file, [unstick, passthrough]),
     meck:expect(file, open, fun(_, [raw]) -> {ok, fd} end),
     meck:expect(file, read_line, fun(fd) -> {ok, integer_to_list(ExpectedInterfaceStats) ++ "\n"} end),
     meck:expect(file, close, fun(fd) -> ok end),
     ActualInterfaceStats = monitoring:get_interface_stats("eth0", "rx_bytes"),
     ?assert(meck:validate(file)),
     ?assertEqual(ok, meck:unload(file)),
     ?assertEqual(ExpectedInterfaceStats, ActualInterfaceStats).


 get_network_stats_error_test() ->
     meck:new(file, [unstick, passthrough]),
     meck:expect(file, open, fun(_, [raw]) -> error end),
     %% TODO logger:should_log is mocked because it fails because there is no logger_plugin module
     meck:new(logger, [passthrough]),
     meck:expect(logger, should_log, fun(_) -> false end),
     Result = monitoring:get_interface_stats("eth0", "rx_bytes"),
     ?assertEqual(Result, 0),
     ?assert(meck:validate(file)),
     ?assertEqual(ok, meck:unload(file)),
     ?assert(meck:validate(logger)),
     ?assertEqual(ok, meck:unload(logger)).


 errors_in_monitoring_collection_test() ->
     meck:new(file, [unstick, passthrough]),
     meck:expect(file, open, fun(_, _) -> error end),
     %% TODO logger:should_log is mocked because it fails because there is no logger_plugin module
     meck:new(logger, [passthrough]),
     meck:expect(logger, should_log, fun(_) -> false end),
     MonState = monitoring:start({127,0,0,1}),
     ?assertEqual(monitoring:cpu_usage(MonState), 0.0),
     ?assertEqual(monitoring:mem_usage(MonState), 0.0),
     ?assertEqual(monitoring:net_usage(MonState), 0.0),
     MonState2 = monitoring:update(MonState),
     MonState3 = monitoring:update(MonState2),
     ?assertEqual(monitoring:cpu_usage(MonState3), 0.0),
     ?assertEqual(monitoring:mem_usage(MonState3), 0.0),
     ?assertEqual(monitoring:net_usage(MonState3), 0.0),
     ?assert(meck:validate(file)),
     ?assertEqual(ok, meck:unload(file)),
     ?assert(meck:validate(logger)),
     ?assertEqual(ok, meck:unload(logger)).




%% ====================================================================
%% Helper functions
%% ====================================================================

create_sample_cpu_stats_file(Cores) ->
    File = "/tmp/sample_cpu_stats",
    case file:open(File, [write]) of
        {ok, WriteFd} ->
            file:write(WriteFd, "cpu 10 20 30 40 50 60 70 80 90\n"),
            write_core_stats(WriteFd, 1, Cores),
            case file:close(WriteFd) of
                ok -> case file:open(File, [read]) of
                          {ok, ReadFd} -> {ok, ReadFd};
                          _ -> error
                      end;
                _ -> error
            end;
        _ -> error
    end.


write_core_stats(WriteFd, Cores, Cores) ->
    file:write(WriteFd, "cpu" ++ integer_to_list(Cores - 1) ++ " 10 20 30 40 50 60 70 80 90\n");
write_core_stats(WriteFd, Core, Cores) ->
    file:write(WriteFd, "cpu" ++ integer_to_list(Core - 1) ++ " 10 20 30 40 50 60 70 80 90\n"),
    write_core_stats(WriteFd, Core + 1, Cores).


create_sample_memory_stats_file() ->
    File = "/tmp/sample_memory_stats",
    MemTotal = 1000,
    MemFree = 300,
    case file:open(File, [write]) of
        {ok, WriteFd} ->
            file:write(WriteFd, "MemTotal: " ++ integer_to_list(MemTotal) ++ " kB\n"),
            file:write(WriteFd, "Buffers: 20208 kB\n"),
            file:write(WriteFd, "MemFree: " ++ integer_to_list(MemFree) ++ " kB\n"),
            file:write(WriteFd, "Cached: 218904 kB\n"),
            case file:close(WriteFd) of
                ok -> case file:open(File, [read]) of
                          {ok, ReadFd} -> {ok, ReadFd, [{<<"mem">>, 100 * (MemTotal - MemFree) / MemTotal}]};
                          _ -> error
                      end;
                _ -> error
            end;
        _ -> error
    end.

-endif.