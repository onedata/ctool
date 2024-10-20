%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module provides functions for gathering and calculating
%%% statistical data. They are invoked by node_manager.
%%% Some statistics require previous measurements to be calculated.
%%% In order to encapsulate this aspect inside this module, there is a
%%% dedicated record that shall be passed to all function calls.
%%% @end
%%%-------------------------------------------------------------------
-module(monitoring).
-author("Lukasz Opiola").
-author("Krzysztof Trzepla").

-include("logging.hrl").
-include("monitoring/monitoring.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

% Record holding all monitoring info and required measurement history.
-record(node_monitoring_state, {
    cpu_stats = [] :: [{Name :: binary(), Value :: float()}],
    mem_stats = [] :: [{Name :: binary(), Value :: float()}],
    net_stats = [] :: [{Name :: binary(), Value :: float()}],
    cpu_last = [] :: [{Name :: binary(), WorkJiffies :: integer(), TotalJiffies :: integer()}],
    net_last = [] :: [{rx_b | tx_b | rx_p | tx_p | maxthp, Name :: binary(), Value :: integer()}],
    erlang_vm_cpu = 0.0 :: float(),
    erlang_vm_mem = [] :: [{Type :: atom(), Size :: non_neg_integer()}],
    erlang_vm_processes_num = 0 :: non_neg_integer(),
    last_update_timer :: stopwatch:instance()
}).

-type node_monitoring_state() :: #node_monitoring_state{}.
-export_type([node_monitoring_state/0]).

% Files to read system stats from
-define(CPU_STATS_FILE, "/proc/stat").
-define(MEM_STATS_FILE, "/proc/meminfo").
-define(NET_STATS_FILE(_Interface, _Type), "/sys/class/net/" ++ _Interface ++ "/statistics/" ++ _Type).
-define(NET_SPEED_FILE(_Interface), "/sys/class/net/" ++ _Interface ++ "/speed").
-define(NET_DUPLEX_FILE(_Interface), "/sys/class/net/" ++ _Interface ++ "/duplex").

%% API
-export([start/0, update/1]).
-export([get_node_state/1, get_memory_stats/0]).
-export([cpu_usage/1, mem_usage/1, net_usage/1, erlang_vm_stats/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes monitoring and returns the node_monitoring_state record
%% that will be used throughout the monitoring process.
%% @end
%%--------------------------------------------------------------------
-spec start() -> #node_monitoring_state{}.
start() ->
    update(#node_monitoring_state{last_update_timer = stopwatch:start()}).


%%--------------------------------------------------------------------
%% @doc
%% Updates monitoring statistics.
%% @end
%%--------------------------------------------------------------------
-spec update(MonitoringState :: #node_monitoring_state{}) -> #node_monitoring_state{}.
update(MonitoringState) ->
    #node_monitoring_state{
        last_update_timer = LastUpdateTimer,
        cpu_last = CPULast,
        net_last = NetLast,
        erlang_vm_cpu = VM_CPU
    } = MonitoringState,

    % make sure the time difference is not zero in case the update function is
    % called twice in a very short interval, float is required for better accuracy
    TimeDiffSeconds = max(0.001, stopwatch:read_seconds(LastUpdateTimer, float)),

    {CPUStats, NewCPULast} = get_cpu_stats(CPULast),
    {NetStats, NewNetLast} = get_network_stats(NetLast, TimeDiffSeconds),
    MemStats = get_memory_stats(),
    NewVM_CPU = case cpu_sup:util() of
        {error, _} -> VM_CPU;
        Other -> Other
    end,
    MonitoringState#node_monitoring_state{
        last_update_timer = stopwatch:start(),
        cpu_stats = CPUStats,
        cpu_last = NewCPULast,
        mem_stats = MemStats,
        net_stats = NetStats,
        net_last = NewNetLast,
        erlang_vm_cpu = NewVM_CPU,
        erlang_vm_mem = erlang:memory(),
        erlang_vm_processes_num = length(erlang:processes())}.


%%--------------------------------------------------------------------
%% @doc
%% Returns a #node_state{} record that contains all required information
%% about current node state and load.
%% @end
%%--------------------------------------------------------------------
-spec get_node_state(MonitoringState :: #node_monitoring_state{}) -> #node_state{}.
get_node_state(MonitoringState) ->
    CPUUsage = cpu_usage(MonitoringState),
    MemUsage = mem_usage(MonitoringState),
    NetUsage = net_usage(MonitoringState),
    #node_state{cpu_usage = CPUUsage, mem_usage = MemUsage, net_usage = NetUsage}.


%%--------------------------------------------------------------------
%% @doc
%% Returns current cpu usage based on node monitoring state.
%% Returns 0.0 in case of an error.
%% @end
%%--------------------------------------------------------------------
-spec cpu_usage(MonitoringState :: #node_monitoring_state{}) -> float().
cpu_usage(#node_monitoring_state{cpu_stats = CPUStats}) ->
    _CPUUsage = proplists:get_value(<<"cpu">>, CPUStats, 0.0).


%%--------------------------------------------------------------------
%% @doc
%% Returns current memory usage based on node monitoring state.
%% Returns 0.0 in case of an error.
%% @end
%%--------------------------------------------------------------------
-spec mem_usage(MonitoringState :: #node_monitoring_state{}) -> float().
mem_usage(#node_monitoring_state{mem_stats = MemStats}) ->
    _MemUsage = proplists:get_value(<<"mem">>, MemStats, 0.0).


%%--------------------------------------------------------------------
%% @doc
%% Returns current network usage based on node monitoring state.
%% Returns 0.0 in case of an error.
%% @end
%%--------------------------------------------------------------------
-spec net_usage(MonitoringState :: #node_monitoring_state{}) -> float().
net_usage(#node_monitoring_state{net_stats = NetStats}) ->
    {NetUsage, MaxThroughput} = lists:foldl(
        fun({Name, Value}, {AccNU, AccMS}) ->
            case Name of
                % Sum bytes sent and received
                <<"net_rx_b_eth", _/binary>> ->
                    {AccNU + Value, AccMS};
                <<"net_tx_b_eth", _/binary>> ->
                    {AccNU + Value, AccMS};
                % Sum throughputs
                <<"net_maxthp_eth", _/binary>> ->
                    {AccNU, AccMS + Value};
                _ ->
                    {AccNU, AccMS}
            end
        end, {0, 0}, NetStats),
    % Calc usage in per cent
    case MaxThroughput of
        0 -> 0.0;
        _ -> NetUsage / MaxThroughput * 100
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns info about resources usage by Erlang VM.
%% @end
%%--------------------------------------------------------------------
-spec erlang_vm_stats(MonitoringState :: #node_monitoring_state{}) ->
    {float(), [{atom(), non_neg_integer()}], non_neg_integer()}.
erlang_vm_stats(#node_monitoring_state{erlang_vm_cpu = CPU, erlang_vm_mem = Mem, erlang_vm_processes_num = PNum}) ->
    {CPU, Mem, PNum}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns current cpu usage in percentage and current measurements that will
%% be used for next calculation.
%% @end
%%--------------------------------------------------------------------
-spec get_cpu_stats(CpuStats) -> {Result, CpuStats} when
    Result :: [{Name :: binary(), Value :: float()}],
    CpuStats :: [{Name :: binary(), WorkJiffies :: integer(), TotalJiffies :: integer()}].
get_cpu_stats(CpuStats) ->
    try
        case file:open(?CPU_STATS_FILE, [read]) of
            {ok, Fd} ->
                CurrentCpuStats = read_cpu_stats(Fd, []),
                case calculate_cpu_stats(CurrentCpuStats, CpuStats, []) of
                    [Main, _OneCore] ->
                        % If there is only one core, make sure that <<"cpu">> entry exists
                        % Main and _OneCore have keys <<"cpu">> and <<"core0">> and the same values
                        {_, Load} = Main,
                        {[{<<"cpu">>, Load}], CurrentCpuStats};
                    MoreCores ->
                        {MoreCores, CurrentCpuStats}
                end;
            Other ->
                ?error("Cannot open CPU stats file: ~tp", [Other]),
                {[], []}
        end
    catch
        T:M:Stacktrace ->
            ?error_stacktrace("Cannot calculate CPU usage - ~tp:~tp", [T, M], Stacktrace),
            {[], []}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reads CPU stats from given file.
%% @end
%%--------------------------------------------------------------------
-spec read_cpu_stats(Fd :: file:fd(), CpuStats) -> CpuStats when
    CpuStats :: [{Name :: binary(), WorkJiffies :: integer(), TotalJiffies :: integer()}].
read_cpu_stats(Fd, Stats) ->
    case file:read_line(Fd) of
        {ok, "cpu" ++ _ = Line} ->
            ["cpu" ++ ID | Values] = string:tokens(string:strip(Line, right, $\n), " "),
            Name = case ID of
                "" -> <<"cpu">>;
                _ -> <<"core", (list_to_binary(ID))/binary>>
            end,
            case is_valid_name(Name, 0) of
                true ->
                    [User, Nice, System | Rest] = lists:map(fun(Value) -> list_to_integer(Value) end, Values),
                    WorkJiffies = User + Nice + System,
                    TotalJiffies = WorkJiffies + lists:foldl(fun(Value, Sum) -> Value + Sum end, 0, Rest),
                    read_cpu_stats(Fd, [{Name, WorkJiffies, TotalJiffies} | Stats]);
                _ -> read_cpu_stats(Fd, Stats)
            end;
        {ok, _} ->
            read_cpu_stats(Fd, Stats);
        _ ->
            file:close(Fd),
            Stats
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calculates cpu usage statistics, given current and previous measurements.
%% @end
%%--------------------------------------------------------------------
-spec calculate_cpu_stats(CpuStats, CpuStats, Result) -> Result when
    CpuStats :: [{Name :: binary(), WorkJiffies :: integer(), TotalJiffies :: integer()}],
    Result :: [{Name :: binary(), Usage :: float()}].
calculate_cpu_stats([{Name, _, _} | CurrentCpuStats], [], Stats) ->
    calculate_cpu_stats(CurrentCpuStats, [], [{Name, 0} | Stats]);
calculate_cpu_stats([{Name, _, T} | CurrentCpuStats], [{Name, _, T} | CpuStats], Stats) ->
    calculate_cpu_stats(CurrentCpuStats, CpuStats, [{Name, 0} | Stats]);
calculate_cpu_stats([{Name, WA, TA} | CurrentCpuStats], [{Name, WB, TB} | CpuStats], Stats) ->
    calculate_cpu_stats(CurrentCpuStats, CpuStats, [{Name, 100 * (WA - WB) / (TA - TB)} | Stats]);
calculate_cpu_stats(_, _, Stats) ->
    Stats.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns current memory usage in percentage.
%% @end
%%--------------------------------------------------------------------
-spec get_memory_stats() -> [{Name :: binary(), Value :: float()}].
get_memory_stats() ->
    try
        case file:open(?MEM_STATS_FILE, [read]) of
            {ok, Fd} ->
                read_memory_stats(Fd, -1, -1, 0);
            Other ->
                ?error("Cannot open memory stats file: ~tp", [Other]),
                []
        end
    catch
        T:M:Stacktrace ->
            ?error_stacktrace("Cannot calculate memory usage - ~tp:~tp", [T, M], Stacktrace),
            []
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reads memory stats from given file.
%% @end
%%--------------------------------------------------------------------
-spec read_memory_stats(Fd :: file:fd(), MemFree :: integer(), MemTotal :: integer(), Counter :: integer()) ->
    [{Name :: binary(), Usage :: float()}].
read_memory_stats(Fd, MemFree, MemTotal, 2) ->
    file:close(Fd),
    [{<<"mem">>, 100 * (MemTotal - MemFree) / MemTotal}];
read_memory_stats(Fd, MemFree, MemTotal, Counter) ->
    GetValue = fun(Line) ->
        [Value | _] = string:tokens(Line, " "),
        list_to_integer(Value)
    end,
    case file:read_line(Fd) of
        {ok, "MemTotal:" ++ Line} ->
            read_memory_stats(Fd, MemFree, GetValue(Line), Counter + 1);
        {ok, "MemAvailable:" ++ Line} ->
            read_memory_stats(Fd, GetValue(Line), MemTotal, Counter + 1);
        eof ->
            case {MemFree, Counter} of
                {-1, 1} ->
                    read_memory_stats(Fd, available_memory:calculate(), MemTotal, Counter + 1);
                _ ->
                    file:close(Fd),
                    []
            end;
        {error, _} ->
            [];
        _ ->
            read_memory_stats(Fd, MemFree, MemTotal, Counter)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns current network usage as total bytes sent since last measurement and avg packets per second.
%% Also, returns current measurements that will be used for next calculation.
%% @end
%%--------------------------------------------------------------------
-spec get_network_stats(NetworkStats, TimeElapsed :: float()) -> {Result, NetworkStats} when
    NetworkStats :: [{rx_b | tx_b | rx_p | tx_p | maxthp, Name :: binary(), Value :: integer()}],
    Result :: [{Name :: binary(), Value :: float()}].
get_network_stats(NetworkStats, TimeElapsed) ->
    try
        Dir = "/sys/class/net/",
        case file:list_dir(Dir) of
            {ok, Interfaces} ->
                ValidInterfaces = lists:filter(fun(Interface) ->
                    IsEthernetIf = case Interface of
                        "eth" ++ _ -> true;
                        _ -> false
                    end,
                    IsEthernetIf andalso is_valid_name(Interface, 11)
                end, Interfaces),
                CurrentNetworkStats = lists:foldl(
                    fun(Interface, Stats) -> [
                        {rx_b, list_to_binary(Interface), get_interface_stats(Interface, "rx_bytes")},
                        {tx_b, list_to_binary(Interface), get_interface_stats(Interface, "tx_bytes")},
                        {rx_p, list_to_binary(Interface), get_interface_stats(Interface, "rx_packets")},
                        {tx_p, list_to_binary(Interface), get_interface_stats(Interface, "tx_packets")},
                        {maxthp, list_to_binary(Interface), get_interface_max_throughput(Interface, TimeElapsed)} |
                        Stats
                    ] end, [], ValidInterfaces),
                Result = calculate_network_stats(CurrentNetworkStats, NetworkStats, [], TimeElapsed),
                {Result, CurrentNetworkStats};
            Other ->
                ?error("Cannot open network stats file: ~tp", [Other]),
                {[], []}
        end
    catch
        T:M:Stacktrace ->
            ?error_stacktrace("Cannot calculate network usage - ~tp:~tp", [T, M], Stacktrace),
            {[], []}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calculates network usage statistics, given current and previous measurements.
%% @end
%%--------------------------------------------------------------------
-spec calculate_network_stats(CurrentNetworkStats, NetworkStats, Stats, TimeElapsed :: float()) -> Result when
    CurrentNetworkStats :: [{rx_b | tx_b | rx_p | tx_p | maxthp, Name :: binary(), Value :: float()}],
    NetworkStats :: [{rx_b | tx_b | rx_p | tx_p | maxthp, Name :: binary(), Value :: float()}],
    Stats :: [{Name :: binary(), Value :: float()}],
    Result :: [{Name :: binary(), Value :: float()}].
% Last net stats were not given (probably first calculation), return empty result.
calculate_network_stats(_, [], [], _) ->
    [];
calculate_network_stats([], _, Stats, _) ->
    lists:reverse(Stats);
calculate_network_stats([Stat | CurrentNetworkStats], [], Stats, TimeElapsed) ->
    calculate_network_stats(CurrentNetworkStats, [], [Stat | Stats], TimeElapsed);
% net_rx_b is a number of bytes received since last update
calculate_network_stats([{rx_b, Name, StatA} | CurrentNetworkStats], [{rx_b, Name, StatB} | NetworkStats], Stats, TimeElapsed) ->
    calculate_network_stats(CurrentNetworkStats, NetworkStats, [{<<"net_rx_b_", Name/binary>>, StatA - StatB} | Stats], TimeElapsed);
% net_rx_b is a number of bytes sent since last update
calculate_network_stats([{tx_b, Name, StatA} | CurrentNetworkStats], [{tx_b, Name, StatB} | NetworkStats], Stats, TimeElapsed) ->
    calculate_network_stats(CurrentNetworkStats, NetworkStats, [{<<"net_tx_b_", Name/binary>>, StatA - StatB} | Stats], TimeElapsed);
% net_rx_b is average packets per second received since last update
calculate_network_stats([{rx_p, Name, StatA} | CurrentNetworkStats], [{rx_p, Name, StatB} | NetworkStats], Stats, TimeElapsed) ->
    calculate_network_stats(CurrentNetworkStats, NetworkStats, [{<<"net_rx_pps_", Name/binary>>, (StatA - StatB) / TimeElapsed} | Stats], TimeElapsed);
% net_rx_b is average packets per second sent since last update
calculate_network_stats([{tx_p, Name, StatA} | CurrentNetworkStats], [{tx_p, Name, StatB} | NetworkStats], Stats, TimeElapsed) ->
    calculate_network_stats(CurrentNetworkStats, NetworkStats, [{<<"net_tx_pps_", Name/binary>>, (StatA - StatB) / TimeElapsed} | Stats], TimeElapsed);
% maxspeed is maximum interface throughput, considering duplex mode
calculate_network_stats([{maxthp, Name, MaxSpeed} | CurrentNetworkStats], [{maxthp, Name, _} | NetworkStats], Stats, TimeElapsed) ->
    calculate_network_stats(CurrentNetworkStats, NetworkStats, [{<<"net_maxthp_", Name/binary>>, MaxSpeed} | Stats], TimeElapsed);
calculate_network_stats(_, _, Stats, _) ->
    lists:reverse(Stats).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks interface usage, where Interface is a name (e.g. eth0)
%% and Type is name of collecting statistics (e.g. rx_bytes)
%% @end
%%--------------------------------------------------------------------
-spec get_interface_stats(Interface :: string(), Type :: string()) -> integer().
get_interface_stats(Interface, Type) ->
    Filename = ?NET_STATS_FILE(Interface, Type),
    case file:open(Filename, [raw]) of
        {ok, Fd} ->
            InterfaceStats = case file:read_line(Fd) of
                {ok, Value} -> list_to_integer(string:strip(Value, right, $\n));
                _ -> 0
            end,
            file:close(Fd),
            InterfaceStats;
        Other ->
            ?error("Cannot open interface stats file: ~tp", [Other]),
            0
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calculates interface max throughput, i. e. how much could it have possibly
%% sent + received during TimeElapsed time.
%% If the interface works in full duplex, the potential throughput is doubled.
%% @end
%%--------------------------------------------------------------------
-spec get_interface_max_throughput(Interface :: string(), TimeElapsed :: float()) -> integer() | float().
get_interface_max_throughput(Interface, TimeElapsed) ->
    DuplexRatio = case file:read_file(?NET_DUPLEX_FILE(Interface)) of
        {ok, <<"full\n">>} -> 2;
        _ -> 1
    end,
    IntSpeedMbps = case file:read_file(?NET_SPEED_FILE(Interface)) of
        {ok, SpeedBin} ->
            binary_to_integer(binary:part(SpeedBin, 0, byte_size(SpeedBin) - 1));
        _ ->
            10
    end,
    % IntSpeedMbps is in Mbps so (IntSpeedMbps * 131072) is in Bytes/s
    IntSpeedMbps * 131072 * DuplexRatio * TimeElapsed.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks whether string contains only following characters a-zA-Z0-9_
%% and with some prefix is not longer than 19 characters. This is a requirement
%% for a valid column name in Round Robin Database.
%% @end
%%--------------------------------------------------------------------
-spec is_valid_name(Name :: string() | binary(), PrefixLength :: integer()) -> boolean().
is_valid_name(Name, PrefixLength) when is_list(Name) ->
    case length(Name) =< 19 - PrefixLength of
        true -> is_valid_name(Name);
        _ -> false
    end;
is_valid_name(Name, PrefixLength) when is_binary(Name) ->
    is_valid_name(binary_to_list(Name), PrefixLength);
is_valid_name(_, _) ->
    false.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks whether string contains only following characters a-zA-Z0-9_
%% @end
%%--------------------------------------------------------------------
-spec is_valid_name(Name :: string()) -> boolean().
is_valid_name([]) ->
    false;
is_valid_name([Character]) ->
    is_valid_character(Character);
is_valid_name([Character | Characters]) ->
    is_valid_character(Character) andalso is_valid_name(Characters).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks whether character belongs to a-zA-Z0-9_ range
%% @end
%%--------------------------------------------------------------------
-spec is_valid_character(Character :: char()) -> boolean().
is_valid_character($_) -> true;
is_valid_character(Character) when Character >= $0 andalso Character =< $9 -> true;
is_valid_character(Character) when Character >= $A andalso Character =< $Z -> true;
is_valid_character(Character) when Character >= $a andalso Character =< $z -> true;
is_valid_character(_) -> false.
