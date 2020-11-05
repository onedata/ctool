%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module MUST be used universally for checking the absolute global
%%% "wall time" (which is identical to Onezone's cluster time). It contains
%%% procedures for clock synchronization between different nodes/clusters.
%%%
%%% The timestamp_*/0 and monotonic_timestamp_*/1 functions are the only
%%% recommended way of acquiring an absolute timestamp across Onedata services.
%%%
%%% Note that the global time IS NOT MONOTONIC. It may warp forwards and
%%% backwards due to imperfect clocks that may be readjusted in random moments
%%% on local or remote nodes. DO NOT USE the global time for measuring time
%%% elapsing between specific moments in time within a single node. However,
%%% this module must be used for time dependent logic that is distributed
%%% between two or more nodes, or assumes possible node restarts (stores the
%%% timestamps in persistence). In such case, the logic must be secured against
%%% time warps (especially backward, as this may yield negative time
%%% differences). To that end, monotonic_timestamp_*/1 can be used, which will
%%% "freeze" time reads until the time warp is compensated. NOTE HOWEVER, that
%%% this may yield ZERO TIME DIFFERENCES and cause possible division by zero.
%%%
%%% Clocks can be synchronized by calling the corresponding procedures:
%%%   * the local clock with a remote clock
%%%     (can be any service providing timestamps)
%%%   * the clock on a remote node in the cluster with the local clock
%%%     (requires that the other node has the same version of this module)
%%%
%%% Clock synchronization performs a series of requests to fetch a remote
%%% timestamp. It calculates the approximate communication delay with the remote
%%% node/server and difference of the clocks (called "bias" in this module).
%%% Finally, it stores the bias on the local or remote node (depending which
%%% clock is being synchronized). All consecutive timestamps are adjusted using
%%% the bias so that they return measurements as close as possible to the target
%%% clock's time. It is recommended to periodically repeat the synchronization
%%% procedure to ensure that the clocks don't become desynchronized over a
%%% longer period. If synchronization is not performed, the local system clock
%%% is used for timestamps.
%%%
%%% The typical synchronization error can be expected to be below a second,
%%% but for highly utilized machines it can grow to a couple of seconds.
%%% Synchronization is discarded if the communication delay exceeds
%%% ?MAX_ALLOWED_SYNC_DELAY_MILLIS or is high compared to the bias.
%%%
%%% Every time a synchronization succeeds, the measured bias is stored in a
%%% file on disk, along with the information when it was measured. It can be
%%% later used to restore the previous synchronization, given that it is not
%%% outdated. This procedure is dedicated for nodes recovering after a failure.
%%% @end
%%%-------------------------------------------------------------------
-module(global_clock).
-author("Lukasz Opiola").

-include("logging.hrl").

-type fetch_remote_timestamp() :: fun(() -> {ok, time:millis()} | {error, term()}).

% difference between readings of two clocks
-type bias() :: time:millis().
% communication delay with a remote server/node (round trip time)
-type delay() :: time:millis().
% Specific clock, as seen by the current erlang node:
%   * system_clock - the native clock on the machine
%   * local_clock  - the clock on this node used to get timestamps,
%                    essentially the system_clock adjusted with bias
%   * remote_clock - the local_clock on a remote node
%                    (remote node's system clock adjusted with bias)
-type clock_type() :: system_clock | local_clock | {remote_clock, node()}.

-export([timestamp_hours/0, timestamp_seconds/0, timestamp_millis/0]).
-export([monotonic_timestamp_seconds/1, monotonic_timestamp_millis/1]).
-export([synchronize_local_with_remote_server/1]).
-export([synchronize_remote_with_local/1]).
-export([is_synchronized/0]).
-export([reset_to_system_time/0]).
-export([try_to_restore_previous_synchronization/0]).
% internal RPC
-export([store_bias/2]).
-export([read_clock_time/1]).

%% The clock's bias is stored in a node-wide cache and defaults to 0 unless a
%% synchronization is done. The bias is measured in milliseconds, as finer
%% resolution does not make sense in environments based on network communication.
-define(CLOCK_BIAS_CACHE, clock_bias_cache).

%% If a backward time warp greater than the threshold is detected, a warning
%% is logged, but not more often than the backoff.
-define(BACKWARD_TIME_WARP_WARN_THRESHOLD_SECONDS, 60).
-define(BACKWARD_TIME_WARP_WARN_BACKOFF_SECONDS, 60).

-define(SYNC_REQUEST_REPEATS, ctool:get_env(clock_sync_request_repeats, 5)).
%% see examine_delay/2 for information how these env variables are used
-define(SATISFYING_SYNC_DELAY_MILLIS, ctool:get_env(clock_sync_satisfying_delay, 2000)).
-define(MAX_ALLOWED_SYNC_DELAY_MILLIS, ctool:get_env(clock_sync_max_allowed_delay, 10000)).

-define(BIAS_BACKUP_FILE, ctool:get_env(clock_sync_backup_file)).
-define(BIAS_BACKUP_VALIDITY_MILLIS, timer:seconds(ctool:get_env(clock_sync_backup_validity_secs, 900))).

%%%===================================================================
%%% API
%%%===================================================================

-spec timestamp_hours() -> time:hours().
timestamp_hours() ->
    timestamp_millis() div 3600000.


-spec timestamp_seconds() -> time:seconds().
timestamp_seconds() ->
    timestamp_millis() div 1000.


-spec timestamp_millis() -> time:millis().
timestamp_millis() ->
    ?MODULE:read_clock_time(local_clock).


%%--------------------------------------------------------------------
%% @doc
%% Returns the current global wall time ensuring that the read is not lower than
%% the previous - which guarantees non-strict monotonicity for consecutive reads.
%% NOTE: if the global clock warps backwards, this may cause the monotonic time to
%% freeze for a significant amount of time, until the difference is compensated.
%% NOTE: during that time, the difference of consecutive reads will be zero.
%% @end
%%--------------------------------------------------------------------
-spec monotonic_timestamp_seconds(Previous :: time:seconds()) -> time:seconds().
monotonic_timestamp_seconds(Previous) ->
    TimestampSeconds = timestamp_seconds(),
    warn_upon_backward_time_warp(TimestampSeconds - Previous),
    max(TimestampSeconds, Previous).


%% @see monotonic_timestamp_seconds/1
-spec monotonic_timestamp_millis(Previous :: time:millis()) -> time:millis().
monotonic_timestamp_millis(Previous) ->
    TimestampMillis = timestamp_millis(),
    warn_upon_backward_time_warp((TimestampMillis - Previous) div 1000),
    max(TimestampMillis, Previous).


-spec synchronize_local_with_remote_server(fetch_remote_timestamp()) -> ok | error.
synchronize_local_with_remote_server(FetchRemoteTimestamp) ->
    try
        % use system_clock as reference, as it will be adjusted by measured bias
        case estimate_bias_and_delay(FetchRemoteTimestamp, system_clock) of
            {delay_ok, AverageBias, _} ->
                store_bias(local_clock, AverageBias);
            {delay_too_high, AverageBias, AverageDelay} ->
                ?error("Failed to synchronize with remote clock - delay too high (~Bms at bias=~Bms)", [
                    AverageDelay, AverageBias
                ]),
                error
        end
    catch
        throw:{error, _} = Error ->
            ?error("Failed to synchronize with remote clock due to ~w", [Error]),
            error;
        Class:Reason ->
            ?error_stacktrace("Failed to synchronize with remote clock - ~w:~p", [Class, Reason]),
            error
    end.


-spec synchronize_remote_with_local(node()) -> ok | error.
synchronize_remote_with_local(Node) ->
    try
        FetchRemoteTimestamp = fun() ->
            case rpc:call(Node, ?MODULE, read_clock_time, [system_clock]) of
                Millis when is_integer(Millis) -> {ok, Millis};
                {badrpc, Reason} -> throw({error, {badrpc, Reason}})
            end
        end,
        % use local_clock as reference to adjust the remote clock (remote node's local_clock)
        case estimate_bias_and_delay(FetchRemoteTimestamp, local_clock) of
            {delay_ok, AverageBias, _} ->
                store_bias({remote_clock, Node}, -AverageBias);
            {delay_too_high, AverageBias, AverageDelay} ->
                ?error("Failed to synchronize node's clock (~p) with local - delay too high (~Bms at bias=~Bms)", [
                    Node, AverageDelay, AverageBias
                ]),
                error
        end
    catch
        throw:{error, _} = Error ->
            ?error("Failed to synchronize node's clock (~p) with local due to ~w", [Node, Error]),
            error;
        Class:Reason ->
            ?error_stacktrace("Failed to synchronize node's clock (~p) with local - ~w:~p", [Node, Class, Reason]),
            error
    end.


-spec is_synchronized() -> boolean().
is_synchronized() ->
    is_integer(node_cache:get(?CLOCK_BIAS_CACHE, undefined)).


%%--------------------------------------------------------------------
%% @doc
%% Resets the clock bias caused by synchronization, making the timestamps return
%% local system time. If the synchronization has not been performed beforehand,
%% it has no effect.
%% @end
%%--------------------------------------------------------------------
-spec reset_to_system_time() -> ok.
reset_to_system_time() ->
    node_cache:clear(?CLOCK_BIAS_CACHE).


%%--------------------------------------------------------------------
%% @doc
%% Attempts to restore the bias that was previously stored on disk,
%% returns a boolean indicating success. See the module's description for more.
%% @end
%%--------------------------------------------------------------------
-spec try_to_restore_previous_synchronization() -> boolean().
try_to_restore_previous_synchronization() ->
    case recover_bias_from_disk() of
        {up_to_date, Bias} ->
            ?info("Restored the previous time synchronization from backup"),
            store_bias_in_cache(Bias),
            true;
        stale ->
            ?info("Discarded a stale time synchronization backup - defaulting to the system clock"),
            false;
        not_found ->
            ?info("Time synchronization backup not found - defaulting to the system clock"),
            false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% exported for internal RPC
%% exported for eunit tests - called by ?MODULE for that reason
-spec read_clock_time(clock_type()) -> time:millis().
read_clock_time(system_clock) ->
    native_node_clock:system_time_millis();
read_clock_time(local_clock) ->
    native_node_clock:system_time_millis() + get_bias_from_cache().


%% @private
-spec estimate_bias_and_delay(fetch_remote_timestamp(), clock_type()) ->
    {delay_ok | delay_too_high, bias(), delay()} | no_return().
estimate_bias_and_delay(FetchRemoteTimestamp, ReferenceClock) ->
    {BiasSum, DelaySum} = lists:foldl(fun(_, {BiasAcc, DelayAcc}) ->
        Stopwatch = stopwatch:start(),
        RemoteTimestamp = case FetchRemoteTimestamp() of
            {ok, Timestamp} -> Timestamp;
            {error, _} = Error -> throw(Error)
        end,
        Delay = stopwatch:read_millis(Stopwatch),
        TimestampAfter = ?MODULE:read_clock_time(ReferenceClock),
        EstimatedMeasurementMoment = TimestampAfter - (Delay div 2),
        Bias = RemoteTimestamp - EstimatedMeasurementMoment,
        {BiasAcc + Bias, DelayAcc + Delay}
    end, {0, 0}, lists:seq(1, ?SYNC_REQUEST_REPEATS)),
    AvgBias = round(BiasSum / ?SYNC_REQUEST_REPEATS),
    AvgDelay = round(DelaySum / ?SYNC_REQUEST_REPEATS),
    {examine_delay(AvgDelay, AvgBias), AvgBias, AvgDelay}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Decides if given communication delay is acceptable. If the delay is lower
%% or equal to ?SATISFYING_SYNC_DELAY_MILLIS, it is always accepted. Otherwise,
%% it can be accepted if lower than half the bias, but not higher than
%% ?MAX_ALLOWED_SYNC_DELAY_MILLIS.
%% @end
%%--------------------------------------------------------------------
-spec examine_delay(delay(), bias()) -> delay_ok | delay_too_high.
examine_delay(Delay, Bias) ->
    SatisfyingDelay = ?SATISFYING_SYNC_DELAY_MILLIS,
    MaxAllowedDelay = ?MAX_ALLOWED_SYNC_DELAY_MILLIS,
    if
        Delay =< SatisfyingDelay -> delay_ok;
        Delay > MaxAllowedDelay -> delay_too_high;
        Delay < abs(Bias) / 2 -> delay_ok;
        true -> delay_too_high
    end.


%% @private
%% exported for internal RPC
-spec store_bias(clock_type(), bias()) -> ok.
store_bias({remote_clock, Node}, Bias) ->
    ok = rpc:call(Node, ?MODULE, ?FUNCTION_NAME, [local_clock, Bias]);
store_bias(local_clock, Bias) ->
    % log on info level upon the first synchronization
    case is_synchronized() of
        false -> ?info("Local clock has been synchronized, current bias: ~Bms", [Bias]);
        true -> ?debug("Local clock has been synchronized, current bias: ~Bms", [Bias])
    end,
    store_bias_in_cache(Bias),
    store_bias_on_disk(Bias).


%% @private
-spec store_bias_in_cache(bias()) -> ok | no_return().
store_bias_in_cache(Bias) ->
    ok = node_cache:put(?CLOCK_BIAS_CACHE, Bias).


%% @private
-spec get_bias_from_cache() -> bias().
get_bias_from_cache() ->
    node_cache:get(?CLOCK_BIAS_CACHE, 0).


%% @private
-spec store_bias_on_disk(bias()) -> ok | no_return().
store_bias_on_disk(Bias) ->
    ok = file:write_file(?BIAS_BACKUP_FILE, json_utils:encode(#{
        <<"biasMilliseconds">> => Bias,
        <<"backupTimestampMilliseconds">> => ?MODULE:read_clock_time(system_clock)
    })).


%% @private
-spec recover_bias_from_disk() -> {up_to_date, bias()} | stale | not_found.
recover_bias_from_disk() ->
    case file:read_file(?BIAS_BACKUP_FILE) of
        {ok, Binary} ->
            try
                #{
                    <<"biasMilliseconds">> := Bias,
                    <<"backupTimestampMilliseconds">> := BackupTimestampMillis
                } = json_utils:decode(Binary),
                MaxValidity = BackupTimestampMillis + ?BIAS_BACKUP_VALIDITY_MILLIS,
                case MaxValidity > ?MODULE:read_clock_time(system_clock) of
                    true -> {up_to_date, Bias};
                    false -> stale
                end
            catch Class:Reason ->
                ?debug_stacktrace("Cannot parse the time synchronization backup file - ~w:~p", [Class, Reason]),
                not_found
            end;
        Other ->
            ?debug("Cannot read the time synchronization backup file - ~p", [Other]),
            not_found
    end.


%% @private
-spec warn_upon_backward_time_warp(time:seconds()) -> ok.
warn_upon_backward_time_warp(TimeDiffSeconds) ->
    case TimeDiffSeconds > -?BACKWARD_TIME_WARP_WARN_THRESHOLD_SECONDS of
        true ->
            ok;
        false ->
            % backoff for some time between warning logs to avoid flooding
            utils:debounce(?BACKWARD_TIME_WARP_WARN_BACKOFF_SECONDS, fun() ->
                ?warning(
                    "Detected a major backward time warp in the global clock - ~B seconds. "
                    "Time-triggered events as well as statistics and other information "
                    "based on absolute time may be temporarily distorted.",
                    [-TimeDiffSeconds]
                )
            end)
    end.
