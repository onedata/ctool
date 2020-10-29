%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains functions for measuring time and clock synchronization.
%%% It MUST be used universally across all services to ensure unified time
%%% management and synchronized clocks between deployments. The timestamp_*/0
%%% functions are the only recommended way of acquiring a timestamp.
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
-module(clock).
-author("Lukasz Opiola").

-include("logging.hrl").

-type hours() :: integer().
-type seconds() :: integer().
-type millis() :: integer().
-type micros() :: integer().
-type nanos() :: integer().
-export_type([hours/0, seconds/0, millis/0, micros/0, nanos/0]).

-type fetch_remote_timestamp() :: fun(() -> {ok, millis()} | {error, term()}).

% difference between readings of two clocks, expressed in given unit
-type bias(Unit) :: Unit.
% communication delay with a remote server/node (round trip time)
-type delay() :: millis().
% Specific clock, as seen by the current erlang node:
%   * system_clock - the native clock on the machine
%   * local_clock  - the clock on this node used to get timestamps,
%                    essentially the system_clock adjusted with bias
%   * remote_clock - the local_clock on a remote node
%                    (remote node's system clock adjusted with bias)
-type clock() :: system_clock | local_clock | {remote_clock, node()}.

-export([timestamp_seconds/0, timestamp_millis/0, timestamp_micros/0, timestamp_nanos/0]).
-export([synchronize_local_with_remote_server/1]).
-export([synchronize_remote_with_local/1]).
-export([is_synchronized/0]).
-export([reset_to_system_time/0]).
-export([try_to_restore_previous_synchronization/0]).
% internal RPC
-export([store_bias_millis/2]).
-export([read_clock_time/2]).

%% The clocks bias is stored in a node-wide cache and defaults to 0 unless a
%% synchronization is done. It is expressed in nanoseconds to simplify the
%% adjustment process when taking timestamps (they are always taken in
%% nanoseconds and then converted to a bigger unit). However, the bias is
%% measured in milliseconds, as finer resolution does not make sense in
%% environments based on network communication.
-define(CLOCK_BIAS_CACHE_NANOS, clock_bias_nanos).

-define(SYNC_REQUEST_REPEATS, node_cache:get(clock_sync_request_repeats, 5)).
% see examine_delay/2 for information how these env variables are used
-define(SATISFYING_SYNC_DELAY_MILLIS, node_cache:get(clock_sync_satisfying_delay, 2000)).
-define(MAX_ALLOWED_SYNC_DELAY_MILLIS, node_cache:get(clock_sync_max_allowed_delay, 10000)).

-define(BIAS_BACKUP_FILE, node_cache:get(clock_sync_backup_file)).
-define(BIAS_BACKUP_VALIDITY_SECONDS, node_cache:get(clock_sync_backup_validity_secs, 900)).

%%%===================================================================
%%% API
%%%===================================================================

-spec timestamp_seconds() -> seconds().
timestamp_seconds() ->
    ?MODULE:read_clock_time(local_clock, nanos) div 1000000000.


-spec timestamp_millis() -> millis().
timestamp_millis() ->
    ?MODULE:read_clock_time(local_clock, nanos) div 1000000.


-spec timestamp_micros() -> micros().
timestamp_micros() ->
    ?MODULE:read_clock_time(local_clock, nanos) div 1000.


-spec timestamp_nanos() -> nanos().
timestamp_nanos() ->
    ?MODULE:read_clock_time(local_clock, nanos).


-spec synchronize_local_with_remote_server(fetch_remote_timestamp()) -> ok | error.
synchronize_local_with_remote_server(FetchRemoteTimestamp) ->
    try
        % use system_clock as reference, as it will be adjusted by measured bias
        case estimate_bias_and_delay(FetchRemoteTimestamp, system_clock) of
            {delay_ok, AverageBiasMillis, _} ->
                store_bias_millis(local_clock, AverageBiasMillis);
            {delay_too_high, AverageBiasMillis, AverageDelayMillis} ->
                ?error("Failed to synchronize with remote clock - delay too high (~Bms at bias=~Bms)", [
                    AverageDelayMillis, AverageBiasMillis
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
            case rpc:call(Node, ?MODULE, read_clock_time, [system_clock, millis]) of
                Timestamp when is_integer(Timestamp) -> {ok, Timestamp};
                {badrpc, Reason} -> throw({error, {badrpc, Reason}})
            end
        end,
        % use local_clock as reference to adjust the remote clock (remote node's local_clock)
        case estimate_bias_and_delay(FetchRemoteTimestamp, local_clock) of
            {delay_ok, AverageBiasMillis, _} ->
                store_bias_millis({remote_clock, Node}, -AverageBiasMillis);
            {delay_too_high, AverageBiasMillis, AverageDelayMillis} ->
                ?error("Failed to synchronize node's clock (~p) with local - delay too high (~Bms at bias=~Bms)", [
                    Node, AverageDelayMillis, AverageBiasMillis
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
    is_integer(node_cache:get(?CLOCK_BIAS_CACHE_NANOS, undefined)).


%%--------------------------------------------------------------------
%% @doc
%% Resets the clock bias caused by synchronization, making the timestamps return
%% local system time. If the synchronization has not been performed beforehand,
%% it has no effect.
%% @end
%%--------------------------------------------------------------------
-spec reset_to_system_time() -> ok.
reset_to_system_time() ->
    node_cache:clear(?CLOCK_BIAS_CACHE_NANOS).


%%--------------------------------------------------------------------
%% @doc
%% Attempts to restore the bias that was previously stored on disk,
%% returns a boolean indicating success. See the module's description for more.
%% @end
%%--------------------------------------------------------------------
-spec try_to_restore_previous_synchronization() -> boolean().
try_to_restore_previous_synchronization() ->
    case recover_bias_millis_from_disk() of
        {up_to_date, BiasMillis} ->
            ?info("Restored the previous time synchronization from backup"),
            store_bias_millis_in_cache(BiasMillis),
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
-spec read_clock_time(clock(), millis | nanos) -> millis() | nanos().
read_clock_time(Clock, millis) ->
    read_clock_time(Clock, nanos) div 1000000;
read_clock_time(system_clock, nanos) ->
    erlang:system_time(nanosecond);
read_clock_time(local_clock, nanos) ->
    erlang:system_time(nanosecond) + get_bias_nanos_from_cache().


%% @private
-spec estimate_bias_and_delay(fetch_remote_timestamp(), clock()) ->
    {delay_ok | delay_too_high, bias(millis()), delay()} | no_return().
estimate_bias_and_delay(FetchRemoteTimestamp, ReferenceClock) ->
    {BiasSum, DelaySum} = lists:foldl(fun(_, {BiasAcc, DelayAcc}) ->
        Before = ?MODULE:read_clock_time(ReferenceClock, millis),
        RemoteTimestamp = case FetchRemoteTimestamp() of
            {ok, Timestamp} -> Timestamp;
            {error, _} = Error -> throw(Error)
        end,
        After = ?MODULE:read_clock_time(ReferenceClock, millis),
        EstimatedMeasurementMoment = (Before + After) div 2,
        Bias = RemoteTimestamp - EstimatedMeasurementMoment,
        {BiasAcc + Bias, DelayAcc + After - Before}
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
-spec examine_delay(delay(), bias(millis())) -> delay_ok | delay_too_high.
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
-spec store_bias_millis(clock(), millis()) -> ok.
store_bias_millis({remote_clock, Node}, BiasMillis) ->
    ok = rpc:call(Node, ?MODULE, ?FUNCTION_NAME, [local_clock, BiasMillis]);
store_bias_millis(local_clock, BiasMillis) ->
    % log on info level upon the first synchronization
    case is_synchronized() of
        false -> ?info("Local clock has been synchronized, current bias: ~Bms", [BiasMillis]);
        true -> ?debug("Local clock has been synchronized, current bias: ~Bms", [BiasMillis])
    end,
    store_bias_millis_in_cache(BiasMillis),
    store_bias_millis_on_disk(BiasMillis).


%% @private
-spec store_bias_millis_in_cache(bias(millis())) -> ok | no_return().
store_bias_millis_in_cache(BiasMillis) ->
    ok = node_cache:put(?CLOCK_BIAS_CACHE_NANOS, BiasMillis * 1000000).


%% @private
-spec get_bias_nanos_from_cache() -> bias(nanos()).
get_bias_nanos_from_cache() ->
    node_cache:get(?CLOCK_BIAS_CACHE_NANOS, 0).


%% @private
-spec store_bias_millis_on_disk(bias(millis())) -> ok | no_return().
store_bias_millis_on_disk(BiasMillis) ->
    ok = file:write_file(?BIAS_BACKUP_FILE, json_utils:encode(#{
        <<"biasMilliseconds">> => BiasMillis,
        <<"backupTimestampMilliseconds">> => ?MODULE:read_clock_time(system_clock, millis)
    })).


%% @private
-spec recover_bias_millis_from_disk() -> {up_to_date, bias(millis())} | stale | not_found.
recover_bias_millis_from_disk() ->
    case file:read_file(?BIAS_BACKUP_FILE) of
        {ok, Binary} ->
            try
                #{
                    <<"biasMilliseconds">> := BiasMillis,
                    <<"backupTimestampMilliseconds">> := BackupTimestampMillis
                } = json_utils:decode(Binary),
                MaxValidity = BackupTimestampMillis + timer:seconds(?BIAS_BACKUP_VALIDITY_SECONDS),
                case MaxValidity > ?MODULE:read_clock_time(system_clock, millis) of
                    true -> {up_to_date, BiasMillis};
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
