%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions for measuring time and format
%%% conversion. It SHOULD be used universally across all services to ensure
%%% unified time management and synchronized clocks between deployments.
%%% The timestamp_*/0 functions are the only recommended way of acquiring a
%%% timestamp.
%%%
%%% Clocks can be synchronized by calling the corresponding procedures:
%%%   * the local clock with a remote clock
%%%     (can be any service providing timestamps)
%%%   * the clock on a remote cluster node with the local clock
%%%     (requires that the node has the same version of time_utils module)
%%%
%%% Clock synchronization performs a series of requests to fetch a remote
%%% timestamp. It calculates the approximate communication delay with the remote
%%% node/server and difference of the clocks (called "bias" in this module).
%%% Finally, it sets the bias using an app env (node-wide) on the local or
%%% remote node (depending which clock is being synchronized). All consecutive
%%% timestamps are adjusted using the bias so that they return measurements as
%%% close as possible to the target clock's time. It is recommended to
%%% periodically repeat the synchronization procedure to ensure that the clocks
%%% don't become desynchronized over a longer period. If synchronization is not
%%% performed, the local system clock is used for timestamps.
%%%
%%% The expected synchronization error can be expected to be below a second,
%%% but for highly utilized machines it can grow to a couple of seconds.
%%% Synchronization is discarded if the communication delay exceeds
%%% ?MAX_ALLOWED_SYNC_DELAY_MILLIS or is high compared to the bias.
%%%
%%% The following names are used across this module and eunit tests:
%%%     * system_clock - the native clock on the machine
%%%     * local_clock  - the clock on this node used to get timestamps,
%%%                      essentially the system_clock adjusted with bias
%%%     * remote_clock - the clock on a remote node, behaving like local_clock
%%%                      (remote node's system clock adjusted with bias)
%%%     * (system/local/remote)_time - time shown by (system/local/remote)_clock
%%% @end
%%%-------------------------------------------------------------------
-module(time_utils).
-author("Lukasz Opiola").

-type seconds() :: integer().
-type millis() :: integer().
-type micros() :: integer().
-type nanos() :: integer().
-type iso8601() :: binary(). % YYYY-MM-DDThh:mm:ssZ
-export_type([seconds/0, millis/0, micros/0, nanos/0, iso8601/0]).

% difference between readings of two clocks
-type bias() :: millis().
% communication delay with a remote server/node
-type delay() :: millis().
% see module's description
-type clock() :: system_clock | local_clock | {remote_clock, node()}.

-export([timestamp_seconds/0, timestamp_millis/0, timestamp_micros/0, timestamp_nanos/0]).
-export([synchronize_local_clock_with_remote/1]).
-export([synchronize_node_clock_with_local/1]).
-export([reset_to_system_time/0]).
-export([datetime_to_seconds/1, seconds_to_datetime/1]).
-export([seconds_to_iso8601/1, iso8601_to_seconds/1]).
-export([datetime_to_iso8601/1, iso8601_to_datetime/1]).

-include("logging.hrl").
-include("global_definitions.hrl").

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).

%% The clocks bias is cached using a application env and defaults to 0 unless a
%% synchronization is done. It is expressed in nanoseconds to simplify the
%% adjustment process when taking timestamps (they are always taken in
%% nanoseconds and then converted to a bigger unit). However, the bias is
%% measured in milliseconds, as finer resolution does not make sense in
%% environments based on network communication.
-define(CLOCK_BIAS_CACHE_NANOS, time_utils_clock_bias_nanos).

-define(SYNC_REQUEST_REPEATS, ctool:get_env(clock_sync_request_repeats, 5)).
-define(SATISFYING_SYNC_DELAY_MILLIS, ctool:get_env(clock_sync_satisfying_delay, 1000)).
-define(MAX_ALLOWED_SYNC_DELAY_MILLIS, ctool:get_env(clock_sync_max_allowed_delay, 10000)).

%%%===================================================================
%%% API
%%%===================================================================

-spec timestamp_seconds() -> seconds().
timestamp_seconds() ->
    timestamp_nanos(local_clock) div 1000000000.


-spec timestamp_millis() -> millis().
timestamp_millis() ->
    timestamp_nanos(local_clock) div 1000000.


-spec timestamp_micros() -> micros().
timestamp_micros() ->
    timestamp_nanos(local_clock) div 1000.


-spec timestamp_nanos() -> nanos().
timestamp_nanos() ->
    timestamp_nanos(local_clock).


-spec synchronize_local_clock_with_remote(fun(() -> millis())) -> ok | error.
synchronize_local_clock_with_remote(FetchRemoteTimestamp) ->
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
    catch Class:Reason ->
        ?error_stacktrace("Failed to synchronize with remote clock - ~w:~p", [Class, Reason]),
        error
    end.


-spec synchronize_node_clock_with_local(node()) -> ok | error.
synchronize_node_clock_with_local(Node) ->
    try
        FetchRemoteTimestamp = fun() ->
            case rpc:call(Node, ?MODULE, timestamp_millis, []) of
                I when is_integer(I) -> I
            end
        end,
        % use local_clock as reference to adjust the remote_clock (remote node's local_clock)
        case estimate_bias_and_delay(FetchRemoteTimestamp, local_clock) of
            {delay_ok, AverageBiasMillis, _} ->
                store_bias_millis({remote_clock, Node}, -AverageBiasMillis);
            {delay_too_high, AverageBiasMillis, AverageDelayMillis} ->
                ?error("Failed to synchronize node's clock (~p) with local - delay too high (~Bms at bias=~Bms)", [
                    Node, AverageDelayMillis, AverageBiasMillis
                ]),
                error
        end
    catch Class:Reason ->
        ?error_stacktrace("Failed to synchronize node's clock (~p) with local - ~w:~p", [Node, Class, Reason]),
        error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Resets the clock bias caused by synchronization, making the timestamps return
%% local system time. If the synchronization has not been performed beforehand,
%% it has no effect.
%% @end
%%--------------------------------------------------------------------
-spec reset_to_system_time() -> ok.
reset_to_system_time() ->
    ctool:set_env(?CLOCK_BIAS_CACHE_NANOS, 0).


-spec seconds_to_iso8601(seconds()) -> iso8601().
seconds_to_iso8601(Seconds) ->
    datetime_to_iso8601(seconds_to_datetime(Seconds)).


-spec iso8601_to_seconds(iso8601()) -> seconds().
iso8601_to_seconds(Iso8601) ->
    datetime_to_seconds(iso8601_to_datetime(Iso8601)).


-spec datetime_to_seconds(calendar:datetime()) -> seconds().
datetime_to_seconds(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.


-spec seconds_to_datetime(seconds()) -> calendar:datetime().
seconds_to_datetime(TimestampSeconds) ->
    calendar:gregorian_seconds_to_datetime(TimestampSeconds + ?UNIX_EPOCH).


-spec datetime_to_iso8601(calendar:datetime()) -> iso8601().
datetime_to_iso8601(DateTime) ->
    iso8601:format(DateTime).


-spec iso8601_to_datetime(iso8601()) -> calendar:datetime().
iso8601_to_datetime(Iso8601) ->
    iso8601:parse(binary_to_list(Iso8601)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec timestamp_millis(clock()) -> millis().
timestamp_millis(Clock) ->
    timestamp_nanos(Clock) div 1000000.


%% @private
-spec timestamp_nanos(clock()) -> nanos().
timestamp_nanos(system_clock) ->
    erlang:system_time(nanosecond);
timestamp_nanos(local_clock) ->
    erlang:system_time(nanosecond) + get_bias_nanos().


-spec store_bias_millis(clock(), millis()) -> ok.
store_bias_millis(local_clock, BiasMillis) ->
    ok = ctool:set_env(?CLOCK_BIAS_CACHE_NANOS, BiasMillis * 1000000);
store_bias_millis({remote_clock, Node}, BiasMillis) ->
    ok = rpc:call(Node, ctool, set_env, [?CLOCK_BIAS_CACHE_NANOS, BiasMillis * 1000000]).


-spec get_bias_nanos() -> nanos().
get_bias_nanos() ->
    ctool:get_env(?CLOCK_BIAS_CACHE_NANOS, 0).


%% @private
-spec estimate_bias_and_delay(fun(() -> millis()), clock()) -> {delay_ok | delay_too_high, bias(), delay()}.
estimate_bias_and_delay(FetchRemoteTimestamp, ReferenceClock) ->
    {BiasSum, DelaySum} = lists:foldl(fun(_, {BiasAcc, DelayAcc}) ->
        Before = timestamp_millis(ReferenceClock),
        RemoteTimestamp = FetchRemoteTimestamp(),
        After = timestamp_millis(ReferenceClock),
        EstimatedMeasurementMoment = (Before + After) div 2,
        Bias = RemoteTimestamp - EstimatedMeasurementMoment,
        {BiasAcc + Bias, DelayAcc + After - Before}
    end, {0, 0}, lists:seq(1, ?SYNC_REQUEST_REPEATS)),
    AvgBias = round(BiasSum / ?SYNC_REQUEST_REPEATS),
    AvgDelay = round(DelaySum / ?SYNC_REQUEST_REPEATS),
    {examine_delay(AvgDelay, AvgBias), AvgBias, AvgDelay}.


%% @private
-spec examine_delay(delay(), bias()) -> delay_ok | delay_too_high.
examine_delay(Delay, Bias) ->
    SatisfyingDelay = ?SATISFYING_SYNC_DELAY_MILLIS,
    MaxAllowedDelay = ?MAX_ALLOWED_SYNC_DELAY_MILLIS,
    if
        Delay < SatisfyingDelay -> delay_ok;
        Delay > MaxAllowedDelay -> delay_too_high;
        Delay < Bias / 2 -> delay_ok;
        true -> delay_too_high
    end.
