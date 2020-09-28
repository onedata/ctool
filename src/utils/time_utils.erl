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
%%% At any time, the local clock can be synchronized with a remote clock and all
%%% consecutive timestamps will be adjusted to the remote clock (best effort).
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

-export([timestamp_seconds/0, timestamp_millis/0, timestamp_micros/0, timestamp_nanos/0]).
-export([synchronize_with_remote_clock/1, reset_to_local_time/0]).
-export([datetime_to_seconds/1, seconds_to_datetime/1]).
-export([seconds_to_iso8601/1, iso8601_to_seconds/1]).
-export([datetime_to_iso8601/1, iso8601_to_datetime/1]).

-include("logging.hrl").
-include("global_definitions.hrl").

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).

%% The clocks bias is cached using am application env and defaults to 0 unless a
%% synchronization is done. It is expressed in milliseconds as finer resolution
%% does not make sense in environments based on network communication.
-define(CLOCK_BIAS_CACHE_MILLIS, time_utils_clock_bias_millis).

-define(SYNCHRONIZATION_REQUEST_REPEATS, 5).
-define(SYNCHRONIZATION_TIMEOUT_MILLIS, 2000).

%%%===================================================================
%%% API
%%%===================================================================

-spec timestamp_seconds() -> seconds().
timestamp_seconds() ->
    timestamp_nanos() div 1000000000.


-spec timestamp_millis() -> millis().
timestamp_millis() ->
    timestamp_nanos() div 1000000.


-spec timestamp_micros() -> micros().
timestamp_micros() ->
    timestamp_nanos() div 1000.


-spec timestamp_nanos() -> nanos().
timestamp_nanos() ->
    ClockBiasMillis = ctool:get_env(?CLOCK_BIAS_CACHE_MILLIS, 0),
    local_timestamp_nanos() + ClockBiasMillis * 1000000.


%%--------------------------------------------------------------------
%% @doc
%% Performs a series of requests to fetch a remote timestamp. Calculates the
%% approximate communication delay with the remote server and difference of the
%% clocks (called "bias" in this module). Caches the bias using an app env
%% (node-wide) and uses it to adjust all consecutive timestamps so that they
%% return measurements as close as possible to the actual remote time. It is
%% recommended to periodically repeat the synchronization procedure to ensure
%% that the clocks don't desynchronize over a longer period. If synchronization
%% is not performed, the local system clock is used for timestamps.
%% @end
%%--------------------------------------------------------------------
-spec synchronize_with_remote_clock(fun(() -> time_utils:millis())) -> ok | error.
synchronize_with_remote_clock(FetchRemoteTimestamp) ->
    try
        {BiasSum, DelaySum} = lists:foldl(fun(_, {BiasAcc, DelayAcc}) ->
            Before = local_timestamp_nanos() div 1000000,
            RemoteTimestamp = FetchRemoteTimestamp(),
            After = local_timestamp_nanos() div 1000000,
            EstimatedMeasurementMoment = (Before + After) div 2,
            Bias = RemoteTimestamp - EstimatedMeasurementMoment,
            {BiasAcc + Bias, DelayAcc + After - Before}
        end, {0, 0}, lists:seq(1, ?SYNCHRONIZATION_REQUEST_REPEATS)),
        case DelaySum / ?SYNCHRONIZATION_REQUEST_REPEATS of
            Allowed when Allowed < ?SYNCHRONIZATION_TIMEOUT_MILLIS ->
                AverageBiasMillis = round(BiasSum / ?SYNCHRONIZATION_REQUEST_REPEATS),
                ctool:set_env(?CLOCK_BIAS_CACHE_MILLIS, AverageBiasMillis);
            TooLong ->
                ?error("Failed to synchronize with remote clock - average delay (~p) exceeded the allowed ~p", [
                    TooLong, ?SYNCHRONIZATION_TIMEOUT_MILLIS
                ]),
                error
        end
    catch Class:Reason ->
        ?error_stacktrace("Failed to synchronize with remote clock - ~w:~p", [Class, Reason]),
        error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Resets the clock bias caused by synchronization, making the timestamps return
%% local system time. If the synchronization has not been performed beforehand,
%% it has no effect.
%% @end
%%--------------------------------------------------------------------
-spec reset_to_local_time() -> ok.
reset_to_local_time() ->
    ctool:unset_env(?CLOCK_BIAS_CACHE_MILLIS).


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
-spec local_timestamp_nanos() -> nanos().
local_timestamp_nanos() ->
    erlang:system_time(nanosecond).