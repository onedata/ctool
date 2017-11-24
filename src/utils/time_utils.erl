%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions for measuring system and cluster time.
%%% @end
%%%-------------------------------------------------------------------
-module(time_utils).
-author("Lukasz Opiola").

-include("logging.hrl").
-include("global_definitions.hrl").

-define(REMOTE_TIMESTAMP_CACHE_TTL, timer:hours(1)).
-define(MAX_LATENCY_TO_CACHE_REMOTE_TIMESTAMP, 500).

%% API
-export([system_time_seconds/0, system_time_milli_seconds/0]).
-export([cluster_time_seconds/0, cluster_time_milli_seconds/0]).
-export([zone_time_seconds/0, zone_time_milli_seconds/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @equiv erlang:system_time(seconds).
%% @end
%%--------------------------------------------------------------------
-spec system_time_seconds() -> non_neg_integer().
system_time_seconds() ->
    erlang:system_time(seconds).

%%--------------------------------------------------------------------
%% @doc
%% @equiv erlang:system_time(milli_seconds).
%% @end
%%---------------------------------------------\-----------------------
-spec system_time_milli_seconds() -> non_neg_integer().
system_time_milli_seconds() ->
    erlang:system_time(milli_seconds).


%%--------------------------------------------------------------------
%% @doc
%% @equiv cluster_time_milli_seconds() div 1000
%% @end
%%--------------------------------------------------------------------
-spec cluster_time_seconds() -> non_neg_integer().
cluster_time_seconds() ->
    cluster_time_milli_seconds() div 1000.


%%--------------------------------------------------------------------
%% @doc
%% Returns current timestamp that is synchronized with cluster manager.
%% It has the accuracy of 1 second in most cases, but this might be worse under
%% very high load. When evaluated on different nodes of a cluster
%% simultaneously, it should yield times at most one second apart from each
%% other.
%% @end
%%--------------------------------------------------------------------
-spec cluster_time_milli_seconds() -> non_neg_integer().
cluster_time_milli_seconds() ->
    remote_timestamp(cluster_time, fun() ->
        gen_server2:call({global, ?CLUSTER_MANAGER}, get_current_time)
    end).


%%--------------------------------------------------------------------
%% @doc
%% @equiv zone_time_milli_seconds() div 1000
%% @end
%%--------------------------------------------------------------------
-spec zone_time_seconds() -> non_neg_integer().
zone_time_seconds() ->
    zone_time_milli_seconds() div 1000.


%%--------------------------------------------------------------------
%% @doc
%% Returns current timestamp that is synchronized with the Onezone service.
%% It has the accuracy of 1 second in most cases, but this might be worse under
%% very high load. When evaluated on different providers within the same zone
%% simultaneously, it should yield times at most one second apart from each
%% other.
%% @end
%%--------------------------------------------------------------------
-spec zone_time_milli_seconds() -> non_neg_integer().
zone_time_milli_seconds() ->
    remote_timestamp(zone_time, fun() ->
        {ok, Timestamp} = oz_providers:get_zone_time(provider),
        Timestamp
    end).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Synchronizes time with a remote server (procedure to get the timestamp should
%% be given as RemoteTimestampFun). Calculates estimated bias between local and
%% remote clock and caches it for some time, hence limiting number of remote
%% API calls. The time is returned in seconds, and in most cases it has one
%% second accuracy.
%% Cache key is used to allow different caches for different remote servers.
%% @end
%%--------------------------------------------------------------------
-spec remote_timestamp(CacheKey :: term(), RemoteTimestampFun :: fun()) ->
    non_neg_integer().
remote_timestamp(CacheKey, RemoteTimestampFun) ->
    Now = system_time_milli_seconds(),
    % If possible, use cached bias (clocks difference between this node and
    % remote server where timestamp is measured).
    case application:get_env(ctool, {bias, CacheKey}) of
        {ok, {Bias, CacheExpiration}} when Now < CacheExpiration ->
            Now + Bias;
        _ ->
            RemoteTimestamp = RemoteTimestampFun(),
            After = system_time_milli_seconds(),
            % Request to the remote server can take some time, so we need to
            % slightly adjust the result. Estimate local time when measurement
            % on the remote server was done - roughly in about the middle of the
            % time taken by the request. Given that the request should take less
            % then a second in total, we get 1 second accuracy.
            EstimatedMeasurementTime = (Now + After) div 2,
            Bias = RemoteTimestamp - EstimatedMeasurementTime,
            % Cache measured bias if the latency was not too big.
            case After - Now > ?MAX_LATENCY_TO_CACHE_REMOTE_TIMESTAMP of
                true ->
                    ok;
                false ->
                    application:set_env(
                        ctool, {bias, CacheKey},
                        {Bias, After + ?REMOTE_TIMESTAMP_CACHE_TTL}
                    )
            end,
            After + Bias
    end.
