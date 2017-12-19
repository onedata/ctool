%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions for measuring system, cluster and
%%% zone time as well as some functions for timestamp conversion.
%%% @end
%%%-------------------------------------------------------------------
-module(time_utils).
-author("Lukasz Opiola").

-include("logging.hrl").
-include("global_definitions.hrl").

-define(REMOTE_TIMESTAMP_CACHE_TTL, timer:hours(1)).
-define(MAX_LATENCY_TO_CACHE_REMOTE_TIMESTAMP, 500).

%% Macro with regex matching allowed datestamps
%%  * YYYY-MM-DDThh:mm:ssZ
%%  * YYYY-MM-DD
-define(DATESTAMP_REGEX,
    "(\\d{4})-(\\d{2})-(\\d{2})(?:$|(?:T(\\d{2}):(\\d{2}):(\\d{2})(?:.\\d{3})?Z){1})").

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).

%% API
-export([system_time_seconds/0, system_time_milli_seconds/0]).
-export([cluster_time_seconds/0, cluster_time_milli_seconds/0]).
-export([remote_timestamp/2]).
-export([datetime_to_datestamp/1, datestamp_to_datetime/1, epoch_to_iso8601/1,
    datetime_to_epoch/1, iso8601_to_epoch/1]).

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
    remote_timestamp(cluster_time_bias, fun() ->
        try
            {ok, gen_server2:call({global, ?CLUSTER_MANAGER}, get_current_time)}
        catch
            exit:{Reason, _} ->
                ?warning("Cannot get cluster time due to: ~p", [Reason]),
                error
        end
    end).


%%--------------------------------------------------------------------
%% @doc
%% Synchronizes time with a remote server (procedure to get the timestamp should
%% be given as RemoteTimestampFun). Calculates estimated bias between local and
%% remote clock and caches it for some time, hence limiting number of remote
%% API calls. The time is returned in seconds, and in most cases it has one
%% second accuracy.
%% Cache key is used to allow different caches for different remote servers.
%% Any errors should be logged inside the RemoteTimestampFun fun and 'error'
%% should be returned.
%% @end
%%--------------------------------------------------------------------
-spec remote_timestamp(CacheKey :: atom(), RemoteTimestampFun :: fun()) ->
    non_neg_integer().
remote_timestamp(CacheKey, RemoteTimestampFun) ->
    Now = system_time_milli_seconds(),
    % If possible, use cached bias (clocks difference between this node and
    % remote server where timestamp is measured).
    case application:get_env(ctool, CacheKey) of
        {ok, {Bias, CacheExpiration}} when Now < CacheExpiration ->
            Now + Bias;
        _ ->
            case RemoteTimestampFun() of
                error ->
                    % Just return the current system time, any errors should
                    % be logged inside the RemoteTimestampFun.
                    system_time_milli_seconds();
                {ok, RemoteTimestamp} ->
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
                                ctool, CacheKey,
                                {Bias, After + ?REMOTE_TIMESTAMP_CACHE_TTL}
                            )
                    end,
                    After + Bias
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Convert unix epoch to iso8601 format.
%% @end
%%--------------------------------------------------------------------
-spec epoch_to_iso8601(Epoch :: non_neg_integer()) -> binary().
epoch_to_iso8601(Epoch) ->
    iso8601:format({Epoch div 1000000, Epoch rem 1000000, 0}).


%%--------------------------------------------------------------------
%% @doc
%% Convert iso8601 format to unix epoch time.
%% @end
%%--------------------------------------------------------------------
-spec iso8601_to_epoch(binary()) -> non_neg_integer().
iso8601_to_epoch(Iso8601) ->
    DateTime = datestamp_to_datetime(Iso8601),
    datetime_to_epoch(DateTime).


%%-------------------------------------------------------------------
%% @doc
%% Converts erlang datetime to unix epoch time.
%% @end
%%-------------------------------------------------------------------
-spec datetime_to_epoch(calendar:datetime()) -> non_neg_integer().
datetime_to_epoch({{_, _, _}, {_, _, _}} = DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.


%%%--------------------------------------------------------------------
%%% @doc
%%% Converts DateTime to format accepted by OAI-PMH which is in form
%%  YYYY-MM-DDThh:mm:ssZ
%%% @end
%%%--------------------------------------------------------------------
-spec datetime_to_datestamp(DateTime :: erlang:datetime()) -> binary().
datetime_to_datestamp(DateTime) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    str_utils:format_bin(
        "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Year, Month, Day, Hour, Minute, Second]).


%%%--------------------------------------------------------------------
%%% @doc
%%% Converts datestamp from format defined by OAI-PMH to
%%% erlang:datetime() or erlang:date().
%%% Converts:
%%%     * YYYY-MM-DDT:hh:mm:ssZ to {{Year, Month, Day},{Hour, Minutes, Seconds}}
%%%     * YYYY-MM-DD to {Year, Month, Day}
%%% @end
%%%--------------------------------------------------------------------
-spec datestamp_to_datetime(undefined | binary()) ->
    calendar:datetime() | calendar:date() | undefined | {error, invalid_date_format}.
datestamp_to_datetime(undefined) -> undefined;
datestamp_to_datetime(Datestamp) ->
    {ok, Regex} = re:compile(?DATESTAMP_REGEX),
    case re:run(Datestamp, Regex, [{capture, all_but_first, list}]) of
        {match, Matched} ->
            case [list_to_integer(E) || E <- Matched] of
                [Y, M, D, H, Min, S] ->
                    {{Y, M, D}, {H, Min, S}};
                [Y, M, D] ->
                    {Y, M, D};
                _ -> {error, invalid_date_format}
            end;
        nomatch -> {error, invalid_date_format}
    end.
