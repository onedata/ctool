%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module MUST be used universally for counting down time from a specific
%%% moment within a single node, without knowing the absolute "wall time".
%%% Use this module if you need to measure a predefined interval or control some
%%% kind of expiry within a single runtime instance. Otherwise, consider using
%%% 'stopwatch' for measuring time elapsed between two moments or 'global_clock'
%%% for reading the absolute global time.
%%%
%%% Uses Erlang's monotonic clock internally for the best accuracy of time passing.
%%% @end
%%%-------------------------------------------------------------------
-module(countdown_timer).
-author("Lukasz Opiola").

-opaque instance() :: time:millis().
-export_type([instance/0]).

-export([start_seconds/1]).
-export([start_millis/1]).
-export([is_expired/1]).
-export([seconds_left/1]).
-export([millis_left/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_seconds(time:seconds()) -> instance().
start_seconds(Seconds) ->
    start_millis(Seconds * 1000).


-spec start_millis(time:millis()) -> instance().
start_millis(Millis) ->
    native_node_clock:monotonic_time_millis() + Millis.


-spec is_expired(instance()) -> boolean().
is_expired(ExpiryMillis) ->
    native_node_clock:monotonic_time_millis() >= ExpiryMillis.


%%--------------------------------------------------------------------
%% @doc
%% Will return 0 if the timer is expired.
%% The result is rounded up to whole seconds.
%% @end
%%--------------------------------------------------------------------
-spec seconds_left(instance()) -> time:seconds().
seconds_left(ExpiryMillis) ->
    ceil(millis_left(ExpiryMillis) / 1000).


%% @doc Will return 0 if the timer is expired.
-spec millis_left(instance()) -> time:millis().
millis_left(ExpiryMillis) ->
    max(0, ExpiryMillis - native_node_clock:monotonic_time_millis()).
