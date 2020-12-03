%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Merely a wrapper for the native Erlang's timestamp functions for better
%%% code modularisation and ease of mocking.
%%%
%%% WARNING: only for internal time logic, DO NOT USE DIRECTLY.
%%% Use below modules for time dependent logic:
%%%   * 'global_clock'
%%%   * 'stopwatch'
%%%   * 'countdown_timer'
%%% @end
%%%-------------------------------------------------------------------
-module(native_node_clock).
-author("Lukasz Opiola").

-export([system_time_millis/0]).
-export([monotonic_time_millis/0]).
-export([monotonic_time_nanos/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec system_time_millis() -> time:millis().
system_time_millis() ->
    erlang:system_time(millisecond).


-spec monotonic_time_millis() -> time:millis().
monotonic_time_millis() ->
    erlang:monotonic_time(millisecond).


-spec monotonic_time_nanos() -> time:nanos().
monotonic_time_nanos() ->
    erlang:monotonic_time(nanosecond).
