%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module MUST be used universally for measuring time elapsed between two
%%% moments within a single node, without knowing the absolute "wall time". Uses
%%% Erlang's monotonic clock under the hood for the best accuracy of time passing.
%%% @end
%%%-------------------------------------------------------------------
-module(stopwatch).
-author("Lukasz Opiola").

-opaque instance() :: time:nanos().
-export_type([instance/0]).

-export([start/0]).
-export([read_seconds/1]).
-export([read_millis/1]).
-export([read_micros/1]).
-export([read_nanos/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> instance().
start() ->
    native_node_clock:monotonic_time_nanos().


-spec read_seconds(instance()) -> time:seconds().
read_seconds(StartTimeNanos) ->
    read_nanos(StartTimeNanos) div 1000000000.


-spec read_millis(instance()) -> time:millis().
read_millis(StartTimeNanos) ->
    read_nanos(StartTimeNanos) div 1000000.


-spec read_micros(instance()) -> time:micros().
read_micros(StartTimeNanos) ->
    read_nanos(StartTimeNanos) div 1000.


-spec read_nanos(instance()) -> time:nanos().
read_nanos(StartTimeNanos) ->
    native_node_clock:monotonic_time_nanos() - StartTimeNanos.
