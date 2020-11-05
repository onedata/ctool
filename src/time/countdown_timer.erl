%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module MUST be used universally for counting down time from a specific
%%% moment within a single node, without knowing the absolute "wall time". Uses
%%% Erlang's monotonic clock under the hood for the best accuracy of time passing.
%%% @end
%%%-------------------------------------------------------------------
-module(countdown_timer).
-author("Lukasz Opiola").

-opaque instance() :: time:millis() | time:infinity().
-export_type([instance/0]).

-export([start_seconds/1]).
-export([start_millis/1]).
-export([is_expired/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_seconds(time:seconds() | time:infinity()) -> instance().
start_seconds(infinity) ->
    infinity;
start_seconds(Seconds) ->
    start_millis(Seconds * 1000).


-spec start_millis(time:millis() | time:infinity()) -> instance().
start_millis(infinity) ->
    infinity;
start_millis(Millis) ->
    native_node_clock:monotonic_time_millis() + Millis.


-spec is_expired(instance()) -> boolean().
is_expired(infinity) ->
    false;
is_expired(ExpiryMillis) ->
    native_node_clock:monotonic_time_millis() >= ExpiryMillis.
