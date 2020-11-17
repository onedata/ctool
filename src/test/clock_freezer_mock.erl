%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module allows freezing the clock on local or remote nodes so that it
%%% always returns the same time and one can manually simulate time passing.
%%% Dedicated for tests of time dependent logic (requires that the logic uses
%%% the ctool's time management modules internally). Uses meck under the hood.
%%%
%%% The API has two version - for the local node and for specific nodes(s).
%%% In the latter case, it uses the mock manager for mocking (via test_utils)
%%% and stores the frozen time on the calling node so that it is possible
%%% to track the time using current_time_* functions.
%%% @end
%%%-------------------------------------------------------------------
-module(clock_freezer_mock).
-author("Lukasz Opiola").

%% API
-export([setup/0, teardown/0]).
-export([setup/1, teardown/1]).
-export([simulate_seconds_passing/1, simulate_seconds_passing/2]).
-export([simulate_millis_passing/1, simulate_millis_passing/2]).
-export([set_current_time_millis/1, set_current_time_millis/2]).
-export([current_time_seconds/0]).
-export([current_time_millis/0]).
-export([current_time_micros/0]).
-export([current_time_nanos/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec setup() -> ok.
setup() ->
    set_current_time_millis(starting_frozen_time()),
    ok = meck:new(native_node_clock, [passthrough]),
    ok = meck:expect(native_node_clock, system_time_millis, fun current_time_millis/0),
    ok = meck:expect(native_node_clock, monotonic_time_millis, fun current_time_millis/0),
    ok = meck:expect(native_node_clock, monotonic_time_nanos, fun current_time_nanos/0).


-spec teardown() -> ok.
teardown() ->
    ok = meck:unload(native_node_clock).


-spec setup(node() | [node()]) -> ok.
setup(NodeOrNodes) ->
    set_current_time_millis(NodeOrNodes, starting_frozen_time()),
    ok = test_utils:mock_new(NodeOrNodes, native_node_clock, [passthrough]),
    ok = test_utils:mock_expect(NodeOrNodes, native_node_clock, system_time_millis, fun current_time_millis/0),
    ok = test_utils:mock_expect(NodeOrNodes, native_node_clock, monotonic_time_millis, fun current_time_millis/0),
    ok = test_utils:mock_expect(NodeOrNodes, native_node_clock, monotonic_time_nanos, fun current_time_nanos/0).


-spec teardown(node() | [node()]) -> ok.
teardown(NodeOrNodes) ->
    ok = test_utils:mock_unload(NodeOrNodes, native_node_clock).


-spec simulate_seconds_passing(time:seconds()) -> ok.
simulate_seconds_passing(Seconds) ->
    simulate_millis_passing(Seconds * 1000).

-spec simulate_seconds_passing(node() | [node()], time:seconds()) -> ok.
simulate_seconds_passing(NodeOrNodes, Seconds) ->
    simulate_millis_passing(NodeOrNodes, Seconds * 1000).


-spec simulate_millis_passing(time:millis()) -> ok.
simulate_millis_passing(Millis) ->
    set_current_time_millis(current_time_millis() + Millis).

-spec simulate_millis_passing(node() | [node()], time:millis()) -> ok.
simulate_millis_passing(NodeOrNodes, Millis) ->
    set_current_time_millis(NodeOrNodes, current_time_millis() + Millis).


-spec set_current_time_millis(time:millis()) -> ok.
set_current_time_millis(Millis) ->
    % use an env variable rather than node_cache to avoid a circular dependency
    % and a non-obvious requirement to init node cache whenever the clock is mocked
    ctool:set_env(clock_freezer_timestamp_millis, Millis).

-spec set_current_time_millis(node() | [node()], time:millis()) -> ok.
set_current_time_millis(NodeOrNodes, Millis) ->
    rpc_call_each_node(NodeOrNodes, ?MODULE, ?FUNCTION_NAME, [Millis]),
    % store the time locally as well to allow tracking the frozen time on this node (using current_time_* functions)
    set_current_time_millis(Millis).


-spec current_time_seconds() -> time:seconds().
current_time_seconds() ->
    current_time_millis() div 1000.

-spec current_time_millis() -> time:millis().
current_time_millis() ->
    case ctool:get_env(clock_freezer_timestamp_millis, undefined) of
        Time when is_integer(Time) -> Time;
        undefined -> error(str_utils:format("~p:setup/x must be called first to use the clock freezer", [?MODULE]))
    end.

-spec current_time_micros() -> time:micros().
current_time_micros() ->
    current_time_millis() * 1000.

-spec current_time_nanos() -> time:nanos().
current_time_nanos() ->
    current_time_millis() * 1000 * 1000.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% The call Module:Function(Args) must return ok
-spec rpc_call_each_node(node() | [node()], atom(), atom(), list()) -> ok.
rpc_call_each_node(Node, Module, Function, Args) when is_atom(Node) ->
    rpc_call_each_node([Node], Module, Function, Args);
rpc_call_each_node(Nodes, Module, Function, Args) ->
    lists:foreach(fun(Node) ->
        ok = rpc:call(Node, Module, Function, Args)
    end, Nodes).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This is to make sure that tests using multiple instances of freezer mock do
%% not experience going back in time. If the time was frozen previously, the
%% ending timestamp might be much higher than the actual system timestamp (due
%% to simulation of passing time). This returns the bigger of the two times.
%% @end
%%--------------------------------------------------------------------
-spec starting_frozen_time() -> time:millis().
starting_frozen_time() ->
    PreviousFrozenTime = try
        current_time_millis()
    catch _:_ ->
        0
    end,
    max(native_node_clock:system_time_millis(), PreviousFrozenTime).
