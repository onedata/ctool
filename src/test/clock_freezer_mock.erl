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
%%% the clock module internally). Uses meck under the hood.
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
-export([simulate_time_passing/1, simulate_time_passing/2]).
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
    ok = meck:new(clock, [passthrough]),
    ok = meck:expect(clock, timestamp_seconds, fun current_time_seconds/0),
    ok = meck:expect(clock, timestamp_millis, fun current_time_millis/0),
    ok = meck:expect(clock, timestamp_micros, fun current_time_micros/0),
    ok = meck:expect(clock, timestamp_nanos, fun current_time_nanos/0).


-spec teardown() -> ok.
teardown() ->
    ok = meck:unload(clock).


-spec setup(node() | [node()]) -> ok.
setup(NodeOrNodes) ->
    set_current_time_millis(NodeOrNodes, starting_frozen_time()),
    ok = test_utils:mock_new(NodeOrNodes, clock, [passthrough]),
    ok = test_utils:mock_expect(NodeOrNodes, clock, timestamp_seconds, fun current_time_seconds/0),
    ok = test_utils:mock_expect(NodeOrNodes, clock, timestamp_millis, fun current_time_millis/0),
    ok = test_utils:mock_expect(NodeOrNodes, clock, timestamp_micros, fun current_time_micros/0),
    ok = test_utils:mock_expect(NodeOrNodes, clock, timestamp_nanos, fun current_time_nanos/0).


-spec teardown(node() | [node()]) -> ok.
teardown(NodeOrNodes) ->
    ok = test_utils:mock_unload(NodeOrNodes, clock).


-spec simulate_time_passing(clock:millis()) -> ok.
simulate_time_passing(Millis) ->
    set_current_time_millis(current_time_millis() + Millis).


-spec simulate_time_passing(node() | [node()], clock:millis()) -> ok.
simulate_time_passing(NodeOrNodes, Millis) ->
    set_current_time_millis(NodeOrNodes, current_time_millis() + Millis).


-spec set_current_time_millis(clock:millis()) -> ok.
set_current_time_millis(Millis) ->
    ctool:set_env(clock_freezer_timestamp_millis, Millis).


-spec set_current_time_millis(node() | [node()], clock:millis()) -> ok.
set_current_time_millis(NodeOrNodes, Millis) ->
    rpc_call_each_node(NodeOrNodes, ?MODULE, ?FUNCTION_NAME, [Millis]),
    % store the time locally as well to allow tracking the frozen time on this node (using current_time_* functions)
    set_current_time_millis(Millis).


-spec current_time_seconds() -> clock:seconds().
current_time_seconds() ->
    current_time_millis() div 1000.


-spec current_time_millis() -> clock:millis().
current_time_millis() ->
    case ctool:get_env(clock_freezer_timestamp_millis, undefined) of
        Time when is_integer(Time) -> Time;
        undefined -> error(str_utils:format("~p:setup/x must be called first to use the time freezer", [?MODULE]))
    end.


-spec current_time_micros() -> clock:micros().
current_time_micros() ->
    current_time_millis() * 1000.


-spec current_time_nanos() -> clock:nanos().
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
-spec starting_frozen_time() -> clock:millis().
starting_frozen_time() ->
    PreviousFrozenTime = try
        current_time_millis()
    catch _:_ ->
        0
    end,
    max(clock:timestamp_millis(), PreviousFrozenTime).