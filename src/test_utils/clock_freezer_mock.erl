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
%%% The mock works only if called by one of whitelisted modules (which are given
%%% during setup) - this way, clock can be frozen selectively for only the
%%% modules that are tested, without affecting the rest of the application logic.
%%%
%%% The setup/teardown API has two versions - for the local node and for
%%% specific nodes(s). In the latter case, it uses the mock manager for mocking.
%%% After setup, the clock_freezer keeps track of the nodes where the mock is
%%% deployed and allows tracking or modifying the current time from a central
%%% point. NOTE that because of this, coordination of the mocking must always be
%%% done from the same node.
%%% @end
%%%-------------------------------------------------------------------
-module(clock_freezer_mock).
-author("Lukasz Opiola").

%% API
-export([setup_for_eunit/1, teardown_for_eunit/0]).
-export([setup_for_ct/2, teardown_for_ct/1]).
-export([simulate_seconds_passing/1]).
-export([simulate_millis_passing/1]).
-export([set_current_time_millis/1]).
-export([current_time_hours/0]).
-export([current_time_seconds/0]).
-export([current_time_millis/0]).
-export([current_time_micros/0]).
-export([current_time_nanos/0]).

%% Exported for internal RPC
-export([reset_global_clock_on_current_node/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec setup_for_eunit([atom()]) -> ok.
setup_for_eunit(WhitelistedModules) ->
    setup_common([node()], WhitelistedModules),
    ok = meck:new(native_node_clock, [passthrough]),
    ok = meck:expect(native_node_clock, system_time_millis, fun mocked_current_time_millis/0),
    ok = meck:expect(native_node_clock, monotonic_time_millis, fun mocked_current_time_millis/0),
    ok = meck:expect(native_node_clock, monotonic_time_nanos, fun mocked_current_time_nanos/0).


-spec teardown_for_eunit() -> ok.
teardown_for_eunit() ->
    ok = meck:unload(native_node_clock).


-spec setup_for_ct(node() | [node()], [atom()]) -> ok.
setup_for_ct(NodeOrNodes, WhitelistedModules) ->
    setup_common(NodeOrNodes, WhitelistedModules),
    rpc_call_each_node(get_target_nodes(), ?MODULE, reset_global_clock_on_current_node, []),
    ok = test_utils:mock_new(NodeOrNodes, native_node_clock, [passthrough]),
    ok = test_utils:mock_expect(NodeOrNodes, native_node_clock, system_time_millis, fun mocked_current_time_millis/0),
    ok = test_utils:mock_expect(NodeOrNodes, native_node_clock, monotonic_time_millis, fun mocked_current_time_millis/0),
    ok = test_utils:mock_expect(NodeOrNodes, native_node_clock, monotonic_time_nanos, fun mocked_current_time_nanos/0).


-spec teardown_for_ct(node() | [node()]) -> ok.
teardown_for_ct(NodeOrNodes) ->
    ok = test_utils:mock_unload(NodeOrNodes, native_node_clock).


-spec simulate_seconds_passing(time:seconds()) -> time:seconds().
simulate_seconds_passing(Seconds) ->
    simulate_millis_passing(Seconds * 1000) div 1000.


-spec simulate_millis_passing(time:millis()) -> time:millis().
simulate_millis_passing(Millis) ->
    NewTimeMillis = current_time_millis() + Millis,
    set_current_time_millis(NewTimeMillis),
    NewTimeMillis.


-spec set_current_time_millis(time:millis()) -> ok.
set_current_time_millis(Millis) ->
    set_current_time_millis_everywhere(Millis).


-spec current_time_hours() -> time:hours().
current_time_hours() ->
    current_time_seconds() div 3600.


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
    current_time_millis() * 1000000.

%%%===================================================================
%%% Exported for internal RPC
%%%===================================================================

-spec reset_global_clock_on_current_node() -> ok.
reset_global_clock_on_current_node() ->
    [AppName | _] = binary:split(atom_to_binary(node(), utf8), <<"@">>),
    % NOTE: this procedure assumes that during CT tests all nodes have node_cache initialized
    % (required by global clock), with exception of some cluster manager nodes - in multinode
    % deployments, backup cluster manager nodes do not have node_cache initialized and will
    % produce badarg errors from node_cache ETS
    try
        global_clock:reset_to_system_time()
    catch
        error:badarg when AppName =:= <<"cluster_manager">> ->
            ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec setup_common(node() | [node()], [atom()]) -> ok.
setup_common(NodeOrNodes, WhitelistedModules) ->
    Nodes = utils:ensure_list(NodeOrNodes),
    set_target_nodes(Nodes),
    ignore_clock_sync_on_target_nodes(),
    set_whitelisted_modules_on_target_nodes(WhitelistedModules),
    set_current_time_millis(starting_frozen_time()).


%% NOTE: env variables are used rather than the node_cache to avoid a circular dependency
%% and a non-obvious requirement to init node cache whenever the clock is mocked

%% @private
-spec set_target_nodes([node()]) -> ok.
set_target_nodes(Nodes) ->
    ctool:set_env(clock_freezer_target_nodes, Nodes).


%% @private
-spec get_target_nodes() -> [node()].
get_target_nodes() ->
    ctool:get_env(clock_freezer_target_nodes).


%% @private
-spec ignore_clock_sync_on_target_nodes() -> ok.
ignore_clock_sync_on_target_nodes() ->
    rpc_call_each_node(get_target_nodes(), ctool, set_env, [clock_sync_ignore_bias_corrections, true]).


%% @private
-spec set_whitelisted_modules_on_target_nodes([atom()]) -> ok.
set_whitelisted_modules_on_target_nodes(Modules) ->
    rpc_call_each_node(get_target_nodes(), ctool, set_env, [clock_freezer_whitelisted_modules, Modules]).


%% @private
%% May be evaluated only on a target node (called in result of meck expect)
-spec is_any_module_whitelisted([atom()]) -> boolean().
is_any_module_whitelisted(Modules) ->
    [] /= lists_utils:intersect(Modules, ctool:get_env(clock_freezer_whitelisted_modules)).


%% @private
%% May be evaluated only on a target node (called in result of meck expect)
-spec mocked_current_time_millis() -> time:millis().
mocked_current_time_millis() ->
    CurrentTimeMillis = current_time_millis(),
    CallingModules = try throw(dummy) catch _:_:Stacktrace -> [M || {M, _, _, _} <- Stacktrace] end,
    case is_any_module_whitelisted(CallingModules) of
        true -> CurrentTimeMillis;
        false -> meck:passthrough([])
    end.


%% @private
%% May be evaluated only on a target node (called in result of meck expect)
-spec mocked_current_time_nanos() -> time:nanos().
mocked_current_time_nanos() ->
    mocked_current_time_millis() * 1000000.


%% @private
-spec set_current_time_millis_everywhere(time:millis()) -> ok.
set_current_time_millis_everywhere(Millis) ->
    % store the time locally as well to allow centralized time management
    rpc_call_each_node(lists_utils:union(get_target_nodes(), [node()]),
        ctool, set_env, [clock_freezer_timestamp_millis, Millis]
    ).


%% @private
%% The call Module:Function(Args) must return ok
-spec rpc_call_each_node([node()], atom(), atom(), list()) -> ok.
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
