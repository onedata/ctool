%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Tests of consistent hashing mechanism.
%%% @end
%%%--------------------------------------------------------------------
-module(consistent_hashing_test).
-author("Tomasz Lichon").

-include("hashing/consistent_hashing.hrl").
-include_lib("eunit/include/eunit.hrl").

helpers_test_() ->
    {foreach,
        fun start/0,
        fun stop/1,
        [
            fun chash_should_create_single_node_ring/0,
            fun chash_should_create_multi_node_ring/0,
            fun chash_should_lookup_single_node_ring/0,
            fun chash_should_lookup_multi_node_ring/0,
            fun chash_should_return_all_nodes_in_ring_of_one_node/0,
            fun chash_should_return_all_nodes_in_ring_of_two_nodes/0,
            fun chash_should_return_all_nodes_in_ring/0,
            fun should_get_node_info_single_node_ring/0,
            fun should_get_node_info_multi_node_ring/0,
            fun should_return_many_nodes/0,
            fun should_handle_failed_nodes/0,
            fun should_resize_ring/0
        ]}.

start() ->
    node_cache:init(),
    consistent_hashing:cleanup(),
    meck:new(utils, [passthrough]),
    meck:expect(utils, rpc_multicall, fun(_, consistent_hashing, Fun, Args) ->
        {[apply(consistent_hashing, Fun, Args)], []}
    end).

stop(_) ->
    node_cache:destroy(),
    meck:unload(utils).

chash_should_create_single_node_ring() ->
    ?assertEqual(ok, consistent_hashing:init([node()], 1)).

chash_should_create_multi_node_ring() ->
    ?assertEqual(ok, consistent_hashing:init([node1, node2, node3], 1)).

chash_should_lookup_single_node_ring() ->
    ?assertEqual(ok, consistent_hashing:init([node()], 1)),

    %then
    ?assertEqual(node(), consistent_hashing:get_assigned_node(key1)),
    ?assertEqual(node(), consistent_hashing:get_assigned_node(key2)).

chash_should_lookup_multi_node_ring() ->
    Nodes = [node1, node2, node3, node4, node5],
    ?assertEqual(ok, consistent_hashing:init(Nodes, 1)),

    %when
    Key1NodeFirst = consistent_hashing:get_assigned_node(key1),
    Key1NodeSecond = consistent_hashing:get_assigned_node(key1),
    Key2NodeFirst = consistent_hashing:get_assigned_node(key2),
    Key2NodeSecond = consistent_hashing:get_assigned_node(key2),

    %then
    ?assertEqual(Key1NodeFirst, Key1NodeSecond),
    ?assertEqual(Key2NodeFirst, Key2NodeSecond),
    ?assertNotEqual(Key1NodeFirst, Key2NodeFirst).

chash_should_return_all_nodes_in_ring_of_one_node() ->
    Nodes = [node1],
    ?assertEqual(ok, consistent_hashing:init(Nodes, 1)),

    %then
    ?assertEqual(Nodes, consistent_hashing:get_all_nodes()).

chash_should_return_all_nodes_in_ring_of_two_nodes() ->
    Nodes = [node1, node2],
    ?assertEqual(ok, consistent_hashing:init(Nodes, 1)),

    %then
    ?assertEqual(Nodes, consistent_hashing:get_all_nodes()).

chash_should_return_all_nodes_in_ring() ->
    Nodes = [node1, node2, node3, node4, node5],
    ?assertEqual(ok, consistent_hashing:init(Nodes, 1)),

    %then
    ?assertEqual(Nodes, consistent_hashing:get_all_nodes()).

should_get_node_info_single_node_ring() ->
    ?assertEqual(ok, consistent_hashing:init([node()], 1)),

    %then
    ?assertEqual(#node_routing_info{assigned_nodes = [node()], failed_nodes = [], all_nodes = [node()]},
        consistent_hashing:get_routing_info(key1)),
    ?assertEqual(#node_routing_info{assigned_nodes = [node()], failed_nodes = [], all_nodes = [node()]},
        consistent_hashing:get_routing_info(key2)).

should_get_node_info_multi_node_ring() ->
    Nodes = [node1, node2, node3, node4, node5],
    ?assertEqual(ok, consistent_hashing:init(Nodes, 1)),

    %when
    Key1NodesFirst = check_routing_info(key1, 1, 0, Nodes),
    Key1NodesSecond = check_routing_info(key1, 1, 0, Nodes),
    Key2NodesFirst = check_routing_info(key2, 1, 0, Nodes),
    Key2NodesSecond = check_routing_info(key2, 1, 0, Nodes),

    %then
    ?assertEqual(Key1NodesFirst, Key1NodesSecond),
    ?assertEqual(Key2NodesFirst, Key2NodesSecond),
    ?assertNotEqual(Key1NodesFirst, Key2NodesFirst).

should_return_many_nodes() ->
    Nodes = [node1, node2, node3, node4, node5],
    ?assertEqual(ok, consistent_hashing:init(Nodes, 3)),

    KeyNodes = check_routing_info(key1, 3, 0, Nodes),
    ?assertEqual(3, length(KeyNodes)).

should_handle_failed_nodes() ->
    Nodes = [node1, node2, node3, node4],
    ?assertEqual(ok, consistent_hashing:init(Nodes, 4)),

    ?assertEqual(ok, consistent_hashing:report_node_failure(node3)),
    check_routing_info(key1, 4, 1, Nodes),

    ?assertEqual(ok, consistent_hashing:report_node_recovery(node3)),
    check_routing_info(key1, 4, 0, Nodes).

should_resize_ring() ->
    Nodes = [node1, node2, node3, node4, node5],
    NewNodes = [node1, node2, node3, node4, node5, node6],
    ?assertEqual(ok, consistent_hashing:init(Nodes, 3)),

    ?assertEqual(ok, consistent_hashing:init_cluster_resizing(NewNodes)),
    KeyNodes = check_routing_info(key1, 3, 0, NewNodes, ?FUTURE_RING),
    ?assertEqual(3, length(KeyNodes)),
    KeyNodes2 = check_routing_info(key1, 3, 0, Nodes),
    ?assertEqual(3, length(KeyNodes2)),
    ?assertEqual(undefined, ctool:get_env(?PREVIOUS_RING, undefined)),

    ?assertEqual(ok, consistent_hashing:finalize_cluster_resizing()),
    KeyNodes3 = check_routing_info(key1, 3, 0, NewNodes),
    ?assertEqual(3, length(KeyNodes3)),
    KeyNodes4 = check_routing_info(key1, 3, 0, Nodes, ?PREVIOUS_RING),
    ?assertEqual(3, length(KeyNodes4)),
    ?assertEqual(undefined, ctool:get_env(?FUTURE_RING, undefined)).

check_routing_info(Key, KeyNodesNum, FailedNodesNum, Nodes) ->
    check_routing_info(Key, KeyNodesNum, FailedNodesNum, Nodes, ?CURRENT_RING).

check_routing_info(Key, KeyNodesNum, FailedNodesNum, Nodes, RingGeneration) ->
    Test = consistent_hashing:get_routing_info(RingGeneration, Key),
    ?assertMatch(#node_routing_info{all_nodes = Nodes}, Test),
    #node_routing_info{assigned_nodes = KeyNodes, failed_nodes = FailedNodes} = Test,
    ?assertEqual(KeyNodesNum, length(KeyNodes)),
    ?assertEqual(FailedNodesNum, length(FailedNodes)),
    KeyNodes.