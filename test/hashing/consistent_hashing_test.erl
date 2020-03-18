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
            fun should_handle_failed_nodes/0
        ]}.

start() ->
    consistent_hashing:cleanup(),
    meck:new(consistent_hashing, [passthrough]),
    meck:expect(consistent_hashing, replicate_ring_to_nodes, fun(_) -> ok end).

stop(_) ->
    meck:unload(consistent_hashing).

chash_should_create_single_node_ring() ->
    ?assertEqual(ok, consistent_hashing:init([node()], 1)).

chash_should_create_multi_node_ring() ->
    ?assertEqual(ok, consistent_hashing:init([node1, node2, node3], 1)).

chash_should_lookup_single_node_ring() ->
    ?assertEqual(ok, consistent_hashing:init([node()], 1)),

    %then
    ?assertEqual(node(), consistent_hashing:get_node(key1)),
    ?assertEqual(node(), consistent_hashing:get_node(key2)).

chash_should_lookup_multi_node_ring() ->
    Nodes = [node1, node2, node3, node4, node5],
    ?assertEqual(ok, consistent_hashing:init(Nodes, 1)),

    %when
    Key1NodeFirst = consistent_hashing:get_node(key1),
    Key1NodeSecond = consistent_hashing:get_node(key1),
    Key2NodeFirst = consistent_hashing:get_node(key2),
    Key2NodeSecond = consistent_hashing:get_node(key2),

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
    ?assertEqual({[node()], [], [node()]}, consistent_hashing:get_full_node_info(key1)),
    ?assertEqual({[node()], [], [node()]}, consistent_hashing:get_full_node_info(key2)).

should_get_node_info_multi_node_ring() ->
    Nodes = [node1, node2, node3, node4, node5],
    ?assertEqual(ok, consistent_hashing:init(Nodes, 1)),

    %when
    Test1 = consistent_hashing:get_full_node_info(key1),
    ?assertMatch({[_], [], Nodes}, Test1),
    {Key1NodesFirst, _, _} = Test1,

    Test2 = consistent_hashing:get_full_node_info(key1),
    ?assertMatch({[_], [], Nodes}, Test2),
    {Key1NodesSecond, _, _} = Test2,

    Test3 = consistent_hashing:get_full_node_info(key2),
    ?assertMatch({[_], [], Nodes}, Test3),
    {Key2NodesFirst, _, _} = Test3,

    Test4 = consistent_hashing:get_full_node_info(key2),
    ?assertMatch({[_], [], Nodes}, Test4),
    {Key2NodesSecond, _, _} = Test4,

    %then
    ?assertEqual(Key1NodesFirst, Key1NodesSecond),
    ?assertEqual(Key2NodesFirst, Key2NodesSecond),
    ?assertNotEqual(Key1NodesFirst, Key2NodesFirst).

should_return_many_nodes() ->
    Nodes = [node1, node2, node3, node4, node5],
    ?assertEqual(ok, consistent_hashing:init(Nodes, 3)),

    Test = consistent_hashing:get_full_node_info(key1),
    ?assertMatch({_, [], Nodes}, Test),
    {Nodes1, _, _} = Test,
    ?assertEqual(3, length(Nodes1)).

should_handle_failed_nodes() ->
    Nodes = [node1, node2, node3, node4],
    ?assertEqual(ok, consistent_hashing:init(Nodes, 4)),

    ?assertEqual(ok, consistent_hashing:report_node_failure(node3)),

    Test1 = consistent_hashing:get_full_node_info(key1),
    ?assertMatch({_, [_], Nodes}, Test1),
    {Nodes1, _, _} = Test1,
    ?assertEqual(4, length(Nodes1)),

    ?assertEqual(ok, consistent_hashing:report_node_recovery(node3)),

    Test2 = consistent_hashing:get_full_node_info(key1),
    ?assertMatch({_, [], Nodes}, Test2),
    {Nodes2, _, _} = Test2,
    ?assertEqual(4, length(Nodes2)).