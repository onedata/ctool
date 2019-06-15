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

-include("global_definitions.hrl").
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
            fun chash_should_return_all_nodes_in_ring/0
        ]}.

start() ->
    consistent_hashing:cleanup().

stop(_) ->
    ok.

chash_should_create_single_node_ring() ->
    ?assertEqual(ok, consistent_hashing:init([node()])).

chash_should_create_multi_node_ring() ->
    ?assertEqual(ok, consistent_hashing:init([node1, node2, node3])).

chash_should_lookup_single_node_ring() ->
    ?assertEqual(ok, consistent_hashing:init([node()])),

    %then
    ?assertEqual(node(), consistent_hashing:get_node(key1)),
    ?assertEqual(node(), consistent_hashing:get_node(key2)).

chash_should_lookup_multi_node_ring() ->
    Nodes = [node1, node2, node3, node4, node5],
    ?assertEqual(ok, consistent_hashing:init(Nodes)),

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
    ?assertEqual(ok, consistent_hashing:init(Nodes)),

    %then
    ?assertEqual(Nodes, consistent_hashing:get_all_nodes()).

chash_should_return_all_nodes_in_ring_of_two_nodes() ->
    Nodes = [node1, node2],
    ?assertEqual(ok, consistent_hashing:init(Nodes)),

    %then
    ?assertEqual(Nodes, consistent_hashing:get_all_nodes()).

chash_should_return_all_nodes_in_ring() ->
    Nodes = [node1, node2, node3, node4, node5],
    ?assertEqual(ok, consistent_hashing:init(Nodes)),

    %then
    ?assertEqual(Nodes, consistent_hashing:get_all_nodes()).
