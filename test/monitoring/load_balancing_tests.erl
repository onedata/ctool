%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Unit tests for monitoring module.
%%% @end
%%%--------------------------------------------------------------------
-module(load_balancing_tests).
-author("Lukasz Opiola").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("monitoring/monitoring.hrl").
-include("logging.hrl").

% Copies of records from load_balancing module
-record(dns_lb_advice, {
    nodes_and_frequency = [] :: [{Node :: term(), Frequency :: float()}],
    node_choices = [] :: [Node :: term()]
}).

-record(dispatcher_lb_advice, {
    should_delegate = false,
    nodes_and_frequency = [] :: [{Node :: node(), Frequency :: float()}],
    all_nodes = [] :: [node()]
}).

-record(load_balancing_state, {
    expected_extra_load = [] :: [{Node :: node(), ExtraLoad :: float()}]
}).

dns_advices_test() ->
    Node1 = node1,
    IP1 = {200, 0, 0, 1},
    NS1 = #node_state{
        node = Node1,
        ip_addr = IP1,
        cpu_usage = 80.0,
        mem_usage = 80.0,
        net_usage = 80.0
    },
    Node2 = node2,
    IP2 = {200, 0, 0, 2},
    NS2 = #node_state{
        node = Node2,
        ip_addr = IP2,
        cpu_usage = 40.0,
        mem_usage = 40.0,
        net_usage = 40.0
    },
    Node3 = node3,
    IP3 = {200, 0, 0, 3},
    NS3 = #node_state{
        node = Node3,
        ip_addr = IP3,
        cpu_usage = 60.0,
        mem_usage = 60.0,
        net_usage = 60.0
    },
    NodeStates = [NS1, NS2, NS3],
    LBState = #load_balancing_state{},
    {Result, NewLBState} = load_balancing:advices_for_dnses(NodeStates, LBState),
    ?assertEqual(NewLBState, LBState),
    Advice1 = proplists:get_value(Node1, Result),
    Advice2 = proplists:get_value(Node2, Result),
    Advice3 = proplists:get_value(Node3, Result),
    ?assertEqual(Advice1, Advice2),
    ?assertEqual(Advice1, Advice3),
    #dns_lb_advice{
        nodes_and_frequency = NodesAndFrequency,
        node_choices = NodeChoices
    } = Advice1,
    ?assertEqual(NodeChoices, [IP1, IP2, IP3, IP1, IP2, IP3]),
    Freq1 = proplists:get_value(IP1, NodesAndFrequency),
    Freq2 = proplists:get_value(IP2, NodesAndFrequency),
    Freq3 = proplists:get_value(IP3, NodesAndFrequency),
    ?assert(Freq1 < Freq2),
    ?assert(Freq1 < Freq3),
    ?assert(Freq3 < Freq2),
    ok.

dispatcher_advices_test() ->
    Node1 = node1,
    IP1 = {200, 0, 0, 1},
    NS1 = #node_state{
        node = Node1,
        ip_addr = IP1,
        cpu_usage = 80.0,
        mem_usage = 80.0
    },
    Node2 = node2,
    IP2 = {200, 0, 0, 2},
    NS2 = #node_state{
        node = Node2,
        ip_addr = IP2,
        cpu_usage = 20.0,
        mem_usage = 20.0
    },
    Node3 = node3,
    IP3 = {200, 0, 0, 3},
    NS3 = #node_state{
        node = Node3,
        ip_addr = IP3,
        cpu_usage = 60.0,
        mem_usage = 60.0
    },
    Node4 = node4,
    IP4 = {200, 0, 0, 4},
    NS4 = #node_state{
        node = Node4,
        ip_addr = IP4,
        cpu_usage = 40.0,
        mem_usage = 40.0
    },
    NodeStates = [NS1, NS2, NS3, NS4],
    LBState = #load_balancing_state{},
    {Result, NewLBState} = load_balancing:advices_for_dispatchers(NodeStates, LBState),
    % Nodes 2 and 4 should be getting extra load (delegated requests)
    % Node 2 should get more delegation as its less loaded
    #load_balancing_state{expected_extra_load = EEL} = NewLBState,
    ?assert(proplists:get_value(Node2, EEL) > proplists:get_value(Node4, EEL)),
    ?assertEqual(length(EEL), 2),
    Nodes1To4 = [Node1, Node2, Node3, Node4],
    ExpectedShouldDelegate =
        [{Node1, true}, {Node2, false}, {Node3, true}, {Node4, false}],
    lists:foreach(
        fun(Node) ->
            #dispatcher_lb_advice{
                nodes_and_frequency = NodesAndFreq,
                all_nodes = AllNodes,
                should_delegate = ShouldDelegate
            } = proplists:get_value(Node, Result),
            ?assertEqual(AllNodes, Nodes1To4),
            ?assertEqual(ShouldDelegate, proplists:get_value(Node, ExpectedShouldDelegate)),
            case ShouldDelegate of
                false ->
                    ?assertEqual(NodesAndFreq, []);
                true ->
                    ?assert(proplists:get_value(Node2, NodesAndFreq) >
                        proplists:get_value(Node4, NodesAndFreq)),
                    ?assertNotEqual(proplists:get_value(Node, NodesAndFreq), undefined),
                    ?assertEqual(length(NodesAndFreq), 3)
            end
        end, Nodes1To4),
    ok.

choose_nodes_for_dns_test() ->
    IP1 = {200, 0, 0, 1},
    IP2 = {200, 0, 0, 2},
    IP3 = {200, 0, 0, 3},
    IP4 = {200, 0, 0, 4},
    NodeChoices = [IP1, IP2, IP3, IP4, IP1, IP2, IP3, IP4],
    DNSAdvice = #dns_lb_advice{
        nodes_and_frequency = [{IP1, 0.1}, {IP2, 0.2}, {IP3, 0.3}, {IP4, 0.4}],
        node_choices = NodeChoices},
    lists:foreach(
        fun(_) ->
            [FirstNode | _] = Nodes = load_balancing:choose_nodes_for_dns(DNSAdvice),
            ?assertEqual(length(Nodes), 4),
            ExpectedListLong = lists:dropwhile(
                fun(X) ->
                    X =/= FirstNode
                end, NodeChoices),
            ExpectedList = lists:sublist(ExpectedListLong, 4),
            ?assertEqual(Nodes, ExpectedList)
        end, lists:seq(1, 50)),
    ok.

choose_ns_nodes_for_dns_test() ->
    IP1 = {200, 0, 0, 1},
    IP2 = {200, 0, 0, 2},
    IP3 = {200, 0, 0, 3},
    IP4 = {200, 0, 0, 4},
    NodeChoices = [IP1, IP2, IP3, IP4, IP1, IP2, IP3, IP4],
    DNSAdvice = #dns_lb_advice{
        nodes_and_frequency = [{IP1, 0.1}, {IP2, 0.2}, {IP3, 0.3}, {IP4, 0.4}],
        node_choices = NodeChoices},
    lists:foreach(
        fun(_) ->
            [FirstNode | _] = Nodes = load_balancing:choose_ns_nodes_for_dns(DNSAdvice),
            ?assertEqual(length(Nodes), 4),
            ExpectedListLong = lists:dropwhile(
                fun(X) ->
                    X =/= FirstNode
                end, NodeChoices),
            ExpectedList = lists:sublist(ExpectedListLong, 4),
            ?assertEqual(Nodes, ExpectedList)
        end, lists:seq(1, 50)),
    ok.


choose_nodes_for_dispatcher_test() ->
    Node1 = node1,
    Node2 = node2,
    Node3 = node3,
    Node4 = node4,
    Nodes = [Node1, Node2, Node3, Node4],
    DispAdviceNoDel = #dispatcher_lb_advice{
        nodes_and_frequency = [],
        should_delegate = false,
        all_nodes = Nodes
    },
    lists:foreach(
        fun(_) ->
            Node = load_balancing:choose_node_for_dispatcher(DispAdviceNoDel, dummy_worker),
            ?assertEqual(Node, node())
        end, lists:seq(1, 50)),
    DispAdviceDel = #dispatcher_lb_advice{
        nodes_and_frequency = [{Node1, 0.3}, {Node3, 0.6}, {Node4, 0.1}],
        should_delegate = true,
        all_nodes = Nodes
    },
    lists:foreach(
        fun(_) ->
            Node = load_balancing:choose_node_for_dispatcher(DispAdviceDel, dummy_worker),
            ?assert(lists:member(Node, [Node1, Node3, Node4]))
        end, lists:seq(1, 50)),
    ?assertEqual(load_balancing:all_nodes_for_dispatcher(DispAdviceNoDel), Nodes),
    ?assertEqual(load_balancing:all_nodes_for_dispatcher(DispAdviceDel), Nodes),
    ok.

choose_index_test() ->
    NodesAndFreq = [{node1, 0.31}, {node2, 0.16}, {node3, 0.32}, {node4, 0.21}],
    ?assertEqual(1, load_balancing:choose_index(NodesAndFreq, 0, 0.12)),
    ?assertEqual(1, load_balancing:choose_index(NodesAndFreq, 0, 0.309999)),
    ?assertEqual(2, load_balancing:choose_index(NodesAndFreq, 0, 0.311)),
    ?assertEqual(2, load_balancing:choose_index(NodesAndFreq, 0, 0.46999)),
    ?assertEqual(3, load_balancing:choose_index(NodesAndFreq, 0, 0.471)),
    ?assertEqual(3, load_balancing:choose_index(NodesAndFreq, 0, 0.57)),
    ?assertEqual(3, load_balancing:choose_index(NodesAndFreq, 0, 0.7899)),
    ?assertEqual(4, load_balancing:choose_index(NodesAndFreq, 0, 0.791)),
    ?assertEqual(4, load_balancing:choose_index(NodesAndFreq, 0, 0.839)),
    ?assertEqual(4, load_balancing:choose_index(NodesAndFreq, 0, 0.999)),
    ?assertEqual(4, load_balancing:choose_index(NodesAndFreq, 0, 1.0)),
    ok.

average_test() ->
    Values = [0.56, 0.234, 0.64, 0.122, 0.667, 0.185, 0.11, 6.7, 8.13, 4.234],
    ?assertEqual(load_balancing:average(Values), 2.1582).

-endif.