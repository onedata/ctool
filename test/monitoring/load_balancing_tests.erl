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



-endif.