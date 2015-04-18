%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This library contains load balancing logic.
%%% @end
%%%-------------------------------------------------------------------
-module(load_balancing).
-author("lopiola").

-include("logging.hrl").
-include("monitoring/monitoring.hrl").

% Record used to express how DNS should be answering. Such records are distributed
% to DNS modules, and later they evaluate choose_nodes_for_dns/1 function on this
% record. The record structure shall not be visible outside this module.
-record(dns_lb_advice, {
    % List of 2-element tuples {Node, Frequency}.
    % Node is an IP address.
    % Frequency (relative to other nodes) is how often
    %   should given node appear at the top of DNS response.
    nodes_and_frequency = [] :: [{Node :: term(), Frequency :: float()}],
    % List of nodes to choose from. It introduces redundancy to above,
    % but consider that it will be used multiple times and each of these
    % times it would have to be calculated.
    % This list is a list of nodes, duplicated once, so we can easily
    % pick N consecutive nodes starting from any between 1 and N.
    node_choices = [] :: [Node :: term()]
}).


% Record used to express how dispatchers should be rerouting requests.
% Such records are distributed to dispatcher modules,
% and later they evaluate choose_node_for_dispatcher/1 function on this
% record. The record structure shall not be visible outside this module.
-record(dispatcher_lb_advice, {
    % Flag meaning if any requests should be rerouted to other nodes.
    should_delegate = false,
    % List of 3-element tuples {Node, Frequency, RequestCount}.
    % Frequency (relative to other nodes) is how often
    %   should requests be rerouted to given node.
    % RequestCount is how many times a request WAS redirected to given node
    %   during using this advice.
    nodes_and_frequency = [] :: [{Node :: node(), Frequency :: float()}],
    % List of all nodes (dispatcher needs such knowledge for multicalls)
    all_nodes = [] :: [node()]
}).


% This records holds a state that should be passed between each calculation
% of load balancing advices. It contains data from previous calculations
% that is needed in next ones.
-record(load_balancing_state, {
    % Contains information how much extra load is expected on given node
    % in current load balancing interval. Extra load is estimated load
    % caused by delegation.
    expected_extra_load = [] :: [{Node :: node(), ExtraLoad :: float()}]
}).

-type dns_lb_advice() :: #dns_lb_advice{}.
-type dispatcher_lb_advice() :: #dispatcher_lb_advice{}.
-type load_balancing_state() :: #load_balancing_state{}.

-export_type([dns_lb_advice/0, dispatcher_lb_advice/0, load_balancing_state/0]).

%% API
-export([advices_for_dnses/1, choose_nodes_for_dns/1, choose_ns_nodes_for_dns/1, initial_advice_for_dns/1]).
-export([advices_for_dispatchers/2, choose_node_for_dispatcher/2]).
-export([initial_advice_for_dispatcher/0, all_nodes_for_dispatcher/1]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns guidelines that should be used by DNS servers in the cluster,
%% based on node states of all nodes. The list must not be empty.
%% @end
%%--------------------------------------------------------------------
-spec advices_for_dnses(NodeStates :: [#node_state{}]) -> [{node(), #dns_lb_advice{}}].
advices_for_dnses(NodeStates) ->
    NodesAndIPs = [{NodeState#node_state.node, NodeState#node_state.ip_addr} || NodeState <- NodeStates],
    LoadsForDNS = [load_for_dns(NodeState) || NodeState <- NodeStates],
    MinLoadForDNS = lists:min(LoadsForDNS),
    LoadsAndNodes = lists:zip(LoadsForDNS, NodesAndIPs),
    % Calculate how often should given nodes appear in the first row in DNS
    % responses. The bigger the load, the less often should the node appear.
    NodesAndFrequency = [{NodeIP, MinLoadForDNS / Load} || {Load, {_, NodeIP}} <- LoadsAndNodes],
    FrequencySum = lists:foldl(fun({_, Freq}, Acc) -> Acc + Freq end, 0.0, NodesAndFrequency),
    NormalizedNodesAndFrequency = [{NodeIP, Freq / FrequencySum} || {NodeIP, Freq} <- NodesAndFrequency],
    NodeChoices = [NodeIP || {NodeIP, _} <- NormalizedNodesAndFrequency],
    NodeChoicesDuplicated = NodeChoices ++ NodeChoices,
    Result = lists:map(
        fun({_, {Node, _}}) ->
            {Node, #dns_lb_advice{nodes_and_frequency = NormalizedNodesAndFrequency,
                node_choices = NodeChoicesDuplicated}}
        end, LoadsAndNodes),
    Result.


%%--------------------------------------------------------------------
%% @doc
%% Returns guidelines that should be used by dispatchers in the cluster,
%% based on node states of all nodes. The list must not be empty.
%% @end
%%--------------------------------------------------------------------
-spec advices_for_dispatchers(NodeStates :: [#node_state{}], LBState :: #load_balancing_state{}) ->
    [{node(), #dispatcher_lb_advice{}}].
advices_for_dispatchers(NodeStates, #load_balancing_state{expected_extra_load = ExtraLoads} = LBState) ->
    Nodes = [NodeState#node_state.node || NodeState <- NodeStates],
    LoadsForDisp = lists:foreach(
        fun(NodeState) ->
            L = load_for_dispatcher(NodeState),
            ExtraLoad = proplists:get_value(NodeState#node_state.node, ExtraLoads, 0.0),
            L * (1.0 - ExtraLoad)
        end, NodeStates),
    AvgLoadForDisp = average(LoadsForDisp),
    MinLoadForDisp = lists:min(LoadsForDisp),
    LoadsAndNodes = lists:zip(LoadsForDisp, Nodes),
    % Nodes that are loaded less than average
    FreeNodes = lists:filter(
        fun({Load, _}) ->
            Load =< AvgLoadForDisp
        end, LoadsAndNodes),
    % Nodes that are overloaded
    OverloadedNodes = lists:filtermap(
        fun({Load, Node}) ->
            case overloaded_for_dispatcher(Load, MinLoadForDisp) of
                false -> false;
                true -> {true, Node}
            end
        end, LoadsAndNodes),
    OverloadedNodesNum = length(OverloadedNodes),
    Result = lists:map(
        fun({Load, Node}) ->
            ShouldDelegate = lists:member(Node, OverloadedNodes),
            NodesAndFrequency =
                case ShouldDelegate of
                    true ->
                        OtherNodes = [{FNode, MinLoadForDisp / FNLoad / OverloadedNodesNum} || {FNLoad, FNode} <- FreeNodes],
                        NAndF = [{Node, MinLoadForDisp / Load} | OtherNodes],
                        FrequencySum = lists:foldl(fun({_, Freq}, Acc) -> Acc + Freq end, 0.0, NAndF),
                        [{Node, Freq / FrequencySum} || {Node, Freq} <- NAndF];
                    false ->
                        []
                end,
            {Node, #dispatcher_lb_advice{should_delegate = ShouldDelegate,
                nodes_and_frequency = NodesAndFrequency, all_nodes = Nodes}}
        end, LoadsAndNodes),
    % Calculate expected extra loads on each node. For example:
    % node A is overloaded and will delegate 70% reqs to node B
    % node B in not overloaded
    % expected extra load on each node is:
    % node A = 0.0
    %                0.7 * node_A_load
    % node B = -------------------------------
    %          node_B_load + 0.7 * node_A_load
    %
    ExtraLoadsSum = lists:foldl(
        fun({NodeFrom, DispLBAdvice}, ExtraLoadsAcc) ->
            #dispatcher_lb_advice{
                should_delegate = ShouldDelegate,
                nodes_and_frequency = NodesAndFreq} = DispLBAdvice,
            case ShouldDelegate of
                false ->
                    ExtraLoadsAcc;
                true ->
                    lists:foldl(
                        fun({NodeTo, Freq}, Acc) ->
                            case NodeFrom of
                                NodeTo ->
                                    Acc;
                                _ ->
                                    Current = proplists:get_value(NodeTo, Acc, 0.0),
                                    NodeToLoad = proplists:get_value(NodeTo, LoadsAndNodes, 0.0),
                                    [{NodeTo, Current + NodeToLoad * Freq} | proplists:delete(NodeTo, Acc)]
                            end
                        end, ExtraLoadsAcc, NodesAndFreq)
            end
        end, [], Result),
    ?dump(ExtraLoadsSum),
    NewExtraLoads = lists:map(
        fun({Node, ExtraLoadSum}) ->
            NodeLoad = proplists:get_value(Node, LoadsAndNodes, 0.0),
            ExtraLoadsSum / (NodeLoad + ExtraLoadsSum)
        end, ExtraLoadsSum),
    ?dump(NewExtraLoads),
    {Result, LBState#load_balancing_state{expected_extra_load = NewExtraLoads}}.


%%--------------------------------------------------------------------
%% @doc
%% Returns list of nodes that should be returned as A records in DNS reply.
%% The records are shuffled according to given load balancing advice.
%% @end
%%--------------------------------------------------------------------
-spec choose_nodes_for_dns(DSNAdvice :: #dns_lb_advice{}) ->
    {[{A :: byte(), B :: byte(), C :: byte(), D :: byte()}], #dns_lb_advice{}}.
choose_nodes_for_dns(DNSAdvice) ->
    #dns_lb_advice{nodes_and_frequency = NodesAndFreq,
        node_choices = NodeChoices} = DNSAdvice,
    Index = choose_index(NodesAndFreq),
    Nodes = lists:sublist(NodeChoices, Index, length(NodesAndFreq)),
    Nodes.


%%--------------------------------------------------------------------
%% @doc
%% Returns list of nodes that should be returned as NS records in DNS reply.
%% As DNS servers work on every node, its always a full list of nodes, but shuffled.
%% @end
%%--------------------------------------------------------------------
-spec choose_ns_nodes_for_dns(DSNAdvice :: #dns_lb_advice{}) ->
    {[{A :: byte(), B :: byte(), C :: byte(), D :: byte()}], #dns_lb_advice{}}.
choose_ns_nodes_for_dns(DNSAdvice) ->
    #dns_lb_advice{nodes_and_frequency = NodesAndFreq,
        node_choices = NodeChoices} = DNSAdvice,
    NumberOfNodes = length(NodesAndFreq),
    random:seed(now()),
    Index = random:uniform(NumberOfNodes),
    Nodes = lists:sublist(NodeChoices, Index, NumberOfNodes),
    Nodes.


%%--------------------------------------------------------------------
%% @doc
%% Returns guidelines that should be used by dispatchers in the cluster,
%% based on node states of all nodes.
%% @end
%%--------------------------------------------------------------------
-spec choose_node_for_dispatcher(DSNAdvice :: #dispatcher_lb_advice{}, WorkerName :: atom()) -> node().
choose_node_for_dispatcher(Advice, WorkerName) ->
    #dispatcher_lb_advice{should_delegate = ShouldDelegate,
        nodes_and_frequency = NodesAndFreq} = Advice,
    Result = case ShouldDelegate of
                 false ->
                     node();
                 true ->
                     Index = choose_index(NodesAndFreq),
                     {Node, _} = lists:nth(Index, NodesAndFreq),
                     Node
             end,

    % TODO testy
    case node() of
        Result -> ok;
        _ -> io:format("del ~p -> ~p~n", [WorkerName, Result])
    end,
    Result.


%%--------------------------------------------------------------------
%% @doc
%% Returns an initial advice for DNS that can be used before CCM
%% starts broadcasting advices.
%% @end
%%--------------------------------------------------------------------
-spec initial_advice_for_dns(NodeIP :: {A :: byte(), B :: byte(), C :: byte(), D :: byte()}) ->
    #dns_lb_advice{}.
initial_advice_for_dns(NodeIP) ->
    #dns_lb_advice{nodes_and_frequency = [{NodeIP, 1.0}], node_choices = [NodeIP, NodeIP]}.


%%--------------------------------------------------------------------
%% @doc
%% Returns an initial advice for dispatcher that can be used before CCM
%% starts broadcasting advices.
%% @end
%%--------------------------------------------------------------------
-spec initial_advice_for_dispatcher() -> #dispatcher_lb_advice{}.
initial_advice_for_dispatcher() ->
    #dispatcher_lb_advice{}.


%%--------------------------------------------------------------------
%% @doc
%% Returns an initial advice for dispatcher that can be used before CCM
%% starts broadcasting advices.
%% @end
%%--------------------------------------------------------------------
-spec all_nodes_for_dispatcher(Advice :: #dispatcher_lb_advice{}) -> [node()].
all_nodes_for_dispatcher(#dispatcher_lb_advice{all_nodes = AllNodes}) ->
    AllNodes.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper function that returns the index of node which should be chosen based on
%% frequency (weights) of nodes.
%% @end
%%--------------------------------------------------------------------
-spec choose_index(NodesAndFrequencies :: [{Node :: term(), Frequency :: float()}]) -> integer().
choose_index(NodesAndFrequencies) ->
    random:seed(now()),
    choose_index(NodesAndFrequencies, 0, random:uniform()).

choose_index([], CurrIndex, _) ->
    CurrIndex;

choose_index(_, CurrIndex, RandomFloat) when RandomFloat < 0.0 ->
    CurrIndex;

choose_index([{_, Freq} | T], CurrIndex, RandomFloat) ->
    choose_index(T, CurrIndex + 1, RandomFloat - Freq).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a single value representing node load.
%% This value is used for comparison in dispatcher load balancing.
%% @end
%%--------------------------------------------------------------------
-spec load_for_dispatcher(NodeState :: #node_state{}) -> float().
load_for_dispatcher(#node_state{cpu_usage = CPU, mem_usage = Mem}) ->
    (CPU * 3 + Mem) / 4.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a single value representing node load.
%% This value is used for comparison in DNS load balancing.
%% @end
%%--------------------------------------------------------------------
-spec load_for_dns(NodeState :: #node_state{}) -> float().
load_for_dns(#node_state{net_usage = NetUsage} = NodeState) ->
    (4.0 + load_for_dispatcher(NodeState) / 100) / 5 * NetUsage.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Decides if the node is overloaded from dispatcher point of view.
%% @end
%%--------------------------------------------------------------------
-spec overloaded_for_dispatcher(NodeState :: #node_state{}, MinLoadForDisp :: float()) -> boolean().
overloaded_for_dispatcher(LoadForDispatcher, MinLoadForDisp) ->
    LoadForDispatcher > 1.5 * MinLoadForDisp andalso LoadForDispatcher > 30.0.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Decides if the node is overloaded from DNS point of view.
%% @end
%%--------------------------------------------------------------------
-spec overloaded_for_dns(NodeState :: #node_state{}, MinLoadForDNS :: float()) -> boolean().
overloaded_for_dns(LoadForDNS, MinLoadForDNS) ->
    LoadForDNS > 1.5 * MinLoadForDNS.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calculates average from a list of numeric values.
%% @end
%%--------------------------------------------------------------------
-spec average(Values :: [integer() | float()]) -> float().
average(Values) ->
    Sum = lists:foldl(
        fun(Value, Acc) ->
            Acc + Value
        end, 0.0, Values),
    Sum / length(Values).