%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Auxiliary functions for managing consistent hashing ring and mapping
%%% terms (usually UUIDs) to nodes in the ring.
%%% Most functions use record ring (stored in environment variable) that represents information about cluster.
%%% It is assumed that particular number of nodes is associated with each label.
%%% Ring record is an internal structure of the module - it can only be copied as indivisible whole between nodes.
%%% @end
%%%--------------------------------------------------------------------
-module(consistent_hashing).
-author("Tomasz Lichon").

-include("hashing/consistent_hashing.hrl").
-include("logging.hrl").

-record(ring, {
    chash :: chash:chash() | undefined,
    failed_nodes = [] :: [node()],
    nodes_assigned_per_label = 1 :: pos_integer(), % Number of nodes associated with each label
    all_nodes :: [node()] % cached list of nodes to return it faster (it is also possible to get all nodes from chash)
}).

-type ring() :: #ring{}.
-type routing_info() :: #node_routing_info{}.

%% Basic API
-export([init/2, cleanup/0, get_assigned_node/1, get_routing_info/1, get_all_nodes/0]).
%% Changes of initialized ring
-export([report_node_failure/1, report_node_recovery/1,
    set_nodes_assigned_per_label/1, get_nodes_assigned_per_label/0]).
%% Export for internal rpc usage
-export([set_ring/1]).

%%%===================================================================
%%% Basic API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialize ring. Requires nodes' list and information about number of nodes associated with each label.
%% @end
%%--------------------------------------------------------------------
-spec init([node()], pos_integer()) -> ok | {error, term()}.
init(Nodes, LabelAssociatedNodesCount) ->
    case is_ring_initialized() of
        false ->
            Ring = init_ring(Nodes, LabelAssociatedNodesCount),
            set_ring(Ring),
            replicate_ring_to_nodes(Nodes);
        _ ->
            ok
    end.


-spec cleanup() -> ok.
cleanup() ->
    ctool:unset_env(consistent_hashing_ring).

%%--------------------------------------------------------------------
%% @doc
%% Get node that is responsible for the data labeled with given term.
%% Crashes with error if node is failed.
%% Function to be used if label is associated only with particular node (no HA available).
%% @end
%%--------------------------------------------------------------------
-spec get_assigned_node(term()) -> node().
get_assigned_node(Label) ->
    case get_ring() of
        #ring{all_nodes = [Node], failed_nodes = []} ->
            Node;
        #ring{all_nodes = [Node], failed_nodes = FailedNodes} ->
            case lists:member(Node, FailedNodes) of
                true -> error({failed_node, Node});
                false -> Node
            end;
        #ring{failed_nodes = [], chash = CHash} ->
            Index = chash:key_of(Label),
            [{_, BestNode}] = chash:successors(Index, CHash, 1),
            BestNode;
        #ring{chash = CHash, failed_nodes = FailedNodes} ->
            Index = chash:key_of(Label),
            [{_, BestNode}] = chash:successors(Index, CHash, 1),
            case lists:member(BestNode, FailedNodes) of
                true -> error({failed_node, BestNode});
                false -> BestNode
            end;
        {error, Error} ->
            error(Error)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get full information about nodes, especially nodes that are responsible for the data labeled with given term.
%% @end
%%--------------------------------------------------------------------
-spec get_routing_info(term()) -> routing_info().
get_routing_info(Label) ->
    case get_ring() of
        #ring{all_nodes = [_] = AllNodes, failed_nodes = FailedNodes} ->
            #node_routing_info{assigned_nodes = AllNodes, failed_nodes = FailedNodes, all_nodes = AllNodes};
        #ring{chash = CHash, nodes_assigned_per_label = NodeCount,
            failed_nodes = FailedNodes , all_nodes = AllNodes} ->
            Index = chash:key_of(Label),
            Nodes = lists:map(fun({_, Node}) -> Node end, chash:successors(Index, CHash, NodeCount)),
            #node_routing_info{assigned_nodes = Nodes, failed_nodes = FailedNodes, all_nodes = AllNodes};
        {error, Error} ->
            error(Error)
    end.


-spec get_all_nodes() -> [node()].
get_all_nodes() ->
    case get_ring() of
        #ring{all_nodes = AllNodes} ->
            AllNodes;
        {error, Error} ->
            error(Error)
    end.

%%%===================================================================
%%% Changes of initialized ring
%%%===================================================================

-spec report_node_failure(node()) -> ok.
report_node_failure(FailedNode) ->
    #ring{failed_nodes = FailedNodes} = Ring = get_ring(),
    set_ring(Ring#ring{failed_nodes = lists:usort([FailedNode | FailedNodes])}).


-spec report_node_recovery(node()) -> ok.
report_node_recovery(RecoveredNode) ->
    #ring{failed_nodes = FailedNodes} = Ring = get_ring(),
    set_ring(Ring#ring{failed_nodes = FailedNodes -- [RecoveredNode]}).


-spec set_nodes_assigned_per_label(pos_integer()) -> ok.
set_nodes_assigned_per_label(Count) ->
    Ring = get_ring(),
    set_ring(Ring#ring{nodes_assigned_per_label = Count}).


-spec get_nodes_assigned_per_label() -> pos_integer().
get_nodes_assigned_per_label() ->
    Ring = get_ring(),
    Ring#ring.nodes_assigned_per_label.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_ring() -> ring() | {error, chash_ring_not_initialized}.
get_ring() ->
    ctool:get_env(consistent_hashing_ring, {error, chash_ring_not_initialized}).

-spec is_ring_initialized() -> boolean().
is_ring_initialized() ->
    get_ring() =/= {error, chash_ring_not_initialized}.

-spec set_ring(ring()) -> ok.
set_ring(Ring) ->
    ctool:set_env(consistent_hashing_ring, Ring).

-spec get_nth_index(non_neg_integer(), chash:chash()) -> non_neg_integer().
get_nth_index(N, CHash) ->
    {Index, _} = lists:nth(N, chash:nodes(CHash)),
    Index.

-spec init_ring([node()], pos_integer()) -> ring().
init_ring([_] = Nodes, LabelAssociatedNodesCount) ->
    #ring{nodes_assigned_per_label = LabelAssociatedNodesCount, all_nodes = Nodes};
init_ring(Nodes, LabelAssociatedNodesCount) ->
    NodeCount = length(Nodes),
    [Node0 | _] = Nodes,
    InitialCHash = chash:fresh(NodeCount, Node0),
    CHash = lists:foldl(fun({I, Node}, CHashAcc) ->
        chash:update(get_nth_index(I, CHashAcc), Node, CHashAcc)
    end, InitialCHash, lists:zip(lists:seq(1, NodeCount), Nodes)),

    #ring{chash = CHash, nodes_assigned_per_label = LabelAssociatedNodesCount, all_nodes = Nodes}.

-spec replicate_ring_to_nodes([node()]) -> ok | {error, term()}.
replicate_ring_to_nodes(Nodes) ->
    {Ans, BadNodes} = FullAns = rpc:multicall(Nodes, ?MODULE, set_ring, [get_ring()]),
    Errors = lists:filter(fun(NodeAns) -> NodeAns =/= ok end, Ans),
    case {Errors, BadNodes} of
        {[], []} ->
            ok;
        _ ->
            ?error("Error replicating ring to nodes ~p~nrpc ans: ~p", [Nodes, FullAns]),
            {error, FullAns}
    end.