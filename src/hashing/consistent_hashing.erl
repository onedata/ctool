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
%%% It is assumed that particular number of nodes is assigned to each label.
%%% Ring record is an internal structure of the module - it can only be copied as indivisible whole between nodes.
%%% 3 generations of ring can be used: current, future and previous. The generations are associated with cluster
%%% resizing. Normally, current ring is used. When number of nodes is changing, future ring is first initialized
%%% showing ring structure after resize. When resize is ready, future ring becomes current ring and current ring
%%% is persisted as previous ring to deliver information about past ring structure if needed.
%%% @end
%%%--------------------------------------------------------------------
-module(consistent_hashing).
-author("Tomasz Lichon").

-include("hashing/consistent_hashing.hrl").
-include("logging.hrl").

-record(ring, {
    chash :: chash:chash() | undefined,
    failed_nodes = [] :: [node()],
    nodes_assigned_per_label = 1 :: pos_integer(), % Number of nodes assigned to each label
    all_nodes :: [node()] % cached list of nodes to return it faster (it is also possible to get all nodes from chash)
}).

-type ring() :: #ring{}.
-type ring_generation() :: ?CURRENT_RING | ?FUTURE_RING | ?PREVIOUS_RING.
-type routing_info() :: #node_routing_info{}.

-export_type([ring_generation/0]).

%% Basic API
-export([init/2, cleanup/0, get_assigned_node/1, get_routing_info/1, get_routing_info/2,
    get_all_nodes/0, get_all_nodes/1]).
%% Changes of initialized ring
-export([report_node_failure/1, report_node_recovery/1,
    set_nodes_assigned_per_label/1, get_nodes_assigned_per_label/0]).
%% Cluster resizing API
-export([init_cluster_resizing/1, finalize_cluster_resizing/0]).
%% Export for internal rpc usage
-export([set_ring/2, finalize_cluster_resizing_internal/0]).
%% Export for tests
-export([init_ring/2, replicate_ring_to_nodes/3]).

-define(INIT_ERROR, {error, chash_ring_not_initialized}).

%%%===================================================================
%%% Basic API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialize ring. Requires nodes' list and information about number of nodes assigned to each label.
%% @end
%%--------------------------------------------------------------------
-spec init([node()], pos_integer()) -> ok | no_return().
init(Nodes, NodesAssignedPerLabel) ->
    case is_ring_initialized() of
        false ->
            Ring = init_ring(Nodes, NodesAssignedPerLabel),
            ok = replicate_ring_to_nodes(Nodes, ?CURRENT_RING, Ring);
        _ ->
            ok
    end.

-spec cleanup() -> ok.
cleanup() ->
    unset_ring(?CURRENT_RING),
    unset_ring(?FUTURE_RING),
    unset_ring(?PREVIOUS_RING).

%%--------------------------------------------------------------------
%% @doc
%% Get node that is responsible for the data labeled with given term.
%% Crashes with error if node is failed.
%% Function to be used if label is assigned only to particular node (no HA available).
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
%% @equiv get_routing_info(?CURRENT_RING, Label).
%% @end
%%--------------------------------------------------------------------
-spec get_routing_info(term()) -> routing_info().
get_routing_info(Label) ->
    get_routing_info(?CURRENT_RING, Label).

%%--------------------------------------------------------------------
%% @doc
%% Get full information about nodes, especially nodes
%% that are responsible for the data labeled with given term.
%% @end
%%--------------------------------------------------------------------
-spec get_routing_info(ring_generation(), term()) -> routing_info().
get_routing_info(RingGeneration, Label) ->
    case get_ring(RingGeneration) of
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
    get_all_nodes(?CURRENT_RING).

-spec get_all_nodes(ring_generation()) -> [node()].
get_all_nodes(RingGeneration) ->
    case get_ring(RingGeneration) of
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
%%% Cluster resizing API
%%%===================================================================

-spec init_cluster_resizing([node()]) -> ok | no_return().
init_cluster_resizing(AllNodes) ->
    CurrentRing = get_ring(),
    Ring = init_ring(AllNodes, CurrentRing#ring.nodes_assigned_per_label),
    % Replicate future ring to all nodes. Use nodes from argument and current ring as they can differ.
    NodesToSync = lists:usort(AllNodes ++ get_all_nodes()),
    ok = replicate_ring_to_nodes(NodesToSync, ?FUTURE_RING, Ring).

-spec finalize_cluster_resizing() -> ok | no_return().
finalize_cluster_resizing() ->
    % Finish on all nodes. Use nodes from current and future rings as they can differ.
    Nodes = lists:usort(get_all_nodes() ++ get_all_nodes(?FUTURE_RING)),
    {Ans, BadNodes} = FullAns = rpc:multicall(Nodes, ?MODULE, finalize_cluster_resizing_internal, []),
    Errors = lists:filter(fun(NodeAns) -> NodeAns =/= ok end, Ans),
    case {Errors, BadNodes} of
        {[], []} ->
            ok;
        _ ->
            ?error("Error finishing cluster resizing for nodes ~p~nrpc ans: ~p", [Nodes, FullAns]),
            throw({error, FullAns})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_nth_index(non_neg_integer(), chash:chash()) -> non_neg_integer().
get_nth_index(N, CHash) ->
    {Index, _} = lists:nth(N, chash:nodes(CHash)),
    Index.

-spec init_ring([node()], pos_integer()) -> ring().
init_ring([_] = Nodes, NodesAssignedPerLabel) ->
    #ring{nodes_assigned_per_label = NodesAssignedPerLabel, all_nodes = Nodes};
init_ring(Nodes, NodesAssignedPerLabel) ->
    SortedUniqueNodes = lists:usort(Nodes),
    NodeCount = length(SortedUniqueNodes),
    [Node0 | _] = SortedUniqueNodes,
    InitialCHash = chash:fresh(NodeCount, Node0),
    CHash = lists:foldl(fun({I, Node}, CHashAcc) ->
        chash:update(get_nth_index(I, CHashAcc), Node, CHashAcc)
    end, InitialCHash, lists:zip(lists:seq(1, NodeCount), SortedUniqueNodes)),

    #ring{chash = CHash, nodes_assigned_per_label = NodesAssignedPerLabel, all_nodes = Nodes}.

-spec replicate_ring_to_nodes([node()], ring_generation(), ring()) -> ok | {error, term()}.
replicate_ring_to_nodes(Nodes, RingGeneration, Ring) ->
    {Ans, BadNodes} = FullAns = rpc:multicall(Nodes, ?MODULE, set_ring, [RingGeneration, Ring]),
    Errors = lists:filter(fun(NodeAns) -> NodeAns =/= ok end, Ans),
    case {Errors, BadNodes} of
        {[], []} ->
            ok;
        _ ->
            ?error("Error replicating ring to nodes ~p~nrpc ans: ~p", [Nodes, FullAns]),
            {error, FullAns}
    end.

-spec finalize_cluster_resizing_internal() -> ok.
finalize_cluster_resizing_internal() ->
    set_ring(?PREVIOUS_RING, get_ring()),
    set_ring(get_ring(?FUTURE_RING)),
    unset_ring(?FUTURE_RING).

%%%===================================================================
%%% Getters/setters
%%%===================================================================

-spec get_ring() -> ring() | ?INIT_ERROR.
get_ring() ->
    get_ring(?CURRENT_RING).

-spec get_ring(ring_generation()) -> ring() | ?INIT_ERROR.
get_ring(RingGeneration) ->
    ctool:get_env(RingGeneration, ?INIT_ERROR).

-spec is_ring_initialized() -> boolean().
is_ring_initialized() ->
    get_ring() =/= ?INIT_ERROR.

-spec set_ring(ring()) -> ok.
set_ring(Ring) ->
    set_ring(?CURRENT_RING, Ring).

-spec set_ring(ring_generation(), ring()) -> ok.
set_ring(RingGeneration, Ring) ->
    ctool:set_env(RingGeneration, Ring).

-spec unset_ring(ring_generation()) -> ok.
unset_ring(RingGeneration) ->
    ctool:unset_env(RingGeneration).