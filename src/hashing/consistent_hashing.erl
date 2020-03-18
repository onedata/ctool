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

-include("logging.hrl").

-record(ring, {
    type :: single_node | multi_node, % single_node type is used to optimize calls (no chash operations needed)
    chash :: chash:chash() | node(),
    failed_nodes = [] :: [node()],
    label_associated_nodes_count = 1 :: pos_integer(), % Number of nodes associated with each label
    all_nodes :: [node()] % cached list of nodes to return it faster (it is also possible to get all nodes from chash)
}).

-type ring() :: #ring{}.

%% API
-export([init/2, replicate_ring_to_nodes/1, cleanup/0, report_node_failure/1, report_node_recovery/1,
    set_label_associated_nodes_count/1, get_label_associated_nodes_count/0,
    get_node/1, get_full_node_info/1, get_all_nodes/0]).
%% Export for internal rpc usage
-export([set_ring/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialize ring. Requires nodes' list and information about number of nodes associated with each label.
%% @end
%%--------------------------------------------------------------------
-spec init([node()], pos_integer()) -> ok | {error, term()}.
init([Node] = Nodes, LabelAssociatedNodesCount) ->
    set_ring(#ring{type = single_node, chash = Node, 
        label_associated_nodes_count = LabelAssociatedNodesCount, all_nodes = Nodes}),
    ?MODULE:replicate_ring_to_nodes(Nodes);
init(Nodes, LabelAssociatedNodesCount) ->
    case get_ring() of
        undefined ->
            NodeCount = length(Nodes),
            [Node0 | _] = Nodes,
            InitialCHash = chash:fresh(NodeCount, Node0),
            CHash = lists:foldl(fun({I, Node}, CHashAcc) ->
                chash:update(get_nth_index(I, CHashAcc), Node, CHashAcc)
            end, InitialCHash, lists:zip(lists:seq(1, NodeCount), Nodes)),

            set_ring(#ring{type = multi_node, chash = CHash,
                label_associated_nodes_count = LabelAssociatedNodesCount, all_nodes = Nodes}),
            ?MODULE:replicate_ring_to_nodes(Nodes);
        _ ->
            ok
    end.


-spec cleanup() -> ok.
cleanup() ->
    ctool:unset_env(consistent_hashing_ring).


-spec replicate_ring_to_nodes([node()]) -> ok | {error, term()}.
replicate_ring_to_nodes(Nodes) ->
    {Ans, BadNodes} = FullAns = rpc:multicall(Nodes, ?MODULE, set_ring, [get_ring()]),
    Errors = lists:filter(fun(NodeAns) -> NodeAns =/= ok end, Ans),
    case {Errors, BadNodes} of
        {[], []} ->
            ok;
        _ ->
            ?error("Error error replicating ring to nodes ~p~nrpc ans: ~p", [Nodes, FullAns]),
            {error, FullAns}
    end.


-spec report_node_failure(node()) -> ok.
report_node_failure(FailedNode) ->
    #ring{failed_nodes = FailedNodes} = Ring = get_ring(),
    set_ring(Ring#ring{failed_nodes = lists:usort([FailedNode | FailedNodes])}).


-spec report_node_recovery(node()) -> ok.
report_node_recovery(RecoveredNode) ->
    #ring{failed_nodes = FailedNodes} = Ring = get_ring(),
    set_ring(Ring#ring{failed_nodes = FailedNodes -- [RecoveredNode]}).


-spec set_label_associated_nodes_count(pos_integer()) -> ok.
set_label_associated_nodes_count(Count) ->
    Ring = get_ring(),
    set_ring(Ring#ring{label_associated_nodes_count = Count}).


-spec get_label_associated_nodes_count() -> pos_integer().
get_label_associated_nodes_count() ->
    Ring = get_ring(),
    Ring#ring.label_associated_nodes_count.

%%--------------------------------------------------------------------
%% @doc
%% Get node that is responsible for the data labeled with given term.
%% Crashes with error if node is failed.
%% Function to be used if label is associated only with particular node (no HA available).
%% @end
%%--------------------------------------------------------------------
-spec get_node(term()) -> node().
get_node(Label) ->
    case get_ring() of
        undefined ->
            error(chash_ring_not_initialized);
        #ring{type = single_node, failed_nodes = [], chash = Node} ->
            Node;
        #ring{type = single_node, failed_nodes = FailedNodes, chash = Node} ->
            case lists:member(Node, FailedNodes) of
                true -> error(failed_node);
                false -> Node
            end;
        #ring{type = multi_node, failed_nodes = [], chash = CHash} ->
            Index = chash:key_of(Label),
            [{_, BestNode}] = chash:successors(Index, CHash, 1),
            BestNode;
        #ring{type = multi_node, chash = CHash, failed_nodes = FailedNodes} ->
            Index = chash:key_of(Label),
            [{_, BestNode}] = chash:successors(Index, CHash, 1),
            case lists:member(BestNode, FailedNodes) of
                true -> error(failed_node);
                false -> BestNode
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get full information about nodes, especially nodes that are responsible for the data labeled with given term.
%% Returns three lists: nodes connected with label, failed nodes and all nodes.
%% @end
%%--------------------------------------------------------------------
-spec get_full_node_info(term()) -> {LabelAssociatedNodes :: [node()], FailedNodes :: [node()], AllNodes :: [node()]}.
get_full_node_info(Label) ->
    case get_ring() of
        undefined ->
            error(chash_ring_not_initialized);
        #ring{type = single_node, chash = Node, failed_nodes = FailedNodes, all_nodes = AllNodes} ->
            {[Node], FailedNodes, AllNodes};
        #ring{chash = CHash, label_associated_nodes_count = NodeCount,
            failed_nodes = FailedNodes , all_nodes = AllNodes} ->
            Index = chash:key_of(Label),
            Nodes = lists:map(fun({_, Node}) -> Node end, chash:successors(Index, CHash, NodeCount)),
            {Nodes, FailedNodes, AllNodes}
    end.


-spec get_all_nodes() -> [node()].
get_all_nodes() ->
    case get_ring() of
        undefined ->
            error(chash_ring_not_initialized);
        #ring{all_nodes = AllNodes} ->
            AllNodes
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_ring() -> ring() | undefined.
get_ring() ->
    ctool:get_env(consistent_hashing_ring, undefined).

-spec set_ring(ring()) -> ok.
set_ring(Ring) ->
    ctool:set_env(consistent_hashing_ring, Ring).

-spec get_nth_index(non_neg_integer(), chash:chash()) -> non_neg_integer().
get_nth_index(N, CHash) ->
    {Index, _} = lists:nth(N, chash:nodes(CHash)),
    Index.
