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
    label_associated_nodes_count = 1 :: pos_integer(), % Number of nodes associated with each label
    all_nodes :: [node()] % cached list of nodes to return it faster (it is also possible to get all nodes from chash)
}).

-type ring() :: #ring{}.
-type ring_name() :: atom().
-type routing_info() :: #node_routing_info{}.

%% Basic API
-export([init/2, cleanup/0, get_associated_node/1, get_routing_info/1, get_all_nodes/0]).
%% Changes of initialized ring
-export([report_node_failure/1, report_node_recovery/1,
    set_label_associated_nodes_count/1, get_label_associated_nodes_count/0]).
%% Cluster reconfiguration API
-export([init_cluster_reconfiguration/1, finish_cluster_reconfiguration/1, get_reconfiguration_nodes/0,
    get_prev_nodes/0, get_reconfigured_routing_info/1]).
%% Export for internal rpc usage
-export([set_ring/2, finish_cluster_reconfiguration/0]).
%% Export for tests
-export([replicate_ring_to_nodes/3]).

-define(RING_ENV, consistent_hashing_ring).
-define(RECONFIGURATION_RING_ENV, reconfiguration_consistent_hashing_ring).
-define(PREV_RING_ENV, prev_consistent_hashing_ring).

%%%===================================================================
%%% Basic API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialize ring. Requires nodes' list and information about number of nodes associated with each label.
%% @end
%%--------------------------------------------------------------------
-spec init([node()], pos_integer()) -> ok | no_return().
init(Nodes, LabelAssociatedNodesCount) ->
    case is_ring_initialized() of
        false ->
            Ring = init_ring(Nodes, LabelAssociatedNodesCount),
            set_ring(Ring),
            ok = ?MODULE:replicate_ring_to_nodes(Nodes, ?RING_ENV, Ring);
        _ ->
            ok
    end.


-spec cleanup() -> ok.
cleanup() ->
    ctool:unset_env(?RING_ENV).

%%--------------------------------------------------------------------
%% @doc
%% Get node that is responsible for the data labeled with given term.
%% Crashes with error if node is failed.
%% Function to be used if label is associated only with particular node (no HA available).
%% @end
%%--------------------------------------------------------------------
-spec get_associated_node(term()) -> node().
get_associated_node(Label) ->
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
        Error ->
            error(Error)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @equiv get_full_node_info(?RING_ENV, Label).
%% @end
%%--------------------------------------------------------------------
-spec get_routing_info(term()) -> routing_info().
get_routing_info(Label) ->
    get_routing_info(?RING_ENV, Label).


-spec get_all_nodes() -> [node()].
get_all_nodes() ->
    get_all_nodes(?RING_ENV).

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


-spec set_label_associated_nodes_count(pos_integer()) -> ok.
set_label_associated_nodes_count(Count) ->
    Ring = get_ring(),
    set_ring(Ring#ring{label_associated_nodes_count = Count}).


-spec get_label_associated_nodes_count() -> pos_integer().
get_label_associated_nodes_count() ->
    Ring = get_ring(),
    Ring#ring.label_associated_nodes_count.

%%%===================================================================
%%% Cluster reconfiguration API
%%%===================================================================

% TODO - do funkcji musza wchodzic posortowane node'y
-spec init_cluster_reconfiguration([node()]) -> ok | no_return().
init_cluster_reconfiguration(Nodes) ->
    CurrentRing = get_ring(),
    Ring = init_ring(Nodes, CurrentRing#ring.label_associated_nodes_count),
    set_ring(?RECONFIGURATION_RING_ENV, Ring),
    ok = ?MODULE:replicate_ring_to_nodes(Nodes, ?RECONFIGURATION_RING_ENV, Ring).

-spec finish_cluster_reconfiguration([node()]) -> ok | no_return().
finish_cluster_reconfiguration(Nodes) ->
    {Ans, BadNodes} = FullAns = rpc:multicall(Nodes, ?MODULE, finish_cluster_reconfiguration, []),
    Errors = lists:filter(fun(NodeAns) -> NodeAns =/= ok end, Ans),
    case {Errors, BadNodes} of
        {[], []} ->
            ok;
        _ ->
            ?error("Error finishing cluster reconfiguiration for nodes ~p~nrpc ans: ~p", [Nodes, FullAns]),
            throw({error, FullAns})
    end.

-spec get_reconfiguration_nodes() -> [node()].
get_reconfiguration_nodes() ->
    get_all_nodes(?RECONFIGURATION_RING_ENV).

-spec get_prev_nodes() -> [node()].
get_prev_nodes() ->
    get_all_nodes(?PREV_RING_ENV).

%%--------------------------------------------------------------------
%% @doc
%% @equiv get_routing_info(?RECONFIGURATION_RING_ENV, Label).
%% @end
%%--------------------------------------------------------------------
-spec get_reconfigured_routing_info(term()) -> routing_info().
get_reconfigured_routing_info(Label) ->
    get_routing_info(?RECONFIGURATION_RING_ENV, Label).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_ring() -> ring() | chash_ring_not_initialized.
get_ring() ->
    get_ring(?RING_ENV).

-spec get_ring(ring_name()) -> ring() | chash_ring_not_initialized.
get_ring(RingName) ->
    ctool:get_env(RingName, chash_ring_not_initialized).

-spec is_ring_initialized() -> boolean().
is_ring_initialized() ->
    get_ring() =/= chash_ring_not_initialized.

-spec set_ring(ring()) -> ok.
set_ring(Ring) ->
    set_ring(?RING_ENV, Ring).

-spec set_ring(ring_name(), ring()) -> ok.
set_ring(RingName, Ring) ->
    ctool:set_env(RingName, Ring).

-spec get_nth_index(non_neg_integer(), chash:chash()) -> non_neg_integer().
get_nth_index(N, CHash) ->
    {Index, _} = lists:nth(N, chash:nodes(CHash)),
    Index.

-spec init_ring([node()], pos_integer()) -> ring().
init_ring([_] = Nodes, LabelAssociatedNodesCount) ->
    #ring{label_associated_nodes_count = LabelAssociatedNodesCount, all_nodes = Nodes};
init_ring(Nodes, LabelAssociatedNodesCount) ->
    NodeCount = length(Nodes),
    [Node0 | _] = Nodes,
    InitialCHash = chash:fresh(NodeCount, Node0),
    CHash = lists:foldl(fun({I, Node}, CHashAcc) ->
        chash:update(get_nth_index(I, CHashAcc), Node, CHashAcc)
    end, InitialCHash, lists:zip(lists:seq(1, NodeCount), Nodes)),

    #ring{chash = CHash, label_associated_nodes_count = LabelAssociatedNodesCount, all_nodes = Nodes}.

-spec replicate_ring_to_nodes([node()], ring_name(), ring()) -> ok | {error, term()}.
replicate_ring_to_nodes(Nodes, RingName, Ring) ->
    {Ans, BadNodes} = FullAns = rpc:multicall(Nodes, ?MODULE, set_ring, [RingName, Ring]),
    Errors = lists:filter(fun(NodeAns) -> NodeAns =/= ok end, Ans),
    case {Errors, BadNodes} of
        {[], []} ->
            ok;
        _ ->
            ?error("Error replicating ring to nodes ~p~nrpc ans: ~p", [Nodes, FullAns]),
            {error, FullAns}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get full information about nodes, especially nodes that are responsible for the data labeled with given term.
%% @end
%%--------------------------------------------------------------------
-spec get_routing_info(ring_name(), term()) -> routing_info().
get_routing_info(RingName, Label) ->
    case get_ring(RingName) of
        #ring{all_nodes = [_] = AllNodes, failed_nodes = FailedNodes} ->
            #node_routing_info{label_associated_nodes = AllNodes, failed_nodes = FailedNodes, all_nodes = AllNodes};
        #ring{chash = CHash, label_associated_nodes_count = NodeCount,
            failed_nodes = FailedNodes , all_nodes = AllNodes} ->
            Index = chash:key_of(Label),
            Nodes = lists:map(fun({_, Node}) -> Node end, chash:successors(Index, CHash, NodeCount)),
            #node_routing_info{label_associated_nodes = Nodes, failed_nodes = FailedNodes, all_nodes = AllNodes};
        Error ->
            error(Error)
    end.

-spec get_all_nodes(ring_name()) -> [node()].
get_all_nodes(RingName) ->
    case get_ring(RingName) of
        #ring{all_nodes = AllNodes} ->
            AllNodes;
        Error ->
            error(Error)
    end.

-spec finish_cluster_reconfiguration() -> ok.
finish_cluster_reconfiguration() ->
    Ring = get_ring(),
    set_ring(?PREV_RING_ENV, Ring),
    NewRing = get_ring(?RECONFIGURATION_RING_ENV),
    set_ring(NewRing).