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
%%% Base for most functions is record ring that represents information about cluster.
%%% It is assumed that particular number of nodes is associated with each key/label
%%% but it is possible to request more.
%%% @end
%%%--------------------------------------------------------------------
-module(consistent_hashing).
-author("Tomasz Lichon").

-record(ring, {
    type :: single_node | multi_node | broken,
    chash :: chash:chash() | node(),
    broken_nodes = [] :: [node()],
    key_connected_nodes = 1 :: pos_integer(), % Number of nodes associated with each key
    nodes_num :: non_neg_integer()
}).

-type ring() :: #ring{}.

%% API
-export([init/1, cleanup/0, get_chash_ring/0, set_chash_ring/1,
    set_broken_node/1, set_fixed_node/1,
    set_key_connected_nodes/1, get_key_connected_nodes/0,
    get_node/1, get_nodes/2, get_all_nodes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialize chash ring
%% @end
%%------------------------------------------`--------------------------
-spec init([node()]) -> ok.
init([Node]) ->
    set_chash_ring(#ring{type = single_node, chash = Node, nodes_num = 1});
init(Nodes) ->
    case get_chash_ring() of
        undefined ->
            NodesNum = length(Nodes),
            [Node0 | _] = Nodes,
            InitialCHash = chash:fresh(NodesNum, Node0),
            CHash = lists:foldl(fun({I, Node}, CHashAcc) ->
                chash:update(get_nth_index(I, CHashAcc), Node, CHashAcc)
            end, InitialCHash, lists:zip(lists:seq(1, length(Nodes)), Nodes)),
            KeyConnectedNodes = ctool:get_env(key_connected_nodes, 1),
            Ring = #ring{type = multi_node, chash = CHash,
                key_connected_nodes = KeyConnectedNodes, nodes_num = NodesNum},
            set_chash_ring(Ring);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Removes chash ring.
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok.
cleanup() ->
    ctool:unset_env(chash).

%%--------------------------------------------------------------------
%% @doc
%% Returns consistent hashing ring.
%% @end
%%--------------------------------------------------------------------
-spec get_chash_ring() -> ring() | undefined.
get_chash_ring() ->
    ctool:get_env(chash, undefined).

%%--------------------------------------------------------------------
%% @doc
%% Sets consistent hashing ring.
%% @end
%%--------------------------------------------------------------------
-spec set_chash_ring(ring()) -> ok.
set_chash_ring(CHash) ->
    ctool:set_env(chash, CHash).

%%--------------------------------------------------------------------
%% @doc
%% Sets information about broken node.
%% @end
%%--------------------------------------------------------------------
-spec set_broken_node(node()) -> ok.
set_broken_node(BrokenNode) ->
    #ring{broken_nodes = BrokenNodes} = Ring = get_chash_ring(),
    Ring2 = Ring#ring{type = broken,
        broken_nodes = lists:usort([BrokenNode | BrokenNodes -- [BrokenNode]])},
    ctool:set_env(chash, Ring2).

%%--------------------------------------------------------------------
%% @doc
%% Sets information about fixed node.
%% @end
%%--------------------------------------------------------------------
-spec set_fixed_node(node()) -> ok.
set_fixed_node(FixedNode) ->
    #ring{broken_nodes = BrokenNodes} = Ring = get_chash_ring(),
    Ring2 = case BrokenNodes -- [FixedNode] of
        [] -> Ring#ring{type = multi_node, broken_nodes = []};
        BrokenNodes2 -> Ring#ring{broken_nodes = BrokenNodes2}
    end,
    ctool:set_env(chash, Ring2).

%%--------------------------------------------------------------------
%% @doc
%% Sets information about number of nodes connected with key.
%% @end
%%--------------------------------------------------------------------
-spec set_key_connected_nodes(pos_integer()) -> ok.
set_key_connected_nodes(Num) ->
    ctool:set_env(key_connected_nodes, Num),
    Ring = get_chash_ring(),
    set_chash_ring(Ring#ring{key_connected_nodes = Num}).

%%--------------------------------------------------------------------
%% @doc
%% Gets information about number of nodes connected with key.
%% @end
%%--------------------------------------------------------------------
-spec get_key_connected_nodes() -> pos_integer().
get_key_connected_nodes() ->
    ctool:get_env(key_connected_nodes, 1).

%%--------------------------------------------------------------------
%% @doc
%% Get node that is responsible for the data labeled with given term.
%% Throws error if node is broken.
%% @end
%%--------------------------------------------------------------------
-spec get_node(term()) -> node().
get_node(Label) ->
    case get_chash_ring() of
        undefined ->
            error(chash_ring_not_initialized);
        #ring{type = single_node, chash = Node} ->
            Node;
        #ring{type = multi_node, chash = CHash} ->
            Index = chash:key_of(Label),
            [{_, BestNode}] = chash:successors(Index, CHash, 1),
            BestNode;
        #ring{type = broken, chash = CHash, broken_nodes = BrokenNodes} ->
            Index = chash:key_of(Label),
            [{_, BestNode}] = chash:successors(Index, CHash, 1),
            case lists:member(BestNode, BrokenNodes) of
                true -> throw(broken_node);
                false -> BestNode
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get nodes that are responsible for the data labeled with given term.
%% Returns three lists: alvie connected nodes, other alive nodes (if requested more that are connected with label
%% by default) and broken nodes. Sum of lists' length is equal to requested nodes' count.
%% @end
%%--------------------------------------------------------------------
-spec get_nodes(term(), non_neg_integer() | all) ->
    {KeyConnectedNodes :: [node()], OtherRequestedNodes :: [node()], BrokenNodes :: [node()], BrokenMaster :: boolean()}.
get_nodes(Label, RequestedNodesNum0) ->
    case get_chash_ring() of
        undefined ->
            error(chash_ring_not_initialized);
        #ring{type = single_node, chash = Node} ->
            {[Node], [], [], false};
        #ring{type = multi_node, chash = CHash, key_connected_nodes = KeyConnectedNodesNum} = Ring ->
            Index = chash:key_of(Label),
            RequestedNodesNum = get_requested_nodes_num(RequestedNodesNum0, Ring),
            Nodes = lists:map(fun({_, Node}) -> Node end,
                    chash:successors(Index, CHash, max(KeyConnectedNodesNum, RequestedNodesNum))),
            {KeyConnectedNodes, OtherRequestedNodes} = lists:split(min(KeyConnectedNodesNum, length(Nodes)), Nodes),
            {KeyConnectedNodes, OtherRequestedNodes, [], false};
        #ring{type = broken, chash = CHash, broken_nodes = BrokenNodes,
            key_connected_nodes = KeyConnectedNodesNum}  = Ring ->
            Index = chash:key_of(Label),
            RequestedNodesNum = get_requested_nodes_num(RequestedNodesNum0, Ring),
            [{_, Node1} | _] = Nodes = chash:successors(Index, CHash, max(KeyConnectedNodesNum, RequestedNodesNum)),
            {KeyConnectedNodes, OtherRequestedNodes} = lists:split(min(KeyConnectedNodesNum, length(Nodes)), Nodes),
            {AliveKeyConnectedNodes, BrokenKeyConnectedNodes} = filter_nodes(KeyConnectedNodes, BrokenNodes),
            {AliveOtherRequestedNodes, BrokenOtherRequestedNodes} = filter_nodes(OtherRequestedNodes, BrokenNodes),
            {AliveKeyConnectedNodes, AliveOtherRequestedNodes, BrokenKeyConnectedNodes ++ BrokenOtherRequestedNodes,
                lists:member(Node1, BrokenNodes)}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get all nodes in ring.
%% @end
%%--------------------------------------------------------------------
-spec get_all_nodes() -> [node()].
get_all_nodes() ->
    case get_chash_ring() of
        undefined ->
            error(chash_ring_not_initialized);
        #ring{type = single_node, chash = Node} ->
            [Node];
        #ring{chash = CHash} ->
            NodesWithIndices = chash:nodes(CHash),
            {_, Nodes} = lists:unzip(NodesWithIndices),
            lists:usort(Nodes)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Get hash index of nth node in ring.
%% @end
%%--------------------------------------------------------------------
-spec get_nth_index(non_neg_integer(), chash:chash()) -> non_neg_integer().
get_nth_index(N, CHash) ->
    {Index, _} = lists:nth(N, chash:nodes(CHash)),
    Index.

-spec filter_nodes(ResponsibleNodes :: [{integer(), node()}], AllBrokenNodes :: [node()]) ->
    {GoodNodes :: [node()], BrokenNodes :: [node()]}.
filter_nodes([], _BrokenNodes) ->
    {[], []};
filter_nodes([{_, Node} | Nodes], BrokenNodes) ->
    {AliveNodesAns, BrokenNodesAns} = filter_nodes(Nodes, BrokenNodes),
    case lists:member(Node, BrokenNodes) of
        true ->
            {AliveNodesAns, [Node | BrokenNodesAns]};
        _ ->
            {[Node | AliveNodesAns], BrokenNodesAns}
    end.

-spec get_requested_nodes_num(non_neg_integer() | all, ring()) -> non_neg_integer().
get_requested_nodes_num(all, #ring{nodes_num = NodesNum}) ->
    NodesNum;
get_requested_nodes_num(Requested, _) ->
    Requested.