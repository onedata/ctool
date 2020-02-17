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
%%% @end
%%%--------------------------------------------------------------------
-module(consistent_hashing).
-author("Tomasz Lichon").

-record(ring, {
    type :: single_node | multi_node | broken,
    chash :: chash:chash() | node(),
    broken_nodes = [] :: [node()]
}).

-type ring() :: #ring{}.

%% API
-export([init/1, cleanup/0, get_chash_ring/0, set_chash_ring/1,
    set_broken_node/1, set_fixed_node/1,
    get_node/1, get_nodes/2, get_alive_and_broken_nodes/2, get_all_nodes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialize chash ring
%% @end
%%--------------------------------------------------------------------
-spec init([node()]) -> ok.
init([Node]) ->
    set_chash_ring(#ring{type = single_node, chash = Node});
init(Nodes) ->
    case get_chash_ring() of
        undefined ->
            [Node0 | _] = Nodes,
            InitialCHash = chash:fresh(length(Nodes), Node0),
            CHash = lists:foldl(fun({I, Node}, CHashAcc) ->
                chash:update(get_nth_index(I, CHashAcc), Node, CHashAcc)
            end, InitialCHash, lists:zip(lists:seq(1, length(Nodes)), Nodes)),
            Ring = #ring{type = multi_node, chash = CHash},
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
set_fixed_node(FixedNodes) ->
    #ring{broken_nodes = BrokenNodes} = Ring = get_chash_ring(),
    Ring2 = case BrokenNodes -- [FixedNodes] of
        [] -> Ring#ring{type = multi_node, broken_nodes = []};
        BrokenNodes2 -> Ring#ring{broken_nodes = BrokenNodes2}
    end,
    ctool:set_env(chash, Ring2).

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
%% @end
%%--------------------------------------------------------------------
-spec get_nodes(term(), non_neg_integer()) -> [node()].
get_nodes(Label, NodesNum) ->
    case get_chash_ring() of
        undefined ->
            error(chash_ring_not_initialized);
        #ring{type = single_node, chash = Node} ->
            [Node];
        #ring{type = multi_node, chash = CHash} ->
            Index = chash:key_of(Label),
            lists:map(fun({_, Node}) -> Node end, chash:successors(Index, CHash, NodesNum));
        #ring{type = broken, chash = CHash, broken_nodes = BrokenNodes} ->
            Index = chash:key_of(Label),
            lists:map(fun({_, Node}) ->
                case lists:member(Node, BrokenNodes) of
                    true -> throw(broken_node);
                    false -> Node
                end
            end, chash:successors(Index, CHash, NodesNum))
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get nodes that are responsible for the data labeled with given term.
%% Returns two lists: alvie and broken nodes.
%% @end
%%--------------------------------------------------------------------
-spec get_alive_and_broken_nodes(term(), non_neg_integer()) -> {AliveNodes :: [node()], BrokenNodes :: [node()]}.
get_alive_and_broken_nodes(Label, NodesNum) ->
    case get_chash_ring() of
        undefined ->
            error(chash_ring_not_initialized);
        #ring{type = single_node, chash = Node} ->
            {[Node], []};
        #ring{type = multi_node, chash = CHash} ->
            Index = chash:key_of(Label),
            Nodes = lists:map(fun({_, Node}) -> Node end, chash:successors(Index, CHash, NodesNum)),
            {Nodes, []};
        #ring{type = broken, chash = CHash, broken_nodes = BrokenNodes} ->
            Index = chash:key_of(Label),
            Nodes = chash:successors(Index, CHash, NodesNum),
            filter_nodes(Nodes, BrokenNodes)
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

-spec filter_nodes(ResponsibleNodes :: [node()], AllBrokenNodes :: [node()]) ->
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