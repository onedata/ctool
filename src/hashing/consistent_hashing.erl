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

-type chash() :: chash:chash() | node().

%% API
-export([init/1, cleanup/0, get_chash_ring/0, set_chash_ring/1, get_node/1,
    get_all_nodes/0, get_label_gen_size/0]).

-define(SINGLE_NODE_CHASH(Node), Node).
-define(IS_SINGLE_NODE_CHASH(CHash), is_atom(CHash)).

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
    set_chash_ring(?SINGLE_NODE_CHASH(Node));
init(Nodes) ->
    case get_chash_ring() of
        undefined ->
            [Node0 | _] = Nodes,
            InitialCHash = chash:fresh(length(Nodes), Node0),
            CHash = lists:foldl(fun({I, Node}, CHashAcc) ->
                chash:update(get_nth_index(I, CHashAcc), Node, CHashAcc)
            end, InitialCHash, lists:zip(lists:seq(1, length(Nodes)), Nodes)),
            set_chash_ring(CHash);
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
-spec get_chash_ring() -> chash() | undefined.
get_chash_ring() ->
    ctool:get_env(chash, undefined).

%%--------------------------------------------------------------------
%% @doc
%% Sets consistent hashing ring.
%% @end
%%--------------------------------------------------------------------
-spec set_chash_ring(chash()) -> ok.
set_chash_ring(CHash) ->
    ctool:set_env(chash, CHash).

%%--------------------------------------------------------------------
%% @doc
%% Get node that is responsible for the data labeled with given term.
%% @end
%%--------------------------------------------------------------------
-spec get_node(term()) -> node().
get_node(Label) ->
    case get_chash_ring() of
        undefined ->
            error(chash_ring_not_initialized);
        Node when ?IS_SINGLE_NODE_CHASH(Node) ->
            Node;
        CHash when is_binary(Label) ->
            Length = min(byte_size(Label), get_label_gen_size()),
            Index = chash:key_of(binary:part(Label, 0, Length)),
            [{_, BestNode}] = chash:successors(Index, CHash, 1),
            BestNode;
        CHash ->
            Index = chash:key_of(Label),
            [{_, BestNode}] = chash:successors(Index, CHash, 1),
            BestNode
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
        Node when ?IS_SINGLE_NODE_CHASH(Node) ->
            [Node];
        CHash ->
            NodesWithIndices = chash:nodes(CHash),
            {_, Nodes} = lists:unzip(NodesWithIndices),
            lists:usort(Nodes)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get number of bytes used to generate Index for binary labels.
%% @end
%%--------------------------------------------------------------------
-spec get_label_gen_size() -> non_neg_integer().
get_label_gen_size() ->
    6.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get hash index of nth node in ring.
%% @end
%%--------------------------------------------------------------------
-spec get_nth_index(non_neg_integer(), chash:chash()) -> non_neg_integer().
get_nth_index(N, CHash) ->
    {Index, _} = lists:nth(N, chash:nodes(CHash)),
    Index.
