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
    get_nodes/2, get_all_nodes/0,
    create_label/2, get_label_hash_part/1, has_hash_part/1, get_hash_part_length/0]).

-define(SINGLE_NODE_CHASH(Node), Node).
-define(IS_SINGLE_NODE_CHASH(CHash), is_atom(CHash)).
-define(HASH_SEPARATOR, "!@hsh_").
-define(HASH_SEPARATOR_BIN, <<?HASH_SEPARATOR>>).
-define(HASH_LENGTH, 4).

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
        CHash ->
            Index = chash:key_of(get_key(Label)),
            [{_, BestNode}] = chash:successors(Index, CHash, 1),
            BestNode
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
        Node when ?IS_SINGLE_NODE_CHASH(Node) ->
            [Node];
        CHash ->
            Index = chash:key_of(get_key(Label)),
            lists:map(fun({_, Node}) -> Node end, chash:successors(Index, CHash, NodesNum))
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
%% Creates label with part used to choose node.
%% @end
%%--------------------------------------------------------------------
-spec create_label(binary(), binary()) -> binary().
create_label(HashPart, Tail) ->
    <<?HASH_SEPARATOR, HashPart/binary, Tail/binary>>.

%%--------------------------------------------------------------------
%% @doc
%% Gets part of the label used to choose node.
%% @end
%%--------------------------------------------------------------------
-spec get_label_hash_part(binary()) -> binary() | undefined.
get_label_hash_part(Label) ->
    case binary:split(Label, ?HASH_SEPARATOR_BIN) of
        [_, Hash | _] ->
            binary:part(Hash, 0, ?HASH_LENGTH);
        _ ->
            undefined
    end.

%%--------------------------------------------------------------------
%% @doc
%% Verifies whether Label has hash part (part used to choose node) defined.
%% @end
%%--------------------------------------------------------------------
-spec has_hash_part(term()) -> boolean().
has_hash_part(Label) when is_binary(Label) ->
    get_label_hash_part(Label) =/= undefined;
has_hash_part(_Label) ->
    false.

%%--------------------------------------------------------------------
%% @doc
%% Get number of bytes used to generate Index for binary labels.
%% @end
%%--------------------------------------------------------------------
-spec get_hash_part_length() -> non_neg_integer().
get_hash_part_length() ->
    ?HASH_LENGTH.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get key used to choose node.
%% @end
%%--------------------------------------------------------------------
-spec get_key(term()) -> term().
get_key(Label) when is_binary(Label) ->
    case get_label_hash_part(Label) of
        undefined -> Label;
        Hash -> Hash
    end;
get_key(Label) ->
    Label.

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
