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
-export([init/1, cleanup/0, get_chash_ring/0, set_chash_ring/1,
    get_node/1, get_nodes/2, get_all_nodes/0,
    get_hashing_key/1, gen_hashing_key/0, get_random_label_part/1, has_hash_part/1, create_label/2]).

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
            Index = chash:key_of(get_hashing_key(Label)),
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
            Index = chash:key_of(get_hashing_key(Label)),
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
%% Get key used to choose node.
%% @end
%%--------------------------------------------------------------------
-spec get_hashing_key(term()) -> term().
get_hashing_key(Label) when is_binary(Label) ->
    case split_hash(Label) of
        {undefined, _} ->
            % Label does not contain hash part - generate key from label's random part
            Len = byte_size(Label),
            binary:part(Label, Len, -1 * min(?HASH_LENGTH, Len));
        {Hash, _} -> Hash
    end;
get_hashing_key(Label) ->
    Label.

%%--------------------------------------------------------------------
%% @doc
%% Get random part of label.
%% @end
%%--------------------------------------------------------------------
-spec get_random_label_part(term()) -> term().
get_random_label_part(Label) when is_binary(Label) ->
    {_, Random} = split_hash(Label),
    Random;
get_random_label_part(Label) ->
    Label.

%%--------------------------------------------------------------------
%% @doc
%% Generates random hashing key.
%% @end
%%--------------------------------------------------------------------
-spec gen_hashing_key() -> binary().
gen_hashing_key() ->
    str_utils:rand_hex(?HASH_LENGTH).

%%--------------------------------------------------------------------
%% @doc
%% Verifies whether Label has hash part (part used to choose node) defined.
%% @end
%%--------------------------------------------------------------------
-spec has_hash_part(term()) -> boolean().
has_hash_part(Label) when is_binary(Label) ->
    {Hash, _} = split_hash(Label),
    Hash =/= undefined;
has_hash_part(_Label) ->
    false.

%%--------------------------------------------------------------------
%% @doc
%% Creates label with part used to choose node.
%% @end
%%--------------------------------------------------------------------
-spec create_label(binary(), binary()) -> binary().
create_label(HashPart, Tail) ->
    <<?HASH_SEPARATOR, HashPart/binary, Tail/binary>>.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Gets part of the label used to choose node.
%% @end
%%--------------------------------------------------------------------
-spec split_hash(binary()) -> {binary() | undefined, binary()}.
split_hash(Label) ->
    case binary:split(Label, ?HASH_SEPARATOR_BIN) of
        [_, Hash | _] ->
            {binary:part(Hash, 0, ?HASH_LENGTH), binary:part(Hash, ?HASH_LENGTH, byte_size(Hash) - ?HASH_LENGTH)};
        _ ->
            {undefined, Label}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get hash index of nth node in ring.
%% @end
%%--------------------------------------------------------------------
-spec get_nth_index(non_neg_integer(), chash:chash()) -> non_neg_integer().
get_nth_index(N, CHash) ->
    {Index, _} = lists:nth(N, chash:nodes(CHash)),
    Index.
