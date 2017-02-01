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
-module(consistent_hasing).
-author("Tomasz Lichon").

-include("global_definitions.hrl").

%% API
-export([init/1, cleanup/0, get_chash_ring/0, set_chash_ring/1, get_node/1,
    get_all_nodes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialize chash ring
%% @end
%%--------------------------------------------------------------------
-spec init([node()]) -> ok.
init(Nodes) ->
    case application:get_env(?CLUSTER_MANAGER, chash) of
        undefined ->
            [Node0 | _] = Nodes,
            InitialCHash = chash:fresh(length(Nodes), Node0),
            CHash = lists:foldl(fun({I, Node}, CHashAcc) ->
                chash:update(get_nth_index(I, CHashAcc), Node, CHashAcc)
            end, InitialCHash, lists:zip(lists:seq(1, length(Nodes)), Nodes)),
            application:set_env(?CLUSTER_MANAGER, chash, CHash);
        {ok, _Val} ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Removes chash ring.
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok.
cleanup() ->
    application:unset_env(?CLUSTER_MANAGER, chash).

%%--------------------------------------------------------------------
%% @doc
%% Returns consistent hasing ring.
%% @end
%%--------------------------------------------------------------------
-spec get_chash_ring() -> chash:chash().
get_chash_ring() ->
    application:get_env(?CLUSTER_MANAGER, chash, undefined).

%%--------------------------------------------------------------------
%% @doc
%% Sets consistent hashing ring.
%% @end
%%--------------------------------------------------------------------
-spec set_chash_ring(chash:chash()) -> ok.
set_chash_ring(CHash) ->
    application:set_env(?CLUSTER_MANAGER, chash, CHash).

%%--------------------------------------------------------------------
%% @doc
%% Get node that is responsible for the data labeled with given term.
%% @end
%%--------------------------------------------------------------------
-spec get_node(term()) -> node().
get_node(Label) ->
    {ok, CHash} = application:get_env(?CLUSTER_MANAGER, chash),
    Index = chash:key_of(Label),
    [{_, BestNode}] = chash:successors(Index, CHash, 1),
    BestNode.

%%--------------------------------------------------------------------
%% @doc
%% Get all nodes in ring.
%% @end
%%--------------------------------------------------------------------
-spec get_all_nodes() -> [node()].
get_all_nodes() ->
    {ok, CHash} = application:get_env(?CLUSTER_MANAGER, chash),
    NodesWithIndices = chash:nodes(CHash),
    UniqueNodesWithIndices = lists:sublist(NodesWithIndices, length(NodesWithIndices)-1),
    [Node || {_, Node} <- UniqueNodesWithIndices].

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
