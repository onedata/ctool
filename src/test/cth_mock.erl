%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(cth_mock).
-author("Jakub Kudzia").

%% API
%% CTH callback
%% initialization
-export([init/2]).
%% posthooks
-export([post_init_per_suite/4, post_end_per_suite/4]).

-include("test/test_utils.hrl").


%%--------------------------------------------------------------------
%% @doc
%% CTH callback called when hook is being installed.
%% Initializes logger state.
%% @end
%%--------------------------------------------------------------------
-spec init(_Id :: term(), _Opts :: term()) -> {ok, [], non_neg_integer()}.
init(_Id, _Opts) ->
    {ok, [], ?CTH_MOCK_PRIORITY}.

%%--------------------------------------------------------------------
%% @doc
%% CTH callback called after init_per_suite.
%% Starts mock manager
%% @end
%%--------------------------------------------------------------------
-spec post_init_per_suite(Suite :: atom(), _Config :: [term()], Return :: [term()],
    State :: []) -> {[term()], []}.
post_init_per_suite(_Suite, _Config, Return, State) ->
    lists:foreach(fun(NodeType) ->
        lists:foreach(fun(N) ->
            {ok, _} = rpc:call(N, mock_manager, start_link, [])
        end, ?config(NodeType, Return, []))
    end, ?CTH_NODES),
    {Return, State}.


%%--------------------------------------------------------------------
%% @doc
%% CTH callback called after end_per_suite.
%% Terminates mock_manager.
%% @end
%%--------------------------------------------------------------------
-spec post_end_per_suite(Suite :: atom(), Config :: [term()], Return :: term(),
    State :: []) -> {[term()], []}.
post_end_per_suite(_Suite, Config, Return, State) ->
    lists:foreach(fun(NodeType) ->
        lists:foreach(fun(N) ->
            ok = rpc:call(N, mock_manager, stop, [])
        end, ?config(NodeType, Config, []))
    end, ?CTH_NODES),
    {Return, State}.
