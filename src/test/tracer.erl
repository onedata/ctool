%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Manages erlang tracer on remote nodes from current node.
%%% @end
%%%--------------------------------------------------------------------
-module(tracer).
-author("Tomasz Lichon").

%% API
-export([start/0, start/1, stop/0, trace_calls/1, trace_calls/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @equiv start(node())
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, MatchDesc} | {error, term()} when
    MatchDesc :: [MatchNum],
    MatchNum :: {matched, node(), integer()} | {matched, node(), 0, RPCError},
    RPCError :: term().
start() -> start(node()).


%%--------------------------------------------------------------------
%% @doc
%% Start tracer server on local node and given remote nodes. Initializes
%% tracking of function calls for all processes.
%% @end
%%--------------------------------------------------------------------
-spec start(node() | [node()]) -> {ok, MatchDesc} | {error, term()} when
    MatchDesc :: [MatchNum],
    MatchNum :: {matched, node(), integer()} | {matched, node(), 0, RPCError},
    RPCError :: term().
start(Node) when is_atom(Node) -> start([Node]);
start(Nodes) ->
    dbg:tracer(),
    lists:foreach(fun(Node) -> dbg:n(Node) end, Nodes),
    dbg:p(all, c).

%%--------------------------------------------------------------------
%% @doc
%% Stops tracer servers and clears trace patterns.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    dbg:stop_clear().

%%--------------------------------------------------------------------
%% @doc
%% Trace calls to selected module.
%% @end
%%--------------------------------------------------------------------
-spec trace_calls(module()) -> {ok, MatchDesc} | {error, term()} when
    MatchDesc :: [MatchNum],
    MatchNum :: {matched, node(), integer()} | {matched, node(), 0, RPCError},
    RPCError :: term().
trace_calls(Module) ->
    dbg:tpl(Module, x).

%%--------------------------------------------------------------------
%% @doc
%% Trace calls to selected function.
%% @end
%%--------------------------------------------------------------------
-spec trace_calls(module(), atom()) -> {ok, MatchDesc} | {error, term()} when
    MatchDesc :: [MatchNum],
    MatchNum :: {matched, node(), integer()} | {matched, node(), 0, RPCError},
    RPCError :: term().
trace_calls(Module, Function) ->
    dbg:tpl(Module, Function, '_', x).


%%%===================================================================
%%% Internal functions
%%%===================================================================