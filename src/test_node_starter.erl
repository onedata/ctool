%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% Functions used by ct test to start nodes for testing
%%% @end
%%% Created : 03. May 2014 8:26 PM
%%%-------------------------------------------------------------------
-module(test_node_starter).
-author("Tomasz Lichon").

-include("test_node_starter.hrl").
-include("assertions.hrl").

%% API
-export([start_test_nodes/1, start_test_nodes/2, start_test_node/3, stop_test_nodes/1]).
-export([start_app_on_node/4, stop_app_on_node/3]).
-export([set_env_vars/2,start_deps/1,stop_deps/1]).
-export([start_deps_for_tester_node/0,stop_deps_for_tester_node/0]).


%% start_test_nodes/1
%% ====================================================================
%% @doc Starts nodes needed for test.
-spec start_test_nodes(NodesNum :: integer()) -> Result when
    Result ::  list().
%% ====================================================================
start_test_nodes(NodesNum) ->
    start_test_nodes(NodesNum, false).

%% start_test_nodes/2
%% ====================================================================
%% @doc Starts nodes needed for test.
-spec start_test_nodes(NodesNum :: integer(), Verbose :: boolean()) -> Result when
    Result ::  list().
%% ====================================================================
start_test_nodes(0, _Verbose) ->
    [];
start_test_nodes(NodesNum, Verbose) ->
    NodeName = list_to_atom("slave"++integer_to_list(NodesNum)),
    Host = ?CURRENT_HOST,

    [start_test_node(NodeName, Host,Verbose) | start_test_nodes(NodesNum-1,Verbose)].

%% start_test_node/3
%% ====================================================================
%% @doc Starts new test node.
-spec start_test_node(NodeName :: atom(), Host :: atom(), Verbose :: boolean()) -> Result when
	Result :: {ok,node()} | {error,Error :: term()}.
%% ====================================================================
start_test_node(NodeName,Host,Verbose) ->
	% Prepare opts
	CodePathOpt = make_code_path(),
	VerboseOpt = case Verbose of
					 true -> "";
					 false -> " -noshell "
	             end,
	CookieOpt = " -setcookie "++atom_to_list(erlang:get_cookie())++" ",

	% Restart node
	stop_test_nodes([?NODE(Host,NodeName)]),
	slave:start(Host, NodeName,CodePathOpt++VerboseOpt++CookieOpt).

%% stop_test_nodes/1
%% ====================================================================
%% @doc Stops test nodes.
-spec stop_test_nodes(Node :: list(node())) -> list(Status) when
    Status :: ok | {error,Error :: term()}.
%% ====================================================================
stop_test_nodes([]) ->
    [];
stop_test_nodes([Node|Rest]) ->
    [slave:stop(Node) | stop_test_nodes(Rest)].

%% start_app_on_node/4
%% ====================================================================
%% @doc Starts app on test node.
-spec start_app_on_node(Node :: atom(),Application :: atom(), Deps :: list(atom()), EnvVars :: list(Env)) -> Result when
    Env :: {Name,Value},
    Name :: atom(),
    Value :: term(),
    Result :: node() | no_return().
%% ====================================================================
start_app_on_node(Node,Application,Deps,EnvVars) ->
	rpc:call(Node,application,start,[ctool]),
	rpc:call(Node,test_node_starter,start_deps,[Deps]),
	rpc:call(Node,application,load,[Application]),
	rpc:call(Node,test_node_starter,set_env_vars,[Application,EnvVars]),
	?assertMatch(ok,rpc:call(Node,application,start,[Application])),
	Node.

%% stop_app_on_node/3
%% ====================================================================
%% @doc Starts app on test node.
-spec stop_app_on_node(Node :: atom(), Application :: atom(), Deps :: list(atom())) -> Result when
    Result :: ok | {error,Error :: term()}.
%% ====================================================================
stop_app_on_node(Node,Application,Deps)->
	rpc:call(Node,application,unload,[Application]),
	rpc:call(Node,test_node_starter,stop_deps,[Deps]),
    rpc:call(Node,application,stop,[ctool]),
    rpc:call(Node,application,stop,[Application]).


%% make_code_path/0
%% ====================================================================
%% @doc Returns current code path string, formatted as erlang slave node argument.
%% @end
-spec make_code_path() -> string().
%% ====================================================================
make_code_path() ->
	lists:foldl(fun(Node, Path) -> " -pa " ++ Node ++ Path end,
		[], code:get_path()).

%% stop_deps/1
%% ====================================================================
%% @doc This function clears after the test.
-spec stop_deps(Deps :: list(atom)) -> list(Result::term()).
%% ====================================================================
stop_deps([]) ->
    [];
stop_deps([FirstDep | Rest]) ->
	[application:stop(FirstDep) | stop_deps(Rest)].

%% start_deps/1
%% ====================================================================
%% @doc This function sets environment for application.
-spec start_deps(Deps :: list(atom)) -> list(Result::term()).
%% ====================================================================
start_deps([]) ->
    [];
start_deps([lager | Rest]) ->
    [lager:start() | start_deps(Rest)];
start_deps([ssl | Rest]) ->
    [ssl:start() | start_deps(Rest)];
start_deps([FirstDep|Rest]) ->
	[application:start(FirstDep) | start_deps(Rest)].

%% set_env_vars/2
%% ====================================================================
%% @doc This function sets environment variables for application.
-spec set_env_vars(Application :: atom(),EnvVars :: list()) -> ok.
%% ====================================================================
set_env_vars(_Application,[]) ->
	ok;
set_env_vars(Application,[{Variable, Value} | Vars]) ->
	application:set_env(Application, Variable, Value),
	set_env_vars(Application,Vars).

%% ====================================================================
%% tester node deps
%% ====================================================================

%% start_deps_for_tester_node/0
%% ====================================================================
%% @doc Starts dependencies needed by tester node (node that does not
%% host application but coordinates test).
-spec start_deps_for_tester_node() -> Result when
    Result ::  ok | {error, Reason},
    Reason :: term().
%% ====================================================================
start_deps_for_tester_node() ->
    %% SASL reboot/start in order to disable TTY logging
    %% Normally `error_logger:tty(false)` should be enough, but some apps could start SASL anyway without our boot options
    application:stop(sasl),
    application:unload(sasl),
    application:load(sasl),
    application:set_env(sasl, sasl_error_logger, false),
    application:start(sasl),
    error_logger:tty(false),

    %% Start all deps
    ssl:start().

%% stop_deps_for_tester_node/0
%% ====================================================================
%% @doc Stops dependencies needed by tester node (node that does not
%% host application but coordinates test).
-spec stop_deps_for_tester_node() -> Result when
    Result ::  ok | {error, Reason},
    Reason :: term().
%% ====================================================================
stop_deps_for_tester_node() ->
    application:stop(ssl),
    application:stop(crypto),
    application:stop(public_key).