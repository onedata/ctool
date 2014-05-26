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
-export([start_test_node/4, start_test_node/5, stop_test_node/2]).
-export([set_env_vars/1,start_deps/1,stop_deps/1]).


%% start_globalregistry_node/4
%% ====================================================================
%% @doc Starts new node with globalregistry, with silent mode
-spec start_test_node(NodeName :: atom(), Host :: atom(), Deps :: list(atom()), EnvVars :: list(Env)) -> node() | no_return() when
	Env :: {Name,Value},
	Name :: atom(),
	Value :: term().
%% ====================================================================
start_test_node(NodeName,Host,Deps,EnvVars) ->
	start_test_node(NodeName,Host,Deps,EnvVars,false).

%% start_globalregistry_node/5
%% ====================================================================
%% @doc Starts new node with globalregistry.
-spec start_test_node(NodeName :: atom(), Host :: atom(), Deps :: list(atom()), EnvVars :: list(Env), Verbose :: boolean()) -> Result when
	Env :: {Name,Value},
	Name :: atom(),
	Value :: term(),
	Result :: node() | no_return().
%% ====================================================================
start_test_node(NodeName,Host,Deps,EnvVars,Verbose) ->
	% Prepare opts
	CodePathOpt = make_code_path(),
	VerboseOpt = case Verbose of
					 true -> "";
					 false -> " -noshell "
	             end,
	CookieOpt = " -setcookie "++atom_to_list(erlang:get_cookie())++" ",

	% Start node
	stop_test_node(?NODE(Host,NodeName),Deps),
	{ok,Node} = slave:start(Host, NodeName,CodePathOpt++VerboseOpt++CookieOpt),

	% Prepare environment
	rpc:call(Node,application,start,[ctool]),
	rpc:call(Node,test_node_starter,start_deps,[Deps]),
	rpc:call(Node,application,load,[globalregistry]),
	rpc:call(Node,test_node_starter,set_env_vars,[EnvVars]),
	?assertMatch(ok,rpc:call(Node,application,start,[globalregistry])),
	Node.

%% stop_globalregistry_node/2
%% ====================================================================
%% @doc Stops globalregistry node.
-spec stop_test_node(Node :: node(), Deps :: list(atom())) -> ok | no_return().
%% ====================================================================
stop_test_node(Node,Deps) ->
	rpc:call(Node,application,unload,[globalregistry]),
	rpc:call(Node,test_node_starter,stop_deps,[Deps]),
    rpc:call(Node,application,stop,[ctool]),
    rpc:call(Node,application,stop,[globalregistry]),
	slave:stop(Node).

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
%% 	application:unload(globalregistry).

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
%% 	application:load(globalregistry).

%% set_env_vars/1
%% ====================================================================
%% @doc This function sets environment variables for application.
-spec set_env_vars(EnvVars :: list()) -> ok.
%% ====================================================================
set_env_vars([]) ->
	ok;
set_env_vars([{Variable, Value} | Vars]) ->
	application:set_env(globalregistry, Variable, Value),
	set_env_vars(Vars).
