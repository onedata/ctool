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
% Starting and stoping nodes
-export([start_test_nodes/1, start_test_nodes/2, start_test_node/3, start_test_node/4]).
-export([stop_test_nodes/1]).

% Starting nodes with distributed app
-export([start_test_nodes_with_dist_app/2]).

% Starting and stoping app
-export([start_app_on_nodes/4]).
-export([stop_app_on_nodes/3]).

% Preparing environment for app
-export([set_env_vars/2,start_deps/1,stop_deps/1]).

% Starting and stoping deps for tester node (ct runner node)
-export([start_deps_for_tester_node/0,stop_deps_for_tester_node/0]).

% Helper function
-export([get_db_node/0]).

%% ====================================================================
%% Starting and stoping nodes
%% ====================================================================

%% start_test_nodes/1
%% ====================================================================
%% @doc Starts new nodes for test, with disabled verbose option
-spec start_test_nodes(NodesNum :: integer()) -> Result when
    Result ::  list().
%% ====================================================================
start_test_nodes(NodesNum) ->
    start_test_nodes(NodesNum, false).

%% start_test_nodes/2
%% ====================================================================
%% @doc Starts new nodes for test.
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
%% @doc Starts new test node, with no additional parameters
-spec start_test_node(NodeName :: atom(), Host :: atom(), Verbose :: boolean()) -> Result when
    Result :: node() | no_return().
%% ====================================================================
start_test_node(NodeName,Host,Verbose) ->
    start_test_node(NodeName,Host,Verbose,"").

%% start_test_node/4
%% ====================================================================
%% @doc Starts new test node.
-spec start_test_node(NodeName :: atom(), Host :: atom(), Verbose :: boolean(), Params :: string()) -> Result when
	Result :: node() | no_return().
%% ====================================================================
start_test_node(NodeName,Host,Verbose,Params) ->
	% Prepare opts
	CodePathOpt = make_code_path(),
	VerboseOpt = case Verbose of
					 true -> "";
					 false -> " -noshell "
	             end,
	CookieOpt = " -setcookie "++atom_to_list(erlang:get_cookie())++" ",

	% Restart node
	stop_test_nodes([?NODE(Host,NodeName)]),
    {Status,Node}=slave:start(Host, NodeName,CodePathOpt++VerboseOpt++CookieOpt++Params),
    ?assertEqual(ok,Status),
    Node.

%% stop_test_nodes/1
%% ====================================================================
%% @doc Stops test nodes.
-spec stop_test_nodes(Node :: list(node())) -> list(Status) when
    Status :: ok.
%% ====================================================================
stop_test_nodes([]) ->
    ok;
stop_test_nodes([Node|Rest]) ->
    slave:stop(Node),
    stop_test_nodes(Rest).

%% ====================================================================
%% Starting and stoping nodes with distributed app
%% ====================================================================

%% start_test_nodes_with_dist_app/2
%% ====================================================================
%% @doc Starts nodes needed for test.
-spec start_test_nodes_with_dist_app(NodesNum :: integer(), CCMNum :: integer()) -> Result when
    Result ::  list().
%% ====================================================================
start_test_nodes_with_dist_app(NodesNum, CCMNum) ->
    start_test_nodes_with_dist_app(NodesNum, CCMNum, false).

%% start_test_nodes_with_dist_app/3
%% ====================================================================
%% @doc Starts nodes needed for test.
-spec start_test_nodes_with_dist_app(NodesNum :: integer(), CCMNum :: integer(), Verbose :: boolean()) -> Result when
    Result ::  list().
%% ====================================================================
start_test_nodes_with_dist_app(0, _CCMNum, _Verbose) ->
    {[],[]};
start_test_nodes_with_dist_app(NodesNum, CCMNum, Verbose) ->
    Nodes = create_nodes_description(?CURRENT_HOST, [], NodesNum),

    DistNodes = create_dist_nodes_list(Nodes, CCMNum),
    DistAppDesc = create_dist_app_description(DistNodes),
    Params = create_nodes_params_for_dist_nodes(Nodes, DistNodes, DistAppDesc),

    {lists:map(fun({NodeName,Host}) -> start_test_node(NodeName,Host,Verbose,Params) end, Nodes), Params}.


%% ====================================================================
%% Starting and stoping app
%% ====================================================================

%% start_app_on_nodes/4
%% ====================================================================
%% @doc Starts app on test node.
-spec start_app_on_nodes(Application :: atom(), Deps :: list(list(atom())), Nodes :: list(atom()), EnvVars :: list(list(Env))) -> Result when
    Env :: {Name,Value},
    Name :: atom(),
    Value :: term(),
    Result :: list(node()) | no_return().
%% ====================================================================
start_app_on_nodes(_Application,_Deps,[],[]) ->
    [];
start_app_on_nodes(Application,Deps,[Node | OtherNodes],[EnvVars | OtherEnvVars]) ->
    [start_app_on_node(Application,Deps,Node,EnvVars) | start_app_on_nodes(Application,Deps,OtherNodes,OtherEnvVars)].

%% start_app_on_node/4
%% ====================================================================
%% @doc Starts app on test node.
-spec start_app_on_node(Application :: atom(),Deps :: list(atom()), Node :: atom(), EnvVars :: list(Env)) -> Result when
    Env :: {Name,Value},
    Name :: atom(),
    Value :: term(),
    Result :: node() | no_return().
%% ====================================================================
start_app_on_node(Application,Deps,Node,EnvVars) ->
	rpc:call(Node,application,start,[ctool]),
	rpc:call(Node,test_node_starter,start_deps,[Deps]),
	rpc:call(Node,application,load,[Application]),
	rpc:call(Node,test_node_starter,set_env_vars,[Application,EnvVars]),
	?assertEqual(ok,rpc:call(Node,application,start,[Application])),
	ok.

%% stop_app_on_nodes/3
%% ====================================================================
%% @doc Stops app on test nodes.
-spec stop_app_on_nodes(Application :: atom(), Deps :: list(atom()), Nodes :: list(atom())) -> list(Result) when
    Result :: ok | {error,Error :: term()}.
%% ====================================================================
stop_app_on_nodes(_Application,_Deps,[])->
    [];
stop_app_on_nodes(Application,Deps,[Node | OtherNodes])->
    [stop_app_on_node(Application,Deps,Node) | stop_app_on_nodes(Application,Deps,OtherNodes)].

%% stop_app_on_node/3
%% ====================================================================
%% @doc Stops app on test node.
-spec stop_app_on_node(Application :: atom(), Deps :: list(atom()), Node :: atom()) -> Result when
    Result :: ok | no_return().
%% ====================================================================
stop_app_on_node(Application,Deps,Node)->
	rpc:call(Node,application,unload,[Application]),
	rpc:call(Node,test_node_starter,stop_deps,[Deps]),
    rpc:call(Node,application,stop,[ctool]),
    ?assertEqual(ok,rpc:call(Node,application,stop,[Application])),
    ok.

%% ====================================================================
%% Preparing environment for app
%% ====================================================================

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
    application:load(lager),
    {ok, [Data]} = file:consult("sys.config"),
    Config = proplists:get_value(lager, Data),
    lists:foreach(
        fun(Key) ->
            application:set_env(lager, Key, proplists:get_value(Key, Config))
        end, proplists:get_keys(Config)),
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
%% Starting and stoping deps for tester node (ct runner node)
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

%% ====================================================================
%% Helper Functions
%% ====================================================================

%% get_db_node/0
%% ====================================================================
%% @doc This function returns db node.
-spec get_db_node() -> atom().
%% ====================================================================
get_db_node() ->
    ?NODE(?CURRENT_HOST,db).

%% ====================================================================
%% Internal Functions
%% ====================================================================

%% make_code_path/0
%% ====================================================================
%% @doc Returns current code path string, formatted as erlang slave node argument.
%% @end
-spec make_code_path() -> string().
%% ====================================================================
make_code_path() ->
    lists:foldl(fun(Node, Path) -> " -pa " ++ Node ++ Path end,
        [], code:get_path()).

%% create_nodes_description/3
%% ====================================================================
%% @doc Creates description of nodes needed for test.
-spec create_nodes_description(Host:: atom(), TmpAns :: list(), Counter :: integer()) -> Result when
    Result ::  list().
%% ====================================================================
create_nodes_description(_Host, Ans, 0) ->
    Ans;
create_nodes_description(Host, Ans, Counter) ->
    Desc = {list_to_atom("slave" ++ integer_to_list(Counter)), Host},
    create_nodes_description(Host, [Desc | Ans], Counter - 1).

%% create_dist_nodes_list/2
%% ====================================================================
%% @doc Creates list of nodes for distributed application
-spec create_dist_nodes_list(Nodes:: list(), DistNodesNum :: integer()) -> Result when
    Result ::  list().
%% ====================================================================
create_dist_nodes_list(_, 0) ->
    [];
create_dist_nodes_list([{NodeName, Host} | Nodes], DistNodesNum) ->
    Node = "'" ++ atom_to_list(NodeName) ++ "@" ++ atom_to_list(Host) ++ "'",
    [Node | create_dist_nodes_list(Nodes, DistNodesNum - 1)].

%% create_dist_app_description/1
%% ====================================================================
%% @doc Creates description of distributed application
-spec create_dist_app_description(DistNodes:: list()) -> Result when
    Result ::  string().
%% ====================================================================
create_dist_app_description(DistNodes) ->
    [Main | Rest] = DistNodes,
    RestString = lists:foldl(fun(N, TmpAns) ->
        case TmpAns of
            "" -> N;
            _ -> TmpAns ++ ", " ++ N
        end
    end, "", Rest),
    "\"[{veil_cluster_node, 1000, [" ++ Main ++ ", {" ++ RestString ++ "}]}]\"".

%% create_nodes_params_for_dist_nodes/3
%% ====================================================================
%% @doc Creates list of nodes for distributed application
-spec create_nodes_params_for_dist_nodes(Nodes:: list(), DistNodes :: list(), DistAppDescription :: string()) -> Result when
    Result ::  list().
%% ====================================================================
create_nodes_params_for_dist_nodes([], _DistNodes, _DistAppDescription) ->
    [];
create_nodes_params_for_dist_nodes([{NodeName, Host}  | Nodes], DistNodes, DistAppDescription) ->
    Node = "'" ++ atom_to_list(NodeName) ++ "@" ++ atom_to_list(Host) ++ "'",
    case lists:member(Node, DistNodes) of
        true ->
            SynchNodes = lists:delete(Node, DistNodes),
            SynchNodesString = lists:foldl(fun(N, TmpAns) ->
                case TmpAns of
                    "" -> N;
                    _ -> TmpAns ++ ", " ++ N
                end
            end, "", SynchNodes),
            Param = " -kernel distributed " ++ DistAppDescription ++ " -kernel sync_nodes_mandatory \"[" ++ SynchNodesString ++ "]\" -kernel sync_nodes_timeout 30000 ",
            [Param | create_nodes_params_for_dist_nodes(Nodes, DistNodes, DistAppDescription)];
        false -> ["" | create_nodes_params_for_dist_nodes(Nodes, DistNodes, DistAppDescription)]
    end.
