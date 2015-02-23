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

-include("test/assertions.hrl").
-include_lib("common_test/include/ct.hrl").

%% API
-export([prepare_test_environment/3, clean_environment/1]).

%% ====================================================================
%% Starting and stoping nodes
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts deps and dockers with needed applications and mocks.
%% @end
%%--------------------------------------------------------------------
-spec prepare_test_environment(Config :: list(), DescriptionFile :: string(),
    Module :: module()) -> Result :: list() | {fail, tuple()}.
prepare_test_environment(Config, DescriptionFile, Module) ->
    try
        DataDir = ?config(data_dir, Config),
        PrivDir = ?config(priv_dir, Config),
        CtTestRoot = filename:join(DataDir, ".."),
        ProjectRoot = filename:join(CtTestRoot, ".."),

        ConfigWithPaths =
            [{ct_test_root, CtTestRoot}, {project_root, ProjectRoot} | Config],

        ProviderUpScript =
            filename:join([ProjectRoot, "bamboos", "docker", "provider_up.py"]),

        LogsDir = filename:join(PrivDir, atom_to_list(Module) ++ "_logs"),
        os:cmd("mkdir -p " ++ LogsDir),

        StartLog = cmd([ProviderUpScript,
            "-b", ProjectRoot,
            "-l", LogsDir,
            DescriptionFile]),

        EnvDesc = json_parser:parse_json_binary_to_atom_proplist(StartLog),

        Dns = ?config(dns, EnvDesc),
        Workers = ?config(op_worker_nodes, EnvDesc),
        CCMs = ?config(op_ccm_nodes, EnvDesc),

        erlang:set_cookie(node(), oneprovider_node),
        os:cmd("echo nameserver " ++ atom_to_list(Dns) ++ " > /etc/resolv.conf"),

        ping_nodes(lists:append(CCMs, Workers)),

        cluster_state_notifier:cast({subscribe_for_init, self(), length(Workers)}),
        receive
            init_finished -> ok
        after
            timer:seconds(50) -> throw(timeout)
        end,

        ok = load_modules(CCMs ++ Workers, [Module]),

        lists:append(ConfigWithPaths, proplists:delete(dns, EnvDesc))
    catch
        E1:E2 ->
            ct:print("Prepare of environment failed ~p:~p~n~p", [E1, E2, erlang:get_stacktrace()]),
            clean_environment(Config),
            {fail, {init_failed, E1, E2}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Cleans environment by running 'cleanup.py' script.
%% @end
%%--------------------------------------------------------------------
-spec clean_environment(Config :: list()) -> ok.
clean_environment(Config) ->
    Dockers = ?config(docker_ids, Config),
    case Dockers of
        undefined ->
            ok;
        _ ->
            DockersStr = lists:foldl(fun(D, Acc) ->
                DStr = atom_to_list(D),
                case Acc of
                    "" -> DStr;
                    _ -> Acc ++ " " ++ DStr
                end
            end, "", Dockers),

            ProjectRoot = ?config(project_root, Config),
            CleanupScript =
                filename:join([ProjectRoot, "bamboos", "docker", "cleanup.py"]),

            cmd([CleanupScript, DockersStr])
    end,
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks connection with nodes.
%% @end
%%--------------------------------------------------------------------
-spec ping_nodes(Nodes :: list()) -> ok | no_return().
ping_nodes(Nodes) ->
    ping_nodes(Nodes, 300).
ping_nodes(_Nodes, 0) ->
    throw(nodes_connection_error);
ping_nodes(Nodes, Tries) ->
    AllConnected = lists:all(fun(Node) ->
        pong == net_adm:ping(Node) end, Nodes),
    NotifierStatus = (catch sys:get_status({global, cluster_state_notifier})), %todo customize gen_server we're waiting for
    case {AllConnected, NotifierStatus} of
        {true, {status, _, _, [_, running, _, _, [_, {data, [{"Status", running}, _, _]}, _]]}} ->
            ok;
        _ ->
            timer:sleep(1000),
            ping_nodes(Nodes, Tries - 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Runs a command given by a string list.
%% @end
%%--------------------------------------------------------------------
cmd(Command) ->
    os:cmd(string:join(Command, " ")).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loads modules code on given nodes.
%% @end
%%--------------------------------------------------------------------
-spec load_modules(Nodes :: [node()], Modules :: [module()]) -> ok.
load_modules(_, []) ->
    ok;

load_modules(Nodes, [Module | Modules]) ->
    {Module, Binary, Filename} = code:get_object_code(Module),
    {_, _} = rpc:multicall(Nodes, code, delete, [Module]),
    {_, _} = rpc:multicall(Nodes, code, purge, [Module]),
    {_Replies, _} = rpc:multicall(Nodes, code, load_binary, [Module, Filename, Binary]),
    load_modules(Nodes, Modules).