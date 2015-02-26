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
-export([prepare_test_environment/4, clean_environment/1]).

%% ====================================================================
%% Starting and stoping nodes
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts deps and dockers with needed applications and mocks.
%% @end
%%--------------------------------------------------------------------
-spec prepare_test_environment(Config :: list(), DescriptionFile :: string(),
    Script :: string(), Module :: module()) -> Result :: list() | {fail, tuple()}.
prepare_test_environment(Config, DescriptionFile, Script, Module) ->
    try
        DataDir = ?config(data_dir, Config),
        PrivDir = ?config(priv_dir, Config),
        CtTestRoot = filename:join(DataDir, ".."),
        ProjectRoot = filename:join(CtTestRoot, ".."),

        ConfigWithPaths =
            [{ct_test_root, CtTestRoot}, {project_root, ProjectRoot} | Config],

        ProviderUpScript =
            filename:join([ProjectRoot, "bamboos", "docker", Script]),

        LogsDir = filename:join(PrivDir, atom_to_list(Module) ++ "_logs"),
        os:cmd("mkdir -p " ++ LogsDir),

        StartLog = cmd([ProviderUpScript,
            "-b", ProjectRoot,
            "-l", LogsDir,
            DescriptionFile]),

        EnvDesc = json_parser:parse_json_binary_to_atom_proplist(StartLog),

        try
            Dns = ?config(dns, EnvDesc),
            AllNodes = case Script of
                "globalregistry_up.py" ->
                    ?config(gr_nodes, EnvDesc);
                _ ->
                    Workers = ?config(op_worker_nodes, EnvDesc),
                    CCMs = ?config(op_ccm_nodes, EnvDesc),
                    Workers ++ CCMs
            end,

            erlang:set_cookie(node(), oneprovider_node),
            os:cmd("echo nameserver " ++ atom_to_list(Dns) ++ " > /etc/resolv.conf"),

            ping_nodes(AllNodes),
            ok = load_modules(AllNodes, [Module]),

            global:sync(),
            lists:append(ConfigWithPaths, proplists:delete(dns, EnvDesc))
        catch
            E11:E12 ->
                ct:print("Prepare of environment failed ~p:~p~n~p", [E11, E12, erlang:get_stacktrace()]),
                clean_environment(EnvDesc),
                {fail, {init_failed, E11, E12}}
        end
    catch
        E21:E22 ->
            ct:print("Prepare of environment failed ~p:~p~n~p", [E21, E22, erlang:get_stacktrace()]),
            {fail, {init_failed, E21, E22}}
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
    case AllConnected of
        true ->
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