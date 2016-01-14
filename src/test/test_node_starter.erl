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
-export([prepare_test_environment/5, prepare_test_environment/4, clean_environment/2, clean_environment/1,
    load_modules/2, maybe_start_cover/0, maybe_stop_cover/0]).

-define(CLEANING_PROC_NAME, cleaning_proc).
-define(TIMEOUT, timer:seconds(60)).

%% This is list of all applications that possibly could be started by test_node_starter
%% Update when adding new application (if you want to use callbacks without specifying apps list)
%% List of tuples {application name, name of node in json output}
-define(ALL_POSSIBLE_APPS, [
    {op_worker, op_worker_nodes},
    {cluster_worker, cluster_worker_nodes},
    {cluster_manager, cluster_manager_nodes},
    {globalregistry, gr_nodes}
]).

%%%===================================================================
%%% Starting and stoping nodes
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts deps and dockers with needed applications and mocks.
%% @end
%%--------------------------------------------------------------------
-spec prepare_test_environment(Config :: list(), DescriptionFile :: string(),
    TestModule :: module(), LoadModules :: [module()]) -> Result :: list() | {fail, tuple()}.
prepare_test_environment(Config, DescriptionFile, TestModule, LoadModules) ->
    prepare_test_environment(Config, DescriptionFile, TestModule, LoadModules, ?ALL_POSSIBLE_APPS).

%%--------------------------------------------------------------------
%% @doc
%% Starts deps and dockers with needed applications and mocks.
%% @end
%%--------------------------------------------------------------------
-spec prepare_test_environment(Config :: list(), DescriptionFile :: string(),
    TestModule :: module(), LoadModules :: [module()], Apps :: [{AppName :: atom(), ConfigName :: atom()}])
        -> Result :: list() | {fail, tuple()}.
prepare_test_environment(Config, DescriptionFile, TestModule, LoadModules, Apps) ->
    try
        DataDir = ?config(data_dir, Config),
        PrivDir = ?config(priv_dir, Config),
        CtTestRoot = filename:join(DataDir, ".."),
        ProjectRoot = filename:join(CtTestRoot, ".."),
        AppmockRoot = filename:join(ProjectRoot, "appmock"),
        CmRoot = filename:join(ProjectRoot, "cluster_manager"),

        ConfigWithPaths =
            [{ct_test_root, CtTestRoot}, {project_root, ProjectRoot} | Config],

        EnvUpScript = filename:join([ProjectRoot, "bamboos", "docker", "env_up.py"]),

        LogsDir = filename:join(PrivDir, atom_to_list(TestModule) ++ "_logs"),
        os:cmd("mkdir -p " ++ LogsDir),

        utils:cmd(["echo", "'" ++ DescriptionFile ++ ":'", ">> prepare_test_environment.log"]),
        utils:cmd(["echo", "'" ++ DescriptionFile ++ ":'", ">> prepare_test_environment_error.log"]),

        StartLog = list_to_binary(utils:cmd([EnvUpScript,
            %% Function is used durgin OP or GR tests so starts OP or GR - not both
            "--bin-cluster-worker", ProjectRoot,
            "--bin-worker", ProjectRoot,
            "--bin-gr", ProjectRoot,
            %% additionally AppMock can be started
            "--bin-appmock", AppmockRoot,
            "--bin-cm", CmRoot,
            "--logdir", LogsDir,
            DescriptionFile, "2>> prepare_test_environment_error.log"])),

        % Save start log to file
        utils:cmd(["echo", binary_to_list(<<"'", StartLog/binary, "'">>), ">> prepare_test_environment.log"]),

        EnvDesc = json_parser:parse_json_binary_to_atom_proplist(StartLog),

        try
            Dns = ?config(dns, EnvDesc),
            AllNodes = lists:flatmap(fun({_, ConfigName}) -> ?config(ConfigName, EnvDesc) end, Apps),

            erlang:set_cookie(node(), test_cookie),
            os:cmd("echo nameserver " ++ atom_to_list(Dns) ++ " > /etc/resolv.conf"),

            ping_nodes(AllNodes),
            global:sync(),
            load_modules(AllNodes, [TestModule, test_utils | LoadModules]),

            lists:append([
                ConfigWithPaths,
                proplists:delete(dns, EnvDesc),
                rebar_git_plugin:get_git_metadata()
            ])
        catch
            E11:E12 ->
                ct:print("Preparation of environment failed ~p:~p~n" ++
                    "For details, check:~n" ++
                    "    prepare_test_environment_error.log~n" ++
                    "    prepare_test_environment.log~n" ++
                    "Stacktrace: ~p", [E11, E12, erlang:get_stacktrace()]),
                clean_environment(EnvDesc),
                {fail, {init_failed, E11, E12}}
        end
    catch
        E21:E22 ->
            ct:print("Prepare of environment failed ~p:~p~n~p",
                [E21, E22, erlang:get_stacktrace()]),
            {fail, {init_failed, E21, E22}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Gathers cover analysis reports if cover is started.
%% Afterwards, cleans environment by running 'cleanup.py' script.
%% @end
%%--------------------------------------------------------------------
-spec clean_environment(Config :: list()) -> ok.
clean_environment(Config) ->
    clean_environment(Config, ?ALL_POSSIBLE_APPS).

%%--------------------------------------------------------------------
%% @doc
%% Gathers cover analysis reports if cover is started.
%% Afterwards, cleans environment by running 'cleanup.py' script.
%% @end
%%--------------------------------------------------------------------
-spec clean_environment(Config :: list(), Apps::[{AppName::atom(), ConfigName::atom()}]) -> ok.
clean_environment(Config, Apps) ->
    try
        case cover:modules() of
            [] ->
                lists:foreach(
                    fun({AppName, ConfigName}) ->
                        Nodes = ?config(ConfigName, Config),
                        lists:foreach(fun(N) -> ok = rpc:call(N, application, stop, [AppName]) end, Nodes)
                    end, Apps
                );
            _ ->
                global:register_name(?CLEANING_PROC_NAME, self()),
                global:sync(),

                lists:foreach(
                    fun({AppName, ConfigName}) ->
                        Nodes = ?config(ConfigName, Config),
                        lists:foreach(fun(N) -> ok = rpc:call(N, application, stop, [AppName]) end, Nodes)
                    end, Apps
                ),

                AllNodes = lists:flatmap(fun({_, ConfigName}) -> ?config(ConfigName, Config) end, Apps),

                lists:foreach(
                    fun(_N) ->
                        receive
                            {app_ended, CoverNode, FileData} ->
                                {Mega, Sec, Micro} = os:timestamp(),
                                CoverFile = atom_to_list(CoverNode) ++
                                    integer_to_list((Mega * 1000000 + Sec) * 1000000 + Micro) ++
                                    ".coverdata",
                                ok = file:write_file(CoverFile, FileData),
                                cover:import(CoverFile),
                                file:delete(CoverFile)
                        after
                            ?TIMEOUT -> throw(cover_not_received)
                        end
                    end, AllNodes
                )
        end
    catch
        E1:E2 ->
            ct:print("Clearing of environment failed ~p:~p~n" ++
                "Stacktrace: ~p", [E1, E2, erlang:get_stacktrace()])
    end,

    Dockers = proplists:get_value(docker_ids, Config, []),
    ProjectRoot = ?config(project_root, Config),
    DockersStr = lists:map(fun atom_to_list/1, Dockers),
    CleanupScript = filename:join([ProjectRoot, "bamboos", "docker", "cleanup.py"]),
    utils:cmd([CleanupScript | DockersStr]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Starts cover server if needed (if apropriate env is set).
%% @end
%%--------------------------------------------------------------------
-spec maybe_start_cover() -> ok.
maybe_start_cover() ->
    case application:get_env(covered_dirs) of
        {ok, []} ->
            ok;
        {ok, Dirs} when is_list(Dirs) ->
            cover:start(),

            ExcludedModules = case application:get_env(covered_excluded_modules) of
                                  {ok, Mods} when is_list(Dirs) ->
                                      Mods;
                                  _ ->
                                      []
                              end,

            case ExcludedModules of
                [] ->
                    lists:foreach(fun(D) ->
                        cover:compile_beam_directory(atom_to_list(D))
                    end, Dirs);
                _ ->
                    lists:foreach(fun(D) ->
                        try
                            DStr = atom_to_list(D),
                            {ok, AllBeams} = file:list_dir(DStr),
                            ExcludedModulesFiles = lists:map(fun(M) ->
                                atom_to_list(M) ++ ".beam"
                            end, ExcludedModules),
                            lists:foreach(fun(File) ->
                                case lists:suffix(".beam", File) of
                                    true ->
                                        cover:compile_beam(DStr ++ "/" ++ File);
                                    _ ->
                                        ok
                                end
                            end, AllBeams -- ExcludedModulesFiles)
                        catch
                            _:_ -> ok % a dir may not exist (it is added for other project)
                        end
                    end, Dirs)
            end,
            ok;
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Stops cover server if it is running.
%% @end
%%--------------------------------------------------------------------
-spec maybe_stop_cover() -> ok.
maybe_stop_cover() ->
    case application:get_env(covered_dirs) of
        {ok, []} ->
            ok;
        {ok, Dirs} when is_list(Dirs) ->
            CoverFile = "cv.coverdata",
            cover:export(CoverFile),
            {ok, FileData} = file:read_file(CoverFile),
            ok = file:delete(CoverFile),
            true = is_pid(global:send(?CLEANING_PROC_NAME, {app_ended, node(), FileData})),
            cover:stop(),
            ok;
        _ ->
            ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
    AllConnected = lists:all(
        fun(Node) ->
            pong == net_adm:ping(Node)
        end, Nodes),
    case AllConnected of
        true -> ok;
        _ ->
            timer:sleep(timer:seconds(1)),
            ping_nodes(Nodes, Tries - 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loads modules code on given nodes.
%% @end
%%--------------------------------------------------------------------
-spec load_modules(Nodes :: [node()], Modules :: [module()]) -> ok.
load_modules(Nodes, Modules) ->
    lists:foreach(fun(Node) ->
        lists:foreach(fun(Module) ->
            {Module, Binary, Filename} = code:get_object_code(Module),
            rpc:call(Node, code, delete, [Module], ?TIMEOUT),
            rpc:call(Node, code, purge, [Module], ?TIMEOUT),
            ?assertEqual({module, Module}, rpc:call(
                Node, code, load_binary, [Module, Filename, Binary], ?TIMEOUT
            ))
        end, Modules)
    end, Nodes).
