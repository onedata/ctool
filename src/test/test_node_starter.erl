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
-define(COOKIE_KEY, "vm.args/setcookie").

%% This is list of all applications that possibly could be started by test_node_starter
%% Update when adding new application (if you want to use callbacks without specifying apps list)
%% List of tuples {application name, name of node in json output}
-define(ALL_POSSIBLE_APPS, [
    {op_worker, op_worker_nodes},
    {cluster_worker, cluster_worker_nodes},
    {op_ccm, op_ccm_nodes},
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
        CcmRoot = filename:join(ProjectRoot, "op_ccm"),

        ConfigWithPaths =
            [{ct_test_root, CtTestRoot}, {project_root, ProjectRoot} | Config],

        EnvUpScript =
            filename:join([ProjectRoot, "bamboos", "docker", "env_up.py"]),

        LogsDir = filename:join(PrivDir, atom_to_list(TestModule) ++ "_logs"),
        os:cmd("mkdir -p " ++ LogsDir),

        utils:cmd(["echo", "'" ++ DescriptionFile ++ ":'", ">> prepare_test_environment.log"]),
        utils:cmd(["echo", "'" ++ DescriptionFile ++ ":'", ">> prepare_test_environment_error.log"]),

        StartLog = list_to_binary(utils:cmd([EnvUpScript,
            %% Function is used durgin OP or GR tests so starts OP or GR - not both
%% TODO uncomment below line after merging VFS-1426 and VFS-1431 to develop
%%             "--bin-cluster-worker", ProjectRoot,
            "--bin-worker", ProjectRoot,
            "--bin-gr", ProjectRoot,
            %% additionally AppMock can be started
            "--bin-appmock", AppmockRoot,
            "--bin-ccm", CcmRoot,
            "--logdir", LogsDir,
            DescriptionFile, "2>> prepare_test_environment_error.log"])),

        % Save start log to file
        utils:cmd(["echo", binary_to_list(<<"'", StartLog/binary, "'">>), ">> prepare_test_environment.log"]),

        EnvDesc = json_parser:parse_json_binary_to_atom_proplist(StartLog),

        try
            Dns = ?config(dns, EnvDesc),
            AllNodesWithCookies = lists:flatmap(
                fun({AppName, ConfigName}) ->
                    Nodes = ?config(ConfigName, EnvDesc),
                    get_cookies(Nodes, AppName, ?COOKIE_KEY, DescriptionFile)
                end, Apps
            ),

            os:cmd("echo nameserver " ++ atom_to_list(Dns) ++ " > /etc/resolv.conf"),

            ping_nodes(AllNodesWithCookies),
            global:sync(),
            load_modules(AllNodesWithCookies, [TestModule, test_utils | LoadModules]),

            lists:append([
                ConfigWithPaths,
                proplists:delete(dns, EnvDesc),
                rebar_git_plugin:get_git_metadata(),
                [{env_description, DescriptionFile}]

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
    case cover:modules() of
        [] ->
            ok;
        _ ->
            DescriptionFile = ?config(env_description, Config),
            global:register_name(?CLEANING_PROC_NAME, self()),
            global:sync(),

            lists:foreach(
                fun({AppName, ConfigName}) ->
                    Nodes = ?config(ConfigName, Config),
                    lists:foreach(fun(N) -> ok = rpc:call(N, application, stop, [AppName]) end, Nodes)
                end, Apps
            ),

            AllNodesWithCookies = lists:flatmap(
                fun({AppName, ConfigName}) ->
                    Nodes = ?config(ConfigName, Config),
                    get_cookies(Nodes, AppName, ?COOKIE_KEY, DescriptionFile)
                end, Apps
            ),

            lists:foreach(fun({_N, _C}) ->
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
            end, AllNodesWithCookies)
    end,

    Dockers = proplists:get_value(docker_ids, Config, []),
    ProjectRoot = ?config(project_root, Config),
    DockersStr = lists:map(fun atom_to_list/1, Dockers),
    CleanupScript =
        filename:join([ProjectRoot, "bamboos", "docker", "cleanup.py"]),
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
-spec ping_nodes(NodesWithCookies :: [{node(), atom()}]) -> ok | no_return().
ping_nodes(NodesWithCookies) ->
    ping_nodes(NodesWithCookies, 300).
ping_nodes(_Nodes, 0) ->
    throw(nodes_connection_error);
ping_nodes(NodesWithCookies, Tries) ->
    AllConnected = lists:all(
        fun({Node, Cookie}) ->
            erlang:set_cookie(node(), Cookie),
            pong == net_adm:ping(Node)
        end, NodesWithCookies),
    case AllConnected of
        true -> ok;
        _ ->
            timer:sleep(timer:seconds(1)),
            ping_nodes(NodesWithCookies, Tries - 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loads modules code on given nodes.
%% @end
%%--------------------------------------------------------------------
-spec load_modules(NodesWithCookies :: [{node(), atom()}], Modules :: [module()]) -> ok.
load_modules(NodesWithCookies, Modules) ->
    lists:foreach(fun({Node, Cookie}) ->
        erlang:set_cookie(node(), Cookie),
        lists:foreach(fun(Module) ->
            {Module, Binary, Filename} = code:get_object_code(Module),
            rpc:call(Node, code, delete, [Module], ?TIMEOUT),
            rpc:call(Node, code, purge, [Module], ?TIMEOUT),
            ?assertEqual({module, Module}, rpc:call(
                Node, code, load_binary, [Module, Filename, Binary], ?TIMEOUT
            ))
        end, Modules)
    end, NodesWithCookies).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get one key by concatenating many json keys, separated with "/" 
%% @end
%%--------------------------------------------------------------------
-spec get_json_key(Node :: atom(), Domain :: string(), NodeType :: string(),
    Key :: string()) -> string().
get_json_key(Node, Domain, NodeType, Key) ->
    [NodeName, DomainName | _] = string:tokens(utils:get_host(Node), "."),
    string:join([Domain, DomainName, NodeType, NodeName, Key], "/").

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get cookies for each Node in Nodes list.
%% CookieKey is the nested json key, independent of node type.
%% Returns list of tuples {Node, Cookie}
%% @end
%%--------------------------------------------------------------------
-spec get_cookies(Nodes::[node()] | [] | undefined , AppName :: string(), CookieKey :: string(),
    DescriptionFile :: string()) -> [{Node :: node(), Cookie :: atom()}] | [].
get_cookies(undefined, _AppName, _CookieKey, _DescriptionFile) -> [];
get_cookies([], _AppName, _CookieKey, _DescriptionFile) -> [];
get_cookies(Nodes, AppName, CookieKey, DescriptionFile) ->
%% TODO change key for ccm and worker if test is in cluster_worker repo
    lists:map(fun(Node) ->
        Key = case AppName of
                  globalregistry ->
                      get_json_key(Node, "globalregistry_domains", "globalregistry", CookieKey);
                  op_ccm ->
                      get_json_key(Node, "provider_domains", "op_ccm", CookieKey);
                  op_worker ->
                      get_json_key(Node, "provider_domains", "op_worker", CookieKey);
                  cluster_worker ->
                      get_json_key(Node, "cluster_domains", "cluster_worker", CookieKey)
              end,
        {Node, json_parser:get_value(Key, DescriptionFile, "/")}
end, Nodes).
