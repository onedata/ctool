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
-include("test/test_utils.hrl").

%% API
-export([prepare_test_environment/3, prepare_test_environment/2,
    clean_environment/1, clean_environment/2, load_modules/2,
    maybe_start_cover/0, maybe_stop_cover/0, finalize/1]).

-define(NODE_CALL_TIMEOUT, timer:seconds(60)).
-define(COOKIE_KEY, "vm.args/setcookie").
-define(CLEANING_PROC_NAME, cleaning_proc).

-define(ENV_UP_RETRIES_NUMBER, 3).
-define(ENV_UP_TIMEOUT_SECONDS, 1800).  % 30 minutes

%% This is list of all applications that possibly could be started by test_node_starter
%% Update when adding new application (if you want to use callbacks without specifying apps list)
%% List of tuples {application name, name of node in json output}
-define(ALL_POSSIBLE_APPS, [
    {op_worker, op_worker_nodes},
    {oz_worker, oz_worker_nodes},
    {cluster_worker, cluster_worker_nodes},
    {cluster_manager, cluster_manager_nodes},
    {onepanel, onepanel_nodes}
]).

-define(FORMAT_STRING_LIST(Strings), string:join(Strings, ", ")).
-define(FORMAT_ATOM_LIST(Atoms), string:join(lists:map(fun atom_to_list/1, Atoms), ", ")).

-define(PREVIOUS_ENV_UP_START_LOG, "/tmp/onedata/previous-env-up-start.log").


%%%===================================================================
%%% Starting and stoping nodes
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts deps and dockers with needed applications and mocks.
%% Sets cookies (read from DescriptionFile) for erlang nodes.
%% @end
%%--------------------------------------------------------------------
-spec prepare_test_environment(Config :: list(), TestModule :: module())
        -> Result :: list() | {fail, tuple()}.
prepare_test_environment(Config, TestModule) ->
    prepare_test_environment(Config, TestModule, ?ALL_POSSIBLE_APPS).

%%--------------------------------------------------------------------
%% @doc
%% Starts deps and dockers with needed applications and mocks.
%% Sets cookies (read from DescriptionFile) for erlang nodes.
%% @end
%%--------------------------------------------------------------------
-spec prepare_test_environment(Config :: list(), TestModule :: module(),
    Apps :: [{AppName :: atom(), ConfigName :: atom()}]) -> Result :: list() | {fail, tuple()}.
prepare_test_environment(Config, TestModule, Apps) ->
    DescriptionFile = env_description(Config),
    LoadModules = ?config(load_modules, Config, []),

    try
        CtTestRoot = test_utils:ct_tests_root_dir(Config),
        ProjectRoot = test_utils:project_root_dir(Config),
        AppmockRoot = filename:join(ProjectRoot, "appmock"),
        CmRoot = filename:join(ProjectRoot, "cluster_manager"),

        ConfigWithPaths = [{ct_test_root, CtTestRoot}, {project_root, ProjectRoot} | Config],

        PrivDir = ?config(priv_dir, Config),
        LogsDir = filename:join(PrivDir, atom_to_list(TestModule) ++ "_logs"),
        os:cmd("mkdir -p " ++ LogsDir),

        utils:cmd(["echo", "'" ++ DescriptionFile ++ ":'", ">> prepare_test_environment.log"]),
        utils:cmd(["echo", "'" ++ DescriptionFile ++ ":'", ">> prepare_test_environment_error.log"]),

        CleanEnv = case os:getenv("clean_env") of
            % for some reason, when atom true is returned under any key in final Config tests skip ¯\_(ツ)_/¯
            "true" -> clean;
            _ -> false
        end,

        StartLog = case CleanEnv of
            clean ->
                retry_running_env_up_script_until(
                    ProjectRoot, AppmockRoot, CmRoot, LogsDir, DescriptionFile, ?ENV_UP_RETRIES_NUMBER
                );
            false ->
                case try_reusing_previous_env() of
                    {true, PreviousStartLog} ->
                        PreviousStartLog;
                    false ->
                        retry_running_env_up_script_until(
                            ProjectRoot, AppmockRoot, CmRoot, LogsDir, DescriptionFile, ?ENV_UP_RETRIES_NUMBER
                        )
                end
        end,

        save_as_previous_env(StartLog),

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

            set_cookies(AllNodesWithCookies),
            os:cmd("echo nameserver " ++ atom_to_list(Dns) ++ " > /etc/resolv.conf"),

            AllNodes = [N || {N, _C} <- AllNodesWithCookies],
            ping_nodes(AllNodes),
            global:sync(),
            load_modules(AllNodes, [TestModule, test_utils | LoadModules]),

            lists:append([
                [{clean_env, CleanEnv} | ConfigWithPaths],
                proplists:delete(dns, EnvDesc),
                rebar_git_plugin:get_git_metadata()
            ])
        catch
            Class1:Reason1:Stacktrace1 ->
                ?ct_pal_exception(
                    "Preparation of environment failed. For details, check:~n"
                    "    prepare_test_environment_error.log~n"
                    "    prepare_test_environment.log",
                    Class1, Reason1, Stacktrace1
                ),
                clean_environment(EnvDesc),
                {fail, {init_failed, Class1, Reason1}}
        end

    catch
        Class2:Reason2:Stacktrace2 ->
            ?ct_pal_exception("Preparation of environment failed", Class2, Reason2, Stacktrace2),
            {fail, {init_failed, Class2, Reason2}}
    end.


%% @private
-spec save_as_previous_env(binary()) -> ok.
save_as_previous_env(StartLog) ->
    file:write_file(?PREVIOUS_ENV_UP_START_LOG, StartLog).


%% @private
-spec try_reusing_previous_env() -> {true, binary()} | false.
try_reusing_previous_env() ->
    case file:read_file(?PREVIOUS_ENV_UP_START_LOG) of
        {error, _} ->
            false;
        {ok, PreviousStartLog} ->
            try
                EnvDesc = json_parser:parse_json_binary_to_atom_proplist(PreviousStartLog),
                DockerIds = ?config(docker_ids, EnvDesc),
                DockerHostnames = lists:map(fun(DockerId) ->
                    DockerIp = try
                        test_utils:get_docker_ip(DockerId)
                    catch _:_ ->
                        error({cannot_resolve_node_ip, DockerId})
                    end,
                    0 == shell_utils:get_return_code(["ping", "-q", "-c", "1", binary_to_list(DockerIp)]) orelse error(
                        {node_not_responding_to_pings, DockerIp, DockerId}
                    ),
                    test_utils:get_docker_hostname(DockerId)
                end, DockerIds),
                ct:pal(binary_to_list(
                    str_utils:join_binary([<<"Reusing the previous environment:">> | DockerHostnames], <<"~n  * ">>)
                )),
                {true, PreviousStartLog}
            catch Class:Reason:Stacktrace ->
                ct:pal(
                    "Unable to reuse the previous environment:~n"
                    "> Caught: ~w:~p~n"
                    "> Stacktrace: ~s~n"
                    "~n"
                    "Starting a new environment...",
                    [Class, Reason, lager:pr_stacktrace(Stacktrace)]
                ),
                false
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Gathers cover analysis reports if cover is started.
%% Afterwards, cleans environment by running 'cleanup.py' script.
%% @end
%%--------------------------------------------------------------------
-spec clean_environment(Config :: list() | test_config:config()) -> ok.
clean_environment(Config) ->
    clean_environment(Config, ?ALL_POSSIBLE_APPS).


%%--------------------------------------------------------------------
%% @doc
%% Gathers cover analysis reports if cover is started.
%% Afterwards, cleans environment by running 'cleanup.py' script.
%% @end
%%--------------------------------------------------------------------
-spec clean_environment(Config :: list() | test_config:config(),
    Apps :: [{AppName :: atom(), ConfigName :: atom()}]) -> ok.
clean_environment(Config, Apps) ->
    case test_config:get_custom(Config, clean_env, clean) of
        false ->
            ct:pal("Environment cleaning skipped (requested with --no-clean)"),
            ok;
        clean ->
            StopStatus = try
                finalize(Config, Apps)
            catch
                Class:Reason:Stacktrace ->
                    ?ct_pal_exception("Environment cleanup failed", Class, Reason, Stacktrace),
                    Reason
            end,
            Dockers = proplists:get_value(docker_ids, Config, []),
            DockersStr = lists:map(fun atom_to_list/1, Dockers),
            ProjectRoot = ?config(project_root, Config),
            ct:pal("Removing dockers: ~s", [?FORMAT_STRING_LIST(
                lists:map(fun(DockerIdStr) -> string:slice(DockerIdStr, 0, 7) end, DockersStr)
            )]),
            remove_dockers(ProjectRoot, DockersStr),

            case StopStatus of
                ok ->
                    ok;
                _ ->
                    throw(StopStatus)
            end
    end.


-spec finalize(Config :: list() | test_config:config()) -> ok.
finalize(Config) ->
    finalize(Config, ?ALL_POSSIBLE_APPS).


-spec finalize(Config :: list() | test_config:config(),
    Apps :: [{AppName :: atom(), ConfigName :: atom()}]) -> ok.
finalize(Config, Apps) ->
    NodesWithCover = get_nodes_with_cover(Config, Apps),
    stop_applications(Config, Apps),
    gather_cover(NodesWithCover).


-spec get_nodes_with_cover(Config :: list() | test_config:config(),
    Apps :: [{AppName :: atom(), ConfigName :: atom()}]) -> [node()].
get_nodes_with_cover(Config, Apps) ->
    lists:flatmap(fun({AppName, ConfigName}) ->
        Nodes = test_config:get_custom(Config, ConfigName, []),
        lists:filter(fun(Node) ->
            case rpc:call(Node, application, get_env, [AppName, covered_dirs]) of
                {ok, []} -> false;
                {ok, _} -> true;
                _ -> false
            end
        end, Nodes)
    end, Apps).


-spec gather_cover([node()]) -> ok.
gather_cover([]) ->
    ok;
gather_cover(NodesWithCover) ->
    erlang:register(?CLEANING_PROC_NAME, self()),

    ct:pal("Gathering cover from nodes: ~s", [?FORMAT_ATOM_LIST(NodesWithCover)]),
    lists:foreach(fun(Node) ->
        receive
            {app_ended, CoverNode, FileData} ->
                CoverFile = str_utils:format("~s-~s.coverdata", [CoverNode, str_utils:rand_hex(10)]),
                ok = file:write_file(CoverFile, FileData),
                ok = cover:import(CoverFile),
                file:delete(CoverFile)
        after
            ?NODE_CALL_TIMEOUT ->
                ct:pal(
                    "WARNING: Could not collect cover data from node: ~p", [
                        Node
                    ])
        end
    end, NodesWithCover),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Starts cover server if needed (if appropriate env is set).
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
                            _:_ ->
                                ok % a dir may not exist (it is added for other project)
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
            ok = cover:export(CoverFile),
            {ok, FileData} = file:read_file(CoverFile),
            ok = file:delete(CoverFile),
            lists:foreach(fun(Node) ->
                Pid = rpc:call(Node, erlang, whereis, [?CLEANING_PROC_NAME]),
                case is_pid(Pid) of
                    true -> Pid ! {app_ended, node(), FileData};
                    false -> ok
                end
            end, nodes(hidden)),
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
%% Stops all started applications
%% @end
%%--------------------------------------------------------------------
-spec stop_applications(Config :: list() | test_config:config(),
    Apps :: [{AppName :: atom(), ConfigName :: atom()}]) -> ok.
stop_applications(Config, Apps) ->
    lists:foreach(
        fun({AppName, ConfigName}) ->
            Nodes = test_config:get_custom(Config, ConfigName, []),
            lists:foreach(fun(Node) ->
                try
                    ct:pal("Stopping application '~s' on node ~s...", [AppName, Node]),
                    ok = rpc:call(Node, application, stop, [AppName])
                catch
                    _:{badmatch, {badrpc, nodedown}} ->
                        ok; % Test can kill nodes
                    Type:Reason:Stacktrace ->
                        ct:pal(
                            "WARNING: Stopping application ~p on node ~p failed - ~p:~p~n"
                            "Stacktrace: ~s", [
                                AppName, Node, Type, Reason, lager:pr_stacktrace(Stacktrace)
                            ])
                end
            end, Nodes)
        end, Apps
    ),
    ct:pal("Applications successfully stopped on all nodes").

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
            true == net_kernel:hidden_connect_node(Node)
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
            try
                {Module, Binary, Filename} = code:get_object_code(Module),
                rpc:call(Node, code, delete, [Module], ?NODE_CALL_TIMEOUT),
                rpc:call(Node, code, purge, [Module], ?NODE_CALL_TIMEOUT),
                ?assertEqual({module, Module}, rpc:call(
                    Node, code, load_binary, [Module, Filename, Binary], ?NODE_CALL_TIMEOUT
                ))
            catch Class:Reason:Stacktrace ->
                ct:print(
                    "Cannot load module '~w' on node ~w, does the module exist in 'test_distributed' directory?~n"
                    "Error was: ~w:~p~n"
                    "Stacktrace: ~s",
                    [Module, Node, Class, Reason, lager:pr_stacktrace(Stacktrace)]
                ),
                error({cannot_load_module, Module})
            end
        end, Modules)
    end, Nodes).


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
-spec get_cookies(Nodes :: [node()] | [] | undefined, AppName :: string(), CookieKey :: string(),
    DescriptionFile :: string()) -> [{Node :: node(), Cookie :: atom()}] | [].
get_cookies(undefined, _AppName, _CookieKey, _DescriptionFile) -> [];
get_cookies([], _AppName, _CookieKey, _DescriptionFile) -> [];
get_cookies(Nodes, AppName, CookieKey, DescriptionFile) ->
    lists:map(fun(Node) ->
        Key = case AppName of
            oz_worker ->
                get_json_key(Node, "zone_domains", "oz_worker", CookieKey);
            cluster_manager ->
                [
                    get_json_key(Node, "zone_domains", "cluster_manager", CookieKey),
                    get_json_key(Node, "provider_domains", "cluster_manager", CookieKey),
                    get_json_key(Node, "cluster_domains", "cluster_manager", CookieKey)
                ];
            op_worker ->
                get_json_key(Node, "provider_domains", "op_worker", CookieKey);
            cluster_worker ->
                get_json_key(Node, "cluster_domains", "cluster_worker", CookieKey);
            onepanel ->
                get_json_key(Node, "onepanel_domains", "onepanel", CookieKey)
        end,
        case AppName of
            cluster_manager ->
                %% if AppName is cluster_manager, the Key is a list
                %% choose value that is different that undefined,
                %% there should be only one such value
                FilterUndefined = fun F([H | T]) ->
                    case json_parser:get_value(H, DescriptionFile, "/") of
                        undefined -> F(T);
                        Value -> {Node, Value}
                    end
                end,
                FilterUndefined(Key);
            _ -> {Node, json_parser:get_value(Key, DescriptionFile, "/")}
        end
    end, Nodes).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set cookies for each Node in NodesWithCookies list.
%% @end
%%--------------------------------------------------------------------
-spec set_cookies([{node(), atom()}]) -> ok.
set_cookies(NodesWithCookies) ->
    lists:foreach(fun({N, C}) ->
        erlang:set_cookie(N, C)
    end, NodesWithCookies).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Try to successfully run env_up.py. Retry RetriesNumber of times.
%% Raises env_up_failed if script fails to start for given number of times.
%% @end
%%--------------------------------------------------------------------
-spec retry_running_env_up_script_until(string(), string(), string(), string(),
    string(), integer()) -> binary().
retry_running_env_up_script_until(ProjectRoot, AppmockRoot, CmRoot,
    LogsDir, DescriptionFile, RetriesNumber) ->

    case run_env_up_script(ProjectRoot, AppmockRoot, CmRoot, LogsDir, DescriptionFile) of
        {success, StartLog} ->
            StartLog;
        {failure, StartLog} ->
            ct:pal(
                "ERROR: the env_up command failed with output:~n"
                "---------------------------------------------~n"
                "~s",
                [case StartLog of
                    <<"">> -> <<"<empty string>">>;
                    Other -> Other
                end]
            ),

            TestmasterDockerId = string:strip(os:cmd(
                "docker ps -a | grep testmaster | awk '{print $1}'"
            ), right, $\n),
            AllEnvUpDockerIds = string:tokens(string:strip(os:cmd(
                "docker ps -a | egrep '[0-9]+\.test' | awk '{print $1}'"
            ), right, $\n), "\n"),
            DockersToRemove = lists:delete(TestmasterDockerId, AllEnvUpDockerIds),
            ct:pal("Removing the following dockers: ~p", [DockersToRemove]),
            remove_dockers(ProjectRoot, DockersToRemove),

            case RetriesNumber > 0 of
                true ->
                    ct:pal("Retrying to run env_up.py. Number of retries left: ~p", [RetriesNumber]),
                    timer:sleep(timer:seconds(1)),
                    retry_running_env_up_script_until(ProjectRoot, AppmockRoot, CmRoot,
                        LogsDir, DescriptionFile, RetriesNumber - 1);
                _ ->
                    error(env_up_failed)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Run env_up.py script
%% @end
%%--------------------------------------------------------------------
-spec run_env_up_script(string(), string(), string(), string(), string()) -> {success | failure, binary()}.
run_env_up_script(ProjectRoot, AppmockRoot, CmRoot, LogsDir, DescriptionFile) ->
    EnvUpScript = filename:join([ProjectRoot, "bamboos", "docker", "env_up.py"]),
    TimeoutStr = integer_to_list(?ENV_UP_TIMEOUT_SECONDS),

    StartLogRaw = str_utils:unicode_list_to_binary(utils:cmd([
        "timeout", TimeoutStr, EnvUpScript,
        %% Function is used during OP or OZ tests so starts OP or OZ - not both
        "--bin-cluster-worker", ProjectRoot,
        "--bin-worker", ProjectRoot,
        "--bin-oz", ProjectRoot,
        "--bin-onepanel", ProjectRoot,
        %% additionally AppMock can be started
        "--bin-appmock", AppmockRoot,
        "--bin-cm", CmRoot,
        "--logdir", LogsDir,
        DescriptionFile, "2>> prepare_test_environment_error.log",
        "
        EXIT_CODE=$?
        if [ \"$EXIT_CODE\" -ne 0 ]; then
            echo \"\\n---------------------------------------------\\n\"
            if [ \"$EXIT_CODE\" -eq 124 ]; then
                echo \"ERROR: interrupted environment preparation due to timeout\\n\"
                echo \"\\n---------------------------------------------\\n\"
            fi
            echo \"STDERR:\\n\"
            cat prepare_test_environment_error.log
            echo \"\\n---------------------------------------------\\n\"
            echo \"Result of the docker ps command:\\n\"
            docker ps -a
            echo \"\"
            if [ \"$EXIT_CODE\" -eq 124 ]; then
                echo \"ERROR: environment failed to start within " ++ TimeoutStr ++ " seconds (see above for more info)\"
            else
                echo \"ERROR: environment preparation returned non-zero exit code: $EXIT_CODE (see above for more info)\"
            fi
        fi
        "
    ])),

    % TODO VFS-1816 remove log filter
    % Some of env_up logs goes to stdout instead of stderr, they need to be
    % removed for proper parsing of JSON with env_up result
    case binary:split(StartLogRaw, <<"\n">>, [global, trim]) of
        [] ->
            {failure, <<"">>};
        NonEmptyList ->
            case lists:last(NonEmptyList) of
                <<"ERROR", _/binary>> ->
                    {failure, StartLogRaw};
                LastOutputLine ->
                    {success, LastOutputLine}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns absolute path to environment description file
%% @end
%%--------------------------------------------------------------------
-spec env_description([term()]) -> file:filename_all().
env_description(Config) ->
    EnvDescriptionRelativePath = ?config(?ENV_DESCRIPTION, Config, ?DEFAULT_ENV_DESCRIPTION),
    ?TEST_FILE(Config, EnvDescriptionRelativePath).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes docker containers.
%% @end
%%--------------------------------------------------------------------
-spec remove_dockers(string(), list(string())) -> string().
remove_dockers(ProjectRoot, DockerIds) ->
    CleanupScript =
        filename:join([ProjectRoot, "bamboos", "docker", "cleanup.py"]),
    utils:cmd([CleanupScript | DockerIds]).
