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
-export([prepare_test_environment/3, clean_environment/1, load_modules/2,
    start_cover/0, stop_cover/0]).

-define(CLEANING_PROC_NAME, cleaning_proc).

%%%===================================================================
%%% Starting and stoping nodes
%%%===================================================================

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
        AppmockRoot = filename:join(ProjectRoot, "appmock"),
        CcmRoot = filename:join(ProjectRoot, "op_ccm"),

        ConfigWithPaths =
            [{ct_test_root, CtTestRoot}, {project_root, ProjectRoot} | Config],

        EnvUpScript =
            filename:join([ProjectRoot, "bamboos", "docker", "env_up.py"]),

        LogsDir = filename:join(PrivDir, atom_to_list(Module) ++ "_logs"),
        os:cmd("mkdir -p " ++ LogsDir),

        utils:cmd(["echo", "'" ++ DescriptionFile ++ ":'",">> prepare_test_environment.log"]),
        utils:cmd(["echo", "'" ++ DescriptionFile ++ ":'",">> prepare_test_environment_error.log"]),

        StartLog = list_to_binary(utils:cmd([EnvUpScript,
            %% Function is used durgin OP or GR tests so starts OP or GR - not both
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
            GrNodes = ?config(gr_nodes, EnvDesc),
            Workers = ?config(op_worker_nodes, EnvDesc),
            CCMs = ?config(op_ccm_nodes, EnvDesc),
            AllNodes = GrNodes ++ Workers ++ CCMs,

            erlang:set_cookie(node(), test_cookie),
            os:cmd("echo nameserver " ++ atom_to_list(Dns) ++ " > /etc/resolv.conf"),

            ping_nodes(AllNodes),
            global:sync(),
            ok = load_modules(AllNodes, [Module]),

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
    case cover:modules() of
        [] ->
            ok;
        _ ->
            GrNodes = ?config(gr_nodes, Config),
            Workers = ?config(op_worker_nodes, Config),
            CCMs = ?config(op_ccm_nodes, Config),
            AllNodes = GrNodes ++ Workers ++ CCMs,

            global:register_name(?CLEANING_PROC_NAME, self()),
            global:sync(),

            lists:foreach(fun(N) ->
                ok = rpc:call(N, application, stop, [op_worker])
            end, Workers),

            lists:foreach(fun(N) ->
                ok = rpc:call(N, application, stop, [op_ccm])
            end, CCMs),

            lists:foreach(fun(N) ->
                ok = rpc:call(N, application, stop, [globalregistry])
            end, GrNodes),

            lists:foreach(fun(_N) ->
                receive
                    {app_ended, CoverNode, FileData} ->
                        {Mega,Sec,Micro} = os:timestamp(),
                        CoverFile = atom_to_list(CoverNode) ++ integer_to_list((Mega*1000000 + Sec)*1000000 + Micro) ++ ".coverdata",
                        ok = file:write_file(CoverFile, FileData),
                        cover:import(CoverFile),
                        file:delete(CoverFile)
                    after
                        timer:minutes(1) ->
                            throw(cover_not_received)
                end
            end, AllNodes)
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
-spec start_cover() -> ok.
start_cover() ->
    case application:get_env(covered_dirs) of
        {ok, []} ->
            ok;
        {ok, Dirs} when is_list(Dirs) ->
            cover:start(),
            lists:foreach(fun(D) ->
                cover:compile_beam_directory(atom_to_list(D))
            end, Dirs),
            ok;
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Stops cover server if it is running.
%% @end
%%--------------------------------------------------------------------
stop_cover() ->
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
load_modules(_, []) ->
    ok;
load_modules(Nodes, [Module | Modules]) ->
    {Module, Binary, Filename} = code:get_object_code(Module),
    {_, _} = rpc:multicall(Nodes, code, delete, [Module]),
    {_, _} = rpc:multicall(Nodes, code, purge, [Module]),
    {_, _} = rpc:multicall(Nodes, code, load_binary, [Module, Filename, Binary]),
    load_modules(Nodes, Modules).
