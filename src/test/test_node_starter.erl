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
-export([prepare_test_environment/2, clean_environment/1]).

%% ====================================================================
%% Starting and stoping nodes
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts deps and dockers with needed applications and mocks.
%% @end
%%--------------------------------------------------------------------
-spec prepare_test_environment(Config :: list(), DescriptionFile :: string()) ->
  Result :: list().
prepare_test_environment(Config, DescriptionFile) ->
    DataDir = proplists:get_value(data_dir, Config),
    CtTestRoot = filename:join(DataDir, ".."),
    ProjectRoot = filename:join(CtTestRoot, ".."),

    ConfigWithPaths =
      [{ct_test_root, CtTestRoot}, {project_root, ProjectRoot} | Config],

    ProviderUpScript =
      filename:join([ProjectRoot, "bamboos", "docker", "provider_up.py"]),

    StartLog = cmd([ProviderUpScript, "-b", ProjectRoot, DescriptionFile]),
    EnvDesc = json_parser:parse_json_binary_to_atom_proplist(StartLog),

    Dns = ?config(dns, EnvDesc),
    Workers = ?config(op_worker_nodes, EnvDesc),
    Ccms = ?config(op_ccm_nodes, EnvDesc),

    erlang:set_cookie(node(), oneprovider_node),
    os:cmd("echo nameserver " ++ atom_to_list(Dns) ++ " > /etc/resolv.conf"),

    ping_nodes(lists:append(Ccms, Workers)),

    cluster_state_notifier:cast({subscribe_for_init, self(), length(Workers)}),
    receive
        init_finished -> ok
    after
        timer:seconds(50) -> throw(timeout)
    end,

    lists:append(ConfigWithPaths, proplists:delete(dns, EnvDesc)).

%%--------------------------------------------------------------------
%% @doc
%% Checks connection with nodes.
%% @end
%%--------------------------------------------------------------------
-spec ping_nodes(Nodes :: list()) -> ok | no_return().
ping_nodes(Nodes) ->
    ping_nodes(Nodes, 30).
ping_nodes(_Nodes, 0) ->
    throw(nodes_connection_error);
ping_nodes(Nodes, Tries) ->
    AllConnected = lists:all(fun(Node) -> pong == net_adm:ping(Node) end, Nodes),
    NotifierStatus = (catch sys:get_status({global, cluster_state_notifier})), %todo customize gen_server we're waiting for
    case {AllConnected, NotifierStatus} of
        {true, {status,_,_,[_,running,_,_,[_,{data,[{"Status",running},_,_]},_]]}} ->
            ok;
        _ ->
            timer:sleep(1000),
            ping_nodes(Nodes, Tries - 1)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts dockers and deps.
%% @end
%%--------------------------------------------------------------------
-spec clean_environment(Config :: list()) -> ok.
clean_environment(Config) ->
    Dockers = ?config(docker_ids, Config),
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
  
    cmd([CleanupScript, DockersStr]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs a command given by a string list.
%% @end
%%--------------------------------------------------------------------
cmd(Command) ->
  os:cmd(string:join(Command, " ")).