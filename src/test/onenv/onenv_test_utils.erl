%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% This module contains test utility functions useful in tests using onenv.
%%% @end
%%%-------------------------------------------------------------------
-module(onenv_test_utils).
-author("Michal Stanisz").

-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/aai/caveats.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([
    prepare_base_test_config/1,
    disable_panel_healthcheck/1,
    kill_node/2,
    start_node/2
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec prepare_base_test_config(test_config:config()) -> test_config:config().
prepare_base_test_config(Config) ->
    NodesPerProvider = lists:foldl(fun(Node, Acc) ->
        ProviderId = rpc:call(Node, oneprovider, get_id, []),
        OtherNodes = maps:get(ProviderId, Acc, []),
        Acc#{ProviderId => [Node | OtherNodes]}
    end, #{}, test_config:get_custom(Config, op_worker_nodes, [])),
    
    ProviderPanels = lists:foldl(fun(Node, Acc) ->
        [WorkerNode | _] = rpc:call(Node, service_op_worker, get_nodes, []),
        ProviderId = rpc:call(WorkerNode, oneprovider, get_id, []),
        OtherNodes = maps:get(ProviderId, Acc, []),
        Acc#{ProviderId => [Node | OtherNodes]}
    end, #{}, test_config:get_custom(Config, op_panel_nodes, [])),
    
    PrimaryCm = maps:fold(fun(ProviderId, [PanelNode | _], Acc) ->
        {ok, Hostname} = rpc:call(PanelNode, service_cluster_manager,  get_main_host, []),
        Acc#{ProviderId => list_to_atom("cluster_manager@" ++ Hostname)}
    end, #{}, ProviderPanels),
    
    ProvidersList = maps:keys(NodesPerProvider),
    
    ProviderSpaces = lists:foldl(fun(ProviderId, Acc) ->
        [Node | _] = maps:get(ProviderId, NodesPerProvider),
        {ok, Spaces} = rpc:call(Node, provider_logic, get_spaces, []),
        Acc#{ProviderId => Spaces}
    end, #{}, ProvidersList),
    
    ProviderUsers = lists:foldl(fun(ProviderId, Acc) ->
        [Node | _] = maps:get(ProviderId, NodesPerProvider),
        {ok, Users} = rpc:call(Node, provider_logic, get_eff_users, []),
        Acc#{ProviderId => Users}
    end, #{}, ProvidersList),
    
    test_config:set_many(Config, [
        [provider_nodes, NodesPerProvider],
        [providers, ProvidersList],
        [provider_panels, ProviderPanels],
        [primary_cm, PrimaryCm],
        [users, ProviderUsers],
        [provider_spaces, ProviderSpaces]
    ]).


-spec disable_panel_healthcheck(test_config:config()) -> ok.
disable_panel_healthcheck(Config) ->
    lists:foreach(fun(PanelNode) ->
        Ctx = rpc:call(PanelNode, service, get_ctx, [op_worker]),
        ok = rpc:call(PanelNode, service, deregister_healthcheck, [op_worker, Ctx])
    end, test_config:get_custom(Config, op_panel_nodes)).


-spec kill_node(test_config:config(), node()) -> ok.
kill_node(Config, Node) ->
    OnenvScript = test_config:get_onenv_script_path(Config),
    Pod = test_config:get_custom(Config, [pods, Node]),
    Service = get_service(Node),
    GetPidCommand = [
        "ps", "-eo", "'%p,%a'", 
        "|", "grep", "beam.*rel/" ++ Service, 
        "|", "head", "-n", "1", 
        "|", "cut", "-d", "','", "-f" , "1"
    ],
    PidToKill = ?assertMatch([_ | _], string:trim(utils:cmd([OnenvScript, "exec", Pod] ++ GetPidCommand))),
    [] = utils:cmd([OnenvScript, "exec", Pod, "kill", "-s", "SIGKILL", PidToKill]),
    ok.


-spec start_node(test_config:config(), node()) -> ok.
start_node(Config, Node) ->
    OnenvScript = test_config:get_onenv_script_path(Config),
    Service = get_service(Node),
    ScriptPath = test_config:get_custom(Config, list_to_atom(Service ++ "_script")),
    Pod = test_config:get_custom(Config, [pods, Node]),
    [] = utils:cmd([OnenvScript, "exec", Pod, ScriptPath, "start"]),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec get_service(node()) -> test_config:service_as_list().
get_service(Node) ->
    [Service, Host] = string:split(atom_to_list(Node), "@"),
    case Service of
        "onepanel" -> case string:find(Host, "onezone") of
            nomatch -> "op_panel";
            _ -> "oz_panel"
        end;
        _ -> Service
    end.
