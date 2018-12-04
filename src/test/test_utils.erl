%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains helper functions for ct tests.
%%% @end
%%%-------------------------------------------------------------------
-module(test_utils).
-author("Krzysztof Trzepla").

-include("test/assertions.hrl").
-include("test/test_utils.hrl").

%% API
-export([mock_new/2, mock_new/3, mock_expect/4, mock_validate/2, mock_unload/1,
    mock_unload/2, mock_validate_and_unload/2, mock_assert_num_calls/5, mock_assert_num_calls/6]).
-export([get_env/3, set_env/4]).
-export([enable_datastore_models/2]).
-export([get_docker_ip/1]).

-define(TIMEOUT, timer:seconds(60)).
-define(ATTEMPTS, 10).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Enables given local models in datastore that runs on given nodes.
%% All given nodes should be form one single provider.
%% @end
%%--------------------------------------------------------------------
-spec enable_datastore_models(Nodes :: [node()], Models :: [model_behaviour:model_type()]) -> ok | no_return().
enable_datastore_models([H | _] = Nodes, Models) ->
    lists:foreach(
        fun(Model) ->
            {Module, Binary, Filename} = code:get_object_code(Model),
            {_, []} = rpc:multicall(Nodes, code, load_binary, [Module, Filename, Binary])
        end, Models),

    mock_unload(Nodes, [plugins]),
    catch mock_new(Nodes, [plugins]),
    ok = mock_expect(Nodes, plugins, apply,
        fun
            (datastore_config_plugin, models, []) ->
                meck:passthrough([datastore_config_plugin, models, []]) ++ Models;
            (A1, A2, A3) ->
                meck:passthrough([A1, A2, A3])
        end),

    lists:foreach(
        fun(Node) ->
            ok = gen_server:call({node_manager, Node},
                {apply, datastore, check_and_initialize_state, [H, Models]},
                ?TIMEOUT),
            ok = gen_server:call({node_manager, Node},
                {apply, datastore, cluster_initialized, []}, ?TIMEOUT)
        end, Nodes).


%%--------------------------------------------------------------------
%% @doc
%% Mocks module on provided nodes using default options.
%% @equiv mock_new(Nodes, Modules, [passthrough, no_history])
%% @end
%%--------------------------------------------------------------------
-spec mock_new(Nodes :: node() | [node()], Modules :: module() | [module()]) -> ok.
mock_new(Nodes, Modules) ->
    mock_new(Nodes, Modules, [passthrough, no_history]).

%%--------------------------------------------------------------------
%% @doc
%% Mocks module on provided nodes using custom options. Possible options are:
%% * passthrough - original module will be kept in the mock
%% * non_strict  - does not throw an error when mocking a module that doesn't
%%                 exist or has been renamed
%% * unstick     - mocking of sticky modules
%% * no_link     - mock is not linked to the creating process and will not
%%                 unload automatically when a crash occurs
%% IMPORTANT! This mock will be automatically removed on calling process exit.
%% @end
%%--------------------------------------------------------------------
-spec mock_new(Nodes :: node() | [node()], Modules :: module() | [module()],
    Options :: [mock_opt()]) -> ok.
mock_new(Nodes, Modules, Options) ->
    lists:foreach(fun(Node) ->
        lists:foreach(fun(Module) ->
            do_action(fun() ->
                rpc:call(Node, mock_manager, new, [Module, Options])
            end)
        end, as_list(Modules))
    end, as_list(Nodes)).

%%--------------------------------------------------------------------
%% @doc
%% Mocks module's function on given nodes using 'Expectation' function.
%% @end
%%--------------------------------------------------------------------
-spec mock_expect(Nodes :: node() | [node()], Module :: module(),
    FunctionName :: atom(), Expectation :: function()) -> ok.
mock_expect(Nodes, Module, FunctionName, Expectation) ->
    lists:foreach(fun(Node) ->
        do_action(fun() ->
            rpc:call(Node, meck, expect, [Module, FunctionName, Expectation], ?TIMEOUT)
        end)
    end, as_list(Nodes)).

%%--------------------------------------------------------------------
%% @doc
%% Validates modules' mocks on provided nodes.
%% @end
%%--------------------------------------------------------------------
-spec mock_validate(Nodes :: node() | [node()],
    Modules :: module() | [module()]) -> ok.
mock_validate(Nodes, Modules) ->
    lists:foreach(fun(Node) ->
        do_action(fun() ->
            case rpc:call(Node, meck, validate, [as_list(Modules)], ?TIMEOUT) of
                true ->
                    ok;
                Other ->
                    Other
            end
        end)
    end, as_list(Nodes)).


%%--------------------------------------------------------------------
%% @doc
%% Unloads all mocks on provided nodes.
%% @end
%%--------------------------------------------------------------------
-spec mock_unload(Nodes :: node() | [node()]) -> ok.
mock_unload(Nodes) ->
    lists:foreach(fun(Node) ->
        do_action(fun() ->
            case rpc:call(Node, meck, unload, [], ?TIMEOUT) of
                Unloaded when is_list(Unloaded) -> ok;
                {badrpc, {'EXIT', {{not_mocked, _}, _}}} ->
                    ok
            end
        end)
    end, as_list(Nodes)).

%%--------------------------------------------------------------------
%% @doc
%% Unloads modules' mocks on provided nodes.
%% @end
%%--------------------------------------------------------------------
-spec mock_unload(Nodes :: node() | [node()],
    Modules :: module() | [module()]) -> ok.
mock_unload(Nodes, Modules) ->
    lists:foreach(fun(Node) ->
        lists:foreach(fun(Module) ->
            do_action(fun() -> rpc:call(Node, mock_manager, unload, [Module]) end)
        end, as_list(Modules))
    end, as_list(Nodes)).

%%--------------------------------------------------------------------
%% @doc
%% Validates and unloads modules' mocks on provided nodes.
%% @end
%%--------------------------------------------------------------------
-spec mock_validate_and_unload(Nodes :: node() | [node()],
    Modules :: module() | [module()]) -> ok.
mock_validate_and_unload(Nodes, Modules) ->
    mock_validate(Nodes, Modules),
    mock_unload(Nodes, Modules).

%%--------------------------------------------------------------------
%% @doc
%% @equiv mock_assert_num_calls(Nodes, Module, FunctionName, FunctionArgs, CallsNumber, ?ATTEMPTS).
%% @end
%%--------------------------------------------------------------------
-spec mock_assert_num_calls(Nodes :: node() | [node()], Module :: module(),
    FunctionName :: atom(), FunctionArgs :: meck:args_spec(), CallsNumber :: non_neg_integer()) -> ok.
mock_assert_num_calls(Nodes, Module, FunctionName, FunctionArgs, CallsNumber) ->
    mock_assert_num_calls(Nodes, Module, FunctionName, FunctionArgs, CallsNumber, ?ATTEMPTS).

%%--------------------------------------------------------------------
%% @doc
%% Validates number of function calls for given nodes.
%% @end
%%--------------------------------------------------------------------
-spec mock_assert_num_calls(Nodes :: node() | [node()], Module :: module(),
    FunctionName :: atom(), FunctionArgs :: meck:args_spec(), CallsNumber :: non_neg_integer(),
    Attempts :: non_neg_integer()) -> ok.
mock_assert_num_calls(Nodes, Module, FunctionName, FunctionArgs, CallsNumber, Attempts) ->
    lists:foreach(fun(Node) ->
        ?assertEqual(CallsNumber, rpc:call(
            Node, meck, num_calls, [Module, FunctionName, FunctionArgs], ?TIMEOUT
        ), Attempts)
    end, as_list(Nodes)).

%%--------------------------------------------------------------------
%% @doc
%% Returns the value of the environment variable 'Name' for 'Application'.
%% @end
%%--------------------------------------------------------------------
-spec get_env(Node :: node(), Application :: atom(), Name :: atom()) ->
    {ok, Value :: term()} | {badrpc, Reason :: term()}.
get_env(Node, Application, Name) ->
    rpc:call(Node, application, get_env, [Application, Name]).

%%--------------------------------------------------------------------
%% @doc
%% Sets the value of the environment variable 'Name' for 'Application'
%% on given node(s).
%% @end
%%--------------------------------------------------------------------
-spec set_env(Nodes :: node() | [node()], Application :: atom(),
    Name :: atom(), Value :: term()) -> ok.
set_env(Nodes, Application, Name, Value) ->
    {Results, []} = rpc:multicall(as_list(Nodes), application, set_env,
        [Application, Name, Value]),
    true = lists:all(fun(X) -> X == ok end, Results),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Contacts docker daemon to check the IP of given node started as docker.
%% @end
%%--------------------------------------------------------------------
-spec get_docker_ip(Node :: atom()) -> binary().
get_docker_ip(Node) ->
    CMD = [
        "docker inspect",
        "--format '{{ .NetworkSettings.IPAddress }}'",
        utils:get_host(Node)
    ],
    re:replace(utils:cmd(CMD), "\\s+", "", [global, {return, binary}]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns term other than list as a single element list.
%% @end
%%--------------------------------------------------------------------
-spec as_list(Term :: term()) -> list().
as_list(Term) when is_list(Term) -> Term;
as_list(Term) -> [Term].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes function with retries (fun should end with ok).
%% @end
%%--------------------------------------------------------------------
-spec do_action(Fun :: fun(() -> ok)) -> ok | no_return().
do_action(Fun) ->
    do_action(Fun, 10).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes function with retries (fun should end with ok).
%% @end
%%--------------------------------------------------------------------
-spec do_action(Fun :: fun(() -> ok), Num :: integer()) -> ok | no_return().
do_action(Fun, 0) ->
    ?assertMatch(ok, Fun());
do_action(Fun, Num) ->
    try
        case Fun() of
            ok ->
                ok;
            Other ->
                ct:print("Action ~p failed with ans: ~p", [Fun, Other]),
                do_action(Fun, Num - 1)
        end
    catch
        E1:E2 ->
            ct:print("Action ~p failed with error: ~p", [Fun, {E1, E2}]),
            do_action(Fun, Num - 1)
    end.
