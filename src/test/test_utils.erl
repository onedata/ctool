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
    mock_unload/2]).
-export([get_env/3, set_env/4]).

-type mock_opt() :: passthrough | non_strict | unstick | no_link.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Mocks module on provided nodes using default options.
%% @equiv mock_new(Nodes, Modules, [passthrough])
%% @end
%%--------------------------------------------------------------------
-spec mock_new(Nodes :: node() | [node()], Modules :: module() | [module()]) -> ok.
mock_new(Nodes, Modules) ->
    mock_new(Nodes, Modules, [passthrough]).

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
    Parent = self(),
    lists:foreach(fun(Node) ->
        lists:foreach(fun(Module) ->
            Ref = make_ref(),
            erlang:spawn_link(Node, fun() ->
                process_flag(trap_exit, true),
                meck:new(Module, Options),
                Parent ! Ref,
                receive
                    {'EXIT', Parent, _} -> meck:unload(Module);
                    {'EXIT', _, normal} -> ok
                end
            end),
            ?assertReceived(Ref, timer:seconds(5))
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
    {Results, _} = ?assertMatch({_, []}, rpc:multicall(
        as_list(Nodes), meck, expect, [Module, FunctionName, Expectation]
    )),
    ?assert(lists:all(fun(Result) -> Result =:= ok end, Results)).

%%--------------------------------------------------------------------
%% @doc
%% Validates modules' mocks on provided nodes.
%% @end
%%--------------------------------------------------------------------
-spec mock_validate(Nodes :: node() | [node()],
    Modules :: module() | [module()]) -> ok.
mock_validate(Nodes, Modules) ->
    lists:foreach(fun(Node) ->
        ?assert(rpc:call(Node, meck, validate, [as_list(Modules)]))
    end, as_list(Nodes)).


%%--------------------------------------------------------------------
%% @doc
%% Unloads all mocks on provided nodes.
%% @end
%%--------------------------------------------------------------------
-spec mock_unload(Nodes :: node() | [node()]) -> ok.
mock_unload(Nodes) ->
    lists:foreach(fun(Node) ->
        ?assertEqual(ok, rpc:call(Node, meck, unload, []))
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
        ?assertEqual(ok, rpc:call(Node, meck, unload, [as_list(Modules)]))
    end, as_list(Nodes)).

%%--------------------------------------------------------------------
%% @doc
%% Returns the value of the environment variable 'Name' for 'Application'.
%% @end
%%--------------------------------------------------------------------
-spec get_env(Node :: node(), Application :: atom(), Name :: atom()) -> Value :: term().
get_env(Node, Application, Name) ->
    rpc:call(Node, application, get_env, [Application, Name]).

%%--------------------------------------------------------------------
%% @doc
%% Sets the value of the environment variable 'Name' for 'Application'.
%% @end
%%--------------------------------------------------------------------
-spec set_env(Node :: node(), Application :: atom(), Name :: atom(), Value :: term()) -> ok.
set_env(Node, Application, Name, Value) ->
    rpc:call(Node, application, set_env, [Application, Name, Value]).

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