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

-include("test/test_utils.hrl").

%% API
-export([mock_new/2, mock_new/3, mock_expect/4, mock_validate/2, mock_unload/2]).
-export([receive_any/0, receive_any/1, receive_msg/1, receive_msg/2]).

-type mock_opts() :: passthrough | non_strict | unstick | no_link.
-type mock_module_opts() :: [mock_opts()].
-type mock_module_spec() :: module() | {module(), mock_module_opts()}.

-define(NEW_MOCK_DEFAULT_OPTIONS, [passthrough, non_strict, unstick, no_link]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Mocks given modules on given nodes using default options:
%% * passthrough - original module will be kept in the mock
%% * non_strict  - does not throw an error when mocking a module that doesn't
%%                 exist or has been renamed
%% * unstick     - mocking of sticky modules
%% * no_link     - mock is not linked to the creating process and will not
%%                 unload automatically when a crash occurs
%% @equiv mock_new(Nodes, ModuleSpecs, [passthrough, non_strict, unstick, no_link])
%% @end
%%--------------------------------------------------------------------
-spec mock_new(Nodes :: node() | [node()], ModuleSpecs :: [mock_module_spec()]) ->
    ok | no_return().
mock_new(Nodes, ModuleSpec) ->
    mock_new(Nodes, ModuleSpec, ?NEW_MOCK_DEFAULT_OPTIONS).

%%--------------------------------------------------------------------
%% @doc
%% Mocks given modules on given nodes using custom options for each module
%% if not provided.
%% @end
%%--------------------------------------------------------------------
-spec mock_new(Nodes :: node() | [node()], ModuleSpecs :: [mock_module_spec()],
    CustomOptions :: mock_module_opts()) -> ok | no_return().
mock_new(Nodes, ModuleSpecs, CustomOptions) ->
    lists:foreach(fun(Node) ->
        lists:foreach(fun
            ({ModuleSpec, Options}) ->
                ?assertEqual(ok, rpc:call(Node, meck, new, [ModuleSpec, Options]));
            (ModuleSpec) ->
                ?assertEqual(ok, rpc:call(Node, meck, new, [ModuleSpec, CustomOptions]))
        end, as_list(ModuleSpecs))
    end, as_list(Nodes)).

%%--------------------------------------------------------------------
%% @doc
%% Mocks module's function on given nodes using 'Expectation' function.
%% @end
%%--------------------------------------------------------------------
-spec mock_expect(Nodes :: node() | [node()], Module :: module(),
    FunctionName :: atom(), Expectation :: function()) -> ok | no_return().
mock_expect(Nodes, Module, FunctionName, Expectation) ->
    lists:foreach(fun(Node) ->
        ?assertEqual(ok, rpc:call(Node, meck, expect, [Module, FunctionName, Expectation]))
    end, as_list(Nodes)).

%%--------------------------------------------------------------------
%% @doc
%% Validates modules' mocks on given nodes.
%% @end
%%--------------------------------------------------------------------
-spec mock_validate(Nodes :: node() | [node()], Modules :: module() | [module()]) ->
    ok | no_return().
mock_validate(Nodes, Modules) ->
    lists:foreach(fun(Node) ->
        lists:foreach(fun(Module) ->
            ?assert(rpc:call(Node, meck, validate, [Module]))
        end, as_list(Modules))
    end, as_list(Nodes)).

%%--------------------------------------------------------------------
%% @doc
%% Validates modules' mocks on given nodes.
%% @end
%%--------------------------------------------------------------------
-spec mock_unload(Nodes :: node() | [node()], Modules :: module() | [module()]) ->
    ok | no_return().
mock_unload(Nodes, Modules) ->
    lists:foreach(fun(Node) ->
        lists:foreach(fun(Module) ->
            ?assertEqual(ok, rpc:call(Node, meck, unload, [Module]))
        end, as_list(Modules))
    end, as_list(Nodes)).

%%--------------------------------------------------------------------
%% @doc
%% Receives any message or returns immediately with an error.
%% @equiv receive_any(0)
%% @end
%%--------------------------------------------------------------------
-spec receive_any() -> {ok, ReceivedMsg :: term()} | {error, timeout}.
receive_any() ->
    receive_any(timer:seconds(0)).

%%--------------------------------------------------------------------
%% @doc
%% Receives any message or returns an error after timeout.
%% @equiv receive_any(0)
%% @end
%%--------------------------------------------------------------------
-spec receive_any(Timeout :: timeout()) ->
    {ok, ReceivedMsg :: term()} | {error, timeout}.
receive_any(Timeout) ->
    receive
        Msg -> {ok, Msg}
    after
        Timeout -> {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Receives given message or returns immediately with an error.
%% @equiv receive_msg(Msg, 0)
%% @end
%%--------------------------------------------------------------------
-spec receive_msg(Msg :: term()) ->
    {ok, ReceivedMsg :: term()} | {error, {timeout, UnreceivedMsg :: term()}}.
receive_msg(Msg) ->
    receive_msg(Msg, timer:seconds(0)).

%%--------------------------------------------------------------------
%% @doc
%% Receives given message or returns an error after timeout.
%% @end
%%--------------------------------------------------------------------
-spec receive_msg(Msg :: term(), Timeout :: timeout()) ->
    {ok, ReceivedMsg :: term()} | {error, {timeout, UnreceivedMsg :: term()}}.
receive_msg(Msg, Timeout) ->
    receive
        Msg -> {ok, Msg}
    after
        Timeout -> {error, {timeout, Msg}}
    end.

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