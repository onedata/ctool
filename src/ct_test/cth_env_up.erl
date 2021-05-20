%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% CT hook responsible for starting test environment.
%%% @end
%%%-------------------------------------------------------------------
-module(cth_env_up).
-author("Jakub Kudzia").

%% API
%% CTH callback
%% initialization
-export([init/2]).
%% prehooks
-export([pre_init_per_suite/3]).
%% posthooks
-export([post_init_per_suite/4, post_end_per_suite/4]).

-include("test/test_utils.hrl").

-record(state, {disabled = false}).
-type state() :: #state{}.

%%--------------------------------------------------------------------
%% @doc
%% CTH callback called when hook is being installed.
%% Initializes logger state.
%% @end
%%--------------------------------------------------------------------
-spec init(_Id :: term(), _Opts :: term()) -> {ok, state(), non_neg_integer()}.
init(_Id, _Opts) ->
    node_cache:init(),
    {ok, #state{}, ?CTH_ENV_UP_PRIORITY}.


%%--------------------------------------------------------------------
%% @doc
%% CTH callback called before init_per_suite.
%% Loads utility test modules.
%% @end
%%--------------------------------------------------------------------
-spec pre_init_per_suite(Suite :: atom(), _Config :: [term()], State :: state()) ->
    {[term()], state()}.
pre_init_per_suite(_Suite, Config, State) ->
    try
        ok = test_utils:load_utility_modules(Config),
        {Config, State}
    catch Class:Reason ->
        ct:print("Failed to load modules during init_per_suite - ~w:~p~nStacktrace: ~s", [
            Class, Reason, lager:pr_stacktrace(erlang:get_stacktrace())
        ]),
        error(failed_to_load_modules)
    end.


%%--------------------------------------------------------------------
%% @doc
%% CTH callback called after init_per_suite.
%% Starts test environment.
%% By default starts environment described in {test}_SUITE_data/?DEFAULT_ENV_DESCRIPTION.
%% You can pass custom environment description file by adding
%% {?ENV_DESCRIPTION, "{your_env_file}.json"} to Config returned by init_per_suite.
%% "{your_env_file}.json" must be present in {test}_SUITE_data.
%% If you intend to perform some initialization after environment is up,
%% pass fun(Config) -> ...end under key ?ENV_UP_POSTHOOK to Config.
%%
%% @end
%%--------------------------------------------------------------------
-spec post_init_per_suite(Suite :: atom(), _Config :: [term()], Return :: [term()],
    State :: state()) -> {[term()], state()}.
post_init_per_suite(Suite, _Config, Return, State) ->
    case ?config(?CTH_ENV_UP, Return) of
        ?DISABLE ->
            {Return, State#state{disabled = true}};
        _ ->
            ct:pal("Environment initialization in ~p", [Suite]),
            NewConfig = test_node_starter:prepare_test_environment(Return, Suite),
            {NewConfig, State}
    end.


%%--------------------------------------------------------------------
%% @doc
%% CTH callback called after end_per_suite.
%% Cleans environment used in given test suite.
%% @end
%%--------------------------------------------------------------------
-spec post_end_per_suite(Suite :: atom(), Config :: [term()], Return :: term(),
    State :: state()) -> {[term()], state()}.
post_end_per_suite(_Suite, _Config, Return, State = #state{disabled = true}) ->
    {Return, State};
post_end_per_suite(Suite, Config, Return, State) ->
    ct:pal("Environment cleaning in ~p", [Suite]),
    test_node_starter:clean_environment(Config),
    {Return, State}.

