%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% CT hook responsible for starting test environment.
%%% @end
%%%-------------------------------------------------------------------
-module(cth_onenv_up).
-author("Michal Stanisz").

%% API
%% CTH callback
%% initialization
-export([init/2]).
%% prehooks
-export([pre_init_per_suite/3]).
%% posthooks
-export([post_init_per_suite/4, post_end_per_suite/4]).

-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").

-record(state, {}).
-type state() :: #state{}.

%%--------------------------------------------------------------------
%% @doc
%% CTH callback called when hook is being installed.
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
    ok = test_utils:load_utility_modules(Config),
    {Config, State}.


%%--------------------------------------------------------------------
%% @doc
%% CTH callback called after `init_per_suite`. Starts test environment.
%% Name of test environment yaml file should be provided in Config 
%% by calling test_config:set_onenv_scenario/2 in `init_per_suite`. 
%% Given file must exist in test_distributed/onenv_scenarios.
%% If you intend to perform some initialization after environment is up,
%% pass fun(Config) -> ... end to test_config:set_posthook/2 in `init_per_suite`.
%%
%% Config returned from `init_per_suite` is stored in Return variable.
%% @end
%%--------------------------------------------------------------------
-spec post_init_per_suite(Suite :: atom(), test_config:config(), Return :: test_config:config(),
    State :: state()) -> {test_config:config(), state()}.
post_init_per_suite(Suite, _, Return, State) ->
    ct:pal("Environment initialization in ~p", [Suite]),
    NewConfig = test_onenv_starter:prepare_test_environment(Return),
    {NewConfig, State}.


%%--------------------------------------------------------------------
%% @doc
%% CTH callback called after end_per_suite.
%% Cleans environment used in given test suite.
%% @end
%%--------------------------------------------------------------------
-spec post_end_per_suite(Suite :: atom(), Config :: test_config:config(), Return :: term(),
    State :: state()) -> {[term()], state()}.
post_end_per_suite(Suite, Config, Return, State) ->
    ct:pal("Environment cleaning in ~p", [Suite]),
    test_onenv_starter:clean_environment(Config),
    {Return, State}.

