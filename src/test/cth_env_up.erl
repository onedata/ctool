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
-export([pre_end_per_suite/3]).
%% posthooks
-export([post_init_per_suite/4]).

-include("test/test_utils.hrl").


%%--------------------------------------------------------------------
%% @doc
%% CTH callback called when hook is being installed.
%% Initializes logger state.
%% @end
%%--------------------------------------------------------------------
-spec init(_Id :: term(), _Opts :: term()) -> {ok, []}.
init(_Id, _Opts) ->
    {ok, []}.


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
    State :: []) -> {[term()], []}.
post_init_per_suite(Suite, _Config, Return, State) ->
    Config2 = add_env_description_file(Return),
    ct:pal("Environment initialization in ~p", [Suite]),
    NewConfig = test_node_starter:prepare_test_environment(Config2, Suite),
    NewConfig2 = maybe_exec_posthook(NewConfig),
    {NewConfig2, State}.

%%--------------------------------------------------------------------
%% @doc
%% CTH callback called after end_per_suite.
%% Cleans environment used in given test suite.
%% @end
%%--------------------------------------------------------------------
-spec pre_end_per_suite(Suite :: atom(), Config :: [term()], State :: []) -> {[term()], []}.
pre_end_per_suite(Suite, Config, State) ->
    ct:pal("Environment cleaning in ~p", [Suite]),
    test_node_starter:clean_environment(Config),
    {Config, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds default environment description file to Config it it's not defined.
%% @end
%%--------------------------------------------------------------------
-spec add_env_description_file(Config :: [term()]) -> [term()].
add_env_description_file(Config) ->
    case ?config(?ENV_DESCRIPTION, Config) of
        undefined -> [{?ENV_DESCRIPTION, ?DEFAULT_ENV_DESCRIPTION} | Config];
        _ -> Config
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes posthook passed in Config if it exists.
%% @end
%%--------------------------------------------------------------------
-spec maybe_exec_posthook(Config :: [term()]) -> [term()].
maybe_exec_posthook(Config) ->
    case ?config(?ENV_UP_POSTHOOK, Config) of
        undefined -> Config;
        Posthook -> Posthook(Config)
    end.
