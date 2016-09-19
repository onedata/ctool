%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This file contains ct tests helper macros and definitions.
%%% @end
%%%--------------------------------------------------------------------
-ifndef(TEST_UTILS_HRL).
-define(TEST_UTILS_HRL, 1).

-include_lib("assertions.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% temporary directory for test files
-define(TEMP_DIR, "/tmp").

%% Returns absolute path to given file in the test data directory
-define(TEST_FILE(Config, X), filename:join(?config(data_dir, Config), X)).

%% Initializes and clears test environment
-define(TEST_INIT(Config, EnvDescription), ?TEST_INIT(Config, EnvDescription, [])).
-define(TEST_INIT(Config, EnvDescription, LoadModules),
    test_node_starter:prepare_test_environment(Config, EnvDescription, ?MODULE, LoadModules)
).
-define(TEST_STOP(Config),
    test_node_starter:clean_environment(Config, ?MODULE)
).
-define(TEST_STOP(Config, Apps),
    test_node_starter:clean_environment(Config, ?MODULE, Apps)
).

%% Logs cases start/stop
-define(CASE_START(Case), ct:print("Starting CASE ~p", [Case])).
-define(CASE_STOP(Case), ct:print("Stopping CASE ~p", [Case])).

%% Macro used to generate case name for default init/end_per_testcase
-define(DEFAULT_CASE(Case), list_to_atom(atom_to_list(Case) ++ "_default")).

%% Utility macros
-define(CURRENT_HOST, ?GET_HOSTNAME(node())).
-define(NODE(NodeHost, NodeName), list_to_atom(atom_to_list(NodeName) ++ "@" ++ atom_to_list(NodeHost))).
-define(GET_NODE_NAME(FullName), list_to_atom(hd(string:tokens(atom_to_list(FullName), "@")))).
-define(GET_HOSTNAME(FullName), list_to_atom(lists:last(string:tokens(atom_to_list(FullName), "@")))).
-define(GET_DOMAIN(FullName), list_to_atom(string:join(lists:sublist(string:tokens(atom_to_list(?GET_HOSTNAME(FullName)), "."), 2, 10), "."))).

-ifdef(config).
-undef(config).
-endif.

-define(config(Key, Config), proplists:get_value(Key, Config)).

-endif.