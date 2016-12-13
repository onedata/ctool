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
-include("cth_common.hrl").

%% temporary directory for test files
-define(TEMP_DIR, "/tmp").

%% Returns absolute path to given file in the test data directory
-define(TEST_FILE(Config, X), filename:join(?config(data_dir, Config), X)).

-define(DEFAULT_ENV_DESCRIPTION, "env_desc.json").

-define(MOCK_MANAGER_NAME, mock_manager).

%% keys that can be added to Config to pass arguments to CT hooks
-define(ENV_DESCRIPTION, env_description).
-define(CTH_ENV_UP, cth_env_up).
-define(LOAD_MODULES, load_modules).
-define(ENV_UP_POSTHOOK, env_up_posthook).

%% values that can be added to Config to pass arguments to CT hooks
-define(DISABLE, disable). % used to disable cth_env_up


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
-define(config(Key, Config, Default), proplists:get_value(Key, Config, Default)).

%% Types used in tests
-type mock_opt() :: passthrough | non_strict | unstick | no_link | no_history.


-endif.