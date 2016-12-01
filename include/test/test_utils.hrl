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

-define(DEFAULT_ENV_DESCRIPTION, "env_desc.json").

-define(ENV_DESCRIPTION, env_description).
-define(LOAD_MODULES, load_modules).
-define(CONFIGURE(EnvDescriptionFile, Config), ?CONFIGURE(EnvDescriptionFile, [], Config)).
-define(CONFIGURE(EnvDescriptionFile, ModulesToLoad, Config),
    [{?ENV_DESCRIPTION, EnvDescriptionFile}, {?LOAD_MODULES, ModulesToLoad} | Config]
).


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

-endif.