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
-include("../logging.hrl").

%% temporary directory for test files
-define(TEMP_DIR, "/tmp").

%% Returns absolute path to given file in the test data directory
-define(TEST_FILE(Config, Path), filename:join(test_utils:data_dir(Config), Path)).

-define(TEST_RELEASE_ETC_DIR(Path), filename:join("/root/bin/node/etc", Path)).

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
-define(GET_DOMAIN_BIN(FullName), list_to_binary(string:join(lists:sublist(string:tokens(atom_to_list(?GET_HOSTNAME(FullName)), "."), 2, 10), "."))).

-ifdef(config).
-undef(config).
-endif.

-define(config(Key, Config), proplists:get_value(Key, Config)).
-define(config(Key, Config, Default), proplists:get_value(Key, Config, Default)).

%% Types used in tests
-type mock_opt() :: passthrough | non_strict | unstick | no_link | no_history.


-define(RAND_ELEMENT(List), lists_utils:random_element(List)).
-define(RAND_STR(), ?RAND_STR(16)).
-define(RAND_STR(Size), string:slice(str_utils:rand_hex(Size), 0, Size)).
-define(RAND_BOOL(), ?RAND_ELEMENT([true, false])).
-define(RAND_INT(To), ?RAND_INT(0, To)).
-define(RAND_INT(From, To), From + rand:uniform(To - From + 1) - 1).
-define(RAND_FLOAT(From, To), From + rand:uniform() * (To - From)).
-define(RAND_JSON_TERM(), ?RAND_ELEMENT([
    ?RAND_BOOL(), ?RAND_STR(), ?RAND_INT(-1000, 1000), -27.8, 17.75,
    [1, 2, 3], #{<<"a">> => <<"B">>}, [#{<<"a">> => <<"B">>}, #{<<"c">> => <<"D">>}]
])).
-define(RAND_SUBLIST(List), lists_utils:random_sublist(List)).
-define(RAND_SUBLIST(List, MinLength, MaxLength), lists_utils:random_sublist(List, MinLength, MaxLength)).
-define(RAND_SUBMAP(Map), maps:with(lists_utils:random_sublist(maps:keys(Map)), Map)).

-define(TOO_LONG_NAME, <<
    "very_very_very_long_name_with_at_least_128_characters_yes_indeed_more_"
    "than_one_hundred_twenty_eight_characters_that_should_not_be_allowed"
>>).

-endif.