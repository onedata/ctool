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

%% Initializes test environment
-define(TEST_INIT(Config, EnvDescription),
    test_node_starter:prepare_test_environment(Config, EnvDescription, ?MODULE)
).

%% Utility macros
-define(CURRENT_HOST, list_to_atom(lists:last(string:tokens(atom_to_list(node()), "@")))).
-define(NODE(NodeHost, NodeName), list_to_atom(atom_to_list(NodeName) ++ "@" ++ atom_to_list(NodeHost))).
-define(GET_NODE_NAME(FullName), list_to_atom(hd(string:tokens(atom_to_list(FullName), "@")))).

-endif.