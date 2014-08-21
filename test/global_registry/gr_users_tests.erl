%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module tests the functionality of gr_groups module.
%% It contains unit tests that base on eunit.
%% @end
%% ===================================================================

-module(gr_users_tests).

-ifdef(TEST).

-include("global_registry/gr_users.hrl").
-include("global_registry/gr_groups.hrl").
-include("global_registry/gr_spaces.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests description
%% ===================================================================

gr_groups_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"get info", fun should_get_info/0},
            {"modify info", fun should_modify_info/0},
            {"merge account", fun should_merge_account/0},
            {"get create space token", fun should_get_create_space_token/0},
            {"get merge account token", fun should_get_merge_account_token/0},
            {"should create space", fun should_create_space/0},
            {"should join space", fun should_join_space/0},
            {"should leave space", fun should_leave_space/0},
            {"should get spaces", fun should_get_spaces/0},
            {"should get space info", fun should_get_space_info/0},
            {"should create group", fun should_create_group/0},
            {"should join group", fun should_join_group/0},
            {"should leave group", fun should_leave_group/0},
            {"should get groups", fun should_get_groups/0},
            {"should get group info", fun should_get_group_info/0}
        ]
    }.

%% ===================================================================
%% Setup/teardown functions
%% ===================================================================

setup() ->
    meck:new(gr_endpoint),
    meck:expect(gr_endpoint, request, fun
        (client, "/user", get) -> {ok, "200", response_headers, response_body};
        (client, "/user/merge/token", get) -> {ok, "200", response_headers, response_body};
        (client, "/user/spaces", get) -> {ok, "200", response_headers, response_body};
        (client, "/user/spaces/token", get) -> {ok, "200", response_headers, response_body};
        (client, "/user/spaces/spaceId", get) -> {ok, "200", response_headers, response_body};
        (client, "/user/spaces/spaceId", delete) -> {ok, "204", response_headers, response_body};
        (client, "/user/groups", get) -> {ok, "200", response_headers, response_body};
        (client, "/user/groups/groupId", get) -> {ok, "200", response_headers, response_body};
        (client, "/user/groups/groupId", delete) -> {ok, "204", response_headers, response_body}
    end),
    meck:expect(gr_endpoint, request, fun
        (client, "/user", patch, <<"body">>) -> {ok, "200", response_headers, response_body};
        (client, "/user/merge", post, <<"body">>) -> {ok, "201", response_headers, response_body};
        (client, "/user/spaces", post, <<"body">>) -> {ok, "201", [{"location", "/spaces/spaceId"}], response_body};
        (client, "/user/spaces/join", post, <<"body">>) -> {ok, "201", [{"location", "/user/spaces/spaceId"}], response_body};
        (client, "/user/groups", post, <<"body">>) -> {ok, "201", [{"location", "/groups/groupId"}], response_body};
        (client, "/user/groups/join", post, <<"body">>) -> {ok, "201", [{"location", "/user/groups/groupId"}], response_body}
    end).

teardown(_) ->
    ?assert(meck:validate(gr_endpoint)),
    ok = meck:unload(gr_endpoint).

%% ===================================================================
%% Tests functions
%% ===================================================================

should_get_info() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"userId">>, <<"userId">>}, {<<"name">>, <<"name">>}]
    end),

    Answer = gr_users:get_info(client),
    ?assertEqual({ok, #user_info{id = <<"userId">>, name = <<"name">>}}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_modify_info() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_users:modify_info(client, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_merge_account() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_users:merge_account(client, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_create_space_token() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"token">>, <<"token">>}]
    end),

    Answer = gr_users:get_create_space_token(client),
    ?assertEqual({ok, <<"token">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_merge_account_token() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"token">>, <<"token">>}]
    end),

    Answer = gr_users:get_merge_account_token(client),
    ?assertEqual({ok, <<"token">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_create_space() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_users:create_space(client, parameters),
    ?assertEqual({ok, <<"spaceId">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_join_space() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_users:join_space(client, parameters),
    ?assertEqual({ok, <<"spaceId">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_leave_space() ->
    Answer = gr_users:leave_space(client, <<"spaceId">>),
    ?assertEqual(ok, Answer).


should_get_spaces() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"spaces">>, <<"spaces">>}]
    end),

    Answer = gr_users:get_spaces(client),
    ?assertEqual({ok, <<"spaces">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_space_info() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"spaceId">>, <<"spaceId">>}, {<<"name">>, <<"name">>}]
    end),

    Answer = gr_users:get_space_info(client, <<"spaceId">>),
    ?assertEqual({ok, #space_info{id = <<"spaceId">>, name = <<"name">>}}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_create_group() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_users:create_group(client, parameters),
    ?assertEqual({ok, <<"groupId">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_join_group() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_users:join_group(client, parameters),
    ?assertEqual({ok, <<"groupId">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_leave_group() ->
    Answer = gr_users:leave_group(client, <<"groupId">>),
    ?assertEqual(ok, Answer).


should_get_groups() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"groups">>, <<"groups">>}]
    end),

    Answer = gr_users:get_groups(client),
    ?assertEqual({ok, <<"groups">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_group_info() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"groupId">>, <<"groupId">>}, {<<"name">>, <<"name">>}]
    end),

    Answer = gr_users:get_group_info(client, <<"groupId">>),
    ?assertEqual({ok, #group_info{id = <<"groupId">>, name = <<"name">>}}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).

-endif.
