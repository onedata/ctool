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

-module(gr_groups_tests).

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
            {"create", fun should_create/0},
            {"remove", fun should_remove/0},
            {"get details", fun should_get_details/0},
            {"modify details", fun should_modify_details/0},
            {"get create space token", fun should_get_create_space_token/0},
            {"get invite user token", fun should_get_invite_user_token/0},
            {"remove user", fun should_remove_user/0},
            {"get users", fun should_get_users/0},
            {"get user details", fun should_get_user_details/0},
            {"should get user privileges", fun should_get_user_privileges/0},
            {"should set user privileges", fun should_set_user_privileges/0},
            {"should create space", fun should_create_space/0},
            {"should join space", fun should_join_space/0},
            {"should leave space", fun should_leave_space/0},
            {"should get spaces", fun should_get_spaces/0},
            {"should get space details", fun should_get_space_details/0}
        ]
    }.

%% ===================================================================
%% Setup/teardown functions
%% ===================================================================

setup() ->
    meck:new(gr_endpoint),
    meck:expect(gr_endpoint, auth_request, fun
        (client, "/groups/groupId", get) -> {ok, "200", response_headers, response_body};
        (client, "/groups/groupId", delete) -> {ok, "204", response_headers, response_body};
        (client, "/groups/groupId/spaces", get) -> {ok, "200", response_headers, response_body};
        (client, "/groups/groupId/spaces/spaceId", get) -> {ok, "200", response_headers, response_body};
        (client, "/groups/groupId/spaces/token", get) -> {ok, "200", response_headers, response_body};
        (client, "/groups/groupId/spaces/spaceId", delete) -> {ok, "204", response_headers, response_body};
        (client, "/groups/groupId/users", get) -> {ok, "200", response_headers, response_body};
        (client, "/groups/groupId/users/token", get) -> {ok, "200", response_headers, response_body};
        (client, "/groups/groupId/users/userId", get) -> {ok, "200", response_headers, response_body};
        (client, "/groups/groupId/users/userId", delete) -> {ok, "204", response_headers, response_body};
        (client, "/groups/groupId/users/userId/privileges", get) -> {ok, "200", response_headers, response_body}
    end),
    meck:expect(gr_endpoint, auth_request, fun
        (client, "/groups", post, <<"body">>) -> {ok, "201", [{"location", "/groups/groupId"}], response_body};
        (client, "/groups/groupId", patch, <<"body">>) -> {ok, "204", response_headers, response_body};
        (client, "/groups/groupId/spaces", post, <<"body">>) -> {ok, "201", [{"location", "/spaces/spaceId"}], response_body};
        (client, "/groups/groupId/spaces/join", post, <<"body">>) -> {ok, "201", [{"location", "/groups/groupId/spaces/spaceId"}], response_body};
        (client, "/groups/groupId/users/userId/privileges", put, <<"body">>) -> {ok, "204", response_headers, response_body}
    end).


teardown(_) ->
    ?assert(meck:validate(gr_endpoint)),
    ok = meck:unload(gr_endpoint).

%% ===================================================================
%% Tests functions
%% ===================================================================

should_create() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_groups:create(client, parameters),
    ?assertEqual({ok, <<"groupId">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_remove() ->
    Answer = gr_groups:remove(client, <<"groupId">>),
    ?assertEqual(ok, Answer).


should_get_details() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"groupId">>, <<"groupId">>}, {<<"name">>, <<"name">>}]
    end),

    Answer = gr_groups:get_details(client, <<"groupId">>),
    ?assertEqual({ok, #group_details{id = <<"groupId">>, name = <<"name">>}}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_modify_details() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_groups:modify_details(client, <<"groupId">>, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_create_space_token() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"token">>, <<"token">>}]
    end),

    Answer = gr_groups:get_create_space_token(client, <<"groupId">>),
    ?assertEqual({ok, <<"token">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_invite_user_token() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"token">>, <<"token">>}]
    end),

    Answer = gr_groups:get_invite_user_token(client, <<"groupId">>),
    ?assertEqual({ok, <<"token">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_remove_user() ->
    Answer = gr_groups:remove_user(client, <<"groupId">>, <<"userId">>),
    ?assertEqual(ok, Answer).


should_get_users() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"users">>, <<"users">>}]
    end),

    Answer = gr_groups:get_users(client, <<"groupId">>),
    ?assertEqual({ok, <<"users">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_user_details() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"userId">>, <<"userId">>}, {<<"name">>, <<"name">>}]
    end),

    Answer = gr_groups:get_user_details(client, <<"groupId">>, <<"userId">>),
    ?assertEqual({ok, #user_details{id = <<"userId">>, name = <<"name">>}}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_user_privileges() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"privileges">>, <<"privileges">>}]
    end),

    Answer = gr_groups:get_user_privileges(client, <<"groupId">>, <<"userId">>),
    ?assertEqual({ok, <<"privileges">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_set_user_privileges() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_groups:set_user_privileges(client, <<"groupId">>, <<"userId">>, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_create_space() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_groups:create_space(client, <<"groupId">>, parameters),
    ?assertEqual({ok, <<"spaceId">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_join_space() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_groups:join_space(client, <<"groupId">>, parameters),
    ?assertEqual({ok, <<"spaceId">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_leave_space() ->
    Answer = gr_groups:leave_space(client, <<"groupId">>, <<"spaceId">>),
    ?assertEqual(ok, Answer).


should_get_spaces() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"spaces">>, <<"spaces">>}]
    end),

    Answer = gr_groups:get_spaces(client, <<"groupId">>),
    ?assertEqual({ok, <<"spaces">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_space_details() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"spaceId">>, <<"spaceId">>}, {<<"name">>, <<"name">>}]
    end),

    Answer = gr_groups:get_space_details(client, <<"groupId">>, <<"spaceId">>),
    ?assertEqual({ok, #space_details{id = <<"spaceId">>, name = <<"name">>}}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).

-endif.
