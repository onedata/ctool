%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module tests the functionality of gr_spaces module.
%% It contains unit tests that base on eunit.
%% @end
%% ===================================================================

-module(gr_spaces_tests).

-ifdef(TEST).

-include("global_registry/gr_users.hrl").
-include("global_registry/gr_groups.hrl").
-include("global_registry/gr_spaces.hrl").
-include("global_registry/gr_providers.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests description
%% ===================================================================

gr_spaces_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"create", fun should_create/0},
            {"remove", fun should_remove/0},
            {"get details", fun should_get_details/0},
            {"modify details", fun should_modify_details/0},
            {"get invite user token", fun should_get_invite_user_token/0},
            {"get invite group token", fun should_get_invite_group_token/0},
            {"get invite provider token", fun should_get_invite_provider_token/0},
            {"remove user", fun should_remove_user/0},
            {"get users", fun should_get_users/0},
            {"get user details", fun should_get_user_details/0},
            {"get user privileges", fun should_get_user_privileges/0},
            {"set user privileges", fun should_set_user_privileges/0},
            {"remove group", fun should_remove_group/0},
            {"get groups", fun should_get_groups/0},
            {"get group details", fun should_get_group_details/0},
            {"get group privileges", fun should_get_group_privileges/0},
            {"set group privileges", fun should_set_group_privileges/0},
            {"remove provider", fun should_remove_provider/0},
            {"get providers", fun should_get_providers/0},
            {"get provider details", fun should_get_provider_details/0}
        ]
    }.

%% ===================================================================
%% Setup/teardown functions
%% ===================================================================

setup() ->
    meck:new(gr_endpoint),
    meck:expect(gr_endpoint, secure_request, fun
        (client, "/spaces/spaceId", get) -> {ok, "200", response_headers, response_body};
        (client, "/spaces/spaceId", delete) -> {ok, "204", response_headers, response_body};
        (client, "/spaces/spaceId/users", get) -> {ok, "200", response_headers, response_body};
        (client, "/spaces/spaceId/users/token", get) -> {ok, "200", response_headers, response_body};
        (client, "/spaces/spaceId/users/userId", get) -> {ok, "200", response_headers, response_body};
        (client, "/spaces/spaceId/users/userId", delete) -> {ok, "204", response_headers, response_body};
        (client, "/spaces/spaceId/users/userId/privileges", get) -> {ok, "200", response_headers, response_body};
        (client, "/spaces/spaceId/groups", get) -> {ok, "200", response_headers, response_body};
        (client, "/spaces/spaceId/groups/token", get) -> {ok, "200", response_headers, response_body};
        (client, "/spaces/spaceId/groups/groupId", get) -> {ok, "200", response_headers, response_body};
        (client, "/spaces/spaceId/groups/groupId", delete) -> {ok, "204", response_headers, response_body};
        (client, "/spaces/spaceId/groups/groupId/privileges", get) -> {ok, "200", response_headers, response_body};
        (client, "/spaces/spaceId/providers", get) -> {ok, "200", response_headers, response_body};
        (client, "/spaces/spaceId/providers/token", get) -> {ok, "200", response_headers, response_body};
        (client, "/spaces/spaceId/providers/providerId", get) -> {ok, "200", response_headers, response_body};
        (client, "/spaces/spaceId/providers/providerId", delete) -> {ok, "204", response_headers, response_body}
    end),
    meck:expect(gr_endpoint, secure_request, fun
        (client, "/spaces", post, <<"body">>) -> {ok, "201", [{"location", "/spaces/spaceId"}], response_body};
        (client, "/spaces/spaceId", patch, <<"body">>) -> {ok, "204", response_headers, response_body};
        (client, "/spaces/spaceId/users/userId/privileges", put, <<"body">>) -> {ok, "204", response_headers, response_body};
        (client, "/spaces/spaceId/groups/groupId/privileges", put, <<"body">>) -> {ok, "204", response_headers, response_body}
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

    Answer = gr_spaces:create(client, parameters),
    ?assertEqual({ok, <<"spaceId">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_remove() ->
    Answer = gr_spaces:remove(client, <<"spaceId">>),
    ?assertEqual(ok, Answer).


should_get_details() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"spaceId">>, <<"spaceId">>}, {<<"name">>, <<"name">>}]
    end),

    Answer = gr_spaces:get_details(client, <<"spaceId">>),
    ?assertEqual({ok, #space_details{id = <<"spaceId">>, name = <<"name">>}}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_modify_details() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_spaces:modify_details(client, <<"spaceId">>, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_invite_user_token() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"token">>, <<"token">>}]
    end),

    Answer = gr_spaces:get_invite_user_token(client, <<"spaceId">>),
    ?assertEqual({ok, <<"token">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_invite_group_token() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"token">>, <<"token">>}]
    end),

    Answer = gr_spaces:get_invite_group_token(client, <<"spaceId">>),
    ?assertEqual({ok, <<"token">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_invite_provider_token() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"token">>, <<"token">>}]
    end),

    Answer = gr_spaces:get_invite_provider_token(client, <<"spaceId">>),
    ?assertEqual({ok, <<"token">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_remove_user() ->
    Answer = gr_spaces:remove_user(client, <<"spaceId">>, <<"userId">>),
    ?assertEqual(ok, Answer).


should_get_users() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"users">>, <<"users">>}]
    end),

    Answer = gr_spaces:get_users(client, <<"spaceId">>),
    ?assertEqual({ok, <<"users">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_user_details() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"userId">>, <<"userId">>}, {<<"name">>, <<"name">>}]
    end),

    Answer = gr_spaces:get_user_details(client, <<"spaceId">>, <<"userId">>),
    ?assertEqual({ok, #user_details{id = <<"userId">>, name = <<"name">>}}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_user_privileges() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"privileges">>, <<"privileges">>}]
    end),

    Answer = gr_spaces:get_user_privileges(client, <<"spaceId">>, <<"userId">>),
    ?assertEqual({ok, <<"privileges">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).

should_set_user_privileges() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_spaces:set_user_privileges(client, <<"spaceId">>, <<"userId">>, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_remove_group() ->
    Answer = gr_spaces:remove_group(client, <<"spaceId">>, <<"groupId">>),
    ?assertEqual(ok, Answer).


should_get_groups() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"groups">>, <<"groups">>}]
    end),

    Answer = gr_spaces:get_groups(client, <<"spaceId">>),
    ?assertEqual({ok, <<"groups">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_group_details() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"groupId">>, <<"groupId">>}, {<<"name">>, <<"name">>}]
    end),

    Answer = gr_spaces:get_group_details(client, <<"spaceId">>, <<"groupId">>),
    ?assertEqual({ok, #group_details{id = <<"groupId">>, name = <<"name">>}}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_group_privileges() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"privileges">>, <<"privileges">>}]
    end),

    Answer = gr_spaces:get_group_privileges(client, <<"spaceId">>, <<"groupId">>),
    ?assertEqual({ok, <<"privileges">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).

should_set_group_privileges() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),

    Answer = gr_spaces:set_group_privileges(client, <<"spaceId">>, <<"groupId">>, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_remove_provider() ->
    Answer = gr_spaces:remove_provider(client, <<"spaceId">>, <<"providerId">>),
    ?assertEqual(ok, Answer).


should_get_providers() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"providers">>, <<"providers">>}]
    end),

    Answer = gr_spaces:get_providers(client, <<"spaceId">>),
    ?assertEqual({ok, <<"providers">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_provider_details() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [
            {<<"providerId">>, <<"providerId">>},
            {<<"urls">>, <<"urls">>},
            {<<"redirectionPoint">>, <<"redirectionPoint">>}
        ]
    end),

    Answer = gr_spaces:get_provider_details(client, <<"spaceId">>, <<"providerId">>),
    ?assertEqual({ok, #provider_details{
        id = <<"providerId">>,
        urls = <<"urls">>,
        redirection_point = <<"redirectionPoint">>}
    }, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).

-endif.
