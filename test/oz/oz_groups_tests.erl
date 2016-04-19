%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module tests the functionality of oz_groups module.
%%% It contains unit tests that base on eunit.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_groups_tests).

-ifdef(TEST).

-include("oz/oz_users.hrl").
-include("oz/oz_groups.hrl").
-include("oz/oz_spaces.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests description
%%%===================================================================

oz_groups_test_() ->
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

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    meck:new(oz_endpoint),
    meck:expect(oz_endpoint, auth_request, fun
        (client, "/groups/groupId", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/groups/groupId", delete) ->
            {ok, 202, response_headers, response_body};
        (client, "/groups/groupId/spaces", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/groups/groupId/spaces/spaceId", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/groups/groupId/spaces/token", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/groups/groupId/spaces/spaceId", delete) ->
            {ok, 202, response_headers, response_body};
        (client, "/groups/groupId/users", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/groups/groupId/users/token", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/groups/groupId/users/userId", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/groups/groupId/users/userId", delete) ->
            {ok, 202, response_headers, response_body};
        (client, "/groups/groupId/users/userId/privileges", get) ->
            {ok, 200, response_headers, response_body}
    end),
    meck:expect(oz_endpoint, auth_request, fun
        (client, "/groups", post, <<"body">>) ->
            {ok, 201, [{<<"location">>, <<"/groups/groupId">>}], response_body};
        (client, "/groups/groupId", patch, <<"body">>) ->
            {ok, 204, response_headers, response_body};
        (client, "/groups/groupId/spaces", post, <<"body">>) ->
            {ok, 201, [{<<"location">>, <<"/spaces/spaceId">>}], response_body};
        (client, "/groups/groupId/spaces/join", post, <<"body">>) ->
            {ok, 201, [{<<"location">>, <<"/groups/groupId/spaces/spaceId">>}], response_body};
        (client, "/groups/groupId/users/userId/privileges", put, <<"body">>) ->
            {ok, 204, response_headers, response_body}
    end).


teardown(_) ->
    ?assert(meck:validate(oz_endpoint)),
    ok = meck:unload(oz_endpoint).

%%%===================================================================
%%% Tests functions
%%%===================================================================

should_create() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_groups:create(client, parameters),
    ?assertEqual({ok, <<"groupId">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_remove() ->
    Answer = oz_groups:remove(client, <<"groupId">>),
    ?assertEqual(ok, Answer).


should_get_details() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"groupId">>, <<"groupId">>}, {<<"name">>, <<"name">>}]
    end),

    Answer = oz_groups:get_details(client, <<"groupId">>),
    ?assertEqual({ok, #group_details{id = <<"groupId">>,
        name = <<"name">>}}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_modify_details() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_groups:modify_details(client, <<"groupId">>, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_get_create_space_token() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"token">>, <<"token">>}]
    end),

    Answer = oz_groups:get_create_space_token(client, <<"groupId">>),
    ?assertEqual({ok, <<"token">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_get_invite_user_token() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"token">>, <<"token">>}]
    end),

    Answer = oz_groups:get_invite_user_token(client, <<"groupId">>),
    ?assertEqual({ok, <<"token">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_remove_user() ->
    Answer = oz_groups:remove_user(client, <<"groupId">>, <<"userId">>),
    ?assertEqual(ok, Answer).


should_get_users() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"users">>, <<"users">>}]
    end),

    Answer = oz_groups:get_users(client, <<"groupId">>),
    ?assertEqual({ok, <<"users">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_get_user_details() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"userId">>, <<"userId">>}, {<<"name">>, <<"name">>}]
    end),

    Answer = oz_groups:get_user_details(client, <<"groupId">>, <<"userId">>),
    ?assertEqual({ok, #user_details{id = <<"userId">>,
        name = <<"name">>}}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_get_user_privileges() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"privileges">>, <<"privileges">>}]
    end),

    Answer = oz_groups:get_user_privileges(client, <<"groupId">>, <<"userId">>),
    ?assertEqual({ok, <<"privileges">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_set_user_privileges() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_groups:set_user_privileges(client,
        <<"groupId">>, <<"userId">>, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_create_space() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_groups:create_space(client, <<"groupId">>, parameters),
    ?assertEqual({ok, <<"spaceId">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_join_space() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_groups:join_space(client, <<"groupId">>, parameters),
    ?assertEqual({ok, <<"spaceId">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_leave_space() ->
    Answer = oz_groups:leave_space(client, <<"groupId">>, <<"spaceId">>),
    ?assertEqual(ok, Answer).


should_get_spaces() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"spaces">>, <<"spaces">>}]
    end),

    Answer = oz_groups:get_spaces(client, <<"groupId">>),
    ?assertEqual({ok, <<"spaces">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_get_space_details() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [
            {<<"spaceId">>, <<"spaceId">>},
            {<<"name">>, <<"name">>},
            {<<"providersSupports">>, [{<<"providerId">>, 123}]}
        ]
    end),

    Answer = oz_groups:get_space_details(client, <<"groupId">>, <<"spaceId">>),
    ?assertEqual({ok, #space_details{id = <<"spaceId">>, name = <<"name">>,
        providers_supports = [{<<"providerId">>, 123}]}}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).

-endif.
