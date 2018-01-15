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

-module(oz_users_tests).

-ifdef(TEST).

-include("oz/oz_users.hrl").
-include("oz/oz_groups.hrl").
-include("oz/oz_spaces.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests description
%%%===================================================================

oz_users_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"get details", fun should_get_details/0},
            {"modify details", fun should_modify_details/0},
            {"merge account", fun should_merge_account/0},
            {"get create space token", fun should_get_create_space_token/0},
            {"get merge account token", fun should_get_merge_account_token/0},
            {"should create space", fun should_create_space/0},
            {"should join space", fun should_join_space/0},
            {"should leave space", fun should_leave_space/0},
            {"should get spaces", fun should_get_spaces/0},
            {"should get space details", fun should_get_space_details/0},
            {"should get default space", fun should_get_default_space/0},
            {"should set default space", fun should_set_default_space/0},
            {"should create group", fun should_create_group/0},
            {"should join group", fun should_join_group/0},
            {"should leave group", fun should_leave_group/0},
            {"should get groups", fun should_get_groups/0},
            {"should get group details", fun should_get_group_details/0}
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    meck:new(oz_endpoint),
    meck:expect(oz_endpoint, request, fun
        (client, "/user", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/user/merge/token", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/user/spaces", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/user/spaces/token", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/user/spaces/default", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/user/spaces/spaceId", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/user/spaces/spaceId", delete) ->
            {ok, 202, response_headers, response_body};
        (client, "/user/groups", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/user/groups/groupId", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/user/groups/groupId", delete) ->
            {ok, 202, response_headers, response_body}
    end),
    meck:expect(oz_endpoint, request, fun
        (client, "/user", patch, <<"body">>) ->
            {ok, 204, response_headers, response_body};
        (client, "/user/merge", post, <<"body">>) ->
            {ok, 201, response_headers, response_body};
        (client, "/user/spaces", post, <<"body">>) ->
            {ok, 201, #{<<"Location">> => <<"/spaces/spaceId">>},
                response_body};
        (client, "/user/spaces/default", put, <<"body">>) ->
            {ok, 204, response_headers, response_body};
        (client, "/user/spaces/join", post, <<"body">>) ->
            {ok, 201, #{<<"Location">> => <<"/user/spaces/spaceId">>},
                response_body};
        (client, "/user/groups", post, <<"body">>) ->
            {ok, 201, #{<<"Location">> => <<"/groups/groupId">>}, response_body};
        (client, "/user/groups/join", post, <<"body">>) ->
            {ok, 201, #{<<"Location">> => <<"/user/groups/groupId">>},
                response_body}
    end).


teardown(_) ->
    ?assert(meck:validate(oz_endpoint)),
    ok = meck:unload(oz_endpoint).

%%%===================================================================
%%% Tests functions
%%%===================================================================

should_get_details() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"userId">>, <<"userId">>}, {<<"name">>, <<"name">>}]
    end),

    Answer = oz_users:get_details(client),
    ?assertEqual({ok, #user_details{id = <<"userId">>,
        name = <<"name">>}}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_modify_details() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_users:modify_details(client, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_merge_account() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_users:merge_account(client, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_get_create_space_token() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"token">>, <<"token">>}]
    end),

    Answer = oz_users:get_create_space_token(client),
    ?assertEqual({ok, <<"token">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_get_merge_account_token() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"token">>, <<"token">>}]
    end),

    Answer = oz_users:get_merge_account_token(client),
    ?assertEqual({ok, <<"token">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_create_space() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_users:create_space(client, parameters),
    ?assertEqual({ok, <<"spaceId">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_join_space() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_users:join_space(client, parameters),
    ?assertEqual({ok, <<"spaceId">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_leave_space() ->
    Answer = oz_users:leave_space(client, <<"spaceId">>),
    ?assertEqual(ok, Answer).


should_get_spaces() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) -> [
        {<<"spaces">>, <<"spaces">>},
        {<<"default">>, <<"default">>}
    ]
    end),

    Answer = oz_users:get_spaces(client),
    ?assertEqual({ok, #user_spaces{
        ids = <<"spaces">>,
        default = <<"default">>}
    }, Answer),

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

    Answer = oz_users:get_space_details(client, <<"spaceId">>),
    ?assertEqual({ok, #space_details{id = <<"spaceId">>, name = <<"name">>,
        providers_supports = [{<<"providerId">>, 123}]}}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_get_default_space() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_users:set_default_space(client, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_set_default_space() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"spaceId">>, <<"spaceId">>}]
    end),

    Answer = oz_users:get_default_space(client),
    ?assertEqual({ok, <<"spaceId">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_create_group() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_users:create_group(client, parameters),
    ?assertEqual({ok, <<"groupId">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_join_group() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_users:join_group(client, parameters),
    ?assertEqual({ok, <<"groupId">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_leave_group() ->
    Answer = oz_users:leave_group(client, <<"groupId">>),
    ?assertEqual(ok, Answer).


should_get_groups() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"groups">>, <<"groups">>}]
    end),

    Answer = oz_users:get_groups(client),
    ?assertEqual({ok, <<"groups">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_get_group_details() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) -> [
        {<<"groupId">>, <<"groupId">>}, {<<"name">>, <<"name">>},
        {<<"type">>, <<"type">>}
    ] end),

    Answer = oz_users:get_group_details(client, <<"groupId">>),
    ?assertEqual({ok, #group_details{id = <<"groupId">>,
        name = <<"name">>, type = <<"type">>}}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).

-endif.
