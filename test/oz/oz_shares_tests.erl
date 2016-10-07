%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module tests the functionality of oz_shares module.
%%% It contains unit tests that base on eunit.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_shares_tests).

-ifdef(TEST).

-include("oz/oz_shares.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests description
%%%===================================================================

oz_shares_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"create", fun should_create/0},
            {"remove", fun should_remove/0},
            {"get details", fun should_get_details/0},
            {"modify details", fun should_modify_details/0}
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    meck:new(oz_endpoint),
    meck:expect(oz_endpoint, auth_request, fun
        (client, "/shares/shareId", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/shares/shareId", delete) ->
            {ok, 202, response_headers, response_body}
    end),
    meck:expect(oz_endpoint, auth_request, fun
        (client, "/spaces/spaceId/shares/shareId", put, <<"body">>) ->
            {ok, 204, [], response_body};
        (client, "/shares/shareId", patch, <<"body">>) ->
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

    Answer = oz_shares:create(client, <<"shareId">>, <<"spaceId">>, parameters),
    ?assertEqual({ok, <<"shareId">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_remove() ->
    Answer = oz_shares:remove(client, <<"shareId">>),
    ?assertEqual(ok, Answer).


should_get_details() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [
            {<<"shareId">>, <<"shareId">>},
            {<<"name">>, <<"val_name">>},
            {<<"publicUrl">>, <<"val_public_url">>},
            {<<"rootFileId">>, <<"val_root_file_id">>},
            {<<"spaceId">>, <<"val_parent_space">>}
        ]
    end),

    Answer = oz_shares:get_details(client, <<"shareId">>),
    ?assertEqual({ok, #share_details{
        id = <<"shareId">>,
        name = <<"val_name">>,
        space = <<"val_parent_space">>,
        root_file = <<"val_root_file_id">>,
        public_url = <<"val_public_url">>
    }}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_modify_details() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_shares:modify_details(client, <<"shareId">>, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).

-endif.
