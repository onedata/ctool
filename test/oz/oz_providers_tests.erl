%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module tests the functionality of oz_providers module.
%%% It contains unit tests that base on eunit.
%%% @end
%%%-------------------------------------------------------------------

-module(oz_providers_tests).

-ifdef(TEST).

-include("oz/oz_spaces.hrl").
-include("oz/oz_providers.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests description
%%%===================================================================

oz_providers_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"register", fun should_register/0},
            {"unregister", fun should_unregister/0},
            {"get details", fun should_get_details/0},
            {"get details for given provider",
                fun should_get_details_for_given_provider/0},
            {"modify details", fun should_modify_details/0},
            {"check ip address", fun should_check_ip_address/0},
            {"check GUI port", fun should_check_gui_port/0},
            {"check REST port", fun should_check_rest_port/0},
            {"create space", fun should_create_space/0},
            {"support space", fun should_support_space/0},
            {"revoke space support", fun should_revoke_space_support/0},
            {"get spaces", fun should_get_spaces/0},
            {"get space details", fun should_get_space_details/0}
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    meck:new(oz_endpoint),
    meck:expect(oz_endpoint, request, fun
        (client, "/providers/providerId", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/provider", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/provider", delete) ->
            {ok, 204, response_headers, response_body};
        (client, "/provider/spaces", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/provider/spaces/spaceId", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/provider/spaces/spaceId", delete) ->
            {ok, 202, response_headers, response_body}
    end),
    meck:expect(oz_endpoint, request, fun
        (client, "/provider", patch, <<"body">>) ->
            {ok, 204, response_headers, response_body};
        (client, "/provider/spaces", post, <<"body">>) ->
            {ok, 201, #{<<"location">> => <<"/spaces/spaceId">>}, response_body};
        (client, "/provider/spaces/support", post, <<"body">>) ->
            {ok, 201, #{<<"location">> => <<"/provider/spaces/spaceId">>},
                response_body}
    end),
    meck:expect(oz_endpoint, request, fun
        (client, "/providers", post, <<"body">>, [{endpoint, rest_no_auth}]) ->
            {ok, 200, response_headers, response_body};
        (client, "/provider/public/check_my_ip", get, <<>>, [{endpoint, rest_no_auth}]) ->
            {ok, 200, response_headers, response_body};
        (client, "/provider/public/check_my_ports", post, <<"body">>, [{endpoint, rest_no_auth}]) ->
            {ok, 200, response_headers, response_body}
    end).


teardown(_) ->
    ?assert(meck:validate(oz_endpoint)),
    ok = meck:unload(oz_endpoint).

%%%===================================================================
%%% Tests functions
%%%===================================================================

should_register() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),
    meck:expect(json_utils, decode,
        fun(response_body) ->
            [
                {<<"providerId">>, <<"providerId">>},
                {<<"macaroon">>, <<"macaroon">>}
            ]
        end),

    Answer = oz_providers:register(client, parameters),
    ?assertEqual({ok, <<"providerId">>, <<"macaroon">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_unregister() ->
    Answer = oz_providers:unregister(client),
    ?assertEqual(ok, Answer).


should_get_details() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode,
        fun(response_body) ->
            [
                {<<"providerId">>, <<"providerId">>},
                {<<"clientName">>, <<"name">>},
                {<<"domain">>, <<"domain">>},
                {<<"latitude">>, <<"latitude">>},
                {<<"longitude">>, <<"longitude">>}
            ]
        end),

    Answer = oz_providers:get_details(client),
    ?assertEqual({ok, #provider_details{
        id = <<"providerId">>,
        name = <<"name">>,
        domain = <<"domain">>,
        latitude = <<"latitude">>,
        longitude = <<"longitude">>
    }}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_get_details_for_given_provider() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode,
        fun(response_body) ->
            [
                {<<"providerId">>, <<"providerId">>},
                {<<"clientName">>, <<"name">>},
                {<<"domain">>, <<"domain">>},
                {<<"latitude">>, <<"latitude">>},
                {<<"longitude">>, <<"longitude">>}
            ]
        end),

    Answer = oz_providers:get_details(client, <<"providerId">>),
    ?assertEqual({ok, #provider_details{
        id = <<"providerId">>,
        name = <<"name">>,
        domain = <<"domain">>,
        latitude = <<"latitude">>,
        longitude = <<"longitude">>
    }}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_modify_details() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_providers:modify_details(client, parameters),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_check_ip_address() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) -> <<"ipAddress">> end),

    Answer = oz_providers:check_ip_address(client),
    ?assertEqual({ok, <<"ipAddress">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_check_gui_port() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun
        ([{<<"gui">>, <<"https://ipAddress:443/connection_check">>}]) ->
            <<"body">>
    end),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"https://ipAddress:443/connection_check">>, <<"ok">>}]
    end),

    Answer = oz_providers:check_port(client, <<"ipAddress">>, 443, <<"gui">>),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_check_rest_port() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun([{<<"rest">>,
        <<"https://ipAddress:8443/rest/latest/connection_check">>}]) ->
        <<"body">>
    end),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"https://ipAddress:8443/rest/latest/connection_check">>, <<"ok">>}]
    end),

    Answer = oz_providers:check_port(client, <<"ipAddress">>, 8443, <<"rest">>),
    ?assertEqual(ok, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).

should_create_space() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_providers:create_space(client, parameters),
    ?assertEqual({ok, <<"spaceId">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_support_space() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    Answer = oz_providers:support_space(client, parameters),
    ?assertEqual({ok, <<"spaceId">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_revoke_space_support() ->
    Answer = oz_providers:revoke_space_support(client, <<"spaceId">>),
    ?assertEqual(ok, Answer).


should_get_spaces() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"spaces">>, <<"spaces">>}]
    end),

    Answer = oz_providers:get_spaces(client),
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

    Answer = oz_providers:get_space_details(client, <<"spaceId">>),
    ?assertEqual({ok, #space_details{id = <<"spaceId">>, name = <<"name">>,
        providers_supports = [{<<"providerId">>, 123}]}}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).

-endif.
