%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module tests the functionality of gr_openid module.
%% It contains unit tests that base on eunit.
%% @end
%% ===================================================================

-module(gr_openid_tests).

-ifdef(TEST).

-include("global_registry/gr_openid.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests description
%% ===================================================================

gr_openid_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"get client access code", fun should_get_client_access_code/0},
            {"get client tokens", fun should_get_client_tokens/0},
            {"delete client token", fun should_remove_client_token/0},
            {"verify client", fun should_verify_client/0},
            {"get grant token", fun should_get_grant_token/0}
        ]
    }.

%% ===================================================================
%% Setup/teardown functions
%% ===================================================================

setup() ->
    meck:new(gr_endpoint),
    meck:expect(gr_endpoint, auth_request, fun
        (client, "/openid/client/tokens", get) -> {ok, "200", response_headers, response_body};
        (client, "/openid/client/access_code", get) -> {ok, "200", response_headers, response_body};
        (client, "/openid/client/tokens/accessId", delete) -> {ok, "204", response_headers, response_body}
    end),
    meck:expect(gr_endpoint, auth_request, fun
        (client, "/openid/client/verify", post, <<"body">>) -> {ok, "200", response_headers, response_body};
        (client, "/openid/provider/tokens", post, <<"body">>) -> {ok, "200", response_headers, response_body}
    end).


teardown(_) ->
    ?assert(meck:validate(gr_endpoint)),
    ok = meck:unload(gr_endpoint).

%% ===================================================================
%% Tests functions
%% ===================================================================

should_get_client_access_code() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"accessCode">>, <<"accessCode">>}]
    end),

    Answer = gr_openid:get_client_access_code(client),
    ?assertEqual({ok, <<"accessCode">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_client_tokens() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"tokenInfo">>, [
            [
                {<<"accessId">>, <<"accessId1">>},
                {<<"clientName">>, <<"clientName1">>}
            ],
            [
                {<<"accessId">>, <<"accessId2">>},
                {<<"clientName">>, <<"clientName2">>}
            ]
        ]}]
    end),

    Answer = gr_openid:get_client_tokens(client),
    ?assertEqual({ok, [
        #client_token{
            access_id = <<"accessId1">>,
            client_name = <<"clientName1">>
        },
        #client_token{
            access_id = <<"accessId2">>,
            client_name = <<"clientName2">>
        }
    ]}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_remove_client_token() ->
    Answer = gr_openid:remove_client_token(client, <<"accessId">>),
    ?assertEqual(ok, Answer).


should_verify_client() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [{<<"verified">>, <<"verified">>}]
    end),

    Answer = gr_openid:verify_client(client, parameters),
    ?assertEqual({ok, <<"verified">>}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2).


should_get_grant_token() ->
    meck:new(mochijson2),
    meck:expect(mochijson2, encode, fun(parameters) -> <<"body">> end),
    meck:expect(mochijson2, decode, fun
        (response_body, [{format, proplist}]) -> [
            {<<"access_token">>, <<"access_token">>},
            {<<"token_type">>, <<"token_type">>},
            {<<"expires_in">>, <<"expires_in">>},
            {<<"refresh_token">>, <<"refresh_token">>},
            {<<"scope">>, <<"scope">>},
            {<<"id_token">>, <<"id_token">>}
        ];
        (<<>>, [{format, proplist}]) -> [
            {<<"iss">>, <<"iss">>},
            {<<"sub">>, <<"sub">>},
            {<<"aud">>, <<"aud">>},
            {<<"name">>, <<"name">>},
            {<<"email">>, [[{<<"email">>, <<"email1">>}], [{<<"email">>, <<"email2">>}]]},
            {<<"exp">>, <<"exp">>},
            {<<"iat">>, <<"iat">>}
        ]
    end),
    meck:new(binary, [unstick, passthrough]),
    meck:expect(binary, split, fun(<<"id_token">>, <<".">>, [global]) -> [header, <<"">>, signature] end),

    Answer = gr_openid:get_grant_token(client, parameters),
    ?assertEqual({ok, #grant_token{
        access_token = <<"access_token">>,
        token_type = <<"token_type">>,
        expires_in = <<"expires_in">>,
        refresh_token = <<"refresh_token">>,
        scope = <<"scope">>,
        id_token = #id_token{
            iss = <<"iss">>,
            sub = <<"sub">>,
            aud = <<"aud">>,
            name = <<"name">>,
            email = [<<"email1">>, <<"email2">>],
            exp = <<"exp">>,
            iat = <<"iat">>
        }
    }}, Answer),

    ?assert(meck:validate(mochijson2)),
    ok = meck:unload(mochijson2),
    ?assert(meck:validate(binary)),
    ok = meck:unload(binary).

-endif.
