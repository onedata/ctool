%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module tests the functionality of gr_openid module.
%%% It contains unit tests that base on eunit.
%%% @end
%%%-------------------------------------------------------------------

-module(gr_openid_tests).

-ifdef(TEST).

-include("global_registry/gr_openid.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests description
%%%===================================================================

gr_openid_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"get tokens", fun should_get_tokens/0},
            {"revoke token", fun should_revoke_token/0},
            {"modify token details", fun should_modify_token_details/0},
            {"get client authorization code",
                fun should_get_client_authorization_code/0},
            {"verify client", fun should_verify_client/0},
            {"get token response", fun should_get_token_response/0}
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    meck:new(gr_endpoint),
    meck:expect(gr_endpoint, auth_request, fun
        (client, "/openid/client/tokens", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/openid/client/tokens/accessId", delete) ->
            {ok, 202, response_headers, response_body};
        (client, "/openid/client/authorization_code", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/openid/provider/tokens", get) ->
            {ok, 200, response_headers, response_body};
        (client, "/openid/provider/tokens/accessId", delete) ->
            {ok, 202, response_headers, response_body}
    end),
    meck:expect(gr_endpoint, auth_request, fun
        (client, "/openid/client/verify", post, <<"body">>) ->
            {ok, 200, response_headers, response_body};
        (client, "/openid/client/tokens/accessId", patch, <<"body">>) ->
            {ok, 204, response_headers, response_body};
        (client, "/openid/provider/tokens/accessId", patch, <<"body">>) ->
            {ok, 204, response_headers, response_body};
        (provider, "/openid/provider/tokens", post, <<"body">>) ->
            {ok, 200, response_headers, response_body}
    end),
    meck:expect(gr_endpoint, noauth_request, fun
        (client, "/openid/client/tokens", post, <<"body">>) ->
            {ok, 200, response_headers, response_body}
    end).


teardown(_) ->
    ?assert(meck:validate(gr_endpoint)),
    ok = meck:unload(gr_endpoint).

%%%===================================================================
%%% Tests functions
%%%===================================================================

should_get_tokens() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) -> [{<<"tokenInfo">>, [
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

    lists:foreach(fun(Function) ->
        Answer = gr_openid:Function(client),
        ?assertEqual({ok, [
            #token_details{
                access_id = <<"accessId1">>,
                client_name = <<"clientName1">>
            },
            #token_details{
                access_id = <<"accessId2">>,
                client_name = <<"clientName2">>
            }
        ]}, Answer)
    end, [get_client_tokens, get_provider_tokens]),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_revoke_token() ->
    lists:foreach(fun(Function) ->
        Answer = gr_openid:Function(client, <<"accessId">>),
        ?assertEqual(ok, Answer)
    end, [revoke_client_token, revoke_provider_token]).


should_modify_token_details() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),

    lists:foreach(fun(Function) ->
        Answer = gr_openid:Function(client, <<"accessId">>, parameters),
        ?assertEqual(ok, Answer)
    end, [modify_client_token_details, modify_provider_token_details]),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_get_client_authorization_code() ->
    meck:new(json_utils),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"authorizationCode">>, <<"authorizationCode">>}]
    end),

    Answer = gr_openid:get_client_authorization_code(client),
    ?assertEqual({ok, <<"authorizationCode">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_verify_client() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),
    meck:expect(json_utils, decode, fun(response_body) ->
        [{<<"verified">>, <<"verified">>}]
    end),

    Answer = gr_openid:verify_client(client, parameters),
    ?assertEqual({ok, <<"verified">>}, Answer),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils).


should_get_token_response() ->
    meck:new(json_utils),
    meck:expect(json_utils, encode, fun(parameters) -> <<"body">> end),
    meck:expect(json_utils, decode, fun(response_body) -> [
        {<<"access_token">>, <<"access_token">>},
        {<<"token_type">>, <<"token_type">>},
        {<<"expires_in">>, <<"expires_in">>},
        {<<"refresh_token">>, <<"refresh_token">>},
        {<<"scope">>, <<"scope">>},
        {<<"id_token">>, <<"id_token">>}
    ];
        (<<>>) -> [
            {<<"iss">>, <<"iss">>},
            {<<"sub">>, <<"sub">>},
            {<<"aud">>, <<"aud">>},
            {<<"name">>, <<"name">>},
            {<<"logins">>, [
                [
                    {<<"provider_id">>, <<"provider1">>},
                    {<<"login">>, <<"login1">>}
                ],
                [
                    {<<"provider_id">>, <<"provider2">>},
                    {<<"login">>, <<"login2">>}
                ]
            ]},
            {<<"emails">>, [<<"email1">>, <<"email2">>]},
            {<<"exp">>, <<"exp">>},
            {<<"iat">>, <<"iat">>}
        ]
    end),
    meck:new(binary, [unstick, passthrough]),
    meck:expect(binary, split, fun(<<"id_token">>, <<".">>, [global]) ->
        [header, <<"">>, signature] end),

    lists:foreach(fun(ClientType) ->
        Answer = gr_openid:get_token_response(ClientType, parameters),
        ?assertEqual({ok, #token_response{
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
                logins = [
                    #id_token_login{provider_id = provider1,
                        login = <<"login1">>},
                    #id_token_login{provider_id = provider2,
                        login = <<"login2">>}
                ],
                emails = [<<"email1">>, <<"email2">>],
                exp = <<"exp">>,
                iat = <<"iat">>
            }
        }}, Answer)
    end, [client, provider]),

    ?assert(meck:validate(json_utils)),
    ok = meck:unload(json_utils),
    ?assert(meck:validate(binary)),
    ok = meck:unload(binary).

-endif.
