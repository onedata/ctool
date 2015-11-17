%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module tests the functionality of gr_endpoint module.
%%% It contains unit tests that base on eunit.
%%% @end
%%%-------------------------------------------------------------------

-module(gr_endpoint_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

% Root macaroon
-define(MACAROON, <<"macaroon">>).
% List of disch macaroons
-define(DISCH_MACAROONS,
    [<<"disch_macaroon">>, <<"another_disch_macaroon">>, <<"moar_disch_macaroon">>]
).

-define(CONTENT_TYPE_HEADER,
    {<<"content-type">>, <<"application/json">>}
).

-define(AUTH_BEARER_TOKEN_HEADER,
    {<<"authorization">>, <<"Bearer AccessToken">>}
).

-define(MACAROON_HEADER,
    {<<"macaroon">>, <<"macaroon">>}
).

% Bound disch macaroons joined into one string with spaces for request header.
-define(DISCHARGE_MACAROONS_HEADER,
    {<<"discharge-macaroons">>, <<"binded_disch_macaroon binded_another_disch_macaroon binded_moar_disch_macaroon">>}
).

%%%===================================================================
%%% Tests description
%%%===================================================================

gr_endpoint_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"provider request (URN, method)", fun should_send_provider_request_1/0},
            {"provider request (URN, method, body)", fun should_send_provider_request_2/0},
            {"provider request (URN, method, body, options)", fun should_send_provider_request_3/0},
            {"provider request (URN, method, headers, body, options)", fun should_send_provider_request_4/0},
            {"user request (URN, method)", fun should_send_user_request_1/0},
            {"user request (URN, method, body)", fun should_send_user_request_2/0},
            {"user request (URN, method, body, options)", fun should_send_user_request_3/0},
            {"user request (URN, method, headers, body, options)", fun should_send_user_request_4/0}
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    meck:new(gr_plugin, [non_strict]),
    meck:expect(gr_plugin, get_gr_url, fun() -> "URL/" end),
    meck:expect(gr_plugin, get_key_path, fun() -> key_path end),
    meck:expect(gr_plugin, get_cert_path, fun() -> cert_path end),
    meck:expect(gr_plugin, get_cacert_path, fun() -> cacert_path end),
    meck:new(file, [unstick, passthrough]),
    meck:expect(file, read_file, fun
        (key_path) -> {ok, key};
        (cert_path) -> {ok, cert};
        (cacert_path) -> {ok, cacert}
    end),

    meck:new(public_key, [unstick, passthrough]),
    meck:expect(public_key, pem_decode, fun
        (key) -> [{key_type, key_encoded, not_encrypted}];
        (cert) -> [{cert_type, cert_encoded, not_encrypted}];
        (cacert) -> [{cacert_type, cacert_encoded, not_encrypted}]
    end),

    meck:new(macaroon),
    meck:expect(macaroon, deserialize, fun(X) -> {ok, X} end),
    meck:expect(macaroon, serialize, fun(X) -> {ok, X} end),
    meck:expect(macaroon, prepare_for_request,
        fun(_Macaroon, DischMacaroon) ->
            {ok, <<"binded_", DischMacaroon/binary>>}
        end).


teardown(_) ->
    ?assert(meck:validate(gr_plugin)),
    ok = meck:unload(gr_plugin),
    ?assert(meck:validate(file)),
    ok = meck:unload(file),
    ?assert(meck:validate(public_key)),
    ok = meck:unload(public_key),
    ?assert(meck:validate(macaroon)),
    ok = meck:unload(macaroon).

%%%===================================================================
%%% Tests functions
%%%===================================================================

should_send_provider_request_1() ->
    meck:new(hackney),
    meck:expect(hackney, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            [],
            []
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            [],
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request(provider, "URN", method)),
    ?assertEqual(ok, gr_endpoint:noauth_request(provider, "URN", method)),
    ?assertEqual(ok, gr_endpoint:auth_request({try_user, undefined}, "URN", method)),
    ?assertEqual(ok, gr_endpoint:noauth_request({try_user, undefined}, "URN", method)),

    ?assert(meck:validate(hackney)),
    ok = meck:unload(hackney).


should_send_provider_request_2() ->
    meck:new(hackney),
    meck:expect(hackney, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            body,
            []
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            body,
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request(provider, "URN", method, body)),
    ?assertEqual(ok, gr_endpoint:noauth_request(provider, "URN", method, body)),
    ?assertEqual(ok, gr_endpoint:auth_request({try_user, undefined}, "URN", method, body)),
    ?assertEqual(ok, gr_endpoint:noauth_request({try_user, undefined}, "URN", method, body)),

    ?assert(meck:validate(hackney)),
    ok = meck:unload(hackney).


should_send_provider_request_3() ->
    meck:new(hackney),
    meck:expect(hackney, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            body,
            [options]
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER],
            body,
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}, options]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request(provider, "URN", method, body, [options])),
    ?assertEqual(ok, gr_endpoint:noauth_request(provider, "URN", method, body, [options])),
    ?assertEqual(ok, gr_endpoint:auth_request({try_user, undefined}, "URN", method, body, [options])),
    ?assertEqual(ok, gr_endpoint:noauth_request({try_user, undefined}, "URN", method, body, [options])),

    ?assert(meck:validate(hackney)),
    ok = meck:unload(hackney).


should_send_provider_request_4() ->
    meck:new(hackney),
    meck:expect(hackney, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, headers],
            body,
            [options]
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, headers],
            body,
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}, options]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request(provider, "URN", method, [headers], body, [options])),
    ?assertEqual(ok, gr_endpoint:noauth_request(provider, "URN", method, [headers], body, [options])),
    ?assertEqual(ok, gr_endpoint:auth_request({try_user, undefined}, "URN", method, [headers], body, [options])),
    ?assertEqual(ok, gr_endpoint:noauth_request({try_user, undefined}, "URN", method, [headers], body, [options])),

    ?assert(meck:validate(hackney)),
    ok = meck:unload(hackney).


should_send_user_request_1() ->
    meck:new(hackney),
    meck:expect(hackney, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?AUTH_BEARER_TOKEN_HEADER],
            [],
            []
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?AUTH_BEARER_TOKEN_HEADER],
            [],
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request({user, <<"AccessToken">>}, "URN", method)),
    ?assertEqual(ok, gr_endpoint:noauth_request({user, <<"AccessToken">>}, "URN", method)),
    ?assertEqual(ok, gr_endpoint:auth_request({try_user, <<"AccessToken">>}, "URN", method)),
    ?assertEqual(ok, gr_endpoint:noauth_request({try_user, <<"AccessToken">>}, "URN", method)),

    ?assert(meck:validate(hackney)),
    ok = meck:unload(hackney).


should_send_user_request_2() ->
    meck:new(hackney),
    meck:expect(hackney, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?AUTH_BEARER_TOKEN_HEADER],
            body,
            []
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?AUTH_BEARER_TOKEN_HEADER],
            body,
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request({user, <<"AccessToken">>}, "URN", method, body)),
    ?assertEqual(ok, gr_endpoint:noauth_request({user, <<"AccessToken">>}, "URN", method, body)),
    ?assertEqual(ok, gr_endpoint:auth_request({try_user, <<"AccessToken">>}, "URN", method, body)),
    ?assertEqual(ok, gr_endpoint:noauth_request({try_user, <<"AccessToken">>}, "URN", method, body)),

    ?assert(meck:validate(hackney)),
    ok = meck:unload(hackney).


should_send_user_request_3() ->
    meck:new(hackney),
    meck:expect(hackney, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?AUTH_BEARER_TOKEN_HEADER],
            body,
            [options]
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?AUTH_BEARER_TOKEN_HEADER],
            body,
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}, options]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request({user, <<"AccessToken">>}, "URN", method, body, [options])),
    ?assertEqual(ok, gr_endpoint:noauth_request({user, <<"AccessToken">>}, "URN", method, body, [options])),
    ?assertEqual(ok, gr_endpoint:auth_request({try_user, <<"AccessToken">>}, "URN", method, body, [options])),
    ?assertEqual(ok, gr_endpoint:noauth_request({try_user, <<"AccessToken">>}, "URN", method, body, [options])),

    ?assert(meck:validate(hackney)),
    ok = meck:unload(hackney).


should_send_user_request_4() ->
    meck:new(hackney),
    meck:expect(hackney, request, fun
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?AUTH_BEARER_TOKEN_HEADER, headers],
            body,
            [options]
        ) -> ok;
        (
            method,
            "URL/URN",
            [?CONTENT_TYPE_HEADER, ?AUTH_BEARER_TOKEN_HEADER, headers],
            body,
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}, options]
        ) -> ok
    end),

    ?assertEqual(ok, gr_endpoint:auth_request({user, <<"AccessToken">>}, "URN", method, [headers], body, [options])),
    ?assertEqual(ok, gr_endpoint:noauth_request({user, <<"AccessToken">>}, "URN", method, [headers], body, [options])),
    ?assertEqual(ok, gr_endpoint:auth_request({try_user, <<"AccessToken">>}, "URN", method, [headers], body, [options])),
    ?assertEqual(ok, gr_endpoint:noauth_request({try_user, <<"AccessToken">>}, "URN", method, [headers], body, [options])),

    ?assert(meck:validate(hackney)),
    ok = meck:unload(hackney).

-endif.