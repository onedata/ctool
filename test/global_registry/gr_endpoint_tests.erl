%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module tests the functionality of gr_endpoint module.
%% It contains unit tests that base on eunit.
%% @end
%% ===================================================================

-module(gr_endpoint_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests description
%% ===================================================================

gr_endpoint_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"provider request (URI, method)", fun should_send_provider_request_1/0},
            {"provider request (URI, method, body)", fun should_send_provider_request_2/0},
            {"provider request (URI, method, body, options)", fun should_send_provider_request_3/0},
            {"provider request (URI, method, headers, body, options)", fun should_send_provider_request_4/0},
            {"user request (URI, method)", fun should_send_user_request_1/0},
            {"user request (URI, method, body)", fun should_send_user_request_2/0},
            {"user request (URI, method, body, options)", fun should_send_user_request_3/0},
            {"user request (URI, method, headers, body, options)", fun should_send_user_request_4/0}
        ]
    }.

%% ===================================================================
%% Setup/teardown functions
%% ===================================================================

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
    end).

teardown(_) ->
    ?assert(meck:validate(gr_plugin)),
    ok = meck:unload(gr_plugin),
    ?assert(meck:validate(file)),
    ok = meck:unload(file),
    ?assert(meck:validate(public_key)),
    ok = meck:unload(public_key).

%% ===================================================================
%% Tests functions
%% ===================================================================

should_send_provider_request_1() ->
    meck:new(ibrowse),
    meck:expect(ibrowse, send_req, fun
        (
            "URL/URI",
            [{"content_type", "application/json"}],
            method,
            [],
            []
        ) -> ok;
        (
            "URL/URI",
            [{"content_type", "application/json"}],
            method,
            [],
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}]
        ) -> ok
    end),

    gr_endpoint:secure_request(provider, "URI", method),
    gr_endpoint:insecure_request(provider, "URI", method),

    ?assert(meck:validate(ibrowse)),
    ok = meck:unload(ibrowse).


should_send_provider_request_2() ->
    meck:new(ibrowse),
    meck:expect(ibrowse, send_req, fun
        (
            "URL/URI",
            [{"content_type", "application/json"}],
            method,
            body,
            []
        ) -> ok;
        (
            "URL/URI",
            [{"content_type", "application/json"}],
            method,
            body,
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}]
        ) -> ok
    end),

    gr_endpoint:secure_request(provider, "URI", method, body),
    gr_endpoint:insecure_request(provider, "URI", method, body),

    ?assert(meck:validate(ibrowse)),
    ok = meck:unload(ibrowse).


should_send_provider_request_3() ->
    meck:new(ibrowse),
    meck:expect(ibrowse, send_req, fun
        (
            "URL/URI",
            [{"content_type", "application/json"}],
            method,
            body,
            [options]
        ) -> ok;
        (
            "URL/URI",
            [{"content_type", "application/json"}],
            method,
            body,
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}, options]
        ) -> ok
    end),

    gr_endpoint:secure_request(provider, "URI", method, body, [options]),
    gr_endpoint:insecure_request(provider, "URI", method, body, [options]),

    ?assert(meck:validate(ibrowse)),
    ok = meck:unload(ibrowse).


should_send_provider_request_4() ->
    meck:new(ibrowse),
    meck:expect(ibrowse, send_req, fun
        (
            "URL/URI",
            [{"content_type", "application/json"}, headers],
            method,
            body,
            [options]
        ) -> ok;
        (
            "URL/URI",
            [{"content_type", "application/json"}, headers],
            method,
            body,
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}, options]
        ) -> ok
    end),

    gr_endpoint:secure_request(provider, "URI", method, [headers], body, [options]),
    gr_endpoint:insecure_request(provider, "URI", method, [headers], body, [options]),

    ?assert(meck:validate(ibrowse)),
    ok = meck:unload(ibrowse).


should_send_user_request_1() ->
    meck:new(ibrowse),
    meck:expect(ibrowse, send_req, fun
        (
            "URL/URI",
            [{"content_type", "application/json"}, {"authorization", <<"Bearer AccessToken">>}],
            method,
            [],
            []
        ) -> ok;
        (
            "URL/URI",
            [{"content_type", "application/json"}, {"authorization", <<"Bearer AccessToken">>}],
            method,
            [],
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}]
        ) -> ok
    end),

    gr_endpoint:secure_request({user, <<"AccessToken">>}, "URI", method),
    gr_endpoint:insecure_request({user, <<"AccessToken">>}, "URI", method),

    ?assert(meck:validate(ibrowse)),
    ok = meck:unload(ibrowse).


should_send_user_request_2() ->
    meck:new(ibrowse),
    meck:expect(ibrowse, send_req, fun
        (
            "URL/URI",
            [{"content_type", "application/json"}, {"authorization", <<"Bearer AccessToken">>}],
            method,
            body,
            []
        ) -> ok;
        (
            "URL/URI",
            [{"content_type", "application/json"}, {"authorization", <<"Bearer AccessToken">>}],
            method,
            body,
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}]
        ) -> ok
    end),

    gr_endpoint:secure_request({user, <<"AccessToken">>}, "URI", method, body),
    gr_endpoint:insecure_request({user, <<"AccessToken">>}, "URI", method, body),

    ?assert(meck:validate(ibrowse)),
    ok = meck:unload(ibrowse).


should_send_user_request_3() ->
    meck:new(ibrowse),
    meck:expect(ibrowse, send_req, fun
        (
            "URL/URI",
            [{"content_type", "application/json"}, {"authorization", <<"Bearer AccessToken">>}],
            method,
            body,
            [options]
        ) -> ok;
        (
            "URL/URI",
            [{"content_type", "application/json"}, {"authorization", <<"Bearer AccessToken">>}],
            method,
            body,
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}, options]
        ) -> ok
    end),

    gr_endpoint:secure_request({user, <<"AccessToken">>}, "URI", method, body, [options]),
    gr_endpoint:insecure_request({user, <<"AccessToken">>}, "URI", method, body, [options]),

    ?assert(meck:validate(ibrowse)),
    ok = meck:unload(ibrowse).


should_send_user_request_4() ->
    meck:new(ibrowse),
    meck:expect(ibrowse, send_req, fun
        (
            "URL/URI",
            [{"content_type", "application/json"}, {"authorization", <<"Bearer AccessToken">>}, headers],
            method,
            body,
            [options]
        ) -> ok;
        (
            "URL/URI",
            [{"content_type", "application/json"}, {"authorization", <<"Bearer AccessToken">>}, headers],
            method,
            body,
            [{ssl_options, [{cacerts, [cacert_encoded]}, {key, {key_type, key_encoded}}, {cert, cert_encoded}]}, options]
        ) -> ok
    end),

    gr_endpoint:secure_request({user, <<"AccessToken">>}, "URI", method, [headers], body, [options]),
    gr_endpoint:insecure_request({user, <<"AccessToken">>}, "URI", method, [headers], body, [options]),

    ?assert(meck:validate(ibrowse)),
    ok = meck:unload(ibrowse).

-endif.
