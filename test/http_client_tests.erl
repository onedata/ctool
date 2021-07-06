%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2015 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module tests the http_client module, which is a wrapper
%% on hackney. The tests check if API calls are correctly translated
%% to hackney calls.
%% @end
%% ===================================================================
-module(http_client_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

main_test_() ->
    {setup,
        % Setup
        fun() ->
            meck:new(hackney)
        end,
        % Teardown
        fun(_) ->
            meck:unload(hackney)
        end,
        [
            {"API test", fun api_t/0},
            {"request_return_stream test", fun request_return_stream_t/0}
        ]
    }.

-define(DEFAULT_SSL_OPTS(), secure_ssl_opts:expand([])).


% Checks if API function calls are properly translated to hackney calls
api_t() ->
    % Some test values
    HTTP_URL = <<"http://test.url">>,
    HTTPS_URL = <<"https://test.url">>,
    HeadersMap = #{<<"key">> => <<"value">>},
    HeadersProplist = [{<<"key">>, <<"value">>}],
    Body = <<"body">>,

    % Specify some tests in tuples(3)
    % {
    %   method_to_call,
    %   args,
    %   expected args in call to hackney:request
    % }

    % 1. test: HTTP request
    Test1 = {
        get,
        [HTTP_URL, HeadersMap, Body],
        [get, HTTP_URL, HeadersProplist, Body, [
            with_body,
            {max_body, undefined},
            {pool, false}
        ]]
    },

    % 2. test: HTTP request
    Test2 = {
        delete,
        [HTTP_URL],
        [delete, HTTP_URL, [], <<>>, [
            with_body,
            {max_body, undefined},
            {pool, false}
        ]]
    },

    % 3. test: HTTP request
    Test3 = {
        request,
        [get, HTTP_URL, HeadersMap, Body, [
            {max_body, 12123}
        ]],
        [get, HTTP_URL, HeadersProplist, Body, [
            with_body,
            {max_body, 12123},
            {pool, false}
        ]]
    },

    % 4. test: HTTPS request
    Test4 = {
        post,
        [HTTPS_URL, HeadersMap],
        [post, HTTPS_URL, HeadersProplist, <<>>, [
            with_body,
            {max_body, undefined},
            {pool, false},
            {ssl_options, ?DEFAULT_SSL_OPTS()}
        ]]
    },

    % 5. test: HTTPS request with some ssl options
    Test5 = {
        post,
        [HTTPS_URL, HeadersMap, Body, [
            option,
            {ssl_options, [
                {keyfile, "a"}, {certfile, "b"}
            ]}
        ]],
        [post, HTTPS_URL, HeadersProplist, Body, [
            option,
            {ssl_options, [{keyfile, "a"}, {certfile, "b"} | ?DEFAULT_SSL_OPTS()]},
            with_body,
            {max_body, undefined},
            {pool, false}
        ]]
    },

    % 6. test: insecure HTTPS request
    Test6 = {
        post,
        [HTTPS_URL, HeadersMap, Body, [{ssl_options, [{secure, false}]}]],
        [post, HTTPS_URL, HeadersProplist, Body, [
            {ssl_options, [{verify, verify_none}]},
            with_body,
            {max_body, undefined},
            {pool, false}
        ]]
    },

    % 7. test: insecure HTTPS request with some ssl options
    Test7 = {
        get,
        [HTTPS_URL, HeadersMap, Body, [
            {ssl_options, [{secure, false}, {keyfile, "a"}, {certfile, "b"}]},
            {max_body, 987665}
        ]],
        [get, HTTPS_URL, HeadersProplist, Body, [
            {ssl_options, [{verify, verify_none}, {keyfile, "a"}, {certfile, "b"}]},
            with_body,
            {max_body, 987665},
            {pool, false}
        ]]
    },

    % Do the tests
    lists:foreach(
        fun({Method, Args, Expected}) ->
            meck:expect(hackney, request,
                fun(AMthd, AURL, AHdrs, ABd, AOpts) ->
                    % If args are not as expected, the test will fail here
                    compare_args([AMthd, AURL, AHdrs, ABd, AOpts], Expected),
                    {ok, 200, [], <<>>}
                end),
            ?assertEqual({ok, 200, #{}, <<>>}, erlang:apply(http_client, Method, Args))
        end, [Test1, Test2, Test3, Test4, Test5, Test6, Test7]),
    ?assert(meck:validate(hackney)).


% Checks if API function calls are properly translated to hackney calls
request_return_stream_t() ->
    % Some test values
    HTTP_URL = <<"http://test.url">>,
    HTTPS_URL = <<"https://test.url">>,
    HeadersMap = #{<<"key">> => <<"value">>},
    HeadersProplist = [{<<"key">>, <<"value">>}],
    Body = <<"body">>,

    % Specify some tests in tuples(2).
    % The function called in tests is request_return_stream/5
    % {
    %   args,
    %   expected args in call to hackney:request
    % }

    % 1. test: HTTP request
    Test1 = {
        [post, HTTP_URL, HeadersMap, Body, []],
        [post, HTTP_URL, HeadersProplist, Body, [
            async,
            {pool, false}
        ]]
    },

    % 2. test: HTTP request
    Test2 = {
        [delete, HTTP_URL, HeadersMap, Body, [option]],
        [delete, HTTP_URL, HeadersProplist, Body, [
            async,
            option,
            {pool, false}
        ]]
    },

    % 3. test: HTTPS request
    Test3 = {
        [get, HTTPS_URL, HeadersMap, Body, [option]],
        [get, HTTPS_URL, HeadersProplist, Body, [
            async,
            option,
            {pool, false},
            {ssl_options, ?DEFAULT_SSL_OPTS()}
        ]]
    },

    % 4. test: HTTPS request with some ssl options
    Test4 = {
        [put, HTTPS_URL, HeadersMap, Body, [
            option,
            {ssl_options, [
                {keyfile, "a"}, {certfile, "b"}
            ]}
        ]],
        [put, HTTPS_URL, HeadersProplist, Body, [
            option,
            {ssl_options, [
                {keyfile, "a"}, {certfile, "b"} | ?DEFAULT_SSL_OPTS()
            ]},
            async,
            {pool, false}
        ]]
    },

    % 5. test: insecure HTTPS request
    Test5 = {
        [post, HTTPS_URL, HeadersMap, Body, [{ssl_options, [{secure, false}]}]],
        [post, HTTPS_URL, HeadersProplist, Body, [
            {ssl_options, [{verify, verify_none}]},
            async,
            {pool, false}
        ]]
    },

    % 6. test: insecure HTTPS request with some ssl options
    Test6 = {
        [delete, HTTPS_URL, HeadersMap, Body, [
            {ssl_options, [{secure, false}, {keyfile, "a"}, {certfile, "b"}]}
        ]],
        [delete, HTTPS_URL, HeadersProplist, Body, [
            {ssl_options, [
                {verify, verify_none}, {keyfile, "a"}, {certfile, "b"}
            ]},
            async,
            {pool, false}
        ]]
    },

    % Do the tests
    lists:foreach(
        fun({Args, Expected}) ->
            meck:expect(hackney, request,
                fun(AMthd, AURL, AHdrs, ABd, AOpts) ->
                    % If args are not as expected, the test will fail here
                    compare_args([AMthd, AURL, AHdrs, ABd, AOpts], Expected),
                    {ok, whatever}
                end),
            ?assertEqual({ok, whatever}, erlang:apply(http_client, request_return_stream, Args))
        end, [Test1, Test2, Test3, Test4, Test5, Test6]),
    ?assert(meck:validate(hackney)).


% Asserts if the arguments of actual call to hackney are the same as expected.
compare_args(Actual, Expected) ->
    [AMthd, AURL, AHdrs, ABd, AOpts] = Actual,
    [EMthd, EURL, EHdrs, EBd, EOpts] = Expected,
    ?assertEqual(AMthd, EMthd),
    ?assertEqual(AURL, EURL),
    compare_proplists(AHdrs, EHdrs),
    ?assertEqual(ABd, EBd),
    compare_proplists(AOpts, EOpts).


% Asserts if two proplists are equal
% (the order within lists is not important).
compare_proplists(ListA, ListB) ->
    ?assertEqual(length(ListA), length(ListB)),
    lists:foreach(
        fun(Record) ->
            case Record of
                {Key, Val} when is_list(Val) ->
                    compare_proplists(Val, proplists:get_value(Key, ListB));
                _ ->
                    ?assert(lists:member(Record, ListB))
            end
        end, ListA).
