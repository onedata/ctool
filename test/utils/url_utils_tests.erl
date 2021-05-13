%%%--------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Tests of url_utils module.
%%% @end
%%%--------------------------------------------------------------------
-module(url_utils_tests).
-author("Jakub Kudzia").

-include_lib("eunit/include/eunit.hrl").

-define(SCHEMES, [http, https]).
-define(PATHS, [<<"/">>, <<"/mypath/a/b/c">>]).
-define(QSS, [<<>>, <<"key1=value1&key2=value2">>]).

-define(HOST, <<"hostname.org">>).
-define(PORT, 1234).
-define(HTTP_PORT, 80).
-define(HTTPS_PORT, 443).


parse_correct_url_with_port_test_() ->
    [
        ?_assertEqual(
            #{scheme => Scheme, host => ?HOST, path => Path, qs => QS, port => ?PORT},
            url_utils:parse(str_utils:format_bin("~s://~s:~p~s?~s", [Scheme, ?HOST, ?PORT, Path, QS]))
        )
        || Scheme <- ?SCHEMES, Path <- ?PATHS, QS <- ?QSS
    ].

parse_correct_url_without_port_test_() ->
    [
        ?_assertEqual(
            #{scheme => Scheme, host => ?HOST, path => Path, qs => QS, port => case Scheme of
                http -> ?HTTP_PORT;
                https -> ?HTTPS_PORT
            end},
            url_utils:parse(str_utils:format_bin("~s://~s~s?~s", [Scheme, ?HOST, Path, QS]))
        )
        || Scheme <- ?SCHEMES, Path <- ?PATHS, QS <- ?QSS
    ].

parse_incorrect_url_test_() ->
    [
        ?_assertError(badarg, url_utils:parse(<<"abc://hostname.org">>)),
        ?_assertError(badarg, url_utils:parse(<<"http://hostname.org:port">>)),
        ?_assertError(badarg, url_utils:parse(<<"http:/hostname.org">>))
    ].

validate_correct_url_with_port_test_() ->
    [
        ?_assertEqual(true,
            url_utils:is_valid(str_utils:format_bin("~s://~s:~p~s?~s", [Scheme, ?HOST, ?PORT, Path, QS]))
        )
        || Scheme <- ?SCHEMES, Path <- ?PATHS, QS <- ?QSS
    ].

validate_correct_url_without_port_test_() ->
    [
        ?_assertEqual(true,
            url_utils:is_valid(str_utils:format_bin("~s://~s~s?~s", [Scheme, ?HOST, Path, QS]))
        )
        || Scheme <- ?SCHEMES, Path <- ?PATHS, QS <- ?QSS
    ].

validate_incorrect_url_test_() ->
    [
        ?_assertEqual(false, url_utils:is_valid(<<"abc://hostname.org">>)),
        ?_assertEqual(false, url_utils:is_valid(<<"http://hostname.org:port">>)),
        ?_assertEqual(false, url_utils:is_valid(<<"http:/hostname.org">>))
    ].
