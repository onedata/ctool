%%%--------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @author Piotr Duleba
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
-author("Piotr Duleba").

-include_lib("eunit/include/eunit.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

-define(SCHEMES, [http, https]).
-define(PATHS, [<<"/">>, <<"/mypath/a/b/c">>]).
-define(QSS, [<<>>, <<"key1=value1&key2=value2">>]).

-define(HOST, <<"hostname.org">>).
-define(PORT, 1234).
-define(HTTP_PORT, 80).
-define(HTTPS_PORT, 443).

-define(URL_MAP(Scheme, Host, Port, Path, QueryString), #{
    scheme => Scheme,
    host => Host,
    port => Port,
    path => Path,
    qs => QueryString
}).

-define(CORRECT_URL_BATCH, [
    {<<"hostname:80">>, ?URL_MAP(http, <<"hostname">>, 80, <<>>, <<>>)},
    {<<"hostname:80/path">>, ?URL_MAP(http, <<"hostname">>, 80, <<"/path">>, <<>>)},
    {<<"hostname:80/path?key=value">>, ?URL_MAP(http, <<"hostname">>, 80, <<"/path">>, <<"key=value">>)},
    {<<"hostname:443">>, ?URL_MAP(https, <<"hostname">>, 443, <<>>, <<>>)},
    {<<"hostname:443/path">>, ?URL_MAP(https, <<"hostname">>, 443, <<"/path">>, <<>>)},
    {<<"hostname:443/path?key=value">>, ?URL_MAP(https, <<"hostname">>, 443, <<"/path">>, <<"key=value">>)},
    
    {<<"http://hostname">>, ?URL_MAP(http, <<"hostname">>, 80, <<>>, <<>>)},
    {<<"http://hostname/path">>, ?URL_MAP(http, <<"hostname">>, 80, <<"/path">>, <<>>)},
    {<<"http://hostname/path?key=value">>, ?URL_MAP(http, <<"hostname">>, 80, <<"/path">>, <<"key=value">>)},
    {<<"https://hostname">>, ?URL_MAP(https, <<"hostname">>, 443, <<>>, <<>>)},
    {<<"https://hostname/path">>, ?URL_MAP(https, <<"hostname">>, 443, <<"/path">>, <<>>)},
    {<<"https://hostname/path?key=value">>, ?URL_MAP(https, <<"hostname">>, 443, <<"/path">>, <<"key=value">>)},
    
    {<<"http://hostname:80">>, ?URL_MAP(http, <<"hostname">>, 80, <<>>, <<>>)},
    {<<"http://hostname:80/path">>, ?URL_MAP(http, <<"hostname">>, 80, <<"/path">>, <<>>)},
    {<<"http://hostname:80/path?key=value">>, ?URL_MAP(http, <<"hostname">>, 80, <<"/path">>, <<"key=value">>)},
    {<<"https://hostname:443">>, ?URL_MAP(https, <<"hostname">>, 443, <<>>, <<>>)},
    {<<"https://hostname:443/path">>, ?URL_MAP(https, <<"hostname">>, 443, <<"/path">>, <<>>)},
    {<<"https://hostname:443/path?key=value">>, ?URL_MAP(https, <<"hostname">>, 443, <<"/path">>, <<"key=value">>)},
    
    {<<"http://hostname:443">>, ?URL_MAP(http, <<"hostname">>, 443, <<>>, <<>>)},
    {<<"http://hostname:443/path">>, ?URL_MAP(http, <<"hostname">>, 443, <<"/path">>, <<>>)},
    {<<"http://hostname:443/path?key=value">>, ?URL_MAP(http, <<"hostname">>, 443, <<"/path">>, <<"key=value">>)},
    {<<"https://hostname:80">>, ?URL_MAP(https, <<"hostname">>, 80, <<>>, <<>>)},
    {<<"https://hostname:80/path">>, ?URL_MAP(https, <<"hostname">>, 80, <<"/path">>, <<>>)},
    {<<"https://hostname:80/path?key=value">>, ?URL_MAP(https, <<"hostname">>, 80, <<"/path">>, <<"key=value">>)},
    
    {<<"http://hostname:1234">>, ?URL_MAP(http, <<"hostname">>, 1234, <<>>, <<>>)},
    {<<"http://hostname:1234/path">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/path">>, <<>>)},
    {<<"http://hostname:1234/path?key=value">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/path">>, <<"key=value">>)},
    {<<"https://hostname:1234">>, ?URL_MAP(https, <<"hostname">>, 1234, <<>>, <<>>)},
    {<<"https://hostname:1234/path">>, ?URL_MAP(https, <<"hostname">>, 1234, <<"/path">>, <<>>)},
    {<<"https://hostname:1234/path?key=value">>, ?URL_MAP(https, <<"hostname">>, 1234, <<"/path">>, <<"key=value">>)},
    
    {<<" http://hostname:1234">>, ?URL_MAP(http, <<"hostname">>, 1234, <<>>, <<>>)},
    {<<" http://hostname:1234/path">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/path">>, <<>>)},
    {<<" https://hostname:1234">>, ?URL_MAP(https, <<"hostname">>, 1234, <<>>, <<>>)},
    {<<" https://hostname:1234/path">>, ?URL_MAP(https, <<"hostname">>, 1234, <<"/path">>, <<>>)},
    {<<"http://hostname:1234  ">>, ?URL_MAP(http, <<"hostname">>, 1234, <<>>, <<>>)},
    {<<"http://hostname:1234/path  ">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/path">>, <<>>)},
    {<<"https://hostname:1234  ">>, ?URL_MAP(https, <<"hostname">>, 1234, <<>>, <<>>)},
    {<<"https://hostname:1234/path  ">>, ?URL_MAP(https, <<"hostname">>, 1234, <<"/path">>, <<>>)},
    {<<" http://hostname:1234  ">>, ?URL_MAP(http, <<"hostname">>, 1234, <<>>, <<>>)},
    {<<" http://hostname:1234/path  ">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/path">>, <<>>)},
    {<<" https://hostname:1234  ">>, ?URL_MAP(https, <<"hostname">>, 1234, <<>>, <<>>)},
    {<<" https://hostname:1234/path  ">>, ?URL_MAP(https, <<"hostname">>, 1234, <<"/path">>, <<>>)},
    
    %% TODO: VFS-7682 - move following cases to INCORRECT_URL_BATCH, as they should cause an error
    {<<"hostname:1234">>, ?URL_MAP(http, <<"hostname">>, 1234, <<>>, <<>>)},
    {<<"hostname:1234/path">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/path">>, <<>>)}
]).

-define(INCORRECT_URL_BATCH, [
    <<"hostname">>,
    <<"hostname/path">>,
    <<"hostname:string">>,
    <<"hostname:string/path">>,
    <<"http://hostname:string">>,
    <<"http://hostname:string/path">>,
    <<"https://hostname:string">>,
    <<"https://hostname:string/path">>,
    <<"unknown://hostname">>,
    <<"unknown://hostname/path">>,
    <<"unknown://hostname:1234">>,
    <<"unknown://hostname:1234/path">>,
    
    <<"hostname?key=value">>,
    <<"hostname/path?key=value">>,
    <<"hostname:string?key=value">>,
    <<"hostname:string/path?key=value">>,
    <<"http://hostname:string?key=value">>,
    <<"http://hostname:string/path?key=value">>,
    <<"https://hostname:string?key=value">>,
    <<"https://hostname:string/path?key=value">>,
    <<"unknown://hostname?key=value">>,
    <<"unknown://hostname/path?key=value">>,
    <<"unknown://hostname:1234?key=value">>,
    <<"unknown://hostname:1234/path?key=value">>
]).


correct_url_infer_components_test_() ->
    [
        ?_assertEqual(ExpResult, url_utils:infer_components(Url))
        || {Url, ExpResult} <- ?CORRECT_URL_BATCH
    ].


incorrect_url_infer_components_test_() ->
    [
        ?_assertThrow(?ERROR_MALFORMED_DATA, url_utils:infer_components(Url))
        || Url <- ?INCORRECT_URL_BATCH
    ].


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
