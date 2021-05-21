%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Test for functions defined in url_utils module.
%%% @end
%%%-------------------------------------------------------------------
-module(url_utils_test).
-author("Piotr Duleba").

-ifdef(TEST).

-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

-define(URL_MAP(Scheme, Host, Port, Path, QueryString), #{
    scheme => Scheme,
    host => Host,
    port => Port,
    path => Path,
    qs => QueryString
}).

-define(CORRECT_URL_BATCH, [
    {<<"hostname:80">>, ?URL_MAP(http, <<"hostname">>, 80, <<"/">>, <<>>)},
    {<<"hostname:80/path">>, ?URL_MAP(http, <<"hostname">>, 80, <<"/path">>, <<>>)},
    {<<"hostname:80/path?key=value">>, ?URL_MAP(http, <<"hostname">>, 80, <<"/path">>, <<"key=value">>)},
    {<<"hostname:443">>, ?URL_MAP(https, <<"hostname">>, 443, <<"/">>, <<>>)},
    {<<"hostname:443/path">>, ?URL_MAP(https, <<"hostname">>, 443, <<"/path">>, <<>>)},
    {<<"hostname:443/path?key=value">>, ?URL_MAP(https, <<"hostname">>, 443, <<"/path">>, <<"key=value">>)},

    {<<"http://hostname">>, ?URL_MAP(http, <<"hostname">>, 80, <<"/">>, <<>>)},
    {<<"http://hostname/path">>, ?URL_MAP(http, <<"hostname">>, 80, <<"/path">>, <<>>)},
    {<<"http://hostname/path?key=value">>, ?URL_MAP(http, <<"hostname">>, 80, <<"/path">>, <<"key=value">>)},
    {<<"https://hostname">>, ?URL_MAP(https, <<"hostname">>, 443, <<"/">>, <<>>)},
    {<<"https://hostname/path">>, ?URL_MAP(https, <<"hostname">>, 443, <<"/path">>, <<>>)},
    {<<"https://hostname/path?key=value">>, ?URL_MAP(https, <<"hostname">>, 443, <<"/path">>, <<"key=value">>)},

    {<<"http://hostname:80">>, ?URL_MAP(http, <<"hostname">>, 80, <<"/">>, <<>>)},
    {<<"http://hostname:80/path">>, ?URL_MAP(http, <<"hostname">>, 80, <<"/path">>, <<>>)},
    {<<"http://hostname:80/path?key=value">>, ?URL_MAP(http, <<"hostname">>, 80, <<"/path">>, <<"key=value">>)},
    {<<"https://hostname:443/">>, ?URL_MAP(https, <<"hostname">>, 443, <<"/">>, <<>>)},
    {<<"https://hostname:443/path">>, ?URL_MAP(https, <<"hostname">>, 443, <<"/path">>, <<>>)},
    {<<"https://hostname:443/path?key=value">>, ?URL_MAP(https, <<"hostname">>, 443, <<"/path">>, <<"key=value">>)},

    {<<"http://hostname:443">>, ?URL_MAP(http, <<"hostname">>, 443, <<"/">>, <<>>)},
    {<<"http://hostname:443/path">>, ?URL_MAP(http, <<"hostname">>, 443, <<"/path">>, <<>>)},
    {<<"http://hostname:443/path?key=value">>, ?URL_MAP(http, <<"hostname">>, 443, <<"/path">>, <<"key=value">>)},
    {<<"https://hostname:80">>, ?URL_MAP(https, <<"hostname">>, 80, <<"/">>, <<>>)},
    {<<"https://hostname:80/path">>, ?URL_MAP(https, <<"hostname">>, 80, <<"/path">>, <<>>)},
    {<<"https://hostname:80/path?key=value">>, ?URL_MAP(https, <<"hostname">>, 80, <<"/path">>, <<"key=value">>)},

    {<<"http://hostname:1234">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/">>, <<>>)},
    {<<"http://hostname:1234/path">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/path">>, <<>>)},
    {<<"http://hostname:1234/path?key=value">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/path">>, <<"key=value">>)},
    {<<"https://hostname:1234">>, ?URL_MAP(https, <<"hostname">>, 1234, <<"/">>, <<>>)},
    {<<"https://hostname:1234/path">>, ?URL_MAP(https, <<"hostname">>, 1234, <<"/path">>, <<>>)},
    {<<"https://hostname:1234/path?key=value">>, ?URL_MAP(https, <<"hostname">>, 1234, <<"/path">>, <<"key=value">>)},

    {<<" http://hostname:1234">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/">>, <<>>)},
    {<<" http://hostname:1234/path">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/path">>, <<>>)},
    {<<" https://hostname:1234">>, ?URL_MAP(https, <<"hostname">>, 1234, <<"/">>, <<>>)},
    {<<" https://hostname:1234/path">>, ?URL_MAP(https, <<"hostname">>, 1234, <<"/path">>, <<>>)},
    {<<"http://hostname:1234  ">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/">>, <<>>)},
    {<<"http://hostname:1234/path  ">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/path">>, <<>>)},
    {<<"https://hostname:1234  ">>, ?URL_MAP(https, <<"hostname">>, 1234, <<"/">>, <<>>)},
    {<<"https://hostname:1234/path  ">>, ?URL_MAP(https, <<"hostname">>, 1234, <<"/path">>, <<>>)},
    {<<" http://hostname:1234  ">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/">>, <<>>)},
    {<<" http://hostname:1234/path  ">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/path">>, <<>>)},
    {<<" https://hostname:1234  ">>, ?URL_MAP(https, <<"hostname">>, 1234, <<"/">>, <<>>)},
    {<<" https://hostname:1234/path  ">>, ?URL_MAP(https, <<"hostname">>, 1234, <<"/path">>, <<>>)},

    %% TODO: VFS-7682 - move following cases to INCORRECT_URL_BATCH, as they should cause an error
    {<<"hostname:1234">>, ?URL_MAP(http, <<"hostname">>, 1234, <<"/">>, <<>>)},
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


correct_url_parse_test() ->
    lists:foreach(fun({InputUrl, ExpResult}) ->
        ?assertEqual(ExpResult, url_utils:infer_components(InputUrl))
    end, ?CORRECT_URL_BATCH).


incorrect_url_parse_test() ->
    lists:foreach(fun(InputUrl) ->
        ?assertThrow(?ERROR_MALFORMED_DATA, url_utils:infer_components(InputUrl))
    end, ?INCORRECT_URL_BATCH).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-endif.

