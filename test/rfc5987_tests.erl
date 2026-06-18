%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2026 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of rfc5987 module.
%%% @end
%%%-------------------------------------------------------------------
-module(rfc5987_tests).
-author("Lukasz Opiola").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


%%%===================================================================
%%% Tests
%%%===================================================================


-define(EXAMPLES, [
    {<<"simple.txt"/utf8>>,               <<"simple.txt">>},
    {<<"report 2024.pdf"/utf8>>,          <<"report%202024.pdf">>},
    {<<"naïve café.txt"/utf8>>,           <<"na%C3%AFve%20caf%C3%A9.txt">>},
    {<<"Naïve file.txt"/utf8>>,           <<"Na%C3%AFve%20file.txt">>},
    {<<"日本語.txt"/utf8>>,                <<"%E6%97%A5%E6%9C%AC%E8%AA%9E.txt">>},
    {<<"file (copy).txt"/utf8>>,          <<"file%20%28copy%29.txt">>},
    {<<"100% done.txt"/utf8>>,            <<"100%25%20done.txt">>},
    {<<"résumé.pdf"/utf8>>,               <<"r%C3%A9sum%C3%A9.pdf">>},
    {<<"say \"hello\".txt"/utf8>>,        <<"say%20%22hello%22.txt">>},
    {<<"£ and € rates"/utf8>>,            <<"%C2%A3%20and%20%E2%82%AC%20rates">>},
    {<<"path;name.txt"/utf8>>,            <<"path%3Bname.txt">>}
]).


encode_test_() ->
    [?_assertEqual(Encoded, rfc5987:encode(Raw))
        || {Raw, Encoded} <- ?EXAMPLES].


decode_test_() ->
    [?_assertEqual(Raw, rfc5987:decode(Encoded))
        || {Raw, Encoded} <- ?EXAMPLES].

roundtrip_test_() ->
    [?_assertEqual(Raw, rfc5987:decode(rfc5987:encode(Raw)))
        || {Raw, _} <- ?EXAMPLES].


-endif.