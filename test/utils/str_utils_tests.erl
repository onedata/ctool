%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Tests of str_utils module.
%%% @end
%%%--------------------------------------------------------------------
-module(str_utils_tests).
-author("Wojciech Geisler").

-include_lib("eunit/include/eunit.hrl").

-export([ensure_suffix_test_/0]).

ensure_suffix_test_() ->
    [{Name,
        ?_assertEqual(Expected, str_utils:ensure_suffix(String, Suffix))
    } || {Name, String, Suffix, Expected} <- [
        {"works on binaries", <<"abc123">>, <<"123">>, <<"abc123">>},
        {"works on lists", "abc123", "123", <<"abc123">>},
        {"works on chardata", ["ab", <<"c123">>], <<"123">>, <<"abc123">>},
        {"adds sufix to binary", <<"abc">>, <<"\n">>, <<"abc\n">>},
        {"adds sufix to list", "abc", <<"\n">>, <<"abc\n">>},
        {"adds sufix to list", "abc", "\n", <<"abc\n">>},
        {"adds partially matched suffix", "abc1", "12", <<"abc112">>}
    ]].


padding_test() ->
    ?assertEqual(<<>>, str_utils:pad_left(<<>>, 0, <<"0">>)),
    ?assertEqual(<<>>, str_utils:pad_right(<<>>, 0, <<"0">>)),
    ?assertEqual(<<"test">>, str_utils:pad_left(<<"test">>, 0, <<"0">>)),
    ?assertEqual(<<"test">>, str_utils:pad_right(<<"test">>, 0, <<"0">>)),
    ?assertEqual(<<"0000test">>, str_utils:pad_left(<<"test">>, 8, <<"0">>)),
    ?assertEqual(<<"test0000">>, str_utils:pad_right(<<"test">>, 8, <<"0">>)).
