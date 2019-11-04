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
