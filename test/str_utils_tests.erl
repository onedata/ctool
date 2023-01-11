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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test/test_utils.hrl").


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


name_validation_test() ->
    V = fun str_utils:validate_name/1,

    ?assertEqual(false, V(<<"aaa*&:|}{][,a"/utf8>>)),
    ?assertEqual(false, V(<<"][aaa*&:|}{][,a]["/utf8>>)),
    ?assertEqual(false, V(<<"A">>)),
    ?assertEqual(false, V(<<"|group_name">>)),
    ?assertEqual(false, V(<<"group_name|">>)),
    ?assertEqual(false, V(<<"-group_name">>)),
    ?assertEqual(false, V(<<".group_name">>)),
    ?assertEqual(false, V(<<" group_name">>)),
    ?assertEqual(false, V(<<"group_name-">>)),
    ?assertEqual(false, V(<<"group_name.">>)),
    ?assertEqual(false, V(<<"group_name ">>)),
    ?assertEqual(false, V(?TOO_LONG_NAME)),
    ?assertEqual(true, V(<<"AB">>)),
    ?assertEqual(true, V(<<"_group_name">>)),
    ?assertEqual(true, V(<<"group_name_">>)),
    ?assertEqual(true, V(<<"group_name">>)),
    ?assertEqual(true, V(<<"_group-name_">>)),
    ?assertEqual(true, V(<<"(group_name)">>)),
    ?assertEqual(true, V(<<"(group) (name)">>)),
    ?assertEqual(true, V(<<"group.- _name">>)),
    ?assertEqual(true, V(<<"My Group Name">>)),
    ?assertEqual(true, V(<<"µńż_źć-21.3(1)"/utf8>>)).


-endif.
