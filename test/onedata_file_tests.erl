%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Eunit tests for onedata_file module.
%%%--------------------------------------------------------------------
-module(onedata_file_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("test/test_utils.hrl").
-include("test/naughty_strings.hrl").

%%%===================================================================
%%% Eunit tests
%%%===================================================================

is_valid_filename_test() ->
    F = fun onedata_file:is_valid_filename/1,

    ?assertEqual(true, F(<<"file.txt">>)),
    ?assertEqual(true, F(<<"a">>)),
    ?assertEqual(true, F(<<"_πœę ßþą_śðæŋ-əłżź.ćńµジ(ャパル)パスで日本を- 旅す.る()"/utf8>>)),
    ?assertEqual(true, F(<<"\n\\t\r">>)),

    ?assertEqual(false, F(<<"">>)),
    ?assertEqual(false, F(<<".">>)),
    ?assertEqual(false, F(<<"..">>)),
    ?assertEqual(false, F(list_to_binary(lists:duplicate(256, "x")))),
    ?assertEqual(false, F(<<"dir/file.txt">>)),
    ?assertEqual(false, F(<<"null\0null">>)),
    ?assertEqual(false, F(<<"\0">>)),

    lists:foreach(fun(UnicodeList) ->
        ?assertNotException(_, _, F(str_utils:unicode_list_to_binary(UnicodeList)))
    end, ?NAUGHTY_STRINGS).


sorting_test() ->
    ExpectedSortingResult = [
        "1",
        "12",
        "123",
        "1234",
        "12345",

        "A",
        "a",
        "Ą",
        "ą",

        "ą.dat",
        "Ą.html",
        "a.png",
        "A.txt",

        "Ǽ",
        "ǽ",
        "ǽ.cpp",
        "Ǽ.h",

        "æąœęfć³π¢«·©↓πß¢ćńąńŋæą©ęgf"
        "alamakota",
        "ĘĄ®ŚƏ™Æq3¢·ĆĄŒĘæ↓ąœęńæą",

        "SS",
        "ss",
        "ß",
        % the "scharfes S" large char has a larger unicode value that the small one,
        % which gives different sorting than for all other letters ¯\_(ツ)_/¯
        "ẞ",

        "STrange FilenamĘ",
        "STrange filename",
        "Strange filęname",
        "strAngE fIlenAMe",
        "strAngE fIlenAMë",
        "strAngE fIlenamë",
        "strange filename",
        "strange fileŃame",
        "strange fileńame",
        "ŚTrange Filename \t\rt\r\n\n\n\n\n",
        "śtrÃngÉ ƒilËńaMę Ps æóćπ³ ’¢rx faer",

        "Zzzz",
        "zzzZ",
        "zzzz"
    ],

    KeysAndNames = lists:map(fun(FileNameAsList) ->
        FileName = str_utils:unicode_list_to_binary(FileNameAsList),
        {onedata_file:filename_to_sorting_key(FileName), FileNameAsList}
    end, ExpectedSortingResult),

    SortedKeysAndNames = lists:sort(?SHUFFLED(KeysAndNames)),
    {_, SortedNames} = lists:unzip(SortedKeysAndNames),
    ?assertEqual(ExpectedSortingResult, SortedNames),

    lists:foreach(fun(UnicodeList) ->
        ?assertNotException(_, _, onedata_file:filename_to_sorting_key(str_utils:unicode_list_to_binary(UnicodeList)))
    end, ?NAUGHTY_STRINGS).


-endif.
