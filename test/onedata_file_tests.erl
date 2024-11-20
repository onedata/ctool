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
    ?assertEqual(false, F(<<"\0">>)).


-endif.
