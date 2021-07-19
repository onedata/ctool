%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for token_utils module.
%%% @end
%%%--------------------------------------------------------------------
-module(filepath_utils_test).
-author("Bartosz Walkowicz").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("errors.hrl").


split_test_() ->
    S = fun filepath_utils:split/1,

    [
        ?_assertEqual([<<"/">>], S(<<"/">>)),
        ?_assertEqual([<<"  ">>], S(<<"  /">>)),
        ?_assertEqual([<<"/">>, <<"1">>, <<"2">>], S(<<"/1/2">>)),
        ?_assertEqual([<<"1">>, <<"2">>], S(<<"1/2">>)),
        ?_assertEqual([<<"/">>, <<"1">>, <<"2">>], S(<<"///1///2////">>)),
        ?_assertEqual([], S(<<"">>)),
        ?_assertEqual(
            [<<".">>, <<".">>, <<".">>, <<".">>, <<"1">>, <<".">>, <<"2">>],
            S(<<"././././1/./2/">>)
        )
    ].


join_test_() ->
    J = fun filepath_utils:join/1,

    [
        ?_assertEqual(<<"/">>, J([<<"/">>])),
        ?_assertEqual(<<"">>, J([])),
        ?_assertEqual(<<"/1/2/3/4">>, J([<<"/">>, <<"1">>, <<"2">>, <<"3">>, <<"4">>])),
        ?_assertEqual(<<"1/2/3/4">>, J([<<"1">>, <<"2">>, <<"3">>, <<"4">>])),
        ?_assertEqual(<<"/1/2/3/4">>, J([<<"/">>, <<"/1">>, <<"//2">>, <<"3">>, <<"4">>])),
        ?_assertEqual(<<"/1/2/3/4">>, J([<<"/">>, <<"1/">>, <<"2////">>, <<"3">>, <<"4">>])),
        ?_assertEqual(<<"/1/2/3/4">>, J([<<"/">>, <<"/1/">>, <<"///2////">>, <<"3">>, <<"4">>])),
        ?_assertEqual(<<"/1/2/3/4">>, J([<<"/">>, <<"/">>, <<"1/">>, <<"///2////">>, <<"3">>, <<"4">>])),
        ?_assertEqual(<<"/1/2/3/4">>, J([<<"/1">>, <<"//2">>, <<"3">>, <<"4">>]))
    ].


sanitize_test_() ->
    S = fun filepath_utils:sanitize/1,

    [
        ?_assertEqual({ok, <<"/a/b">>}, S(<<"/a/b///////////">>)),
        ?_assertEqual({ok, <<"a/b">>}, S(<<"a/b/">>)),
        ?_assertEqual({ok, <<"    a/b">>}, S(<<"    a/b/">>)),
        ?_assertEqual({ok, <<"/a/b    ">>}, S(<<"/a/b    ">>)),
        ?_assertEqual({ok, <<"/a/b">>}, S(<<"////a///b/././////./././">>)),

        ?_assertEqual({ok, <<"/a/b/c">>}, S(<<"/a/b/c">>)),
        ?_assertEqual({ok, <<"/a/b/..c">>}, S(<<"/a/b/..c">>)),
        ?_assertEqual({error, invalid_path}, S(<<"/a/b/../c">>)),
        ?_assertEqual({error, invalid_path}, S(<<"/a/b/\0null\0/c">>))
    ].


is_ancestor_test_() ->
    P = fun filepath_utils:is_ancestor/2,

    [
        ?_assertEqual(false, P(<<"/c/b/a">>, <<"/a/b/c">>)),
        ?_assertEqual(false, P(<<"/a/b/c/d">>, <<"/a/b/c">>)),
        ?_assertEqual(false, P(<<"a/b/c/d">>, <<"a/b/c">>)),
        ?_assertEqual(false, P(<<"/a/b/c">>, <<"/a/b/c">>)),

        ?_assertEqual({true, <<"a/b/c">>}, P(<<"/">>, <<"/a/b/c">>)),
        ?_assertEqual({true, <<"c">>}, P(<<"/a/b">>, <<"/a/b/c">>)),
        ?_assertEqual({true, <<"c">>}, P(<<"a/b">>, <<"a/b/c">>)),
        ?_assertEqual({true, <<"b/c">>}, P(<<"/a">>, <<"/a/b/c">>))
    ].


is_equal_or_descendant_test_() ->
    P = fun filepath_utils:is_equal_or_descendant/2,

    [
        ?_assertEqual(false, P(<<"/c/b/a">>, <<"/a/b/c">>)),
        ?_assertEqual({true, <<>>}, P(<<"/a/b/c">>, <<"/a/b/c">>)),
        ?_assertEqual({true, <<>>}, P(<<"a/b/c">>, <<"a/b/c">>)),
        ?_assertEqual({true, <<"d">>}, P(<<"/a/b/c/d">>, <<"/a/b/c">>))
    ].


is_descendant_test_() ->
    P = fun filepath_utils:is_descendant/2,

    [
        ?_assertEqual(false, P(<<"c/b/a">>, <<"a/b/c">>)),
        ?_assertEqual(false, P(<<"/c/b/a">>, <<"/a/b/c">>)),
        ?_assertEqual(false, P(<<"/a/b/c">>, <<"/a/b/c">>)),
        ?_assertEqual(false, P(<<"/a/b/c">>, <<"/a/b/c">>)),
        ?_assertEqual(false, P(<<"/a/b/cc">>, <<"/a/b/c">>)),
        ?_assertEqual(false, P(<<"a/b/cc">>, <<"a/b/c">>)),
        ?_assertEqual({true, <<"a/b/c">>}, P(<<"/a/b/c">>, <<"/">>)),
        ?_assertEqual({true, <<"d">>}, P(<<"/a/b/c/d">>, <<"/a/b/c">>)),
        ?_assertEqual({true, <<"b/c/d">>}, P(<<"/a/b/c/d">>, <<"/a">>))
    ].

relative_test_() ->
    P = fun filepath_utils:relative/2,

    [
        ?_assertThrow(?EINVAL, P(<<"/c/b/a">>, <<"/a/b/c">>)),
        ?_assertThrow(?EINVAL, P(<<"/a/b/c/d">>, <<"/a/b/c">>)),
        ?_assertThrow(?EINVAL, P(<<"a/b/c/d">>, <<"a/b/c">>)),

        ?_assertEqual(<<>>, P(<<"a/b/c">>, <<"a/b/c">>)),
        ?_assertEqual(<<>>, P(<<"/a/b/c">>, <<"/a/b/c">>)),
        ?_assertEqual(<<"a/b/c">>, P(<<"/">>, <<"/a/b/c">>)),
        ?_assertEqual(<<"c">>, P(<<"/a/b">>, <<"/a/b/c">>)),
        ?_assertEqual(<<"c">>, P(<<"a/b">>, <<"a/b/c">>)),
        ?_assertEqual(<<"b/c">>, P(<<"/a">>, <<"/a/b/c">>))
    ].


consolidate_test_() ->
    C = fun filepath_utils:consolidate/1,

    [
        ?_assertEqual([<<"/c/b">>], C([<<"/c/b">>, <<"/c/b/q">>])),
        ?_assertEqual([<<"/a/b/c">>], C([<<"/a/b/c/d">>, <<"/a/b/c">>])),
        ?_assertEqual(
            [<<"/a/b/c">>, <<"/p/o/i">>],
            C([<<"/a/b/c/d">>, <<"/a/b/c">>, <<"/p/o/i">>, <<"/a/b/c/e/w/q">>])
        ),
        ?_assertEqual([<<"/a/b/c">>, <<"/c/b/a">>], C([<<"/c/b/a">>, <<"/a/b/c">>])),

        % Absolute and relative paths should not be mixed (it is the responsibility of the caller)
        % as they are not comparable and as such the result may be undesirable
        ?_assertEqual([<<"/c/b">>, <<"c/b/q">>], C([<<"/c/b">>, <<"c/b/q">>]))
    ].


intersect_test_() ->
    I = fun filepath_utils:intersect/2,

    [
        ?_assertEqual([], I([<<"/q/w/e">>], [<<"/a/s/d">>])),
        ?_assertEqual([<<"/q/w/e">>], I([<<"/q/w/e">>], [<<"/e/w/q">>, <<"/q/w/e">>])),
        ?_assertEqual(
            [<<"/z/x/c/d/e">>],
            I([<<"/q/w/e">>, <<"/z/x/c">>], [<<"/a/s/d">>, <<"/z/x/c/d/e">>])
        ),

        % Absolute and relative paths should not be mixed (it is the responsibility of the caller)
        % as they are not comparable and as such the result may be undesirable
        ?_assertEqual([], I([<<"/q/w/e">>], [<<"/e/w/q">>, <<"q/w/e">>]))
    ].


check_relation_test_() ->
    F = fun filepath_utils:check_relation/2,

    [
        ?_assertEqual(undefined, F(<<"/qwe/binar">>, <<"asd">>)),
        ?_assertEqual(undefined, F(<<"/qwe/binar">>, <<"/qwe/users">>)),
        ?_assertEqual(undefined, F(<<"/qwe/binar">>, <<"/qwe/bin">>)),
        ?_assertEqual(undefined, F(<<"/qwe/binar">>,  <<"/qwe/binaries">>)),
        ?_assertEqual(undefined, F(<<"/qwe/bin/users">>, <<"/asd/bin/users/qwe">>)),

        ?_assertEqual(equal, F(<<"/qwe/binaries">>, <<"/qwe/binaries">>)),

        ?_assertEqual({ancestor, <<"asd">>}, F(<<"/qwe/bin">>, <<"/qwe/bin/asd">>)),
        ?_assertEqual({ancestor, <<"asd/zxc">>}, F(<<"/qwe/bin">>, <<"/qwe/bin/asd/zxc">>)),

        ?_assertEqual({descendant, <<"chmod">>}, F(<<"/qwe/binaries/chmod">>, <<"/qwe/binaries">>)),
        ?_assertEqual({descendant, <<"bin/users">>}, F(<<"/qwe/bin/users">>, <<"/qwe">>))
    ].


-endif.
