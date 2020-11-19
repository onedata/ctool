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


sanitize_test_() ->
    F = fun filepath_utils:sanitize/1,

    [
        ?_assertEqual(<<"/a/b">>, F(<<"/a/b/">>)),
        ?_assertEqual(<<"/a/b">>, F(<<"    /a/b/">>)),
        ?_assertEqual(<<"/a/b">>, F(<<"/a/b    ">>))
    ].


is_ancestor_test_() ->
    F = fun filepath_utils:is_ancestor/2,

    [
        ?_assertEqual(false, F(<<"/c/b/a">>, <<"/a/b/c">>)),
        ?_assertEqual(false, F(<<"/a/b/c/d">>, <<"/a/b/c/">>)),
        ?_assertEqual(false, F(<<"/a/b/c">>, <<"/a/b/c">>)),

        ?_assertEqual({true, <<"c">>}, F(<<"/a/b">>, <<"/a/b/c">>)),
        ?_assertEqual({true, <<"b">>}, F(<<"/a">>, <<"/a/b/c">>))
    ].


is_equal_or_descendant_test_() ->
    F = fun filepath_utils:is_equal_or_descendant/2,

    [
        ?_assertEqual(false, F(<<"/c/b/a">>, <<"/a/b/c">>)),
        ?_assertEqual(false, F(<<"/a/b/c">>, <<"/a/b/c/">>)),
        ?_assertEqual(true, F(<<"/a/b/c">>, <<"/a/b/c">>)),
        ?_assertEqual(true, F(<<"/a/b/c/d">>, <<"/a/b/c">>))
    ].


is_descendant_test_() ->
    F = fun filepath_utils:is_descendant/2,

    [
        ?_assertEqual(false, F(<<"/c/b/a">>, <<"/a/b/c">>)),
        ?_assertEqual(false, F(<<"/a/b/c">>, <<"/a/b/c/">>)),
        ?_assertEqual(false, F(<<"/a/b/c">>, <<"/a/b/c">>)),
        ?_assertEqual(false, F(<<"/a/b/cc">>, <<"/a/b/c">>)),
        ?_assertEqual(true, F(<<"/a/b/c/d">>, <<"/a/b/c">>))
    ].


consolidate_test_() ->
    F = fun filepath_utils:consolidate/1,

    [
        ?_assertEqual([<<"/c/b">>], F([<<"/c/b">>, <<"/c/b/q">>])),
        ?_assertEqual([<<"/a/b/c">>], F([<<"/a/b/c/d">>, <<"/a/b/c">>])),
        ?_assertEqual(
            [<<"/a/b/c">>, <<"p/o/i">>],
            F([<<"/a/b/c/d">>, <<"/a/b/c">>, <<"p/o/i">>, <<"/a/b/c/e/w/q">>])
        ),
        ?_assertEqual([<<"/a/b/c">>, <<"/c/b/a">>], F([<<"/c/b/a">>, <<"/a/b/c">>]))
    ].


intersect_test_() ->
    F = fun filepath_utils:intersect/2,

    [
        ?_assertEqual([], F([<<"/q/w/e">>], [<<"/a/s/d">>])),
        ?_assertEqual([<<"/q/w/e">>], F([<<"/q/w/e">>], [<<"/e/w/q">>, <<"/q/w/e">>])),
        ?_assertEqual(
            [<<"/z/x/c/d/e">>],
            F([<<"/q/w/e">>, <<"/z/x/c">>], [<<"/a/s/d">>, <<"/z/x/c/d/e">>])
        )
    ].


check_path_relation_test_() ->
    F = fun filepath_utils:check_paths_relation/2,

    [
        ?_assertEqual(
            undefined,
            F(<<"/qwe/binar">>, [<<"asd">>, <<"/qwe/users">>])
        ),
        ?_assertEqual(
            undefined,
            F(<<"/qwe/binar">>, [<<"/qwe/bin">>, <<"/qwe/binaries">>])
        ),
        ?_assertEqual(
            {ancestor, ordsets:from_list([<<"asd">>, <<"chmod">>])},
            F(
                <<"/qwe/bin">>,
                [<<"/asd/binaries">>, <<"/qwe/bin/chmod">>, <<"/asd/qwe/asd">>, <<"/qwe/bin/asd">>]
            )
        ),
        ?_assertEqual(
            equal,
            F(<<"/qwe/binaries">>, [<<"/asd/bin">>, <<"/qwe/binaries">>])
        ),
        ?_assertEqual(
            descendant,
            F(<<"/qwe/binaries/chmod">>, [<<"/asd/bin">>, <<"/qwe/binaries">>])
        ),
        ?_assertEqual(
            descendant,
            F(<<"/qwe/bin/users">>, [<<"/asd/bin/users/qwe">>, <<"/qwe/bin">>])
        ),
        ?_assertEqual(
            undefined,
            F(<<"/qwe/bin/users">>, [<<"/asd/bin/users/qwe">>])
        )
    ].


-endif.
