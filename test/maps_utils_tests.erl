%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Eunit tests for lists_utils module.
%%%--------------------------------------------------------------------
-module(maps_utils_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


is_submap_test() ->
    OriginalMap = #{
        a => 1,
        b => 2,
        c => 3
    },
    ?assert(maps_utils:is_submap(#{}, OriginalMap)),
    ?assert(maps_utils:is_submap(#{a => 1}, OriginalMap)),
    ?assert(maps_utils:is_submap(#{b => 2}, OriginalMap)),
    ?assert(maps_utils:is_submap(#{a => 1, c => 3}, OriginalMap)),
    ?assert(maps_utils:is_submap(#{c => 3, b => 2, a => 1}, OriginalMap)),
    ?assertNot(maps_utils:is_submap(#{c => 1}, OriginalMap)),
    ?assertNot(maps_utils:is_submap(#{c => 3, b => 2, a => 17}, OriginalMap)).


generate_from_list_test() ->
    ?assertEqual(#{}, maps_utils:generate_from_list(fun(Value) -> {Value, Value} end, [])),
    ?assertEqual(#{
        1 => 1,
        2 => 2,
        3 => 3,
        4 => 4
    }, maps_utils:generate_from_list(fun(Value) -> {Value, Value} end, [1, 2, 3, 4])).


generate_test() ->
    ?assertEqual(#{}, maps_utils:generate(fun() -> {key, value} end, 0)),
    ?assertEqual(#{key => value}, maps_utils:generate(fun() -> {key, value} end, 1)),
    ?assertEqual(#{key => value}, maps_utils:generate(fun() -> {key, value} end, 17)),
    ?assertEqual(#{}, maps_utils:generate(fun(Ordinal) -> {Ordinal, value} end, 0)),
    ?assertEqual(#{1 => value, 2 => value}, maps_utils:generate(fun(Ordinal) -> {Ordinal, value} end, 2)).


random_submap_test() ->
    OriginalMap = maps_utils:generate(fun() -> {rand:uniform(100), rand:uniform(100)} end, 17),
    ?assert(maps_utils:is_submap(maps_utils:random_submap(OriginalMap), OriginalMap)),
    ?assert(maps_utils:is_submap(maps_utils:random_submap(OriginalMap, 0, all), OriginalMap)),
    ?assert(maps_utils:is_submap(maps_utils:random_submap(OriginalMap, 0, 16), OriginalMap)),
    ?assert(maps_utils:is_submap(maps_utils:random_submap(OriginalMap, 1, 16), OriginalMap)),
    ?assert(maps_utils:is_submap(maps_utils:random_submap(OriginalMap, 1, 1), OriginalMap)),
    ?assert(maps_utils:is_submap(maps_utils:random_submap(OriginalMap, 10, 12), OriginalMap)).


fold_while_test() ->
    OriginalMap = #{
        a => 1,
        b => 2,
        c => 3
    },

    FoldFun = fun(Key, Value, {not_found, Acc}) ->
        case Value =:= 2 of
            true -> {halt, {found, Key, Acc}};
            false -> {cont, {not_found, Acc ++ [Key]}}
        end
    end,

    ?assertMatch({not_found, []}, maps_utils:fold_while(FoldFun, {not_found, []}, #{})),

    SuccessResult = maps_utils:fold_while(FoldFun, {not_found, []}, OriginalMap),
    ?assertMatch({found, b, _}, SuccessResult),
    {found, b, CheckedKeys1} = SuccessResult,
    ?assert(lists_utils:is_subset(CheckedKeys1, [a, b, c])),

    FailureResult = maps_utils:fold_while(FoldFun, {not_found, []}, OriginalMap#{b => 17}),
    ?assertMatch({not_found, _}, FailureResult),
    {not_found, CheckedKeys2} = FailureResult,
    ?assertEqual(lists:sort(CheckedKeys2), [a, b, c]).


-endif.
