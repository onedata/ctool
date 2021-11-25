%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Eunit tests for lists_utils module.
%%%--------------------------------------------------------------------
-module(lists_utils_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_subset_test() ->
    ?assert(lists_utils:is_subset([1, 5, 9], [9, 8, 4, 1, 1, 6, 5])),
    ?assert(not lists_utils:is_subset([1, 5, 9], [9, 8, 4, 6, 5])),
    ?assert(lists_utils:is_subset([], [9, 8, 4, 6, 5])),
    ?assert(not lists_utils:is_subset([1, 5, 9], [])).


replace_test() ->
    R = fun lists_utils:replace/3,
    ?assertEqual(R(old, new, []), []),
    ?assertEqual(R(old, new, [old]), [new]),
    ?assertEqual(R(old, new, [old, old, old, old, old]), [new, old, old, old, old]),
    ?assertEqual(R(old, new, [0, 0, 0, old, old]), [0, 0, 0, new, old]),
    ?assertEqual(R(old, new, [0, 0, 0, 0, 0]), [0, 0, 0, 0, 0]).


replace_at_test() ->
    R = fun lists_utils:replace_at/3,
    ?assertException(error, badarg, R(val, 1, [])),
    ?assertEqual([val], R(val, 1, [old])),
    ?assertEqual([1, val, 3], R(val, 2, [1, 20, 3])),
    ?assertEqual([1, 2, 3, 4, val, 6, 7, 8, 9], R(val, 5, [1, 2, 3, 4, 50, 6, 7, 8, 9])).


index_of_test() ->
    L = lists:seq(1, 100),
    ?assertEqual(undefined, lists_utils:index_of(1, [])),
    ?assertEqual(1, lists_utils:index_of(1, L)),
    ?assertEqual(50, lists_utils:index_of(50, L)),
    ?assertEqual(100, lists_utils:index_of(100, L)),
    ?assertEqual(undefined, lists_utils:index_of(101, L)),
    ?assertEqual(undefined, lists_utils:index_of(<<"another missing value">>, L)).


searchmap_test() ->
    L = lists:seq(1, 100),
    ?assertEqual(error, lists_utils:searchmap(fun(_) -> {true, any} end, [])),
    ?assertEqual(error, lists_utils:searchmap(fun(_) -> false end, L)),
    ?assertEqual({ok, {found, 13}}, lists_utils:searchmap(fun(Element) ->
        case Element of
            13 -> {true, {found, 13}};
            _ -> false
        end
    end, L)).


pmap_pforeach_pfiltermap_test_() ->
    {timeout, 60, fun() ->
        List = lists:seq(1, 100),
        Length = length(List),
        FilterMapFunGen = fun(M, F) ->
            fun(X) ->
                Result = M(X),
                case F(Result) of
                    true -> {true, Result};
                    false -> false
                end
            end
        end,

        SimpleMapFun = fun(X) -> X * X end,
        SimpleFilterFun = fun(X) -> X rem 2 =:= 0 end,
        SimpleFilterMapFun = FilterMapFunGen(SimpleMapFun, SimpleFilterFun),

        ?assertEqual(lists:map(SimpleMapFun, List), lists_utils:pmap(SimpleMapFun, List)),
        ?assertEqual(lists:foreach(SimpleMapFun, List), lists_utils:pforeach(SimpleMapFun, List)),
        ?assertEqual(lists:filtermap(SimpleFilterMapFun, List),
            lists_utils:pfiltermap(SimpleFilterMapFun, List)),
        ?assertEqual(lists:filtermap(SimpleFilterMapFun, List),
            lists_utils:pfiltermap(SimpleFilterMapFun, List, Length * 2)),
        ?assertEqual(lists:filtermap(SimpleFilterMapFun, List),
            lists_utils:pfiltermap(SimpleFilterMapFun, List, Length div 2)),

        AnotherMapFun = fun(X) -> 8 / X end,
        AnotherFilterFun = fun(X) -> X > 0.5 end,
        AnotherMapFunLongLasting = fun(X) -> timer:sleep(5000 + rand:uniform(1000)), AnotherMapFun(X) end,
        AnotherFilterMapFun = FilterMapFunGen(AnotherMapFun, AnotherFilterFun),
        AnotherLongLastingFilterMapFun = FilterMapFunGen(AnotherMapFunLongLasting, AnotherFilterFun),

        ?assertEqual(lists:map(AnotherMapFun, List), lists_utils:pmap(AnotherMapFunLongLasting, List)),
        ?assertEqual(lists:foreach(AnotherMapFun, List), lists_utils:pforeach(AnotherMapFunLongLasting, List)),
        ?assertEqual(lists:filtermap(AnotherFilterMapFun, List),
            lists_utils:pfiltermap(AnotherLongLastingFilterMapFun, List)),
        ?assertEqual(lists:filtermap(AnotherFilterMapFun, List),
            lists_utils:pfiltermap(AnotherLongLastingFilterMapFun, List, Length * 2)),
        ?assertEqual(lists:filtermap(AnotherFilterMapFun, List),
            lists_utils:pfiltermap(AnotherLongLastingFilterMapFun, List, Length div 2)),

        CrashingFun = fun(X) ->
            case rand:uniform(4) of
                1 -> error(crash_crash);
                2 -> throw(crash_crash);
                3 -> exit(crash_crash);
                4 -> X / 0
            end
        end,
        ?assertException(
            error, {parallel_call_failed, {failed_processes, _}},
            lists_utils:pmap(CrashingFun, List)
        ),
        ?assertException(
            error, {parallel_call_failed, {failed_processes, _}},
            lists_utils:pforeach(CrashingFun, List)
        ),
        ?assertException(
            error, {parallel_call_failed, {failed_processes, _}},
            lists_utils:pfiltermap(CrashingFun, List)
        )
    end}.


generate_test() ->
    ?assertEqual([], lists_utils:generate(fun() -> {key, value} end, 0)),
    ?assertEqual([value], lists_utils:generate(fun() -> value end, 1)),
    ?assertEqual(lists:duplicate(17, value), lists_utils:generate(fun() -> value end, 17)),
    ?assertEqual([], lists_utils:generate(fun(Ordinal) -> Ordinal * 2 end, 0)),
    ?assertEqual([2, 4], lists_utils:generate(fun(Ordinal) -> Ordinal * 2 end, 2)).


-endif.
