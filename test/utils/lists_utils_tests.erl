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


pmap_pforeach_test_() ->
    {timeout, 60, fun() ->
        List = lists:seq(1, 100),

        SimpleFun = fun(X) -> 2 * X - 15 end,
        ?assertEqual(lists:map(SimpleFun, List), lists_utils:pmap(SimpleFun, List)),
        ?assertEqual(lists:foreach(SimpleFun, List), lists_utils:pforeach(SimpleFun, List)),

        AnotherFun = fun(X) -> 8 / X end,
        AnotherFunLongLasting = fun(X) -> timer:sleep(5000 + rand:uniform(1000)), AnotherFun(X) end,
        ?assertEqual(lists:map(AnotherFun, List), lists_utils:pmap(AnotherFunLongLasting, List)),
        ?assertEqual(lists:foreach(AnotherFun, List), lists_utils:pforeach(AnotherFunLongLasting, List)),

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
        )
    end}.


-endif.
