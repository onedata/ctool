%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Tests of cache utility functions.
%%% @end
%%%-------------------------------------------------------------------
-module(node_cache_tests).
-author("Michal Stanisz").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(VALUE, value).
-define(TTL, 8).
-define(FUN_TRUE(TTL), fun() -> {ok, ?VALUE, TTL} end).
-define(KEYS, [
    atom_key,
    23467234,
    {tuple, key},
    <<"binary_key">>
]).

%%%===================================================================
%%% Tests functions
%%%===================================================================

-define(TEST_CASES, [
    {"put_with_ttl", fun put_with_ttl/1},
    {"put_infinity", fun put_infinity/1},
    {"get_expired", fun get_expired/1},
    {"get_non_existing", fun get_non_existing/1},
    {"get_default", fun get_default/1},
    {"clear", fun clear/1},
    {"acquire_with_put_ttl", fun acquire_with_put_ttl/1},
    {"acquire_with_put_infinity", fun acquire_with_put_infinity/1}
]).

node_cache_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        lists:flatten(
            lists:map(fun({TestDescription, Fun}) ->
                lists:map(fun(Key) ->
                    {TestDescription, fun() -> Fun(Key) end}
                end, ?KEYS)
            end, ?TEST_CASES)
        )
    }.

put_with_ttl(Key) ->
    ok = node_cache:put(Key, ?VALUE, ?TTL),
    ?assertEqual(?VALUE, node_cache:get(Key)).

put_infinity(Key) ->
    ok = node_cache:put(Key, ?VALUE),
    ?assertEqual(?VALUE, node_cache:get(Key)),
    simulate_time_passing(?TTL),
    ?assertEqual(?VALUE, node_cache:get(Key)).

get_expired(Key) ->
    node_cache:put(Key, ?VALUE, ?TTL),
    simulate_time_passing(?TTL),
    ?assertError({badkey, Key}, node_cache:get(Key)).

get_non_existing(Key) ->
    ?assertError({badkey, Key}, node_cache:get(Key)).

get_default(Key) ->
    ?assertEqual(default, node_cache:get(Key, default)).

clear(Key) ->
    node_cache:put(Key, ?VALUE),
    node_cache:clear(Key),
    ?assertError({badkey, Key}, node_cache:get(Key)).
    
acquire_with_put_ttl(Key) ->
    ?assertEqual({ok, ?VALUE}, node_cache:acquire(Key, ?FUN_TRUE(?TTL))),
    ?assertEqual(?VALUE, node_cache:get(Key)),
    simulate_time_passing(?TTL),
    ?assertError({badkey, Key}, node_cache:get(Key)).

acquire_with_put_infinity(Key) ->
    ?assertEqual({ok, ?VALUE}, node_cache:acquire(Key, ?FUN_TRUE(infinity))),
    ?assertEqual(?VALUE, node_cache:get(Key)),
    simulate_time_passing(?TTL),
    ?assertEqual(?VALUE, node_cache:get(Key)).


%%%===================================================================
%%% Helper functions
%%%===================================================================
    
setup() ->
    node_cache:init(),
    meck:new(node_cache, [passthrough]),
    meck:expect(node_cache, now, fun timestamp_mock/0).

teardown(_) ->
    node_cache:destroy(),
    meck:unload().

timestamp_mock() ->
    case get(mocked_time) of
        undefined -> 1500000000; % starting timestamp
        Val -> Val
    end.

simulate_time_passing(Milliseconds) ->
    put(mocked_time, timestamp_mock() + Milliseconds).   

-endif.
