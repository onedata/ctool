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
-define(FUN_FALSE, fun() -> {false, ?VALUE} end).
-define(FUN_TRUE(TTL), fun() -> {true, ?VALUE, TTL} end).
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
    {"with_ttl", fun with_ttl/1},
    {"infinity", fun infinity/1},
    {"expired", fun expired/1},
    {"clear", fun clear/1},
    {"default_without_put", fun default_without_put/1},
    {"default_with_put_ttl", fun default_with_put_ttl/1},
    {"default_with_put_infinity", fun default_with_put_infinity/1},
    {"get_non_existing", fun get_non_existing/1}
]).

node_cache_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        lists:flatten(
            lists:map(fun({TestDescription, Fun}) ->
                lists:map(fun(Key) ->
                    {TestDescription, fun() -> ?_test(Fun(Key)) end}
                end, ?KEYS)
            end, ?TEST_CASES)
        )
    }.

with_ttl(Key) ->
    ok = node_cache:put(Key, ?VALUE, ?TTL),
    ?assertEqual({ok, ?VALUE}, node_cache:get(Key)).

infinity(Key) ->
    ok = node_cache:put(Key, ?VALUE),
    ?assertEqual({ok, ?VALUE}, node_cache:get(Key)),
    simulate_time_passing(?TTL),
    ?assertEqual({ok, ?VALUE}, node_cache:get(Key)).

expired(Key) ->
    node_cache:put(Key, ?VALUE, ?TTL),
    simulate_time_passing(?TTL),
    ?assertEqual({error, not_found}, node_cache:get(Key)).

clear(Key) ->
    node_cache:put(Key, ?VALUE),
    node_cache:clear(Key),
    ?assertEqual({error, not_found}, node_cache:get(Key)).

default_without_put(Key) ->
    ?assertEqual({ok, ?VALUE}, node_cache:get(Key, ?FUN_FALSE)),
    ?assertEqual({error, not_found}, node_cache:get(Key)).
    
default_with_put_ttl(Key) ->
    ?assertEqual({ok, ?VALUE}, node_cache:get(Key, ?FUN_TRUE(?TTL))),
    ?assertEqual({ok, ?VALUE}, node_cache:get(Key)),
    simulate_time_passing(?TTL),
    ?assertEqual({error, not_found}, node_cache:get(Key)).

default_with_put_infinity(Key) ->
    ?assertEqual({ok, ?VALUE}, node_cache:get(Key, ?FUN_TRUE(infinity))),
    ?assertEqual({ok, ?VALUE}, node_cache:get(Key)),
    simulate_time_passing(?TTL),
    ?assertEqual({ok, ?VALUE}, node_cache:get(Key)).

get_non_existing(Key) ->
    ?assertEqual({error, not_found}, node_cache:get(Key)).


%%%===================================================================
%%% Helper functions
%%%===================================================================
    
setup() ->
    node_cache:init(),
    meck:new(node_cache, [passthrough]),
    meck:expect(node_cache, now, fun timestamp_mock/0).

teardown(_) ->
    ?assert(meck:validate([node_cache])),
    meck:unload(),
    node_cache:destroy().

timestamp_mock() ->
    case get(mocked_time) of
        undefined -> 1500000000; % starting timestamp
        Val -> Val
    end.

simulate_time_passing(Milliseconds) ->
    put(mocked_time, timestamp_mock() + Milliseconds).   

-endif.
