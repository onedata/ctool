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

%%%===================================================================
%%% Tests functions
%%%===================================================================

node_cache_test_() ->
    {foreach,
        fun init_per_testcase/0,
        fun end_per_testcase/1,
        [
            {"with_ttl", fun with_ttl/0},
            {"infinity", fun infinity/0},
            {"expired", fun expired/0},
            {"clear", fun clear/0},
            {"get_non_existing", fun get_non_existing/0},
            {"default_without_put", fun default_without_put/0},
            {"default_with_put_ttl", fun default_with_put_ttl/0},
            {"default_with_put_infinity", fun default_with_put_infinity/0},
            
            {"map_with_ttl", fun map_with_ttl/0},
            {"map_infinity", fun map_infinity/0},
            {"map_expired", fun map_expired/0},
            {"map_clear", fun map_clear/0},
            {"map_get_non_existing", fun map_get_non_existing/0},
            {"map_default_without_put", fun map_default_without_put/0},
            {"map_default_with_put_ttl", fun map_default_with_put_ttl/0},
            {"map_default_with_put_infinity", fun map_default_with_put_infinity/0},
            {"map_multi", fun map_multi/0},
            {"single_value_and_map", fun single_value_and_map/0}
        ]
    }.

with_ttl() ->
    ok = node_cache:put(key, ?VALUE, ?TTL),
    ?assertEqual({ok, ?VALUE}, node_cache:get(key)).

infinity() ->
    ok = node_cache:put(key, ?VALUE),
    ?assertEqual({ok, ?VALUE}, node_cache:get(key)),
    simulate_time_passing(?TTL),
    ?assertEqual({ok, ?VALUE}, node_cache:get(key)).

expired() ->
    node_cache:put(key, ?VALUE, ?TTL),
    simulate_time_passing(?TTL),
    ?assertEqual({error, not_found}, node_cache:get(key)).

clear() ->
    node_cache:put(key, ?VALUE),
    node_cache:clear(key),
    ?assertEqual({error, not_found}, node_cache:get(key)).

default_without_put() ->
    ?assertEqual({ok, ?VALUE}, node_cache:get(key, ?FUN_FALSE)),
    ?assertEqual({error, not_found}, node_cache:get(key)).
    
default_with_put_ttl() ->
    ?assertEqual({ok, ?VALUE}, node_cache:get(key, ?FUN_TRUE(?TTL))),
    ?assertEqual({ok, ?VALUE}, node_cache:get(key)),
    simulate_time_passing(?TTL),
    ?assertEqual({error, not_found}, node_cache:get(key)).

default_with_put_infinity() ->
    ?assertEqual({ok, ?VALUE}, node_cache:get(key, ?FUN_TRUE(infinity))),
    ?assertEqual({ok, ?VALUE}, node_cache:get(key)),
    simulate_time_passing(?TTL),
    ?assertEqual({ok, ?VALUE}, node_cache:get(key)).

get_non_existing() ->
    ?assertEqual({error, not_found}, node_cache:get(key)).
    

map_with_ttl() ->
    ok = node_cache:put({key, childkey}, ?VALUE, ?TTL),
    ?assertEqual({ok, ?VALUE}, node_cache:get({key, childkey})).

map_infinity() ->
    ok = node_cache:put({key, childkey}, ?VALUE),
    ?assertEqual({ok, ?VALUE}, node_cache:get({key, childkey})),
    simulate_time_passing(?TTL),
    ?assertEqual({ok, ?VALUE}, node_cache:get({key, childkey})).

map_expired() ->
    node_cache:put({key, childkey}, ?VALUE, ?TTL),
    simulate_time_passing(?TTL),
    ?assertEqual({error, not_found}, node_cache:get({key, childkey})).

map_clear() ->
    node_cache:put({key, childkey}, ?VALUE),
    node_cache:clear({key, childkey}),
    ?assertEqual({error, not_found}, node_cache:get({key, childkey})).

map_default_without_put() ->
    ?assertEqual({ok, ?VALUE}, node_cache:get({key, childkey}, ?FUN_FALSE)),
    ?assertEqual({error, not_found}, node_cache:get({key, childkey})).

map_default_with_put_ttl() ->
    ?assertEqual({ok, ?VALUE}, node_cache:get({key, childkey}, ?FUN_TRUE(?TTL))),
    ?assertEqual({ok, ?VALUE}, node_cache:get({key, childkey})),
    simulate_time_passing(?TTL),
    ?assertEqual({error, not_found}, node_cache:get({key, childkey})).

map_default_with_put_infinity() ->
    ?assertEqual({ok, ?VALUE}, node_cache:get({key, childkey}, ?FUN_TRUE(infinity))),
    ?assertEqual({ok, ?VALUE}, node_cache:get({key, childkey})),
    simulate_time_passing(?TTL),
    ?assertEqual({ok, ?VALUE}, node_cache:get({key, childkey})).

map_get_non_existing() ->
    ?assertEqual({error, not_found}, node_cache:get({key, childkey})).

map_multi() ->
    ok = node_cache:put({key, childkey}, ?VALUE, ?TTL),
    simulate_time_passing(?TTL-1),
    ok = node_cache:put({key, childkey1}, ?VALUE, ?TTL),
    ?assertEqual({ok, ?VALUE}, node_cache:get({key, childkey})),
    simulate_time_passing(1),
    ?assertEqual({error, not_found}, node_cache:get({key, childkey})),
    ?assertEqual({ok, ?VALUE}, node_cache:get({key, childkey1})).

single_value_and_map() ->
    ok = node_cache:put({key, childkey}, ?VALUE, ?TTL),
    ok = node_cache:put({key, childkey1}, ?VALUE, ?TTL),
    ok = node_cache:put(key, ?VALUE, ?TTL),
    ?assertEqual({ok, ?VALUE}, node_cache:get({key, childkey})),
    ?assertEqual({ok, ?VALUE}, node_cache:get({key, childkey1})),
    ?assertEqual({ok, ?VALUE}, node_cache:get(key)),
    node_cache:clear({key, childkey}),
    ?assertEqual({ok, ?VALUE}, node_cache:get({key, childkey1})),
    ?assertEqual({ok, ?VALUE}, node_cache:get(key)),
    node_cache:clear(key),
    ?assertEqual({ok, ?VALUE}, node_cache:get({key, childkey1})).


%%%===================================================================
%%% Helper functions
%%%===================================================================
    
init_per_testcase() ->
    node_cache:init(),
    meck:new(node_cache, [passthrough]),
    meck:expect(node_cache, now, fun timestamp_mock/0).

end_per_testcase(_) ->
    ?assert(meck:validate([node_cache])),
    meck:unload(),
    ets:delete(node_cache).

timestamp_mock() ->
    case get(mocked_time) of
        undefined -> 1500000000; % starting timestamp
        Val -> Val
    end.

simulate_time_passing(Milliseconds) ->
    put(mocked_time, timestamp_mock() + Milliseconds).   


-endif.
