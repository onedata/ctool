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
-module(simple_cache_tests).
-author("Michal Stanisz").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(APP_NAME, simple_cache).
-define(NAME, name).
-define(NAME_TUPLE, {name, key}).
-define(VALUE, value).
-define(TTL, 8).
-define(FUN_FALSE, fun() -> {false, ?VALUE} end).
-define(FUN_TRUE(TTL), fun() -> {true, ?VALUE, TTL} end).

%%%===================================================================
%%% Tests functions
%%%===================================================================


simple_cache_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
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
            {"map_default_with_put_infinity", fun map_default_with_put_infinity/0}
        ]
    }.

with_ttl() ->
    ok = simple_cache:put(?NAME, ?VALUE, ?TTL),
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME)).

infinity() ->
    ok = simple_cache:put(?NAME, ?VALUE),
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME)),
    simulate_time_passing(?TTL),
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME)).

expired() ->
    simple_cache:put(?NAME, ?VALUE, ?TTL),
    simulate_time_passing(?TTL),
    ?assertEqual({error, not_found}, simple_cache:get(?NAME)).

clear() ->
    simple_cache:put(?NAME, ?VALUE),
    simple_cache:clear(?NAME),
    ?assertEqual({error, not_found}, simple_cache:get(?NAME)).

default_without_put() ->
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME, ?FUN_FALSE)),
    ?assertEqual({error, not_found}, simple_cache:get(?NAME)).
    
default_with_put_ttl() ->
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME, ?FUN_TRUE(?TTL))),
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME)),
    simulate_time_passing(?TTL),
    ?assertEqual({error, not_found}, simple_cache:get(?NAME)).

default_with_put_infinity() ->
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME, ?FUN_TRUE(infinity))),
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME)),
    simulate_time_passing(?TTL),
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME)).

get_non_existing() ->
    ?assertEqual({error, not_found}, simple_cache:get(?NAME)).
    

map_with_ttl() ->
    ok = simple_cache:put(?NAME_TUPLE, ?VALUE, ?TTL),
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME_TUPLE)).

map_infinity() ->
    ok = simple_cache:put(?NAME_TUPLE, ?VALUE),
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME_TUPLE)),
    simulate_time_passing(?TTL),
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME_TUPLE)).

map_expired() ->
    simple_cache:put(?NAME_TUPLE, ?VALUE, ?TTL),
    simulate_time_passing(?TTL),
    ?assertEqual({error, not_found}, simple_cache:get(?NAME_TUPLE)).

map_clear() ->
    simple_cache:put(?NAME_TUPLE, ?VALUE),
    simple_cache:clear(?NAME_TUPLE),
    ?assertEqual({error, not_found}, simple_cache:get(?NAME_TUPLE)).

map_default_without_put() ->
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME_TUPLE, ?FUN_FALSE)),
    ?assertEqual({error, not_found}, simple_cache:get(?NAME_TUPLE)).

map_default_with_put_ttl() ->
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME_TUPLE, ?FUN_TRUE(?TTL))),
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME_TUPLE)),
    simulate_time_passing(?TTL),
    ?assertEqual({error, not_found}, simple_cache:get(?NAME_TUPLE)).

map_default_with_put_infinity() ->
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME_TUPLE, ?FUN_TRUE(infinity))),
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME_TUPLE)),
    simulate_time_passing(?TTL),
    ?assertEqual({ok, ?VALUE}, simple_cache:get(?NAME_TUPLE)).

map_get_non_existing() ->
    ?assertEqual({error, not_found}, simple_cache:get(?NAME_TUPLE)).


setup() ->
    meck:new(time_utils, [non_strict]),
    meck:expect(time_utils, system_time_millis, fun timestamp_mock/0).

teardown(_) ->
    ?assert(meck:validate([time_utils])),
    meck:unload(),
    application:unset_env(?APP_NAME, ?NAME).
    
 timestamp_mock() ->
    case get(mocked_time) of
        undefined -> 1500000000; % starting timestamp
        Val -> Val
    end.

simulate_time_passing(Milliseconds) ->
    put(mocked_time, timestamp_mock() + Milliseconds).   


-endif.
