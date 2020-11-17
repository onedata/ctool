%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2018-2020 ACK CYFRONET AGH
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
-define(VALUE2, value2).
-define(ERROR, {error, reason}).
-define(TTL, 8).
-define(ACQUIRE_CALLBACK_OK(Value, TTL), fun() -> {ok, Value, TTL} end).
-define(ACQUIRE_CALLBACK_OK(TTL), ?ACQUIRE_CALLBACK_OK(?VALUE, TTL)).
-define(ACQUIRE_CALLBACK_ERROR(), fun() -> ?ERROR end).
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
    {"overwrite_value", fun overwrite_value/1},
    {"put_infinity", fun put_infinity/1},
    {"get_expired", fun get_expired/1},
    {"get_expired_default", fun get_expired_default/1},
    {"get_non_existing", fun get_non_existing/1},
    {"get_default", fun get_default/1},
    {"clear", fun clear/1},
    {"acquire_with_put_ttl", fun acquire_with_put_ttl/1},
    {"acquire_after_ttl", fun acquire_after_ttl/1},
    {"acquire_with_put_infinity", fun acquire_with_put_infinity/1},
    {"acquire_with_error", fun acquire_with_error/1}
]).

node_cache_test_() ->
    {foreach,
        fun() ->
            clock_freezer_mock:setup(),
            node_cache:init()
        end,
        fun(_) ->
            clock_freezer_mock:teardown(),
            node_cache:destroy()
        end,
        lists:flatmap(fun({TestDescription, Fun}) ->
            [{TestDescription, fun() -> Fun(Key) end} || Key <- ?KEYS]
        end, ?TEST_CASES)
    }.

put_with_ttl(Key) ->
    ok = node_cache:put(Key, ?VALUE, ?TTL),
    ?assertEqual(?VALUE, node_cache:get(Key)),
    clock_freezer_mock:simulate_seconds_passing(?TTL - 1),
    ?assertEqual(?VALUE, node_cache:get(Key)).

overwrite_value(Key) ->
    ok = node_cache:put(Key, ?VALUE, ?TTL),
    ok = node_cache:put(Key, ?VALUE2, ?TTL),
    ?assertEqual(?VALUE2, node_cache:get(Key)).

put_infinity(Key) ->
    ok = node_cache:put(Key, ?VALUE),
    ?assertEqual(?VALUE, node_cache:get(Key)),
    clock_freezer_mock:simulate_seconds_passing(?TTL),
    ?assertEqual(?VALUE, node_cache:get(Key)).

get_expired(Key) ->
    node_cache:put(Key, ?VALUE, ?TTL),
    clock_freezer_mock:simulate_seconds_passing(?TTL),
    ?assertError({badkey, Key}, node_cache:get(Key)).

get_expired_default(Key) ->
    node_cache:put(Key, ?VALUE, ?TTL),
    clock_freezer_mock:simulate_seconds_passing(?TTL),
    ?assertEqual(?VALUE2, node_cache:get(Key, ?VALUE2)).

get_non_existing(Key) ->
    ?assertError({badkey, Key}, node_cache:get(Key)).

get_default(Key) ->
    ?assertEqual(default, node_cache:get(Key, default)),
    ok = node_cache:put(Key, ?VALUE, ?TTL),
    ?assertEqual(?VALUE, node_cache:get(Key, default)).

clear(Key) ->
    node_cache:put(Key, ?VALUE),
    node_cache:clear(Key),
    ?assertError({badkey, Key}, node_cache:get(Key)).
    
acquire_with_put_ttl(Key) ->
    ?assertEqual({ok, ?VALUE}, node_cache:acquire(Key, ?ACQUIRE_CALLBACK_OK(?TTL))), 
    ?assertEqual(?VALUE, node_cache:get(Key)),
    ?assertEqual({ok, ?VALUE}, node_cache:acquire(Key, ?ACQUIRE_CALLBACK_OK(?TTL))), 
    
    clock_freezer_mock:simulate_seconds_passing(?TTL - 1),
    ?assertEqual(?VALUE, node_cache:get(Key)),
    clock_freezer_mock:simulate_seconds_passing(1),
    ?assertError({badkey, Key}, node_cache:get(Key)),
    ?assertEqual({ok, ?VALUE}, node_cache:acquire(Key, ?ACQUIRE_CALLBACK_OK(?TTL))).

acquire_after_ttl(Key) ->
    ?assertEqual({ok, ?VALUE}, node_cache:acquire(Key, ?ACQUIRE_CALLBACK_OK(?TTL))),
    ?assertEqual({ok, ?VALUE}, node_cache:acquire(Key, ?ACQUIRE_CALLBACK_OK(?VALUE2, ?TTL))),
    clock_freezer_mock:simulate_seconds_passing(?TTL),
    ?assertEqual({ok, ?VALUE2}, node_cache:acquire(Key, ?ACQUIRE_CALLBACK_OK(?VALUE2, ?TTL))).

acquire_with_put_infinity(Key) ->
    ?assertEqual({ok, ?VALUE}, node_cache:acquire(Key, ?ACQUIRE_CALLBACK_OK(infinity))),
    ?assertEqual(?VALUE, node_cache:get(Key)),
    clock_freezer_mock:simulate_seconds_passing(?TTL),
    ?assertEqual(?VALUE, node_cache:get(Key)).

acquire_with_error(Key) ->
    ?assertEqual(?ERROR, node_cache:acquire(Key, ?ACQUIRE_CALLBACK_ERROR())),
    ?assertError({badkey, Key}, node_cache:get(Key)).


-endif.
