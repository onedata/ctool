%%%-------------------------------------------------------------------
%%% @author Micha; Stanisz
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

-include_lib("eunit/include/eunit.hrl").

-define(APP_NAME, simple_cache).
-define(NAME, name).
-define(VALUE, value).
-define(TTL, 8).
-define(FUN_FALSE, fun() -> {false, ?VALUE} end).
-define(FUN_TRUE(TTL), fun() -> {true, ?VALUE, TTL} end).

put_with_ttl_test() ->
    meck:new(time_utils, [passthrough]),
    meck:expect(time_utils, system_time_millis, fun() -> 0 end),
    ok = simple_cache:put(?NAME, ?VALUE, ?TTL),
    {ok, {Value, ?TTL}} = application:get_env(?APP_NAME, ?NAME),
    ?assertEqual(?VALUE, Value),
    ok = meck:unload(time_utils).

put_infinite_test() ->
    ok = simple_cache:put(?NAME, ?VALUE),
    {ok, {Value, infinity}} = application:get_env(?APP_NAME, ?NAME),
    ?assertEqual(?VALUE, Value).

get_exists_ttl_test() ->
    meck:new(time_utils, [passthrough]),
    meck:expect(time_utils, system_time_millis, fun() -> 0 end),
    simple_cache:put(?NAME, ?VALUE, ?TTL),
    {ok, Value} = simple_cache:get(?NAME),
    ?assertEqual(?VALUE, Value),
    ok = meck:unload(time_utils).

get_exists_infinite_test() ->
    meck:new(time_utils, [passthrough]),
    meck:expect(time_utils, system_time_millis, fun() -> ?TTL end),
    simple_cache:put(?NAME, ?VALUE),
    {ok, Value} = simple_cache:get(?NAME),
    ?assertEqual(?VALUE, Value),
    ok = meck:unload(time_utils).

get_invalid_expired_test() ->
    meck:new(time_utils, [passthrough]),
    meck:expect(time_utils, system_time_millis, fun() -> 0 end),
    simple_cache:put(?NAME, ?VALUE, ?TTL),
    meck:expect(time_utils, system_time_millis, fun() -> ?TTL end),
    {error, Value} = simple_cache:get(?NAME),
    ?assertEqual(not_found, Value),
    ok = meck:unload(time_utils).
    
clear_test() ->
    simple_cache:put(?NAME, ?VALUE),
    simple_cache:clear(?NAME),
    ?assertEqual(undefined, application:get_env(?APP_NAME, ?NAME)).

get_invalid_non_exists_test() ->
    {error, Value} = simple_cache:get(?NAME),
    ?assertEqual(not_found, Value).

get_default_without_put_test() ->
    {ok, Value} = simple_cache:get(?NAME, ?FUN_FALSE),
    ?assertEqual(?VALUE, Value),
    ?assertEqual(undefined, application:get_env(?APP_NAME, ?NAME)).
    
get_default_with_put_ttl_test() ->
    meck:new(time_utils, [passthrough]),
    meck:expect(time_utils, system_time_millis, fun() -> 0 end),
    {ok, Value} = simple_cache:get(?NAME, ?FUN_TRUE(?TTL)),
    ?assertEqual(?VALUE, Value),
    {ok, {CachedValue, TTL}} = application:get_env(?APP_NAME, ?NAME),
    ?assertEqual(?VALUE, CachedValue),
    ?assertEqual(?TTL, TTL),
    ok = meck:unload(time_utils).

get_default_with_put_infinity_test() ->
    {ok, Value} = simple_cache:get(?NAME, ?FUN_TRUE(infinity)),
    ?assertEqual(?VALUE, Value),
    {ok, {CachedValue, TTL}} = application:get_env(?APP_NAME, ?NAME),
    ?assertEqual(?VALUE, CachedValue),
    ?assertEqual(infinity, TTL).
