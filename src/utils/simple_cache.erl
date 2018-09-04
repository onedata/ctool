%%%-------------------------------------------------------------------
%%% @author Micha; Stanisz
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains functions for simple cache operations.
%%% @end
%%%-------------------------------------------------------------------
-module(simple_cache).
-author("Michal Stanisz").

%% API
-export([get/1, get/2, put/2, put/3, clear/1]).

-define(APP_NAME, simple_cache).

-type key() :: atom() | {atom(), term()}.
-type ttl() :: non_neg_integer() | infinity.
-type value() :: term().


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% If cache exists and is not expired returns cached value. 
%% @end
%%--------------------------------------------------------------------
-spec get(key()) -> {ok, value()} | {error, not_found}.
get({Key, ChildKey}) ->
    case maps:find(map, application:get_env(?APP_NAME, Key, #{})) of
        {ok, Map} -> check_validity(maps:find(ChildKey, Map));
        _ -> {error, not_found}
    end;
get(Key) ->
    check_validity(maps:find(single_value, application:get_env(?APP_NAME, Key, #{}))).

%%--------------------------------------------------------------------
%% @doc
%% Returns cached or calculated value.
%% DefaultValue function is used for calculation of value if cache does 
%% not exist or is expired. 
%% This function should return a tuple {true, Value} or {true, Value, TTL} 
%% if Value is to be stored in cache otherwise it should return {false, Value}.
%% {true, Value} is equivalent to {true, Value, infinity}.
%% @end
%%--------------------------------------------------------------------
-spec get(key(), fun(() -> 
    {true, value()} | 
    {true, value(), ttl()} | 
    {false, value()})) -> {ok, value()}.
get(Key, DefaultValue) ->
    case ?MODULE:get(Key) of
        {ok, Value} ->
            {ok, Value};
        {error, not_found} ->
            case DefaultValue() of
                {false, Value} ->
                    {ok, Value};
                {true, Value, TTL} ->
                    ?MODULE:put(Key, Value, TTL),
                    {ok, Value};
                {true, Value} ->
                    ?MODULE:put(Key, Value, infinity),
                    {ok, Value};
                Error ->
                    Error
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% @equiv set_cached_value(Name, Value, infinity).
%% @end
%%--------------------------------------------------------------------
-spec put(key(), value()) -> ok.
put(Key, Value) ->
    put(Key, Value, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Stores value in cache for given time in milliseconds or infinitely.
%% @end
%%--------------------------------------------------------------------
-spec put(key(), value(), ttl()) -> ok.
put(Key, Value, TTL) ->
    ValidUntil = case TTL of
        infinity -> infinity;
        _ -> time_utils:system_time_millis() + TTL
    end,
    do_put(Key, Value, ValidUntil).


%%--------------------------------------------------------------------
%% @doc
%% Invalidates cache.
%% @end
%%--------------------------------------------------------------------
-spec clear(key()) -> ok.
clear({Key, ChildKey}) ->
    case application:get_env(?APP_NAME, Key) of
        {ok, CacheMap} -> 
            case maps:find(map, CacheMap) of
                {ok, Map} -> application:set_env(?APP_NAME, Key, maps:put(map, maps:remove(ChildKey, Map), CacheMap));
                _ -> ok
            end;
        _ -> 
            ok
    end;
clear(Key) ->
    case application:get_env(?APP_NAME, Key) of
        {ok, CacheMap} ->
            application:set_env(?APP_NAME, Key, maps:remove(single_value, CacheMap));
        _ ->
            ok
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns cached value if given record is valid.
%% @end
%%--------------------------------------------------------------------
-spec check_validity({ok, {value(), ttl()}} | error) -> {ok, value()} | {error, not_found}.
check_validity(Record) ->
    Now = time_utils:system_time_millis(),
    case Record of
        {ok, {Value, infinity}} -> {ok, Value};
        {ok, {Value, ValidUntil}} when ValidUntil > Now -> {ok, Value};
        _ -> {error, not_found}
    end.

-spec do_put(Key :: key(), Value :: value(), ValidUntil :: ttl()) -> ok.
do_put({Key, ChildKey}, Value, ValidUntil) ->
    CacheMap = application:get_env(?APP_NAME, Key, #{}),
    Map = maps:get(map, CacheMap, #{}),
    NewCacheMap = maps:put(map, maps:put(ChildKey, {Value, ValidUntil}, Map), CacheMap),
    application:set_env(?APP_NAME, Key, NewCacheMap);
do_put(Key, Value, ValidUntil) ->
    CacheMap = application:get_env(?APP_NAME, Key, #{}),
    NewCacheMap = maps:put(single_value, {Value, ValidUntil}, CacheMap),
    application:set_env(?APP_NAME, Key, NewCacheMap).
