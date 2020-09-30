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

-type key() :: atom() | {atom(), term()}.
-type value() :: term().
-type ttl() :: time_utils:millis() | infinity.

-define(APP_NAME, simple_cache).
-define(NOW(), time_utils:timestamp_millis()).

-compile([{no_auto_import, [get/1]}]).

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
%% Returns cached or calculated value. ValueProvider function is used if
%% the cache does not exist or is expired to calculate the value and caching
%% time. The function should return a tuple {true, Value} or {true, Value, TTL}
%% if Value is to be stored in the cache, otherwise {false, Value}.
%% {true, Value} is equivalent to {true, Value, infinity}.
%% @end
%%--------------------------------------------------------------------
-spec get(key(), ValueProvider) -> {ok, value()} | Error when
    Error :: term(),
    ValueProvider :: fun(() ->
        {true, value()} |
        {true, value(), ttl()} |
        {false, value()} |
        Error
    ).
get(Key, ValueProvider) ->
    case get(Key) of
        {ok, Value} ->
            {ok, Value};
        {error, not_found} ->
            case ValueProvider() of
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
        _ -> ?NOW() + TTL
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
%% Returns cached value if given entry is valid (has not expired).
%% @end
%%--------------------------------------------------------------------
-spec check_validity({ok, {value(), ttl()}} | error) -> {ok, value()} | {error, not_found}.
check_validity({ok, {Value, infinity}}) ->
    {ok, Value};
check_validity({ok, {Value, ValidUntil}}) ->
    case ValidUntil > ?NOW() of
        true -> {ok, Value};
        false -> {error, not_found}
    end;
check_validity(_) ->
    {error, not_found}.


%% @private
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
