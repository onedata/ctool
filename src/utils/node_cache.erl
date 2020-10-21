%%%-------------------------------------------------------------------
%%% @author Micha; Stanisz
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc 
%%% This module implements node cache based on ets.
%%% @end
%%%-------------------------------------------------------------------
-module(node_cache).
-author("Michal Stanisz").

-include("errors.hrl").

%% API
-export([init/0]).
-export([get/1, get/2, put/2, put/3, clear/1]).
% for mocking in tests
-export([now/0]).

-type key() :: atom() | {atom(), term()}.
-type value() :: term().
-type ttl() :: time_utils:millis() | infinity.
-type cache_map() :: #{map => map(), single_value => term()}.

% call using module for mocking in tests
-define(NOW(), ?MODULE:now()).

-compile([{no_auto_import, [get/1]}]).

%%%===================================================================
%%% API
%%%===================================================================

init() ->
    node_cache = ets:new(node_cache, [set, public, named_table]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% If cache exists and is not expired returns cached value.
%% @end
%%--------------------------------------------------------------------
-spec get(key()) -> {ok, value()} | ?ERROR_NOT_FOUND.
get({Key, ChildKey}) ->
    case maps:find(map, get_cache_map(Key)) of
        {ok, Map} -> check_validity(maps:find(ChildKey, Map));
        _ -> {error, not_found}
    end;
get(Key) ->
    check_validity(maps:find(single_value, get_cache_map(Key))).

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
get(Key, ValueProvider) when is_function(ValueProvider, 0)->
    case get(Key) of
        {ok, Value} ->
            {ok, Value};
        {error, not_found} ->
            case ValueProvider() of
                {false, Value} ->
                    {ok, Value};
                {true, Value, TTL} ->
                    put(Key, Value, TTL),
                    {ok, Value};
                {true, Value} ->
                    put(Key, Value, infinity),
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
clear(Key) ->
    case get_cache_map(Key) of
        CacheMap when is_map(CacheMap) -> clear_value(Key, CacheMap);
        _ -> ok
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
-spec check_validity({ok, {value(), ttl()}} | error) -> {ok, value()} | ?ERROR_NOT_FOUND.
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
-spec clear_value(key(), cache_map()) -> ok.
clear_value({Key, ChildKey}, CacheMap) ->
    case maps:find(map, CacheMap) of
        {ok, Map} -> set_cache_map(Key, maps:put(map, maps:remove(ChildKey, Map), CacheMap));
        _ -> ok
    end;
clear_value(Key, CacheMap) ->
    set_cache_map(Key, maps:remove(single_value, CacheMap)).


%% @private
-spec do_put(Key :: key(), Value :: value(), ValidUntil :: ttl()) -> ok.
do_put({Key, ChildKey}, Value, ValidUntil) ->
    CacheMap = get_cache_map(Key),
    Map = maps:get(map, CacheMap, #{}),
    NewCacheMap = maps:put(map, maps:put(ChildKey, {Value, ValidUntil}, Map), CacheMap),
    set_cache_map(Key, NewCacheMap);
do_put(Key, Value, ValidUntil) ->
    CacheMap = get_cache_map(Key),
    NewCacheMap = maps:put(single_value, {Value, ValidUntil}, CacheMap),
    set_cache_map(Key, NewCacheMap).


%% @private
-spec get_cache_map(key()) -> cache_map().
get_cache_map({Key, _}) ->
    get_cache_map(Key);
get_cache_map(Key) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, Value}] -> Value;
        [] -> #{}
    end.


%% @private
-spec set_cache_map(key(), cache_map()) -> ok.
set_cache_map(Key, CacheMap) ->
    true = ets:insert(?MODULE, {Key, CacheMap}),
    ok.


%% @private
-spec now() -> time_utils:millis().
now() ->
    erlang:system_time(millisecond).
