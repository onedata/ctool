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


%%%===================================================================
%%% API
%%%===================================================================

-define(APP_NAME, simple_cache).

%%--------------------------------------------------------------------
%% @doc
%% If cache exists and is not expired returns cached value. 
%% @end
%%--------------------------------------------------------------------
-spec get(Name :: atom()) -> {ok, term()} | {error, not_found}.
get(Name) ->
    Now = time_utils:system_time_millis(),
    case application:get_env(?APP_NAME, Name) of
        {ok, {Value, infinity}} ->
            {ok, Value};
        {ok, {Value, Timestamp}} when Timestamp > Now ->
            {ok, Value};
        _ ->
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns cached or calculated value.
%% DefaultValue function is used for calculation of value if cache does 
%% not exist or is expired. 
%% This function should return a tuple {true, Value} or {true, Value, TTL} 
%% if Value is to be stored in cache otherwise it should return {false, Value}.
%% {true, Value} is equivalent of {true, Value, infinity}.
%% @end
%%--------------------------------------------------------------------
-spec get(Name :: atom(), DefaultValue :: fun(() -> 
    {true, Value :: term()} | 
    {true, Value :: term(), TTL :: integer()} | 
    {false, Value :: term()})
) -> {ok, term()}.
get(Name, DefaultValue) ->
    case ?MODULE:get(Name) of
        {ok, Value} ->
            {ok, Value};
        {error, not_found} ->
            case DefaultValue() of
                {false, Value} ->
                    {ok, Value};
                {true, Value, TTL} ->
                    put(Name, Value, TTL),
                    {ok, Value};
                {true, Value} ->
                    ?MODULE:put(Name, Value, infinity),
                    {ok, Value};
                Error ->
                    Error
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% @equiv set_cached_value(AppName, Name, Value, infinite).
%% @end
%%--------------------------------------------------------------------
-spec put(Name :: atom(), Value :: term()) -> term().
put(Name, Value) ->
    put(Name, Value, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Stores value in cache for given time in milliseconds or infinitely.
%% @end
%%--------------------------------------------------------------------
-spec put(Name :: atom(), Value :: term(), TTL :: non_neg_integer() | infinity) -> term().
put(Name, Value, TTL) ->
    ValidUntil = case TTL of
        infinity ->
            infinity;
        _ ->
            time_utils:system_time_millis() + TTL
    end,
    application:set_env(?APP_NAME, Name, {Value, ValidUntil}).

%%--------------------------------------------------------------------
%% @doc
%% Invalidates cache.
%% @end
%%--------------------------------------------------------------------
-spec clear(Name :: atom()) -> ok.
clear(Name) ->
    application:unset_env(?APP_NAME, Name).