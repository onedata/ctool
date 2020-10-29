%%%-------------------------------------------------------------------
%%% @author Micha; Stanisz
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc 
%%% This module implements a node-wide cache based on ets.
%%% @end
%%%-------------------------------------------------------------------
-module(node_cache).
-author("Michal Stanisz").

-include("errors.hrl").

%% API
-export([init/0, destroy/0]).
-export([get/1, get/2, acquire/2, put/2, put/3, clear/1]).
% for mocking in tests
-export([now/0]).

-type key() :: term().
-type value() :: term().
-type ttl() :: time_utils:seconds() | infinity.

%% function called by acquire/2 when there is no valid value in cache
-type acquire_callback() :: fun(() ->
        {ok, value(), ttl()} |
        {error, Reason :: term()}).

% call using module for mocking in tests
-define(NOW(), ?MODULE:now()).

-compile([{no_auto_import, [get/1]}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Must be called before cache can be used. Should be done in a 
%% long-living process(like application supervisor). Cache will 
%% be destroyed automatically when calling process dies.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    node_cache = ets:new(
        node_cache, 
        [set, public, named_table, {read_concurrency, true}]
    ),
    ok.


-spec destroy() -> ok.
destroy() ->
    true = ets:delete(node_cache),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% If cache exists and is not expired returns cached value.
%% @end
%%--------------------------------------------------------------------
-spec get(key()) -> value() | no_return().
get(Key) ->
    case do_get(Key) of
        {ok, Value} -> Value;
        ?ERROR_NOT_FOUND -> error({badkey, Key})
    end.


%%--------------------------------------------------------------------
%% @doc
%% If cache exists and is not expired returns cached value otherwise 
%% returns provided default value.
%% @end
%%--------------------------------------------------------------------
-spec get(key(), Default) -> value() | Default.
get(Key, Default) ->
    case do_get(Key) of
        {ok, Value} -> Value;
        ?ERROR_NOT_FOUND -> Default
    end.


%%--------------------------------------------------------------------
%% @doc
%% If cache exists and is not expired returns cached value. 
%% Otherwise AcquireCallback is executed and returned value 
%% is cached for provided time (unless error was returned).
%% 
%% NOTE: This function is NOT atomic so calling it in parallel 
%% might result in multiple AcquireCallback executions.
%% In that case only one value will be stored in cache.
%% @end
%%--------------------------------------------------------------------
-spec acquire(key(), acquire_callback()) -> {ok, value()} | {error, term()}.
acquire(Key, AcquireCallback) when is_function(AcquireCallback, 0) ->
    case do_get(Key) of
        ?ERROR_NOT_FOUND ->
            case AcquireCallback() of
                {ok, Value, TTL} ->
                    put(Key, Value, TTL),
                    {ok, Value};
                {error, _} = Error ->
                    Error
            end;
        {ok, Value} ->
            {ok, Value}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @equiv put(Name, Value, infinity).
%% @end
%%--------------------------------------------------------------------
-spec put(key(), value()) -> ok.
put(Key, Value) ->
    put(Key, Value, infinity).


%%--------------------------------------------------------------------
%% @doc
%% Stores value in cache for given time in milliseconds or infinitely.
%% @end
-spec put(key(), value(), ttl()) -> ok.
put(Key, Value, TTL) ->
    ValidUntil = case TTL of
        infinity -> infinity;
        _ -> ?NOW() + TTL
    end,
    true = ets:insert(?MODULE, {Key, {Value, ValidUntil}}),
    ok.


-spec clear(key()) -> ok.
clear(Key) ->
    true = ets:delete(?MODULE, Key),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec do_get(key()) -> {ok, value()} | ?ERROR_NOT_FOUND.
do_get(Key) ->
    check_validity(
        case ets:lookup(?MODULE, Key) of
            [{Key, ValueAndTtl}] -> {ok, ValueAndTtl};
            [] -> ?ERROR_NOT_FOUND
        end
    ).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns cached value if given entry is valid (has not expired).
%% @end
%%--------------------------------------------------------------------
-spec check_validity({ok, {value(), ttl()}} | ?ERROR_NOT_FOUND) -> 
    {ok, value()} | ?ERROR_NOT_FOUND. 
check_validity({ok, {Value, infinity}}) ->
    {ok, Value};
check_validity({ok, {Value, ValidUntil}}) ->
    case ValidUntil > ?NOW() of
        true -> {ok, Value};
        false -> ?ERROR_NOT_FOUND
    end;
check_validity(_) ->
    ?ERROR_NOT_FOUND.


%% @private
%% Exported for eunit tests and called by ?MODULE
-spec now() -> time_utils:millis().
now() ->
    erlang:system_time(second).