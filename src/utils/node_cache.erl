%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2018-2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc 
%%% This module implements a node-wide cache based on ets. 
%%% Entries can be cached infinitely or with desired TTL.
%%% Before use cache must be initialized, see {@link init/0}.
%%%
%%% Operation atomicity is achieved using critical sections,
%%% hence poor performance should be expected. This applies to
%%% update/2,3 and acquire/2 (but if there is no cached value).
%%% @end
%%%-------------------------------------------------------------------
-module(node_cache).
-author("Michal Stanisz").

-include("errors.hrl").

%% API
-export([init/0, destroy/0]).
-export([put/2, put/3]).
-export([get/1, get/2]).
-export([update/2, update/3]).
-export([acquire/2]).
-export([clear/1]).

-type key() :: term().
-type value() :: term().
-type ttl() :: time:seconds() | time:infinity().

-type update_callback() :: fun((value()) ->
    {ok, value(), ttl()} |
    {error, Reason :: term()}).

%% function called by acquire/2 when there is no valid value in cache
-type acquire_callback() :: fun(() ->
    {ok, value(), ttl()} |
    {error, Reason :: term()}).

-export_type([update_callback/0, acquire_callback/0]).

-define(critical_section(Key, Fun), global:trans({{?MODULE, ?FUNCTION_NAME, Key}, self()}, Fun, [node()])).

-compile([{no_auto_import, [get/1]}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Must be called before cache can be used. Should be done in a 
%% long-living process (like application supervisor). Cache will 
%% be destroyed automatically when calling process dies.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    ?MODULE = ets:new(
        ?MODULE,
        [set, public, named_table, {read_concurrency, true}]
    ),
    ok.


-spec destroy() -> ok.
destroy() ->
    true = ets:delete(?MODULE),
    ok.


-spec put(key(), value()) -> ok.
put(Key, Value) ->
    put(Key, Value, infinity).

-spec put(key(), value(), ttl()) -> ok.
put(Key, Value, TTL) ->
    insert_into_cache(Key, Value, TTL).


-spec get(key()) -> value() | no_return().
get(Key) ->
    case lookup_in_cache(Key) of
        {true, Value} -> Value;
        false -> error({badkey, Key})
    end.


-spec get(key(), Default) -> value() | Default.
get(Key, Default) ->
    case lookup_in_cache(Key) of
        {true, Value} -> Value;
        false -> Default
    end.


-spec update(key(), update_callback()) -> {ok, value()} | {error, term()}.
update(Key, UpdateCallback) ->
    update_internal(Key, fun() -> get(Key) end, UpdateCallback).

-spec update(key(), update_callback(), value()) -> {ok, value()} | {error, term()}.
update(Key, UpdateCallback, DefaultInitialValue) ->
    update_internal(Key, fun() -> get(Key, DefaultInitialValue) end, UpdateCallback).


%%--------------------------------------------------------------------
%% @doc
%% If there is a still valid, cached value for given key, it is 
%% returned. Otherwise AcquireCallback is executed and returned
%% value is cached for provided time (unless error was returned).
%% @end
%%--------------------------------------------------------------------
-spec acquire(key(), acquire_callback()) -> {ok, value()} | {error, term()}.
acquire(Key, AcquireCallback) when is_function(AcquireCallback, 0) ->
    case lookup_in_cache(Key) of
        {true, Value} ->
            {ok, Value};
        false ->
            ?critical_section(Key, fun() ->
                % avoid unnecessary overwrites in case of parallel executions
                case lookup_in_cache(Key) of
                    {true, Value} ->
                        {ok, Value};
                    false ->
                        case AcquireCallback() of
                            {ok, Value, TTL} ->
                                put(Key, Value, TTL),
                                {ok, Value};
                            {error, _} = Error ->
                                Error
                        end
                end
            end)
    end.


-spec clear(key()) -> ok.
clear(Key) ->
    true = ets:delete(?MODULE, Key),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec insert_into_cache(key(), value(), ttl()) -> ok.
insert_into_cache(Key, Value, TTL) ->
    Expiry = case TTL of
        infinity -> infinity;
        _ -> countdown_timer:start_seconds(TTL)
    end,
    true = ets:insert(?MODULE, {Key, {Value, Expiry}}),
    ok.


%% @private
-spec lookup_in_cache(key()) -> {true, value()} | false.
lookup_in_cache(Key) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, {Value, infinity}}] ->
            {true, Value};
        [{Key, {Value, ExpiryTimer}}] ->
            case countdown_timer:is_expired(ExpiryTimer) of
                false -> {true, Value};
                true -> false
            end;
        [] ->
            false
    end.


%% @private
-spec update_internal(key(), fun(() -> value()), update_callback()) -> {ok, value()} | {error, term()}.
update_internal(Key, GetCallback, UpdateCallback) ->
    ?critical_section(Key, fun() ->
        CurrentValue = GetCallback(),
        case UpdateCallback(CurrentValue) of
            {ok, UpdatedValue, TTL} ->
                put(Key, UpdatedValue, TTL),
                {ok, UpdatedValue};
            {error, _} = Error ->
                Error
        end
    end).
