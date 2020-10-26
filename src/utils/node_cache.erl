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
-export([get/1, get/2, put/2, put/3, clear/1]).
% for mocking in tests
-export([now/0]).

-type key() :: term().
-type value() :: term().
-type ttl() :: time_utils:millis() | infinity.
-type value_provider() :: fun(() ->
        {true, value()} |
        {true, value(), ttl()} |
        {false, value()} |
        {error, term()}).

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
    node_cache = ets:new(node_cache, [set, public, named_table]),
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
-spec get(key()) -> {ok, value()} | ?ERROR_NOT_FOUND.
get(Key) ->
    check_validity(
        case ets:lookup(?MODULE, Key) of
            [{Key, Value}] -> {ok, Value};
            [] -> ?ERROR_NOT_FOUND
        end
    ).

%%--------------------------------------------------------------------
%% @doc
%% Returns cached or calculated value. ValueProvider function is used if
%% the cache does not exist or is expired to calculate the value and caching
%% time. The function should return a tuple {true, Value} or {true, Value, TTL}
%% if Value is to be stored in the cache, otherwise {false, Value}.
%% {true, Value} is equivalent to {true, Value, infinity}.
%% @end
%%--------------------------------------------------------------------
-spec get(key(), value_provider()) -> {ok, value()} | {error, term()}.
get(Key, ValueProvider) when is_function(ValueProvider, 0) ->
    case get(Key) of
        {ok, Value} ->
            {ok, Value};
        ?ERROR_NOT_FOUND ->
            case ValueProvider() of
                {false, Value} ->
                    {ok, Value};
                {true, Value, TTL} ->
                    put(Key, Value, TTL, new),
                    {ok, Value};
                {true, Value} ->
                    put(Key, Value, infinity, new),
                    {ok, Value};
                Error ->
                    Error
            end
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
%% @equiv put(Name, Value, infinity, replace).
%% @end
%%--------------------------------------------------------------------
-spec put(key(), value(), ttl()) -> ok.
put(Key, Value, TTL) ->
    put(Key, Value, TTL, replace).

%%--------------------------------------------------------------------
%% @doc
%% Stores value in cache for given time in milliseconds or infinitely.
%% Distinction for new and replace is introduced only to appease dialyzer 
%% which does not allow for calling ets:insert directly after ets:lookup.
%% @end
%%--------------------------------------------------------------------
-spec put(key(), value(), ttl(), new | replace) -> ok.
put(Key, Value, TTL, Mode) ->
    ValidUntil = case TTL of
        infinity -> infinity;
        _ -> ?NOW() + TTL
    end,
    do_put(Key, {Value, ValidUntil}, Mode), 
    ok.


-spec clear(key()) -> ok.
clear(Key) ->
    true == ets:delete(?MODULE, Key),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Insert given value to ets.
%% Distinction for new and replace is introduced only to appease dialyzer 
%% which does not allow for calling ets:insert directly after ets:lookup.
%% @end
%%--------------------------------------------------------------------
-spec do_put(key(), {value(), ttl()}, new | replace) -> boolean().
do_put(Key, Value, new) ->
    ets:insert_new(?MODULE, {Key, Value});
do_put(Key, Value, _) ->
    true == ets:insert(?MODULE, {Key, Value}).

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
        false -> ?ERROR_NOT_FOUND
    end;
check_validity(_) ->
    ?ERROR_NOT_FOUND.


%% @private
%% Exported for eunit tests and called by ?MODULE
-spec now() -> time_utils:millis().
now() ->
    erlang:system_time(millisecond).