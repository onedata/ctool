%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions related to the ctool app.
%%% @end
%%%-------------------------------------------------------------------
-module(ctool).
-author("Lukasz Opiola").

-include("logging.hrl").

-define(APP_NAME, ctool).

%% API
-export([get_env/1, get_env/2, set_env/2, unset_env/1]).
-export([priv_dir/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_env(Key :: atom()) -> term() | no_return().
get_env(Key) ->
    case application:get_env(?APP_NAME, Key) of
        {ok, Value} ->
            Value;
        undefined ->
            ?alert("Could not find required env variable for ctool: ~p", [Key]),
            error({missing_env_variable, Key})
    end.


-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    application:get_env(?APP_NAME, Key, Default).


-spec set_env(Key :: atom(), Value :: term()) -> ok.
set_env(Key, Value) ->
    application:set_env(?APP_NAME, Key, Value).


-spec unset_env(Key :: atom()) -> ok.
unset_env(Key) ->
    application:unset_env(?APP_NAME, Key).


-spec priv_dir() -> file:filename().
priv_dir() ->
    code:priv_dir(?APP_NAME).