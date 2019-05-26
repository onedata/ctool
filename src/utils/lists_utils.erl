%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains utility functions for lists operations.
%%% @end
%%%--------------------------------------------------------------------
-module(lists_utils).
-author("Krzysztof Trzepla").

-type key() :: term().
-type value() :: term().
-type kvlist() :: [{Key :: key(), Value :: value()}].

%% API
-export([key_get/2, key_get/3, key_store/2, key_store/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @equiv key_get(Key, KvList, undefined)
%% @end
%%--------------------------------------------------------------------
-spec key_get(Key :: key(), KvList :: kvlist()) -> Value :: value().
key_get(Key, KvList) ->
    key_get(Key, KvList, undefined).

%%--------------------------------------------------------------------
%% @doc Returns value associated with provided key form key-value list.
%% If provided key is missing returns default value.
%% @end
%%--------------------------------------------------------------------
-spec key_get(Key :: key(), KvList :: kvlist(), Default :: value()) ->
    Value :: value().
key_get(Key, KvList, Default) ->
    case lists:keyfind(Key, 1, KvList) of
        {Key, Value} -> Value;
        false -> Default
    end.

%%--------------------------------------------------------------------
%% @doc Merge key-value list KVListSrc with KvListDst. If key from KvListSrc
%% already exists in KvListDst it will be overwritten.
%% @end
%%--------------------------------------------------------------------
-spec key_store(KVListSrc :: kvlist(), KvListDst :: kvlist()) ->
    KvList :: kvlist().
key_store(KVListSrc, KvListDst) ->
    lists:foldl(fun({Key, Value}, KvList) ->
        key_store(Key, Value, KvList)
    end, KvListDst, KVListSrc).

%%--------------------------------------------------------------------
%% @doc Stores {Key, Value} pair in the provided key-value KvList.
%% If Key already exists in the KvList it will be overwritten.
%% @end
%%--------------------------------------------------------------------
-spec key_store(Key :: key(), Value :: value(), KvList :: kvlist()) ->
    KvList :: kvlist().
key_store(Key, Value, KvList) ->
    lists:keystore(Key, 1, KvList, {Key, Value}).