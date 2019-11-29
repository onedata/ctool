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
-type kvlist(Key, Value) :: [{Key, Value}].
-type kvlist() :: kvlist(key(), value()).
-type terms() :: [tuple()].

-export_type([key/0, value/0, terms/0, kvlist/0, kvlist/2]).

%% API
-export([hd/1]).
-export([key_get/2, key_get/3, key_store/2, key_store/3]).
-export([union/1, union/2, intersect/2, subtract/2, foldl_while/3]).
-export([ensure_length/2]).


%%%===================================================================
%%% API functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Returns head of the list or 'undefined' if the lists is empty.
%% @end
%%--------------------------------------------------------------------
-spec hd([]) -> undefined; ([T, ...]) -> T.
hd([]) -> undefined;
hd([Head | _]) -> Head.


%%--------------------------------------------------------------------
%% @doc @equiv key_get(Key, KvList, undefined)
%% @end
%%--------------------------------------------------------------------
-spec key_get(Key, KvList :: kvlist(Key, Value)) -> Value.
key_get(Key, KvList) ->
    key_get(Key, KvList, undefined).


%%--------------------------------------------------------------------
%% @doc Returns value associated with provided key form key-value list.
%% If provided key is missing returns default value.
%% @end
%%--------------------------------------------------------------------
-spec key_get(Key, KvList :: kvlist(Key, Value), Default) -> Value | Default.
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
-spec key_store(KVListSrc :: kvlist(K1, V1), KvListDst :: kvlist(K2, V2)) ->
    KvList :: kvlist(K1 | K2, V1 | V2).
key_store(KVListSrc, KvListDst) ->
    lists:foldl(fun({Key, Value}, KvList) ->
        key_store(Key, Value, KvList)
    end, KvListDst, KVListSrc).


%%--------------------------------------------------------------------
%% @doc Stores {Key, Value} pair in the provided key-value KvList.
%% If Key already exists in the KvList it will be overwritten.
%% @end
%%--------------------------------------------------------------------
-spec key_store(Key, Value, KvList :: kvlist(Key, Value)) -> kvlist(Key, Value).
key_store(Key, Value, KvList) ->
    lists:keystore(Key, 1, KvList, {Key, Value}).


%%--------------------------------------------------------------------
%% @doc @equiv union([List1, List2])
%% @end
%%--------------------------------------------------------------------
-spec union(List1 :: [T], List2 :: [T]) -> [T].
union(List1, List2) ->
    union([List1, List2]).

%%--------------------------------------------------------------------
%% @doc Returns a union of lists without duplicates.
%% @end
%%--------------------------------------------------------------------
-spec union(ListOfLists :: [[T]]) -> [T].
union(ListOfLists) ->
    Union = ordsets:union(
        lists:map(fun ordsets:from_list/1, ListOfLists)
    ),
    ordsets:to_list(Union).


%%--------------------------------------------------------------------
%% @doc Returns a deduplicated list of elements from the List1
%% which belong to the List2.
%% @end
%%--------------------------------------------------------------------
-spec intersect(List1 :: [T], List2 :: [T]) -> [T].
intersect(List1, List2) ->
    ordsets:to_list(ordsets:intersection(
        ordsets:from_list(List1), ordsets:from_list(List2)
    )).


%%--------------------------------------------------------------------
%% @doc Returns a deduplicated list of elements from the List1 that
%% do not belong to the List2.
%% @end
%%--------------------------------------------------------------------
-spec subtract(List1 :: [T], List2 :: [T]) -> [T].
subtract(List1, List2) ->
    ordsets:to_list(ordsets:subtract(
        ordsets:from_list(List1), ordsets:from_list(List2)
    )).


%%--------------------------------------------------------------------
%% @doc Shortens or duplicates list to ensure exact number of elements.
%% For example:
%% (5, [a, b]) -> [a, b, a, b, a]
%% (1, [a, b]) -> [a]
%% (9, []) -> error(badarg)
%% @end
%%--------------------------------------------------------------------
-spec ensure_length(TargetLength :: non_neg_integer(), nonempty_list(X)) -> [X].
ensure_length(0, _) -> [];
ensure_length(TargetLength, []) -> error(badarg, [TargetLength, []]);
ensure_length(TargetLength, List) ->
    Repeats = utils:ceil(TargetLength / length(List)),
    lists:sublist(lists:append(lists:duplicate(Repeats, List)), TargetLength).


%%--------------------------------------------------------------------
%% @doc
%% Foldls the list until Fun returns {halt, Term}.
%% The return value for Fun is expected to be
%% {cont, Acc} to continue the fold with Acc as the new accumulator or
%% {halt, Acc} to halt the fold and return Acc as the return value of this function
%% @end
%%--------------------------------------------------------------------
-spec foldl_while(F, Accu, List) -> Accu1 when
    F :: fun((Elem :: T, AccIn) -> AccOut),
    Accu :: term(), Accu1 :: term(),
    AccIn :: term(), AccOut :: {cont, term()} | {halt, term()},
    List :: [T], T :: term().
foldl_while(F, Accu, List) ->
    do_foldl(F, {cont, Accu}, List).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec do_foldl(F, AccOut, List) -> AccIn when
    F :: fun((Elem :: T, AccIn) -> AccOut),
    AccIn :: term(), AccOut :: {cont, term()} | {halt, term()},
    List :: [T], T :: term().
do_foldl(_F, {halt, Accu}, _) -> Accu;
do_foldl(_F, {cont, Accu}, []) -> Accu;
do_foldl(F, {cont, Accu}, [Hd | Tail]) -> do_foldl(F, F(Hd, Accu), Tail).
