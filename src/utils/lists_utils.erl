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
-export([union/1, union/2, intersect/2, subtract/2]).
-export([ensure_length/2, number_items/1]).
-export([shuffle/1, random_element/1, random_sublist/1, random_sublist/3]).
-export([foldl_while/3]).


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
%% @doc Adds sequence number to each list element.
%% @end
%%--------------------------------------------------------------------
-spec number_items([T]) -> [{pos_integer(), T}].
number_items(List) ->
    lists:zip(lists:seq(1, length(List)), List).


-spec shuffle([T]) -> [T].
shuffle(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].


-spec random_element([T]) -> T.
random_element([]) ->
    error(badarg);
random_element(List) ->
    lists:nth(rand:uniform(length(List)), List).


%%--------------------------------------------------------------------
%% @doc
%% @equiv random_sublist(List, 0, length(List))
%% @end
%%--------------------------------------------------------------------
-spec random_sublist([T]) -> [T].
random_sublist(List) ->
    random_sublist(List, 0, length(List)).


%%--------------------------------------------------------------------
%% @doc
%% Returns a random sublist of a list, with random length within given limits.
%% @end
%%--------------------------------------------------------------------
-spec random_sublist([T], MinLen :: non_neg_integer(), MaxLen :: all | non_neg_integer()) -> [T].
random_sublist(List, MinLength, all) ->
    random_sublist(List, MinLength, length(List));
random_sublist(List, MinLength, MaxLength) ->
    Shuffled = shuffle(List),
    lists:sublist(Shuffled, MinLength + rand:uniform(MaxLength - MinLength + 1) - 1).


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
