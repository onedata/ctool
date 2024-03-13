%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016-2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module provides an extension of maps module functionality.
%%% @end
%%%--------------------------------------------------------------------
-module(maps_utils).
-author("Krzysztof Trzepla").

%% API
-export([is_empty/1]).
-export([merge/1]).
-export([remove_undefined/1, undefined_to_null/1]).
-export([is_submap/2]).
-export([put_if_defined/3, put_if_defined/4]).
-export([map_key_value/2]).
-export([generate/2, generate_from_list/2]).
-export([random_submap/1, random_submap/3]).
-export([fold_while/3]).
-export([all/2]).
-export([update_existing_key/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec is_empty(#{}) -> boolean().
is_empty(Map) ->
    maps:size(Map) == 0.


%%--------------------------------------------------------------------
%% @doc
%% Removes undefined values from the map.
%% @end
%%--------------------------------------------------------------------
-spec remove_undefined(#{K => V | undefined}) -> #{K => V}.
remove_undefined(Map) ->
    maps:filter(fun
        (_Key, undefined) -> false;
        (_Key, _Value) -> true
    end, Map).


%%--------------------------------------------------------------------
%% @doc
%% Recursively replaces 'undefined' with 'null'.
%% @end
%%--------------------------------------------------------------------
-spec undefined_to_null(map()) -> map().
undefined_to_null(Map) ->
    maps:map(fun
        (_Key, undefined) -> null;
        (_Key, SubMap) when is_map(SubMap) -> undefined_to_null(SubMap);
        (_Key, Value) -> Value
    end, Map).


%%-------------------------------------------------------------------
%% @doc
%% Merges a list of maps. Rightmost maps take precedence in case of repeated keys.
%% @end
%%-------------------------------------------------------------------
-spec merge
    ([]) -> #{};
    ([#{Key => Value}, ...]) -> #{Key => Value}.
merge(ListOfMaps) ->
    lists:foldr(fun maps:merge/2, #{}, ListOfMaps).


%%-------------------------------------------------------------------
%% @doc
%% Checks if all associations from the first map exists in the second one.
%% @end
%%-------------------------------------------------------------------
-spec is_submap(#{Key => Value}, #{Key => Value}) -> boolean().
is_submap(Smaller, Bigger) ->
    Smaller =:= maps:with(maps:keys(Smaller), Bigger).


-spec put_if_defined(map(), term(), term()) -> map().
put_if_defined(Map, Key, Value) ->
    put_if_defined(Map, Key, Value, undefined).


-spec put_if_defined(map(), term(), term(), term()) -> map().
put_if_defined(Map, _Key, UndefinedValue, UndefinedValue) ->
    Map;
put_if_defined(Map, Key, DefinedValue, _UndefinedValue) ->
    maps:put(Key, DefinedValue, Map).


%% see notes for generate_from_list/2
-spec map_key_value(fun((OldKey, OldValue) -> {NewKey, NewValue}), #{OldKey => OldValue}) -> #{NewKey => NewValue}.
map_key_value(MapKeyValueFun, Map) ->
    generate_from_list(fun({OldKey, OldValue}) ->
        MapKeyValueFun(OldKey, OldValue)
    end, maps:to_list(Map)).


-spec generate(fun(() -> {Key, Value}) | fun((Ordinal :: non_neg_integer()) -> {Key, Value}), non_neg_integer()) ->
    #{Key => Value}.
generate(Generator, Count) ->
    generate_from_list(fun
        (Ordinal) when is_function(Generator, 1) -> Generator(Ordinal);
        (_Ordinal) when is_function(Generator, 0) -> Generator()
    end, lists:seq(1, Count)).


%% NOTE: this implementation (building a proplist and then calling maps:from_list/2) performs much better
%%       than building a map key-by-key using a fold function that inserts a new key every time
%%       (effectively destroying the old map and building a new one)
-spec generate_from_list(fun((Element) -> Value | {Key, Value}), [Element]) -> #{Element | Key => Value}.
generate_from_list(Generator, Elements) ->
    maps:from_list(lists:map(fun(Element) ->
        case Generator(Element) of
            {Key, Value} -> {Key, Value};
            Value -> {Element, Value}
        end
    end, Elements)).


-spec random_submap(#{Key => Value}) -> #{Key => Value}.
random_submap(Map) ->
    random_submap(Map, 0, maps:size(Map)).

-spec random_submap(#{Key => Value}, non_neg_integer(), all | non_neg_integer()) -> #{Key => Value}.
random_submap(Map, MinSize, all) ->
    random_submap(Map, MinSize, maps:size(Map));
random_submap(Map, MinSize, MaxSize) ->
    maps:with(lists_utils:random_sublist(maps:keys(Map), MinSize, MaxSize), Map).


%%--------------------------------------------------------------------
%% @doc
%% Works like fold, but allows stopping the folding with `{halt, Acc}`
%% to immediately return the Acc. Otherwise, `{cont, Acc}` should be used
%% to continue the folding.
%% @end
%%--------------------------------------------------------------------
-spec fold_while(fun((K, V, Acc) -> {cont, Acc} | {halt, Acc}), Acc, #{K => V}) -> Acc.
fold_while(Fun, InitialAcc, Map) ->
    do_fold_while(Fun, {cont, InitialAcc}, maps:iterator(Map)).

%% @private
-spec do_fold_while(fun((K, V, Acc) -> {cont, Acc} | {halt, Acc}), Acc, maps:iterator(K, V)) -> Acc.
do_fold_while(_Fun, {halt, Acc}, _) ->
    Acc;
do_fold_while(Fun, {cont, Acc}, Iterator) ->
    case maps:next(Iterator) of
        none ->
            Acc;
        {Key, Value, NewIterator} ->
            do_fold_while(Fun, Fun(Key, Value, Acc), NewIterator)
    end.


-spec all(fun((K, V) -> boolean()), #{K => V}) -> boolean().
all(Predicate, Map) ->
    fold_while(fun(Key, Value, _) ->
        case Predicate(Key, Value) of
            true -> {cont, true};
            false -> {halt, false}
        end
    end, true, Map).


%% @formatter:off
-spec update_existing_key(#{X => Y}, X, Y) -> #{X => Y}.
update_existing_key(Map, Key, Value) when is_map_key(Key, Map) -> Map#{Key => Value};
update_existing_key(Map, _Key, _Value)                         -> Map.
%% @formatter:on
