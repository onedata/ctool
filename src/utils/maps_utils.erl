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
-export([merge/1]).
-export([remove_undefined/1, undefined_to_null/1]).
-export([is_submap/2]).
-export([put_if_defined/3, put_if_defined/4]).

%%%===================================================================
%%% API functions
%%%===================================================================

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