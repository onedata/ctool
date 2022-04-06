%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Functions for manipulating nested maps and lists.
%%% Location is defined by a path of nested keys.
%%% Containers can be a mix of lists and maps at different levels.
%%% API follows maps module conventions.
%%%
%%% All intermediate containers traversed in a path must be either a map
%%% or a list of 2-tuples. Otherwise {badnested, term()} is raised.
%%% @end
%%%--------------------------------------------------------------------
-module(kv_utils).
-author("Wojciech Geisler").

%% @formatter:off
-type nested(K, V) :: #{K => V | nested(K, V)} | [{K, V | nested(K, V)}].
-type nested() :: nested(_, _).
-type path(K) :: K | [K].
-type path() :: path(_).

% used in functions transferring values between containers
-type mapping(K1, K2, V) ::
    {From :: path(K1), To :: path(K2)} |
    {From :: path(K1), To :: path(K2), Default :: V}.
%% @formatter:on

-export_type([nested/0, nested/2, path/0, path/1]).

%% API
-export([get/2, get/3, find/2, is_key/2, put/3, update_with/3, update_with/4, remove/2, rename_entry/3]).
-export([copy_all/3, copy_found/2, copy_found/3]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns value at a given path. Raises if the value is missing
%% @end
%%--------------------------------------------------------------------
-spec get(path(K), nested(K, V)) -> V.
get(Key, Nested) when not is_list(Key) ->
    get([Key], Nested);

get(Path, Nested) ->
    case find(Path, Nested) of
        {ok, Found} -> Found;
        error -> error({badkeys, Path})
    end.


%%--------------------------------------------------------------------
%% @doc Returns value at a given path or Default if value is missing.
%% Still raises if some intermediary key exists but does not contain
%% a map or proplist.
%% @end
%%--------------------------------------------------------------------
-spec get(path(K), nested(K, V), Default :: V) -> V.
get(Key, Nested, Default) when not is_list(Key) ->
    get([Key], Nested, Default);

get(Path, Nested, Default) ->
    case find(Path, Nested) of
        {ok, Found} -> Found;
        error -> Default
    end.


%%--------------------------------------------------------------------
%% @doc Returns value at a given path or 'error' if the value
%% or any intermediary is missing.
%% Raises error if an intermediary key exists but is not a valid nested
%% container (cannot be descended into).
%% @end
%%--------------------------------------------------------------------
-spec find(path(K), nested(K, V)) -> {ok, V} | error.
find(Key, Nested) when not is_list(Key) ->
    find([Key], Nested);

find([], Term) ->
    {ok, Term};

find([Key | Tail], Map) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, SubNested} -> find(Tail, SubNested);
        error -> error
    end;

find([Key | Tail], List) when is_list(List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, SubNested} -> find(Tail, SubNested);
        false -> error;
        _Not2Tuple -> error({badnested, List})
    end;

find(_, BadNested) ->
    error({badnested, BadNested}).


-spec is_key(path(K), nested(K, _)) -> boolean().
is_key(Key, Nested) ->
    find(Key, Nested) /= error.


%%--------------------------------------------------------------------
%% @doc Stores given value in a location given by nested keys path.
%% Value existing in the given location is replaced.
%% If any intermediate step encounters an invalid nested container
%% an exception is raised.
%% If intermediate containers are missing, they are created, with
%% the same type as their parent (map in a map, list in a list).
%% @end
%%--------------------------------------------------------------------
-spec put(path(K), Value :: V, nested(K, V)) -> nested(K, V).
put(Key, Value, Nested) when not is_list(Key) ->
    put([Key], Value, Nested);

put([], Value, _Nested) ->
    Value;

put([Key | Tail], Value, Map) when is_map(Map) ->
    SubNested = maps:get(Key, Map, #{}),
    maps:put(Key, put(Tail, Value, SubNested), Map);

put([Key | Tail], Value, List) when is_list(List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, SubNested} ->
            lists:keyreplace(Key, 1, List, {Key, put(Tail, Value, SubNested)});
        false ->
            [{Key, put(Tail, Value, [])} | List];
        _Not2Tuple ->
            error({badnested, List})
    end;

put(_, _, BadNested) ->
    error({badnested, BadNested}).


-spec update_with(path(K), Fun :: fun((OldValue :: term()) -> NewValue :: term()), nested(K, V)) -> nested(K, V).
update_with(Key, Fun, Nested) ->
    case find(Key, Nested) of
        {ok, Value} -> put(Key, Fun(Value), Nested);
        error -> error({badkeys, Key})
    end.


-spec update_with(path(K), Fun :: fun((OldValue :: term()) -> NewValue :: term()), Init :: V, nested(K, V)) -> nested(K, V).
update_with(Key, Fun, Init, Nested) ->
    case find(Key, Nested) of
        {ok, Value} -> put(Key, Fun(Value), Nested);
        error -> put(Key, Init, Nested)
    end.


%%--------------------------------------------------------------------
%% @doc Removes value at given path.
%% Does nothing if one of the keys in path is missing, never throws.
%% @end
%%--------------------------------------------------------------------
-spec remove(path(K1), nested(K1 | K2, V)) -> nested(K2, V).
remove(Key, Nested) when not is_list(Key) ->
    remove([Key], Nested);

remove(Path, Nested) ->
    [Key | RevIntermediate] = lists:reverse(Path),
    % all but last
    Intermediate = lists:reverse(RevIntermediate),
    try find(Intermediate, Nested) of
        {ok, List} when is_list(List) ->
            put(Intermediate, keydelete(Key, List), Nested);
        {ok, Map} when is_map(Map) ->
            put(Intermediate, maps:remove(Key, Map), Nested);
        _ ->
            Nested
    catch error:_ ->
        Nested
    end.


%%--------------------------------------------------------------------
%% @doc Removes value from the old location and inserts at new.
%% Returns error if the value was not found.
%% @end
%%--------------------------------------------------------------------
-spec rename_entry(OldPath :: path(K), NewPath :: path(K), nested(K, V)) ->
    {ok, nested(K, V)} | error.
rename_entry(OldPath, NewPath, Nested) ->
    case find(OldPath, Nested) of
        {ok, Found} -> {ok, put(NewPath, Found, remove(OldPath, Nested))};
        error -> error
    end.


%%--------------------------------------------------------------------
%% @doc Copies values between containers according to given paths mapping.
%% Overwrites existing values at Target path.
%% Raises error when value is missing from the Source container.
%% @end
%%--------------------------------------------------------------------
-spec copy_all([mapping(K1, K2, V)],
    Source :: nested(K1, V), Target :: nested(K2, V)) -> nested(K2, V).
copy_all(Mappings, Source, Target) ->
    lists:foldl(fun
        ({From, To}, TargetAcc) ->
            put(To, get(From, Source), TargetAcc);
        ({From, To, Default}, TargetAcc) ->
            put(To, get(From, Source, Default), TargetAcc)
    end, Target, Mappings).


%%--------------------------------------------------------------------
%% @doc Returns new map with requested keys, skipping missing entries.
%% {@equiv copy_found(Mappings, Source, #{})}
%% @end
%%--------------------------------------------------------------------
-spec copy_found([mapping(K1, K2, V)], Source :: nested(K1, V)) -> nested(K2, V).
copy_found(Mappings, Source) ->
    copy_found(Mappings, Source, #{}).


%%--------------------------------------------------------------------
%% @doc Copies values between containers according to given paths mapping.
%% Silently skips values missing in the source container.
%% @end
%%--------------------------------------------------------------------
-spec copy_found([mapping(K1, K2, V)],
    Source :: nested(K1, V), Target :: nested(K2, V)) -> nested(K2, V).
copy_found(Mappings, Source, Target) ->
    lists:foldl(fun
        ({From, To}, TargetAcc) ->
            case find(From, Source) of
                {ok, Value} -> put(To, Value, TargetAcc);
                error -> TargetAcc
            end;
        ({From, To, Default}, TargetAcc) ->
            put(To, get(From, Source, Default), TargetAcc)
    end, Target, Mappings).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Removes all tuples whose first element matches Key.
%% This differs from lists:keydelete which removes only the first occurrence.
%% @end
%%--------------------------------------------------------------------
-spec keydelete(term(), list()) -> list().
keydelete(Key, List) ->
    lists:filter(fun
        (Elem) when element(1, Elem) == Key -> false;
        (_) -> true
    end, List).

