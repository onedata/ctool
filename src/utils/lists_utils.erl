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

-include("logging.hrl").

-type key() :: term().
-type value() :: term().
-type kvlist(Key, Value) :: [{Key, Value}].
-type kvlist() :: kvlist(key(), value()).
-type terms() :: [tuple()].

-export_type([key/0, value/0, terms/0, kvlist/0, kvlist/2]).

%% API
-export([hd/1]).
-export([union/1, union/2, intersect/2, subtract/2]).
-export([is_subset/2]).
-export([replace/3, replace_at/3]).
-export([ensure_length/2, enumerate/1, index_of/2]).
-export([shuffle/1, random_element/1, random_sublist/1, random_sublist/3]).
-export([pmap/2, pforeach/2, pfiltermap/2, pfiltermap/3]).
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
    ordsets:to_list(ordsets:union(
        lists:map(fun ordsets:from_list/1, ListOfLists)
    )).


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


-spec is_subset(Subset :: list(), AllElements :: list()) -> boolean().
is_subset(Subset, AllElements) ->
    lists:all(fun(X) -> lists:member(X, AllElements) end, Subset).


%%--------------------------------------------------------------------
%% @doc Replaces the first occurrence of Element with Replacement (if any).
%% @end
%%--------------------------------------------------------------------
-spec replace(T, T, [T]) -> [T].
replace(Element, Replacement, [Element | T]) ->
    [Replacement | T];
replace(Element, Replacement, [H | T]) ->
    [H | replace(Element, Replacement, T)];
replace(_Element, _Replacement, []) ->
    [].


-spec replace_at(term(), pos_integer(), [term()]) -> [term()].
replace_at(NewValue, 1, [_ | Rest]) ->
    [NewValue | Rest];
replace_at(NewValue, Index, [Element | Rest]) ->
    [Element | replace_at(NewValue, Index - 1, Rest)];
replace_at(_NewValue, _Index, []) ->
    error(badarg).


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


-spec enumerate([T]) -> [{pos_integer(), T}].
enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).


-spec index_of(term(), [term()]) -> pos_integer() | undefined.
index_of(_Element, []) ->
    undefined;
index_of(Element, List) ->
    Index = foldl_while(fun(E, Offset) ->
        case E =:= Element of
            true -> {halt, Offset};
            false -> {cont, Offset + 1}
        end
    end, 1, List),
    case Index > length(List) of
        true -> undefined;
        false -> Index
    end.


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
%% A parallel version of lists:map/2 - each element is processed by
%% a new async process. Raises an error if any of the processes crash
%% or a process somehow dies without reporting back.
%% @end
%%--------------------------------------------------------------------
-spec pmap(fun((X) -> Y), [X]) -> [Y] | no_return().
pmap(Fun, Elements) ->
    Parent = self(),
    Ref = erlang:make_ref(),

    Pids = lists:map(fun(Element) ->
        spawn(fun() ->
            Result = try
                Fun(Element)
            catch Type:Reason:Stacktrace ->
                {'$pmap_error', self(), Type, Reason, Stacktrace}
            end,
            Parent ! {Ref, self(), Result}
        end)
    end, Elements),

    Gather = fun
        % PidsOrResults is initially the list of pids, gradually replaced by corresponding results
        F(PendingPids = [_ | _], PidsOrResults) ->
            receive
                {Ref, Pid, Result} ->
                    NewPidsOrResults = lists_utils:replace(Pid, Result, PidsOrResults),
                    F(lists:delete(Pid, PendingPids), NewPidsOrResults)
            after 5000 ->
                case lists:any(fun erlang:is_process_alive/1, PendingPids) of
                    true ->
                        F(PendingPids, PidsOrResults);
                    false ->
                        error({parallel_call_failed, {processes_dead, Pids}})
                end
            end;
        % wait for all pids to report back and then look for errors
        F([], AllResults) ->
            Errors = lists:filtermap(fun
                ({'$pmap_error', Pid, Type, Reason, Stacktrace}) ->
                    {true, {Pid, Type, Reason, Stacktrace}};
                (_) ->
                    false
            end, AllResults),
            case Errors of
                [] ->
                    AllResults;
                _ ->
                    error({parallel_call_failed, {failed_processes, Errors}})
            end
    end,
    Gather(Pids, Pids).


%%--------------------------------------------------------------------
%% @doc
%% A parallel version of lists:foreach/2 - each element is processed by
%% a new async process. Raises an error if any of the processes crash
%% or a process somehow dies without reporting back.
%% @end
%%--------------------------------------------------------------------
-spec pforeach(fun((X) -> term()), [X]) -> ok | no_return().
pforeach(Fun, Elements) ->
    pmap(Fun, Elements),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% A parallel version of lists:filtermap/2 - elements are processed by
%% a limited number of processes. Raises an error if any of the processes
%% crash or a process somehow dies without reporting back.
%% @end
%%--------------------------------------------------------------------
-spec pfiltermap(
    Fun :: fun((X :: A) -> {true, Y :: B} | false),
    Elements :: [X :: A],
    MaxProcesses :: pos_integer()
) -> [X :: B].
pfiltermap(Fun, Elements, MaxProcesses)
    when is_integer(MaxProcesses)
    andalso MaxProcesses > 0
->
    Length = length(Elements),
    case Length > MaxProcesses of
        true ->
            {L1, L2} = lists:split(MaxProcesses, Elements),
            %% TODO VFS-7568 use tail recursion
            pfiltermap(Fun, L1) ++
            pfiltermap(Fun, L2, MaxProcesses);
        _ ->
            pfiltermap(Fun, Elements)
    end.


%%--------------------------------------------------------------------
%% @doc
%% A parallel version of lists:filtermap/2 - each element is processed by
%% a new async process. Raises an error if any of the processes crash
%% or a process somehow dies without reporting back.
%% TODO VFS-7568 parallelize also filtering step
%% @end
%%--------------------------------------------------------------------
-spec pfiltermap(
    Fun :: fun((X :: A) -> {true, Y :: B} | false),
    Elements :: [X :: A]
) -> [X :: B].
pfiltermap(Fun, Elements) ->
    lists:filtermap(fun(MappedResult) ->
        MappedResult
    end, pmap(fun(Element) -> Fun(Element) end, Elements)).


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
do_foldl(_F, {halt, Acc}, _) -> Acc;
do_foldl(_F, {cont, Acc}, []) -> Acc;
do_foldl(F, {cont, Acc}, [Hd | Tail]) -> do_foldl(F, F(Hd, Acc), Tail).
