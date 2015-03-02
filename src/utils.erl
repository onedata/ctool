%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module stores utility functions for use in other modules.
%%% @end
%%%-------------------------------------------------------------------
-module(utils).

%% API
-export([binary_join/2, ensure_running/1, pmap/2, pforeach/2, time/0, mtime/0,
    record_type/1, ensure_binary/1, ensure_list/1, ensure_unicode_list/1,
    ensure_unicode_binary/1, access_token_hash/1, trim_spaces/1, ceil/1,
    aggregate_over_first_element/1, average/1, random_shuffle/1,
    random_element/1, get_host/1, get_host_as_atom/1, cmd/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Joins bineries with Separator
%% @end
%%--------------------------------------------------------------------
-spec binary_join(Binaries :: [binary()], Separator :: binary()) -> binary().
binary_join([], _Sep) ->
    <<>>;
binary_join([Part], _Sep) ->
    Part;
binary_join([Head | Tail], Sep) ->
    lists:foldl(
        fun(A, B) ->
            <<B/binary, Sep/binary, A/binary>>
        end, Head, Tail).

%%--------------------------------------------------------------------
%% @doc
%% Ensures that Application is started. See {@link application}
%% @end
%%--------------------------------------------------------------------
-spec ensure_running(Application :: atom()) -> ok | {error, Reason :: term()}.
ensure_running(Application) ->
    case application:start(Application) of
        {error, {already_started, Application}} -> ok;
        EverythingElse -> EverythingElse
    end.

%%--------------------------------------------------------------------
%% @doc
%% A  parallel version of lists:map/2. See {@link lists:map/2}
%% @end
%%--------------------------------------------------------------------
-spec pmap(Fun :: fun((X :: A) -> B), L :: [A]) -> [B].
pmap(Fun, L) ->
    Self = self(),
    Ref = erlang:make_ref(),
    PIDs = lists:map(fun(X) ->
        spawn(fun() -> pmap_f(Self, Ref, Fun, X) end)
    end, L),
    pmap_gather(PIDs, Ref, []).

%%--------------------------------------------------------------------
%% @doc
%% A parallel version of lists:foreach/2. See {@link lists:foreach/2}
%% @end
%%--------------------------------------------------------------------
-spec pforeach(Fun :: fun((X :: A) -> any()), L :: [A]) -> ok.
pforeach(Fun, L) ->
    Self = self(),
    Ref = erlang:make_ref(),
    lists:foreach(fun(X) ->
        spawn(fun() -> pforeach_f(Self, Ref, Fun, X) end)
    end, L),
    pforeach_gather(length(L), Ref).

%%--------------------------------------------------------------------
%% @doc
%% Returns hash of given AccessToken. Can be used to confirm user's GlobalId using GlobalRegistry.
%% @end
%%--------------------------------------------------------------------
-spec access_token_hash(AccessToken :: binary()) -> Hash :: binary().
access_token_hash(AccessToken) ->
    base64:encode(crypto:hash(sha512, AccessToken)).

%%--------------------------------------------------------------------
%% @doc
%% Returns time in seconds.
%% @end
%%--------------------------------------------------------------------
-spec time() -> Result :: integer().
time() ->
    {M, S, _} = now(),
    M * 1000000 + S.

%%--------------------------------------------------------------------
%% @doc
%% Returns time in milliseconds.
%% @end
%%--------------------------------------------------------------------
-spec mtime() -> Result :: integer().
mtime() ->
    {M, S, U} = now(),
    erlang:trunc(M * 1000000000 + S * 1000 + U / 1000).

%%--------------------------------------------------------------------
%% @doc
%% Gets record type for given record. Since the is now way of knowing whether
%% given tuple is record, this method behaviour is unspecified for non-record tuples.
%% @end
%%--------------------------------------------------------------------
-spec record_type(Record :: tuple()) ->
    atom() | no_return().
record_type(Record) when is_tuple(Record) ->
    element(1, Record).

%%--------------------------------------------------------------------
%% @doc
%% Translates given term to binary if needed.
%% @end
%%--------------------------------------------------------------------
-spec ensure_binary(BinLikeTerm :: atom() | binary() | integer() | string() | list()) -> binary().
ensure_binary(Bin) when is_binary(Bin) ->
    Bin;
ensure_binary(List) when is_list(List) ->
    iolist_to_binary(List);
ensure_binary(Integer) when is_integer(Integer) ->
    integer_to_binary(Integer);
ensure_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8).

%%--------------------------------------------------------------------
%% @doc
%% Translates given term to list if needed.
%% @end
%%--------------------------------------------------------------------
-spec ensure_list(ListLikeTerm :: atom() | binary() | integer() | string() | list()) -> list().
ensure_list(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
ensure_list(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
ensure_list(List) when is_list(List) ->
    List;
ensure_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom).

%%--------------------------------------------------------------------
%% @doc
%% Converts a unicode list to utf8 binary.
%% @end
%%--------------------------------------------------------------------
-spec ensure_unicode_binary(String :: string() | binary()) -> binary().
ensure_unicode_binary(Binary) when is_binary(Binary) ->
    Binary;
ensure_unicode_binary(String) ->
    unicode:characters_to_binary(String).

%%--------------------------------------------------------------------
%% @doc
%% Converts a utf8 binary to unicode list.
%% @end
%%--------------------------------------------------------------------
-spec ensure_unicode_list(Binary :: binary() | string()) -> string().
ensure_unicode_list(String) when is_list(String) ->
    String;
ensure_unicode_list(Binary) ->
    unicode:characters_to_list(Binary).

%%--------------------------------------------------------------------
%% @doc
%% Trims spaces from front and end of given binary.
%% @end
%%--------------------------------------------------------------------
-spec trim_spaces(binary()) -> binary().
trim_spaces(Binary) when is_binary(Binary) ->
    list_to_binary(string:strip(binary_to_list(Binary), both, $ )).

%%--------------------------------------------------------------------
%% @doc
%% Math ceil function (works on positive values).
%% @end
%%--------------------------------------------------------------------
-spec ceil(N :: number()) -> integer().
ceil(N) when trunc(N) == N -> N;
ceil(N) -> trunc(N + 1).

%%--------------------------------------------------------------------
%% @doc
%% Aggregates list over first element of tuple.
%% @end
%%--------------------------------------------------------------------
-spec aggregate_over_first_element(List :: [{K, V}]) -> [{K, [V]}].
aggregate_over_first_element(List) ->
    lists:reverse(
        lists:foldl(fun({Key, Value}, []) -> [{Key, [Value]}];
            ({Key, Value}, [{Key, AccValues} | Tail]) ->
                [{Key, [Value | AccValues]} | Tail];
            ({Key, Value}, Acc) -> [{Key, [Value]} | Acc]
        end, [], lists:keysort(1, List))).

%%--------------------------------------------------------------------
%% @doc
%% Calculates average of listed numbers.
%% @end
%%--------------------------------------------------------------------
-spec average(List :: list()) -> float().
average(List) ->
    lists:foldl(fun(N, Acc) -> N + Acc end, 0, List) / length(List).

%%--------------------------------------------------------------------
%% @doc
%% Shuffles list randomly.
%% @end
%%--------------------------------------------------------------------
-spec random_shuffle(List :: list()) -> NewList :: list().
random_shuffle(List) ->
    From = 0,
    To = length(List) + 1,
    [X || {_, X} <- lists:sort([{crypto:rand_uniform(From, To), N} || N <- List])].

%%--------------------------------------------------------------------
%% @doc
%% Get random element of list
%% @end
%%--------------------------------------------------------------------
-spec random_element([term()]) -> term().
random_element(List) ->
    RandomIndex = crypto:rand_uniform(1, length(List)+1),
    lists:nth(RandomIndex, List).

%%--------------------------------------------------------------------
%% @doc
%% get host from node()
%% @end
%%--------------------------------------------------------------------
-spec get_host(node()) -> string().
get_host(Node) ->
    lists:last(string:tokens(atom_to_list(Node), "@")).

%%--------------------------------------------------------------------
%% @doc
%% get host from node(), as atom
%% @end
%%--------------------------------------------------------------------
-spec get_host_as_atom(node()) -> atom().
get_host_as_atom(Node) ->
    list_to_atom(get_host(Node)).

%%--------------------------------------------------------------------
%% @doc
%% Runs a command given by a string list.
%% @end
%%--------------------------------------------------------------------
cmd(Command) ->
    os:cmd(string:join(Command, " ")).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Runs a function on X and returns its result to parent.
%% @end
%%--------------------------------------------------------------------
-spec pmap_f(Parent :: pid(), Ref :: reference(), Fun :: fun((E :: A) -> any()), X :: A) -> {pid(), reference(), term()}.
pmap_f(Parent, Ref, Fun, X) -> Parent ! {self(), Ref, (catch Fun(X))}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Gathers the results of pmap.
%% @end
%%--------------------------------------------------------------------
-spec pmap_gather(PIDs :: [pid()], Ref :: reference(), Acc :: list()) -> list().
pmap_gather([], _Ref, Acc) -> lists:reverse(Acc);
pmap_gather([PID | T], Ref, Acc) ->
    receive
        {PID, Ref, Result} -> pmap_gather(T, Ref, [Result | Acc])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Runs a function on X and signals parent that it's done.
%% @end
%%--------------------------------------------------------------------
-spec pforeach_f(Parent :: pid(), Ref :: reference(), Fun :: fun((E :: A) -> any()), X :: A) -> reference().
pforeach_f(Parent, Ref, Fun, X) -> catch Fun(X), Parent ! Ref.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Joins pforeach processes.
%% @end
%%--------------------------------------------------------------------
-spec pforeach_gather(PIDs :: [pid()], Ref :: reference()) -> ok.
pforeach_gather(0, _Ref) -> ok;
pforeach_gather(N, Ref) ->
    receive
        Ref -> pforeach_gather(N - 1, Ref)
    end.
