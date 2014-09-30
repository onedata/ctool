%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module stores utility functions for use in other modules.
%% @end
%% ===================================================================
-module(opn_utils).

%% API
-export([ensure_running/1, pmap/2, pforeach/2, time/0, mtime/0, record_type/1, ensure_binary/1, ensure_list/1, access_token_hash/1]).

%% ====================================================================
%% API functions
%% ====================================================================

%% ensure_running/1
%% ====================================================================
%% @doc Ensures that Application is started. See {@link application}
%% @end
-spec ensure_running(Application :: atom()) -> ok | {error, Reason :: term()}.
%% ====================================================================
ensure_running(Application) ->
    case application:start(Application) of
        {error, {already_started, Application}} -> ok;
        EverythingElse -> EverythingElse
    end.


%% pmap/2
%% ====================================================================
%% @doc A parallel version of lists:map/2. See {@link lists:map/2}
%% @end
-spec pmap(Fun :: fun((X :: A) -> B), L :: [A]) -> [B].
%% ====================================================================
pmap(Fun, L) ->
    Self = self(),
    Ref = erlang:make_ref(),
    PIDs = lists:map(fun(X) -> spawn(fun() -> pmap_f(Self, Ref, Fun, X) end) end, L),
    pmap_gather(PIDs, Ref, []).


%% pforeach/2
%% ====================================================================
%% @doc A parallel version of lists:foreach/2. See {@link lists:foreach/2}
%% @end
-spec pforeach(Fun :: fun((X :: A) -> any()), L :: [A]) -> ok.
%% ====================================================================
pforeach(Fun, L) ->
    Self = self(),
    Ref = erlang:make_ref(),
    lists:foreach(fun(X) -> spawn(fun() -> pforeach_f(Self, Ref, Fun, X) end) end, L),
    pforeach_gather(length(L), Ref).


%% access_token_hash/1
%% ====================================================================
%% @doc Returns hash of given AccessToken. Can be used to confirm user's GlobalId using GlobalRegistry.
%% @end
-spec access_token_hash(AccessToken :: binary()) -> Hash :: binary().
%% ====================================================================
access_token_hash(AccessToken) ->
    base64:encode(crypto:hash(sha512, AccessToken)).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% pmap_f/4
%% ====================================================================
%% @doc Runs a function on X and returns its result to parent.
%% @end
-spec pmap_f(Parent :: pid(), Ref :: reference(), Fun :: fun((E :: A) -> any()), X :: A) -> {pid(), reference(), term()}.
%% ====================================================================
pmap_f(Parent, Ref, Fun, X) -> Parent ! {self(), Ref, (catch Fun(X))}.


%% pmap_gather/3
%% ====================================================================
%% @doc Gathers the results of pmap.
%% @end
-spec pmap_gather(PIDs :: [pid()], Ref :: reference(), Acc :: list()) -> list().
%% ====================================================================
pmap_gather([], _Ref, Acc) -> lists:reverse(Acc);
pmap_gather([PID | T], Ref, Acc) ->
    receive
        {PID, Ref, Result} -> pmap_gather(T, Ref, [Result | Acc])
    end.


%% pforeach_f/4
%% ====================================================================
%% @doc Runs a function on X and signals parent that it's done.
%% @end
-spec pforeach_f(Parent :: pid(), Ref :: reference(), Fun :: fun((E :: A) -> any()), X :: A) -> reference().
%% ====================================================================
pforeach_f(Parent, Ref, Fun, X) -> catch Fun(X), Parent ! Ref.


%% pforeach_gather/2
%% ====================================================================
%% @doc Joins pforeach processes.
%% @end
-spec pforeach_gather(PIDs :: [pid()], Ref :: reference()) -> ok.
%% ====================================================================
pforeach_gather(0, _Ref) -> ok;
pforeach_gather(N, Ref) ->
    receive
        Ref -> pforeach_gather(N - 1, Ref)
    end.

%% time/0
%% ====================================================================
%% @doc Returns time in seconds.
%% @end
-spec time() -> Result :: integer().
time() ->
    {M, S, _} = now(),
    M * 1000000 + S.

%% mtime/0
%% ====================================================================
%% @doc Returns time in milliseconds.
%% @end
-spec mtime() -> Result :: integer().
mtime() ->
    {M, S, U} = now(),
    erlang:trunc(M * 1000000 + S * 1000 + U / 1000).


%% record_type/1
%% ====================================================================
%% @doc Gets record type for given record. Since the is now way of knowing whether
%%      given tuple is record, this method behaviour is unspecified for non-record tuples.
%% @end
-spec record_type(Record :: tuple()) ->
    atom() | no_return().
%% ====================================================================
record_type(Record) when is_tuple(Record) ->
    element(1, Record).


%% ensure_binary/1
%% ====================================================================
%% @doc Translates given term to binary if needed.
%% @end
-spec ensure_binary(BinLikeTerm :: atom() | binary() | integer() | string() | list()) -> binary().
%% ====================================================================
ensure_binary(Bin) when is_binary(Bin) ->
    Bin;
ensure_binary(List) when is_list(List) ->
    iolist_to_binary(List);
ensure_binary(Integer) when is_integer(Integer) ->
    integer_to_binary(Integer);
ensure_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8).


%% ensure_list/1
%% ====================================================================
%% @doc Translates given term to list if needed.
%% @end
-spec ensure_list(ListLikeTerm :: atom() | binary() | integer() | string() | list()) -> list().
%% ====================================================================
ensure_list(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
ensure_list(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
ensure_list(List) when is_list(List) ->
    List;
ensure_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom).
