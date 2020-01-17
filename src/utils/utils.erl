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
-export([ensure_running/1, pmap/2, pforeach/2]).
-export([record_type/1, trim_spaces/1, ceil/1, average/1]).
-export([get_host/1, get_host_as_atom/1, cmd/1]).
-export([process_info/1, process_info/2]).
-export([ensure_defined/3, undefined_to_null/1, null_to_undefined/1]).
-export([timeout/2, timeout/4]).
-export([duration/1, adjust_duration/2]).
-export([mkdtemp/0, mkdtemp/3, rmtempdir/1, run_with_tempdir/1]).
-export([to_binary/1]).
-export([save_file_on_hosts/3, save_file/2]).
-export([ensure_list/1]).

-type time_unit() :: us | ms | s | min | h.

-define(TIMEOUT, 60000).

%%%===================================================================
%%% API
%%%===================================================================

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
pmap(Fun, [X]) ->
    [catch Fun(X)];
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
-spec pforeach(Fun :: fun((X :: A) -> any()), L :: [A]) -> ok | error.
pforeach(Fun, [X]) ->
    catch Fun(X),
    ok;
pforeach(Fun, L) ->
    Self = self(),
    Ref = erlang:make_ref(),
    Pids = lists:map(fun(X) ->
        spawn(fun() -> pforeach_f(Self, Ref, Fun, X) end)
    end, L),
    pforeach_gather(Pids, Ref).


%%--------------------------------------------------------------------
%% @doc
%% Executes Fun waiting Timeout milliseconds for results.
%% Note that execution of Fun might continue even after this function
%% returns with {error, timeout}
%% @end
%%--------------------------------------------------------------------
-spec timeout(Fun :: fun(() -> Result), TimeoutMillis :: non_neg_integer()) ->
    {done, Result} | {error, timeout}.
timeout(Fun, Timeout) when is_function(Fun, 0) ->
    case rpc:call(node(), erlang, apply, [Fun, []], Timeout) of
        {badrpc, timeout} -> {error, timeout};
        {badrpc, _} = Error -> error(Error);
        Result -> {done, Result}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Executes Module:Function(Args) waiting Timeout milliseconds for results.
%% Note that execution of the called might continue even after this
%% function returns with {error, timeout}
%% @end
%%--------------------------------------------------------------------
-spec timeout(module(), Function :: atom(), Args :: list(),
    TimeoutMillis :: non_neg_integer()) ->
    {done, Result :: term()} | {error, timeout}.
timeout(Module, Function, Args, Timeout) ->
    case rpc:call(node(), Module, Function, Args, Timeout) of
        {badrpc, timeout} -> {error, timeout};
        {badrpc, _} = Error -> error(Error);
        Result -> {done, Result}
    end.


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
ceil(N) when trunc(N) == N -> trunc(N);
ceil(N) -> trunc(N + 1).

%%--------------------------------------------------------------------
%% @doc
%% Calculates average of listed numbers.
%% @end
%%--------------------------------------------------------------------
-spec average(List :: list(number())) -> float().
average(List) ->
    lists:sum(List) / length(List).

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
-spec cmd(Command :: [string()]) -> string().
cmd(Command) ->
    os:cmd(string:join(Command, " ")).

%%--------------------------------------------------------------------
%% @doc
%% Measures execution time of a function. Returns function result and its
%% duration both in microseconds and adjusted using adjust_duration function.
%% @end
%%--------------------------------------------------------------------
-spec duration(Function :: fun(() -> Result)) -> {Result, UsDuration :: integer(),
    AdjustedDuration :: integer() | float(), TimeUnit :: string()} when
    Result :: term().
duration(Function) ->
    T1 = erlang:monotonic_time(micro_seconds),
    Result = Function(),
    T2 = erlang:monotonic_time(micro_seconds),
    UsDuration = T2 - T1,
    {AdjustedDuration, TimeUnit} = adjust_duration(UsDuration, us),
    {Result, UsDuration, AdjustedDuration, TimeUnit}.

%%--------------------------------------------------------------------
%% @doc
%% Adjusts duration, so that it is in the most condensed and simplified unit.
%% @end
%%--------------------------------------------------------------------
-spec adjust_duration(Duration :: integer() | float(), TimeUnit :: time_unit()) ->
    {AdjustedDuration :: integer() | float(), AdjustedTimeUnit :: string()}.
adjust_duration(Duration, Unit) ->
    {NextUnit, Factor} = next_time_unit(Unit),
    case (Duration > Factor) and (NextUnit /= undefined) of
        true -> adjust_duration(Duration / Factor, NextUnit);
        _ -> {Duration, atom_to_list(Unit)}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Creates a temporary dir (with any name) and returns its path.
%% @end
%%--------------------------------------------------------------------
-spec mkdtemp() -> DirPath :: list().
mkdtemp() ->
    mochitemp:mkdtemp().

%%--------------------------------------------------------------------
%% @doc
%% Creates a temporary dir (with given location and name) and returns its path.
%% @end
%%--------------------------------------------------------------------
-spec mkdtemp(Suffix :: string(), Prefix :: string(), Dir :: string()) -> Path :: string().
mkdtemp(Suffix, Prefix, Dir) ->
    mochitemp:mkdtemp(Suffix, Prefix, Dir).

%%--------------------------------------------------------------------
%% @doc
%% Removes a temporary dir.
%% @end
%%--------------------------------------------------------------------
-spec rmtempdir(Dir :: string()) -> ok.
rmtempdir(Dir) ->
    mochitemp:rmtempdir(Dir).


%%--------------------------------------------------------------------
%% @doc
%% Provides given Fun with a temporary directory and ensures
%% cleanup after execution.
%% @end
%%--------------------------------------------------------------------
-spec run_with_tempdir(fun((file:filename()) -> Result)) -> Result.
run_with_tempdir(Fun) ->
    TempDir = mkdtemp(),
    try
        Fun(TempDir)
    after
        catch rmtempdir(TempDir)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Ensures value is defined.
%% @end
%%--------------------------------------------------------------------
-spec ensure_defined(Value, UndefinedValue :: term(), DefaultValue) ->
    Value | DefaultValue.
ensure_defined(UndefinedValue, UndefinedValue, DefaultValue) ->
    DefaultValue;
ensure_defined(Value, _, _) ->
    Value.


%%--------------------------------------------------------------------
%% @doc
%% If given term is undefined returns null, otherwise returns unchanged term.
%% @end
%%--------------------------------------------------------------------
-spec undefined_to_null(undefined | T) -> null | T.
undefined_to_null(undefined) ->
    null;
undefined_to_null(Other) ->
    Other.


%%--------------------------------------------------------------------
%% @doc
%% If given term is undefined returns null, otherwise returns unchanged term.
%% @end
%%--------------------------------------------------------------------
-spec null_to_undefined(null | T) -> undefined | T.
null_to_undefined(null) ->
    undefined;
null_to_undefined(Other) ->
    Other.


%%--------------------------------------------------------------------
%% @doc
%% Provides process info. Works on local and remote node.
%% @end
%%--------------------------------------------------------------------
-spec process_info(Pid :: pid()) -> {atom(), term()} | undefined.
process_info(Pid) ->
    MyNode = node(),
    case node(Pid) of
        MyNode ->
            erlang:process_info(Pid);
        OtherNode ->
            rpc:call(OtherNode, erlang, process_info, [Pid])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Provides process info. Works on local and remote node.
%% @end
%%--------------------------------------------------------------------
-spec process_info(Pid :: pid(), Args :: atom() | [atom]) ->
    {atom(), term()}  | [{atom(), term()}] | [] | undefined.
process_info(Pid, Args) ->
    MyNode = node(),
    case node(Pid) of
        MyNode ->
            erlang:process_info(Pid, Args);
        OtherNode ->
            rpc:call(OtherNode, erlang, process_info, [Pid, Args])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Encodes given given term as binary.
%% @end
%%--------------------------------------------------------------------
-spec to_binary(term()) -> binary().
to_binary(Term) when is_binary(Term) ->
    Term;
to_binary(Term) ->
    base64:encode(term_to_binary(Term)).


%%--------------------------------------------------------------------
%% @doc
%% Saves given file on given hosts.
%% @end
%%--------------------------------------------------------------------
-spec save_file_on_hosts(Hosts :: [node()], Path :: file:name_all(), Content :: binary()) ->
    ok | [{node(), Reason :: term()}].
save_file_on_hosts(Hosts, Path, Content) ->
    Res = lists:foldl(
        fun(Host, Acc) ->
            case rpc:call(Host, ?MODULE, save_file, [Path, Content]) of
                ok ->
                    Acc;
                {error, Reason} ->
                    [{Host, Reason} | Acc]
            end
        end, [], Hosts),
    case Res of
        [] -> ok;
        Other -> Other
    end.


%%--------------------------------------------------------------------
%% @doc
%% Saves given file under given path.
%% @end
%%--------------------------------------------------------------------
-spec save_file(Path :: file:name_all(), Content :: binary()) -> ok | {error, term()}.
save_file(Path, Content) ->
    try
        file:make_dir(filename:dirname(Path)),
        ok = file:write_file(Path, Content),
        ok
    catch
        _:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Ensures that returned value is a list.
%% @end
%%--------------------------------------------------------------------
-spec ensure_list(T | [T]) -> [T].
ensure_list(List) when is_list(List) -> List;
ensure_list(Element) -> [Element].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% For given time unit returns the next one and division factor describing how
%% many times the next unit is bigger that the current unit.
%% @end
%%--------------------------------------------------------------------
-spec next_time_unit(TimeUnit :: time_unit()) ->
    {NextTimeUnit :: time_unit(), Factor :: integer()}.
next_time_unit(us) -> {ms, 1000};
next_time_unit(ms) -> {s, 1000};
next_time_unit(s) -> {min, 60};
next_time_unit(min) -> {h, 60};
next_time_unit(h) -> {undefined, undefiend}.

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
-spec pmap_gather(PIDs :: [pid()], Ref :: reference(), Acc :: list()) ->
    list().
pmap_gather([], _Ref, Acc) -> lists:reverse(Acc);
pmap_gather([PID | T] = Pids, Ref, Acc) ->
    receive
        {PID, Ref, Result} -> pmap_gather(T, Ref, [Result | Acc])
    after
        ?TIMEOUT ->
            IsAnyAlive = lists:foldl(fun
                (_, true) ->
                    true;
                (Pid, _Acc) ->
                    erlang:is_process_alive(Pid)
            end, false, Pids),
            case IsAnyAlive of
                true ->
                    pmap_gather(Pids, Ref, Acc);
                false ->
                    Errors = lists:map(fun(_) -> {error, slave_error} end, Pids),
                    lists:reverse(Acc) ++ Errors
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Runs a function on X and signals parent that it's done.
%% @end
%%--------------------------------------------------------------------
-spec pforeach_f(Parent :: pid(), Ref :: reference(),
    Fun :: fun((E :: A) -> any()), X :: A) -> {reference(), pid()}.
pforeach_f(Parent, Ref, Fun, X) -> catch Fun(X), Parent ! {Ref, self()}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Joins pforeach processes.
%% @end
%%--------------------------------------------------------------------
-spec pforeach_gather([pid()], Ref :: reference()) -> ok | error.
pforeach_gather([], _Ref) -> ok;
pforeach_gather(Pids, Ref) ->
    receive
        {Ref, Pid} -> pforeach_gather(Pids -- [Pid], Ref)
    after
        ?TIMEOUT ->
            IsAnyAlive = lists:foldl(fun
                (_, true) ->
                    true;
                (Pid, _Acc) ->
                    erlang:is_process_alive(Pid)
            end, false, Pids),
            case IsAnyAlive of
                true ->
                    pforeach_gather(Pids, Ref);
                false ->
                    error
            end
    end.

