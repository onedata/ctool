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
-export([ensure_running/1, pmap/2, pforeach/2,
    record_type/1, access_token_hash/1, trim_spaces/1, ceil/1,
    aggregate_over_first_element/1, average/1, random_shuffle/1, get_values/2,
    random_element/1, get_host/1, get_host_as_atom/1, cmd/1, ensure_defined/3,
    process_info/1, process_info/2]).
-export([duration/1, adjust_duration/2]).
-export([mkdtemp/0, mkdtemp/3, rmtempdir/1]).
-export([to_binary/1]).
-export([save_file_on_hosts/3, save_file/2]).

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
-spec pmap(Fun :: fun((X :: A) -> B), L :: [A]) -> [B] | error.
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
pforeach(Fun, L) ->
    Self = self(),
    Ref = erlang:make_ref(),
    Pids = lists:map(fun(X) ->
        spawn(fun() -> pforeach_f(Self, Ref, Fun, X) end)
    end, L),
    pforeach_gather(Pids, Ref).

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
%% Returns a list of values associated with the keys. For a key that
%% is missing in the property list an 'undefined' value is returned.
%% @end
%%--------------------------------------------------------------------
-spec get_values(Keys :: list(), List :: proplists:proplist()) -> Values :: list().
get_values(Keys, List) ->
    lists:map(fun(Key) ->
        lists_utils:key_get(Key, List)
    end, Keys).


%%--------------------------------------------------------------------
%% @doc
%% Get random element of list
%% @end
%%--------------------------------------------------------------------
-spec random_element([term()]) -> term().
random_element(List) ->
    RandomIndex = crypto:rand_uniform(1, length(List) + 1),
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


%%--------------------------------------------------------------------
%% @doc
%% Measures execution time of a function. Returns function result and its
%% duration both in microseconds and addjusted using adjust_duration function.
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
-spec mkdtemp() ->    DirPath :: list().
mkdtemp() ->
    mochitemp:mkdtemp().

%%--------------------------------------------------------------------
%% @doc
%% Creates a temporary dir (with given location and name) and returns its path.
%% @end
%%--------------------------------------------------------------------
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
%% Ensures value is defined.
%% @end
%%--------------------------------------------------------------------
-spec ensure_defined(Value :: term(), UndefinedValue :: term(), DefaultValue :: term()) ->term().
ensure_defined(UndefinedValue, UndefinedValue, DefaultValue) ->
    DefaultValue;
ensure_defined(Value, _, _) ->
    Value.

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
-spec save_file_on_hosts(Hosts :: [atom()], Path :: file:name_all(), Content :: binary()) ->
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
    list() | error.
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
                    error
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Runs a function on X and signals parent that it's done.
%% @end
%%--------------------------------------------------------------------
-spec pforeach_f(Parent :: pid(), Ref :: reference(),
    Fun :: fun((E :: A) -> any()), X :: A) -> {reference(),pid()}.
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

