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

-include("logging.hrl").

%% API
-export([ensure_running/1]).
-export([record_type/1, trim_spaces/1, ceil/1, average/1]).
-export([get_host/1, get_host_as_atom/1, cmd/1]).
-export([process_info/1, process_info/2]).
-export([ensure_defined/2, ensure_defined/3, undefined_to_null/1, null_to_undefined/1]).
-export([throttle/2, throttle/3]).
-export([timeout/2, timeout/4]).
-export([duration/1, adjust_duration/2]).
-export([mkdtemp/0, mkdtemp/3, rmtempdir/1, run_with_tempdir/1]).
-export([to_binary/1]).
-export([save_file_on_hosts/3, save_file/2]).
-export([ensure_list/1]).
-export([to_atom/1, to_boolean/1]).
-export([encode_pid/1, decode_pid/1]).
-export([rpc_multicall/4, rpc_multicall/5]).

-type time_unit() :: us | ms | s | min | h.

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
%% @equiv throttle(erlang:fun_to_list(Fun), Interval, Fun).
%% The function itself is used as throttler identifier.
%% @end
%%--------------------------------------------------------------------
-spec throttle(Interval :: time:seconds(), fun(() -> Result)) -> Result.
throttle(Interval, Fun) ->
    throttle(erlang:fun_to_list(Fun), Interval, Fun).


%%--------------------------------------------------------------------
%% @doc
%% Executes given Fun, but not more often than the Interval (excessive calls
%% are completely discarded). Works deterministically within a single process,
%% otherwise it is best effort - duplicate Fun execution may occur when
%% called by parallel processes within a very short time window.
%%
%% Identifier is an arbitrary term that identifies the throttler instance
%% (calls with different identifiers are independently throttled, even if the
%% same function is given).
%%
%% Always returns the result of last execution that was applied.
%% @end
%%--------------------------------------------------------------------
-spec throttle(Identifier :: term(), Interval :: time:seconds(), fun(() -> Result)) -> Result.
throttle(Identifier, Interval, Fun) when is_function(Fun, 0) ->
    {ok, Res} = node_cache:acquire({throttle, Identifier}, fun() ->
        {ok, Fun(), Interval}
    end),
    Res.


%%--------------------------------------------------------------------
%% @doc
%% Executes Fun waiting Timeout milliseconds for results.
%% Note that execution of Fun might continue even after this function
%% returns with {error, timeout}
%% @end
%%--------------------------------------------------------------------
-spec timeout(Fun :: fun(() -> Result), time:millis()) ->
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
    Stopwatch = stopwatch:start(),
    Result = Function(),
    UsDuration = stopwatch:read_micros(Stopwatch),
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
%% @equiv ensure_defined(Value, undefined, DefaultValue).
%% @end
%%--------------------------------------------------------------------
-spec ensure_defined(Value, DefaultValue) -> Value | DefaultValue.
ensure_defined(Value, DefaultValue) ->
    ensure_defined(Value, undefined, DefaultValue).


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


-spec to_atom(term()) -> atom() | no_return().
to_atom(Atom) when is_atom(Atom) -> Atom;
to_atom(Binary) when is_binary(Binary) -> binary_to_atom(Binary, utf8).


-spec to_boolean(binary() | boolean()) -> boolean() | no_return().
to_boolean(Boolean) when is_boolean(Boolean) -> Boolean;
to_boolean(<<"true">>) -> true;
to_boolean(<<"false">>) -> false.


-spec encode_pid(pid()) -> binary().
encode_pid(Pid) ->
    % todo remove after VFS-3657
    list_to_binary(pid_to_list(Pid)).


-spec decode_pid(binary()) -> pid().
decode_pid(Pid) ->
    % todo remove after VFS-3657
    list_to_pid(binary_to_list(Pid)).


%%--------------------------------------------------------------------
%% @doc
%% Sometimes, rpc:multicall may crash if the list of nodes is empty
%% (as of OTP 24.0.2), using this wrapper mitigates the risk of a crash.
%% @end
%%--------------------------------------------------------------------
-spec rpc_multicall(Nodes, Module, Function, Args) -> {ResL, BadNodes} when
    Nodes :: [node()],
    Module :: module(),
    Function :: atom(),
    Args :: [term()],
    ResL :: [term()],
    BadNodes :: [node()];
    (Module, Function, Args, Timeout) -> {ResL, BadNodes} when
    Module :: module(),
    Function :: atom(),
    Args :: [term()],
    Timeout :: timeout(),
    ResL :: [term()],
    BadNodes :: [node()].
rpc_multicall(Nodes, Module, Function, Args) ->
    rpc_multicall(Nodes, Module, Function, Args, infinity).

-spec rpc_multicall(Nodes, Module, Function, Args, timeout()) -> {ResL, BadNodes} when
    Nodes :: [node()],
    Module :: module(),
    Function :: atom(),
    Args :: [term()],
    ResL :: [term()],
    BadNodes :: [node()].
rpc_multicall([], _, _, _, _) ->
    {[], []};
rpc_multicall([ThisNode], Module, Function, Args, infinity) when ThisNode =:= node() ->
    {[catch erlang:apply(Module, Function, Args)], []};
rpc_multicall(Nodes, Module, Function, Args, Timeout) ->
    rpc:multicall(Nodes, Module, Function, Args, Timeout).

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
