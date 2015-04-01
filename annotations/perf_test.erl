%%%--------------------------------------------------------------------
%%% @author Michal Wrzeszcz
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This file contains definitions of annotations used during
%%% performance tests.
%%% @end
%%%--------------------------------------------------------------------
-module(perf_test).
-author("Michal Wrzeszcz").

-annotation('function').
-include_lib("ctool/include/test/assertions.hrl"). % this file is built by parent project so include_lib must be used
-include_lib("annotations/include/types.hrl").

-export([around_advice/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Function executed instead of annotated function. May start
%% annotated function inside.
%% @end
%%--------------------------------------------------------------------
-spec around_advice(#annotation{}, M :: atom(), F :: atom(), Inputs :: list()) ->
    Result :: term().
around_advice(#annotation{data = {perf_cases, Cases}}, M, F, Inputs) ->
    case os:getenv("perf_test") of
        "true" ->
            Cases;
        _ ->
            annotation:call_advised(M, F, Inputs)
    end;

around_advice(#annotation{data = {_, _} = SingleExt}, M, F, Inputs) ->
    around_advice(#annotation{data = [SingleExt]}, M, F, Inputs);

around_advice(#annotation{data = ConfExt}, M, F, Inputs) when is_list(ConfExt) ->
    case os:getenv("perf_test") of
        "true" ->
            process_flag(trap_exit, true),
            Repeats = proplists:get_value(repeats, ConfExt, 1),
            case proplists:get_value(perf_configs, ConfExt, []) of
                [] ->
                    Ext = proplists:get_value(perf_config, ConfExt, []),
                    Ans = exec_perf_config(M, F, Inputs, Ext, Repeats),
                    ?assertEqual(ok, Ans);
                Exts ->
                    AnsSum = lists:foldl(
                        fun(Ext, Acc) ->
                            Ans = exec_perf_config(M, F, Inputs, Ext, Repeats),
                            case Acc of
                                ok -> Ans;
                                _ -> Acc
                            end
                        end, ok, Exts),
                    ?assertEqual(ok, AnsSum)
            end;
        _ ->
            Ext = proplists:get_value(ct_config, ConfExt, []),
            [I1] = Inputs,  % get first arg (test config)
            annotation:call_advised(M, F, [I1 ++ Ext])
    end;

around_advice(#annotation{}, M, F, Inputs) ->
    around_advice(#annotation{data = []}, M, F, Inputs).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Executes multiple test configurations.
%% @end
%%--------------------------------------------------------------------
-spec exec_perf_config(M :: atom(), F :: atom(), Inputs :: list(),
    Ext :: list() | tuple(), Repeats :: integer()) -> ok.
exec_perf_config(M, F, Inputs, {Name, ExtList}, Repeats) ->
    exec_perf_config(M, F, Inputs, ExtList, Name, Repeats);
exec_perf_config(M, F, Inputs, Ext, Repeats) ->
    exec_perf_config(M, F, Inputs, Ext, [], Repeats).

%%--------------------------------------------------------------------
%% @doc
%% Executes multiple test configurations.
%% @end
%%--------------------------------------------------------------------
-spec exec_perf_config(M :: atom(), F :: atom(), Inputs :: list(), Ext :: list(),
    ConfigName :: term(), Repeats :: integer()) -> ok | error.
exec_perf_config(M, F, Inputs, Ext, ConfigName, Repeats) ->
    [I1] = Inputs,  % get first arg (test config)
    {ValuesSums, ValuesLists, OkNum, Errors} = exec_multiple_tests(M, F, [I1 ++ Ext], Repeats),
    [{test_suites, Json}] = case file:read_file("perf_results.json") of
                                {ok, FileBinary} ->
                                    json_parser:parse_json_binary_to_atom_proplist(FileBinary);
                                _ ->
                                    [{test_suites, []}]
                            end,
    {ok, File} = file:open("perf_results.json", [write]),

    MJson = proplists:get_value(M, Json, []),
    Json2 = proplists:delete(M, Json),

    FJson = proplists:get_value(F, MJson, []),
    MJson2 = proplists:delete(F, MJson),

    ConfKey = case ConfigName of
                  N when is_atom(N) -> N;
                  _ ->
                      list_to_atom("config" ++ integer_to_list(length(FJson) + 1))
              end,

    Json3 = [
        {test_suites, [
            {M, [
                {F, [
                    {ConfKey,
                        [
                            {timestamp, get_timestamp()},
                            {config_extension, Ext},
                            {repeats, Repeats},
                            {ok_counter, OkNum},
                            {results_sums, [
                                {K, [
                                    {unit, U},
                                    {sum, S}
                                ]} || {K, S, U} <- ValuesSums]},
                            {results_lists, [
                                {K, [
                                    {unit, U},
                                    {list, L}
                                ]} || {K, L, U} <- ValuesLists]},
                            {errors, Errors}
                        ]
                    }
                    | FJson]}
                | MJson2]}
            | Json2]
        }
    ],

    file:write(File, [iolist_to_binary(mochijson2:encode(prepare_to_write(Json3)))]),
    file:close(File),
    case Errors of
        [] -> ok;
        _ -> error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Executes test configuration many times.
%% @end
%%--------------------------------------------------------------------
-spec exec_multiple_tests(M :: atom(), F :: atom(), Inputs :: list(),
    Count :: integer()) -> {ValuesSums, ValuesLists, OkNum, Errors} when
    ValuesSums :: list(),
    ValuesLists :: list(),
    OkNum :: integer(),
    Errors :: list().
exec_multiple_tests(M, F, Inputs, Repeats) ->
    exec_multiple_tests(M, F, Inputs, 1, Repeats + 1, [], [], 0, []).

exec_multiple_tests(_M, _F, _Inputs, Repeats, Repeats, ValuesSums, ValuesLists, OkNum, Errors) ->
    ReversedValuesLists = lists:map(fun({K, Val, U}) ->
        {K, lists:reverse(Val), U}
    end, ValuesLists),
    {ValuesSums, ReversedValuesLists, OkNum, lists:reverse(Errors)};

exec_multiple_tests(M, F, Inputs, Repeat, Repeats, ValuesSums, ValuesLists, OkNum, Errors) ->
    case exec_test(M, F, Inputs) of
        {error, E} ->
            exec_multiple_tests(M, F, Inputs, Repeat + 1, Repeats, ValuesSums, ValuesLists, OkNum, [{Repeat, E} | Errors]);
        V ->
            case ValuesSums of
                [] ->
                    InitValuesLists = lists:map(fun({K, Val, U}) ->
                        {K, [{Repeat, Val}], U}
                    end, V),
                    exec_multiple_tests(M, F, Inputs, Repeat + 1, Repeats, V, InitValuesLists, OkNum + 1, Errors);
                _ ->
                    NewVSums = lists:zipwith(fun({K, V1, U}, {K, V2, U}) ->
                        {K, V1 + V2, U}
                    end, V, ValuesSums),
                    NewVLists = lists:zipwith(fun({K, V1, U}, {K, V2, U}) ->
                        {K, [{Repeat, V1} | V2], U}
                    end, V, ValuesLists),
                    exec_multiple_tests(M, F, Inputs, Repeat + 1, Repeats, NewVSums, NewVLists, OkNum + 1, Errors)
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Executes test configuration and returns lists of pairs {key, value} to be logged.
%% @end
%%--------------------------------------------------------------------
-spec exec_test(M :: atom(), F :: atom(), Inputs :: list()) ->
    list() | {error, term()}.
exec_test(M, F, Inputs) ->
    try
        BeforeProcessing = os:timestamp(),
        Ans = annotation:call_advised(M, F, Inputs),
        AfterProcessing = os:timestamp(),
        case check_links() of
            ok ->
                TestTime = utils:milliseconds_diff(AfterProcessing, BeforeProcessing),
                case Ans of
                    {K, V, U} when is_number(V) ->
                        [{test_time, TestTime, ms}, {K, V, U}];
                    AnsList when is_list(AnsList) ->
                        lists:foldl(fun(AnsPart, Acc) ->
                            case AnsPart of
                                {K2, V2, U2} when is_number(V2) ->
                                    [{K2, V2, U2} | Acc];
                                _ ->
                                    Acc
                            end
                        end, [{test_time, TestTime, ms}], AnsList);
                    _ ->
                        [{test_time, TestTime, ms}]
                end;
            E ->
                E
        end
    catch
        E1:E2 ->
            {error, gui_str:format("~p:~p~n~p", [E1, E2, erlang:get_stacktrace()])}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Checks if linked processes have not failed.
%% @end
%%--------------------------------------------------------------------
-spec check_links() -> ok | {error, term()}.
check_links() ->
    receive
        {'EXIT', _, normal} ->
            check_links();
        {'EXIT', _, _} ->
            {error, linked_proc_error}
    after 0 ->
        ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Prepares input to results file.
%% @end
%%--------------------------------------------------------------------
-spec prepare_to_write(Input :: term()) -> term().
prepare_to_write({struct, List}) ->
    prepare_to_write(List);

prepare_to_write([{_, _} | _] = Input) ->
    {struct, lists:map(fun(I) -> prepare_to_write(I) end, Input)};

prepare_to_write({K, V}) when is_list(V) ->
    {K, prepare_to_write(V)};

prepare_to_write({K, V}) ->
    {K, V};

prepare_to_write(Any) ->
    Any.

%%--------------------------------------------------------------------
%% @doc
%% Get current time in milliseconds.
%% @end
%%--------------------------------------------------------------------
-spec get_timestamp() -> integer().
get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).