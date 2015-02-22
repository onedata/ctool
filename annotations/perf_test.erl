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
-spec around_advice(#annotation{}, M :: atom(), F :: atom(), Inputs :: list()) -> Result :: term().
around_advice(#annotation{data = {perf_cases, Cases}}, M, F, Inputs) ->
  case os:getenv("perf_test") of
    "true" ->
      Cases;
    _ ->
      annotation:call_advised(M, F, Inputs)
  end;

around_advice(#annotation{data = {_, _} = SingleExt}, M, F, Inputs) ->
  around_advice(#annotation{data=[SingleExt]}, M, F, Inputs);

around_advice(#annotation{data = ConfExt}, M, F, Inputs) when is_list(ConfExt)->
  case os:getenv("perf_test") of
    "true" ->
      process_flag(trap_exit, true),
      Repeats = proplists:get_value(repeats, ConfExt, 1),
      case proplists:get_value(perf_configs, ConfExt, []) of
        [] ->
          Ext = proplists:get_value(perf_config, ConfExt, []),
          exec_perf_config(M, F, Inputs, Ext, Repeats);
        Exts ->
          lists:foreach(
            fun(Ext) -> exec_perf_config(M, F, Inputs, Ext, Repeats) end,
          Exts)
      end;
    _ ->
      Ext = proplists:get_value(ct_config, ConfExt, []),
      [I1] = Inputs,  % get first arg (test config)
      annotation:call_advised(M, F, [I1 ++ Ext])
  end;

around_advice(#annotation{}, M, F, Inputs) ->
  around_advice(#annotation{data=[]}, M, F, Inputs).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Executes multiple test configurations.
%% @end
%%--------------------------------------------------------------------
-spec exec_perf_config(M :: atom(), F :: atom(), Inputs :: list(), Ext :: list(), Repeats :: integer()) -> ok.
exec_perf_config(M, F, Inputs, Ext, Repeats) ->
  [I1] = Inputs,  % get first arg (test config)
  Ans = exec_multiple_tests(M, F, [I1 ++ Ext], Repeats),
  {ok, File} = file:open("perf_results", [append]),
  io:fwrite(File, "Module: ~p, Fun: ~p, Values: ~p, ok_counter ~p, errors: ~p, repeats ~p, conf_ext: ~p~n", [M, F] ++ Ans ++ [Repeats, Ext]),
  file:close(File),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Executes test configuration many times. Returns [Values, OkNum, Errors].
%% @end
%%--------------------------------------------------------------------
-spec exec_multiple_tests(M :: atom(), F :: atom(), Inputs :: list(), Count :: integer()) -> list().
exec_multiple_tests(M, F, Inputs, Count) ->
  exec_multiple_tests(M, F, Inputs, Count, [], 0, []).

exec_multiple_tests(_M, _F, _Inputs, 0, Values, OkNum, Errors) ->
  [Values, OkNum, Errors];

exec_multiple_tests(M, F, Inputs, Count, Values, OkNum, Errors) ->
  case exec_test(M, F, Inputs) of
    {error, E} ->
      exec_multiple_tests(M, F, Inputs, Count - 1, Values, OkNum, [E | Errors]);
    V ->
      case Values of
        [] ->
          exec_multiple_tests(M, F, Inputs, Count - 1, V, OkNum + 1, Errors);
        _ ->
          NewV = lists:zipwith(fun({K, V1}, {K, V2}) ->
            {K, V1 + V2}
          end, V, Values),
          exec_multiple_tests(M, F, Inputs, Count - 1, NewV, OkNum + 1, Errors)
      end
  end.

%%--------------------------------------------------------------------
%% @doc
%% Executes test configuration and returns lists of pairs {key, value} to be logged.
%% @end
%%--------------------------------------------------------------------
-spec exec_test(M :: atom(), F :: atom(), Inputs :: list()) -> list() | {error, term()}.
exec_test(M, F, Inputs) ->
  try
    BeforeProcessing = os:timestamp(),
    Ans = annotation:call_advised(M, F, Inputs),
    AfterProcessing = os:timestamp(),
    case check_links() of
      ok ->
        TestTime = timer:now_diff(AfterProcessing, BeforeProcessing),
        case Ans of
          {K, V} when is_number(V) ->
            [{test_time, TestTime}, {K, V}];
          AnsList when is_list(AnsList) ->
            lists:foldl(fun(AnsPart, Acc) ->
              case AnsPart of
                {K2, V2} when is_number(V2) ->
                  [{K2, V2} | Acc];
                _ ->
                  Acc
              end
            end, [{test_time, TestTime}], AnsList);
          _ ->
            [{test_time, TestTime}]
        end;
      E ->
        E
    end
  catch
    E1:E2 ->
      {error, {E1,E2}}
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
    {'EXIT',_,_} ->
      {error, linked_proc_error}
  after 0 ->
    ok
  end.