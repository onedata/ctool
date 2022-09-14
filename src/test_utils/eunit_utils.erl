%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Helper functions for eunit tests.
%%% @end
%%%-------------------------------------------------------------------
-module(eunit_utils).
-author("Lukasz Opiola").

%% API
-export([debug_log/2, dump/1]).


%%%===================================================================
%%% API
%%%===================================================================


-spec debug_log(string(), [term()]) -> ok.
debug_log(Format, Args) ->
    FormattedOutput = lists:flatten(io_lib:format(Format, Args)),
    io:format(
        user,
        "~n"
        "----------------------------------------------------------~n"
        "~s~n"
        "----------------------------------------------------------~n",
        [FormattedOutput]
    ),
    io:put_chars(<<>>).  % force flush the log to eunit output


-spec dump(term()) -> ok.
dump(Term) ->
    debug_log("[DUMP] ~p", [Term]).
