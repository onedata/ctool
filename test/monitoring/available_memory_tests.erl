%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Unit tests for available_memory module.
%%% @end
%%%--------------------------------------------------------------------
-module(available_memory_tests).
-author("Tomasz Lichon").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

calculate_test() ->
    Val = available_memory:calculate(),
    ?assert(is_integer(Val)),
    ?assert(Val>0).

-endif.