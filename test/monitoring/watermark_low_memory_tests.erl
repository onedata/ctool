%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Unit tests for monitoring module.
%%% @end
%%%--------------------------------------------------------------------
-module(watermark_low_memory_tests).
-author("Lukasz Opiola").
-author("Krzysztof Trzepla").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


calculate_test() ->
    ?assert(is_integer(watermark_low_memory:calculate())).

-endif.