%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains convenience macro to execute generic requests
%%% with error handling.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(OZ_RUNNER_HRL).
-define(OZ_RUNNER_HRL, 1).

-define(run(RequestBody),
    begin
        {current_function, CurrentFunction} = process_info(self(), current_function),
        oz_runner:run(CurrentFunction, RequestBody)
    end).

-endif.