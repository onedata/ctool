%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains configuration records for performance tests.
%%% @end
%%%-------------------------------------------------------------------
-ifndef(PERFORMANCE_HRL).
-define(PERFORMANCE_HRL, 1).

%% output test parameter and also internal representation of input test
%% parameter used by performance test module
%% * name        - name of parameter
%% * description - human-readable description of parameter
%% * value       - value of parameter (IMPORTANT! this value must be numeric)
%% * unit        - unit of parameter
-record(parameter, {
    name :: atom(),
    description = "" :: string(),
    value :: term(),
    unit = "" :: string()
}).

-endif.