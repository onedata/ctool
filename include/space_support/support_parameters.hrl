%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions and records related to support_parameters.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(SUPPORT_PARAMETERS_HRL).
-define(SUPPORT_PARAMETERS_HRL, 1).

-record(support_parameters, {
    accounting_enabled = undefined :: undefined | boolean(),
    dir_stats_service_enabled = undefined :: undefined | boolean(),
    dir_stats_service_status = undefined :: undefined | support_parameters:dir_stats_service_status()
}).

-record(support_parameters_registry, {
    registry = #{} :: support_parameters_registry:registry()
}).

% set by default if no specific parameters are requested
-define(DEFAULT_SUPPORT_PARAMETERS, #support_parameters{
    accounting_enabled = false,
    dir_stats_service_enabled = true,
    dir_stats_service_status = disabled
}).

-endif.
