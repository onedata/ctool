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

% Set:
% - for space supports that have been created before the dir stats service was
%   introduced. Currently, such space supports have dir stats disabled by
%   default, they must be manually enabled after upgrading the software
%   to a version supporting them.
% - in case of new supports granted by legacy providers (which don't support dir stats).
-define(DEFAULT_SUPPORT_PARAMETERS_FOR_LEGACY_PROVIDERS, #support_parameters{
    accounting_enabled = false,
    dir_stats_service_enabled = false,
    dir_stats_service_status = disabled
}).

-endif.
