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

% Set by default if no specific parameters are requested.
% Currently, all new space supports have dir stats service enabled.
-define(DEFAULT_SUPPORT_PARAMETERS, #support_parameters{
    accounting_enabled = false,
    dir_stats_service_enabled = true,
    % the default parameters are still subject to tweaking;
    % (@see support_parameters:ensure_dir_stats_service_status_adequate/1)
    % so the final status will be consistent with the dir_stats_service_enabled flag
    % (also @see support_parameters_tests.erl)
    dir_stats_service_status = disabled
}).

% Set for spaces that have been created before the dir stats service was introduced.
% Currently, such spaces have dir stats disabled by default, they must be manually
% enabled after upgrading the software to a version supporting them.
-define(POST_SPACE_UPGRADE_SUPPORT_PARAMETERS, #support_parameters{
    accounting_enabled = false,
    dir_stats_service_enabled = false,
    dir_stats_service_status = disabled
}).

-endif.
