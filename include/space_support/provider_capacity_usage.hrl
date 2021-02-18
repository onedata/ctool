%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions and records used in provider_capacity_usage module.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(PROVIDER_CAPACITY_USAGE_HRL).
-define(PROVIDER_CAPACITY_USAGE_HRL, 1).

% Denotes that no usage report has been submitted by the provider and hence the
% usage is unknown. All legacy supports have this value until upgraded.
-define(UNKNOWN_USAGE_VALUE, -1).

% Refer to provider_capacity_usage.erl for description of below records and types

-record(storage_capacity_usage, {
    granted :: provider_capacity_usage:bytes(),
    used :: provider_capacity_usage:bytes(),
    overfull :: provider_capacity_usage:overfull()
}).

-record(provider_capacity_usage, {
    overfull = false :: provider_capacity_usage:overfull(),
    per_storage = #{} :: #{onedata:storage_id() => #storage_capacity_usage{}}
}).

-record(capacity_usage_registry_update_result, {
    new_registry :: provider_capacity_usage:registry(),
    % informs about providers that transitioned from/to overfull
    % as a result of the previous report consumption
    transitioned_from_overfull = [] :: [onedata:provider_id()],
    transitioned_to_overfull = [] :: [onedata:provider_id()]
}).

-endif.
