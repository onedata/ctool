%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module provides a high-level API for handling service caveats.
%%% Existence of any service caveat in a token determines that the token
%%% is intended exclusively for accessing whitelisted services and limits the
%%% available API to necessary minimum.
%%%
%%% NOTE: if oz_worker service is whitelisted (possibly among other services),
%%% there are no API limitations at all.
%%% @end
%%%--------------------------------------------------------------------
-module(service_caveats).
-author("Lukasz Opiola").

-include("errors.hrl").
-include("aai/aai.hrl").
-include("graph_sync/gri.hrl").

-type cv_service() :: #cv_service{}.

%% API
-export([filter/1]).
-export([to_allowed_api/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec filter([caveats:caveat()]) -> [cv_service()].
filter(Caveats) ->
    caveats:filter([cv_service], Caveats).


%%--------------------------------------------------------------------
%% @doc
%% Returns the allowed API inferred from service caveat whitelist.
%% @end
%%--------------------------------------------------------------------
-spec to_allowed_api(cv_service()) -> cv_api:cv_api().
to_allowed_api(ServiceCaveat) ->
    AllowedServiceTypes = allowed_service_types(ServiceCaveat),
    #cv_api{whitelist = lists:flatmap(fun(ServiceType) ->
        allowed_api_by_service_type(ServiceType)
    end, AllowedServiceTypes)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec allowed_service_types(cv_service()) -> [onedata:service()].
allowed_service_types(#cv_service{whitelist = Whitelist}) ->
    lists:usort([Type || ?SERVICE(Type, _Id) <- Whitelist]).


%% @private
-spec allowed_api_by_service_type(onedata:service()) -> [cv_api:matchspec()].
allowed_api_by_service_type(?OZ_WORKER) -> [
    {?OZ_WORKER, all, ?GRI_PATTERN('*', '*', '*', '*')}
];
allowed_api_by_service_type(?OZ_PANEL) -> [
    {?OZ_PANEL, all, ?GRI_PATTERN('*', '*', '*', '*')},
    {?OZ_WORKER, get, ?GRI_PATTERN(od_user, '*', '*', '*')},
    {?OZ_WORKER, get, ?GRI_PATTERN(od_cluster, '*', '*', '*')},
    {?OZ_WORKER, get, ?GRI_PATTERN(od_provider, '*', '*', '*')}
];
allowed_api_by_service_type(?OP_WORKER) -> [
    {?OP_WORKER, all, ?GRI_PATTERN('*', '*', '*', '*')},
    {?OZ_WORKER, get, ?GRI_PATTERN(od_user, '*', '*', '*')},
    {?OZ_WORKER, create, ?GRI_PATTERN(od_user, '*', {idp_access_token, '*'}, '*')},
    {?OZ_WORKER, get, ?GRI_PATTERN(od_group, '*', '*', '*')},
    {?OZ_WORKER, get, ?GRI_PATTERN(od_space, '*', '*', '*')},
    {?OZ_WORKER, all, ?GRI_PATTERN(od_share, '*', '*', '*')},
    {?OZ_WORKER, get, ?GRI_PATTERN(od_provider, '*', '*', '*')},
    {?OZ_WORKER, all, ?GRI_PATTERN(od_handle, '*', '*', '*')},
    {?OZ_WORKER, get, ?GRI_PATTERN(od_handle_service, '*', '*', '*')}
];
allowed_api_by_service_type(?OP_PANEL) -> [
    {?OP_PANEL, all, ?GRI_PATTERN('*', '*', '*', '*')},
    {?OZ_WORKER, get, ?GRI_PATTERN(od_user, '*', '*', '*')},
    {?OZ_WORKER, get, ?GRI_PATTERN(od_cluster, '*', '*', '*')},
    {?OZ_WORKER, get, ?GRI_PATTERN(od_provider, '*', '*', '*')}
].
