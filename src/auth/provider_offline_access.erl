%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module encapsulates concepts related to offline access mechanisms
%%% for Oneproviders. Oneproviders offer operations that are long-lasting and
%%% need to progress even if the ordering client no longer has a session
%%% there – e.g. data transfers or trash cleaning. They must be performed in the
%%% user's context, hence the Oneprovider must be able to obtain user's
%%% authorization for such long-lasting operations.
%%%
%%% Oneproviders can realize offline access by requesting user's offline access
%%% tokens, that are confined as much as possible.
%%% The operation is allowed only if all below conditions are met:
%%%   * the user is supported by the provider
%%%   * the provider presents a valid access token of the user
%%%   * the user has granted offline access permissions for the provider (NYI)
%%%
%%% The offline access tokens have the following parameters:
%%%   * they are always temporary access tokens with TTL configurable by the
%%%     Onezone admin (Oneprovider must control the TTL and schedule recreation of the token if needed)
%%%   * an offline access token can be used to create another offline access token
%%%     (Oneproviders can prolong the access as needed)
%%%   * they include a copy of all data access, interface and API caveats from
%%%     the original access token, but replace the other caveats with:
%%%       * a time caveat, with TTL as stated above
%%%       * a service caveat, limiting the service when the token can be used only to the specific Oneprovider
%%%       * a consumer caveat, limiting consuming party only to the specific Oneprovider
%%%       * an API caveat that allows only reading basic user data (original API caveats are retained!)
%%%   * Note that IP - related caveats are dropped as they are no longer relevant
%%%     (the obtained token will be used only internally in Oneprovider)
%%%   * Note that as interface caveats are retained, Oneprovider must always include the proper interface in the auth
%%%     context, despite the token being actually used only internally.
%%% @end
%%%--------------------------------------------------------------------
-module(provider_offline_access).
-author("Lukasz Opiola").

-include("aai/aai.hrl").
-include("graph_sync/gri.hrl").

%% API
-export([build_token_caveats/3]).

-define(RETAINED_CAVEAT_TYPES, [
    cv_interface,
    cv_api,
    cv_data_readonly,
    cv_data_path,
    cv_data_objectid
]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec build_token_caveats([caveats:caveat()], ProviderId :: onedata:service_id(), time:seconds()) ->
    [caveats:caveat()].
build_token_caveats(OriginalCaveats, ProviderId, Ttl) ->
    RetainedCaveats = caveats:filter(?RETAINED_CAVEAT_TYPES, OriginalCaveats),
    lists:flatten([
        #cv_time{valid_until = global_clock:timestamp_seconds() + Ttl},
        #cv_service{whitelist = [?SERVICE(?OP_WORKER, ProviderId)]},
        #cv_consumer{whitelist = [?SUB(?ONEPROVIDER, ProviderId)]},
        % do not include the API caveat if it's already there
        lists_utils:union([offline_access_api_caveat()], RetainedCaveats)
    ]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec offline_access_api_caveat() -> caveats:caveat().
offline_access_api_caveat() ->
    #cv_api{whitelist = [
        {?OP_WORKER, all, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')},
        {?OZ_WORKER, get, ?GRI_PATTERN(od_user, <<"*">>, <<"*">>, '*')},
        {?OZ_WORKER, create, ?GRI_PATTERN(od_user, <<"*">>, {<<"idp_access_token">>, <<"*">>}, '*')},
        {?OZ_WORKER, get, ?GRI_PATTERN(od_group, <<"*">>, <<"instance">>, shared)},
        {?OZ_WORKER, get, ?GRI_PATTERN(od_space, <<"*">>, <<"*">>, '*')},
        {?OZ_WORKER, get, ?GRI_PATTERN(od_provider, <<"*">>, <<"*">>, '*')}
    ]}.
