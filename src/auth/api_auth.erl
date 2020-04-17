%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module handles verification of authorization in the context of an API
%%% operation. It checks the caveats that have an impact on allowed API:
%%%     * cv_api caveats
%%%     * service caveats (see service_caveats module for more)
%%%     * data access caveats (see data_access_caveats module for more).
%%%
%%% These caveats are lazy; they are not verified during authentication, but
%%% must be verified whenever the #auth{} resulting from a token is used to
%%% authorize an operation.
%%%
%%% NOTE: Service caveats do not explicitly confine the API, but indicate that
%%% the token is intended for specific services and should not work elsewhere.
%%% Hence, they implicitly limit the API to the subset necessary for whitelisted
%%% services - see data_access_caveats:to_allowed_api/1.
%%%
%%% NOTE: Data access caveats do not explicitly confine the API, but their
%%% presence indicates that the token is intended for data access only, which
%%% means the allowed API should be limited to necessary minimum. The available
%%% API depends on the caveat (see data_access_caveats:to_allowed_api/2).
%%% @end
%%%--------------------------------------------------------------------
-module(api_auth).
-author("Lukasz Opiola").

-include("errors.hrl").
-include("aai/aai.hrl").
-include("graph_sync/gri.hrl").

%% API
-export([check_authorization/4]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Verifies authorization to perform an operation based on given caveats.
%% Three checks are performed:
%%  1) The operation must be whitelisted in all cv_api caveats (if any)
%%  2) If any service caveat is present, the operation must fall within
%%     allowed API imposed by the caveat
%%  3) If any data access caveat is present, the operation must fall within
%%     allowed API imposed by the caveat.
%% @end
%%--------------------------------------------------------------------
-spec check_authorization(aai:auth(), onedata:service(), cv_api:operation(), gri:gri()) ->
    ok | errors:error().
check_authorization(#auth{caveats = Caveats}, Service, Operation, GRI) ->
    case verify_api_caveats_against_operation(Caveats, Service, Operation, GRI) of
        ok ->
            case verify_service_caveats_against_operation(Caveats, Service, Operation, GRI) of
                ok ->
                    verify_data_access_caveats_against_operation(Caveats, Service, Operation, GRI);
                {error, _} = Error2 ->
                    Error2
            end;
        {error, _} = Error1 ->
            Error1
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Verifies all provided API caveats against an operation. If no API caveats
%% are given, the verification is always successful (whole API is allowed).
%% @end
%%--------------------------------------------------------------------
-spec verify_api_caveats_against_operation([caveats:caveat()], onedata:service(), cv_api:operation(), gri:gri()) ->
    ok | errors:error().
verify_api_caveats_against_operation(Caveats, Service, Operation, GRI) ->
    ApiCaveats = caveats:filter([cv_api], Caveats),
    lists:foldl(fun
        (ApiCaveat, ok) ->
            cv_api:verify(ApiCaveat, Service, Operation, GRI);
        (_ApiCaveat, {error, _} = Error) ->
            Error
    end, ok, ApiCaveats).


%% @private
-spec verify_service_caveats_against_operation([caveats:caveat()], onedata:service(),
    cv_api:operation(), gri:gri()) -> ok | errors:error().
verify_service_caveats_against_operation(Caveats, Service, Operation, GRI) ->
    ServiceCaveats = service_caveats:filter(Caveats),
    lists:foldl(fun
        (ServiceCaveat, ok) ->
            ApiCaveat = service_caveats:to_allowed_api(ServiceCaveat),
            case cv_api:verify(ApiCaveat, Service, Operation, GRI) of
                ok -> ok;
                {error, _} -> ?ERROR_TOKEN_CAVEAT_UNVERIFIED(ServiceCaveat)
            end;
        (_ServiceCaveat, {error, _} = Error) ->
            Error
    end, ok, ServiceCaveats).


%% @private
-spec verify_data_access_caveats_against_operation([caveats:caveat()], onedata:service(),
    cv_api:operation(), gri:gri()) -> ok | errors:error().
verify_data_access_caveats_against_operation(Caveats, Service, Operation, GRI) ->
    DataAccessCaveats = data_access_caveats:filter(Caveats),
    lists:foldl(fun
        (DataAccessCaveat, ok) ->
            ApiCaveat = data_access_caveats:to_allowed_api(Service, DataAccessCaveat),
            case cv_api:verify(ApiCaveat, Service, Operation, GRI) of
                ok -> ok;
                {error, _} -> ?ERROR_TOKEN_CAVEAT_UNVERIFIED(DataAccessCaveat)
            end;
        (_DataAccessCaveat, {error, _} = Error) ->
            Error
    end, ok, DataAccessCaveats).