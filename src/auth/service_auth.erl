%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains functions for packing / unpacking service auth
%%% macaroons. Apart from the auth macaroon itself, they carry information which
%%% service is being authorized. Currently, this functionality applies only to
%%% ?OP_WORKER and ?OP_PANEL services.
%%% @end
%%%--------------------------------------------------------------------
-module(service_auth).
-author("Lukasz Opiola").

-include("onedata.hrl").

%% API
-export([pack/2, unpack/1]).

-define(SEPARATOR, $-).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Packs a service auth macaroon based on service and auth macaroon.
%% @end
%%--------------------------------------------------------------------
-spec pack(onedata:service(), Macaroon :: binary()) -> ServiceAuth :: binary().
pack(Service, Macaroon) when Service == ?OP_WORKER orelse Service == ?OP_PANEL ->
    <<(onedata:service_shortname(Service))/binary, ?SEPARATOR, Macaroon/binary>>;
pack(_, _) ->
    error(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Unpacks a service auth macaroon into service and auth macaroon.
%% @end
%%--------------------------------------------------------------------
-spec unpack(ServiceAuth :: binary()) -> {onedata:service(), Macaroon :: binary()}.
unpack(<<ServiceShortname:3/binary, ?SEPARATOR, ServiceAuth/binary>>) ->
    case onedata:service_from_shortname(ServiceShortname) of
        Service when Service == ?OP_WORKER orelse Service == ?OP_PANEL ->
            {Service, ServiceAuth};
        _ ->
            error(badarg)
    end;
unpack(Macaroon) ->
    % Auth macaroons that were not packed are assumed to be op_worker macaroons
    {?OP_WORKER, Macaroon}.
