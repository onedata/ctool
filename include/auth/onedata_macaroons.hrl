%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains caveat macros used in macaroon building and verifying.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ONEDATA_MACAROONS_HRL).
-define(ONEDATA_MACAROONS_HRL, 1).

-define(TIME_CAVEAT(Timestamp, MaxTtl), {time, Timestamp, MaxTtl}).
-define(TIME_INFINITY, infinity).

-define(AUTHORIZATION_NONE_CAVEAT, authorization_none).

% Takes:
% - session id during macaroon creation
% - verifier fun during macaroon verification: fun(SessionId) -> boolean()
-define(SESSION_ID_CAVEAT(SessionIdOrVerifyFun), {session_id, SessionIdOrVerifyFun}).

-define(SERVICE_TYPE_CAVEAT(SessionType), {service_type, SessionType}).

-define(SERVICE_ID_CAVEAT(ServiceId), {service_id, ServiceId}).

-endif.