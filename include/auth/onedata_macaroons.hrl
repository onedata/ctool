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

-define(SESSION_ID_CAVEAT(SessionId), {session_id_caveat, SessionId}).
-define(SESSION_ID_VERIFIER(SessionVerifyFun), {session_id_verifier, SessionVerifyFun}).

-define(CLUSTER_TYPE_CAVEAT(ClusterType), {cluster_type, ClusterType}).

-define(CLUSTER_ID_CAVEAT(ClusterId), {cluster_id, ClusterId}).


-endif.
