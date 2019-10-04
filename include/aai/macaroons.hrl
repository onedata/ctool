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
-ifndef(MACAROONS_HRL).
-define(MACAROONS_HRL, 1).


-define(TIME_CAVEAT(Timestamp, MaxTtl), {time, Timestamp, MaxTtl}).
-define(TIME_INFINITY, infinity).

-define(AUTHORIZATION_NONE_CAVEAT, authorization_none).

-define(AUDIENCE_CAVEAT(Audience), {audience, Audience}).

-endif.