%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Macros for HTTP header names used in Onedata.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(HTTP_HEADERS_HRL).
-define(HTTP_HEADERS_HRL, 1).

% Headers carrying subject token
-define(HTTP_AUTHORIZATION, <<"authorization">>).
-define(HTTP_X_AUTH_TOKEN, <<"x-auth-token">>).
-define(HTTP_MACAROON, <<"macaroon">>). % @todo VFS-5554 Deprecated

% Headers carrying audience token
-define(HTTP_X_ONEDATA_AUDIENCE_TOKEN, <<"x-onedata-audience-token">>).

% Header carrying original peer IP for request proxied by http_port_forwarder
-define(HTTP_X_ONEDATA_FORWARDED_FOR, <<"x-onedata-forwarded-for">>).

% Standard HTTP headers
-define(HTTP_CONTENT_TYPE, <<"content-type">>).

-endif.
