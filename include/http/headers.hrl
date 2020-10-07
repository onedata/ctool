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
-define(HDR_AUTHORIZATION, <<"authorization">>).
-define(HDR_X_AUTH_TOKEN, <<"x-auth-token">>).
-define(HDR_MACAROON, <<"macaroon">>). % @todo VFS-5554 Deprecated

% Headers carrying the consumer token - of party that consumes a token
-define(HDR_X_ONEDATA_CONSUMER_TOKEN, <<"x-onedata-consumer-token">>).

% Headers carrying the service token - authentication of a service in Onedata
-define(HDR_X_ONEDATA_SERVICE_TOKEN, <<"x-onedata-service-token">>).

% Header carrying original peer IP for request proxied by http_port_forwarder
-define(HDR_X_ONEDATA_FORWARDED_FOR, <<"x-onedata-forwarded-for">>).


%% Standard HTTP headers

-define(HDR_ACCEPT, <<"accept">>).
-define(HDR_ACCESS_CONTROL_ALLOW_HEADERS, <<"access-control-allow-headers">>).
-define(HDR_ACCESS_CONTROL_ALLOW_METHODS, <<"access-control-allow-methods">>).
-define(HDR_ACCESS_CONTROL_ALLOW_ORIGIN, <<"access-control-allow-origin">>).
-define(HDR_ACCESS_CONTROL_REQUEST_METHOD, <<"access-control-request-method">>).
-define(HDR_ALLOW, <<"allow">>).
-define(HDR_CONNECTION, <<"connection">>).
-define(HDR_CONTENT_DISPOSITION, <<"content-disposition">>).
-define(HDR_CONTENT_LENGTH, <<"content-length">>).
-define(HDR_CONTENT_TYPE, <<"content-type">>).
-define(HDR_SET_COOKIE, <<"set-cookie">>).
-define(HDR_COOKIE, <<"cookie">>).
-define(HDR_LOCATION, <<"location">>).
-define(HDR_UPGRADE, <<"upgrade">>).
-define(HDR_CONTENT_SECURITY_POLICY, <<"content-security-policy">>).
-define(HDR_CACHE_CONTROL, <<"cache-control">>).
-define(HDR_PRAGMA, <<"pragma">>).
-define(HDR_EXPIRES, <<"expires">>).

-endif.
