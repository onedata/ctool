%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Macros for HTTP code integers.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(HTTP_CODES_HRL).
-define(HTTP_CODES_HRL, 1).

-define(HTTP_200_OK, 200).
-define(HTTP_201_CREATED, 201).
-define(HTTP_202_ACCEPTED, 202).
-define(HTTP_204_NO_CONTENT, 204).
-define(HTTP_206_PARTIAL_CONTENT, 206).

-define(HTTP_301_MOVED_PERMANENTLY, 301).
-define(HTTP_302_FOUND, 302).
-define(HTTP_303_SEE_OTHER, 303).
-define(HTTP_307_TEMPORARY_REDIRECT, 307).

-define(HTTP_400_BAD_REQUEST, 400).
-define(HTTP_401_UNAUTHORIZED, 401).
-define(HTTP_403_FORBIDDEN, 403).
-define(HTTP_404_NOT_FOUND, 404).
-define(HTTP_405_METHOD_NOT_ALLOWED, 405).
-define(HTTP_409_CONFLICT, 409).
-define(HTTP_413_PAYLOAD_TOO_LARGE, 413).
-define(HTTP_415_UNSUPPORTED_MEDIA_TYPE, 415).
-define(HTTP_426_UPGRADE_REQUIRED, 426).
-define(HTTP_429_TOO_MANY_REQUESTS, 429).

-define(HTTP_500_INTERNAL_SERVER_ERROR, 500).
-define(HTTP_501_NOT_IMPLEMENTED, 501).
-define(HTTP_503_SERVICE_UNAVAILABLE, 503).

-endif.
