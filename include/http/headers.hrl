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

% Header containing original peer IP for connection proxied by http_port_forwarder
-define(HTTP_X_ONEDATA_FORWARDED_FOR, <<"x-onedata-forwarded-for">>).

-endif.
