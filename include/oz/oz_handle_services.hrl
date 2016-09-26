%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc OZ definition of handle_service_details record.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(OZ_HANDLE_SERVICES_HRL).
-define(OZ_HANDLE_SERVICES_HRL, 1).

-record(handle_service_details, {
    name :: binary() | undefined,
    proxy_endpoint :: binary() | undefined,
    service_properties = [] :: [term()]
}).

-endif.
