%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc OZ definition of handle_details record.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(OZ_HANDLES_HRL).
-define(OZ_HANDLES_HRL, 1).

-record(handle_details, {
    handle_service_id :: binary() | undefined,
    public_handle :: binary() | undefined,
    resource_type :: binary() | undefined,
    resource_id :: binary() | undefined,
    metadata :: binary() | undefined,
    timestamp :: calendar:datetime() | undefined
}).

-endif.
