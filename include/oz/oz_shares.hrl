%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc OZ definition of Share record.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(OZ_SHARES_HRL).
-define(OZ_SHARES_HRL, 1).

-record(share_details, {
    id :: binary(),
    name :: binary(),
    public_url = undefined :: undefined | binary(),
    root_file = undefined :: undefined | binary(),
    space = undefined :: undefined | binary()
}).

-endif.
