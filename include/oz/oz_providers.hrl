%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc OZ definition of provider record.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(OZ_PROVIDERS_HRL).
-define(OZ_PROVIDERS_HRL, 1).

%% provider_details record contains following fields:
%% * id               - unique provider ID assigned by OZ
%% * domain         - domain where oneprovider GUI is available
-record(provider_details, {
    id :: binary(),
    name :: binary(),
    domain :: undefined | binary(),
    latitude :: undefined | float(),
    longitude :: undefined | float()
}).

-endif.
