%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc OZ definition of Space record.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(OZ_SPACES_HRL).
-define(OZ_SPACES_HRL, 1).

%% space_details record contains following fields:
%% * id   - unique Space ID assigned by OZ
%% * name - Space name
%% * providers_supports - Space support sizes
-record(space_details, {
    id :: binary(),
    name :: binary(),
    providers_supports :: [{ProviderId :: binary(), Size :: non_neg_integer()}]
}).

-endif.