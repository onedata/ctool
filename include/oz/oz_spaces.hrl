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

-record(space_details, {
    % Required fields
    id :: binary(),
    name :: binary(),
    canonicalName = undefined :: undefined | binary(),
    providers_supports = [] :: [{ProviderId :: binary(), Size :: non_neg_integer()}],
    shares = [] :: [binary()]
}).

-endif.
