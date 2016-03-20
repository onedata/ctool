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
%% * size - Space size
-record(space_details, {
    id :: binary(),
    name :: binary(),
    size :: [{ProviderId :: binary(), Size :: pos_integer()}]
}).

-endif.