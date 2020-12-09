%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions and records used in support_stage module.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(SUPPORT_STAGE_HRL).
-define(SUPPORT_STAGE_HRL, 1).

% description in support_stage.erl
-record(support_stage_details, {
    provider_stage :: support_stage:provider_support_stage(),
    per_storage :: #{
        support_stage:storage_id() => support_stage:storage_support_stage()
    }
}).

%% Used to denote that the provider is in legacy version that does not recognize
%% the new space support model (stages, sync progress etc).
-define(LEGACY_SUPPORT, legacy_support).

-endif.
