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

-ifndef(SPACE_SUPPORT_HRL).
-define(SPACE_SUPPORT_HRL, 1).

%% Used to denote that the provider is in legacy version that does not recognize
%% support stages. When any transition is applied, this entry is overwritten
%% (as if the space has been freshly supported).
-define(LEGACY_SUPPORT_STAGE_DETAILS, legacy_support).

-record(support_stage_details, {
    provider_stage :: support_stage:provider_support_stage(),
    per_storage :: #{
        support_stage:storage_id() => support_stage:storage_support_stage()
    }
}).

-endif.
