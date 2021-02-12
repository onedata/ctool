%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions and records related to support stages.
%%% @see support_stage
%%% @end
%%%-------------------------------------------------------------------

-ifndef(SUPPORT_STAGE_HRL).
-define(SUPPORT_STAGE_HRL, 1).

% description in support_stage.erl
-record(support_stage_details, {
    provider_stage :: support_stage:provider_support_stage(),
    per_storage :: #{
        onedata:storage_id() => support_stage:storage_support_stage()
    }
}).

-endif.
