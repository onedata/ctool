%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions and records used in provider_sync_progress module.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(PROVIDER_SYNC_PROGRESS_HRL).
-define(PROVIDER_SYNC_PROGRESS_HRL, 1).

% Refer to provider_sync_progress.erl for description of below records and types

-record(peer_summary, {
    seen_seq :: provider_sync_progress:seen_seq(),
    seq_timestamp :: provider_sync_progress:seq_timestamp(),
    diff :: provider_sync_progress:diff(),
    delay :: provider_sync_progress:delay(),
    desync :: provider_sync_progress:desync()
}).

-record(provider_summary, {
    legacy :: provider_sync_progress:legacy(),
    joining :: provider_sync_progress:joining(),
    archival :: provider_sync_progress:archival(),
    % true if any of the peer summaries for other providers is marked as desync
    desync :: provider_sync_progress:desync(),
    % timestamp of the last sync progress report of the provider
    last_report = 0 :: time:seconds(),
    % entries for each provider in the space, including self
    per_peer :: #{provider_sync_progress:provider_id() => #peer_summary{}}
}).

-record(registry_update_result, {
    new_registry :: provider_sync_progress:registry(),
    % informs about providers that transitioned from joining of from/to desync
    % as a result of the previous report consumption
    transitioned_from_joining = [] :: [provider_sync_progress:provider_id()],
    transitioned_from_desync = [] :: [provider_sync_progress:provider_id()],
    transitioned_to_desync = [] :: [provider_sync_progress:provider_id()]
}).

-endif.
