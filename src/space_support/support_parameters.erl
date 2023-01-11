%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Record expressing the parameters (configuration) of a space support
%%% in the context of a specific supporting provider.
%%% @end
%%%-------------------------------------------------------------------
-module(support_parameters).
-author("Lukasz Opiola").

-behaviour(jsonable_record).
-behaviour(persistent_record).

-include("space_support/support_parameters.hrl").
-include("errors.hrl").


%% API
-export([all_dir_stats_service_statuses/0]).
-export([update/2]).

%% jsonable_record callbacks
-export([to_json/1, from_json/1]).

%% persistent_record callbacks
-export([version/0, db_encode/2, db_decode/2]).


-type dir_stats_service_status() :: initializing | enabled | stopping | disabled.
-export_type([dir_stats_service_status/0]).

-type record() :: #support_parameters{}.
-export_type([record/0]).


%%%===================================================================
%%% API
%%%===================================================================

-spec all_dir_stats_service_statuses() -> [dir_stats_service_status()].
all_dir_stats_service_statuses() ->
    [initializing, enabled, stopping, disabled].


%%--------------------------------------------------------------------
%% @doc
%% Updates support parameters with new values by applying an overlay record,
%% but only the fields that are defined in the overlay record are changed.
%% @end
%%--------------------------------------------------------------------
-spec update(record(), record()) -> {ok, record()} | errors:error().
update(RecordToUpdate, OverlayRecord) ->
    NewRecord = RecordToUpdate#support_parameters{
        accounting_enabled = utils:ensure_defined(
            OverlayRecord#support_parameters.accounting_enabled,
            RecordToUpdate#support_parameters.accounting_enabled
        ),
        dir_stats_service_enabled = utils:ensure_defined(
            OverlayRecord#support_parameters.dir_stats_service_enabled,
            RecordToUpdate#support_parameters.dir_stats_service_enabled
        ),
        dir_stats_service_status = utils:ensure_defined(
            OverlayRecord#support_parameters.dir_stats_service_status,
            RecordToUpdate#support_parameters.dir_stats_service_status
        )
    },
    case NewRecord of
        #support_parameters{accounting_enabled = true, dir_stats_service_enabled = false} ->
            ?ERROR_BAD_DATA(
                <<"dirStatsServiceEnabled">>,
                <<"Dir stats service must be enabled if accounting is enabled">>
            );
        _ ->
            {ok, ensure_dir_stats_service_status_adequate(NewRecord)}
    end.

%%%===================================================================
%%% jsonable_record callbacks
%%%===================================================================

-spec to_json(record()) -> json_utils:json_term().
to_json(Record) ->
    #{
        <<"accountingEnabled">> => utils:undefined_to_null(Record#support_parameters.accounting_enabled),
        <<"dirStatsServiceEnabled">> => utils:undefined_to_null(Record#support_parameters.dir_stats_service_enabled),
        <<"dirStatsServiceStatus">> => case Record#support_parameters.dir_stats_service_status of
            undefined -> null;
            Status -> dir_stats_service_status_to_json(Status)
        end
    }.


-spec from_json(json_utils:json_term()) -> record().
from_json(RecordJson) ->
    #support_parameters{
        accounting_enabled = utils:null_to_undefined(maps:get(<<"accountingEnabled">>, RecordJson, null)),
        dir_stats_service_enabled = utils:null_to_undefined(maps:get(<<"dirStatsServiceEnabled">>, RecordJson, null)),
        dir_stats_service_status = case maps:get(<<"dirStatsServiceStatus">>, RecordJson, null) of
            null -> undefined;
            StatusJson -> dir_stats_service_status_from_json(StatusJson)
        end
    }.

%%%===================================================================
%%% persistent_record callbacks
%%%===================================================================

-spec version() -> persistent_record:record_version().
version() ->
    1.


-spec db_encode(record(), persistent_record:nested_record_encoder()) -> json_utils:json_term().
db_encode(Record, _NestedRecordEncoder) ->
    to_json(Record).


-spec db_decode(json_utils:json_term(), persistent_record:nested_record_decoder()) -> record().
db_decode(RecordJson, _NestedRecordDecoder) ->
    from_json(RecordJson).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec dir_stats_service_status_to_json(dir_stats_service_status()) -> json_utils:json_term().
dir_stats_service_status_to_json(initializing) -> <<"initializing">>;
dir_stats_service_status_to_json(enabled) -> <<"enabled">>;
dir_stats_service_status_to_json(stopping) -> <<"stopping">>;
dir_stats_service_status_to_json(disabled) -> <<"disabled">>.


%% @private
-spec dir_stats_service_status_from_json(json_utils:json_term()) -> dir_stats_service_status().
dir_stats_service_status_from_json(<<"initializing">>) -> initializing;
dir_stats_service_status_from_json(<<"enabled">>) -> enabled;
dir_stats_service_status_from_json(<<"stopping">>) -> stopping;
dir_stats_service_status_from_json(<<"disabled">>) -> disabled.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% When dir_stats_service_enabled flag is toggled and before the change is
%% acknowledged by a provider, the service status may be conflicting with the
%% settings. This function tweaks the status in advance so that it is not confusing,
%% assuming that the provider will sooner or later converge to this status.
%% @end
%%--------------------------------------------------------------------
-spec ensure_dir_stats_service_status_adequate(record()) -> record().
ensure_dir_stats_service_status_adequate(SP = #support_parameters{
    dir_stats_service_enabled = true, dir_stats_service_status = disabled
}) ->
    SP#support_parameters{dir_stats_service_status = initializing};

ensure_dir_stats_service_status_adequate(SP = #support_parameters{
    dir_stats_service_enabled = true, dir_stats_service_status = stopping
}) ->
    SP#support_parameters{dir_stats_service_status = initializing};

ensure_dir_stats_service_status_adequate(SP = #support_parameters{
    dir_stats_service_enabled = false, dir_stats_service_status = enabled
}) ->
    SP#support_parameters{dir_stats_service_status = stopping};

ensure_dir_stats_service_status_adequate(SP = #support_parameters{
    dir_stats_service_enabled = false, dir_stats_service_status = initializing
}) ->
    SP#support_parameters{dir_stats_service_status = stopping};

ensure_dir_stats_service_status_adequate(SP) ->
    SP.
