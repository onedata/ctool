%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module defines jsonable record interface - implemented for erlang
%%% records that are to be encoded/decoded to/from JSON. Each such record should
%%% have a dedicated module implementing the callbacks.
%%%
%%% This interface is intended especially for records stored in persistent
%%% database and transmitted via system APIs.
%%% @end
%%%-------------------------------------------------------------------
-module(jsonable_record).
-author("Bartosz Walkowicz").


%%--------------------------------------------------------------------
%% @doc
%% Returns the current version of the record's definition (as defined in code).
%% The version is used to compare versions and trigger an upgrade if needed.
%% @end
%%--------------------------------------------------------------------
-callback version() -> record_json_encoder:record_version().


%%--------------------------------------------------------------------
%% @doc
%% Encodes record as json object.
%% @end
%%--------------------------------------------------------------------
-callback to_json(record_json_encoder:record()) -> json_utils:json_map().


%%--------------------------------------------------------------------
%% @doc
%% Decodes record from json object.
%% @end
%%--------------------------------------------------------------------
-callback from_json(json_utils:json_map()) -> record_json_encoder:record().


%%--------------------------------------------------------------------
%% @doc
%% Upgrades older records (must be implemented if record version > 1).
%% @end
%%--------------------------------------------------------------------
-callback upgrade_json(record_json_encoder:record_version(), json_utils:json_map()) ->
    {record_json_encoder:record_version(), json_utils:json_map()}.


-optional_callbacks([upgrade_json/2]).