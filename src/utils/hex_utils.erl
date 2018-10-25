%%%-------------------------------------------------------------------
%%% @author Michał Stanisz
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc 
%%% This module provides an interface to the hex NIF library.
%%% @end
%%%-------------------------------------------------------------------
-module(hex_utils).

-include("global_definitions.hrl").
-include_lib("ctool/include/logging.hrl").

-export([hex/1]).
-on_load(init/0).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Converts binary string to a binary hex string.
%% @end
%%--------------------------------------------------------------------
-spec hex(binary()) -> binary().
hex(_) ->
    erlang:nif_error(hex_utils_not_loaded).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initialization function for the module.
%% Loads the NIF native library. The library is searched for
%% in application priv dir.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok | {error, Reason :: term()}.
init() ->
    LibName = "hex_utils",
    LibPath = filename:join(code:priv_dir(?CTOOL_APP_NAME), LibName),
    
    case erlang:load_nif(LibPath, 0) of
        ok -> ok;
        {error, {reload, _}} -> ok;
        {error, Reason} -> {error, Reason}
    end.
