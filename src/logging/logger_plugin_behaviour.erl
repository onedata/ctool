%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour specifies an API for logger plugin - a module
%%% that can customize logging procedures. Every project using logging
%%% must implement this behaviour and the implmeneting module
%%% must be called logger_plugin.
%%% @end
%%%-------------------------------------------------------------------
-module(logger_plugin_behaviour).
-author("Lukasz Opiola").

%%--------------------------------------------------------------------
%% @doc
%% Should return a list of key, value tuples to be concatenated
%% to standard log metadata.
%% @end
%%--------------------------------------------------------------------
-callback gather_metadata() -> [{Key :: term(), Value :: term()}].