%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Behaviour to standardize listener API for Onedata products.
%%% @end
%%%-------------------------------------------------------------------
-module(listener_behaviour).
-author("Lukasz Opiola").


%%--------------------------------------------------------------------
%% @doc
%% Returns the port on which the listener is running.
%% @end
%%--------------------------------------------------------------------
-callback port() -> integer().


%%--------------------------------------------------------------------
%% @doc
%% Starts the listener.
%% @end
%%--------------------------------------------------------------------
-callback start() -> ok | {error, Reason :: term()}.


%%--------------------------------------------------------------------
%% @doc
%% Returns the status of the listener.
%% @end
%%--------------------------------------------------------------------
-callback healthcheck() -> ok | {error, server_not_responding}.


%%--------------------------------------------------------------------
%% @doc
%% Stops the listener and performs any required cleanup.
%% @end
%%--------------------------------------------------------------------
-callback stop() -> ok | {error, Reason :: term()}.


%%--------------------------------------------------------------------
%% @doc
%% Reloads web certs and restarts.
%% @end
%%--------------------------------------------------------------------
-callback reload_web_certs() -> ok | {error, Reason :: term()}.
