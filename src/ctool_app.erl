%%%--------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc It is the main module of application. It initializes appropriate components of a node.
%%% @end
%%%--------------------------------------------------------------------
-module(ctool_app).
-author("Michal Stanisz").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start(StartType :: application:start_type(), StartArgs :: term()) ->
    {ok, Pid :: pid()}.
start(_StartType, _StartArgs) ->
    onedata_logger:configure_logger(),
    {ok, self()}.


-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.
