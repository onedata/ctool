%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module includes definitions related to cv_interface caveat.
%%% @end
%%%--------------------------------------------------------------------
-module(cv_interface).
-author("Lukasz Opiola").

-type interface() :: rest | oneclient | graphsync.
-export_type([interface/0]).

%% API
-export([valid_interfaces/0]).
-export([serialize_interface/1]).
-export([deserialize_interface/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec valid_interfaces() -> [interface()].
valid_interfaces() ->
    [rest, oneclient, graphsync].


-spec serialize_interface(interface()) -> binary().
serialize_interface(graphsync) -> <<"graphsync">>;
serialize_interface(rest) -> <<"rest">>;
serialize_interface(oneclient) -> <<"oneclient">>.


-spec deserialize_interface(binary()) -> interface().
deserialize_interface(<<"graphsync">>) -> graphsync;
deserialize_interface(<<"rest">>) -> rest;
deserialize_interface(<<"oneclient">>) -> oneclient.
