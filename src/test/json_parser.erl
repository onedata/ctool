%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Json utils functions
%%% @end
%%%-------------------------------------------------------------------
-module(json_parser).
-author("Tomasz Lichon").

%% API
-export([parse_json_binary_to_atom_proplist/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Parse json binary as proplist of atoms
%% i. e.
%% json binary: {"a": ["a1"], "b": ["b1", "b2"]}
%% is converted to erlang proplist: [{a, [a1]}, {b, [b1, b2]}]
%% @end
%%--------------------------------------------------------------------
-spec parse_json_binary_to_atom_proplist(JsonBinary :: binary()) -> list() | no_return().
parse_json_binary_to_atom_proplist(JsonBinary) ->
    Json = json_utils:decode(JsonBinary),
    convert_to_atoms(Json).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Parse proplist containing binaries to proplist containing atoms
%% @end
%%--------------------------------------------------------------------
-spec convert_to_atoms(List :: list()) -> list() | no_return().
convert_to_atoms([]) ->
    [];
convert_to_atoms({K, V}) ->
    {binary_to_atom(K, unicode), convert_to_atoms(V)};
convert_to_atoms([Head | Tail]) ->
    [convert_to_atoms(Head) | convert_to_atoms(Tail)];
convert_to_atoms(Binary) when is_binary(Binary) ->
    binary_to_atom(Binary, unicode);
convert_to_atoms(Other) ->
    Other.
