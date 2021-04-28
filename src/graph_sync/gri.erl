%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for manipulating GRIs (Graph Resource Identifiers). GRIs are used to
%%% uniquely identify a specific resource in the global graph of entities and
%%% their relations. It is made up of four components:
%%%   entity_type - e.g. 'od_user'
%%%   entity_id   - specific id of an entity of given type, e.g.
%%%                 <<"03c70855ac978d7aff5368c346fb787c47">>
%%%   aspect      - abstract aspect of the entity, e.g. 'client_tokens', used to
%%%                 differentiate operations that can be performed on an entity.
%%%   scope       - used to achieve granular access to an aspect based on client's
%%%                 authorization: 'private', 'protected', 'shared', 'public'.
%%%                 'auto' scope means the maximum scope (if any) the client is
%%%                 authorized to access.
%%% GRI can be used as pattern to match against a regular GRI, in such case it
%%% can contain wildcards, eg.:
%%%  od_space.*.instance:private = #gri_pattern{type = od_space, id = '*', aspect = instance, scope = private}
%%%  od_user.7496a15.*:protected = #gri_pattern{type = od_user, id = <<"7496a15">>, aspect = '*', scope = protected}
%%%  *.*.user,*:auto             = #gri_pattern{type = '*', id = '*', aspect = {user,'*'}, scope = protected}
%%%  *.*.*:*                     = #gri_pattern{type = '*', id = '*', aspect = '*', scope = '*'}
%%% @end
%%%-------------------------------------------------------------------
-module(gri).
-author("Lukasz Opiola").

-include("errors.hrl").
-include("graph_sync/gri.hrl").

-type entity_type() :: oz_worker | od_user | od_group | od_space | od_share
| od_provider | od_handle_service | od_handle | od_cluster | od_harvester
| od_storage | od_token | od_atm_inventory | space_stats | temporary_token_secret
| onp_ceph | onp_cluster | onp_host | onp_panel | onp_provider | onp_service
| onp_space | onp_storage | onp_user | onp_zone
| op_archive | op_dataset | op_file | op_group | op_handle | op_handle_service
| op_metrics | op_provider | op_qos | op_share | op_space | op_transfer | op_user.

-type entity_type_pattern() :: '*' | entity_type().

-type entity_id() :: undefined | binary().
-type entity_id_pattern() :: '*' | entity_id().

-type aspect() :: atom() | {atom(), atom() | binary()}.
-type aspect_pattern() :: '*' | {'*' | atom(), '*' | atom() | binary()} | aspect().

-type scope() :: private | protected | shared | public | auto.
-type scope_pattern() :: '*' | scope().

-type gri() :: #gri{}.
-type gri_pattern() :: #gri_pattern{}.
-type serialized() :: binary().

-export_type([entity_type/0, entity_id/0, aspect/0, scope/0]).
-export_type([entity_type_pattern/0, entity_id_pattern/0, aspect_pattern/0, scope_pattern/0]).
-export_type([gri/0, gri_pattern/0, serialized/0]).

% Indicates the format of the GRI:
%   regular - identifies a certain graph resource
%   pattern - used for matching against regular GRIs, can include wildcards '*'
-type format() :: regular | pattern.

-export([serialize/1, deserialize/1]).
-export([serialize_pattern/1, deserialize_pattern/1]).
-export([serialize_type/1, deserialize_type/1]).
-export([matches/2]).

-define(ATOM_TO_BIN(Atom), atom_to_binary(Atom, utf8)).
-define(BIN_TO_ATOM(Bin), binary_to_existing_atom(Bin, utf8)).

%%%===================================================================
%%% API
%%%===================================================================

-spec serialize(gri()) -> serialized().
serialize(GRI) ->
    serialize(GRI, regular).


-spec deserialize(serialized()) -> gri().
deserialize(Serialized) ->
    deserialize(Serialized, regular).


-spec serialize_pattern(gri_pattern()) -> serialized().
serialize_pattern(GRI) ->
    serialize(GRI, pattern).


-spec deserialize_pattern(serialized()) -> gri_pattern().
deserialize_pattern(Serialized) ->
    deserialize(Serialized, pattern).


-spec matches(gri(), gri_pattern()) -> boolean().
matches(GRI, Pattern) ->
    try
        matches_type(GRI, Pattern) andalso matches_id(GRI, Pattern) andalso
            matches_aspect(GRI, Pattern) andalso matches_scope(GRI, Pattern)
    catch _:_ ->
        false
    end.


-spec serialize_type(entity_type()) -> binary().
serialize_type(Type) ->
    serialize_type(Type, regular).


-spec deserialize_type(binary()) -> entity_type().
deserialize_type(Type) ->
    deserialize_type(Type, regular).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec serialize(gri() | gri_pattern(), format()) -> serialized().
serialize(GRIorPattern, Format) ->
    {Type, Id, Aspect, Scope} = unpack(GRIorPattern, Format),
    <<
        (serialize_type(Type, Format))/binary, ".",
        (serialize_id(Id, Format))/binary, ".",
        (serialize_aspect(Aspect, Format))/binary, ":",
        (serialize_scope(Scope, Format))/binary
    >>.


%% @private
-spec deserialize(serialized(), format()) -> gri() | gri_pattern().
deserialize(Serialized, Format) ->
    try
        [Type, Id, AspectAndScope] = binary:split(Serialized, <<".">>, [global]),
        {Aspect, Scope} = case binary:split(AspectAndScope, <<":">>, [global]) of
            [A, S] -> {A, S};
            [A] -> {A, <<"private">>}
        end,
        pack({
            deserialize_type(Type, Format),
            deserialize_id(Id, Format),
            deserialize_aspect(Aspect, Format),
            deserialize_scope(Scope, Format)
        }, Format)
    catch _:_ ->
        throw(?ERROR_BAD_GRI)
    end.


%% @private
-spec pack({entity_type_pattern(), entity_id_pattern(), aspect_pattern(), scope_pattern()}, format()) ->
    gri() | gri_pattern().
pack({Type, Id, Aspect, Scope}, regular) ->
    #gri{type = Type, id = Id, aspect = Aspect, scope = Scope};
pack({Type, Id, Aspect, Scope}, pattern) ->
    #gri_pattern{type = Type, id = Id, aspect = Aspect, scope = Scope}.


%% @private
-spec unpack(gri() | gri_pattern(), format()) ->
    {entity_type_pattern(), entity_id_pattern(), aspect_pattern(), scope_pattern()}.
unpack(#gri{type = Type, id = Id, aspect = Aspect, scope = Scope}, regular) ->
    {Type, Id, Aspect, Scope};
unpack(#gri_pattern{type = Type, id = Id, aspect = Aspect, scope = Scope}, pattern) ->
    {Type, Id, Aspect, Scope};
unpack(_, _) ->
    throw(?ERROR_BAD_GRI).


%% @private
-spec serialize_type(entity_type_pattern(), format()) -> binary().
serialize_type('*', pattern) -> <<"*">>;
serialize_type('*', regular) -> throw(?ERROR_BAD_GRI);

serialize_type(oz_worker, _) -> <<"oz_worker">>;
serialize_type(od_user, _) -> <<"user">>;
serialize_type(od_group, _) -> <<"group">>;
serialize_type(od_space, _) -> <<"space">>;
serialize_type(od_share, _) -> <<"share">>;
serialize_type(od_provider, _) -> <<"provider">>;
serialize_type(od_handle_service, _) -> <<"handleService">>;
serialize_type(od_handle, _) -> <<"handle">>;
serialize_type(od_cluster, _) -> <<"cluster">>;
serialize_type(od_harvester, _) -> <<"harvester">>;
serialize_type(od_storage, _) -> <<"storage">>;
serialize_type(od_token, _) -> <<"token">>;
serialize_type(od_atm_inventory, _) -> <<"atm_inventory">>;
serialize_type(space_stats, _) -> <<"space_stats">>;
serialize_type(temporary_token_secret, _) -> <<"temporary_token_secret">>;

serialize_type(onp_ceph, _) -> <<"onp_ceph">>;
serialize_type(onp_cluster, _) -> <<"onp_cluster">>;
serialize_type(onp_host, _) -> <<"onp_host">>;
serialize_type(onp_panel, _) -> <<"onp_panel">>;
serialize_type(onp_provider, _) -> <<"onp_provider">>;
serialize_type(onp_service, _) -> <<"onp_service">>;
serialize_type(onp_space, _) -> <<"onp_space">>;
serialize_type(onp_storage, _) -> <<"onp_storage">>;
serialize_type(onp_user, _) -> <<"onp_user">>;
serialize_type(onp_zone, _) -> <<"onp_zone">>;

serialize_type(op_archive, _) -> <<"op_archive">>;
serialize_type(op_dataset, _) -> <<"op_dataset">>;
serialize_type(op_file, _) -> <<"file">>;
serialize_type(op_group, _) -> <<"op_group">>;
serialize_type(op_handle, _) -> <<"op_handle">>;
serialize_type(op_handle_service, _) -> <<"op_handle_service">>;
serialize_type(op_metrics, _) -> <<"op_metrics">>;
serialize_type(op_provider, _) -> <<"op_provider">>;
serialize_type(op_qos, _) -> <<"op_qos">>;
serialize_type(op_share, _) -> <<"op_share">>;
serialize_type(op_space, _) -> <<"op_space">>;
serialize_type(op_transfer, _) -> <<"op_transfer">>;
serialize_type(op_user, _) -> <<"op_user">>;

serialize_type(_, _) -> throw(?ERROR_BAD_GRI).


%% @private
-spec deserialize_type(binary(), format()) -> entity_type_pattern().
deserialize_type(<<"*">>, pattern) -> '*';
deserialize_type(<<"*">>, regular) -> throw(?ERROR_BAD_GRI);

deserialize_type(<<"oz_worker">>, _) -> oz_worker;
deserialize_type(<<"user">>, _) -> od_user;
deserialize_type(<<"group">>, _) -> od_group;
deserialize_type(<<"space">>, _) -> od_space;
deserialize_type(<<"share">>, _) -> od_share;
deserialize_type(<<"provider">>, _) -> od_provider;
deserialize_type(<<"handleService">>, _) -> od_handle_service;
deserialize_type(<<"handle">>, _) -> od_handle;
deserialize_type(<<"cluster">>, _) -> od_cluster;
deserialize_type(<<"harvester">>, _) -> od_harvester;
deserialize_type(<<"storage">>, _) -> od_storage;
deserialize_type(<<"token">>, _) -> od_token;
deserialize_type(<<"atm_inventory">>, _) -> od_atm_inventory;
deserialize_type(<<"space_stats">>, _) -> space_stats;
deserialize_type(<<"temporary_token_secret">>, _) -> temporary_token_secret;

deserialize_type(<<"onp_ceph">>, _) -> onp_ceph;
deserialize_type(<<"onp_cluster">>, _) -> onp_cluster;
deserialize_type(<<"onp_host">>, _) -> onp_host;
deserialize_type(<<"onp_panel">>, _) -> onp_panel;
deserialize_type(<<"onp_provider">>, _) -> onp_provider;
deserialize_type(<<"onp_service">>, _) -> onp_service;
deserialize_type(<<"onp_space">>, _) -> onp_space;
deserialize_type(<<"onp_storage">>, _) -> onp_storage;
deserialize_type(<<"onp_user">>, _) -> onp_user;
deserialize_type(<<"onp_zone">>, _) -> onp_zone;

deserialize_type(<<"op_archive">>, _) -> op_archive;
deserialize_type(<<"op_dataset">>, _) -> op_dataset;
deserialize_type(<<"file">>, _) -> op_file;
deserialize_type(<<"op_group">>, _) -> op_group;
deserialize_type(<<"op_handle">>, _) -> op_handle;
deserialize_type(<<"op_handle_service">>, _) -> op_handle_service;
deserialize_type(<<"op_metrics">>, _) -> op_metrics;
deserialize_type(<<"op_provider">>, _) -> op_provider;
deserialize_type(<<"op_qos">>, _) -> op_qos;
deserialize_type(<<"op_share">>, _) -> op_share;
deserialize_type(<<"op_space">>, _) -> op_space;
deserialize_type(<<"op_transfer">>, _) -> op_transfer;
deserialize_type(<<"op_user">>, _) -> op_user;

deserialize_type(_, _) -> throw(?ERROR_BAD_GRI).


%% @private
-spec matches_type(gri(), gri_pattern()) -> boolean().
matches_type(#gri{type = _}, #gri_pattern{type = '*'}) -> true;
matches_type(#gri{type = Type}, #gri_pattern{type = Type}) -> true;
matches_type(_, _) -> false.


%% @private
-spec serialize_id(entity_id_pattern(), format()) -> binary().
serialize_id(undefined, _) -> <<"null">>;
serialize_id(?SELF, _) -> <<"self">>;
serialize_id('*', pattern) -> <<"*">>;
serialize_id('*', regular) -> throw(?ERROR_BAD_GRI);
serialize_id(Bin, _) when is_binary(Bin) -> Bin.


%% @private
-spec deserialize_id(binary(), format()) -> entity_id_pattern().
deserialize_id(<<"null">>, _) -> undefined;
deserialize_id(<<"self">>, _) -> ?SELF;
deserialize_id(<<"*">>, pattern) -> '*';
deserialize_id(<<"*">>, regular) -> throw(?ERROR_BAD_GRI);
deserialize_id(Bin, _) -> Bin.


%% @private
-spec matches_id(gri(), gri_pattern()) -> boolean().
matches_id(#gri{id = _}, #gri_pattern{id = '*'}) -> true;
matches_id(#gri{id = Id}, #gri_pattern{id = Id}) -> true;
matches_id(_, _) -> false.


%% @private
-spec serialize_aspect(aspect_pattern(), format()) -> binary().
serialize_aspect({'*', '*'}, pattern) -> <<"*,*">>;
serialize_aspect({'*', Bin}, pattern) -> <<"*,", Bin/binary>>;
serialize_aspect({Atom, '*'}, pattern) when is_atom(Atom) -> <<(?ATOM_TO_BIN(Atom))/binary, ",*">>;
serialize_aspect({'*', _}, regular) -> throw(?ERROR_BAD_GRI);
serialize_aspect({_, '*'}, regular) -> throw(?ERROR_BAD_GRI);
serialize_aspect({A, B}, _) when is_atom(A) andalso is_binary(B) -> <<(?ATOM_TO_BIN(A))/binary, ",", B/binary>>;
serialize_aspect('*', pattern) -> <<"*">>;
serialize_aspect('*', regular) -> throw(?ERROR_BAD_GRI);
serialize_aspect(Aspect, _) when is_atom(Aspect) -> ?ATOM_TO_BIN(Aspect);
serialize_aspect(_, _) -> throw(?ERROR_BAD_GRI).


%% @private
-spec deserialize_aspect(binary() | [binary()], format()) -> aspect_pattern().
deserialize_aspect(Binary, Format) when is_binary(Binary) ->
    deserialize_aspect(binary:split(Binary, <<",">>, [global]), Format);
deserialize_aspect([<<"*">>, <<"*">>], pattern) -> {'*', '*'};
deserialize_aspect([<<"*">>, Bin], pattern) -> {'*', Bin};
deserialize_aspect([Bin, <<"*">>], pattern) -> {?BIN_TO_ATOM(Bin), '*'};
deserialize_aspect([_, <<"*">>], regular) -> throw(?ERROR_BAD_GRI);
deserialize_aspect([<<"*">>, _], regular) -> throw(?ERROR_BAD_GRI);
deserialize_aspect([A, B], _) -> {?BIN_TO_ATOM(A), B};
deserialize_aspect([<<"*">>, _], regular) -> throw(?ERROR_BAD_GRI);
deserialize_aspect([<<"*">>], pattern) -> '*';
deserialize_aspect([<<"*">>], regular) -> throw(?ERROR_BAD_GRI);
deserialize_aspect([Bin], _) -> ?BIN_TO_ATOM(Bin).


%% @private
-spec matches_aspect(gri(), gri_pattern()) -> boolean().
matches_aspect(#gri{aspect = _}, #gri_pattern{aspect = '*'}) -> true;
matches_aspect(#gri{aspect = {Aspect, _}}, #gri_pattern{aspect = {Aspect, '*'}}) -> true;
matches_aspect(#gri{aspect = {_, Bin}}, #gri_pattern{aspect = {'*', Bin}}) -> true;
matches_aspect(#gri{aspect = {_, _}}, #gri_pattern{aspect = {'*', '*'}}) -> true;
matches_aspect(#gri{aspect = Aspect}, #gri_pattern{aspect = Aspect}) -> true;
matches_aspect(_, _) -> false.


%% @private
-spec serialize_scope(scope_pattern(), format()) -> binary().
serialize_scope(private, _) -> <<"private">>;
serialize_scope(protected, _) -> <<"protected">>;
serialize_scope(shared, _) -> <<"shared">>;
serialize_scope(public, _) -> <<"public">>;
serialize_scope(auto, _) -> <<"auto">>;
serialize_scope('*', pattern) -> <<"*">>;
serialize_scope('*', regular) -> throw(?ERROR_BAD_GRI).


%% @private
-spec deserialize_scope(binary(), format()) -> scope_pattern().
deserialize_scope(<<"private">>, _) -> private;
deserialize_scope(<<"protected">>, _) -> protected;
deserialize_scope(<<"shared">>, _) -> shared;
deserialize_scope(<<"public">>, _) -> public;
deserialize_scope(<<"auto">>, _) -> auto;
deserialize_scope(<<"*">>, pattern) -> '*';
deserialize_scope(<<"*">>, regular) -> throw(?ERROR_BAD_GRI);
deserialize_scope(_, _) -> throw(?ERROR_BAD_GRI).


%% @private
-spec matches_scope(gri(), gri_pattern()) -> boolean().
matches_scope(#gri{scope = _}, #gri_pattern{scope = '*'}) -> true;
matches_scope(#gri{scope = Scope}, #gri_pattern{scope = Scope}) -> true;
matches_scope(_, _) -> false.