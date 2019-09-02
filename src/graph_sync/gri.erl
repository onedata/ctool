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
%%%     od_space.*.instance:private <-> #gri{type = od_space, id = <<"*">>, aspect = instance, scope = private}
%%%     od_user.7496a15.*:protected <-> #gri{type = od_user, id = <<"7496a15">>, aspect = '*', scope = protected}
%%%     *.*.user,*:auto             <-> #gri{type = '*', id = <<"*">>, aspect = {user,'*'}, scope = protected}
%%%     *.*.*:*                     <-> #gri{type = '*', id = <<"*">>, aspect = '*', scope = '*'}
%%% @end
%%%-------------------------------------------------------------------
-module(gri).
-author("Lukasz Opiola").

-include("api_errors.hrl").
-include("graph_sync/graph_sync.hrl").

% '*' wildcard is used only in GRI patterns
-type entity_type() :: '*' |
oz_worker | od_user | od_group | od_space | od_share |
od_provider | od_handle_service | od_handle | od_cluster | od_harvester |
op_file | op_replica | op_transfer | op_user |
op_space | op_share | op_provider | op_metrics.
-type entity_id() :: undefined | binary().
-type aspect() :: atom() | {atom(), atom() | binary()}.
% '*' wildcard is used only in GRI patterns
-type scope() :: '*' | private | protected | shared | public | auto.
-type gri() :: #gri{}.
-type serialized() :: binary().

-export_type([entity_type/0, entity_id/0, aspect/0, scope/0]).
-export_type([gri/0, serialized/0]).

% Indicates the mode in which GRI is used:
%   regular - identifies a certain graph resource
%   pattern - used for matching against regular GRIs, can include wildcards
-type mode() :: regular | pattern.

-export([serialize/1, deserialize/1]).
-export([serialize_pattern/1, deserialize_pattern/1]).
-export([serialize_type/2, deserialize_type/2]).
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


-spec serialize_pattern(gri()) -> serialized().
serialize_pattern(GRI) ->
    serialize(GRI, pattern).


-spec deserialize_pattern(serialized()) -> gri().
deserialize_pattern(Serialized) ->
    deserialize(Serialized, pattern).


-spec matches(GRI :: gri(), Pattern :: gri()) -> boolean().
matches(GRI, Pattern) ->
    try
        matches_type(GRI, Pattern) andalso matches_id(GRI, Pattern) andalso
            matches_aspect(GRI, Pattern) andalso matches_scope(GRI, Pattern)
    catch _:_ ->
        false
    end.


-spec serialize_type(entity_type(), mode()) -> binary().
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

serialize_type(op_file, _) -> <<"file">>;
serialize_type(op_replica, _) -> <<"op_replica">>;
serialize_type(op_transfer, _) -> <<"op_transfer">>;
serialize_type(op_user, _) -> <<"op_user">>;
serialize_type(op_space, _) -> <<"op_space">>;
serialize_type(op_share, _) -> <<"op_share">>;
serialize_type(op_provider, _) -> <<"op_provider">>;
serialize_type(op_metrics, _) -> <<"op_metrics">>;

serialize_type(_, _) -> throw(?ERROR_BAD_GRI).


-spec deserialize_type(binary(), mode()) -> entity_type().
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

deserialize_type(<<"file">>, _) -> op_file;
deserialize_type(<<"op_replica">>, _) -> op_replica;
deserialize_type(<<"op_transfer">>, _) -> op_transfer;
deserialize_type(<<"op_user">>, _) -> op_user;
deserialize_type(<<"op_space">>, _) -> op_space;
deserialize_type(<<"op_share">>, _) -> op_share;
deserialize_type(<<"op_provider">>, _) -> op_provider;
deserialize_type(<<"op_metrics">>, _) -> op_metrics;

deserialize_type(_, _) -> throw(?ERROR_BAD_GRI).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec serialize(gri(), mode()) -> serialized().
serialize(#gri{type = Type, id = Id, aspect = Aspect, scope = Scope}, Mode) ->
    <<
        (serialize_type(Type, Mode))/binary, ".",
        (serialize_id(Id, Mode))/binary, ".",
        (serialize_aspect(Aspect, Mode))/binary, ":",
        (serialize_scope(Scope, Mode))/binary
    >>.


%% @private
-spec deserialize(serialized(), mode()) -> gri().
deserialize(Serialized, Mode) ->
    try
        [Type, Id, AspectAndScope] = binary:split(Serialized, <<".">>, [global]),
        {Aspect, Scope} = case binary:split(AspectAndScope, <<":">>, [global]) of
            [A, S] -> {A, S};
            [A] -> {A, <<"private">>}
        end,
        #gri{
            type = deserialize_type(Type, Mode),
            id = deserialize_id(Id, Mode),
            aspect = deserialize_aspect(Aspect, Mode),
            scope = deserialize_scope(Scope, Mode)
        }
    catch _:_ ->
        throw(?ERROR_BAD_GRI)
    end.


%% @private
-spec matches_type(GRI :: gri(), Pattern :: gri()) -> boolean().
matches_type(#gri{type = _}, #gri{type = '*'}) -> true;
matches_type(#gri{type = Type}, #gri{type = Type}) -> true;
matches_type(_, _) -> false.


%% @private
-spec serialize_id(undefined | entity_id(), mode()) -> binary().
serialize_id(undefined, _) -> <<"null">>;
serialize_id(?SELF, _) -> <<"self">>;
serialize_id(<<"*">>, pattern) -> <<"*">>;
serialize_id(<<"*">>, regular) -> throw(?ERROR_BAD_GRI);
serialize_id(Bin, _) when is_binary(Bin) -> Bin.


%% @private
-spec deserialize_id(binary(), mode()) -> undefined | entity_id().
deserialize_id(<<"null">>, _) -> undefined;
deserialize_id(<<"self">>, _) -> ?SELF;
deserialize_id(<<"*">>, pattern) -> <<"*">>;
deserialize_id(<<"*">>, regular) -> throw(?ERROR_BAD_GRI);
deserialize_id(Bin, _) -> Bin.


%% @private
-spec matches_id(GRI :: gri(), Pattern :: gri()) -> boolean().
matches_id(#gri{id = _}, #gri{id = <<"*">>}) -> true;
matches_id(#gri{id = Id}, #gri{id = Id}) -> true;
matches_id(_, _) -> false.


%% @private
-spec serialize_aspect(aspect(), mode()) -> binary().
serialize_aspect({'*', <<"*">>}, pattern) -> <<"*,*">>;
serialize_aspect({'*', Bin}, pattern) -> <<"*,", Bin/binary>>;
serialize_aspect({Atom, <<"*">>}, pattern) when is_atom(Atom) -> <<(?ATOM_TO_BIN(Atom))/binary, ",*">>;
serialize_aspect({'*', _}, regular) -> throw(?ERROR_BAD_GRI);
serialize_aspect({_, <<"*">>}, regular) -> throw(?ERROR_BAD_GRI);
serialize_aspect({A, B}, _) when is_atom(A) andalso is_binary(B) -> <<(?ATOM_TO_BIN(A))/binary, ",", B/binary>>;
serialize_aspect('*', pattern) -> <<"*">>;
serialize_aspect('*', regular) -> throw(?ERROR_BAD_GRI);
serialize_aspect(Aspect, _) when is_atom(Aspect) -> ?ATOM_TO_BIN(Aspect);
serialize_aspect(_, _) -> throw(?ERROR_BAD_GRI).


%% @private
-spec deserialize_aspect(binary() | [binary()], mode()) -> aspect().
deserialize_aspect(Binary, Mode) when is_binary(Binary) ->
    deserialize_aspect(binary:split(Binary, <<",">>, [global]), Mode);
deserialize_aspect([<<"*">>, <<"*">>], pattern) -> {'*', <<"*">>};
deserialize_aspect([<<"*">>, Bin], pattern) -> {'*', Bin};
deserialize_aspect([Bin, <<"*">>], pattern) -> {?BIN_TO_ATOM(Bin), <<"*">>};
deserialize_aspect([_, <<"*">>], regular) -> throw(?ERROR_BAD_GRI);
deserialize_aspect([<<"*">>, _], regular) -> throw(?ERROR_BAD_GRI);
deserialize_aspect([A, B], _) -> {?BIN_TO_ATOM(A), B};
deserialize_aspect([<<"*">>, _], regular) -> throw(?ERROR_BAD_GRI);
deserialize_aspect([<<"*">>], pattern) -> '*';
deserialize_aspect([<<"*">>], regular) -> throw(?ERROR_BAD_GRI);
deserialize_aspect([Bin], _) -> ?BIN_TO_ATOM(Bin).


%% @private
-spec matches_aspect(GRI :: gri(), Pattern :: gri()) -> boolean().
matches_aspect(#gri{aspect = _}, #gri{aspect = '*'}) -> true;
matches_aspect(#gri{aspect = {Aspect, _}}, #gri{aspect = {Aspect, <<"*">>}}) -> true;
matches_aspect(#gri{aspect = {_, Bin}}, #gri{aspect = {'*', Bin}}) -> true;
matches_aspect(#gri{aspect = {_, _}}, #gri{aspect = {'*', <<"*">>}}) -> true;
matches_aspect(#gri{aspect = Aspect}, #gri{aspect = Aspect}) -> true;
matches_aspect(_, _) -> false.


%% @private
-spec serialize_scope(scope(), mode()) -> binary().
serialize_scope(private, _) -> <<"private">>;
serialize_scope(protected, _) -> <<"protected">>;
serialize_scope(shared, _) -> <<"shared">>;
serialize_scope(public, _) -> <<"public">>;
serialize_scope(auto, _) -> <<"auto">>;
serialize_scope('*', pattern) -> <<"*">>;
serialize_scope('*', regular) -> throw(?ERROR_BAD_GRI).


%% @private
-spec deserialize_scope(binary(), mode()) -> scope().
deserialize_scope(<<"private">>, _) -> private;
deserialize_scope(<<"protected">>, _) -> protected;
deserialize_scope(<<"shared">>, _) -> shared;
deserialize_scope(<<"public">>, _) -> public;
deserialize_scope(<<"auto">>, _) -> auto;
deserialize_scope(<<"*">>, pattern) -> '*';
deserialize_scope(<<"*">>, regular) -> throw(?ERROR_BAD_GRI);
deserialize_scope(_, _) -> throw(?ERROR_BAD_GRI).


%% @private
-spec matches_scope(GRI :: gri(), Pattern :: gri()) -> boolean().
matches_scope(#gri{scope = _}, #gri{scope = '*'}) -> true;
matches_scope(#gri{scope = Scope}, #gri{scope = Scope}) -> true;
matches_scope(_, _) -> false.