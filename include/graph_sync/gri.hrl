%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions and records connected with Graph Sync.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(GRAPH_SYNC_CTOOL_HRL).
-define(GRAPH_SYNC_CTOOL_HRL, 1).

% Graph Resource Identifier - a record identifying a certain resource in the graph.
-record(gri, {
    type :: gri:entity_type(),
    id :: gri:entity_id(),
    aspect :: gri:aspect(),
    scope = private :: gri:scope()
}).

% Used for GRI pattern matching; each of the fields can contain a wildcard (*)
-record(gri_pattern, {
    type :: gri:entity_type_pattern(),
    id :: gri:entity_id_pattern(),
    aspect :: gri:aspect_pattern(),
    scope = private :: gri:scope_pattern()
}).

-define(GRI(Type, Id, Aspect), #gri{type = Type, id = Id, aspect = Aspect}).
-define(GRI(Type, Id, Aspect, Scope), #gri{type = Type, id = Id, aspect = Aspect, scope = Scope}).
-define(GRI_PATTERN(Type, Id, Aspect), #gri_pattern{type = Type, id = Id, aspect = Aspect}).
-define(GRI_PATTERN(Type, Id, Aspect, Scope), #gri_pattern{type = Type, id = Id, aspect = Aspect, scope = Scope}).

% Special id expressing "myself" (the client that is authenticated)
-define(SELF, <<"self">>).

-endif.
