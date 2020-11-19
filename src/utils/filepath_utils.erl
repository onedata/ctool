%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015-2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This file contains various functions operating on file paths
%%% @end
%%%--------------------------------------------------------------------
-module(filepath_utils).
-author("Tomasz Lichon").

% File path validation
-export([ends_with_slash/1]).

% File path manipulation
-export([
    ensure_ends_with_slash/1, ensure_begins_with_slash/1,
    ensure_begins_with_prefix/2, parent_dir/1
]).

-export([sanitize/1]).
-export([is_ancestor/2, is_equal_or_descendant/2, is_descendant/2]).
-export([consolidate/1, intersect/2, check_relation/2]).

-type name() :: binary().

-type raw_path() :: binary().
% Absolute filepath with no whitespaces and no trailing '/'
-type sanitized_path() :: binary().

-type relation() :: equal | descendant | {ancestor, ordsets:ordset(name())}.

-export_type([name/0, raw_path/0, sanitized_path/0, relation/0]).


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Returns true when given path ends with '/'
%%--------------------------------------------------------------------
-spec ends_with_slash(binary()) -> boolean().
ends_with_slash(<<"">>) ->
    false;
ends_with_slash(Path) ->
    binary:last(Path) =:= $/.


%%--------------------------------------------------------------------
%% @doc
%% Appends '/' to the end of filepath if last character is different
%% @end
%%--------------------------------------------------------------------
-spec ensure_ends_with_slash(binary()) -> binary().
ensure_ends_with_slash(<<"">>) ->
    <<"/">>;
ensure_ends_with_slash(Path) ->
    case ends_with_slash(Path) of
        true -> Path;
        false -> <<Path/binary, "/">>
    end.


%%--------------------------------------------------------------------
%% @equiv ensure_begins_with_prefix(Path, <<"/">>)
%%--------------------------------------------------------------------
-spec ensure_begins_with_slash(Path :: binary()) -> binary().
ensure_begins_with_slash(Path) ->
    ensure_begins_with_prefix(Path, <<"/">>).


%%--------------------------------------------------------------------
%% @doc Ensures that path begins with given prefix
%%--------------------------------------------------------------------
-spec ensure_begins_with_prefix(Path :: binary(), Prefix :: binary()) -> binary().
ensure_begins_with_prefix(Path, Prefix) ->
    Size = size(Prefix),
    case Path of
        <<Prefix:Size/binary, _/binary>> ->
            Path;
        _ ->
            <<Prefix/binary, Path/binary>>
    end.


%%--------------------------------------------------------------------
%% @doc Get parent dir
%%--------------------------------------------------------------------
-spec parent_dir(binary()) -> binary().
parent_dir(Path) ->
    ensure_ends_with_slash(filename:dirname(filename:absname(Path))).


-spec sanitize(raw_path()) -> {ok, sanitized_path()}.
sanitize(RawPath) ->
    ensure_absolute_path(string:trim(string:trim(RawPath), trailing, "/")).



-spec is_ancestor(sanitized_path(), sanitized_path()) -> {true, name()} | false.
is_ancestor(PossibleAncestor, ReferencePath) ->
    is_ancestor(PossibleAncestor, byte_size(PossibleAncestor), ReferencePath).


-spec is_equal_or_descendant(sanitized_path(), sanitized_path()) -> boolean().
is_equal_or_descendant(PossibleDescendant, ReferencePath) ->
    is_equal_or_descendant(PossibleDescendant, ReferencePath, byte_size(ReferencePath)).


-spec is_descendant(sanitized_path(), sanitized_path()) -> boolean().
is_descendant(PossibleDescendant, ReferencePath) ->
    is_descendant(PossibleDescendant, ReferencePath, byte_size(ReferencePath)).


%%--------------------------------------------------------------------
%% @doc
%% Consolidates paths by removing ones that are descendants of others, e.g.:
%% >> consolidate([/a/b, /a/b/c, /q/w/e]).
%% [/a/b, /q/w/e]
%% @end
%%--------------------------------------------------------------------
-spec consolidate([sanitized_path()]) -> [sanitized_path()].
consolidate(Paths) ->
    consolidate(lists:usort(Paths), []).


%%--------------------------------------------------------------------
%% @doc
%% Intersects 2 path lists.
%%
%%                        !!! NOTE !!!
%% Path lists must be consolidated before calling this function.
%% @end
%%--------------------------------------------------------------------
-spec intersect([sanitized_path()], [sanitized_path()]) -> [sanitized_path()].
intersect(PathsA, PathsB) ->
    intersect(PathsA, PathsB, []).


%%--------------------------------------------------------------------
%% @doc
%% Checks whether Path is ancestor, equal or descendant to any of specified paths.
%% In case when Path is ancestor to one path and equal/descendant to another,
%% then equal/descendant takes precedence.
%% Additionally, if it is ancestor, it returns collection of it's immediate
%% children names.
%% @end
%%--------------------------------------------------------------------
-spec check_relation(sanitized_path(), [sanitized_path()]) ->
    undefined | relation().
check_relation(Path, ReferencePaths) ->
    PathLen = size(Path),

    lists:foldl(fun
        (_, descendant) ->
            descendant;
        (_, equal) ->
            equal;
        (ReferencePath, Acc) ->
            ReferencePathLen = size(ReferencePath),
            case PathLen >= ReferencePathLen of
                true ->
                    case Path == ReferencePath of
                        true ->
                            equal;
                        false ->
                            case is_descendant(Path, ReferencePath, ReferencePathLen) of
                                true -> descendant;
                                false -> Acc
                            end
                    end;
                false ->
                    case is_ancestor(Path, PathLen, ReferencePath) of
                        {true, Child} ->
                            NamesAcc = case Acc of
                                undefined -> ordsets:new();
                                {ancestor, Children} -> Children
                            end,
                            {ancestor, ordsets:add_element(Child, NamesAcc)};
                        false -> Acc
                    end
            end
    end, undefined, ReferencePaths).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec consolidate(Paths :: [sanitized_path()], SortedConsolidatedPaths :: [sanitized_path()]) ->
    UpdatedSortedConsolidatedPaths :: [sanitized_path()].
consolidate([], SortedConsolidatedPaths) ->
    lists:reverse(SortedConsolidatedPaths);
consolidate([Path], SortedConsolidatedPaths) ->
    lists:reverse([Path | SortedConsolidatedPaths]);
consolidate([PathA, PathB | RestOfPaths], SortedConsolidatedPaths) ->
    case is_equal_or_descendant(PathB, PathA) of
        true ->
            consolidate([PathA | RestOfPaths], SortedConsolidatedPaths);
        false ->
            consolidate([PathB | RestOfPaths], [PathA | SortedConsolidatedPaths])
    end.


%% @private
-spec intersect(
    PathsSetA :: [sanitized_path()],
    PathsSetB :: [sanitized_path()],
    Intersection :: [sanitized_path()]
) ->
    UpdatedIntersection :: [sanitized_path()].
intersect([], _, Intersection) ->
    lists:reverse(Intersection);
intersect(_, [], Intersection) ->
    lists:reverse(Intersection);
intersect([PathA | RestA] = PathsA, [PathB | RestB] = PathsB, Intersection) ->
    PathALen = size(PathA),
    PathBLen = size(PathB),

    case PathA < PathB of
        true ->
            case is_descendant(PathB, PathA, PathALen) of
                true ->
                    intersect(RestA, RestB, [PathB | Intersection]);
                false ->
                    intersect(RestA, PathsB, Intersection)
            end;
        false ->
            case is_equal_or_descendant(PathA, PathB, PathBLen) of
                true ->
                    intersect(RestA, RestB, [PathA | Intersection]);
                false ->
                    intersect(PathsA, RestB, Intersection)
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks whether PossibleAncestor is ancestor of ReferencePath. If it is then
%% returns additionally it's immediate child.
%% @end
%%--------------------------------------------------------------------
-spec is_ancestor(sanitized_path(), pos_integer(), sanitized_path()) ->
    {true, name()} | false.
is_ancestor(PossibleAncestor, PossibleAncestorLen, ReferencePath) ->
    case ReferencePath of
        <<PossibleAncestor:PossibleAncestorLen/binary, "/", RelativePath/binary>> ->
            [Name | _] = string:split(RelativePath, <<"/">>),
            {true, Name};
        _ ->
            false
    end.


%% @private
-spec is_equal_or_descendant(sanitized_path(), sanitized_path(), pos_integer()) ->
    boolean().
is_equal_or_descendant(Path, Path, _PathLen) ->
    true;
is_equal_or_descendant(PossibleDescendant, ReferencePath, PathLen) ->
    is_descendant(PossibleDescendant, ReferencePath, PathLen).


%% @private
-spec is_descendant(sanitized_path(), sanitized_path(), pos_integer()) ->
    boolean().
is_descendant(PossibleDescendant, ReferencePath, ReferencePathLen) ->
    case PossibleDescendant of
        <<ReferencePath:ReferencePathLen/binary, "/", _/binary>> ->
            true;
        _ ->
            false
    end.


%% @private
-spec ensure_absolute_path(sanitized_path()) ->
    {ok, sanitized_path()} | {error, relative_path}.
ensure_absolute_path(RawPath) ->
    Tokens = binary:split(RawPath, <<"/">>, [global]),
    case lists:any(fun(X) -> X == <<".">> orelse X == <<"..">> end, Tokens) of
        true ->
            {error, relative_path};
        _ ->
            {ok, ensure_begins_with_slash(RawPath)}
    end.
