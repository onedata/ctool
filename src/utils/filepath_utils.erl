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

-export([pure_path/1]).
-export([is_ancestor/2, is_path_or_subpath/2, is_subpath/2]).
-export([consolidate/1, intersect/2, check_paths_relation/2]).

-type filename() :: binary().

-type raw_path() :: binary().
% Sanitized filepath with no whitespaces and no trailing '/'
-type pure_path() :: binary().

-type relation() :: subpath | {ancestor, ordsets:ordset(filename())}.

-export_type([filename/0, raw_path/0, pure_path/0, relation/0]).


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


-spec pure_path(raw_path()) -> pure_path().
pure_path(RawPath) ->
    string:trim(string:trim(RawPath), trailing, "/").


-spec is_ancestor(pure_path(), pure_path()) -> {true, filename()} | false.
is_ancestor(PossibleAncestor, Path) ->
    is_ancestor(PossibleAncestor, byte_size(PossibleAncestor), Path).


-spec is_path_or_subpath(pure_path(), pure_path()) -> boolean().
is_path_or_subpath(PossiblePathOrSubPath, Path) ->
    is_path_or_subpath(PossiblePathOrSubPath, Path, byte_size(Path)).


-spec is_subpath(pure_path(), pure_path()) -> boolean().
is_subpath(PossibleSubPath, Path) ->
    is_subpath(PossibleSubPath, Path, byte_size(Path)).


%%--------------------------------------------------------------------
%% @doc
%% Consolidates paths by removing ones that are subpaths of others, e.g.:
%% >> consolidate([/a/b/, /a/b/c, /q/w/e]).
%% [/a/b, /q/w/e]
%% @end
%%--------------------------------------------------------------------
-spec consolidate([pure_path()]) -> [pure_path()].
consolidate(Paths) ->
    consolidate(lists:usort(Paths), []).


%%--------------------------------------------------------------------
%% @doc
%% Intersects 2 path sets.
%% NOTE !!!
%% Those whitelists must be consolidated before calling this function.
%% @end
%%--------------------------------------------------------------------
-spec intersect([pure_path()], [pure_path()]) -> [pure_path()].
intersect(PathsSetA, PathsSetB) ->
    intersect(PathsSetA, PathsSetB, []).


%%--------------------------------------------------------------------
%% @doc
%% Checks whether Path is ancestor or subpath to any of specified paths.
%% In case when Path is ancestor to one path and subpath to another,
%% then subpath takes precedence.
%% Additionally, if it is ancestor, it returns collection of it's immediate
%% children names.
%% @end
%%--------------------------------------------------------------------
-spec check_paths_relation(pure_path(), [pure_path()]) ->
    undefined | relation().
check_paths_relation(Path, PathsSet) ->
    PathLen = size(Path),

    lists:foldl(fun
        (_, subpath) ->
            subpath;
        (PathToCheck, Acc) ->
            PathToCheckLen = size(PathToCheck),
            case PathLen >= PathToCheckLen of
                true ->
                    case is_path_or_subpath(Path, PathToCheck, PathToCheckLen) of
                        true -> subpath;
                        false -> Acc
                    end;
                false ->
                    case is_ancestor(Path, PathLen, PathToCheck) of
                        {true, Child} ->
                            NamesAcc = case Acc of
                                undefined -> ordsets:new();
                                {ancestor, Children} -> Children
                            end,
                            {ancestor, ordsets:add_element(Child, NamesAcc)};
                        false -> Acc
                    end
            end
    end, undefined, PathsSet).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec consolidate(Paths :: [pure_path()], ConsolidatedPaths :: [pure_path()]) ->
    UpdatedConsolidatedPaths :: [pure_path()].
consolidate([], ConsolidatedPaths) ->
    lists:reverse(ConsolidatedPaths);
consolidate([Path], ConsolidatedPaths) ->
    lists:reverse([Path | ConsolidatedPaths]);
consolidate([PathA, PathB | RestOfPaths], ConsolidatedPaths) ->
    case is_path_or_subpath(PathB, PathA) of
        true ->
            consolidate([PathA | RestOfPaths], ConsolidatedPaths);
        false ->
            consolidate([PathB | RestOfPaths], [PathA | ConsolidatedPaths])
    end.


%% @private
-spec intersect(
    PathsSetA :: [pure_path()],
    PathsSetB :: [pure_path()],
    Intersection :: [pure_path()]
) ->
    UpdatedIntersection :: [pure_path()].
intersect([], _, Intersection) ->
    lists:reverse(Intersection);
intersect(_, [], Intersection) ->
    lists:reverse(Intersection);
intersect([PathA | RestA] = PathsSettA, [PathB | RestB] = PathsSetB, Intersection) ->
    PathALen = size(PathA),
    PathBLen = size(PathB),

    case PathA < PathB of
        true ->
            case is_subpath(PathB, PathA, PathALen) of
                true ->
                    intersect(RestA, RestB, [PathB | Intersection]);
                false ->
                    intersect(RestA, PathsSetB, Intersection)
            end;
        false ->
            case is_path_or_subpath(PathA, PathB, PathBLen) of
                true ->
                    intersect(RestA, RestB, [PathA | Intersection]);
                false ->
                    intersect(PathsSettA, RestB, Intersection)
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks whether PossibleAncestor is ancestor of Path. If it is then
%% returns additionally it's immediate child.
%% @end
%%--------------------------------------------------------------------
-spec is_ancestor(pure_path(), pos_integer(), pure_path()) ->
    {true, filename()} | false.
is_ancestor(PossibleAncestor, PossibleAncestorLen, Path) ->
    case Path of
        <<PossibleAncestor:PossibleAncestorLen/binary, "/", SubPath/binary>> ->
            [Name | _] = string:split(SubPath, <<"/">>),
            {true, Name};
        _ ->
            false
    end.


%% @private
-spec is_path_or_subpath(pure_path(), pure_path(), pos_integer()) ->
    boolean().
is_path_or_subpath(Path, Path, _PathLen) ->
    true;
is_path_or_subpath(PossiblePathOrSubPath, Path, PathLen) ->
    is_subpath(PossiblePathOrSubPath, Path, PathLen).


%% @private
-spec is_subpath(pure_path(), pure_path(), pos_integer()) ->
    boolean().
is_subpath(PossibleSubPath, Path, PathLen) ->
    case PossibleSubPath of
        <<Path:PathLen/binary, "/", _/binary>> ->
            true;
        _ ->
            false
    end.
