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

-include("utils/filepath_utils.hrl").

% File path validation
-export([ends_with_slash/1]).

% File path manipulation
-export([
    ensure_ends_with_slash/1, ensure_begins_with_slash/1,
    ensure_begins_with_prefix/2, parent_dir/1, basename_and_parent_dir/1
]).

-export([sanitize/1]).
-export([split/1, join/1]).
-export([is_ancestor/2, is_equal_or_descendant/2, is_descendant/2]).
-export([consolidate/1, intersect/2, check_relation/2]).

-type name() :: binary().

-type raw_path() :: binary().
% Filepath with removed '.' and no trailing '/'
-type sanitized_path() :: binary().

-type path() :: raw_path() | sanitized_path().


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


-spec basename_and_parent_dir(path()) -> {Name :: name(), Parent :: path()}.
basename_and_parent_dir(Path) ->
    case lists:reverse(split(Path)) of
        [Leaf | Tokens] ->
            {Leaf, join([<<?DIRECTORY_SEPARATOR>> | lists:reverse(Tokens)])};
        _ ->
            {<<"">>, <<?DIRECTORY_SEPARATOR>>}
    end.


-spec split(path()) -> [binary()].
split(Path) ->
    Tokens = lists:filter(
        fun(Token) -> Token /= <<".">> end,
        binary:split(Path, <<?DIRECTORY_SEPARATOR>>, [global, trim_all])
    ),
    case Path of
        <<?DIRECTORY_SEPARATOR, _/binary>> ->
            [<<?DIRECTORY_SEPARATOR>> | Tokens];
        _ ->
            Tokens
    end.


-spec join([binary()]) -> path().
join([]) ->
    <<>>;
join([<<?DIRECTORY_SEPARATOR>> = Sep, <<?DIRECTORY_SEPARATOR>> | Tokens]) ->
    join([Sep | Tokens]);
join([<<?DIRECTORY_SEPARATOR>> = Sep | Tokens]) ->
    <<Sep/binary, (join(Tokens))/binary>>;
join(Tokens) ->
    Tokens1 = lists:filtermap(fun(Token) ->
        case string:trim(Token, both, ?DIRECTORY_SEPARATOR) of
            <<>> -> false;
            Bin -> {true, Bin}
        end
    end, Tokens),

    str_utils:join_binary(Tokens1, <<?DIRECTORY_SEPARATOR>>).


-spec sanitize(raw_path()) -> {ok, sanitized_path()} | {error, invalid_path}.
sanitize(RawPath) ->
    Tokens = split(RawPath),
    IsInvalidFilePathToken = fun(Token) ->
        Token == <<"..">> orelse binary:match(Token, <<"\0">>) /= nomatch
    end,
    case lists:any(IsInvalidFilePathToken, Tokens) of
        true ->
            {error, invalid_path};
        false ->
            {ok, join(Tokens)}
    end.


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
-spec consolidate(SortedPaths :: [sanitized_path()], ConsolidatedPaths :: [sanitized_path()]) ->
    UpdatedConsolidatedPaths :: [sanitized_path()].
consolidate([], ConsolidatedPaths) ->
    lists:reverse(ConsolidatedPaths);
consolidate([Path], ConsolidatedPaths) ->
    lists:reverse([Path | ConsolidatedPaths]);
consolidate([PathA, PathB | RestOfSortedPaths], ConsolidatedPaths) ->
    case is_equal_or_descendant(PathB, PathA) of
        true ->
            consolidate([PathA | RestOfSortedPaths], ConsolidatedPaths);
        false ->
            consolidate([PathB | RestOfSortedPaths], [PathA | ConsolidatedPaths])
    end.


%% @private
-spec intersect(
    ConsolidatedPathsA :: [sanitized_path()],
    ConsolidatedPathsB :: [sanitized_path()],
    Intersection :: [sanitized_path()]
) ->
    UpdatedIntersection :: [sanitized_path()].
intersect([], _, Intersection) ->
    lists:reverse(Intersection);
intersect(_, [], Intersection) ->
    lists:reverse(Intersection);
intersect([PathA | RestA] = ConsolidatedPathsA, [PathB | RestB] = ConsolidatedPathsB, Intersection) ->
    PathALen = size(PathA),
    PathBLen = size(PathB),

    case PathA < PathB of
        true ->
            case is_descendant(PathB, PathA, PathALen) of
                true ->
                    intersect(RestA, RestB, [PathB | Intersection]);
                false ->
                    intersect(RestA, ConsolidatedPathsB, Intersection)
            end;
        false ->
            case is_equal_or_descendant(PathA, PathB, PathBLen) of
                true ->
                    intersect(RestA, RestB, [PathA | Intersection]);
                false ->
                    intersect(ConsolidatedPathsA, RestB, Intersection)
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
        <<PossibleAncestor:PossibleAncestorLen/binary, ?DIRECTORY_SEPARATOR, RelPath/binary>> ->
            [Name | _] = string:split(RelPath, <<?DIRECTORY_SEPARATOR>>),
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
        <<ReferencePath:ReferencePathLen/binary, ?DIRECTORY_SEPARATOR, _/binary>> ->
            true;
        _ ->
            false
    end.
