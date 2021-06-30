%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2015-2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This file contains various functions operating on file paths
%%% TODO VFS-7864 check whether all functions properly handle root directory
%%% @end
%%%--------------------------------------------------------------------
-module(filepath_utils).
-author("Tomasz Lichon").
-author("Bartosz Walkowicz").

-include("onedata.hrl").
-include("errors.hrl").

% File path validation
-export([is_invalid_name/1]).
-export([ends_with_slash/1]).

% File path manipulation
-export([ensure_ends_with_slash/1]).
-export([ensure_begins_with_slash/1, ensure_begins_with_prefix/2]).
-export([parent_dir/1, basename_and_parent_dir/1]).

-export([split_and_skip_dots/1, split/1, join/1]).

-export([sanitize/1]).
-export([is_ancestor/2, is_equal_or_descendant/2, is_descendant/2, relative/2]).
-export([consolidate/1, intersect/2, check_relation/2]).

-type name() :: binary().
-type tokens() :: [name()].

-type raw_path() :: binary().
% Filepath with removed '.' and no trailing '/'
-type sanitized_path() :: binary().

-type path() :: raw_path() | sanitized_path().


-type relation() ::
    equal |
    {descendant, RelPathFromAncestor :: sanitized_path()} |
    {ancestor, RelPathToDescendant :: sanitized_path()}.

-export_type([name/0, tokens/0, raw_path/0, sanitized_path/0, path/0, relation/0]).


%%%===================================================================
%%% API
%%%===================================================================


-spec is_invalid_name(name()) -> boolean().
is_invalid_name(Name) ->
    binary:match(Name, <<"\0">>) /= nomatch.


-spec ends_with_slash(path()) -> boolean().
ends_with_slash(<<"">>) ->
    false;
ends_with_slash(Path) ->
    binary:last(Path) =:= ?DIRECTORY_SEPARATOR_CHAR.


-spec ensure_ends_with_slash(path()) -> path().
ensure_ends_with_slash(<<"">>) ->
    <<?DIRECTORY_SEPARATOR>>;
ensure_ends_with_slash(Path) ->
    case ends_with_slash(Path) of
        true -> Path;
        false -> <<Path/binary, ?DIRECTORY_SEPARATOR>>
    end.


-spec ensure_begins_with_slash(path()) -> path().
ensure_begins_with_slash(Path) ->
    ensure_begins_with_prefix(Path, <<?DIRECTORY_SEPARATOR>>).


-spec ensure_begins_with_prefix(path(), Prefix :: binary()) -> path().
ensure_begins_with_prefix(Path, Prefix) ->
    Size = size(Prefix),
    case Path of
        <<Prefix:Size/binary, _/binary>> ->
            Path;
        _ ->
            <<Prefix/binary, Path/binary>>
    end.


-spec parent_dir(path()) -> path().
parent_dir(Path) ->
    ensure_ends_with_slash(filename:dirname(filename:absname(Path))).


-spec basename_and_parent_dir(Path :: path()) ->
    {Name :: name(), AbsoluteParentPath :: path()}.
basename_and_parent_dir(Path) ->
    case lists:reverse(split(Path)) of
        [Leaf | Tokens] ->
            {Leaf, join([<<?DIRECTORY_SEPARATOR>> | lists:reverse(Tokens)])};
        _ ->
            {<<"">>, <<?DIRECTORY_SEPARATOR>>}
    end.


-spec split_and_skip_dots(path()) -> {ok, tokens()} | {error, invalid_path}.
split_and_skip_dots(Path) ->
    Tokens = lists:filter(fun(Token) -> Token /= <<".">> end, split(Path)),

    case lists:any(fun(X) -> X =:= <<"..">> end, Tokens) of
        true -> {error, invalid_path};
        false -> {ok, Tokens}
    end.


-spec split(path()) -> tokens().
split(Path) ->
    Tokens = binary:split(Path, <<?DIRECTORY_SEPARATOR>>, [global, trim_all]),
    case Path of
        <<?DIRECTORY_SEPARATOR, _/binary>> ->
            [<<?DIRECTORY_SEPARATOR>> | Tokens];
        _ ->
            Tokens
    end.


-spec join(tokens()) -> path().
join([]) ->
    <<>>;
join([FirstToken | _] = Tokens) ->
    TrimmedTokens = lists:filtermap(fun(Token) ->
        case string:trim(Token, both, ?DIRECTORY_SEPARATOR) of
            <<>> -> false;
            Bin -> {true, Bin}
        end
    end, Tokens),

    case FirstToken of
        <<?DIRECTORY_SEPARATOR, _/binary>> ->
            AllTokens = case TrimmedTokens of
                [] -> [<<?DIRECTORY_SEPARATOR>>];
                _ -> [<<>> | TrimmedTokens]
            end,
            str_utils:join_binary(AllTokens, <<?DIRECTORY_SEPARATOR>>);
        _ ->
            str_utils:join_binary(TrimmedTokens, <<?DIRECTORY_SEPARATOR>>)
    end.


-spec sanitize(raw_path()) -> {ok, sanitized_path()} | {error, invalid_path}.
sanitize(RawPath) ->
    case split_and_skip_dots(RawPath) of
        {ok, Tokens} ->
            case lists:any(fun is_invalid_name/1, Tokens) of
                true -> {error, invalid_path};
                false -> {ok, join(Tokens)}
            end;
        {error, _} = Error ->
            Error
    end.


-spec is_ancestor(sanitized_path(), sanitized_path()) ->
    {true, RelPathToDescendant :: sanitized_path()} | false.
is_ancestor(?DIRECTORY_SEPARATOR_BIN, <<?DIRECTORY_SEPARATOR, RelPathToDescendant/binary>>) ->
    {true, RelPathToDescendant};
is_ancestor(PossibleAncestor, ReferencePath) ->
    is_ancestor(PossibleAncestor, byte_size(PossibleAncestor), ReferencePath).


-spec is_equal_or_descendant(sanitized_path(), sanitized_path()) ->
    {true, RelPathFromAncestor :: sanitized_path()} | false.
is_equal_or_descendant(PossibleDescendant, ReferencePath) ->
    is_equal_or_descendant(PossibleDescendant, ReferencePath, byte_size(ReferencePath)).


-spec is_descendant(sanitized_path(), sanitized_path()) ->
    {true, RelPathFromAncestor :: sanitized_path()} | false.
is_descendant(<<?DIRECTORY_SEPARATOR, RelPathFromAncestor/binary>>, ?DIRECTORY_SEPARATOR_BIN) ->
    {true, RelPathFromAncestor};
is_descendant(PossibleDescendant, ReferencePath) ->
    is_descendant(PossibleDescendant, ReferencePath, byte_size(ReferencePath)).


-spec relative(sanitized_path(), sanitized_path()) -> sanitized_path().
relative(AncestorPath, AncestorPath) ->
    <<>>;
relative(AncestorPath, DescendantPath) ->
    case is_ancestor(AncestorPath, DescendantPath) of
        {true, RelativePath} -> RelativePath;
        false -> throw(?EINVAL)
    end.


-spec check_relation(sanitized_path(), sanitized_path()) ->
    undefined | relation().
check_relation(Path, ReferencePath) ->
    PathLen = size(Path),
    ReferencePathLen = size(ReferencePath),

    case PathLen >= ReferencePathLen of
        true ->
            case is_equal_or_descendant(Path, ReferencePath, ReferencePathLen) of
                {true, <<>>} -> equal;
                {true, RelPath} -> {descendant, RelPath};
                false -> undefined
            end;
        false ->
            case is_ancestor(Path, PathLen, ReferencePath) of
                {true, RelPath} -> {ancestor, RelPath};
                false -> undefined
            end
    end.


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
intersect(ConsolidatedPathsA, ConsolidatedPathsB) ->
    intersect(ConsolidatedPathsA, ConsolidatedPathsB, []).


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
        {true, _} ->
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
                {true, _} ->
                    intersect(RestA, RestB, [PathB | Intersection]);
                false ->
                    intersect(RestA, ConsolidatedPathsB, Intersection)
            end;
        false ->
            case is_equal_or_descendant(PathA, PathB, PathBLen) of
                {true, _} ->
                    intersect(RestA, RestB, [PathA | Intersection]);
                false ->
                    intersect(ConsolidatedPathsA, RestB, Intersection)
            end
    end.


%% @private
-spec is_ancestor(sanitized_path(), pos_integer(), sanitized_path()) ->
    {true, RelPathToDescendant :: sanitized_path()} | false.
is_ancestor(PossibleAncestor, PossibleAncestorLen, ReferencePath) ->
    case ReferencePath of
        <<PossibleAncestor:PossibleAncestorLen/binary, ?DIRECTORY_SEPARATOR, RelPath/binary>> ->
            {true, RelPath};
        _ ->
            false
    end.


%% @private
-spec is_equal_or_descendant(sanitized_path(), sanitized_path(), pos_integer()) ->
    {true, RelPathFromAncestor :: sanitized_path()} | false.
is_equal_or_descendant(Path, Path, _PathLen) ->
    {true, <<>>};
is_equal_or_descendant(PossibleDescendant, ReferencePath, PathLen) ->
    is_descendant(PossibleDescendant, ReferencePath, PathLen).


%% @private
-spec is_descendant(sanitized_path(), sanitized_path(), pos_integer()) ->
    {true, RelPathFromAncestor :: sanitized_path()} | false.
is_descendant(PossibleDescendant, ReferencePath, ReferencePathLen) ->
    case PossibleDescendant of
        <<ReferencePath:ReferencePathLen/binary, ?DIRECTORY_SEPARATOR, RelPath/binary>> ->
            {true, RelPath};
        _ ->
            false
    end.
