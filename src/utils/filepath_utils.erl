%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 ACK CYFRONET AGH
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
-export([ensure_ends_with_slash/1, ensure_begins_with_slash/1,
    ensure_begins_with_prefix/2, parent_dir/1]).

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

%%%===================================================================
%%% Internal functions
%%%===================================================================