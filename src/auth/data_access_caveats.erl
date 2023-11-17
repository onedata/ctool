%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module provides a high-level API for handling data access caveats.
%%% Data access caveats are treated in a special way in all system components.
%%% Existence of any data access caveat in a token determines that the token
%%% is intended exclusively for accessing user data and causes all other APIs
%%% to be disallowed or limited to the minimum required to handle data access
%%% requests.
%%%
%%% The list of data access caveats:
%%%     * cv_interface, but only if (interface = oneclient)
%%%     * cv_data_readonly
%%%     * cv_data_path
%%%     * cv_data_objectid
%%% @end
%%%--------------------------------------------------------------------
-module(data_access_caveats).
-author("Lukasz Opiola").

-include("aai/aai.hrl").
-include("graph_sync/gri.hrl").

% Decides if data access caveats are allowed in the context of authorization.
-type policy() :: disallow_data_access_caveats | allow_data_access_caveats.
-export_type([policy/0]).

% Canonical path has the following requirements:
%   - starts with a slash
%   - first element is a space id
%   - does not have a trailing slash
% examples:
%   /74ab36db1f4d3b4da4d2eb7b64d60dfc
%   /e0a52fd71154069c8c2418aac1724e01/dir1
%   /e0a52fd71154069c8c2418aac1724e01/dir1/file.txt
%   /  <- invalid (no space id)
%   /e0a52fd71154069c8c2418aac1724e01/dir1/  <- invalid (trailing slash)
-type canonical_path() :: binary().
-type objectid() :: file_id:objectid().
-type allowed_spaces() :: nonrestricted | [file_id:space_id()].
-export_type([canonical_path/0, objectid/0, allowed_spaces/0]).

-type cv_data_path() :: #cv_data_path{}.
-type cv_data_objectid() :: #cv_data_objectid{}.
-type data_access_caveat() :: #cv_interface{} | #cv_data_readonly{} | #cv_data_path{} | #cv_data_objectid{}.

-define(DATA_ACCESS_CAVEATS, [
    cv_interface, cv_data_readonly, cv_data_path, cv_data_objectid
]).

%% API
-export([sanitize_path_caveat/1]).
-export([sanitize_objectid_caveat/1]).
-export([filter/1]).
-export([to_allowed_api/2]).
-export([match_available_spaces/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec sanitize_path_caveat(cv_data_path()) -> boolean().
sanitize_path_caveat(#cv_data_path{whitelist = Whitelist}) ->
    try
        true = length(Whitelist) > 0,
        lists:foreach(fun(Path) ->
            % see canonical_path()
            true = $/ == binary:first(Path),
            true = length(filename:split(Path)) > 1,
            false = $/ == binary:last(Path)
        end, Whitelist),
        true
    catch _:_ ->
        false
    end.


-spec sanitize_objectid_caveat(cv_data_objectid()) -> boolean().
sanitize_objectid_caveat(#cv_data_objectid{whitelist = Whitelist}) ->
    try
        true = length(Whitelist) > 0,
        lists:foreach(fun(Objectid) ->
            {ok, Guid} = file_id:objectid_to_guid(Objectid),
            <<_/binary>> = file_id:guid_to_space_id(Guid)
        end, Whitelist),
        true
    catch _:_ ->
        false
    end.


-spec filter([caveats:caveat()]) -> [data_access_caveat()].
filter(Caveats) ->
    Filtered = caveats:filter(?DATA_ACCESS_CAVEATS, Caveats),
    lists:filtermap(fun
        (#cv_interface{interface = oneclient} = Cv) -> {true, Cv};
        (#cv_interface{interface = _}) -> false;
        (Cv) -> {true, Cv}
    end, Filtered).


%%--------------------------------------------------------------------
%% @doc
%% Returns the allowed API in given service for given data access caveat.
%% OZ-WORKER API is limited to fetching user's data and his spaces' data.
%% OP-WORKER API is limited to operations on files.
%% (OZ|OP)-PANEL API is completely disallowed.
%% @end
%%--------------------------------------------------------------------
-spec to_allowed_api(onedata:service(), data_access_caveat()) -> cv_api:cv_api().
to_allowed_api(?OZ_PANEL, _) ->
    #cv_api{whitelist = []};

to_allowed_api(?OP_PANEL, _) ->
    #cv_api{whitelist = []};

to_allowed_api(?OP_WORKER, _) ->
    #cv_api{whitelist = [
        {?OP_WORKER, all, ?GRI_PATTERN(op_file, <<"*">>, <<"*">>, '*')}
    ]};

to_allowed_api(?OZ_WORKER, DataAccessCaveat) ->
    AllowedSpaces = infer_available_spaces(DataAccessCaveat),
    #cv_api{whitelist = lists:flatten([
        {?OZ_WORKER, get, ?GRI_PATTERN(od_user, <<"*">>, <<"instance">>, '*')},
        {?OZ_WORKER, get, ?GRI_PATTERN(od_share, <<"*">>, <<"instance">>, '*')},
        [{?OZ_WORKER, get, ?GRI_PATTERN(od_space, S, <<"instance">>, '*')} || S <- AllowedSpaces]
    ])}.


-spec match_available_spaces([data_access_caveat()], [file_id:space_id()]) -> [file_id:space_id()].
match_available_spaces(DataAccessCaveats, AllSpaceIds) ->
    lists:foldl(fun(DataAccessCaveat, Acc) ->
        case infer_available_spaces(DataAccessCaveat) of
            [<<"*">>] -> Acc;
            Whitelist -> lists_utils:intersect(Acc, Whitelist)
        end
    end, AllSpaceIds, DataAccessCaveats).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the list of spaces that are available in regard to given data access caveat.
%% A one-item list with a wildcard character indicates no limit on available spaces.
%% @end
%%--------------------------------------------------------------------
-spec infer_available_spaces(data_access_caveat()) -> [file_id:space_id()].
infer_available_spaces(#cv_interface{interface = oneclient}) ->
    % Oneclient interface caveat does not confine access to specific spaces
    [<<"*">>];
infer_available_spaces(#cv_data_readonly{}) ->
    % Data readonly caveat does not confine access to specific spaces
    [<<"*">>];
infer_available_spaces(#cv_data_path{whitelist = PathsWhitelist}) ->
    % Data path caveat includes a list of canonical paths that precisely
    % narrow down the allowed spaces
    lists:filtermap(fun(Path) ->
        case filename:split(Path) of
            [<<"/">>, SpaceId | _] -> {true, SpaceId};
            _ -> false  % Invalid paths are ignored
        end
    end, PathsWhitelist);
infer_available_spaces(#cv_data_objectid{whitelist = ObjectIdsWhitelist}) ->
    % Data objectid caveat includes a list of file ids that precisely
    % narrow down the allowed spaces
    lists:filtermap(fun(Objectid) ->
        try
            {ok, Guid} = file_id:objectid_to_guid(Objectid),
            {true, <<_/binary>> = file_id:guid_to_space_id(Guid)}
        catch _:_ ->
            false  % Invalid ObjectIds are ignored
        end
    end, ObjectIdsWhitelist).
