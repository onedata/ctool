%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2016 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Convenience functions concerning URL manipulation.
%% @end
%% ===================================================================
-module(url_utils).
-author("Lukasz Opiola").

-include_lib("hackney/include/hackney_lib.hrl").

%% API
-export([parse/1, is_valid/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Parses given URI and returns identified parts.
%% @end
%%--------------------------------------------------------------------
-spec parse(Uri :: http_client:url()) ->
    #{
        scheme => http | https,
        host => binary(),
        port => non_neg_integer(),
        path => binary(),
        qs => binary()
    }.
parse(Uri) ->
    #hackney_url{
        scheme = Scheme, host = Host, port = Port, path = Path, qs = QueryString
    } = hackney_url:parse_url(Uri),
    #{
        scheme => Scheme,
        host => list_to_binary(Host),
        port => Port,
        path => Path,
        qs => QueryString
    }.


-spec is_valid(http_client:url()) -> boolean().
is_valid(Uri) ->
    try
       parse(Uri),
       true
    catch
        _:_ ->
            false
    end.