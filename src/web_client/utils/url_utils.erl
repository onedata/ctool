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

-include("errors.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

%% API
-export([
    parse/1,
    infer_components/1
]).

-define(DEFAULT_HTTP_PORT, 80).
-define(DEFAULT_HTTPS_PORT, 443).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Parses given URI and returns identified parts.
%% @end
%%--------------------------------------------------------------------
-spec parse(Uri :: http_client:url()) -> #{scheme => http | https,
host => binary(), port => non_neg_integer(), path => binary(), qs => binary()}.
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


%%--------------------------------------------------------------------
%% @doc
%% Parses given URI and returns identified parts.
%% Function infers missing schema or port in incomplete urls if possible.
%% @end
%%--------------------------------------------------------------------
-spec infer_components(http_client:url()) -> #{scheme => http | https,
host => binary(), port => non_neg_integer(), path => binary(), qs => binary()}.
infer_components(URL) ->
    try
        infer_components_insecure(URL)
    catch
        _:_ -> throw(?ERROR_MALFORMED_DATA)
    end.


%%%===================================================================
%%% Helpers
%%%===================================================================


%% @private
-spec infer_components_insecure(http_client:url()) -> #{scheme => http | https,
host => binary(), port => non_neg_integer(), path => binary(), qs => binary()}.
infer_components_insecure(URL) ->
    URLTrimmed = list_to_binary(string:trim(binary_to_list(URL))),
    #{
        host := ParsedHost, path := ParsedPath, qs := ParsedQueryString
    } = parse(URLTrimmed),
    URLScheme = get_scheme_from_url(URLTrimmed),
    URLPort = get_port_from_url(URLTrimmed, ParsedHost),

    {FinalScheme, FinalPort} = case {URLScheme, URLPort} of
        {undefined, undefined} ->
            throw(?ERROR_MALFORMED_DATA);
        {undefined, ?DEFAULT_HTTPS_PORT} ->
            {https, ?DEFAULT_HTTPS_PORT};
        {undefined, ?DEFAULT_HTTP_PORT} ->
            {http, ?DEFAULT_HTTP_PORT};
        %% TODO: VFS-7682 - following match should throw an error
        {undefined, CustomPort} ->
            {http, CustomPort};
        {http, CustomPort} ->
            {http, utils:ensure_defined(CustomPort, ?DEFAULT_HTTP_PORT)};
        {https, CustomPort} ->
            {https, utils:ensure_defined(CustomPort, ?DEFAULT_HTTPS_PORT)}
    end,

    #{
        scheme => FinalScheme,
        host => ParsedHost,
        port => FinalPort,
        path => ParsedPath,
        qs => ParsedQueryString
    }.


%% @private
-spec get_scheme_from_url(http_client:url()) -> http | https | undefined.
get_scheme_from_url(URL) ->
    case hd(binary:split(URL, [<<"://">>])) of
        <<"https">> -> https;
        <<"http">> -> http;
        _ -> undefined
    end.


%% @private
-spec get_port_from_url(http_client:url(), binary()) -> integer() | undefined.
get_port_from_url(URL, Hostname) ->
    case binary:split(URL, Hostname) of
        [_, <<":", PortAndPathPart/binary>>] ->
            binary_to_integer(hd(string:split(PortAndPathPart, "/")));
        _ ->
            undefined
    end.
