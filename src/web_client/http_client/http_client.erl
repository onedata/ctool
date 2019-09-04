%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2015 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This is a HTTP client library, using hackney to perform requests.
%% It is used to unify all HTTP calls across onedata.
%%
%% The API includes:
%%      - some convenience functions (get/post/put/delete)
%%      - the main, general function request/5 that automatically returns the
%%          response body. Response format is {ok, Code, Headers, Body}
%%      - request_return_stream/5 - function that allows streaming the
%%          response body. Response format is {ok, StreamRef}.
%%          See hackney docs for streaming instructions.
%%
%% Possible options: see opts/0 type
%%
%% NOTE: If request_return_stream/5 is used, only the follow_redirect is taken
%%      into consideration for the redirection. If a valid redirection happens,
%%      the following is returned:
%%          {see_other, To, Headers} - for status 303 POST requests
%%          {redirect, To, Headers} - otherwise
%%
%%
%% @end
%% ===================================================================
-module(http_client).
-author("Lukasz Opiola").

-include_lib("hackney/include/hackney_lib.hrl").

% Allowed methods - standard HTTP/1.1 and some more
-type method() :: delete | get | head | post | put | connect | options | trace |
copy | lock | mkcol | move | propfind | proppatch | search | unlock | %% WEBDAV
report | mkactivity | checkout | merge | %% SUBVERSION
msearch | notify | subscribe | unsubscribe | %% UPNP
patch | purge. %% RFC-5789
% Request URL
-type url() :: string() | binary().
% Request / response headers
-type headers() :: #{Key :: binary() => Value :: binary()}.
% Request / response body
-type body() :: string() | binary().
% Response code
-type code() :: integer().
% Request options
-type opts() :: [opt()].
% Response
-type response() ::
    {ok, code(), headers(), body()} |
    {ok, code(), headers()} | % HEAD response
    {error, term()}.

% All possible request options
-type opt() ::
%% to pass tcp options
{connect_options, [gen_tcp:option()]} |
%% to pass ssl options
{ssl_options, [ssl_opt()]} |
%% Specifying maximum body length that can be automatically returned
%% from request. By default, whole body is returned regardless of its size.
%% NOTE: in case of a large body, function request_return_stream/5
%% can be used to stream the body.
{max_body, integer()} |
%% the response messages will be sent to this PID
%% (valid with request_return_stream/5)
{stream_to, pid()} |
%% to set a cookie or a list of cookies.
{cookie, binary() | [binary()]} |
%% false by default, automatically follow redirections
{follow_redirect, boolean()} |
%% 5 by default, the maximum number of redirections for a request
{max_redirect, integer()} |
%% false by default, to force the redirection even on POST
{force_redirect, boolean()} |
%% timeout used when establishing a connection, in milliseconds. Default: 8000.
{connect_timeout, infinity | integer()} |
%% timeout used when receiving a connection. Default: 5000.
{recv_timeout, infinity | integer()} |
%% to connect via a proxy
{proxy, proxy_opt()}.

% Security flags indicating verification type on SSL layer.
%    true -> will cause peer cert and hostname validation
%    only_verify_peercert -> will only verify peer cert and ignore hostname
%    false -> will NOT PERFORM ANY validation
-type secure_flag() :: true | false | only_verify_peercert.

% SSL options that can be passed to http_client
-type ssl_opt() :: {secure, secure_flag()} |
% Indicates to what hostname the client is connecting when in case it is
% different then the one in URL. If specified, server's web certificate will be
% validated against this hostname. Exemplary usage:
%    Opts = [{ssl_options, [{hostname, <<"example.com">>}]}],
%    get(<<"127.0.0.1">>, #{}, <<>>, Opts). <- required for verification
%    get(<<"example.com">>, #{}, <<>>, []). <- ok, hostname taken from the URL
{hostname, binary()} |
{certfile, string()} |
{keyfile, string()} |
{cacerts, [Der :: binary()]}.

% Proxy options (one of them can be used)
-type proxy_opt() ::
%% URL to use for the proxy. Used for basic HTTP proxy
binary() |
%% Host and port to connect, for HTTP proxy
{Host :: binary(), Port :: binary} |
%% Host and Port to connect
{socks5, Host :: binary(), Port :: binary()} |
%% Host and Port to connect to
{connect, Host :: binary(), Port :: binary()}.

% Options passed to hackney
-type hackney_opts() :: [term()].

%% API - convenience functions
-export([get/1, get/2, get/3, get/4]).
-export([post/1, post/2, post/3, post/4]).
-export([put/1, put/2, put/3, put/4]).
-export([patch/1, patch/2, patch/3, patch/4]).
-export([delete/1, delete/2, delete/3, delete/4]).
-export([request/1, request/2, request/3, request/4]).
% Performs the request
-export([request/5]).
% Performs the request, but instead the body return the ref for streaming.
-export([request_return_stream/5]).

-export_type([method/0, url/0, headers/0, body/0, code/0, opts/0, response/0,
    secure_flag/0, opt/0, ssl_opt/0, proxy_opt/0, hackney_opts/0]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP GET request.
%% @end
%%--------------------------------------------------------------------
-spec get(URL :: url()) -> Response :: response().
get(URL) ->
    request(get, URL, #{}, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP GET request.
%% @end
%%--------------------------------------------------------------------
-spec get(URL :: url(), Headers :: headers()) -> Response :: response().
get(URL, Headers) ->
    request(get, URL, Headers, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP GET request.
%% @end
%%--------------------------------------------------------------------
-spec get(URL :: url(), Headers :: headers(), Body :: body()) ->
    Response :: response().
get(URL, Headers, Body) ->
    request(get, URL, Headers, Body, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP GET request.
%% @end
%%--------------------------------------------------------------------
-spec get(URL :: url(), Headers :: headers(), Body :: body(),
    Opts :: opts()) -> Response :: response().
get(URL, Headers, Body, Opts) ->
    request(get, URL, Headers, Body, Opts).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP POST request.
%% @end
%%--------------------------------------------------------------------
-spec post(URL :: url()) -> Response :: response().
post(URL) ->
    request(post, URL, #{}, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP POST request.
%% @end
%%--------------------------------------------------------------------
-spec post(URL :: url(), Headers :: headers()) -> Response :: response().
post(URL, Headers) ->
    request(post, URL, Headers, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP POST request.
%% @end
%%--------------------------------------------------------------------
-spec post(URL :: url(), Headers :: headers(), Body :: body()) ->
    Response :: response().
post(URL, Headers, Body) ->
    request(post, URL, Headers, Body, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP POST request.
%% @end
%%--------------------------------------------------------------------
-spec post(URL :: url(), Headers :: headers(), Body :: body(), Opts :: opts()) ->
    Response :: response().
post(URL, Headers, Body, Opts) ->
    request(post, URL, Headers, Body, Opts).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP PUT request.
%% @end
%%--------------------------------------------------------------------
-spec put(URL :: url()) -> Response :: response().
put(URL) ->
    request(put, URL, #{}, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP PUT request.
%% @end
%%--------------------------------------------------------------------
-spec put(URL :: url(), Headers :: headers()) -> Response :: response().
put(URL, Headers) ->
    request(put, URL, Headers, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP PUT request.
%% @end
%%--------------------------------------------------------------------
-spec put(URL :: url(), Headers :: headers(), Body :: body()) ->
    Response :: response().
put(URL, Headers, Body) ->
    request(put, URL, Headers, Body, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP PUT request.
%% @end
%%--------------------------------------------------------------------
-spec put(URL :: url(), Headers :: headers(), Body :: body(), Opts :: opts()) ->
    Response :: response().
put(URL, Headers, Body, Opts) ->
    request(put, URL, Headers, Body, Opts).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP PATCH request.
%% @end
%%--------------------------------------------------------------------
-spec patch(URL :: url()) -> Response :: response().
patch(URL) ->
    request(patch, URL, #{}, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP PATCH request.
%% @end
%%--------------------------------------------------------------------
-spec patch(URL :: url(), Headers :: headers()) -> Response :: response().
patch(URL, Headers) ->
    request(patch, URL, Headers, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP PATCH request.
%% @end
%%--------------------------------------------------------------------
-spec patch(URL :: url(), Headers :: headers(), Body :: body()) ->
    Response :: response().
patch(URL, Headers, Body) ->
    request(patch, URL, Headers, Body, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP PATCH request.
%% @end
%%--------------------------------------------------------------------
-spec patch(URL :: url(), Headers :: headers(), Body :: body(), Opts :: opts()) ->
    Response :: response().
patch(URL, Headers, Body, Opts) ->
    request(patch, URL, Headers, Body, Opts).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP DELETE request.
%% @end
%%--------------------------------------------------------------------
-spec delete(URL :: url()) -> Response :: response().
delete(URL) ->
    request(delete, URL, #{}, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP DELETE request.
%% @end
%%--------------------------------------------------------------------
-spec delete(URL :: url(), Headers :: headers()) -> Response :: response().
delete(URL, Headers) ->
    request(delete, URL, Headers, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP DELETE request.
%% @end
%%--------------------------------------------------------------------
-spec delete(URL :: url(), Headers :: headers(), Body :: body()) ->
    Response :: response().
delete(URL, Headers, Body) ->
    request(delete, URL, Headers, Body, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP DELETE request.
%% @end
%%--------------------------------------------------------------------
-spec delete(URL :: url(), Headers :: headers(), Body :: body(), Opts :: opts()) ->
    Response :: response().
delete(URL, Headers, Body, Opts) ->
    request(delete, URL, Headers, Body, Opts).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP request.
%% @end
%%--------------------------------------------------------------------
-spec request(URL :: url()) -> Response :: response().
request(URL) ->
    request(get, URL, #{}, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP request.
%% @end
%%--------------------------------------------------------------------
-spec request(Method :: method(), URL :: url()) -> Response :: response().
request(Method, URL) ->
    request(Method, URL, #{}, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP request.
%% @end
%%--------------------------------------------------------------------
-spec request(Method :: method(), URL :: url(), Headers :: headers()) ->
    Response :: response().
request(Method, URL, Headers) ->
    request(Method, URL, Headers, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP request.
%% @end
%%--------------------------------------------------------------------
-spec request(Method :: method(), URL :: url(), Headers :: headers(),
    Body :: body()) -> Response :: response().
request(Method, URL, Headers, Body) ->
    request(Method, URL, Headers, Body, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP request.
%% @end
%%--------------------------------------------------------------------
-spec request(Method :: method(), URL :: url(), Headers :: headers(),
    Body :: body(), Opts :: opts()) -> Response :: response().
request(Method, URL, Headers, Body, Opts) ->
    % If max_body is specified in opts, accept the option, else use 'undefined'
    % which will cause the function to return all the body regardless of
    % its length.
    MaxBody = proplists:get_value(max_body, Opts, undefined),
    % with_body option forces hackney to always return the body
    Opts2 = [with_body | store_option({max_body, MaxBody}, Opts)],
    do_request(Method, URL, Headers, Body, Opts2).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP request and returns a reference that can be used to
%% stream the response body.
%% @end
%%--------------------------------------------------------------------
-spec request_return_stream(Method :: method(), URL :: url(),
    Headers :: headers(), Body :: body(), Opts :: opts()) ->
    {ok, StrmRef :: term()} | {error, term()}.
request_return_stream(Method, URL, Headers, Body, Opts) ->
    do_request(Method, URL, Headers, Body, [async | Opts]).


%% ====================================================================
%% Internal functions
%% ====================================================================

%--------------------------------------------------------------------
%% @private
%% @doc
%% Calls hackney to perform a HTTP request.
%% @end
%%--------------------------------------------------------------------
-spec do_request(Method :: method(), URL :: url(),
    Headers :: headers(), Body :: body(), Opts :: hackney_opts()) ->
    Response :: response() | {ok, StreamRef :: term()}.
do_request(Method, URL, Headers, Body, Opts) ->
    HeadersProplist = maps:to_list(Headers),
    % Do not use connection pools
    Opts2 = [{pool, false} | Opts],
    Opts3 = case should_use_ssl(URL) of
        true ->
            SslOpts = proplists:get_value(ssl_options, Opts, []),
            % resolve SSL opts based on secure flag.
            store_option({ssl_options, secure_ssl_opts:expand(URL, SslOpts)}, Opts2);
        false ->
            Opts2
    end,
    case hackney:request(Method, URL, HeadersProplist, Body, Opts3) of
        {ok, RespCode, RespHeaders, RespBody} ->
            {ok, RespCode, maps:from_list(RespHeaders), RespBody};
        {ok, RespCode, RespHeaders} ->
            {ok, RespCode, maps:from_list(RespHeaders)};
        Other ->
            Other
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that given option is present in options list
%% (adds it or overwrites the old value if present).
%% @end
%%--------------------------------------------------------------------
-spec store_option(Opt :: {atom(), term()}, hackney_opts()) -> hackney_opts().
store_option({Key, _Value} = Opt, Opts) ->
    [Opt | proplists:delete(Key, Opts)].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Decides if SSL should be used for given URL based on protocol.
%% @end
%%--------------------------------------------------------------------
-spec should_use_ssl(url()) -> boolean().
should_use_ssl(Str) when is_list(Str) ->
    should_use_ssl(list_to_binary(Str));
should_use_ssl(<<"https:", _/binary>>) ->
    true;
should_use_ssl(_) ->
    false.
