%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains miscellaneous functions used commonly
%% in GUI modules.
%% @end
%% ===================================================================

-module(gui_utils).
-include_lib("public_key/include/public_key.hrl").
-include("gui/common.hrl").

% Initialization of n2o settings and cleanup
-export([init_n2o_ets_and_envs/4, cleanup_n2o/1]).

% Convenience functions to manipulate response headers
-export([cowboy_ensure_header/3, onrequest_adjust_headers/1]).

% Cookies policy handling
-export([cookie_policy_popup_body/1, is_cookie_policy_accepted/1]).

% Functions used to perform secure server-server http requests
-export([https_get/2, https_post/3, toggle_server_cert_verification/1]).

% Misscellaneous convenience functions
-export([proplist_to_url_params/1, fully_qualified_url/1, validate_email/1, normalize_email/1]).

%% Name of cookie remembering if cookie policy is accepted (value is T/F)
-define(cookie_policy_cookie_name, "cookie_policy_accepted").
%% Maximum redirects to follow when doing http request
-define(max_redirects, 5).
%% Maximum depth of CA cert analize
-define(ca_cert_max_depth, 11).

-define(mail_validation_regexp,
    <<"^[a-z0-9!#$%&'*+\\/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+\\/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?$">>).


%% ====================================================================
%% API functions
%% ====================================================================

%% init_n2o_ets_and_envs/4
%% @doc Initializes all environment settings required by n2o and creates
%% required ets tables. Should be called before starting a cowboy listener for
%% n2o GUI.
%% @end
-spec init_n2o_ets_and_envs(GuiPort :: integer(), RoutingModule :: atom(), SessionLogicModule :: atom(), BridgeModule :: atom()) -> ok.
%% ====================================================================
init_n2o_ets_and_envs(GuiPort, RoutingModule, SessionLogicModule, BridgeModule) ->
    % Transition port - the same as gui port
    ok = application:set_env(n2o, transition_port, GuiPort),
    % Custom route handler
    ok = application:set_env(n2o, route, RoutingModule),
    % Custom session handler for n2o
    ok = application:set_env(n2o, session, gui_session_handler),
    % Custom cowboy bridge for n2o
    ok = application:set_env(n2o, bridge, BridgeModule),
    % Custom session logic handler for gui_session_handler
    ok = application:set_env(ctool, session_logic_module, SessionLogicModule),

    SessionLogicModule:init(),

    % Ets tables needed by n2o
    ets:new(actions, [set, named_table, {keypos, 1}, public]),
    ets:new(globals, [set, named_table, {keypos, 1}, public]),
    ets:new(caching, [set, named_table, {keypos, 1}, public]),
    ets:insert(globals, {onlineusers, 0}),
    ok.


%% cleanup_n2o/1
%% @doc Cleans up n2o setup, such as ets tables.
%% Should be called after stopping a cowboy listener for
%% n2o GUI.
%% @end
-spec cleanup_n2o(SessionLogicModule :: atom()) -> ok.
%% ====================================================================
cleanup_n2o(SessionLogicModule) ->
    SessionLogicModule:cleanup(),
    ets:delete(actions),
    ets:delete(globals),
    ets:delete(caching),
    ok.


%% cowboy_ensure_header/3
%% ====================================================================
%% @doc Sets a response header, but prevents duplicate entries. Header must
%% be normalized to lowercase (e. g. content-type and not Content-Type)
%% @end
-spec cowboy_ensure_header(Name :: binary(), Value :: binary(), Req :: req()) -> req().
%% ====================================================================
cowboy_ensure_header(Name, Value, Req) when is_binary(Name) and is_binary(Value) ->
    Req2 = cowboy_req:delete_resp_header(Name, Req),
    cowboy_req:set_resp_header(Name, Value, Req2).


%% onrequest_adjust_headers/1
%% ====================================================================
%% @doc Callback hook for cowboy to modify response headers for HTTPS GUI.
%% Those headers improve security of https connection.
%% @end
-spec onrequest_adjust_headers(Req :: req()) -> req().
%% ====================================================================
onrequest_adjust_headers(Req) ->
    Req2 = cowboy_req:set_resp_header(<<"Strict-Transport-Security">>, <<"max-age=31536000; includeSubDomains">>, Req),
    cowboy_req:set_resp_header(<<"X-Frame-Options">>, <<"SAMEORIGIN">>, Req2).


%% cookie_policy_popup_body/1
%% ====================================================================
%% @doc Returns a set of elements that renders to a floating popup asking for acceptance
%% of privacy policy, or an empty list if the privacy policy has already been accepted.
%% @end
-spec cookie_policy_popup_body(PrivacyPolicyURL :: binary()) -> [] | term().
%% ====================================================================
cookie_policy_popup_body(PrivacyPolicyURL) ->
    case is_cookie_policy_accepted(?REQ) of
        true ->
            [];
        false ->
            [
                #panel{id = <<"cookie_policy_popup">>, class = <<"dialog dialog-info wide">>,
                    style = <<"position: fixed; bottom: 0; height: 60px; z-index: 2000;",
                    "line-height: 60px; text-align: center; margin: 0; padding: 0; width: 100%;">>,
                    body = [
                        #p{style = <<"margin: 0 10px; display: inline;">>,
                            body = <<"This website uses cookies. By continuing to browse the site, you are agreeing to our use of cookies.">>},
                        #form{style = "display: inline;", class = <<"control-group">>, body = [
                            #link{class = <<"btn btn-mini btn-info">>, target = <<"_blank">>, url = PrivacyPolicyURL,
                                style = <<"margin: 14px 10px; width: 65px;">>, body = <<"Learn more">>},
                            #link{class = <<"btn btn-mini btn-success">>, id = <<"accept_cookie_policy">>, body = <<"OK">>,
                                style = <<"margin: 14px 10px; width: 65px;">>, actions = gui_jq:bind_element_click(<<"accept_cookie_policy">>,
                                    <<"function (e){ document.cookie = '", ?cookie_policy_cookie_name, "=true;expires=Fri, 01 Jan 2100 00:00:00 GMT';",
                                    "$('#cookie_policy_popup').hide(); }">>)}
                        ]}
                    ]}
            ]
    end.


%% is_cookie_policy_accepted/1
%% ====================================================================
%% @doc Returns true if the client browser has sent a proper cookie
%% implying that the privacy policy has been accepted.
%% @end
-spec is_cookie_policy_accepted(Req :: req()) -> term().
%% ====================================================================
is_cookie_policy_accepted(Req) ->
    {Cookie, _} = cowboy_req:cookie(<<?cookie_policy_cookie_name>>, Req),
    case Cookie of
        <<"true">> -> true;
        _ -> false
    end.


%% https_get/2
%% ====================================================================
%% @doc Performs a HTTPS GET. Host is verified according to locally installed CA certs
%% (path is provided in environment variable). Only if connection is secure,
%% the request is performed.
%% @end
-spec https_get(URLBin :: binary() | string(), ReqHeadersBin :: [{binary() | string(), binary() | string()}]) ->
    {ok, binary()} | {error, unknown_cert} | {error, term()}.
%% ====================================================================
https_get(URLBin, ReqHeadersBin) ->
    URL = gui_str:to_list(URLBin),
    ReqHeaders = lists:map(
        fun({Key, Value}) ->
            {gui_str:to_list(Key), gui_str:to_list(Value)}
        end, ReqHeadersBin),
    perform_request(URL, ReqHeaders, get, "", ?max_redirects).


%% https_post/3
%% ====================================================================
%% @doc Performs a HTTPS POST. Host is verified according to locally installed CA certs
%% (path is provided in environment variable). Only if connection is secure,
%% the request is performed.
%% @end
-spec https_post(URLBin :: binary() | string(), ReqHeadersBin :: [{binary() | string(), binary() | string()}], Body :: binary() | string()) ->
    {ok, binary()} | {error, unknown_cert} | {error, term()}.
%% ====================================================================
https_post(URLBin, ReqHeadersBin, Body) ->
    URL = gui_str:to_list(URLBin),
    ReqHeaders = lists:map(
        fun({Key, Value}) ->
            {gui_str:to_list(Key), gui_str:to_list(Value)}
        end, ReqHeadersBin),
    %% 0 max redirects, according to RFC post requests should not be redirected
    perform_request(URL, ReqHeaders, post, gui_str:to_list(Body), 0).


%% toggle_server_cert_verification/1
%% ====================================================================
%% @doc This function allows toggling server cert verification in runtime.
%% @end
-spec toggle_server_cert_verification(boolean()) -> boolean().
%% ====================================================================
toggle_server_cert_verification(Flag) ->
    application:set_env(ctool, verify_server_cert, Flag).


%% proplist_to_params/1
%% ====================================================================
%% @doc Converts a proplist to a single x-www-urlencoded binary. Adding a third
%% field 'no_encode' to a tuple will prevent URL encoding.
%% @end
%% ====================================================================
-spec proplist_to_url_params([{binary(), binary()} | {binary(), binary(), no_encode}]) -> binary().
%% ====================================================================
proplist_to_url_params(List) ->
    lists:foldl(
        fun(Tuple, Acc) ->
            {KeyEncoded, ValueEncoded} = case Tuple of
                                             {Key, Value, no_encode} ->
                                                 {Key, Value};
                                             {Key, Value} ->
                                                 {gui_str:url_encode(Key), gui_str:url_encode(Value)}
                                         end,
            Suffix = case Acc of
                         <<"">> -> <<"">>;
                         _ -> <<Acc/binary, "&">>
                     end,
            <<Suffix/binary, KeyEncoded/binary, "=", ValueEncoded/binary>>
        end, <<"">>, List).


%% fully_qualified_url/1
%% ====================================================================
%% @doc Converts the given URL to a fully quialified url, without leading www.
%% @end
%% ====================================================================
-spec fully_qualified_url(binary()) -> binary().
%% ====================================================================
fully_qualified_url(Binary) ->
    case Binary of
        <<"https://www.", Rest/binary>> -> <<"https://", Rest/binary>>;
        <<"https://", _/binary>> -> Binary;
        <<"www.", Rest/binary>> -> <<"https://", Rest/binary>>;
        _ -> <<"https://", Binary/binary>>
    end.


%% validate_email/1
%% ====================================================================
%% @doc Returns true if the given string is a valid email address according to RFC.
%% @end
%% ====================================================================
-spec validate_email(binary()) -> boolean().
%% ====================================================================
validate_email(Email) ->
    case re:run(Email, ?mail_validation_regexp) of
        {match, _} -> true;
        _ -> false
    end.


%% normalize_email/1
%% ====================================================================
%% @doc Performs gmail email normalization by removing all the dots in the local part.
%% @end
%% ====================================================================
-spec normalize_email(binary()) -> binary().
%% ====================================================================
normalize_email(Email) ->
    case binary:split(Email, [<<"@">>], [global]) of
        [Account, Domain] ->
            case Domain of
                <<"gmail.com">> -> <<(binary:replace(Account, <<".">>, <<"">>, [global]))/binary, "@", Domain/binary>>;
                _ -> Email
            end;
        _ ->
            Email
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% perform_request/4
%% ====================================================================
%% @doc Performs a HTTPS request with given args.
%% @end
-spec perform_request(URL :: string(), ReqHeaders :: [{string(), string()}], Method :: atom(), Body :: binary(), Redirects :: integer()) ->
    {ok, binary()} | {error, unknown_cert} | {error, term()}.
%% ====================================================================
perform_request(URL, ReqHeaders, Method, Body, Redirects) ->
    try
        Options = case application:get_env(ctool, verify_server_cert) of
                      {ok, true} ->
                          [{response_format, binary}, {ssl_options, ssl_opts(Domain)}];
                      _ ->
                          ?debug("Performing a HTTPS connection without server cert verification [~p]", [URL]),
                          [{response_format, binary}]
                  end,
        {ok, {_, _, Domain, _, _, _}} = http_uri:parse(URL),
        case ibrowse:send_req(URL, ReqHeaders, Method, Body, Options) of
            {ok, Rcode, RespHeaders, ResponseBody}
                when (Rcode =:= "301" orelse Rcode =:= "302" orelse Rcode =:= "303" orelse Rcode =:= "307") andalso Redirects > 0 ->
                % Code in {301, 302, 303, 307} - we are being redirected
                case get_redirect_url(URL, RespHeaders) of
                    undefined -> ResponseBody;
                    URL -> ResponseBody;
                    NewURL -> perform_request(NewURL, ReqHeaders, Method, Body, Redirects - 1)
                end;

            {ok, "200", _, ResponseBody} ->
                % Answer ok
                {ok, ResponseBody};

            {error, {conn_failed, {error, {tls_alert, "certificate unknown"}}}} ->
                % Host authenticity cannot be confirmed
                {error, unknown_cert};

            {error, Other} ->
                {error, Other};

            Other ->
                {error, Other}
        end
    catch
        _:M ->
            {error, M}
    end.


%% get_redirect_url/1
%% ====================================================================
%% @doc
%% Retrieves redirect URL from a HTTP response.
%% @end
-spec get_redirect_url(OldURL :: string(), Headers :: list()) -> string().
%% ====================================================================
get_redirect_url(OldURL, Headers) ->
    Location = proplists:get_value("location", Headers, proplists:get_value("Location", Headers)),
    case Location of
        "http://" ++ _ -> Location;
        "https://" ++ _ -> Location;
        [$/ | _] = Location ->
            {url, _, Host, Port, _, _, _, Protocol, _} = ibrowse_lib:parse_url(OldURL),
            PortFrag = case {Protocol, Port} of
                           {http, 80} -> "";
                           {https, 443} -> "";
                           _ -> ":" ++ integer_to_list(Port)
                       end,
            atom_to_list(Protocol) ++ "://" ++ Host ++ PortFrag ++ Location;
        _ -> undefined
    end.


%% ssl_opts/1
%% ====================================================================
%% @doc Returns list of ssl opts for secure connection.
%% @end
-spec ssl_opts(ReqHostname :: string()) -> [tuple()].
%% ====================================================================
ssl_opts(ReqHostname) ->
    VerifyFun =
        fun(_, {bad_cert, _}, RequestedHostname) ->
            {unknown, RequestedHostname};

            (_, {extension, _}, RequestedHostname) ->
                {unknown, RequestedHostname};

            (_, valid, RequestedHostname) ->
                {valid, RequestedHostname};

            (Cert, valid_peer, RequestedHostname) ->
                % If peer is valid, make sure one of domain names contained in cert matches our requested adress
                #'OTPCertificate'{tbsCertificate = #'OTPTBSCertificate'{extensions = Extensions}} = Cert,
                AllowedHostnames = lists:foldl(
                    fun(#'Extension'{extnID = ExtID, extnValue = ExtVal}, Acc) ->
                        case ExtID of
                            ?'id-ce-subjectAltName' ->
                                Acc ++ lists:map(
                                    fun({dNSName, DNSName}) ->
                                        % Create regexps from allowed domain names, to later match them against requested address
                                        ReplacedDots = re:replace(DNSName, "\\.", "\\\\.", [global, {return, list}]),
                                        _ReplacedWildcards = re:replace(ReplacedDots, "\\*", ".*", [global, {return, list}])
                                    end, ExtVal);
                            _ ->
                                Acc
                        end
                    end, [], Extensions),

                Valid = lists:foldl(
                    fun(RegExp, Acc) ->
                        case re:run(RequestedHostname, RegExp) of
                        % At least one domain name matched, the peer is indeed valid
                            {match, _} -> valid;
                            _ -> Acc
                        end
                    end, unknown, AllowedHostnames),
                {Valid, RequestedHostname}
        end,

    CaCertFileAtom = case application:get_env(ctool, root_cacert_file) of
                         {ok, Val} -> Val;
                         _ -> throw("root_cacert_file env missing")
                     end,
    % Return ssl opts for a secure connection
    [
        {verify, verify_peer},
        {cacertfile, atom_to_list(CaCertFileAtom)},
        {verify_fun, {VerifyFun, ReqHostname}},
        {depth, ?ca_cert_max_depth}
    ].
