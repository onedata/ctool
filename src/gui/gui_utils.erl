%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains miscellaneous functions used commonly
%%% in GUI modules.
%%% @end
%%%-------------------------------------------------------------------

-module(gui_utils).
-include_lib("public_key/include/public_key.hrl").
-include("gui/common.hrl").
-include("logging.hrl").

% Initialization of n2o settings and cleanup
-export([init_n2o_ets_and_envs/4, cleanup_n2o/1]).

% Convenience functions to manipulate response headers
-export([cowboy_ensure_header/3, onrequest_adjust_headers/1]).

% Cookies policy handling
-export([cookie_policy_popup_body/1, is_cookie_policy_accepted/1]).

%% Name of cookie remembering if cookie policy is accepted (value is T/F)
-define(cookie_policy_cookie_name, "cookie_policy_accepted").
%% Maximum redirects to follow when doing http request
-define(max_redirects, 5).
%% Maximum depth of CA cert analize
-define(ca_cert_max_depth, 11).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Initializes all environment settings required by n2o and creates
%% required ets tables. Should be called before starting a cowboy listener for
%% n2o GUI.
%% @end
%%--------------------------------------------------------------------
-spec init_n2o_ets_and_envs(GuiPort :: integer(), RoutingModule :: module(),
    SessionLogicModule :: module(), BridgeModule :: module()) -> ok.
init_n2o_ets_and_envs(GuiPort, RoutingModule, SessionLogicModule, BridgeModule) ->
    % Transition port - the same as gui port
    ok = application:set_env(n2o, port, GuiPort),
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


%%--------------------------------------------------------------------
%% @doc Cleans up n2o setup, such as ets tables.
%% Should be called after stopping a cowboy listener for
%% n2o GUI.
%% @end
%%--------------------------------------------------------------------
-spec cleanup_n2o(SessionLogicModule :: module()) -> ok.
cleanup_n2o(SessionLogicModule) ->
    SessionLogicModule:cleanup(),
    ets:delete(actions),
    ets:delete(globals),
    ets:delete(caching),
    ok.


%%--------------------------------------------------------------------
%% @doc Sets a response header, but prevents duplicate entries. Header must
%% be normalized to lowercase (e. g. content-type and not Content-Type)
%% @end
%%--------------------------------------------------------------------
-spec cowboy_ensure_header(Name :: binary(), Value :: binary(), Req :: cowboy_req:req()) -> cowboy_req:req().
cowboy_ensure_header(Name, Value, Req) when is_binary(Name) and is_binary(Value) ->
    Req2 = cowboy_req:delete_resp_header(Name, Req),
    cowboy_req:set_resp_header(Name, Value, Req2).


%%--------------------------------------------------------------------
%% @doc Callback hook for cowboy to modify response headers for HTTPS GUI.
%% Those headers improve security of https connection.
%% @end
%%--------------------------------------------------------------------
-spec onrequest_adjust_headers(Req :: cowboy_req:req()) -> cowboy_req:req().
onrequest_adjust_headers(Req) ->
    Req2 = cowboy_req:set_resp_header(<<"Strict-Transport-Security">>, <<"max-age=31536000; includeSubDomains">>, Req),
    cowboy_req:set_resp_header(<<"X-Frame-Options">>, <<"SAMEORIGIN">>, Req2).


%%--------------------------------------------------------------------
%% @doc Returns a set of elements that renders to a floating popup asking for acceptance
%% of privacy policy, or an empty list if the privacy policy has already been accepted.
%% @end
%%--------------------------------------------------------------------
-spec cookie_policy_popup_body(PrivacyPolicyURL :: binary()) -> [] | term().
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


%%--------------------------------------------------------------------
%% @doc Returns true if the client browser has sent a proper cookie
%% implying that the privacy policy has been accepted.
%% @end
%%--------------------------------------------------------------------
-spec is_cookie_policy_accepted(Req :: cowboy_req:req()) -> term().
is_cookie_policy_accepted(Req) ->
    case gui_ctx:cookie(<<?cookie_policy_cookie_name>>, Req) of
        <<"true">> -> true;
        _ -> false
    end.

