%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains page manipulation and asynchronous updates
%% functions, mostly based on jquery.
%% IMPORTANT: n2o's wf module must not be used directly!
%% These functions are a wrapper to that module, which gives control over
%% such aspects as javascript escaping and event wiring sequence.
%% @end
%% ===================================================================

-module(gui_jq).
-include("gui/common.hrl").
-include("logging.hrl").

% General javascript wiring
-export([wire/1, wire/2, wire/4]).

% Wiring postbacks and form submissions
-export([postback_action/2, form_submit_action/3]).

% Redirections
-export([redirect/1, redirect_to_login/1, redirect_from_login/0]).

% Useful functions for binding custom events
-export([register_escape_event/1, bind_enter_to_submit_button/2, bind_element_click/2]).

% DOM updates
-export([update/2, replace/2, insert_top/2, insert_bottom/2, insert_before/2, insert_after/2, remove/1]).

% Commonly used jquery functions
-export([show/1, hide/1, add_class/2, remove_class/2, slide_up/2, slide_down/2, fade_in/2]).
-export([focus/1, select_text/1, set_value/2]).


%% ====================================================================
%% API functions
%% ====================================================================

%% wire/1
%% ====================================================================
%% @doc Sends a javascript code snippet or action record (which will be
%% rendered to js) to the client for immediate evaluation.
%% NOTE! Does not js_escape the script, the developer has to make sure 
%% the wired javascript is safe, or use DOM manipulation functions, which 
%% include safe escaping.
%% @end
-spec wire(Action :: term()) -> ok.
%% ====================================================================
wire(Action) ->
    wire(Action, false).


%% wire/2
%% ====================================================================
%% @doc Sends a javascript code snippet or action record (which will be
%% rendered to js) to the client for immediate evaluation.
%% NOTE! Does not js_escape the script, the developer has to make sure
%% the wired javascript is safe, or use DOM manipulation functions, which
%% include safe escaping.
%% Eager flag can be used. It is ensured that all eager actions
%% will be evaluated before normal actions.
%% @end
-spec wire(Script :: string() | binary(), Eager :: boolean()) -> ok.
%% ====================================================================
wire(Script, Eager) when is_binary(Script) ->
    wire(gui_str:to_list(Script), Eager);

wire(Action, Eager) ->
    Actions = case get(actions) of undefined -> []; E -> E end,
    case Eager of
        true ->
            put(actions, [#wire{actions = Action} | Actions]);
        false ->
            put(actions, Actions ++ [#wire{actions = Action}])
    end.


%% wire/4
%% ====================================================================
%% @doc Convienience function to render javascript code.
%% Eager flag can be used.
%% @end
-spec wire(Target :: binary(), Method :: binary(), Args :: binary(), Eager :: boolean()) -> ok.
%% ====================================================================
wire(Target, Method, Args, Eager) ->
    RenderedArgs = case Args of
                       <<"">> -> <<"">>;
                       <<"''">> -> <<"''">>;
                       _ -> <<"'", Args/binary, "'">>
                   end,
    Script = <<"$('#", Target/binary, "').", Method/binary, "(", RenderedArgs/binary, ");">>,
    wire(Script, Eager).


%% postback_action/2
%% ====================================================================
%% @doc Returns action records that can be assigned to actions field of an element.
%% It will cause form submission with given postback, and values of field(s)
%% given in Sources arg will be available by gui_ctx:form_param function.
%% @end
-spec postback_action(TriggerID :: binary(), Postback :: term()) -> ok.
%% ====================================================================
postback_action(TriggerID, Postback) ->
    #event{type = "click", postback = Postback, target = gui_str:to_list(TriggerID)}.

%% form_submit_action/3
%% ====================================================================
%% @doc Returns action records that can be assigned to actions field of an element.
%% It will cause form submission with given postback, and values of field(s)
%% given in Sources arg will be available by gui_ctx:form_param function.
%% @end
-spec form_submit_action(TriggerID :: binary(), Postback :: term(), Sources :: binary() | [binary()] ) -> ok.
%% ====================================================================
form_submit_action(TriggerID, Postback, SourcesArg) ->
    Sources = lists:map(fun(Source) -> gui_str:to_list(Source) end, lists:flatten([SourcesArg])),
    #event{type = "click", postback = Postback, target = gui_str:to_list(TriggerID), source = Sources}.


%% redirect/1
%% ====================================================================
%% @doc Redirects to given page.
%% @end
-spec redirect(URL :: binary()) -> ok.
%% ====================================================================
redirect(URL) ->
    wf:redirect(URL).


%% redirect_to_login/1
%% ====================================================================
%% @doc Redirects to login page. Can remember the source page, so that
%% a user can be redirected back after logging in.
%% @end
-spec redirect_to_login(SaveSourcePage :: boolean()) -> ok.
%% ====================================================================
redirect_to_login(SaveSourcePage) ->
    PageName = gui_ctx:get_requested_page(),
    case SaveSourcePage andalso PageName /= <<"/">> of
        false -> wf:redirect(<<"/login">>);
        true -> wf:redirect(<<"/login?x=", PageName/binary>>)
    end.


%% redirect_from_login/0
%% ====================================================================
%% @doc Redirects back from login page to the originally requested page.
%% If it hasn't been stored before, redirects to index page.
%% @end
-spec redirect_from_login() -> ok.
%% ====================================================================
redirect_from_login() ->
    case wf:q(<<"x">>) of
        undefined -> wf:redirect(<<"/">>);
        TargetPage -> wf:redirect(TargetPage)
    end.


%% register_escape_event/1
%% ====================================================================
%% @doc Binds escape button so that it generates an event every time it's pressed.
%% The event will call the function api_event(Tag, [], Context) on page module.
%% Tag has to be a string as n2o engine requires so.
%% @end
-spec register_escape_event(Tag :: string()) -> ok.
%% ====================================================================
register_escape_event(Tag) ->
    wire(#api{name = "escape_pressed", tag = Tag}, false),
    wire(<<"$(document).bind('keydown', function (e){if (e.which == 27) escape_pressed();});">>, false).


%% bind_enter_to_submit_button/2
%% ====================================================================
%% @doc Makes any enter keypresses on InputID (whenever it is focused)
%% perform a click on a selected ButtonToClickID. This way, it allows
%% easy form submission with enter key.
%% @end
-spec bind_enter_to_submit_button(InputID :: binary(), ButtonToClickID :: binary()) -> string().
%% ====================================================================
bind_enter_to_submit_button(InputID, ButtonToClickID) ->
    Script = <<"$('#", InputID/binary, "').bind('keydown', function (e){",
    "if (e.which == 13) { e.preventDefault(); document.getElementById('", ButtonToClickID/binary, "').click(); } });">>,
    wire(Script, false).


%% bind_element_click/2
%% ====================================================================
%% @doc Binds click actions on a selected InputID to evaluation of given
%% javascript code. The code must be wrapped in function(event){}.
%% @end
-spec bind_element_click(InputID :: binary(), Javascript :: binary()) -> string().
%% ====================================================================
bind_element_click(InputID, Javascript) ->
    Script = <<"$('#", InputID/binary, "').bind('click', ", Javascript/binary, ");">>,
    wire(Script, false).


%% update/2
%% ====================================================================
%% @doc Updates contents of a DOM element.
%% @end
-spec update(Target :: binary(), Content :: term()) -> ok.
%% ====================================================================
update(Target, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(Target, <<"html">>, RenderedElements, true).


%% replace/2
%% ====================================================================
%% @doc Replaces a DOM element with another.
%% @end
-spec replace(Target :: binary(), Content :: term()) -> ok.
%% ====================================================================
replace(Target, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(Target, <<"replaceWith">>, RenderedElements, true).


%% insert_top/2
%% ====================================================================
%% @doc Prepends an element to a DOM element.
%% @end
-spec insert_top(Target :: binary(), Content :: term()) -> ok.
%% ====================================================================
insert_top(Target, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(Target, <<"prepend">>, RenderedElements, true).


%% insert_bottom/2
%% ====================================================================
%% @doc Appends an element to a DOM element.
%% @end
-spec insert_bottom(Target :: binary(), Content :: term()) -> ok.
%% ====================================================================
insert_bottom(Target, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(Target, <<"append">>, RenderedElements, true).


%% insert_before/2
%% ====================================================================
%% @doc Inserts an element before a DOM element.
%% @end
-spec insert_before(Target :: binary(), Content :: term()) -> ok.
%% ====================================================================
insert_before(Target, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(Target, <<"before">>, RenderedElements, true).


%% insert_after/2
%% ====================================================================
%% @doc Inserts an element after a DOM element.
%% @end
-spec insert_after(Target :: binary(), Content :: term()) -> ok.
%% ====================================================================
insert_after(Target, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(Target, <<"after">>, RenderedElements, true).


%% remove/1
%% ====================================================================
%% @doc Removes an element from DOM.
%% @end
-spec remove(Target :: binary()) -> ok.
%% ====================================================================
remove(Target) ->
    wire(Target, <<"remove">>, <<"">>, true).


%% show/1
%% ====================================================================
%% @doc Displays an HTML element.
%% @end
-spec show(Target :: binary()) -> ok.
%% ====================================================================
show(Target) ->
    wire(Target, <<"show">>, <<"">>, false).


%% hide/1
%% ====================================================================
%% @doc Hides an HTML element.
%% @end
-spec hide(Target :: binary()) -> ok.
%% ====================================================================
hide(Target) ->
    wire(Target, <<"hide">>, <<"">>, false).


%% add_class/2
%% ====================================================================
%% @doc Adds a class to an HTML element.
%% @end
-spec add_class(Target :: binary(), Class :: binary()) -> ok.
%% ====================================================================
add_class(Target, Class) ->
    wire(Target, <<"addClass">>, Class, false).


%% remove_class/2
%% ====================================================================
%% @doc Removes a class from an HTML element.
%% @end
-spec remove_class(Target :: binary(), Class :: binary()) -> ok.
%% ====================================================================
remove_class(Target, Class) ->
    wire(Target, <<"removeClass">>, Class, false).


%% slide_up/2
%% ====================================================================
%% @doc Animates an HTML element, displaying it in sliding motion.
%% @end
-spec slide_up(Target :: binary(), Speed :: integer()) -> ok.
%% ====================================================================
slide_up(Target, Speed) ->
    wire(Target, <<"slideUp">>, integer_to_binary(Speed), false).


%% slide_down/2
%% ====================================================================
%% @doc Animates an HTML element, hiding it in sliding motion.
%% @end
-spec slide_down(Target :: binary(), Speed :: integer()) -> ok.
%% ====================================================================
slide_down(Target, Speed) ->
    wire(Target, <<"slideDown">>, integer_to_binary(Speed), false).


%% fade_in/2
%% ====================================================================
%% @doc Animates an HTML element, making it appear over time.
%% @end
-spec fade_in(Target :: binary(), Speed :: integer()) -> ok.
%% ====================================================================
fade_in(Target, Speed) ->
    wire(Target, <<"fadeIn">>, integer_to_binary(Speed), false).


%% focus/1
%% ====================================================================
%% @doc Focuses an HTML element.
%% @end
-spec focus(Target :: binary()) -> ok.
%% ====================================================================
focus(Target) ->
    wire(Target, <<"focus">>, <<"">>, false).


%% select_text/1
%% ====================================================================
%% @doc Focuses an HTML element (e. g. a textbox) and selects its text.
%% @end
-spec select_text(Target :: binary()) -> ok.
%% ====================================================================
select_text(Target) ->
    Script = <<"$('#", Target/binary, "').focus().select();">>,
    wire(Script).


%% set_value/2
%% ====================================================================
%% @doc Sets value of an HTML element - e. g. textbox.
%% @end
-spec set_value(Target :: binary(), Value :: binary()) -> ok.
%% ====================================================================
set_value(Target, Value) ->
    wire(Target, <<"val">>, Value, false).

