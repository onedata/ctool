%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains page manipulation and asynchronous updates
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
-export([register_escape_event/1, bind_enter_to_submit_button/2, bind_enter_to_change_focus/2,
    bind_key_to_click/2, bind_key_to_click_on_class/2, bind_element_click/2]).

% DOM updates
-export([update/2, replace/2, insert_top/2, insert_bottom/2, insert_before/2, insert_after/2, remove/1]).

% Commonly used jquery functions
-export([show/1, hide/1, add_class/2, remove_class/2, slide_up/2, slide_down/2, fade_in/2, fade_out/2, delay/2]).
-export([focus/1, set_text/2, select_text/1, set_value/2, set_width/2, click/1, prop/3, css/3]).

% Bootbox functions
-export([confirm_popup/2, info_popup/3, dialog_popup/3]).


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
-spec wire(TargetID :: binary(), Method :: binary(), Args :: binary() | integer(), Eager :: boolean()) -> ok.
%% ====================================================================
wire(TargetID, Method, Args, Eager) ->
    RenderedArgs = case Args of
                       <<"">> -> <<"">>;
                       <<"''">> -> <<"''">>;
                       _ when is_integer(Args) -> integer_to_binary(Args);
                       _ when is_binary(Args) -> <<"'", Args/binary, "'">>
                   end,
    Script = <<"$('#", TargetID/binary, "').", Method/binary, "(", RenderedArgs/binary, ");">>,
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
-spec form_submit_action(TriggerID :: binary(), Postback :: term(), Sources :: binary() | [binary()]) -> ok.
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
        TargetIDPage -> wf:redirect(TargetIDPage)
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


%% bind_enter_to_change_focus/2
%% ====================================================================
%% @doc Makes any enter keypresses on InputID (whenever it is focused)
%% change focus to selected target. This way, it allows
%% easy switching between text elements with enter key.
%% @end
-spec bind_enter_to_change_focus(InputID :: binary(), TargetID :: binary()) -> string().
%% ====================================================================
bind_enter_to_change_focus(InputID, TargetID) ->
    Script = <<"$('#", InputID/binary, "').bind('keydown', function (e){",
    "if (e.which == 13) { e.preventDefault(); document.getElementById('", TargetID/binary, "').focus(); } });">>,
    wire(Script, false).


%% bind_key_to_click/2
%% ====================================================================
%% @doc Makes any keypresses of given key to click on selected target.
%% @end
-spec bind_key_to_click(KeyCode :: binary(), TargetID :: binary()) -> string().
%% ====================================================================
bind_key_to_click(KeyCode, TargetID) ->
    Script = <<"$(document).bind('keydown', function (e){",
    "if (e.which == ", KeyCode/binary, ") { e.preventDefault(); document.getElementById('", TargetID/binary, "').click(); } });">>,
    wire(Script, false).


%% bind_key_to_click_on_class/2
%% ====================================================================
%% @doc Makes any keypresses of given key to click on selected class.
%% @end
-spec bind_key_to_click_on_class(KeyCode :: binary(), ClassID :: binary()) -> string().
%% ====================================================================
bind_key_to_click_on_class(KeyCode, ClassID) ->
    Script = <<"$(document).bind('keydown', function (e){",
    "if (e.which == ", KeyCode/binary, ") { e.preventDefault(); $('.", ClassID/binary, "').click(); } });">>,
    gui_jq:wire(Script, false).


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
-spec update(TargetID :: binary(), Content :: term()) -> ok.
%% ====================================================================
update(TargetID, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(TargetID, <<"html">>, RenderedElements, true).


%% replace/2
%% ====================================================================
%% @doc Replaces a DOM element with another.
%% @end
-spec replace(TargetID :: binary(), Content :: term()) -> ok.
%% ====================================================================
replace(TargetID, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(TargetID, <<"replaceWith">>, RenderedElements, true).


%% insert_top/2
%% ====================================================================
%% @doc Prepends an element to a DOM element.
%% @end
-spec insert_top(TargetID :: binary(), Content :: term()) -> ok.
%% ====================================================================
insert_top(TargetID, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(TargetID, <<"prepend">>, RenderedElements, true).


%% insert_bottom/2
%% ====================================================================
%% @doc Appends an element to a DOM element.
%% @end
-spec insert_bottom(TargetID :: binary(), Content :: term()) -> ok.
%% ====================================================================
insert_bottom(TargetID, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(TargetID, <<"append">>, RenderedElements, true).


%% insert_before/2
%% ====================================================================
%% @doc Inserts an element before a DOM element.
%% @end
-spec insert_before(TargetID :: binary(), Content :: term()) -> ok.
%% ====================================================================
insert_before(TargetID, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(TargetID, <<"before">>, RenderedElements, true).


%% insert_after/2
%% ====================================================================
%% @doc Inserts an element after a DOM element.
%% @end
-spec insert_after(TargetID :: binary(), Content :: term()) -> ok.
%% ====================================================================
insert_after(TargetID, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(TargetID, <<"after">>, RenderedElements, true).


%% remove/1
%% ====================================================================
%% @doc Removes an element from DOM.
%% @end
-spec remove(TargetID :: binary()) -> ok.
%% ====================================================================
remove(TargetID) ->
    wire(TargetID, <<"remove">>, <<"">>, true).


%% show/1
%% ====================================================================
%% @doc Displays an HTML element.
%% @end
-spec show(TargetID :: binary()) -> ok.
%% ====================================================================
show(TargetID) ->
    wire(TargetID, <<"show">>, <<"">>, false).


%% hide/1
%% ====================================================================
%% @doc Hides an HTML element.
%% @end
-spec hide(TargetID :: binary()) -> ok.
%% ====================================================================
hide(TargetID) ->
    wire(TargetID, <<"hide">>, <<"">>, false).


%% add_class/2
%% ====================================================================
%% @doc Adds a class to an HTML element.
%% @end
-spec add_class(TargetID :: binary(), Class :: binary()) -> ok.
%% ====================================================================
add_class(TargetID, Class) ->
    wire(TargetID, <<"addClass">>, Class, false).


%% remove_class/2
%% ====================================================================
%% @doc Removes a class from an HTML element.
%% @end
-spec remove_class(TargetID :: binary(), Class :: binary()) -> ok.
%% ====================================================================
remove_class(TargetID, Class) ->
    wire(TargetID, <<"removeClass">>, Class, false).


%% slide_up/2
%% ====================================================================
%% @doc Animates an HTML element, displaying it in sliding motion.
%% @end
-spec slide_up(TargetID :: binary(), Speed :: integer()) -> ok.
%% ====================================================================
slide_up(TargetID, Speed) ->
    wire(TargetID, <<"slideUp">>, Speed, false).


%% slide_down/2
%% ====================================================================
%% @doc Animates an HTML element, hiding it in sliding motion.
%% @end
-spec slide_down(TargetID :: binary(), Speed :: integer()) -> ok.
%% ====================================================================
slide_down(TargetID, Speed) ->
    wire(TargetID, <<"slideDown">>, Speed, false).


%% fade_in/2
%% ====================================================================
%% @doc Animates an HTML element, making it appear over time.
%% @end
-spec fade_in(TargetID :: binary(), Speed :: integer()) -> ok.
%% ====================================================================
fade_in(TargetID, Speed) ->
    wire(TargetID, <<"fadeIn">>, Speed, false).


%% fade_out/2
%% ====================================================================
%% @doc Animates an HTML element, making it disappear over time.
%% @end
-spec fade_out(TargetID :: binary(), Speed :: integer()) -> ok.
%% ====================================================================
fade_out(TargetID, Speed) ->
    wire(TargetID, <<"fadeOut">>, Speed, false).


%% delay/2
%% ====================================================================
%% @doc Delays javascript actions on given target.
%% @end
-spec delay(TargetID :: binary(), Time :: integer()) -> ok.
%% ====================================================================
delay(TargetID, Time) ->
    wire(TargetID, <<"delay">>, Time, false).


%% focus/1
%% ====================================================================
%% @doc Focuses an HTML element.
%% @end
-spec focus(TargetID :: binary()) -> ok.
%% ====================================================================
focus(TargetID) ->
    wire(TargetID, <<"focus">>, <<"">>, false).


%% set_text/2
%% ====================================================================
%% @doc Set the content of each element in the set of matched elements
%% to the specified text.
%% @end
-spec set_text(TargetID :: binary(), Value :: binary()) -> ok.
%% ====================================================================
set_text(TargetID, Value) ->
    wire(TargetID, <<"text">>, Value, false).


%% select_text/1
%% ====================================================================
%% @doc Focuses an HTML element (e. g. a textbox) and selects its text.
%% @end
-spec select_text(TargetID :: binary()) -> ok.
%% ====================================================================
select_text(TargetID) ->
    Script = <<"$('#", TargetID/binary, "').focus().select();">>,
    wire(Script).


%% set_value/2
%% ====================================================================
%% @doc Sets value of an HTML element - e. g. textbox.
%% @end
-spec set_value(TargetID :: binary(), Value :: binary()) -> ok.
%% ====================================================================
set_value(TargetID, Value) ->
    wire(TargetID, <<"val">>, Value, false).


%% set_width/2
%% ====================================================================
%% @doc Set the CSS width of each element in the set of matched elements.
%% @end
-spec set_width(TargetID :: binary(), Value :: binary()) -> ok.
%% ====================================================================
set_width(TargetID, Value) ->
    wire(TargetID, <<"width">>, Value, false).


%% click/2
%% ====================================================================
%% @doc Performs click action on given element.
%% @end
-spec click(TargetID :: binary()) -> ok.
%% ====================================================================
click(TargetID) ->
    wire(TargetID, <<"click">>, <<"">>, false).


%% prop/3
%% ====================================================================
%% @doc Set one or more properties for the set of matched elements.
%% @end
-spec prop(TargetID :: binary(), PropertyName :: binary(), Value :: binary()) -> string().
%% ====================================================================
prop(TargetID, PropertyName, Value) ->
    Script = <<"$('#", TargetID/binary, "').prop('", PropertyName/binary, "','", Value/binary, "');">>,
    wire(Script, false).


%% css/3
%% ====================================================================
%% @doc Set one or more CSS properties for the set of matched elements.
%% @end
-spec css(TargetID :: binary(), PropertyName :: binary(), Value :: binary()) -> string().
%% ====================================================================
css(TargetID, PropertyName, Value) ->
    Script = <<"$('#", TargetID/binary, "').css('", PropertyName/binary, "','", Value/binary, "');">>,
    wire(Script, false).


%% confirm_popup/2
%% ====================================================================
%% @doc Displays confirm popup using Bootbox API with custom message.
%% In case of message confirmation it executes supplied script.
%% NOTE! It is required to add bootbox.js script to web page.
-spec confirm_popup(Message :: binary(), Script :: binary()) -> binary().
%% ====================================================================
confirm_popup(Message, Script) ->
    gui_jq:wire(<<"bootbox.confirm(
        '", Message/binary, "',
        function(result) {
            if(result) {", Script/binary, "}
        }
    );">>).


%% info_popup/3
%% ====================================================================
%% @doc Displays info popup using Bootbox API with custom title,
%% message and "OK" button. In case of message confirmation it executes
%% supplied script.
%% NOTE! It is required to add bootbox.js script to web page.
-spec info_popup(Title :: binary(), Message :: binary(), Script :: binary()) -> binary().
%% ====================================================================
info_popup(Title, Message, Script) ->
    gui_jq:wire(<<"var box = bootbox.dialog({
        title: '", Title/binary, "',
        message: '", Message/binary, "',
        buttons: {
            'OK': {
                className: 'btn-primary confirm',
                callback: function() {", Script/binary, "}
            }
        }
    });">>).


%% dialog_popup/3
%% ====================================================================
%% @doc Displays info popup using Bootbox API with custom title,
%% message and "OK", "Cancel" buttons. In case of message confirmation
%% it executes supplied script.
%% NOTE! It is required to add bootbox.js script to web page.
-spec dialog_popup(Title :: binary(), Message :: binary(), Script :: binary()) -> binary().
%% ====================================================================
dialog_popup(Title, Message, Script) ->
    gui_jq:wire(<<"var box = bootbox.dialog({
        title: '", Title/binary, "',
        message: '", Message/binary, "',
        buttons: {
            'Cancel': {
                className: 'cancel'
            },
            'OK': {
                className: 'btn-primary confirm',
                callback: function() {", Script/binary, "}
            }
        }
    });">>).