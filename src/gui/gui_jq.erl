%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains page manipulation and asynchronous updates
%%% functions, mostly based on jquery.
%%% IMPORTANT: n2o's wf module must not be used directly!
%%% These functions are a wrapper to that module, which gives control over
%%% such aspects as javascript escaping and event wiring sequence.
%%% @end
%%%-------------------------------------------------------------------

-module(gui_jq).
-include("gui/common.hrl").
-include("logging.hrl").

% General javascript wiring
-export([wire/1, wire/2, wire/4]).

% Wiring postbacks and form submissions
-export([postback_action/2, form_submit_action/3]).

% Redirections
-export([redirect/1, redirect_to_login/0, redirect_to_login/1, redirect_from_login/0]).

% Useful functions for binding custom events
-export([register_escape_event/1, bind_enter_to_submit_button/2, bind_enter_to_change_focus/2,
    bind_key_to_click/2, bind_key_to_click_on_class/2, bind_element_click/2]).

% DOM updates
-export([update/2, replace/2, insert_top/2, insert_bottom/2, insert_before/2, insert_after/2, remove/1]).

% Commonly used jquery functions
-export([show/1, hide/1, add_class/2, remove_class/2, slide_up/2, slide_down/2, fade_in/2, fade_out/2, delay/2]).
-export([focus/1, set_text/2, select_text/1, set_value/2, set_width/2, click/1, prop/3, css/3]).

% Bootbox functions
-export([confirm_popup/2, info_popup/3, info_popup/4, dialog_popup/3, dialog_popup/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Sends a javascript code snippet or action record (which will be
%% rendered to js) to the client for immediate evaluation.
%% NOTE! Does not js_escape the script, the developer has to make sure 
%% the wired javascript is safe, or use DOM manipulation functions, which 
%% include safe escaping.
%% @end
%%--------------------------------------------------------------------
-spec wire(Action :: term()) -> ok.
wire(Action) ->
    wire(Action, false).

%%--------------------------------------------------------------------
%% @doc Sends a javascript code snippet or action record (which will be
%% rendered to js) to the client for immediate evaluation.
%% NOTE! Does not js_escape the script, the developer has to make sure
%% the wired javascript is safe, or use DOM manipulation functions, which
%% include safe escaping.
%% Eager flag can be used. It is ensured that all eager actions
%% will be evaluated before normal actions.
%% @end
%%--------------------------------------------------------------------
-spec wire(Script :: string() | binary() | #api{}, Eager :: boolean()) -> ok.
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

%%--------------------------------------------------------------------
%% @doc Convienience function to render javascript code.
%% Eager flag can be used.
%% @end
%%--------------------------------------------------------------------
-spec wire(TargetID :: binary(), Method :: binary(), Args :: binary() | integer(), Eager :: boolean()) -> ok.
wire(TargetID, Method, Args, Eager) ->
    RenderedArgs = case Args of
                       <<"">> -> <<"">>;
                       <<"''">> -> <<"''">>;
                       _ when is_integer(Args) -> integer_to_binary(Args);
                       _ when is_binary(Args) -> <<"'", Args/binary, "'">>
                   end,
    Script = <<"$('#", TargetID/binary, "').", Method/binary, "(", RenderedArgs/binary, ");">>,
    wire(Script, Eager).

%%--------------------------------------------------------------------
%% @doc Returns action records that can be assigned to actions field of an element.
%% It will cause form submission with given postback, and values of field(s)
%% given in Sources arg will be available by gui_ctx:form_param function.
%% @end
%%--------------------------------------------------------------------
-spec postback_action(TriggerID :: binary(), Postback :: term()) -> #event{}.
postback_action(TriggerID, Postback) ->
    #event{type = "click", postback = Postback, target = gui_str:to_list(TriggerID)}.

%%--------------------------------------------------------------------
%% @doc Returns action records that can be assigned to actions field of an element.
%% It will cause form submission with given postback, and values of field(s)
%% given in Sources arg will be available by gui_ctx:form_param function.
%% @end
%%--------------------------------------------------------------------
-spec form_submit_action(TriggerID :: binary(), Postback :: term(),
    Sources :: binary() | [binary()]) -> #event{}.
form_submit_action(TriggerID, Postback, SourcesArg) ->
    Sources = lists:map(fun(Source) ->
        gui_str:to_list(Source)
    end, lists:flatten([SourcesArg])),
    #event{type = "click", postback = Postback, target = gui_str:to_list(TriggerID), source = Sources}.

%%--------------------------------------------------------------------
%% @doc Redirects to given page.
%% @end
%%--------------------------------------------------------------------
-spec redirect(URL :: binary()) -> ok.
redirect(URL) ->
    wf:redirect(URL),
    ok.

%%--------------------------------------------------------------------
%% @equiv redirect_to_login(<<"/login">>).
%%--------------------------------------------------------------------
-spec redirect_to_login() -> ok.
redirect_to_login() ->
    redirect_to_login(<<"/login">>),
    ok.

%%--------------------------------------------------------------------
%% @doc Redirects the user to login page.
%% @end
%%--------------------------------------------------------------------
-spec redirect_to_login(LoginURL :: binary()) -> ok.
redirect_to_login(LoginURL) ->
    wf:redirect(LoginURL),
    ok.

%%--------------------------------------------------------------------
%% @doc Redirects back from login page to the index page.
%% @end
%%--------------------------------------------------------------------
-spec redirect_from_login() -> ok.
redirect_from_login() ->
    wf:redirect(<<"/">>),
    ok.

%%--------------------------------------------------------------------
%% @doc Binds escape button so that it generates an event every time it's pressed.
%% The event will call the function api_event(Tag, [], Context) on page module.
%% Tag has to be a string as n2o engine requires so.
%% @end
%%--------------------------------------------------------------------
-spec register_escape_event(Tag :: string()) -> ok.
register_escape_event(Tag) ->
    wire(#api{name = Tag, tag = Tag}, false),
    wire(<<"$(document).bind('keydown', function (e){if (e.which == 27) ", (list_to_binary(Tag))/binary, "();});">>, false).

%%--------------------------------------------------------------------
%% @doc Makes any enter keypresses on InputID (whenever it is focused)
%% perform a click on a selected ButtonToClickID. This way, it allows
%% easy form submission with enter key.
%% @end
%%--------------------------------------------------------------------
-spec bind_enter_to_submit_button(InputID :: binary(), ButtonToClickID :: binary()) -> ok.
bind_enter_to_submit_button(InputID, ButtonToClickID) ->
    Script = <<"$('#", InputID/binary, "').bind('keydown', function (e){",
    "if (e.which == 13) { e.preventDefault(); document.getElementById('", ButtonToClickID/binary, "').click(); } });">>,
    wire(Script, false).

%%--------------------------------------------------------------------
%% @doc Makes any enter keypresses on InputID (whenever it is focused)
%% change focus to selected target. This way, it allows
%% easy switching between text elements with enter key.
%% @end
%%--------------------------------------------------------------------
-spec bind_enter_to_change_focus(InputID :: binary(), TargetID :: binary()) -> ok.
bind_enter_to_change_focus(InputID, TargetID) ->
    Script = <<"$('#", InputID/binary, "').bind('keydown', function (e){",
    "if (e.which == 13) { e.preventDefault(); document.getElementById('", TargetID/binary, "').focus(); } });">>,
    wire(Script, false).

%%--------------------------------------------------------------------
%% @doc Makes any keypresses of given key to click on selected target.
%% @end
%%--------------------------------------------------------------------
-spec bind_key_to_click(KeyCode :: binary(), TargetID :: binary()) -> ok.
bind_key_to_click(KeyCode, TargetID) ->
    Script = <<"$(document).bind('keydown', function (e){",
    "if (e.which == ", KeyCode/binary, ") { e.preventDefault(); document.getElementById('", TargetID/binary, "').click(); } });">>,
    wire(Script, false).

%%--------------------------------------------------------------------
%% @doc Makes any keypresses of given key to click on selected class.
%% @end
%%--------------------------------------------------------------------
-spec bind_key_to_click_on_class(KeyCode :: binary(), ClassID :: binary()) -> ok.
bind_key_to_click_on_class(KeyCode, ClassID) ->
    Script = <<"$(document).bind('keydown', function (e){",
    "if (e.which == ", KeyCode/binary, ") { e.preventDefault(); $('.", ClassID/binary, "').click(); } });">>,
    wire(Script, false).

%%--------------------------------------------------------------------
%% @doc Binds click actions on a selected InputID to evaluation of given
%% javascript code. The code must be wrapped in function(event){}.
%% @end
%%--------------------------------------------------------------------
-spec bind_element_click(InputID :: binary(), Javascript :: binary()) -> ok.
bind_element_click(InputID, Javascript) ->
    Script = <<"$('#", InputID/binary, "').bind('click', ", Javascript/binary, ");">>,
    wire(Script, false).

%%--------------------------------------------------------------------
%% @doc Updates contents of a DOM element.
%% @end
%%--------------------------------------------------------------------
-spec update(TargetID :: binary(), Content :: term()) -> ok.
update(TargetID, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(TargetID, <<"html">>, RenderedElements, true).

%%--------------------------------------------------------------------
%% @doc Replaces a DOM element with another.
%% @end
%%--------------------------------------------------------------------
-spec replace(TargetID :: binary(), Content :: term()) -> ok.
replace(TargetID, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(TargetID, <<"replaceWith">>, RenderedElements, true).

%%--------------------------------------------------------------------
%% @doc Prepends an element to a DOM element.
%% @end
%%--------------------------------------------------------------------
-spec insert_top(TargetID :: binary(), Content :: term()) -> ok.
insert_top(TargetID, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(TargetID, <<"prepend">>, RenderedElements, true).

%%--------------------------------------------------------------------
%% @doc Appends an element to a DOM element.
%% @end
%%--------------------------------------------------------------------
-spec insert_bottom(TargetID :: binary(), Content :: term()) -> ok.
insert_bottom(TargetID, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(TargetID, <<"append">>, RenderedElements, true).

%%--------------------------------------------------------------------
%% @doc Inserts an element before a DOM element.
%% @end
%%--------------------------------------------------------------------
-spec insert_before(TargetID :: binary(), Content :: term()) -> ok.
insert_before(TargetID, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(TargetID, <<"before">>, RenderedElements, true).

%%--------------------------------------------------------------------
%% @doc Inserts an element after a DOM element.
%% @end
%%--------------------------------------------------------------------
-spec insert_after(TargetID :: binary(), Content :: term()) -> ok.
insert_after(TargetID, Elements) ->
    RenderedElements = gui_str:js_escape(wf:render(Elements)),
    wire(TargetID, <<"after">>, RenderedElements, true).

%%--------------------------------------------------------------------
%% @doc Removes an element from DOM.
%% @end
%%--------------------------------------------------------------------
-spec remove(TargetID :: binary()) -> ok.
remove(TargetID) ->
    wire(TargetID, <<"remove">>, <<"">>, true).

%%--------------------------------------------------------------------
%% @doc Displays an HTML element.
%% @end
%%--------------------------------------------------------------------
-spec show(TargetID :: binary()) -> ok.
show(TargetID) ->
    wire(TargetID, <<"show">>, <<"">>, false).

%%--------------------------------------------------------------------
%% @doc Hides an HTML element.
%% @end
%%--------------------------------------------------------------------
-spec hide(TargetID :: binary()) -> ok.
hide(TargetID) ->
    wire(TargetID, <<"hide">>, <<"">>, false).

%%--------------------------------------------------------------------
%% @doc Adds a class to an HTML element.
%% @end
%%--------------------------------------------------------------------
-spec add_class(TargetID :: binary(), Class :: binary()) -> ok.
add_class(TargetID, Class) ->
    wire(TargetID, <<"addClass">>, Class, false).

%%--------------------------------------------------------------------
%% @doc Removes a class from an HTML element.
%% @end
%%--------------------------------------------------------------------
-spec remove_class(TargetID :: binary(), Class :: binary()) -> ok.
remove_class(TargetID, Class) ->
    wire(TargetID, <<"removeClass">>, Class, false).

%%--------------------------------------------------------------------
%% @doc Animates an HTML element, displaying it in sliding motion.
%% @end
%%--------------------------------------------------------------------
-spec slide_up(TargetID :: binary(), Speed :: integer()) -> ok.
slide_up(TargetID, Speed) ->
    wire(TargetID, <<"slideUp">>, Speed, false).

%%--------------------------------------------------------------------
%% @doc Animates an HTML element, hiding it in sliding motion.
%% @end
%%--------------------------------------------------------------------
-spec slide_down(TargetID :: binary(), Speed :: integer()) -> ok.
slide_down(TargetID, Speed) ->
    wire(TargetID, <<"slideDown">>, Speed, false).

%%--------------------------------------------------------------------
%% @doc Animates an HTML element, making it appear over time.
%% @end
%%--------------------------------------------------------------------
-spec fade_in(TargetID :: binary(), Speed :: integer()) -> ok.
fade_in(TargetID, Speed) ->
    wire(TargetID, <<"fadeIn">>, Speed, false).

%%--------------------------------------------------------------------
%% @doc Animates an HTML element, making it disappear over time.
%% @end
%%--------------------------------------------------------------------
-spec fade_out(TargetID :: binary(), Speed :: integer()) -> ok.
fade_out(TargetID, Speed) ->
    wire(TargetID, <<"fadeOut">>, Speed, false).

%%--------------------------------------------------------------------
%% @doc Delays javascript actions on given target.
%% @end
%%--------------------------------------------------------------------
-spec delay(TargetID :: binary(), Time :: integer()) -> ok.
delay(TargetID, Time) ->
    wire(TargetID, <<"delay">>, Time, false).

%%--------------------------------------------------------------------
%% @doc Focuses an HTML element.
%% @end
%%--------------------------------------------------------------------
-spec focus(TargetID :: binary()) -> ok.
focus(TargetID) ->
    wire(TargetID, <<"focus">>, <<"">>, false).

%%--------------------------------------------------------------------
%% @doc Set the content of each element in the set of matched elements
%% to the specified text.
%% @end
%%--------------------------------------------------------------------
-spec set_text(TargetID :: binary(), Value :: binary()) -> ok.
set_text(TargetID, Value) ->
    wire(TargetID, <<"text">>, Value, false).

%%--------------------------------------------------------------------
%% @doc Focuses an HTML element (e. g. a textbox) and selects its text.
%% @end
%%--------------------------------------------------------------------
-spec select_text(TargetID :: binary()) -> ok.
select_text(TargetID) ->
    Script = <<"$('#", TargetID/binary, "').focus().select();">>,
    wire(Script).

%%--------------------------------------------------------------------
%% @doc Sets value of an HTML element - e. g. textbox.
%% @end
%%--------------------------------------------------------------------
-spec set_value(TargetID :: binary(), Value :: binary()) -> ok.
set_value(TargetID, Value) ->
    wire(TargetID, <<"val">>, Value, false).

%%--------------------------------------------------------------------
%% @doc Set the CSS width of each element in the set of matched elements.
%% @end
%%--------------------------------------------------------------------
-spec set_width(TargetID :: binary(), Value :: binary()) -> ok.
set_width(TargetID, Value) ->
    wire(TargetID, <<"width">>, Value, false).

%%--------------------------------------------------------------------
%% @doc Performs click action on given element.
%% @end
%%--------------------------------------------------------------------
-spec click(TargetID :: binary()) -> ok.
click(TargetID) ->
    wire(TargetID, <<"click">>, <<"">>, false).

%%--------------------------------------------------------------------
%% @doc Set one or more properties for the set of matched elements.
%% @end
%%--------------------------------------------------------------------
-spec prop(TargetID :: binary(), PropertyName :: binary(), Value :: binary()) -> ok.
prop(TargetID, PropertyName, Value) ->
    Script = <<"$('#", TargetID/binary, "').prop('", PropertyName/binary, "','", Value/binary, "');">>,
    wire(Script, false).

%%--------------------------------------------------------------------
%% @doc Set one or more CSS properties for the set of matched elements.
%% @end
%%--------------------------------------------------------------------
-spec css(TargetID :: binary(), PropertyName :: binary(), Value :: binary()) -> ok.
css(TargetID, PropertyName, Value) ->
    Script = <<"$('#", TargetID/binary, "').css('", PropertyName/binary, "','", Value/binary, "');">>,
    wire(Script, false).

%%--------------------------------------------------------------------
%% @doc Displays confirm popup using Bootbox API with custom message.
%% In case of message confirmation it executes supplied script.
%% NOTE! It is required to add bootbox.js script to web page.
%% @end
%%--------------------------------------------------------------------
-spec confirm_popup(Message :: binary(), Script :: binary()) -> ok.
confirm_popup(Message, Script) ->
    wire(<<"bootbox.confirm(
        '", Message/binary, "',
        function(result) {
            if(result) {", Script/binary, "}
        }
    );">>).

%%--------------------------------------------------------------------
%% @equiv info_popup(Title, Message, Script, <<"btn-primary">>)
%% @end
%%--------------------------------------------------------------------
-spec info_popup(Title :: binary(), Message :: binary(), Script :: binary()) -> ok.
info_popup(Title, Message, Script) ->
    info_popup(Title, Message, Script, <<"btn-primary">>).

%%--------------------------------------------------------------------
%% @doc Displays info popup using Bootbox API with custom title,
%% message and "OK" button. In case of message confirmation it executes
%% supplied script.
%% NOTE! It is required to add bootbox.js script to web page.
%% @end
%%--------------------------------------------------------------------
-spec info_popup(Title :: binary(), Message :: binary(), Script :: binary(),
    ConfirmButtonClass :: binary()) -> ok.
info_popup(Title, Message, Script, ConfirmButtonClass) ->
    wire(<<"var box = bootbox.dialog({
        title: '", Title/binary, "',
        message: '", Message/binary, "',
        buttons: {
            'OK': {
                className: '", ConfirmButtonClass/binary, " confirm',
                callback: function() {", Script/binary, "}
            }
        }
    });">>).

%%--------------------------------------------------------------------
%% @equiv dialog_popup(Title, Message, Script, <<"btn-primary">>)
%% @end
%%--------------------------------------------------------------------
-spec dialog_popup(Title :: binary(), Message :: binary(), Script :: binary()) -> ok.
dialog_popup(Title, Message, Script) ->
    dialog_popup(Title, Message, Script, <<"btn-primary">>).

%%--------------------------------------------------------------------
%% @doc Displays info popup using Bootbox API with custom title,
%% message and "OK", "Cancel" buttons. In case of message confirmation
%% it executes supplied script. Allows to set confirm button class.
%% NOTE! It is required to add bootbox.js script to web page.
%% @end
%%--------------------------------------------------------------------
-spec dialog_popup(Title :: binary(), Message :: binary(), Script :: binary(),
    ConfirmButtonClass :: binary()) -> ok.
dialog_popup(Title, Message, Script, ConfirmButtonClass) ->
    wire(<<"var box = bootbox.dialog({
        title: '", Title/binary, "',
        message: '", Message/binary, "',
        buttons: {
            'Cancel': {
                className: 'cancel'
            },
            'OK': {
                className: '", ConfirmButtonClass/binary, " confirm',
                callback: function() {", Script/binary, "}
            }
        }
    });">>).