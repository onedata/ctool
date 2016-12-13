%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% gen_server responsible for mocking and unloading modules during
%%% tests on given node.
%%% @end
%%%-------------------------------------------------------------------
-module(mock_manager).
-author("Jakub Kudzia").

-behaviour(gen_server).

%% API
-export([start/0, new/2, unload/1, stop/0]).

-include("test/test_utils.hrl").

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {}).
-define(TIMEOUT, timer:seconds(60)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts mock manager
%% @end
%%--------------------------------------------------------------------
-spec(start() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start() ->
    gen_server:start({local, ?MOCK_MANAGER_NAME}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Mocks given Module.
%% @end
%%--------------------------------------------------------------------
-spec new(module(), Options :: [mock_opt()]) -> any().
new(Module, Options) ->
    gen_server:call(?MOCK_MANAGER_NAME, {new, Module, Options}, ?TIMEOUT).

%%--------------------------------------------------------------------
%% @doc
%% Unload mocked module.
%% @end
%%--------------------------------------------------------------------
-spec unload(module()) -> any().
unload(Module) ->
    gen_server:call(?MOCK_MANAGER_NAME, {unload, Module}, ?TIMEOUT).

%%--------------------------------------------------------------------
%% @doc
%% Stops mock_manager.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:cast(?MOCK_MANAGER_NAME, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes mock_manager.
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({new, Module, Options} = _Request, _From, State) ->
    Reply = try
        ok = check_and_unload_mock(Module),
        ok = meck:new(Module, Options)
    catch
        Type:Reason ->
            {error, Type, Reason}
    end,
    {reply, Reply, State};
handle_call({unload, Module} = _Request, _From, State) ->
    Reply = try
        ok = check_and_unload_mock(Module)
    catch
        Type:Reason ->
            {error, Type, Reason}
    end,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Unloads modules' mock and checks unload.
%% @end
%%--------------------------------------------------------------------
-spec check_and_unload_mock(Module :: module()) -> ok | unload_failed.
check_and_unload_mock(Module) ->
    check_and_unload_mock(Module, 100).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Unloads modules' mock and checks unload.
%% @end
%%--------------------------------------------------------------------
-spec check_and_unload_mock(Module :: module(), Num :: integer()) -> ok | unload_failed.
check_and_unload_mock(_Module, 0) ->
    unload_failed;
check_and_unload_mock(Module, Num) ->
    case whereis(meck_util:proc_name(Module)) of
        undefined ->
            ok;
        _ ->
            try
                meck:unload(Module)
            catch
                _:_ -> ok % will be checked and rerun
            end,
            check_and_unload_mock(Module, Num - 1)
    end.