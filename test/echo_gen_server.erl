%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Echo Server for Unit Test
-module(echo_gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).
-export([call/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts echo server
-spec start_link(term()) -> {ok, pid()} | {error, Reason::term()}.
start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).

%% @doc Returns `{echo, Msg}'
-spec call(term(), term()) -> {echo, term()}.
call(Name, Msg) ->
    gen_server:call(Name, {call, Msg}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    {ok, []}.

%% @priate
handle_call({call, Msg}, _From, State) ->
    {reply, {echo, Msg}, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
