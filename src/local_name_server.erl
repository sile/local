%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% TODO: doc
-module(local_name_server).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Application Internal API
%%----------------------------------------------------------------------------------------------------------------------
-export([register_name/3]).
-export([unregister_name/2]).
-export([whereis_name/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------------------------------------------------------
-record(state,
        {
          name                           :: local:name_server_name(),
          pid_to_name = gb_trees:empty() :: gb_trees:tree(pid(), local:process_name())
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec start_link(local:name_server_name()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()}.
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

%%----------------------------------------------------------------------------------------------------------------------
%% Application Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec register_name(local:name_server_name(), local:process_name(), pid()) -> yes | no.
register_name(Server, Name, Pid) ->
    gen_server:call(Server, {register_name, {Name, Pid}}).

%% @private
-spec unregister_name(local:name_server_name(), local:process_name()) -> ok.
unregister_name(Server, Name) ->
    gen_server:call(Server, {unregister_name, Name}).

%% @private
-spec whereis_name(local:name_server_name(), local:process_name()) -> pid() | undefined.
whereis_name(Server, Name) ->
    try ets:lookup(Server, Name) of
        []         -> undefined;
        [{_, Pid}] -> Pid
    catch
        _:_ -> undefined
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([Name]) ->
    _ = process_flag(trap_exit, true),
    State =
        #state{
           name = Name
          },
    _ = ets:new(Name, [named_table, protected, set, {read_concurrency, true}]),
    {ok, State}.

%% @private
handle_call({register_name, Arg}, _From, State) ->
    handle_register_name(Arg, State);
handle_call({unregister_name, Arg}, _From, State) ->
    handle_unregister_name(Arg, State);
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({'EXIT', Pid, Reason}, State) ->
    handle_exit(Pid, Reason, State);
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec handle_register_name(Arg, #state{}) -> {reply, Result, #state{}} when
      Arg    :: {local:process_name(), pid()},
      Result :: yes | no.
handle_register_name({Name, Pid}, State) ->
    case ets:lookup(State#state.name, Name) =:= [] andalso not gb_trees:is_defined(Pid, State#state.pid_to_name) of
        false -> {reply, no, State};
        true  ->
            true = ets:insert(State#state.name, {Name, Pid}),
            true = link(Pid),
            PidToName = gb_trees:insert(Pid, Name, State#state.pid_to_name),
            {reply, yes, State#state{pid_to_name = PidToName}}
    end.

-spec handle_unregister_name(local:process_name(), #state{}) -> {reply, ok, #state{}}.
handle_unregister_name(Name, State) ->
    case ets:lookup(State#state.name, Name) of
        []         -> {reply, ok, State};
        [{_, Pid}] ->
            true = local_lib:unlink_and_flush(Pid),
            true = ets:delete(State#state.name, Name),
            PidToName = gb_trees:delete(Pid, State#state.pid_to_name),
            {reply, ok, State#state{pid_to_name = PidToName}}
    end.

-spec handle_exit(pid(), term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_exit(Pid, Reason, State) ->
    case gb_trees:lookup(Pid, State#state.pid_to_name) of
        none          -> {stop, Reason, State};
        {value, Name} ->
            true = ets:delete(State#state.name, Name),
            PidToName = gb_trees:delete(Pid, State#state.pid_to_name),
            {noreply, State#state{pid_to_name = PidToName}}
    end.
