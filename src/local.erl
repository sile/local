%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A Local Name Registration Facility
-module(local).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_name_server/1]).
-export([stop_name_server/1]).
-export([which_name_servers/0]).

-export([register_name/2]).
-export([unregister_name/1]).
-export([whereis_name/1]).
-export([send/2]).

-export([name_server_child_spec/1, name_server_child_spec/3]).

-export_type([name/0]).
-export_type([name_server_name/0]).
-export_type([process_name/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Types
%%----------------------------------------------------------------------------------------------------------------------
-type name() :: {name_server_name(), process_name()}.
-type name_server_name() :: atom().
-type process_name() :: term().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a name server process
%%
%% The process is registered locally as `ServerName' using `register/2'.
%%
%% If the process is successfully started the function returns `ok'.
%%
%% If there already exists a process with the specified `ServerName' the function returnes `{error, already_present}'.
-spec start_name_server(name_server_name()) -> ok | {error, Reason} when
      Reason :: already_present.
start_name_server(ServerName) when is_atom(ServerName) ->
    local_sup:start_name_server(ServerName);
start_name_server(ServerName) -> error(badarg, [ServerName]).

%% @doc Stops the name server `ServerName'
%%
%% If successful, the function returns `ok'.
%% If the name server identified by `ServerName' does not exist, the function returns `{error, not_found}'.
-spec stop_name_server(name_server_name()) -> ok | {error, Reason} when
      Reason :: not_found.
stop_name_server(ServerName) when is_atom(ServerName) ->
    local_sup:stop_name_server(ServerName);
stop_name_server(ServerName) -> error(badarg, [ServerName]).

%% @doc Returns a list of running name server
-spec which_name_servers() -> [name_server_name()].
which_name_servers() ->
    local_sup:which_name_servers().

%% @doc Locally assocates the name `Name' with a pid `Pid'.
%%
%% Let `NameServer' is `element(1, Name)', the registered name is limited to the name server `NameServer' scope.
%%
%% Assumes that the name server is already started, crashes otherwise.
%%
%% The function returns `yes' if successful, `no' if it failes.
%% For example, `no' is returned if an attempt is made to register an already registered process or
%% to register a process with a name that is already in use.
-spec register_name(Name :: name(), Pid :: pid()) -> yes | no.
register_name({NameServer, ProcName}, Pid) when is_atom(NameServer), is_pid(Pid) ->
    local_name_server:register_name(NameServer, ProcName, Pid);
register_name(Name, Pid) -> error(badarg, [Name, Pid]).

%% @doc Removes the locally registered name `Name'
-spec unregister_name(Name :: name()) -> ok.
unregister_name({NameServer, ProcName}) when is_atom(NameServer) ->
    local_name_server:unregister_name(NameServer, ProcName);
unregister_name(Name) -> error(badarg, [Name]).

%% @doc Returns the pid with the locally registered name `Name'
%%
%% Returns `undefined' if the name is not locally registered.
-spec whereis_name(name()) -> pid() | undefined.
whereis_name({NameServer, ProcName}) when is_atom(NameServer) ->
    local_name_server:whereis_name(NameServer, ProcName);
whereis_name(Name) -> error(badarg, [Name]).

%% @doc Sends the message `Msg' to the pid locally registered as `Name'
%%
%% Failure: If `Name' is not a locally registered name, the calling function will exit with reason `{badarg, {Name, Msg}}'
-spec send(name(), term()) -> pid().
send(Name, Msg) ->
    case whereis_name(Name) of
        undefined -> error({badarg, {Name, Msg}});
        Pid       -> _ = Pid ! Msg, Pid
    end.

%% @equiv name_server_child_spec(Name, Name, 5000)
-spec name_server_child_spec(name_server_name()) -> supervisor:child_spec().
name_server_child_spec(Name) ->
    name_server_child_spec(Name, Name, 5000).

%% @doc Returns the child spec for a local name server that is used in embedded mode.
%%
%% To embed a local name server in your application, you can simply add `ChildSpec' to your supervision tree.
-spec name_server_child_spec(ChildId, ServerName, Shutdown) -> ChildSpec when
      ChildId    :: supervisor:child_id(),
      ServerName :: name_server_name(),
      Shutdown   :: supervisor:shutdown(),
      ChildSpec  :: supervisor:child_spec().
name_server_child_spec(ChildId, ServerName, Shutdown) when is_atom(ServerName) ->
    {ChildId, {local_name_server, start_link, [ServerName]}, permanent, Shutdown, worker, [local_name_server]};
name_server_child_spec(ChildId, ServerName, Shutdown) ->
    error(badarg, [ChildId, ServerName, Shutdown]).
