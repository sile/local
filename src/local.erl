%% @copyright 2013-2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% TODO: doc
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

-export([make_name_server_child_spec/1]).

-export_type([name_server_name/0]).
-export_type([name/0]).
-export_type([process_name/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Types
%%----------------------------------------------------------------------------------------------------------------------
-type name_server_name() :: atom().
-type name() :: {name_server_name(), process_name()}.
-type process_name() :: term().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec start_name_server(name_server_name()) -> ok | {error, Reason} when
      Reason :: already_present | {already_started, pid()}.
start_name_server(Name) when is_atom(Name) ->
    local_sup:start_name_server(Name);
start_name_server(Name) -> error(badarg, [Name]).

-spec stop_name_server(name_server_name()) -> ok | {error, Reason} when
      Reason :: not_found.
stop_name_server(Name) when is_atom(Name) ->
    local_sup:stop_name_server(Name);
stop_name_server(Name) -> error(badarg, [Name]).


-spec which_name_servers() -> [name_server_name()].
which_name_servers() ->
    local_sup:which_name_servers().

-spec register_name(name(), pid()) -> yes | no.
register_name({Server, Name}, Pid) when is_atom(Server), is_pid(Pid) ->
    local_name_server:register_name(Server, Name, Pid);
register_name(Name, Pid) -> error(badarg, [Name, Pid]).

-spec unregister_name(name()) -> ok.
unregister_name({Server, Name}) when is_atom(Server) ->
    local_name_server:unregister_name(Server, Name);
unregister_name(Name) -> error(badarg, [Name]).

-spec whereis_name(name()) -> pid() | undefined.
whereis_name({Server, Name}) when is_atom(Server) ->
    local_name_server:whereis_name(Server, Name);
whereis_name(Name) -> error(badarg, [Name]).

-spec send(name(), term()) -> pid().
send(Name, Msg) ->
    case whereis_name(Name) of
        undefined -> error({badarg, {Name, Msg}});
        Pid       -> _ = Pid ! Msg, Pid
    end.

-spec make_name_server_child_spec(name_server_name()) -> supervisor:child_spec().
make_name_server_child_spec(Name) when is_atom(Name) ->
    {Name, {local_name_server, start_link, [Name]}, permanent, 5000, worker, [local_name_server]};
make_name_server_child_spec(Name) -> error(badarg, [Name]).
