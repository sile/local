%% @copyright 2013-2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(local_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).

-export([start_name_server/1]).
-export([stop_name_server/1]).
-export([which_name_servers/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts root supervisor
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_name_server(local:name_server_name()) -> ok | {error, Reason} when
      Reason :: already_present | {already_started, pid()}.
start_name_server(Name) ->
    Child = local:make_name_server_child_spec(Name),
    case supervisor:start_child(?MODULE, Child) of
        {ok, _Pid}                      -> ok;
        {error, already_present}        -> {error, already_present};
        {error, {already_started, Pid}} -> {error, {already_started, Pid}};
        Other                           -> error({unexpected_result, Other}, [Name])
    end.

-spec stop_name_server(local:name_server_name()) -> ok | {error, Reason} when
      Reason :: not_found.
stop_name_server(Name) ->
    case supervisor:terminate_child(?MODULE, Name) of
        {error, not_found} -> {error, not_found};
        ok                 -> ok = supervisor:delete_child(?MODULE, Name)
    end.

-spec which_name_servers() -> [local:name_server_name()].
which_name_servers() ->
    [Id || {Id, _, _, _} <- supervisor:which_children(?MODULE)].

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
