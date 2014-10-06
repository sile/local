%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
-module(local_tests).

-on_load(init/0).
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(NS, test_name_server). % Name Server
-define(NAME(Name), {?NS, Name}).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
start_and_stop_name_server_test_() ->
    [
     {"start and stop",
      fun () ->
              ?assertEqual([],    local:which_name_servers()),
              ?assertEqual(ok,    local:start_name_server(?NS)),
              ?assertEqual([?NS], local:which_name_servers()),
              ?assertEqual(ok,    local:stop_name_server(?NS)),
              ?assertEqual([],    local:which_name_servers())
      end},
     {"duplicated start",
      fun () ->
              ?assertEqual(ok, local:start_name_server(?NS)),
              ?assertMatch({error, already_present}, local:start_name_server(?NS)),
              ?assertEqual(ok, local:stop_name_server(?NS))
      end},
     {"delete not-started server",
      fun () ->
              ?assertEqual({error, not_found}, local:stop_name_server(?NS))
      end}
    ].

register_and_unregister_test_() ->
    {foreach,
     fun ()  -> ok = local:start_name_server(?NS) end,
     fun (_) -> ok = local:stop_name_server(?NS) end,
     [
      {"register process name",
       fun () ->
               ?assertEqual(yes, local:register_name(?NAME(hoge), self())),
               ?assertEqual(self(), local:whereis_name(?NAME(hoge)))
       end},
      {"duplicated registration",
       fun () ->
               yes = local:register_name(?NAME(hoge), self()),
               ?assertEqual(no, local:register_name(?NAME(hoge), self())),
               ?assertEqual(no, local:register_name(?NAME(fuga), self()))
       end},
      {"unregister process name",
       fun () ->
               yes = local:register_name(?NAME(hoge), self()),

               ?assertEqual(ok, local:unregister_name(?NAME(hoge))),
               ?assertEqual(yes, local:register_name(?NAME(hoge), self())),

               ?assertEqual(ok, local:unregister_name(?NAME(hoge))),
               ?assertEqual(yes, local:register_name(?NAME(fuga), self()))
       end},
      {"unregister not-registered name",
       fun () ->
               ?assertEqual(ok, local:unregister_name(?NAME(hoge)))
       end}
     ]}.

whereis_test_() ->
    {foreach,
     fun ()  -> ok = local:start_name_server(?NS) end,
     fun (_) -> ok = local:stop_name_server(?NS) end,
     [
      {"resolve name",
       fun () ->
               yes = local:register_name(?NAME(hoge), self()),
               ?assertEqual(self(), local:whereis_name(?NAME(hoge)))
       end},
      {"resolve not-registered name",
       fun () ->
               ?assertEqual(undefined, local:whereis_name(?NAME(hoge)))
       end},
      {"resolve unregistered name",
       fun () ->
               yes = local:register_name(?NAME(hoge), self()),
               ok = local:unregister_name(?NAME(hoge)),
               ?assertEqual(undefined, local:whereis_name(?NAME(hoge)))
       end}
     ]}.

send_test_() ->
    {foreach,
     fun ()  -> ok = local:start_name_server(?NS) end,
     fun (_) -> ok = local:stop_name_server(?NS) end,
     [
      {"send message to registered process",
       fun () ->
               yes = local:register_name(?NAME(hoge), self()),
               ?assertEqual(self(), local:send(?NAME(hoge), hello)),
               receive
                   hello -> ?assert(true)
               after 10 -> ?assert(timeout)
               end
       end},
      {"send message to not-registered process",
       fun () ->
               ?assertError({badarg, _}, local:send(?NAME(hoge), hello))
       end}
     ]}.

process_down_test_() ->
    {foreach,
     fun ()  -> ok = local:start_name_server(?NS) end,
     fun (_) -> ok = local:stop_name_server(?NS) end,
     [
      {"register downed process",
       fun () ->
               process_flag(trap_exit, true),

               Pid = spawn_link(fun () -> ok end),
               receive {'EXIT', Pid, _} -> ok end,

               yes = local:register_name(?NAME(hoge), Pid),
               ?assertEqual(undefined, local:whereis_name(?NAME(hoge))),

               yes = local:register_name(?NAME(hoge), self()),
               ?assertEqual(self(), local:whereis_name(?NAME(hoge)))
       end},
      {"register then process down",
       fun () ->
               process_flag(trap_exit, true),

               Pid = spawn_link(timer, sleep, [infinity]),

               yes = local:register_name(?NAME(hoge), Pid),
               ?assertEqual(Pid, local:whereis_name(?NAME(hoge))),

               exit(Pid, shutdown),
               receive {'EXIT', Pid, _} -> ok end,

               ?assertEqual(undefined, local:whereis_name(?NAME(hoge)))
       end},
      {"name server down",
       fun () ->
               process_flag(trap_exit, true),

               yes = local:register_name(?NAME(hoge), self()),
               ?assertEqual(self(), local:whereis_name(?NAME(hoge))),

               Ns = whereis(?NS),
               exit(Ns, kill),

               receive {'EXIT', Ns, Reason} -> ?assertEqual(killed, Reason) after 50 -> ?assert(timeout) end,

               ?assertEqual(undefined, local:whereis_name(?NAME(hoge))),

               timer:sleep(20), % wait restarting

               yes = local:register_name(?NAME(hoge), self()),
               ?assertEqual(self(), local:whereis_name(?NAME(hoge)))
       end},
      {"other linked process down",
       fun () ->
               Ns = whereis(?NS),
               Monitor = monitor(process, Ns),
               spawn(fun () ->
                             link(Ns),
                             exit(abort)
                     end),
               receive
                   {'DOWN', Monitor, _, Ns, Reason} -> ?assertEqual(abort, Reason)
               after 50 -> ?assert(timeout)
               end,

               timer:sleep(20), % wait restarting

               yes = local:register_name(?NAME(hoge), self()),
               ?assertEqual(self(), local:whereis_name(?NAME(hoge)))
       end}
     ]}.

gen_server_test_() ->
    {foreach,
     fun ()  -> ok = local:start_name_server(?NS) end,
     fun (_) -> ok = local:stop_name_server(?NS) end,
     [
      {"gen_server via name",
       fun() ->
               Result = echo_gen_server:start_link({via, local, ?NAME(hoge)}),
               ?assertMatch({ok, _}, Result),

               ?assertEqual({echo, hello}, echo_gen_server:call({via, local, ?NAME(hoge)}, hello))
       end}
     ]}.

which_processes_test_() ->
    {foreach,
     fun ()  -> ok = local:start_name_server(?NS) end,
     fun (_) -> ok = local:stop_name_server(?NS) end,
     [
      {"get a list of registered process",
       fun () ->
               ?assertEqual([], local:which_processes(?NS)),

               yes = local:register_name(?NAME(hoge), self()),
               ?assertEqual([{hoge, self()}], local:which_processes(?NS)),

               ok = local:unregister_name(?NAME(hoge)),
               ?assertEqual([], local:which_processes(?NS))
       end},
      {"get a filtered list of registered process",
       fun () ->
               yes = local:register_name(?NAME(hoge), self()),

               ?assertEqual([],               local:which_processes(?NS, fuga)),
               ?assertEqual([{hoge, self()}], local:which_processes(?NS, hoge)),
               ?assertEqual([{hoge, self()}], local:which_processes(?NS, '_'))
       end}
     ]}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
init() ->
    {ok, _} = application:ensure_all_started(local),
    _ = error_logger:tty(false),
    ok.
