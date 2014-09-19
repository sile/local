%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
-module(local_lib_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
link_and_flush_test_() ->
    [
     {"normal unlink",
      fun () ->
              Pid = spawn_link(timer, sleep, [infinity]),
              true = link(Pid),
              true = local_lib:unlink_and_flush(Pid),
              exit(Pid, kill),
              ?assert(true)
      end},
     {"flush unlink",
      fun () ->
              process_flag(trap_exit, true),

              Pid = spawn_link(timer, sleep, [infinity]),
              true = link(Pid),
              exit(Pid, kill),
              timer:sleep(10),
              true = local_lib:unlink_and_flush(Pid),
              receive
                  _ -> ?assert(false)
              after 10 -> ?assert(true)
              end
      end},
     {"flush unlink",
      fun () ->
              process_flag(trap_exit, true),

              Pid = spawn_link(timer, sleep, [infinity]),
              true = link(Pid),
              exit(Pid, kill),
              timer:sleep(10),
              true = unlink(Pid),
              receive
                  {'EXIT', Pid, killed} -> ?assert(true)
              after 10 -> ?assert(false)
              end
      end}
    ].
