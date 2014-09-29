local
=====

A Local Name Registration Facility.

The library provides the following features:
- Registration of locally scoped names
  - `gen_server` (and other OTP behaviours) can use the registration facility through `{via, local, local:name()}` formatted name
- Can be embedded in another aplication

QuickStart
----------
```bash
# clone
$ git clone git://github.com/sile/jsone.git
$ cd local

# compile
$ make compile

# run tests
$ make eunit

# dialyze
$ make dialyze

# Erlang shell
$ make start
1> local:start(sample_name_server).
ok
```

Usage Example
-------------
```erlang
%% Starts name server
> ok = local:start_name_server(sample_name_server).
> local:which_name_servers().
[sample_name_server]


%% Registers process name
> self().
<0.42.0>

> local:register_name({sample_name_server, hoge}, self()). % succeeded
yes

> local:register_name({sample_name_server, hoge}, self()). % failed: name collision
no

> local:whereis_name({sample_name_server, hoge}).
<0.42.0>

%% Registered name can be used as gen_server's via name
> gen_server:cast({via, local, {sample_name_server, hoge}}, hello).
ok

> flush().
Shell got {'$gen_cast',hello}
ok


%% Unregisters process name
> local:unregister_name({sample_name_server, hoge}).
ok

> local:whereis_name({sample_name_server, hoge}).
undefined


%% Stops name server
> ok = local:stop_name_server(sample_name_server).
> local:which_name_servers().
[]
```

Embedded Mode
-------------

Local can be embedded in your application.

```erlang
%% Embedding Example
-module(your_app_sup).

-behaviour(supervisor).

-export([init/1]). % 'supervisor' callback API

init([]) ->
    Child1 = local:name_server_child_spec(your_app_name_server_1),
    Child2 = local:name_server_child_spec(your_app_name_server_2),
    {ok, { {one_for_one, 5, 10}, [Child1, Child2]} }.
```

API
---
See (EDoc Document)[doc/local.md]
