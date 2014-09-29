

# Module local #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


A Local Name Registration Facility.
Copyright (c) 2014, Takeru Ohta <phjgt308@gmail.com>


<a name="types"></a>

## Data Types ##




### <a name="type-name">name()</a> ###



<pre><code>
name() = {<a href="#type-name_server_name">name_server_name()</a>, <a href="#type-process_name">process_name()</a>}
</code></pre>





### <a name="type-name_server_name">name_server_name()</a> ###



<pre><code>
name_server_name() = atom()
</code></pre>





### <a name="type-process_name">process_name()</a> ###



<pre><code>
process_name() = term()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#name_server_child_spec-1">name_server_child_spec/1</a></td><td>Equivalent to <a href="#name_server_child_spec-3"><tt>name_server_child_spec(Name, Name, 5000)</tt></a>.</td></tr><tr><td valign="top"><a href="#name_server_child_spec-3">name_server_child_spec/3</a></td><td>Returns the child spec for a local name server that is used in embedded mode.</td></tr><tr><td valign="top"><a href="#register_name-2">register_name/2</a></td><td>Locally assocates the name <code>Name</code> with a pid <code>Pid</code>.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Sends the message <code>Msg</code> to the pid locally registered as <code>Name</code></td></tr><tr><td valign="top"><a href="#start_name_server-1">start_name_server/1</a></td><td>Starts a name server process.</td></tr><tr><td valign="top"><a href="#stop_name_server-1">stop_name_server/1</a></td><td>Stops the name server <code>ServerName</code></td></tr><tr><td valign="top"><a href="#unregister_name-1">unregister_name/1</a></td><td>Removes the locally registered name <code>Name</code></td></tr><tr><td valign="top"><a href="#whereis_name-1">whereis_name/1</a></td><td>Returns the pid with the locally registered name <code>Name</code></td></tr><tr><td valign="top"><a href="#which_name_servers-0">which_name_servers/0</a></td><td>Returns a list of running name server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="name_server_child_spec-1"></a>

### name_server_child_spec/1 ###


<pre><code>
name_server_child_spec(Name::<a href="#type-name_server_name">name_server_name()</a>) -&gt; <a href="supervisor.md#type-child_spec">supervisor:child_spec()</a>
</code></pre>

<br></br>


Equivalent to [`name_server_child_spec(Name, Name, 5000)`](#name_server_child_spec-3).
<a name="name_server_child_spec-3"></a>

### name_server_child_spec/3 ###


<pre><code>
name_server_child_spec(ChildId, ServerName, Shutdown) -&gt; ChildSpec
</code></pre>

<ul class="definitions"><li><code>ChildId = <a href="supervisor.md#type-child_id">supervisor:child_id()</a></code></li><li><code>ServerName = <a href="#type-name_server_name">name_server_name()</a></code></li><li><code>Shutdown = <a href="supervisor.md#type-shutdown">supervisor:shutdown()</a></code></li><li><code>ChildSpec = <a href="supervisor.md#type-child_spec">supervisor:child_spec()</a></code></li></ul>


Returns the child spec for a local name server that is used in embedded mode.


To embed a local name server in your application, you can add the child spec `ChildSpec` to your supervision tree.
<a name="register_name-2"></a>

### register_name/2 ###


<pre><code>
register_name(Name::<a href="#type-name">name()</a>, Pid::pid()) -&gt; yes | no
</code></pre>

<br></br>



Locally assocates the name `Name` with a pid `Pid`.



Let `NameServer` is `element(1, Name)`, the registered name is limited to the name server `NameServer` scope.



Assumes that the name server is already started, crashes otherwise.


The function returns `yes` if successful, `no` if it failes.
For example, `no` is returned if an attempt is made to register an already registered process or
to register a process with a name that is already in use.
<a name="send-2"></a>

### send/2 ###


<pre><code>
send(Name::<a href="#type-name">name()</a>, Msg::term()) -&gt; pid()
</code></pre>

<br></br>



Sends the message `Msg` to the pid locally registered as `Name`


Failure: If `Name` is not a locally registered name, the calling function will exit with reason `{badarg, {Name, Msg}}`
<a name="start_name_server-1"></a>

### start_name_server/1 ###


<pre><code>
start_name_server(ServerName::<a href="#type-name_server_name">name_server_name()</a>) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = already_present</code></li></ul>


Starts a name server process



The process is registered locally as `ServerName` using `register/2`.



If the process is successfully started the function returns `ok`.


If there already exists a process with the specified `ServerName` the function returnes `{error, already_present}`.
<a name="stop_name_server-1"></a>

### stop_name_server/1 ###


<pre><code>
stop_name_server(ServerName::<a href="#type-name_server_name">name_server_name()</a>) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = not_found</code></li></ul>


Stops the name server `ServerName`


If successful, the function returns `ok`.
If the name server identified by `ServerName` does not exist, the function returns `{error, not_found}`.
<a name="unregister_name-1"></a>

### unregister_name/1 ###


<pre><code>
unregister_name(Name::<a href="#type-name">name()</a>) -&gt; ok
</code></pre>

<br></br>


Removes the locally registered name `Name`
<a name="whereis_name-1"></a>

### whereis_name/1 ###


<pre><code>
whereis_name(Name::<a href="#type-name">name()</a>) -&gt; pid() | undefined
</code></pre>

<br></br>



Returns the pid with the locally registered name `Name`


Returns `undefined` if the name is not locally registered.
<a name="which_name_servers-0"></a>

### which_name_servers/0 ###


<pre><code>
which_name_servers() -&gt; [<a href="#type-name_server_name">name_server_name()</a>]
</code></pre>

<br></br>


Returns a list of running name server
