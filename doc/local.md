

# Module local #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) 2013-2014, Takeru Ohta <phjgt308@gmail.com>


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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#make_name_server_child_spec-1">make_name_server_child_spec/1</a></td><td></td></tr><tr><td valign="top"><a href="#register_name-2">register_name/2</a></td><td></td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_name_server-1">start_name_server/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop_name_server-1">stop_name_server/1</a></td><td></td></tr><tr><td valign="top"><a href="#unregister_name-1">unregister_name/1</a></td><td></td></tr><tr><td valign="top"><a href="#whereis_name-1">whereis_name/1</a></td><td></td></tr><tr><td valign="top"><a href="#which_name_servers-0">which_name_servers/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="make_name_server_child_spec-1"></a>

### make_name_server_child_spec/1 ###


<pre><code>
make_name_server_child_spec(Name::<a href="#type-name_server_name">name_server_name()</a>) -&gt; <a href="supervisor.md#type-child_spec">supervisor:child_spec()</a>
</code></pre>

<br></br>



<a name="register_name-2"></a>

### register_name/2 ###


<pre><code>
register_name(Name::<a href="#type-name">name()</a>, Pid::pid()) -&gt; yes | no
</code></pre>

<br></br>



<a name="send-2"></a>

### send/2 ###


<pre><code>
send(Name::<a href="#type-name">name()</a>, Msg::term()) -&gt; pid()
</code></pre>

<br></br>



<a name="start_name_server-1"></a>

### start_name_server/1 ###


<pre><code>
start_name_server(Name::<a href="#type-name_server_name">name_server_name()</a>) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = already_present | {already_started, pid()}</code></li></ul>


<a name="stop_name_server-1"></a>

### stop_name_server/1 ###


<pre><code>
stop_name_server(Name::<a href="#type-name_server_name">name_server_name()</a>) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = not_found</code></li></ul>


<a name="unregister_name-1"></a>

### unregister_name/1 ###


<pre><code>
unregister_name(Name::<a href="#type-name">name()</a>) -&gt; ok
</code></pre>

<br></br>



<a name="whereis_name-1"></a>

### whereis_name/1 ###


<pre><code>
whereis_name(Name::<a href="#type-name">name()</a>) -&gt; pid() | undefined
</code></pre>

<br></br>



<a name="which_name_servers-0"></a>

### which_name_servers/0 ###


<pre><code>
which_name_servers() -&gt; [<a href="#type-name_server_name">name_server_name()</a>]
</code></pre>

<br></br>



