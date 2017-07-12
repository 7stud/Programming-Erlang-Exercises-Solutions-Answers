Joe Armstrong's **ezwebrame** code no longer works.  I asked for help on the erlang-questions forum with some of the errors I was getting, and Joe himself answered and essentially said, "It no longer works.  Tough luck." Oh, well.

Instead, I decided to try [gun](https://github.com/ninenines/gun) as an http client, which has builtin websockets support and is maintained by the same person who maintains cowboy, to communicate with a cowboy server using websockets.  gun is for `erlang 18+`, though, so if you need to install a more recent version of erlang, you can use `kerl` or `evm` to install multiple versions of erlang on your system and switch between them as needed.

To setup cowboy, I followed the cowboy User Guide's [Getting Started](https://ninenines.eu/docs/en/cowboy/2.0/guide/getting_started/) section. Once that was setup correctly, I was at the following prompt:

`(hello_erlang@127.0.0.1)1>`

To setup gun, I opened up another terminal window, and the [gun docs](https://github.com/ninenines/gun/blob/master/doc/src/guide/start.asciidoc) say you can use something called `erlang.mk` and add gun as a dependency to your application.  According to the [Erlang.mk docs](https://erlang.mk/guide/getting_started.html#_getting_started_with_otp_releases), if you create something called _a release_, then you can use the command `make run` (or `gmake run`) to compile and execute your code. After reading the Erlang.mk docs, it wasn't clear to me how to run _an application_ or a _library_, so I opted to create _a release_.  First, I created a directory for my release, then I downloaded erlang.mk:

```
~/erlang_programs$ mkdir my_gun && cd $_

~/erlang_programs/my_gun$ curl -O "https:/erlang.mk/erlang.mk"   
```

Then I used the following erlang.mk [command](https://erlang.mk/guide/getting_started.html#_getting_started_with_otp_releases) to create a release:

```
~/erlang_programs/my_gun$ gmake -f erlang.mk bootstrap-lib bootstrap-rel
git clone https://github.com/ninenines/erlang.mk .erlang.mk.build
Cloning into '.erlang.mk.build'...
remote: Counting objects: 7116, done.
remote: Compressing objects: 100% (10/10), done.
remote: Total 7116 (delta 2), reused 6 (delta 1), pack-reused 7105
Receiving objects: 100% (7116/7116), 3.29 MiB | 2.02 MiB/s, done.
Resolving deltas: 100% (4504/4504), done.
if [ -f build.config ]; then cp build.config .erlang.mk.build; fi
cd .erlang.mk.build && gmake
gmake[1]: Entering directory '/Users/7stud/erlang_programs/my_gun/.erlang.mk.build'
export LC_COLLATE=C; \
awk 'FNR==1 && NR!=1{print ""}1' core/core.mk index/*.mk core/index.mk core/deps.mk plugins/protobuffs.mk core/erlc.mk core/docs.mk core/rel.mk core/test.mk core/compat.mk plugins/asciidoc.mk plugins/bootstrap.mk plugins/c_src.mk plugins/ci.mk plugins/ct.mk plugins/dialyzer.mk plugins/edoc.mk plugins/erlydtl.mk plugins/escript.mk plugins/eunit.mk plugins/proper.mk plugins/relx.mk plugins/shell.mk plugins/syntastic.mk plugins/triq.mk plugins/xref.mk plugins/cover.mk plugins/sfx.mk core/plugins.mk core/deps-tools.mk \
	| sed 's/^ERLANG_MK_VERSION =.*/ERLANG_MK_VERSION = 2017.07.06-1-gff27159/' \
	| sed 's:^ERLANG_MK_WITHOUT =.*:ERLANG_MK_WITHOUT = :' > erlang.mk
gmake[1]: Leaving directory '/Users/7stud/erlang_programs/my_gun/.erlang.mk.build'
cp .erlang.mk.build/erlang.mk ./erlang.mk
rm -rf .erlang.mk.build

~/erlang_programs/my_gun$ ls
Makefile	erlang.mk	rel		relx.config	src
```
As you can see, the erlang.mk command that I used created some files and directories.  Next, I edited the Makefile to add gun as a dependency:

```make
PROJECT = my_gun
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = gun

include erlang.mk
```

Then I compiled and executed the release:

```
~/erlang_programs/my_gun$ gmake run
 DEP    gun
gmake[1]: Entering directory '/Users/7stud/erlang_programs/my_gun/deps/gun'
 DEP    cowlib
 DEP    ranch
gmake[2]: Entering directory '/Users/7stud/erlang_programs/my_gun/deps/cowlib'
 DEPEND cowlib.d
 ERLC   cow_base64url.erl cow_cookie.erl cow_date.erl cow_hpack.erl cow_http.erl cow_http2.erl cow_http_hd.erl cow_http_te.erl cow_mimetypes.erl cow_multipart.erl cow_qs.erl cow_spdy.erl cow_sse.erl cow_uri.erl cow_ws.erl
 APP    cowlib
gmake[2]: Leaving directory '/Users/7stud/erlang_programs/my_gun/deps/cowlib'
gmake[2]: Entering directory '/Users/7stud/erlang_programs/my_gun/deps/ranch'
 DEPEND ranch.d
 ERLC   ranch.erl ranch_acceptor.erl ranch_acceptors_sup.erl ranch_app.erl ranch_conns_sup.erl ranch_listener_sup.erl ranch_protocol.erl ranch_server.erl ranch_ssl.erl ranch_sup.erl ranch_tcp.erl ranch_transport.erl
 APP    ranch
gmake[2]: Leaving directory '/Users/7stud/erlang_programs/my_gun/deps/ranch'
 DEPEND gun.d
 ERLC   gun.erl gun_app.erl gun_content_handler.erl gun_data.erl gun_http.erl gun_http2.erl gun_spdy.erl gun_sse.erl gun_sup.erl gun_ws.erl gun_ws_handler.erl
 APP    gun
 GEN    rebar.config
gmake[1]: Leaving directory '/Users/7stud/erlang_programs/my_gun/deps/gun'
 DEPEND my_gun.d
 APP    my_gun
 GEN    /Users/7stud/erlang_programs/my_gun/.erlang.mk/relx
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          /Users/7stud/erlang_programs/my_gun/ebin
          /Users/7stud/erlang_programs/my_gun/deps
          /Users/7stud/.evm/erlang_versions/otp_src_19.2/lib/erlang/lib
          /Users/7stud/erlang_programs/my_gun/apps
===> Resolved my_gun_release-1
===> rendering builtin_hook_status hook to "/Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/bin/hooks/builtin/status"
===> Including Erts from /Users/7stud/.evm/erlang_versions/otp_src_19.2/lib/erlang
===> release successfully created!
===> tarball /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/my_gun_release-1.tar.gz successfully created!
Exec: /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/erts-8.2/bin/erlexec -boot /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/releases/1/my_gun_release -mode embedded -boot_var ERTS_LIB_DIR /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/lib -config /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/releases/1/sys.config -args_file /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/releases/1/vm.args -pa -- console
Root: /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release
/Users/7stud/erlang_programs/my_gun/_rel/my_gun_release
heart_beat_kill_pid = 41598
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]


=PROGRESS REPORT==== 10-Jul-2017::21:30:00 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.352.0>},
                       {id,alarm_handler},
                       {mfargs,{alarm_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2017::21:30:00 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.351.0>},
                       {id,sasl_safe_sup},
                       {mfargs,
                           {supervisor,start_link,
                               [{local,sasl_safe_sup},sasl,safe]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 10-Jul-2017::21:30:00 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.353.0>},
                       {id,release_handler},
                       {mfargs,{release_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2017::21:30:00 ===
         application: sasl
          started_at: 'my_gun@127.0.0.1'

=PROGRESS REPORT==== 10-Jul-2017::21:30:00 ===
          supervisor: {local,runtime_tools_sup}
             started: [{pid,<0.359.0>},
                       {id,ttb_autostart},
                       {mfargs,{ttb_autostart,start_link,[]}},
                       {restart_type,temporary},
                       {shutdown,3000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2017::21:30:00 ===
         application: runtime_tools
          started_at: 'my_gun@127.0.0.1'
Eshell V8.2  (abort with ^G)

(my_gun@127.0.0.1)1> 
```
At the erlang shell prompt, `(my_gun@127.0.0.1)1>`, you can now call gun functions that send requests to cowboy.  However, typing a lot of code in the shell is a pain, which highlights another advantage of using a release: you can create a function containing the gun commands that you want to execute, and call that function from the erlang shell.   When you issue the command `gmake run`, all the code in the `src` directory of your release will be compiled.  So, I created the following file in the `src` directory of my release (I hit `Ctrl+CC` to get out of the erlang shell):

```erlang
-module(my).
-compile(export_all).

get() ->
    {ok, _} = application:ensure_all_started(gun),
    {ok, ConnPid} = gun:open("localhost", 8080),
    {ok, _Protocol} = gun:await_up(ConnPid),

    StreamRef = gun:get(ConnPid, "/"),

    case gun:await(ConnPid, StreamRef) of
        {response, fin, _Status, _Headers} ->
            no_data;
        {response, nofin, _Status, _Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            io:format("~s~n", [Body])
    end.
```

I cobbled that code together from the following files in the [gun docs](https://github.com/ninenines/gun/tree/master/doc/src/guide):
```
introduction.asciidoc
start.asciidoc
http.asciidoc
```

`my:get()` sends a GET request--nothing to do with websockets yet--to the cowboy server that I setup to listen on port 8080; and the case statement handles the response.  Note that `my:get()` sends the GET request to the same route, "/", that the cowboy Getting Started guide already setup a route and handler for.

Here's the result of compiling and running the release:

```
~/erlang_programs/my_gun$ gmake run
gmake[1]: Entering directory '/Users/7stud/erlang_programs/my_gun/deps/gun'
gmake[2]: Entering directory '/Users/7stud/erlang_programs/my_gun/deps/cowlib'
gmake[2]: Leaving directory '/Users/7stud/erlang_programs/my_gun/deps/cowlib'
gmake[2]: Entering directory '/Users/7stud/erlang_programs/my_gun/deps/ranch'
gmake[2]: Leaving directory '/Users/7stud/erlang_programs/my_gun/deps/ranch'
 GEN    rebar.config
gmake[1]: Leaving directory '/Users/7stud/erlang_programs/my_gun/deps/gun'
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          /Users/7stud/erlang_programs/my_gun/ebin
          /Users/7stud/erlang_programs/my_gun/deps
          /Users/7stud/.evm/erlang_versions/otp_src_19.2/lib/erlang/lib
          /Users/7stud/erlang_programs/my_gun/apps
          /Users/7stud/erlang_programs/my_gun/_rel
===> Resolved my_gun_release-1
===> rendering builtin_hook_status hook to "/Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/bin/hooks/builtin/status"
===> Including Erts from /Users/7stud/.evm/erlang_versions/otp_src_19.2/lib/erlang
===> release successfully created!
===> tarball /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/my_gun_release-1.tar.gz successfully created!
Exec: /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/erts-8.2/bin/erlexec -boot /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/releases/1/my_gun_release -mode embedded -boot_var ERTS_LIB_DIR /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/lib -config /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/releases/1/sys.config -args_file /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/releases/1/vm.args -pa -- console
Root: /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release
/Users/7stud/erlang_programs/my_gun/_rel/my_gun_release
heart_beat_kill_pid = 42813
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]


=PROGRESS REPORT==== 10-Jul-2017::21:54:25 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.353.0>},
                       {id,alarm_handler},
                       {mfargs,{alarm_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2017::21:54:25 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.352.0>},
                       {id,sasl_safe_sup},
                       {mfargs,
                           {supervisor,start_link,
                               [{local,sasl_safe_sup},sasl,safe]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 10-Jul-2017::21:54:25 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.354.0>},
                       {id,release_handler},
                       {mfargs,{release_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2017::21:54:25 ===
         application: sasl
          started_at: 'my_gun@127.0.0.1'

=PROGRESS REPORT==== 10-Jul-2017::21:54:25 ===
          supervisor: {local,runtime_tools_sup}
             started: [{pid,<0.360.0>},
                       {id,ttb_autostart},
                       {mfargs,{ttb_autostart,start_link,[]}},
                       {restart_type,temporary},
                       {shutdown,3000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2017::21:54:25 ===
         application: runtime_tools
          started_at: 'my_gun@127.0.0.1
Eshell V8.2  (abort with ^G)
(my_gun@127.0.0.1)1>
```

At the prompt, I executed `my:get()`:

```
(my_gun@127.0.0.1)1> my:get().

=PROGRESS REPORT==== 10-Jul-2017::21:54:27 ===
          supervisor: {local,inet_gethost_native_sup}
             started: [{pid,<0.367.0>},{mfa,{inet_gethost_native,init,[[]]}}]

=PROGRESS REPORT==== 10-Jul-2017::21:54:27 ===
          supervisor: {local,kernel_safe_sup}
             started: [{pid,<0.366.0>},
                       {id,inet_gethost_native_sup},
                       {mfargs,{inet_gethost_native,start_link,[]}},
                       {restart_type,temporary},
                       {shutdown,1000},
                       {child_type,worker}]
Hello Erlang!
ok
(my_gun@127.0.0.1)2> 
```
Okay, on to websockets.  Once you establish a `gun <---> cowboy`connection, in order to use websockets you need to send a special upgrade request to a cowboy route whose handler _upgrades_ the connection to a websocket.  As a result, you need to add a new route to cowboy, say, "/please_upgrade_to_websocket" and you need to create a handler for that route.  A handler is actually a module, and inside the module you are required to define an `init/2` function.  You can read about upgrade requests in the gun docs [here](https://github.com/ninenines/gun/blob/master/doc/src/guide/websocket.asciidoc).  You can read about cowboy handlers in general [here](https://ninenines.eu/docs/en/cowboy/2.0/guide/handlers/) and websocket handlers in particular [here](https://ninenines.eu/docs/en/cowboy/2.0/guide/ws_handlers/).

Switch to the terminal window where cowboy is running--the window should be displaying the prompt:

`(hello_erlang@127.0.0.1)1>`

You can kill the cowboy server with `Ctrl+CC`. Here's the new cowboy route:

***hello_erlang/src/hello_erlang_app.erl***
```erlang
-module(hello_erlang_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
               {"/", hello_handler, []},
               {"/please_upgrade_to_websocket", myws_handler, []} %<**** NEW ROUTE ****
        ]}
    ]),

    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch} }
    ),

    hello_erlang_sup:start_link().

stop(_State) ->
	ok.

stop(_State) ->
	ok.
```
Here's the new route's handler:

***hello_erlang/src/myws_handler***
```erlang
-module(myws_handler).
-compile(export_all).

init(Req, State) ->
    {cowboy_websocket, Req, State}.  %Perform websocket setup

websocket_handle({text, Msg}, State) ->  %Automatically called when data arrives
    {
     reply, 
     {text, io_lib:format("Server received: ~s", [Msg]) },
     State
    };
websocket_handle(_Other, State) ->  %Ignore
    {ok, State}.
```

The handler just prepends the text "Server received: " to whatever text arrives through the websocket and sends the new text back.

Then, I added the following code to gun:

***~/erlang_programs/my_gun/src/my.erl***
```erlang
-module(my).
%-compile(export_all).
-export([get/0, ws/0]).

get() ->
    ...

%******** NEW CODE BELOW ***********
ws() ->
    {ok, _} = application:ensure_all_started(gun),
    {ok, ConnPid} = gun:open("localhost", 8080),
    {ok, _Protocol} = gun:await_up(ConnPid),

    gun:ws_upgrade(ConnPid, "/please_upgrade_to_websocket"),

    receive
        {gun_ws_upgrade, ConnPid, ok, Headers} ->
            upgrade_success(ConnPid, Headers);
        {gun_response, ConnPid, _, _, Status, Headers} ->
            exit({ws_upgrade_failed, Status, Headers});
        {gun_error, _ConnPid, _StreamRef, Reason} ->
            exit({ws_upgrade_failed, Reason})
    %% More clauses here as needed.
    after 1000 ->
        exit(timeout)
    end,

    gun:shutdown(ConnPid).

upgrade_success(ConnPid, Headers) ->
    io:format("Upgraded ~w. Success!~nHeaders:~n~p~n", 
              [ConnPid, Headers]).
```

Switch to the cowboy terminal window and restart cowboy:
```
~/erlang_programs/cowboy_apps/hello_erlang$ gmake run
gmake[1]: Entering directory '/Users/7stud/erlang_programs/cowboy_apps/hello_erlang/deps/cowboy'
gmake[2]: Entering directory '/Users/7stud/erlang_programs/cowboy_apps/hello_erlang/deps/cowlib'
gmake[2]: Leaving directory '/Users/7stud/erlang_programs/cowboy_apps/hello_erlang/deps/cowlib'
gmake[2]: Entering directory '/Users/7stud/erlang_programs/cowboy_apps/hello_erlang/deps/ranch'
gmake[2]: Leaving directory '/Users/7stud/erlang_programs/cowboy_apps/hello_erlang/deps/ranch'
 GEN    rebar.config
gmake[1]: Leaving directory '/Users/7stud/erlang_programs/cowboy_apps/hello_erlang/deps/cowboy'
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          /Users/7stud/erlang_programs/cowboy_apps/hello_erlang/ebin
          /Users/7stud/erlang_programs/cowboy_apps/hello_erlang/deps
          /Users/7stud/.evm/erlang_versions/otp_src_19.2/lib/erlang/lib
          /Users/7stud/erlang_programs/cowboy_apps/hello_erlang/apps
          /Users/7stud/erlang_programs/cowboy_apps/hello_erlang/_rel
===> Resolved hello_erlang_release-1
===> rendering builtin_hook_status hook to "/Users/7stud/erlang_programs/cowboy_apps/hello_erlang/_rel/hello_erlang_release/bin/hooks/builtin/status"
===> Including Erts from /Users/7stud/.evm/erlang_versions/otp_src_19.2/lib/erlang
===> release successfully created!
===> tarball /Users/7stud/erlang_programs/cowboy_apps/hello_erlang/_rel/hello_erlang_release/hello_erlang_release-1.tar.gz successfully created!
Exec: /Users/7stud/erlang_programs/cowboy_apps/hello_erlang/_rel/hello_erlang_release/erts-8.2/bin/erlexec -boot /Users/7stud/erlang_programs/cowboy_apps/hello_erlang/_rel/hello_erlang_release/releases/1/hello_erlang_release -mode embedded -boot_var ERTS_LIB_DIR /Users/7stud/erlang_programs/cowboy_apps/hello_erlang/_rel/hello_erlang_release/lib -config /Users/7stud/erlang_programs/cowboy_apps/hello_erlang/_rel/hello_erlang_release/releases/1/sys.config -args_file /Users/7stud/erlang_programs/cowboy_apps/hello_erlang/_rel/hello_erlang_release/releases/1/vm.args -pa -- console
Root: /Users/7stud/erlang_programs/cowboy_apps/hello_erlang/_rel/hello_erlang_release
/Users/7stud/erlang_programs/cowboy_apps/hello_erlang/_rel/hello_erlang_release
heart_beat_kill_pid = 44891
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]


=PROGRESS REPORT==== 10-Jul-2017::22:40:08 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.384.0>},
                       {id,alarm_handler},
                       {mfargs,{alarm_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2017::22:40:08 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.383.0>},
                       {id,sasl_safe_sup},
                       {mfargs,
                           {supervisor,start_link,
                               [{local,sasl_safe_sup},sasl,safe]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 10-Jul-2017::22:40:08 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.385.0>},
                       {id,release_handler},
                       {mfargs,{release_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2017::22:40:08 ===
         application: sasl
          started_at: 'hello_erlang@127.0.0.1'

=PROGRESS REPORT==== 10-Jul-2017::22:40:08 ===
          supervisor: {local,runtime_tools_sup}
             started: [{pid,<0.391.0>},
                       {id,ttb_autostart},
                       {mfargs,{ttb_autostart,start_link,[]}},
                       {restart_type,temporary},
                       {shutdown,3000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2017::22:40:08 ===
         application: runtime_tools
          started_at: 'hello_erlang@127.0.0.1'
Eshell V8.2  (abort with ^G)
(hello_erlang@127.0.0.1)1> 
```
Switch to the gun terminal window and restart gun:
```
~/erlang_programs/my_gun$ gmake run
gmake[1]: Entering directory '/Users/7stud/erlang_programs/my_gun/deps/gun'
gmake[2]: Entering directory '/Users/7stud/erlang_programs/my_gun/deps/cowlib'
gmake[2]: Leaving directory '/Users/7stud/erlang_programs/my_gun/deps/cowlib'
gmake[2]: Entering directory '/Users/7stud/erlang_programs/my_gun/deps/ranch'
gmake[2]: Leaving directory '/Users/7stud/erlang_programs/my_gun/deps/ranch'
 GEN    rebar.config
gmake[1]: Leaving directory '/Users/7stud/erlang_programs/my_gun/deps/gun'
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          /Users/7stud/erlang_programs/my_gun/ebin
          /Users/7stud/erlang_programs/my_gun/deps
          /Users/7stud/.evm/erlang_versions/otp_src_19.2/lib/erlang/lib
          /Users/7stud/erlang_programs/my_gun/apps
          /Users/7stud/erlang_programs/my_gun/_rel
===> Resolved my_gun_release-1
===> rendering builtin_hook_status hook to "/Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/bin/hooks/builtin/status"
===> Including Erts from /Users/7stud/.evm/erlang_versions/otp_src_19.2/lib/erlang
===> release successfully created!
===> tarball /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/my_gun_release-1.tar.gz successfully created!
Exec: /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/erts-8.2/bin/erlexec -boot /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/releases/1/my_gun_release -mode embedded -boot_var ERTS_LIB_DIR /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/lib -config /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/releases/1/sys.config -args_file /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/releases/1/vm.args -pa -- console
Root: /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release
/Users/7stud/erlang_programs/my_gun/_rel/my_gun_release
heart_beat_kill_pid = 45280
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]


=PROGRESS REPORT==== 10-Jul-2017::22:49:12 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.353.0>},
                       {id,alarm_handler},
                       {mfargs,{alarm_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2017::22:49:12 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.352.0>},
                       {id,sasl_safe_sup},
                       {mfargs,
                           {supervisor,start_link,
                               [{local,sasl_safe_sup},sasl,safe]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 10-Jul-2017::22:49:12 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.354.0>},
                       {id,release_handler},
                       {mfargs,{release_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2017::22:49:12 ===
         application: sasl
          started_at: 'my_gun@127.0.0.1'

=PROGRESS REPORT==== 10-Jul-2017::22:49:12 ===
          supervisor: {local,runtime_tools_sup}
             started: [{pid,<0.360.0>},
                       {id,ttb_autostart},
                       {mfargs,{ttb_autostart,start_link,[]}},
                       {restart_type,temporary},
                       {shutdown,3000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2017::22:49:12 ===
         application: runtime_tools
          started_at: 'my_gun@127.0.0.1'
Eshell V8.2  (abort with ^G)
(my_gun@127.0.0.1)1> 
```
Now execute my:ws() in the gun terminal window:
```
(my_gun@127.0.0.1)1> my:ws().

=PROGRESS REPORT==== 10-Jul-2017::22:55:43 ===
          supervisor: {local,inet_gethost_native_sup}
             started: [{pid,<0.367.0>},{mfa,{inet_gethost_native,init,[[]]}}]

=PROGRESS REPORT==== 10-Jul-2017::22:55:43 ===
          supervisor: {local,kernel_safe_sup}
             started: [{pid,<0.366.0>},
                       {id,inet_gethost_native_sup},
                       {mfargs,{inet_gethost_native,start_link,[]}},
                       {restart_type,temporary},
                       {shutdown,1000},
                       {child_type,worker}]
Upgraded <0.365.0>. Success!
Headers:
[{<<"connection">>,<<"Upgrade">>},
 {<<"date">>,<<"Tue, 11 Jul 2017 04:55:42 GMT">>},
 {<<"sec-websocket-accept">>,<<"eann9OkRXioYj7N6HzsNILl2/JI=">>},
 {<<"server">>,<<"Cowboy">>},
 {<<"upgrade">>,<<"websocket">>}]
ok
(my_gun@127.0.0.1)2> 
```
Based on that output, the gun client successfully upgraded the connection to a websocket.  At this point, the gun client only detects whether the upgrade request succeeded or not.  Let's change that so that the gun client sends some data to the server if the upgrade request succeeds:

```erlang
-module(my).
-compile(export_all).

get() ->
    ...

ws() ->
    {ok, _} = application:ensure_all_started(gun),
    {ok, ConnPid} = gun:open("localhost", 8080),
    {ok, _Protocol} = gun:await_up(ConnPid),

    gun:ws_upgrade(ConnPid, "/websocket"),
    receive
    {gun_ws_upgrade, ConnPid, ok, Headers} ->
            upgrade_success(ConnPid, Headers);
    {gun_response, ConnPid, _, _, Status, Headers} ->
            exit({ws_upgrade_failed, Status, Headers});
    {gun_error, _ConnPid, _StreamRef, Reason} ->
            exit({ws_upgrade_failed, Reason})
    %% More clauses here as needed.
    after 1000 ->
        exit(timeout)
    end,

    gun:shutdown(ConnPid).

upgrade_success(ConnPid, Headers) ->
    io:format("Upgraded ~w. Success!~nHeaders:~n~p~n", 
              [ConnPid, Headers]),

    %% ****** NEW CODE *******
    gun:ws_send(ConnPid, {text, "It's raining!"}),
    
    receive
        {gun_ws, ConnPid, {text, Msg} } ->
            io:format("~s~n", [Msg])
    end.
```

Here are the results:
```
~/erlang_programs/my_gun$ gmake run
gmake[1]: Entering directory '/Users/7stud/erlang_programs/my_gun/deps/gun'
gmake[2]: Entering directory '/Users/7stud/erlang_programs/my_gun/deps/cowlib'
gmake[2]: Leaving directory '/Users/7stud/erlang_programs/my_gun/deps/cowlib'
gmake[2]: Entering directory '/Users/7stud/erlang_programs/my_gun/deps/ranch'
gmake[2]: Leaving directory '/Users/7stud/erlang_programs/my_gun/deps/ranch'
 GEN    rebar.config
gmake[1]: Leaving directory '/Users/7stud/erlang_programs/my_gun/deps/gun'
 DEPEND my_gun.d
 ERLC   my.erl
 APP    my_gun
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          /Users/7stud/erlang_programs/my_gun/ebin
          /Users/7stud/erlang_programs/my_gun/deps
          /Users/7stud/.evm/erlang_versions/otp_src_19.2/lib/erlang/lib
          /Users/7stud/erlang_programs/my_gun/apps
          /Users/7stud/erlang_programs/my_gun/_rel
===> Resolved my_gun_release-1
===> rendering builtin_hook_status hook to "/Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/bin/hooks/builtin/status"
===> Including Erts from /Users/7stud/.evm/erlang_versions/otp_src_19.2/lib/erlang
===> release successfully created!
===> tarball /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/my_gun_release-1.tar.gz successfully created!
Exec: /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/erts-8.2/bin/erlexec -boot /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/releases/1/my_gun_release -mode embedded -boot_var ERTS_LIB_DIR /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/lib -config /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/releases/1/sys.config -args_file /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release/releases/1/vm.args -pa -- console
Root: /Users/7stud/erlang_programs/my_gun/_rel/my_gun_release
/Users/7stud/erlang_programs/my_gun/_rel/my_gun_release
heart_beat_kill_pid = 47215
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]


=PROGRESS REPORT==== 10-Jul-2017::23:07:24 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.353.0>},
                       {id,alarm_handler},
                       {mfargs,{alarm_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2017::23:07:24 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.352.0>},
                       {id,sasl_safe_sup},
                       {mfargs,
                           {supervisor,start_link,
                               [{local,sasl_safe_sup},sasl,safe]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 10-Jul-2017::23:07:24 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.354.0>},
                       {id,release_handler},
                       {mfargs,{release_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2017::23:07:24 ===
         application: sasl
          started_at: 'my_gun@127.0.0.1'

=PROGRESS REPORT==== 10-Jul-2017::23:07:24 ===
          supervisor: {local,runtime_tools_sup}
             started: [{pid,<0.360.0>},
                       {id,ttb_autostart},
                       {mfargs,{ttb_autostart,start_link,[]}},
                       {restart_type,temporary},
                       {shutdown,3000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2017::23:07:24 ===
         application: runtime_tools
          started_at: 'my_gun@127.0.0.1'
Eshell V8.2  (abort with ^G)
(my_gun@127.0.0.1)1> 
```
And:
```
(my_gun@127.0.0.1)1> my:ws().

=PROGRESS REPORT==== 10-Jul-2017::23:07:28 ===
          supervisor: {local,inet_gethost_native_sup}
             started: [{pid,<0.367.0>},{mfa,{inet_gethost_native,init,[[]]}}]

=PROGRESS REPORT==== 10-Jul-2017::23:07:28 ===
          supervisor: {local,kernel_safe_sup}
             started: [{pid,<0.366.0>},
                       {id,inet_gethost_native_sup},
                       {mfargs,{inet_gethost_native,start_link,[]}},
                       {restart_type,temporary},
                       {shutdown,1000},
                       {child_type,worker}]
Upgraded <0.365.0>. Success!
Headers:
[{<<"connection">>,<<"Upgrade">>},
 {<<"date">>,<<"Tue, 11 Jul 2017 05:07:28 GMT">>},
 {<<"sec-websocket-accept">>,<<"RnPQPxQFMHCuzb8SoJtjUjlp558=">>},
 {<<"server">>,<<"Cowboy">>},
 {<<"upgrade">>,<<"websocket">>}]
Server received: It's raining!
ok

(my_gun@127.0.0.1)2>
```



