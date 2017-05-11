1. For my first version,  I used monitors.  I pretty much followed the literal description in the exercise.

2. For my second version, I used links.  I created a _link set_ where all the Workers were linked to a MasterLink.  That way if one Worker fails, they all fail--including the MasterLink. Then, I created a Monitor process to monitor the MasterLink, which means that when any Worker fails the Monitor detects the MasterLink failing.  The only thing the Monitor has to do after receiving a 'DOWN' message is to restart all the Workers.  For testing,  I used erlang's observer app, `observer:start()`, to kill random Workers.  In the observer window, you can click on the Processes tab, then you can right click on a running process, then choose Kill Process.  After killing a process, you can selelct View>Refresh to display the new Worker processes that are created.

Both versions are included below:

```erlang
-module(e6).
-compile(export_all).

supervisor_init(Funcs) ->
    spawn(?MODULE, supervisor, [Funcs]).
    
supervisor(Funcs) ->
    Workers = [ {spawn_monitor(Func), Func} || Func <- Funcs], %% { {Pid,Ref}, Func}
    supervisor_loop(Workers).

supervisor_loop(Workers) ->
    receive
        {'DOWN', Ref, process, Pid, Why} ->
            io:format("supervisor_loop(): Worker ~w went down: ~w~n", [{Pid,Ref}, Why]),
            io:format("...restarting all workers.~n"),
            
            stop_workers(Workers),
            NewWorkers = [ {spawn_monitor(Func), Func} || {_, Func} <- Workers],
            
            io:format("supervisor_loop(): Old Workers:~n~p~n", [Workers]),
            io:format("supervisor_loop(): NewWorkers:~n~p~n", [NewWorkers]),
            
            supervisor_loop(NewWorkers);
        stop ->
            stop_workers(Workers),
            
            io:format("monitor_loop():~n"),
            io:format("\tMonitor finished shutting down workers.~n"),
            io:format("\tMonitor terminating normally.~n");
        {request, current_workers, From} ->
                From ! {reply, Workers, self()},
                supervisor_loop(Workers)
    end.

stop_workers(Workers) ->
    lists:foreach(fun({{Pid,Ref},_}) ->
                          demonitor(Ref),
                          Pid ! stop
                  end,
                  Workers).  %% { {Pid,Ref}, Func}

%%==== TESTS ========

worker(Id) ->
    receive 
        stop -> ok
    after Id*1000 ->
        io:format("Worker~w: I'm still alive in ~w~n", [Id, self()]),
        worker(Id)
    end.

test() ->
    Funcs = [fun() -> worker(Id) end || Id <- lists:seq(1, 4)],
    Supervisor = supervisor_init(Funcs),
   
    timer:sleep(5200),

    FiveTimes = 5,
    TimeBetweenKillings = 5200,
    kill_rand_worker(FiveTimes, TimeBetweenKillings, Supervisor),

    Supervisor ! stop.

kill_rand_worker(0, _, _) ->
    ok;
kill_rand_worker(NumTimes, TimeBetweenKillings, Monitor) ->
    Workers = get_workers(Monitor), 
    kill_rand_worker(Workers),
    timer:sleep(TimeBetweenKillings),
    kill_rand_worker(NumTimes-1, TimeBetweenKillings, Monitor).

get_workers(Monitor) ->
    Monitor ! {request, current_workers, self()},
    receive
        {reply, CurrentWorkers, Monitor} -> 
            CurrentWorkers
    end.

kill_rand_worker(Workers) ->
    RandNum = rand:uniform(length(Workers) ),
    {{Pid, _}, _} = lists:nth(RandNum, Workers),  %% { {Pid, Ref} Func}
    io:format("kill_rand_worker(): about to kill ~w~n", [Pid]),
    exit(Pid, kill).
```

In the shell:

```
$ ./run
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.2  (abort with ^G)

1> Worker1: I'm still alive in <0.58.0>
Worker2: I'm still alive in <0.59.0>
Worker1: I'm still alive in <0.58.0>
Worker3: I'm still alive in <0.60.0>
Worker1: I'm still alive in <0.58.0>
Worker4: I'm still alive in <0.61.0>
Worker2: I'm still alive in <0.59.0>
Worker1: I'm still alive in <0.58.0>
Worker1: I'm still alive in <0.58.0>
kill_rand_worker(): about to kill <0.58.0>
supervisor_loop(): Worker {<0.58.0>,#Ref<0.0.3.102>} went down: killed
...restarting all workers.
supervisor_loop(): Old Workers:
[{{<0.58.0>,#Ref<0.0.3.102>},#Fun<e6.1.128383431>},
 {{<0.59.0>,#Ref<0.0.3.103>},#Fun<e6.1.128383431>},
 {{<0.60.0>,#Ref<0.0.3.104>},#Fun<e6.1.128383431>},
 {{<0.61.0>,#Ref<0.0.3.105>},#Fun<e6.1.128383431>}]
supervisor_loop(): NewWorkers:
[{{<0.64.0>,#Ref<0.0.3.126>},#Fun<e6.1.128383431>},
 {{<0.65.0>,#Ref<0.0.3.127>},#Fun<e6.1.128383431>},
 {{<0.66.0>,#Ref<0.0.3.128>},#Fun<e6.1.128383431>},
 {{<0.67.0>,#Ref<0.0.3.129>},#Fun<e6.1.128383431>}]
Worker1: I'm still alive in <0.64.0>
Worker2: I'm still alive in <0.65.0>
Worker1: I'm still alive in <0.64.0>
Worker3: I'm still alive in <0.66.0>
Worker1: I'm still alive in <0.64.0>
Worker4: I'm still alive in <0.67.0>
Worker2: I'm still alive in <0.65.0>
Worker1: I'm still alive in <0.64.0>
Worker1: I'm still alive in <0.64.0>
kill_rand_worker(): about to kill <0.65.0>
supervisor_loop(): Worker {<0.65.0>,#Ref<0.0.3.127>} went down: killed
...restarting all workers.
supervisor_loop(): Old Workers:
[{{<0.64.0>,#Ref<0.0.3.126>},#Fun<e6.1.128383431>},
 {{<0.65.0>,#Ref<0.0.3.127>},#Fun<e6.1.128383431>},
 {{<0.66.0>,#Ref<0.0.3.128>},#Fun<e6.1.128383431>},
 {{<0.67.0>,#Ref<0.0.3.129>},#Fun<e6.1.128383431>}]
supervisor_loop(): NewWorkers:
[{{<0.68.0>,#Ref<0.0.3.144>},#Fun<e6.1.128383431>},
 {{<0.69.0>,#Ref<0.0.3.145>},#Fun<e6.1.128383431>},
 {{<0.70.0>,#Ref<0.0.3.146>},#Fun<e6.1.128383431>},
 {{<0.71.0>,#Ref<0.0.3.147>},#Fun<e6.1.128383431>}]
Worker1: I'm still alive in <0.68.0>
Worker2: I'm still alive in <0.69.0>
Worker1: I'm still alive in <0.68.0>
Worker3: I'm still alive in <0.70.0>
Worker1: I'm still alive in <0.68.0>
Worker4: I'm still alive in <0.71.0>
Worker2: I'm still alive in <0.69.0>
Worker1: I'm still alive in <0.68.0>
Worker1: I'm still alive in <0.68.0>
kill_rand_worker(): about to kill <0.70.0>
supervisor_loop(): Worker {<0.70.0>,#Ref<0.0.3.146>} went down: killed
...restarting all workers.
supervisor_loop(): Old Workers:
[{{<0.68.0>,#Ref<0.0.3.144>},#Fun<e6.1.128383431>},
 {{<0.69.0>,#Ref<0.0.3.145>},#Fun<e6.1.128383431>},
 {{<0.70.0>,#Ref<0.0.3.146>},#Fun<e6.1.128383431>},
 {{<0.71.0>,#Ref<0.0.3.147>},#Fun<e6.1.128383431>}]
supervisor_loop(): NewWorkers:
[{{<0.72.0>,#Ref<0.0.3.162>},#Fun<e6.1.128383431>},
 {{<0.73.0>,#Ref<0.0.3.163>},#Fun<e6.1.128383431>},
 {{<0.74.0>,#Ref<0.0.3.164>},#Fun<e6.1.128383431>},
 {{<0.75.0>,#Ref<0.0.3.165>},#Fun<e6.1.128383431>}]
Worker1: I'm still alive in <0.72.0>
Worker2: I'm still alive in <0.73.0>
Worker1: I'm still alive in <0.72.0>
Worker3: I'm still alive in <0.74.0>
Worker1: I'm still alive in <0.72.0>
Worker4: I'm still alive in <0.75.0>
Worker2: I'm still alive in <0.73.0>
Worker1: I'm still alive in <0.72.0>
Worker1: I'm still alive in <0.72.0>
kill_rand_worker(): about to kill <0.73.0>
supervisor_loop(): Worker {<0.73.0>,#Ref<0.0.3.163>} went down: killed
...restarting all workers.
supervisor_loop(): Old Workers:
[{{<0.72.0>,#Ref<0.0.3.162>},#Fun<e6.1.128383431>},
 {{<0.73.0>,#Ref<0.0.3.163>},#Fun<e6.1.128383431>},
 {{<0.74.0>,#Ref<0.0.3.164>},#Fun<e6.1.128383431>},
 {{<0.75.0>,#Ref<0.0.3.165>},#Fun<e6.1.128383431>}]
supervisor_loop(): NewWorkers:
[{{<0.76.0>,#Ref<0.0.3.180>},#Fun<e6.1.128383431>},
 {{<0.77.0>,#Ref<0.0.3.181>},#Fun<e6.1.128383431>},
 {{<0.78.0>,#Ref<0.0.3.182>},#Fun<e6.1.128383431>},
 {{<0.79.0>,#Ref<0.0.3.183>},#Fun<e6.1.128383431>}]
Worker1: I'm still alive in <0.76.0>
Worker2: I'm still alive in <0.77.0>
Worker1: I'm still alive in <0.76.0>
Worker3: I'm still alive in <0.78.0>
Worker1: I'm still alive in <0.76.0>
Worker4: I'm still alive in <0.79.0>
Worker2: I'm still alive in <0.77.0>
Worker1: I'm still alive in <0.76.0>
Worker1: I'm still alive in <0.76.0>
kill_rand_worker(): about to kill <0.76.0>
supervisor_loop(): Worker {<0.76.0>,#Ref<0.0.3.180>} went down: killed
...restarting all workers.
supervisor_loop(): Old Workers:
[{{<0.76.0>,#Ref<0.0.3.180>},#Fun<e6.1.128383431>},
 {{<0.77.0>,#Ref<0.0.3.181>},#Fun<e6.1.128383431>},
 {{<0.78.0>,#Ref<0.0.3.182>},#Fun<e6.1.128383431>},
 {{<0.79.0>,#Ref<0.0.3.183>},#Fun<e6.1.128383431>}]
supervisor_loop(): NewWorkers:
[{{<0.80.0>,#Ref<0.0.3.198>},#Fun<e6.1.128383431>},
 {{<0.81.0>,#Ref<0.0.3.199>},#Fun<e6.1.128383431>},
 {{<0.82.0>,#Ref<0.0.3.200>},#Fun<e6.1.128383431>},
 {{<0.83.0>,#Ref<0.0.3.201>},#Fun<e6.1.128383431>}]
Worker1: I'm still alive in <0.80.0>
Worker2: I'm still alive in <0.81.0>
Worker1: I'm still alive in <0.80.0>
Worker3: I'm still alive in <0.82.0>
Worker1: I'm still alive in <0.80.0>
Worker4: I'm still alive in <0.83.0>
Worker2: I'm still alive in <0.81.0>
Worker1: I'm still alive in <0.80.0>
Worker1: I'm still alive in <0.80.0>
monitor_loop():
        Monitor finished shutting down workers.
        Monitor terminating normally.
```
Check to make sure there are no leftover processes:
```
i().
Pid                   Initial Call                          Heap     Reds Msgs
Registered            Current Function                     Stack              
<0.0.0>               otp_ring0:start/2                      376      637    0
init                  init:loop/1                              2              
<0.1.0>               erts_code_purger:start/0               233        4    0
erts_code_purger      erts_code_purger:loop/0                  3              
<0.4.0>               erlang:apply/2                         987   119748    0
erl_prim_loader       erl_prim_loader:loop/3                   5              
<0.30.0>              gen_event:init_it/6                    610      226    0
error_logger          gen_event:fetch_msg/5                    8              
<0.31.0>              erlang:apply/2                        1598      416    0
application_controlle gen_server:loop/6                        7              
<0.33.0>              application_master:init/4              233       64    0
                      application_master:main_loop/2           6              
<0.34.0>              application_master:start_it/4          233       59    0
                      application_master:loop_it/4             5              
<0.35.0>              supervisor:kernel/1                    610     1784    0
kernel_sup            gen_server:loop/6                        9              
<0.36.0>              erlang:apply/2                        4185   107485    0
code_server           code_server:loop/1                       3              
<0.38.0>              rpc:init/1                             233       21    0
rex                   gen_server:loop/6                        9              
<0.39.0>              global:init/1                          233       44    0
global_name_server    gen_server:loop/6                        9              
<0.40.0>              erlang:apply/2                         233       21    0
                      global:loop_the_locker/1                 5              
<0.41.0>              erlang:apply/2                         233        3    0
                      global:loop_the_registrar/0              2              
<0.42.0>              inet_db:init/1                         233      255    0
inet_db               gen_server:loop/6                        9              
<0.43.0>              global_group:init/1                    233       55    0
global_group          gen_server:loop/6                        9              
<0.44.0>              file_server:init/1                     233       78    0
file_server_2         gen_server:loop/6                        9              
<0.45.0>              supervisor_bridge:standard_error/      233       34    0
standard_error_sup    gen_server:loop/6                        9              
<0.46.0>              erlang:apply/2                         233       10    0
standard_error        standard_error:server_loop/1             2              
<0.47.0>              supervisor_bridge:user_sup/1           233       53    0
                      gen_server:loop/6                        9              
<0.48.0>              user_drv:server/2                     1598     4545    0
user_drv              user_drv:server_loop/6                   9              
<0.49.0>              group:server/3                        1598    16834    0
user                  group:server_loop/3                      4              
<0.50.0>              group:server/3                         987    12510    0
                      group:server_loop/3                      4              
<0.51.0>              erlang:apply/2                        4185     9788    0
                      shell:shell_rep/4                       17              
<0.52.0>              kernel_config:init/1                   233      258    0
                      gen_server:loop/6                        9              
<0.53.0>              supervisor:kernel/1                    233       56    0
kernel_safe_sup       gen_server:loop/6                        9              
<0.62.0>              erlang:apply/2                        2586    18839    0
                      c:pinfo/1                               50              
Total                                                      22815   293827    0
                                                             222              
ok
2> 
```
    
Here's a version that uses links:
```erlang
-module(e6).
-compile(export_all).

monitor_init(NumWorkers) ->
    register(?MODULE, Pid = spawn(?MODULE, monitor, [NumWorkers]) ),
    Pid.
    
monitor(NumWorkers) ->
    MasterLink = create_link_set(NumWorkers),  %% => {Pid, Ref}
    monitor_loop(MasterLink, NumWorkers).

monitor_loop({MasterPid, MasterRef}, NumWorkers) ->
    receive
        {'DOWN', MasterRef, process, MasterPid, Why} ->
            io:format("monitor_loop(): MasterLink ~w went down: ~w~n", [{MasterPid,MasterRef}, Why]),
            io:format("...restarting all workers.~n"),

            NewMasterLink = create_link_set(NumWorkers),
            monitor_loop(NewMasterLink, NumWorkers);
        {request, stop, _From} ->
             exit(MasterPid, kill),
            io:format("\tMonitor sent kill signal to MasterLink.~n"),
            io:format("\tMonitor terminating normally.~n")
    end.

create_link_set(NumWorkers) ->
    NewMasterLink =
        fun() -> 
                NewWorkers = [ spawn_link(?MODULE, worker, [Id]) || Id <- lists:seq(1, NumWorkers)],
                io:format("create_link_set(): NewWorkers: ~w~n", [NewWorkers]),
                receive 
                after infinity -> ok   %%MasterLink sits and does nothing.
                end
        end,
    MasterPidRef = spawn_monitor(NewMasterLink),  %% monitor_loop() monitors MasterLink
    io:format("create_link_set(): NewMasterLink: ~w~n", [MasterPidRef]),
    MasterPidRef.

stop() ->
    ?MODULE ! {request, stop, self()}.
    

%%==== TESTS ========

worker(Id) ->
    receive 
        stop -> ok
    after Id*1000 ->
        io:format("Worker~w: I'm still alive in ~w~n", [Id, self()]),
        worker(Id)
    end.

test() ->
    observer:start(),  %%I use the Processes tab to kill random workers.
    timer:sleep(500),  %%Allow output from erlang shell startup to print.

    NumWorkers = 4,
    monitor_init(NumWorkers),
   
    timer:sleep(30000),  %%Give me time to kill some processes in observer app.

    stop().
```

In the shell:
```
$ ./run
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.2  (abort with ^G)

1> create_link_set(): NewMasterLink: {<0.79.0>,#Ref<0.0.4.277>}
create_link_set(): NewWorkers: [<0.80.0>,<0.81.0>,<0.82.0>,<0.83.0>]
Worker1: I'm still alive in <0.80.0>
Worker2: I'm still alive in <0.81.0>
Worker1: I'm still alive in <0.80.0>
Worker3: I'm still alive in <0.82.0>
Worker1: I'm still alive in <0.80.0>
Worker4: I'm still alive in <0.83.0>
Worker2: I'm still alive in <0.81.0>
Worker1: I'm still alive in <0.80.0>
Worker1: I'm still alive in <0.80.0>
Worker3: I'm still alive in <0.82.0>
Worker2: I'm still alive in <0.81.0>
Worker1: I'm still alive in <0.80.0>
Worker1: I'm still alive in <0.80.0>
monitor_loop(): MasterLink {<0.79.0>,#Ref<0.0.4.277>} went down: killed
...restarting all workers.
create_link_set(): NewMasterLink: {<0.603.0>,#Ref<0.0.4.1863>}
create_link_set(): NewWorkers: [<0.604.0>,<0.605.0>,<0.606.0>,<0.607.0>]
Worker1: I'm still alive in <0.604.0>
Worker2: I'm still alive in <0.605.0>
Worker1: I'm still alive in <0.604.0>
Worker3: I'm still alive in <0.606.0>
Worker1: I'm still alive in <0.604.0>
Worker4: I'm still alive in <0.607.0>
Worker2: I'm still alive in <0.605.0>
Worker1: I'm still alive in <0.604.0>
Worker1: I'm still alive in <0.604.0>
monitor_loop(): MasterLink {<0.603.0>,#Ref<0.0.4.1863>} went down: killed
...restarting all workers.
create_link_set(): NewMasterLink: {<0.1072.0>,#Ref<0.0.4.3279>}
create_link_set(): NewWorkers: [<0.1073.0>,<0.1074.0>,<0.1075.0>,<0.1076.0>]
Worker1: I'm still alive in <0.1073.0>
Worker2: I'm still alive in <0.1074.0>
Worker1: I'm still alive in <0.1073.0>
Worker3: I'm still alive in <0.1075.0>
Worker1: I'm still alive in <0.1073.0>
Worker4: I'm still alive in <0.1076.0>
Worker2: I'm still alive in <0.1074.0>
Worker1: I'm still alive in <0.1073.0>
Worker1: I'm still alive in <0.1073.0>
monitor_loop(): MasterLink {<0.1072.0>,#Ref<0.0.4.3279>} went down: killed
...restarting all workers.
create_link_set(): NewMasterLink: {<0.1302.0>,#Ref<0.0.4.3974>}
create_link_set(): NewWorkers: [<0.1303.0>,<0.1304.0>,<0.1305.0>,<0.1306.0>]
Worker1: I'm still alive in <0.1303.0>
Worker2: I'm still alive in <0.1304.0>
Worker1: I'm still alive in <0.1303.0>
Worker3: I'm still alive in <0.1305.0>
Worker1: I'm still alive in <0.1303.0>
Worker4: I'm still alive in <0.1306.0>
Worker2: I'm still alive in <0.1304.0>
Worker1: I'm still alive in <0.1303.0>
Worker1: I'm still alive in <0.1303.0>
Worker3: I'm still alive in <0.1305.0>
Worker2: I'm still alive in <0.1304.0>
Worker1: I'm still alive in <0.1303.0>
Worker1: I'm still alive in <0.1303.0>
Worker4: I'm still alive in <0.1306.0>
Worker2: I'm still alive in <0.1304.0>
Worker1: I'm still alive in <0.1303.0>
monitor_loop(): MasterLink {<0.1302.0>,#Ref<0.0.4.3974>} went down: killed
...restarting all workers.
create_link_set(): NewMasterLink: {<0.1757.0>,#Ref<0.0.4.5356>}
create_link_set(): NewWorkers: [<0.1758.0>,<0.1759.0>,<0.1760.0>,<0.1761.0>]
Worker1: I'm still alive in <0.1758.0>
Worker2: I'm still alive in <0.1759.0>
Worker1: I'm still alive in <0.1758.0>
        Monitor sent kill signal to MasterLink.
        Monitor terminating normally.
```

After closing the observer window, I checked to see if there were any left over processes from my program:

```
i().
Pid                   Initial Call                          Heap     Reds Msgs
Registered            Current Function                     Stack              
<0.0.0>               otp_ring0:start/2                      376      637    0
init                  init:loop/1                              2              
<0.1.0>               erts_code_purger:start/0               233        4    0
erts_code_purger      erts_code_purger:loop/0                  3              
<0.4.0>               erlang:apply/2                        1598   250248    0
erl_prim_loader       erl_prim_loader:loop/3                   5              
<0.30.0>              gen_event:init_it/6                    610      242    0
error_logger          gen_event:fetch_msg/5                    8              
<0.31.0>              erlang:apply/2                        1598      416    0
application_controlle gen_server:loop/6                        7              
<0.33.0>              application_master:init/4              233       64    0
                      application_master:main_loop/2           6              
<0.34.0>              application_master:start_it/4          233       59    0
                      application_master:loop_it/4             5              
<0.35.0>              supervisor:kernel/1                    610     1765    0
kernel_sup            gen_server:loop/6                        9              
<0.36.0>              erlang:apply/2                        6772   117111    0
code_server           code_server:loop/1                       3              
<0.38.0>              rpc:init/1                             233       21    0
rex                   gen_server:loop/6                        9              
<0.39.0>              global:init/1                          233       44    0
global_name_server    gen_server:loop/6                        9              
<0.40.0>              erlang:apply/2                         233       21    0
                      global:loop_the_locker/1                 5              
<0.41.0>              erlang:apply/2                         233        3    0
                      global:loop_the_registrar/0              2              
<0.42.0>              inet_db:init/1                         233      249    0
inet_db               gen_server:loop/6                        9              
<0.43.0>              global_group:init/1                    233       55    0
global_group          gen_server:loop/6                        9              
<0.44.0>              file_server:init/1                     610      447    0
file_server_2         gen_server:loop/6                        9              
<0.45.0>              supervisor_bridge:standard_error/      233       34    0
standard_error_sup    gen_server:loop/6                        9              
<0.46.0>              erlang:apply/2                         233       10    0
standard_error        standard_error:server_loop/1             2              
<0.47.0>              supervisor_bridge:user_sup/1           233       53    0
                      gen_server:loop/6                        9              
<0.48.0>              user_drv:server/2                     2586     4172    0
user_drv              user_drv:server_loop/6                   9              
<0.49.0>              group:server/3                        2586    11381    0
user                  group:server_loop/3                      4              
<0.50.0>              group:server/3                         987    12514    0
                      group:server_loop/3                      4              
<0.51.0>              erlang:apply/2                        4185     9788    0
                      shell:shell_rep/4                       17              
<0.52.0>              kernel_config:init/1                   233      258    0
                      gen_server:loop/6                        9              
<0.53.0>              supervisor:kernel/1                    233      131    0
kernel_safe_sup       gen_server:loop/6                        9              
<0.58.0>              erlang:apply/2                        2586    18804    0
                      c:pinfo/1                               50              
<0.61.0>              wxe_master:init/1                     2586      747    0
wxe_master            gen_server:loop/6                        9              
<0.62.0>              erlang:apply/2                         610    12865    0
                      timer:sleep/1                            5              
<0.64.0>              timer:init/1                           376      491    0
timer_server          gen_server:loop/6                        9              
Total                                                      31938   442634    0
                                                             245              
ok
2> 
```
Nothing that starts with `e6` in there!
    
    
                    



