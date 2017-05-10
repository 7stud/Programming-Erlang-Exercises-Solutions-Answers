Debugging hell! 

1. I kept a list of the monitored Workers, and each monitored Worker has the following strucuture:
    
        { {Pid, Ref}, Func }
        
    `{Pid, Ref}` is returned by `spawn_monitor(Func)`.

2. Originally, I had `restart_worker()` handle the case where the pid of the killed worker is not found in the list of monitored pids, i.e if `lists:keyfind()` returns false, but I don't think that's possible: it would mean that the monitor received a 'DOWN' message from a process that it wasn't monitoring, so I eliminated that case.


```erlang
-module(e5).
%%-compile(export_all).
-export([monitor_workers_init/1, monitor_workers/1]).
-export([test/0]).

monitor_workers_init(Funcs) ->
    spawn(?MODULE, monitor_workers, [Funcs]).

monitor_workers(Funcs) ->
    Workers = [ {spawn_monitor(Func), Func} || Func <- Funcs],  %% {{Pid, Ref}, Func}
    io:format("moniter_workers(): Workers: ~n~p~n", [Workers]),
    monitor_loop(Workers).

monitor_loop(Workers) ->
    receive
        {'DOWN', Ref, process, Pid, Why} ->
            io:format("monitor_loop(): Worker ~w went down: ~w~n.", [{Pid, Ref}, Why]),
            NewWorkers = restart_worker({Pid, Ref}, Workers),
            monitor_loop(NewWorkers);
        stop ->
            ok;
        {request, current_workers, From} ->
            From ! {reply, Workers, self()},
            monitor_loop(Workers)
    end.

restart_worker(PidRef, Workers) ->
    {_PidRef, Func} = lists:keyfind(PidRef, 1, Workers),  %%If returns false, then error,  
                                                          %%but I don't think that's possible.
    NewPidRef = spawn_monitor(Func),          
    io:format("...restarting ~w => ~w) ~n", [PidRef, NewPidRef]),

    NewWorkers = lists:keydelete(PidRef, 1, Workers),
    [{NewPidRef, Func} | NewWorkers].
    
shutdown(Monitor) ->
    Monitor ! {request, current_workers, self()},
    receive
        {reply, Workers, Monitor} ->  %%Monitor is bound!
            [ Pid ! stop || { {Pid, _}, _} <- Workers ]  %% { {Pid, Ref}, Func}
    end,
    Monitor ! stop,
    io:format("shutdown(): sent stop message to Monitor.~n").
    
%======== TESTS ==========

worker(N) ->
    receive
        stop -> ok
    after N*1000 ->
            io:format("Worker~w (~w) is still alive.~n", [N, self()] ),
            worker(N)
    end.


test() ->
    timer:sleep(500),  %%Allow output from startup of the erlang shell to print.

    Funcs = [fun() -> worker(N) end || N <- lists:seq(1, 4) ],
    Monitor = monitor_workers_init(Funcs),
    io:format("Monitor is: ~w~n", [Monitor]),

    timer:sleep(5200), %%Let monitored processes run for awhile.

    FiveTimes = 5,
    TimeBetweenKillings = 5200,
    kill_rand_worker(FiveTimes, TimeBetweenKillings, Monitor),

    shutdown(Monitor).

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
    {{Pid, _Ref}, _Func} = lists:nth(RandNum, Workers),
    io:format("kill_rand_worker(): about to kill ~w~n", [Pid]),
    exit(Pid, kill).
```
I can spot at least one issue with my `shutdown()` function, which is noted by the comment in all caps:
```erlang
shutdown(Monitor) ->
    Monitor ! {request, current_workers, self()},
    receive
        {reply, Workers, Monitor} ->  %%Monitor is bound!
        
            %% **** WHAT IF A WORKER FAILS HERE AND RESTARTS ********
            
            lists:map( fun({{Pid, _Ref}, _Func}) ->
                               Pid ! stop
                       end,
                       Workers)
    end,
    Monitor ! stop,
    io:format("shutdown(): Monitor sent stop message.~n").
```
If a Worker restarts immediately after `shutdown()` receives the Workers list from the Monitor, then the new Worker process will not get sent a stop message, so the new Worker process will continue on forever.

In the shell:
```
$ ./run
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.2  (abort with ^G)

1> Monitor is: <0.59.0>
moniter_workers(): Workers: 
[{{<0.60.0>,#Ref<0.0.4.132>},#Fun<e5.3.10310925>},
 {{<0.61.0>,#Ref<0.0.4.133>},#Fun<e5.3.10310925>},
 {{<0.62.0>,#Ref<0.0.4.134>},#Fun<e5.3.10310925>},
 {{<0.63.0>,#Ref<0.0.4.135>},#Fun<e5.3.10310925>}]
Worker1 (<0.60.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker3 (<0.62.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker4 (<0.63.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
kill_rand_worker(): about to kill <0.63.0>
monitor_loop(): Worker {<0.63.0>,#Ref<0.0.4.135>} went down: killed
....restarting {<0.63.0>,#Ref<0.0.4.135>} => {<0.64.0>,#Ref<0.0.4.150>}) 
Worker3 (<0.62.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker3 (<0.62.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker4 (<0.64.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
kill_rand_worker(): about to kill <0.60.0>
monitor_loop(): Worker {<0.60.0>,#Ref<0.0.4.132>} went down: killed
....restarting {<0.60.0>,#Ref<0.0.4.132>} => {<0.65.0>,#Ref<0.0.4.165>}) 
Worker1 (<0.65.0>) is still alive.
Worker3 (<0.62.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.65.0>) is still alive.
Worker4 (<0.64.0>) is still alive.
Worker1 (<0.65.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.65.0>) is still alive.
Worker3 (<0.62.0>) is still alive.
Worker1 (<0.65.0>) is still alive.
kill_rand_worker(): about to kill <0.65.0>
monitor_loop(): Worker {<0.65.0>,#Ref<0.0.4.165>} went down: killed
....restarting {<0.65.0>,#Ref<0.0.4.165>} => {<0.66.0>,#Ref<0.0.4.179>}) 
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.66.0>) is still alive.
Worker4 (<0.64.0>) is still alive.
Worker1 (<0.66.0>) is still alive.
Worker3 (<0.62.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.66.0>) is still alive.
Worker1 (<0.66.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.66.0>) is still alive.
kill_rand_worker(): about to kill <0.62.0>
monitor_loop(): Worker {<0.62.0>,#Ref<0.0.4.134>} went down: killed
....restarting {<0.62.0>,#Ref<0.0.4.134>} => {<0.67.0>,#Ref<0.0.4.193>}) 
Worker4 (<0.64.0>) is still alive.
Worker1 (<0.66.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.66.0>) is still alive.
Worker1 (<0.66.0>) is still alive.
Worker3 (<0.67.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.66.0>) is still alive.
Worker4 (<0.64.0>) is still alive.
Worker1 (<0.66.0>) is still alive.
kill_rand_worker(): about to kill <0.66.0>
monitor_loop(): Worker {<0.66.0>,#Ref<0.0.4.179>} went down: killed
....restarting {<0.66.0>,#Ref<0.0.4.179>} => {<0.68.0>,#Ref<0.0.4.207>}) 
Worker2 (<0.61.0>) is still alive.
Worker3 (<0.67.0>) is still alive.
Worker1 (<0.68.0>) is still alive.
Worker1 (<0.68.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.68.0>) is still alive.
Worker4 (<0.64.0>) is still alive.
Worker3 (<0.67.0>) is still alive.
Worker1 (<0.68.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.68.0>) is still alive.
shutdown(): sent stop message to Monitor.
```
There are a couple of things in the program output that demonstrate that eveything is working correctly:

1.  The worker numbers 1-4 appear in every secton of the output.

2.  In the restart output, you can actually examine the pid of the process being killed and look for the corresponding Worker number in the prior section of the output, then make a note of that Worker number.  Then examine the pid of the new process in the restart output and in the subsequent output check if the new pid corresponds to the same Worker number.

Checking to make sure that none of the processes that were started are still alive:
```
i().
Pid                   Initial Call                          Heap     Reds Msgs
Registered            Current Function                     Stack              
<0.0.0>               otp_ring0:start/2                      376      637    0
init                  init:loop/1                              2              
<0.1.0>               erts_code_purger:start/0               233        4    0
erts_code_purger      erts_code_purger:loop/0                  3              
<0.4.0>               erlang:apply/2                        6772   119366    0
erl_prim_loader       erl_prim_loader:loop/3                   5              
<0.30.0>              gen_event:init_it/6                    610      226    0
error_logger          gen_event:fetch_msg/5                    8              
<0.31.0>              erlang:apply/2                        1598      416    0
application_controlle gen_server:loop/6                        7              
<0.33.0>              application_master:init/4              233       64    0
                      application_master:main_loop/2           6              
<0.34.0>              application_master:start_it/4          233       59    0
                      application_master:loop_it/4             5              
<0.35.0>              supervisor:kernel/1                    610     1765    0
kernel_sup            gen_server:loop/6                        9              
<0.36.0>              erlang:apply/2                        4185   107483    0
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
<0.44.0>              file_server:init/1                     233       78    0
file_server_2         gen_server:loop/6                        9              
<0.45.0>              supervisor_bridge:standard_error/      233       34    0
standard_error_sup    gen_server:loop/6                        9              
<0.46.0>              erlang:apply/2                         233       10    0
standard_error        standard_error:server_loop/1             2              
<0.47.0>              supervisor_bridge:user_sup/1           233       53    0
                      gen_server:loop/6                        9              
<0.48.0>              user_drv:server/2                     1598     4452    0
user_drv              user_drv:server_loop/6                   9              
<0.49.0>              group:server/3                         610    12645    0
user                  group:server_loop/3                      4              
<0.50.0>              group:server/3                         987    12510    0
                      group:server_loop/3                      4              
<0.51.0>              erlang:apply/2                        4185     9788    0
                      shell:shell_rep/4                       17              
<0.52.0>              kernel_config:init/1                   233      258    0
                      gen_server:loop/6                        9              
<0.53.0>              supervisor:kernel/1                    233       56    0
kernel_safe_sup       gen_server:loop/6                        9              
<0.57.0>              erlang:apply/2                        2586    18839    0
                      c:pinfo/1                               50              
Total                                                      27612   289136    0
                                                             222              
ok

```
No processes from e5 in there!

