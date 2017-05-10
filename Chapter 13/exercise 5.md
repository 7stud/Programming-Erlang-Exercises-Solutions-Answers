Debugging hell!

Originally, I had the `restart_worker()` function handle the case where the pid of the killed worker is not found in the list of monitored pids, i.e if `lists:keyfind()` returns false, but I don't think that's possible: it would mean that the monitor received a 'DOWN' message from a process it wasn't monitoring, so I eliminated that case.


```erlang
-module(e5).
%%-compile(export_all).
-export([monitor_workers_init/1, monitor_workers/1]).
-export([test/0]).

monitor_workers_init(Funcs) ->
    spawn(?MODULE, monitor_workers, [Funcs]).

monitor_workers(Funcs) ->
    Workers = lists:map(
                 fun(Func) -> 
                         {spawn_monitor(Func), Func} %%{{Pid, Ref}, Func}
                 end,  
                 Funcs),
    io:format("moniter_workers(): Workers: ~n~p~n", [Workers]),
    monitor_loop(Workers).

monitor_loop(Workers) ->
    receive
        {'DOWN', Ref, process, Pid, Why} ->
            io:format("monitor_loop(): Worker (~w, ~w) went down: ~w~n.", [Pid, Ref, Why]),
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
            lists:map( fun({{Pid, _Ref}, _Func}) ->
                               Pid ! stop
                       end,
                       Workers)
    end,
    Monitor ! stop,
    io:format("shutdown(): Monitor sent stop message.~n").
    
%======== TESTS ==========

worker(N) ->
    receive
        stop -> ok
    after N*1000 ->
            io:format("Worker~w (~w) is still alive.~n", [N, self()] ),
            worker(N)
    end.


test() ->
    timer:sleep(500),  %%Allow output from erlang shell startup to print.

    Funcs = lists:map(
              fun(N) ->
                      fun() -> worker(N) end
              end,
              lists:seq(1, 4)
             ),
    Monitor = monitor_workers_init(Funcs),
    io:format("Monitor is: ~w~n", [Monitor]),

    timer:sleep(5200), %%Let monitored processes run for awhile.

    FiveTimes = 5,
    TimeInterval = 5200,
    kill_rand_worker(FiveTimes, TimeInterval, Monitor),

    shutdown(Monitor).

kill_rand_worker(0, _, _) ->
    ok;
kill_rand_worker(NumTimes, TimeInterval, Monitor) ->
    Workers = get_workers(Monitor), 
    kill_rand_worker(Workers),
    timer:sleep(TimeInterval),
    kill_rand_worker( NumTimes-1, TimeInterval, Monitor).

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

I can see at least one issue with my `shutdown()` function:
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
If a Worker restarted immediately after `shutdown()` received the Workers list from the Monitor, then the new Worker process would not get sent a stop message, so the new Worker process would continue on forever.

In the shell:
```
$ ./run
-----Now, do some Erlang for great Good!------

Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

-----Now, do some Erlang for great Good!------

Eshell V8.2  (abort with ^G)
1> Monitor is: <0.59.0>
moniter_workers(): Workers: 
[{{<0.60.0>,#Ref<0.0.3.108>},#Fun<e5.3.13623047>},
 {{<0.61.0>,#Ref<0.0.3.109>},#Fun<e5.3.13623047>},
 {{<0.62.0>,#Ref<0.0.3.110>},#Fun<e5.3.13623047>},
 {{<0.63.0>,#Ref<0.0.3.111>},#Fun<e5.3.13623047>}]
Worker1 (<0.60.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker3 (<0.62.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker4 (<0.63.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
kill_rand_worker(): about to kill <0.62.0>
monitor_loop(): Worker (<0.62.0>, #Ref<0.0.3.110>) went down: killed
....restarting {<0.62.0>,#Ref<0.0.3.110>} => {<0.64.0>,#Ref<0.0.3.126>}) 
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker4 (<0.63.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker3 (<0.64.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker2 (<0.61.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
kill_rand_worker(): about to kill <0.61.0>
monitor_loop(): Worker (<0.61.0>, #Ref<0.0.3.109>) went down: killed
....restarting {<0.61.0>,#Ref<0.0.3.109>} => {<0.65.0>,#Ref<0.0.3.140>}) 
Worker1 (<0.60.0>) is still alive.
Worker3 (<0.64.0>) is still alive.
Worker4 (<0.63.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker2 (<0.65.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker3 (<0.64.0>) is still alive.
Worker2 (<0.65.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
kill_rand_worker(): about to kill <0.65.0>
monitor_loop(): Worker (<0.65.0>, #Ref<0.0.3.140>) went down: killed
....restarting {<0.65.0>,#Ref<0.0.3.140>} => {<0.66.0>,#Ref<0.0.3.154>}) 
Worker4 (<0.63.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker3 (<0.64.0>) is still alive.
Worker2 (<0.66.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker2 (<0.66.0>) is still alive.
Worker4 (<0.63.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker3 (<0.64.0>) is still alive.
kill_rand_worker(): about to kill <0.64.0>
monitor_loop(): Worker (<0.64.0>, #Ref<0.0.3.126>) went down: killed
....restarting {<0.64.0>,#Ref<0.0.3.126>} => {<0.67.0>,#Ref<0.0.3.169>}) 
Worker1 (<0.60.0>) is still alive.
Worker2 (<0.66.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker2 (<0.66.0>) is still alive.
Worker3 (<0.67.0>) is still alive.
Worker4 (<0.63.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker2 (<0.66.0>) is still alive.
kill_rand_worker(): about to kill <0.67.0>
monitor_loop(): Worker (<0.67.0>, #Ref<0.0.3.169>) went down: killed
....restarting {<0.67.0>,#Ref<0.0.3.169>} => {<0.68.0>,#Ref<0.0.3.183>}) 
Worker1 (<0.60.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker2 (<0.66.0>) is still alive.
Worker4 (<0.63.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker3 (<0.68.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker2 (<0.66.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
Worker1 (<0.60.0>) is still alive.
shutdown(): Monitor stopped.
```

There are a couple of things in the output that demonstrate eveything is working correctly:

1.  The worker numbers 1-4 appear in every secton of the output.

2.  In the restart output you can actually examine the pid of the process being killed and look for the corresponding Worker number in the prior section of output that matches that pid, then make note of that Worker number.  Then examine the pid of the new process and in the subsequent output check if the new pid corresponds to the same Worker number.
