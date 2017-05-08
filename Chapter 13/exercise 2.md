Using `on_exit()` in `my_spawn/3`:

```erlang
-module(my).
-export([atomizer/0, my_spawn/3, test/0]).

on_exit(Pid, Fun) ->
    spawn( fun() ->
                   Ref = monitor(process, Pid),
                   receive 
                       {'DOWN', Ref, process, Pid, Why} ->
                           Fun(Why);
                       Other -> io:format("Other: ~w~n", [Other])
                   end
           end).

my_spawn(Mod, Fun, Args) ->
    Pid = spawn(Mod, Fun, Args),
    statistics(wall_clock), %%Get the elapsed time since the previous call 
                            %%to statistics(wall_clock) and throw it away.

    %%exit(Pid, kill),

    TerminationFun = 
        fun(Why) ->
                {_, WallTime} = statistics(wall_clock),  %%Get the elapsed time since the 
                                                         %%previous call to statistics(wall_clock).
                io:format("Process (~w) terminated. ", [Pid]),
                io:format("It lived for ~w milliseconds.~n", [WallTime]),
                io:format("Then it died due to: ~p~n", [Why])
        end,

    on_exit(Pid, TerminationFun), %%Returns Pid of monitor.
    Pid. %%Need to return Pid of function being monitored.

atomize() ->
    receive
        List -> list_to_atom(List)
    end.
    
test() ->
    timer:sleep(500),  %%Allow time for shell to startup. 
    io:format("testing...~n"),

    Pid = my_spawn(my, atomize, []),
    timer:sleep(2000),  %%Allow atomize() to run for awhile.
    Pid ! hello.  %%Causes error in Pid.


```

In the shell:
```
$ ./run
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false

Eshell V8.2  (abort with ^G)

1> testing...
Process (<0.59.0>) terminated. It lived for 2001 milliseconds.
Then it died due to: {badarg,[{erlang,list_to_atom,[hello],[]},
                              {e2,atomize,0,[{file,"e2.erl"},{line,32}]}]}

=ERROR REPORT==== 7-May-2017::15:16:44 ===
Error in process <0.59.0> with exit value:
{badarg,[{erlang,list_to_atom,[hello],[]},
         {e2,atomize,0,[{file,"e2.erl"},{line,32}]}]}
```

Once again, there's a serious problem with the solution: if the process running the funtion dies before the monitor can be created, then the timing info won't be printed out.  If we are forced to use `on_ext()`, which requires a Pid as an argument, then we have to create the process first, which means there will always be a chance that the process will die before the monitor can be established. (**Edit:** A monitor will still work if the process doesn't exist when the monitor is created: the monitor will receive a 'DOWN' message and Why will be noproc, meaning no process).
