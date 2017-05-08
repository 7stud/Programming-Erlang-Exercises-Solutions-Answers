```erlang
-module(e3).
-export([my_spawn/4, killer/2, test/0, loop/1]).

killer(Pid, Timeout) ->
    receive
    after Timeout ->
            io:format("killer(): ~w sent 'kill' signal after ~w milliseconds.~n", 
                      [Pid, Timeout]),
            exit(Pid, kill)
    end.

my_spawn(Mod, Func, Args, Timeout) ->
    Pid = spawn(Mod, Func, Args),
    %%exit(Pid, testing),
    io:format("Func running in process: ~w~n", [Pid]),
    spawn(?MODULE, killer, [Pid, Timeout]),
    Pid.
 
%-------------                
    
test() ->
    timer:sleep(500), %%Allow time for the output from the shell startup to be printed.
    my_spawn(?MODULE, loop, [1], 7500).
    
loop(N) ->
    receive
    after 1000 ->
            io:format("loop: tick ~w~n", [N]),
            loop(N+1)
    end.
    
```
Note that if `Func` is trapping exits, then the process it is running in won't die unless you send the process a 'kill' signal.

In the shell,
```
$ ./run
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.2  (abort with ^G)

1> Func running in process: <0.59.0>
loop: tick 1
loop: tick 2
loop: tick 3
loop: tick 4
loop: tick 5
loop: tick 6
loop: tick 7
killer(): <0.59.0> sent 'kill' signal after 7500 milliseconds.
```
