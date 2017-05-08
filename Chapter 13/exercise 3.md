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
