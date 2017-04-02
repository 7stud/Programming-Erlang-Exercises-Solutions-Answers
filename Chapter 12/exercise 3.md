I found the ring exercise extremely difficult.  I wrote a solution program five or six times, and every time it would hang somewhere.  Finally, I got something to work, but it seems overly complex.  I had to create a separate receive loop for the "start" process in order to decrement the loop count.  I also passed the pid of the next process as an argument to the receive loop to keep the pid from going out of scope.

```erlang
-module(ring2).
-export([ring/2]).
-include_lib("eunit/include/eunit.hrl"). 
                   
ring(NumProcs, NumLoops) ->
    NextPid = spawn(fun() -> create_ring(NumProcs-1, self()) end),
    NextPid ! {NumLoops, "hello"},
    start(NextPid).  %receive loop for the "start" process.

create_ring(1, StartPid) ->  %...then stop spawning processes.
    loop(StartPid);  %receive loop for the other processes.
create_ring(NumProcs, StartPid) ->
    NextPid = spawn(fun() -> create_ring(NumProcs-1, StartPid) end),
    loop(NextPid).

%receive loop for the "start" process:
start(NextPid) ->
    receive 
        {1, Msg} ->  %...then stop looping.
            io:format("*start* received message: ~s (~w)~n", [Msg, 1]),
            NextPid ! stop;  %kill other processes; this processs will die because it stops looping.
        {NumLoops, Msg} ->
            io:format("*start* received message: ~s (~w)~n", [Msg, NumLoops]),
            NextPid ! {NumLoops-1, Msg},
            start(NextPid)
    end.
   
%receive loop for the other processes:
loop(NextPid) ->
    receive
        {NumLoops, Msg} ->
            io:format("Process ~w received message: ~s (~w)~n", [self(), Msg, NumLoops]),
            NextPid ! {NumLoops, Msg},
            loop(NextPid);
        stop -> 
            NextPid ! stop  % When sent to non-existent "start" process, this still returns stop.
    end.
    
```

In the shell:

```
21> c(ring2).
{ok,ring2}
        
22> ring2:ring(3,4).  %3 processes, 4 loops
Process <0.154.0> received message: hello (4)
Process <0.155.0> received message: hello (4)
*start* received message: hello (4)
Process <0.154.0> received message: hello (3)
Process <0.155.0> received message: hello (3)
*start* received message: hello (3)
Process <0.154.0> received message: hello (2)
Process <0.155.0> received message: hello (2)
*start* received message: hello (2)
Process <0.154.0> received message: hello (1)
Process <0.155.0> received message: hello (1)
*start* received message: hello (1)
stop

23> i().
...
...
<0.26.0>              erlang:apply/2                       17731     6847    0
                      shell:shell_rep/4                       17              
<0.27.0>              kernel_config:init/1                   233     1318    0
                      gen_server:loop/6                        9              
<0.28.0>              supervisor:kernel/1                    233     1984    0
kernel_safe_sup       gen_server:loop/6                        9              
<0.32.0>              erlang:apply/2                        4185    53027   15
                      c:pinfo/1                               50              
Total                                                      75921  1069020   15
                                                             219              
ok
24> 
```
