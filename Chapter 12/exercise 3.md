I thought the ring exercise was very difficult.  I wrote five or six different solutions, and every time my program would hang somewhere.  Finally, I got something to work--but it seems overly complex. I created a separate receive loop for the "start" process in order to decrement the loop count in addition to a receive loop for the other processes.  I also passed the pid of the next process as an argument to the receive loops to keep the pid from going out of scope.

```erlang
-module(ring3).
-export([ring/2]).
-include_lib("eunit/include/eunit.hrl"). 
 
                   
ring(NumProcs, NumLoops) ->
    StartPid = self(),
    NextPid = spawn(fun() -> create_ring(NumProcs-1, StartPid) end),
    NextPid ! {NumLoops, "hello"},
    start(NextPid).  %receive-loop for this process, i.e. the "start" process.

create_ring(1, StartPid) ->  %...then stop spawning processes.
    loop(StartPid);  %The last process gets the "start" process as its NextPid.
create_ring(NumProcs, StartPid) ->
    NextPid = spawn(fun() -> create_ring(NumProcs-1, StartPid) end),
    loop(NextPid).

%receive loop for the "start" process:
start(NextPid) ->
    receive 
        {1, Msg} ->  %...then stop looping.
            io:format("**start ~w received: ~s (~w)~n", [self(), Msg, 1]),
            NextPid ! stop;  %kill other processes; this processs will die because it stops looping.
        {NumLoops, Msg} ->
            io:format("**start ~w received: ~s (~w)~n", [self(), Msg, NumLoops]),
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
15> c(ring3).       
{ok,ring3}

16> ring3:ring(5,4).
Process <0.140.0> received message: hello (4)
Process <0.141.0> received message: hello (4)
Process <0.142.0> received message: hello (4)
Process <0.143.0> received message: hello (4)
**start <0.32.0> received: hello (4)
Process <0.140.0> received message: hello (3)
Process <0.141.0> received message: hello (3)
Process <0.142.0> received message: hello (3)
Process <0.143.0> received message: hello (3)
**start <0.32.0> received: hello (3)
Process <0.140.0> received message: hello (2)
Process <0.141.0> received message: hello (2)
Process <0.142.0> received message: hello (2)
Process <0.143.0> received message: hello (2)
**start <0.32.0> received: hello (2)
Process <0.140.0> received message: hello (1)
Process <0.141.0> received message: hello (1)
Process <0.142.0> received message: hello (1)
Process <0.143.0> received message: hello (1)
**start <0.32.0> received: hello (1)
stop

17> i().
...
...
<0.25.0>              group:server/3                        2586    40094    0
                      group:server_loop/3                      4              
<0.26.0>              erlang:apply/2                       17731     6223    0
                      shell:shell_rep/4                       17              
<0.27.0>              kernel_config:init/1                   233     2322    0
                      gen_server:loop/6                        9              
<0.28.0>              supervisor:kernel/1                    233     2150    0
kernel_safe_sup       gen_server:loop/6                        9              
<0.32.0>              erlang:apply/2                         610    31978    8
                      c:pinfo/1                               50              
Total                                                      49206  1005265    8
                                                             219              
ok

18> ring3:ring(3, 7).
Process <0.146.0> received message: hello (7)
Process <0.147.0> received message: hello (7)
**start <0.32.0> received: hello (7)
Process <0.146.0> received message: hello (6)
Process <0.147.0> received message: hello (6)
**start <0.32.0> received: hello (6)
Process <0.146.0> received message: hello (5)
Process <0.147.0> received message: hello (5)
**start <0.32.0> received: hello (5)
Process <0.146.0> received message: hello (4)
Process <0.147.0> received message: hello (4)
**start <0.32.0> received: hello (4)
Process <0.146.0> received message: hello (3)
Process <0.147.0> received message: hello (3)
**start <0.32.0> received: hello (3)
Process <0.146.0> received message: hello (2)
Process <0.147.0> received message: hello (2)
**start <0.32.0> received: hello (2)
Process <0.146.0> received message: hello (1)
Process <0.147.0> received message: hello (1)
**start <0.32.0> received: hello (1)
stop

19> i().
...
...
<0.25.0>              group:server/3                        2586    58135    0
                      group:server_loop/3                      4              
<0.26.0>              erlang:apply/2                       17731     6407    0
                      shell:shell_rep/4                       17              
<0.27.0>              kernel_config:init/1                   233     2322    0
                      gen_server:loop/6                        9              
<0.28.0>              supervisor:kernel/1                    233     2150    0
kernel_safe_sup       gen_server:loop/6                        9              
<0.32.0>              erlang:apply/2                        4185    51215    9
                      c:pinfo/1                               50              
Total                                                      51793  1046386    9
                                                             219              
ok

```
