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
    statistics(wall_clock),

    TerminationFun = 
        fun(Why) ->
                {_, WallTime} = statistics(wall_clock),
                io:format("Process (~w) terminated. ", [Pid]),
                io:format("It lived for ~w milliseconds.~n", [WallTime]),
                io:format("Then it died due to: ~p~n", [Why])
        end,

    on_exit(Pid, TerminationFun), %%Returns Pid of monitor.
    Pid.  %%Need to return the Pid of the function that is being monitored.

    
atomizer() ->
    receive
        List -> list_to_atom(List)
    end.
    
test() ->
    timer:sleep(500),  %%Give everything time to start up, so output appears after 1> prompt.
    io:format("testing...~n"),

    Pid = my_spawn(my, atomizer, []),

    timer:sleep(1000),
    Pid ! hello.

```

In the shell:
```
$ ./run.sh
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false

Eshell V8.2  (abort with ^G)

1> testing...
Process (<0.59.0>) terminated. It lived for 1001 milliseconds.
Then it died due to: {badarg,[{erlang,list_to_atom,[hello],[]},
                              {my,atomizer,0,[{file,"my.erl"},{line,32}]}]}

=ERROR REPORT==== 7-May-2017::13:10:23 ===
Error in process <0.59.0> with exit value:
{badarg,[{erlang,list_to_atom,[hello],[]},
         {my,atomizer,0,[{file,"my.erl"},{line,32}]}]}
```

I'm sick of typing:

```
$ erl

1> c(my).

2> my:test().

```

So I created a shell script in the same directory called `run.sh`:

```
#!/usr/bin/env sh

erlc -W my.erl
erl -s my test
```

Then I made it executable:
```
$ chmod u+x run.sh
```
Now, I can run my program like this:
```
$ ./run.sh
```
And if I need to run my program again, I just hit the up arrow on my keyboard, which will scroll to the previous command, then I hit Return.  No more typing in the shell!
