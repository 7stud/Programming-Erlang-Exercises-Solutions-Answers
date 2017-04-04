```erlang
-module(ex1).
-export([my_spawn/3]).

my_spawn(Mod, Func, Args) ->
    {Pid, Ref}  = spawn_monitor(Mod, Func, Args),
    statistics(runtime),
    statistics(wall_clock),
    io:format("Pid: ~w, Ref: ~w~n", [Pid, Ref]),

    receive
        {'DOWN', Ref, process, Pid, Why} ->  %Ref and Pid are already bound!
            {_, RunTime} = statistics(runtime),
            {_, WallTime} = statistics(wall_clock),
            
            io:format("Process ~w (~w) lived for ~w (~w) milliseconds,~n", [Pid, Ref, RunTime, WallTime]),
            io:format("then died due to: ~p~n", [Why]),
            io:format("*---------*", [])
    end.

```

-------

```erlang
-module(a).
-export([calc/1]).

calc(Denominator) ->
    receive
    after 5000 ->
        10/Denominator
    end.  
```

In the shell:

```
28> c(a).                      
{ok,a}

29> c(ex1).                    
{ok,ex1}

30> ex1:my_spawn(a, calc, [0]).
Pid: <0.150.0>, Ref: #Ref<0.0.0.912>

=ERROR REPORT==== 4-Apr-2017::02:50:53 ===
Error in process <0.150.0> with exit value: {badarith,[{a,calc,1,[{file,"a.erl"},{line,8}]}]}

Process <0.150.0> (#Ref<0.0.0.912>) lived for 0 (5005) milliseconds,
then died due to: {badarith,[{a,calc,1,[{file,"a.erl"},{line,8}]}]}
*---------*
ok

31> ex1:my_spawn(a, calc, [0]).
Pid: <0.152.0>, Ref: #Ref<0.0.0.922>
Process <0.152.0> (#Ref<0.0.0.922>) lived for 0 (5005) milliseconds,
then died due to: {badarith,[{a,calc,1,[{file,"a.erl"},{line,8}]}]}

=ERROR REPORT==== 4-Apr-2017::02:51:16 ===
Error in process <0.152.0> with exit value: {badarith,[{a,calc,1,[{file,"a.erl"},{line,8}]}]}

*---------*
ok

32> 
```

In my tests, the ERROR REPORT can get interleaved anywhere in the output.
