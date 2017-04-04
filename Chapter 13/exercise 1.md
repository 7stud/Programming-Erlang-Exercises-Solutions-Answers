
-module(ex1).
-export([my_spawn/3]).

my_spawn(Mod, Func, Args) ->
    {Pid, Ref}  = spawn_monitor(Mod, Func, Args),
    io:format("Pid: ~w, Ref: ~w~n", [Pid, Ref]),
    statistics(runtime),
    statistics(wall_clock),

    receive
        {'DOWN', Ref, process, Pid, Why} ->   %Ref and Pid are bound!
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
-export([test/0]).


test() ->
    receive
        after 5000 ->
                10/0   %Make process crash with badarith error
        end.
        
```

In the shell:

```
12> c(ex1).                   
{ok,ex1}

13> c(a).

14> ex1:my_spawn(a, test, []).
Pid: <0.88.0>, Ref: #Ref<0.0.0.454>
Process <0.88.0> (#Ref<0.0.0.454>) lived for 0 (5005) milliseconds,
then died due to: {badarith,[{a,test,0,[{file,"a.erl"},{line,8}]}]}
*---------*
=ERROR REPORT==== 4-Apr-2017::01:04:30 ===
Error in process <0.88.0> with exit value: {badarith,[{a,test,0,[{file,"a.erl"},{line,8}]}]}

ok
15>
```
