```erlang
-module(ex1).
-export([my_spawn/3]).

my_spawn(Mod, Func, Args) ->
    {Pid, Ref}  = spawn_monitor(Mod, Func, Args),
    statistics(runtime),
    statistics(wall_clock),
    io:format("Pid: ~w, Ref: ~w~n", [Pid, Ref]),

    receive
        {'DOWN', Ref, process, Pid, Why} ->
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
-export([calc/0]).


calc() ->
    receive
        after 5000 ->
                10/0
        end.       
```

In the shell:

```
18> c(a).                     
a.erl:8: Warning: this expression will fail with a 'badarith' exception
{ok,a}

19> c(ex1).                   
{ok,ex1}

20> ex1:my_spawn(a, calc, []).
Pid: <0.112.0>, Ref: #Ref<0.0.0.634>
Process <0.112.0> (#Ref<0.0.0.634>) lived for 0 (5005) milliseconds,
then died due to: {badarith,[{a,calc,0,[{file,"a.erl"},{line,8}]}]}
*---------*
=ERROR REPORT==== 4-Apr-2017::01:14:03 ===
Error in process <0.112.0> with exit value: {badarith,[{a,calc,0,[{file,"a.erl"},{line,8}]}]}

ok
21> 

```
