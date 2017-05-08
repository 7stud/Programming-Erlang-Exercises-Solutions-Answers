```erlang
-module(e1).
-export([my_spawn/3, atomize/0, test/0]).

my_spawn(Mod, Func, Args) ->
    %%Create separate process to run Func:
    FuncPid = spawn(Mod, Func, Args),
    statistics(wall_clock),
    
    %%Create separate process for the monitor:
    spawn(fun() ->
        Ref = monitor(process, FuncPid),  %%Ref identifies the FuncPid process
        receive
            {'DOWN', Ref, process, FuncPid, Why} -> %% Ref and FuncPid are bound!
                {_, WallTime} = statistics(wall_clock),
                io:format("Process ~w lived for ~w milliseconds,~n", 
                          [FuncPid, WallTime]),
                io:format("then died due to: ~p~n", [Why]),
                io:format("*---------*~n")
        end 
    end),  %%Monitor process dies after receiving a 'DOWN' message.

    FuncPid. %%To mimic spawn(Mod, Func, Args), return the Pid of the
             %%process that is running Func.

atomize() ->
    receive
        List -> list_to_atom(List)
    end.

test() ->
    timer:sleep(500), %%Allow time for shell startup
                      %%so output appears after 1> prompt.
    io:format("testing...~n"),  

    Atomizer = my_spawn(e1, atomize, []),
    timer:sleep(2000), %%Let atomize() run for awhile.
    Atomizer ! hello,
    ok.

```


In the shell:

```
$ ./run
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.2  (abort with ^G)

1> testing...
Process <0.59.0> lived for 2001 milliseconds,
then died due to: {badarg,[{erlang,list_to_atom,[hello],[]},
                           {e1,atomize,0,[{file,"e1.erl"},{line,27}]}]}

=ERROR REPORT==== 7-May-2017::15:23:31 ===
Error in process <0.59.0> with exit value:
{badarg,[{erlang,list_to_atom,[hello],[]},
         {e1,atomize,0,[{file,"e1.erl"},{line,27}]}]}
*---------*


```
I got sick of typing:

```
$ erl

1> c(e1).

2> e1:test().

```
over and over again.  I shortened my module names to two letters, which makes things easier, but it's still a pain.  So I created a shell script in the same directory called `run`:

```
#!/usr/bin/env bash

erlc -W e1.erl
erl -s e1 test
```

Then I made it executable:
```
$ chmod u+x run
```
Now, I can run my elrang program from the command line like this:
```
$ ./run
```
And if I need to run my program again, I kill the erlang shell with Ctrl+CC, then I can just hit the up arrow on my keyboard, which will scroll to the previous command, and hit Return.  No more typing in the shell!  

Note that sometimes after running my shell script, the shell will be unresponsive, so I can't check `i()` or run `observer:start()`.  If the shell is unresponsive after I run my shell script, and I need the shell to be responsive, then I create a function called `test_init()`, which just spawns my `test()` function; then I substitute the name `test_init` in place of `test` in my shell script.  Just keep that in mind when you use a shell script.
