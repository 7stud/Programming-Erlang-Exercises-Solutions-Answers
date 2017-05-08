I feel like there should be a trickier solution than my solution, maybe using a server like process to handle the `register()`'ing.  I envision `start/2` sending a message to the server process to `register()` a name, which would allow the first message in the mailbox to win when two processes try to register the same name.  But if `start()` is executing in two different processes, I don't know how to connect those function calls to a central server process.  I guess I could register the name `server`, then both processes could send a message to `server`--but that would require that the server process be started prior to calling `start/2`, which I don't think is a very convenient solution. 

As a result, my solution uses the VM as the central process, and I call `registered()` to discover what names have previously been registered.  However, I am unsure if my solution ***guarantees that one process succeeds and one process fails***, and because I am unable to reason about that I don't believe my solution is correct!


```erlang
-module(one).
-export([start/2]).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
    Fun = fun() -> wait() end,

    true = start(hello, Fun),
    {error, {name_taken, hello}} = start(hello, Fun),
    
    %The following works:
    %unregister(hello),
    %true = start(hello, Fun),
    
    hello ! stop,
    receive after 10 -> true end, %Pause here required for next line to work 
    true = start(hello, Fun),

    all_tests_passed.
     

start(Name, Fun) ->
    case member(Name, registered() ) of
        false ->
            register(Name, spawn(Fun) );
        true ->
            {error, {name_taken, Name}}
    end.

%----------

wait() ->                        
    receive
        stop -> void
    end.
        
%-----------

member_test() ->
    false = member(1, []),
    true = member(a, [a,b]),
    false = member(x, [a,b,c]),
    true = member(x, [a,b,x]),
    all_tests_passed.

member(_, []) ->
    false;
member(X, [X|_]) ->
    true;
member(X, [_|Ys]) ->
    member(X, Ys).
    
```
------
Okay, to settle my unease with my solution, I did some searching around.  First of all, I overlooked the `whereis()` function listed on p. 195 (alongside the register functions), so here is my code refactored to use `whereis()`:
```erlang
start(Atom, Fun) ->
    case whereis(Atom) of  %whereis() is listed on p. 195 along with register().
        undefined ->
            register(Atom, spawn(Fun) );
        _ ->
            {error, {name_taken, Atom}}
    end.
```
Secondly, I think the question is mistated because if two processes simultaneously call `start/2` one of them *will* fail.  Rather, the question should require that one process be guaranteed to *spawn* the Fun and the other process be guaranteed *not to spawn* the Fun.

With my code, it's possible for two processes to simultaneously execute `start/2` and each spawn their own Fun.  When two processes execute at the same time, it's possible for a line in one process to execute, then a line in the other process to execute.  So, what happens if execution proceeds in this order:

    process1:  case whereis(hello) of 
    process2:  case whereis(hello) of

In the first line, `whereis()` will return `undefined`,  meaning that no process has been registered with the name hello (that's assuming that some other process hasn't already registered the name hello).  Thereafter, execution could switch to process2 and `whereis()` will return `undefined` again. As a result, both processes will attempt to call register():

    case whereis(Atom) of 
        undefined ->
            register(Atom, spawn(Fun) );
            
            
    process1: register(hello, spawn(Fun) );
    process2: register(hello, spawn(Fun) ); 

Let's assume that process1 wins and `register()`'s the name hello.  Next, when process2 calls `register()`, any expressions in the argument list have to be evaluated first, so `spawn(Fun)` will execute and return a pid.  Then `register()` will execute and throw an exception because process1 already took the name hello, which will kill process2.  But after process2 dies, the process spawned by process2 will still be alive and running.  As a result, both processes will spawn a new process--one of the spawned process will be named hello and the other spawned process will not have a name.

To guarantee that only one process is able to spawn a function, the fix is:

```erlang
start(Name, Fun) ->
    spawn(fun() ->
                  register(Name, Fun),
                  Fun()
          end).
```

With that code, if register() throws an exception then the spawned process will fail (also taking down the process that called start/2).  The difference between my solution and the fix is that my start/2 function first checks if the name is registered, then spawns a fun, while the fix first spawns a fun and inside the spawned fun the fix tries to register the name.

I found the fix here:

http://erlang.org/pipermail/erlang-questions/2007-July/028139.html


        
