I feel like there should be a trickier solution than my solution, e.g. using a server like process to handle the `register()`'ing.  I envision `start/2` sending a message to the server process to `register()` a name, which would allow the first message in the mailbox to win when two processes try to register the same name.  But if `start()` is executing in two different processes, I don't know how to connect those function calls to a central server process.  I guess I could register the name `server`, then both processes could send a message to `server`--but that would require that the server process be started prior to calling `start/2`, which I don't think is a very convenient solution. 

As a result, my solution uses the VM as the central process, and I find out what processes have previously been registered by calling `registered()`.  However, I am unsure if my solution ***guarantees that one process succeeds and one process fails***, and because I unable to reason about that I don't believe my solution is correct!


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

Okay, I did some searching and I found an explanation for why the above will ***not*** guarantee that one process will succeed and another process will fail.  First of all, I overlooked the `whereis()` function listed on p. 195 along with the register functions, so here is my code refactored to use `whereis()`:

start(Atom, Fun) ->
    case whereis(Atom) of  %whereis() is listed on p. 195 along with register().
        undefined ->
            register(Atom, spawn(Fun) );
        _ ->
            {error, {name_taken, Atom}}
    end.

Secondly, I think the question is mistated because one of the two processes that is calling `start/2` *will* fail.  Rather, the question should require that one process is guaranteed to *spawn* the Fun, and the other process is guaranteed *not* to spawn the Fun.

With my code, it's possible for two processes executing `start/2` at the same time to spawn a Fun.  When two processes execute at the same time, it's possible for one line in one process to execute, then one line in another process to execute.  So, what if this happens:

    process1:  case whereis(hello) of 
    process2:  case whereis(hello) of

Both those lines will return `undefined`, meaning that no process has been registered with the name hello (that's assuming that some other process hasn't already registered the name hello).  Subsequently, this could happen:

    process1: register(Atom, spawn(hello) );
    process2: register(Atom, spawn(hello) ); 

process1 will win and `register()` the name hello.  But when process2 calls `register()`, any expressions in the argument list have to be evaluated first, so `spawn(hello)` will execute.  Then `register()` will throw an exception because process1 already took that name, which will kill process2.  That will leave the process2's spawned process still running.  As a result, both processes will spawn a new process, one spawned process will be named hello and the other spawned process will not have a name.

To guarantee that only one process is able to spawn the Fun, the fix is:

```erlang
start(Name, Fun) ->
    spawn(fun() ->
                  register(Name, Fun),
                  Fun()
          end).
```

With that code, if register() throws an exception then the spawned process will fail (also taking down the process that called start/2).

I found the fix here:

http://erlang.org/pipermail/erlang-questions/2007-July/028139.html


        
