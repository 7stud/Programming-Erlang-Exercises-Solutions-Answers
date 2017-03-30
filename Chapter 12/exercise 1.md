I feel like there should be a trickier solution than my solution, e.g. using a server like process to handle the `register()`'ing.  I envision `start/2` sending a message to the server process to `register()` a name, which would allow the first message in the mailbox to win when two processes try to register the same name.  But if `start()` is executing in two different processes, I don't know how to connect those function calls to a central server process.  I guess I could register the name `server`, then both processes could send a message to `server`, but that would require that the server process be started prior to calling `start/2`, which I don't think is a very good solution. 

As a result, my solution uses the VM as the central process, and I find out what processes have previously been registered by calling `registered()`:


```erlang
-module(one).
-export([start/2]).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
    true = start(hello, fun() -> wait() end ),
    {error, {name_taken, hello}} = start(hello, fun() -> wait() end),
    
    %The following works:
    %unregister(hello),
    %true = start(hello, fun() -> wait() end ),
    
    hello ! stop,
    receive after 10 -> true end, %Pause here required for next line to work 
    true = start(hello, fun() -> wait() end ),

    all_tests_passed.
     

start(Atom, Fun) ->
    case member(Atom, registered() ) of
        false ->
            register(Atom, spawn(Fun) );
        true ->
            {error, {name_taken, Atom}}
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
