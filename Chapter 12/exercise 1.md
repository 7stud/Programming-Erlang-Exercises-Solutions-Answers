I feel like there should be a trickier solution than my solution, e.g. using a server like process to handle registering, then the first message in the mailbox is the one that wins.
```erlang
-module(one).
-export([start/2]).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
    true = start(hello, fun() -> wait() end ),
    {error, {not_available, hello}} = start(hello, fun() -> wait() end),
    
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
            {error, {not_available, Atom}}
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
