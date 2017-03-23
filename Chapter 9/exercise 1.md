#### *Write some some modules that produce dialyzer errors:*


```erlang
-module(dia3).
-export([add/2]).
-include_lib("eunit/include/eunit.hrl").


-spec add(X,Y) -> Result when
      X         :: integer(),
      Y         :: integer(),
      Result    :: integer().

add(X,Y) ->
    X/Y.   %The result is a float.
    
```

Running dialyzer:
```
~/erlang_programs$ dialyzer dia3.erl 
-----Now, do some Erlang for great Good!------

  Checking whether the PLT /Users/7stud/.dialyzer_plt is up-to-date... yes
  Proceeding with analysis...
dia3.erl:6: Invalid type specification for function dia3:add/2. 
The success typing is (number(),number()) -> float()
Unknown functions:
  eunit:test/1
 done in 0m0.50s
done (warnings were emitted)
```

------------
```erlang
-module(dia4).
-export([f/2]).


-spec f(String1, String2) -> Result when
      String1     :: string(),
      String2     :: string(),
      Result      :: pos_integer().

f(X, Y) ->
    io:format("~w: ~w~n", [X,Y]),
    %lists:length(X) + lists:length(Y).
    X+Y.
```

Running dialyzer:

```
~/erlang_programs$ dialyzer dia4.erl 
-----Now, do some Erlang for great Good!------

  Checking whether the PLT /Users/7stud/.dialyzer_plt is up-to-date... yes
  Proceeding with analysis...
dia4.erl:5: Invalid type specification for function dia4:f/2. The success typing is (number(),number()) -> number()
 done in 0m0.52s
done (warnings were emitted)

```
