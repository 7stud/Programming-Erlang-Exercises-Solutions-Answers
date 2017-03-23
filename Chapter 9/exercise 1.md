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
dia3.erl:6: Invalid type specification for function dia3:add/2. The success typing is (number(),number()) -> float()
Unknown functions:
  eunit:test/1
 done in 0m0.50s
done (warnings were emitted)
```
