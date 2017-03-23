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
