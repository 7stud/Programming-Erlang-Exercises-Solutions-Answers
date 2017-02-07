Here are a few different solutions:

```erlang
-module(math_functions).
%-export([even1/1, even2/1, even3/1, odd1/1, odd2/1]).
-compile(export_all).

even1(X) when (X rem 2 =:= 0) -> true;
even1(_) -> false.
 
even2(X) ->
    if
        X rem 2 =:= 0   -> true;
        true            -> false
    end.

even3(X) -> 
    case X rem 2 of
        0   -> true;
        1   -> false
    end.


% odd() can be defined in the same myriad ways as above, plus one additional way:

odd2(X) -> not even1(X).
```
