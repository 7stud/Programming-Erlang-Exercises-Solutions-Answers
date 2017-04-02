```erlang
filter(F, L) ->
    [X || X <- L, F(X)].
```

In the shell:
```erlang
115> c(math_functions).
{ok,math_functions}

117> F = fun(X) -> math_functions:even1(X) end.
#Fun<erl_eval.6.90072148>

118> L = lists:seq(0, 10).
[0,1,2,3,4,5,6,7,8,9,10]

119> math_functions:filter(F, L).
[0,2,4,6,8,10]
```

Or, like this:
```erlang
math_functions:filter(fun math_functions:even1/1, L).
[0,2,4,6,8,10]
```
