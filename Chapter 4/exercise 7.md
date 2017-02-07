```erlang
split(L) ->
    split_acc(L, [], []).

split_acc([H|T], Evens, Odds) when (H rem 2 =:= 0) ->
    split_acc(T, [H|Evens], Odds);
split_acc([H|T], Evens, Odds) ->
    split_acc(T, Evens, [H|Odds]);
split_acc([], Evens, Odds) ->
    {lists:reverse(Evens), lists:reverse(Odds)}.
```

In the shell:

```erlang
129> c(math_functions).
{ok,math_functions}

130> math_functions:split(lists:seq(0, 20) ).
{[0,2,4,6,8,10,12,14,16,18,20],[1,3,5,7,9,11,13,15,17,19]}

131> math_functions:split([]).
{[],[]}

132> math_functions:split([2, 4]).
{[2,4],[]}

```
