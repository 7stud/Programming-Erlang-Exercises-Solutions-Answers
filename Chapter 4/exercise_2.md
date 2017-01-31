I used the following functions from the `erlang` module:

```
size/1        Gets the size of a tuple.
element/2     Gets the element of a tuple at the specified index.
```
my.erl:
```erlang
-module(my).
-compile(export_all).   %You  can use this instead of -export([f/1, g/2, ...]).

tuple_to_list({}) -> [];
tuple_to_list(T) -> tuple_to_list_acc(size(T), T, []).   %Will acces the elements of the tuple starting at the largest 
tuple_to_list_acc(0, _, Acc) -> Acc;                     %index and work down to the smallest index.
tuple_to_list_acc(CurrIndex, T, Acc) ->
   NewAcc = [element(CurrIndex, T) | Acc],
   tuple_to_list_acc(CurrIndex-1, T, NewAcc).
```

In the shell:

```
80> c(my).
{ok,my}

81> my:tuple_to_list({1, 2, 3}).
[1,2,3]

82> my:tuple_to_list({}).       
[]

83> my:tuple_to_list({1, true, "hello"}).
[1,true,"hello"]

84> my:tuple_to_list({1, true, {2.3, "hello"}}).
[1,true,{2.3,"hello"}]
```

Oh boy...I looked at the exercise again while thinking about list comprehensions:

```
module(my).
-compile(export_all).

tuple_to_list(T) -> 
    [
        element(I, T) || I <- lists:seq(1, size(T))
    ].

%tuple_to_list({}) -> [];
%tuple_to_list(T) -> tuple_to_list_acc(size(T), T, []).
%
%tuple_to_list_acc(0, _, Acc) -> Acc;
%tuple_to_list_acc(CurrIndex, T, Acc) ->
%   NewAcc = [element(CurrIndex, T) | Acc],
%   tuple_to_list_acc(CurrIndex-1, T, NewAcc).
```

Wow, like a boss!  My first erlang one liner:

    tuple_to_list(T) -> [element(I, T) || I <- lists:sequ(1, size(T))].

In the shell:

```erlang
30> c(my).
{ok,my}

31> my:tuple_to_list({1, 2, 3}).
[1,2,3]

32> my:tuple_to_list({}).       
[]

33> my:tuple_to_list({1, true, {2.3, false}, fun(X) -> X*2 end}).
[1,true,{2.3,false},#Fun<erl_eval.6.90072148>]

```
